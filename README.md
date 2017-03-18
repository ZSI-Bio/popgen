## **Popgen:** a toolset for population structure analysis using the NGS data

The goal of the project is to analyze the influence of various pre-processing methods, such as variant filtering, LD-pruning, and PCA or multi-dimensional scaling transformations of genotype matrix, and their parameters on the final results of the classification/clustering algorithms for population structure analysis.

Popgen icnludes two frameworks:

* *popgenR*, a tool for the analysis of population structure with automated estimation of the optimal QC’s and machine learning’s parameters. Such a framework is capable to fine-tune these parameters for different levels of ethnic populations, such as super- and sub-population groups define in 1000 Genomes dataset.

* *popgenSpark*, a tool that deal with dataset containing large number of samples (e.g. > 10,000 individuals). Both dimensionality reduction and machine learning methods have been implemented using Apache Spark.


# Installation
* **popgenR**

Install the package from the source code:
```sh
R CMD INSTALL popgen_0.1.0_R_x86_64-pc-linux-gnu.tar.gz
```
* **popgenSpark**

In the folder *popgen-spark* run
```sh
sbt assembly
```
## Examples

* **popgenR**
1. Load data:

``` R
genofile           <- chr22load() # Load chr 22 
panel              <- panelLoad() # Load panel file
pop.df             <- panel[, c(1, 3)] # Select SampleId and Region
colnames(pop.df)   <- c("SampleId", "Region")
```

2. Tune the LD:

```R
min.maf <- 0.005
max.maf <- 0.05
npc     <- 5
ld.set  <- seq(0.1, 1, by = 0.1)

ld.tuning.result <- tuningLD(genofile, pop.df, min.maf, max.maf, ld.set, n.pc = n.pc, 
                                    n.clusters = length(unique((pop.df$Region)))) # ld tuning
ld.tuning.result$crossTbl # tuned parameters
plotTuning(ld.tuning.result$crossTbl) # plot
```

3. Perform clustering and plot the map:

```R
n.clusters <- length(unique((pop.df$Region))) # perform clustering for n = number of population groups

pca.df     <- getQCPCAdf(genofile, pop.df, min.maf, max.maf, ld = ld.tuning.result$bestLD, npc = npc, 
                                                          with.eig.tbl = F) # QC analysis, ld.tuning.result$bestLD is tuned LD

obj.hclust <- HClust( data = pca.df, k = n.clusters) # hierarchical clustering
obj.kmeans <- kmeansClust( data = pca.df, k = n.clusters) # kmeans
obj.emc    <- emcClust( data = pca.df, k = n.clusters) # em

summaryClustering(obj.hclust, obj.kmeans, obj.emc) # clustering evaluation metrics

obj.hclust@conf.matr # confusion matrix

prediction.df <- cbind(pca.df, obj.hclust@prediction) # bind with PCs, needed for plot

attach(prediction.df)
plotMap2(prediction.df, PC3, PC2) # or plotMap(prediction.df, PC3, PC2)
detach(prediction.df)

```

More examples can be found in the folder *popgen-0.1.0/tests/*


* **popgenSpark**

1. PCA with Spark Mllib

```scala

val df = sqlContext.read.parquet("path/to/chr22snps.parquet")
val nPC: Int = 5 
val labels: String = "Region"
val ignoredColumn = "SampleId"

val colNames = ds.columns.filter(c => c != labels && c != ignoredColumn)

val assembler = new VectorAssembler()
    .setInputCols(colNames)
    .setOutputCol("pcafeatures")

val pca = new PCA()
    .setInputCol("pcafeatures")
    .setOutputCol("features")
    .setK(nPC)

val pipeline = new Pipeline()
    .setStages(Array(assembler, pca))

val model = pipeline.fit(ds)
val pcaFeaturesDF = model.transform(ds).select(labels, ignoredColumn, "features")

```
2. K-means with H2O:

```scala

val numClust = 5

val h2oDF = h2oContext.asH2OFrame(df)
h2oDF.replace(h2oDF.find("Region"), h2oDF.vec("Region").toCategoricalVec()).remove()
h2oDF.update()

val kmeansParameters = new KMeansParameters()
kmeansParameters._train = h2oDF._key
kmeansParameters._response_column = labels
kmeansParameters._ignored_columns =  ignoredColumns
kmeansParameters._k = numClust

val kmeans = new KMeans(kmeansParameters)
val kmeansModel = kmeans.trainModel().get()

val predictionDF = kmeansModel.score(h2oDF)
predictionDF.add("prediction", predictionDF.vec("predict").toCategoricalVec)
predictionDF.update()

```


# Run the test

* **popgenSpark**

```sh
spark-submit <spark-options> --class com.zsibio.popgen.ClusteringMllibDF <path/to/jar> <path/to/chr.parquet>
```
