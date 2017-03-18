# Popgen
# Example 1. Clustering of super- and sub-population groups
# Super-population
# Please note that not all performed steps are included in this example


genofile           <- chr22load() # Load chr 22 (Original repo 1000 Genomes)
panel              <- panelLoad() # Load panel file
panel$`Sub-region` <- as.character(panel$`Sub-region`)
pop.df             <- panel[, c(1, 3)] # Choose SampleId and Region
colnames(pop.df)   <- c("SampleId", "Region")
n.clusters         <- length(unique((pop.df$Region))) # perform clustering for n = number of population groups

min.maf <- 0.005
max.maf <- 0.05
npc     <- 5
ld      <- 0.2

pca.df  <- getQCPCAdf(genofile, pop.df, min.maf, max.maf, ld=ld, npc = npc, with.eig.tbl = F) # QC analysis (breifly description in 'man')

obj.hclust <- HClust( data = pca.df, k = n.clusters) # hierarchical clustering
obj.kmeans <- kmeansClust( data = pca.df, k = n.clusters) # kmeans
obj.emc    <- emcClust( data = pca.df, k = n.clusters) # em

summaryClustering(obj.hclust, obj.kmeans, obj.emc) # clustering evaluationn metrics

prediction <- obj.hclust@prediction # returm a vector of prediction, needed for plot
prediction.df <- cbind(pca.df, prediction) # bind with PCs, needed for plot

attach(prediction.df)
plotMap2(prediction.df, PC3, PC2) # or plotMap(prediction.df, PC3, PC2)
detach(prediction.df)

obj.hclust@conf.matr # return confusion matrix

# -------
# Sub-population
#
# Let us supposed that we would like to analyze the African's majority
cluster    <- 4      # cluster in which Agfrican's majority was assigned by hier. algorithm
pop        <- "AFR"  # population abbriviation

# Let us see how this sub-population looks like
prediction.df.super              <- cbind(panel$`Sub-region`, prediction.df) %>% select(- Region)
colnames(prediction.df.super)[1] <- "Region"
prediction.df.sub                <- prediction.df.super [prediction.df.super$prediction == cluster, ] # select only those observations, which belong to 4th cluster
attach(prediction.df.sub)
plotMap2(prediction.df.sub, PC3, PC4)
detach(prediction.df.sub)

pop.df.super <- cbind(panel, prediction)

pop.df           <- pop.df.super[pop.df.super$prediction == cluster, ]
pop.df           <- pop.df[, c(1, 2)]
colnames(pop.df) <- c("SampleId", "Region")
pop.df$Region    <- as.factor(as.character(pop.df$Region))
n.clusters       <- length(levels(pop.df$Region))

# new QC parameters for sub-population
min.maf <- 0.005
max.maf <- 0.1
npc     <- 10
ld      <- 0.5

pca.df.sub  <- getQCPCAdf(genofile, pop.df, min.maf, max.maf, ld, npc = npc, with.eig.tbl = F)

obj.hclust <- HClust( data = pca.df.sub, k = n.clusters)
obj.kmeans <- kmeansClust( data = pca.df.sub, k = n.clusters)
obj.emc    <- emcClust( data = pca.df.sub, k = n.clusters)

summaryClustering(obj.hclust, obj.kmeans, obj.emc)
obj.emc@conf.matr

prediction    <- obj.emc@prediction
prediction.df <- cbind(pca.df.sub, prediction)

attach(prediction.df)
plotMap2(prediction.df, PC2, PC3 )
detach(prediction.df)
