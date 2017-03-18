package com.zsibio

// import com.zsibio.popgen.LoadData
// import com.zsibio.popgen.Time

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._

import org.apache.spark.ml.{Pipeline, PipelineModel}
import org.apache.spark.ml.clustering.{KMeans, KMeansModel}
import org.apache.spark.ml.feature.{StringIndexer, VectorAssembler, VectorSlicer, StandardScaler, PCA}
import org.apache.spark.mllib.linalg.{DenseVector, Vector, Vectors}

object ClusteringMllibDF {

  /* PCA */

  def pcaML(ds: DataFrame, nPC: Int = 4, labels: String = "Region", outputCol : String = "features") : DataFrame ={
    val colNames = ds.columns.filter(c => c != labels && c != "SampleId")
    val assembler = new VectorAssembler()
      .setInputCols(colNames)
      .setOutputCol("pcafeatures")

    /*  val standardizer = new StandardScaler()
        .setInputCol("features")
        .setOutputCol("normFeatures")
        .setWithMean(false)
        .setWithStd(true)*/

    val pca = new PCA()
      .setInputCol("pcafeatures")
      .setOutputCol("features")
      .setK(nPC)

    val pipeline = new Pipeline()
      .setStages(Array(assembler, pca))//standardizer, pca))

    val model = pipeline.fit(ds)
    val pcaFeaturesDF = model.transform(ds).select("Region", "SampleId", "features")//"pcaFeatures")

    pcaFeaturesDF
  }

  /* Transform DF if there is no PCA step */
  def transformDF (df: DataFrame) : DataFrame ={
    val colNames = df.drop("Region").drop("SampleId").drop("label").columns
    val assembler = new VectorAssembler().setInputCols(colNames).setOutputCol("features")
    val output = assembler.transform(df)
    val kmeansDF = output.select("label", "features")
    kmeansDF
  }

  def kmeansML (df: DataFrame, k: Int) : KMeansModel ={
    val kmeans = new KMeans().setK(k).setFeaturesCol("features").setPredictionCol("prediction")
    val kmeansModel = kmeans.fit(df)
    kmeansModel
  }

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setAppName("popgenClusteringWithMllib")
      .set("spark.driver.maxResultSize", "0")

    val sc = new SparkContext(conf)
    val sqlContext = new SQLContext(sc)


    if (args.length < 1) {
      println("There is no input files.")
      println("agrs(0) is a chromosome (parquet format)")
      println("agrs(1) is a number of columns to select")
      println("agrs(2) is a number for scaling the data; if nothing then 1")
      exit(1)
    }

    val chr = args(0)
    val dsX = if (args.length == 2) 1 else args(2).toInt
    val nSelectCols = args(1).toInt
    val dataObj = new LoadQCdataFrame(chr, nSelectCols, dsX, sc, sqlContext)

    val timeObj = new Time()
    /*
      PCA + kMeans
     */

    val pcaDF = dataObj.dfExploded.cache()
    // println(s"DF = ${pcaDF.count} x ${pcaDF.columns.length}")
    val nPC = 5//args(1).toInt
    var t0 = System.currentTimeMillis()
    val resultPcaDF = pcaML(pcaDF, nPC, "Region", "features")
    var t1 = System.currentTimeMillis()
    pcaDF.unpersist
    val pcaTime = timeObj.formatTimeDiff(t0, t1)

    val kmeansDF = resultPcaDF.cache()
    t0 = System.currentTimeMillis()
    val kmeansModel = kmeansML(kmeansDF, 5)
    t1 = System.currentTimeMillis()
    val predictionDF = kmeansModel.transform(kmeansDF)
    kmeansDF.unpersist
    val kmeansTime = timeObj.formatTimeDiff(t0, t1)

    println(s"PCA time: $pcaTime")
    println(s"kMeans time: $kmeansTime")
  }
}
