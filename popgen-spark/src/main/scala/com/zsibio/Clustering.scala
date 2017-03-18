package com.zsibio

import org.apache.spark.SparkContext
import org.apache.spark.ml.{Pipeline, PipelineModel}
import org.apache.spark.ml.clustering.{KMeans, KMeansModel}
import org.apache.spark.ml.feature.{StringIndexer, VectorAssembler}
import org.apache.spark.mllib.linalg.{DenseVector, Vector, Vectors}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{DataFrame, SQLContext}
import org.apache.spark.mllib.clustering.{BisectingKMeans, BisectingKMeansModel, GaussianMixture, GaussianMixtureModel}

trait ClusteringMethods {
  def gmm(ds: DataFrame, categoricalVars : Array[String], K : Int, maxIteration: Int): GaussianMixtureModel
  def bkm(ds: DataFrame, categoricalVars : Array[String], K : Int, maxIteration: Int): BisectingKMeansModel
  def predict(model: GaussianMixtureModel, ds: DataFrame, categoricalVars : Array[String]) : DataFrame
  def predict(model: BisectingKMeansModel, ds: DataFrame, categoricalVars : Array[String]) : DataFrame
  def gmmKTuning(ds: DataFrame, categoricalVars : Array[String], kSet : Seq[Int], maxIteration: Int, nReapeat: Int): scala.collection.Map[Int, Double]
  def bkmKTuning(ds: DataFrame, categoricalVars : Array[String], kSet : Seq[Int], maxIteration: Int, nReapeat: Int): scala.collection.Map[Int, Double]
  def purity(ds: DataFrame): Double
}

@SerialVersionUID(15L)
class Clustering (sc: SparkContext, sqlContext: SQLContext) extends Serializable with ClusteringMethods{

  /* Function for DataFrame with multiple columns of features */
/*
    private def dfToRDD (ds: DataFrame, categoricalVars : Array[String]) : RDD[Vector] = {
    val arrsnps : Array[DataFrame] = categoricalVars.map{var d : DataFrame = ds; variable => {d = d.drop(variable); d}}
    val snps : DataFrame = arrsnps(arrsnps.length - 1)
    snps.rdd.map(row => Vectors.dense(row.toSeq.toArray.map(_.asInstanceOf[Double]))).cache()
  }*/

  /* Function for DataFrame transformation with SINGLE column with multiple features */
  private def dfToRDD (ds: DataFrame,  categoricalVars : Array[String]) : RDD[Vector] = {
    val arrsnps : Array[DataFrame] = categoricalVars.map{var d : DataFrame = ds; variable => {d = d.drop(variable); d}}
    val pcaFeaturesDS : DataFrame = arrsnps(arrsnps.length - 1)
    // val pcaFeaturesDS : DataFrame = ds.select("pcaFeatures")
    val featuresDense : RDD[DenseVector] = pcaFeaturesDS.rdd.map(row => row(0).asInstanceOf[DenseVector])
    val features = featuresDense.map{row => row.toArray}
    features.map{row => Vectors.dense(row)}
  }

  def kmeansML(ds: DataFrame, labels: String = "Region", ignoredColumn: String = "SampleId", k: Int) : PipelineModel ={
    val labelIndexer = new StringIndexer()
      .setInputCol(labels)
      .setOutputCol("label")
      .fit(ds)

    val colNames = ds.drop(labels).drop(ignoredColumn).columns
    val assembler = new VectorAssembler()
      .setInputCols(colNames)
      .setOutputCol("features")

    val kmeans = new KMeans()
      .setK(k)
      .setFeaturesCol("features")
      .setPredictionCol("Predict")

    val pipeline = new Pipeline().setStages(Array(labelIndexer, assembler, kmeans))

    val model = pipeline.fit(ds)
    model
  }

  def predict(model: PipelineModel, ds: DataFrame) : DataFrame ={
    val predicted = model.transform(ds)
    predicted
  }

  def gmmKTuning(ds: DataFrame, categoricalVars : Array[String], kSet : Seq[Int], maxIteration: Int = 10, nReapeat: Int = 10): scala.collection.Map[Int, Double] = {
    kSet.map(k => {
      val purityAvg: Double  = (0 until nReapeat).par.map { _ =>
        val split = ds.randomSplit(Array(0.7, 0.3), 1234)
        val trainingSet = split(0)
        val validationSet = split(1)

        val model = gmm(trainingSet, categoricalVars, k, maxIteration)
        val prediction = predict(model, validationSet, categoricalVars)
        purity(prediction.select("Region", "Predict"))
      }.sum / nReapeat
      (k, purityAvg)
    }).sortBy(_._2).toMap
  }

  /* Gaussian Mixture */
  def gmm(ds: DataFrame, categoricalVars : Array[String], K : Int, maxIteration: Int = 10): GaussianMixtureModel ={
    val snpsRDD: RDD[Vector] = dfToRDD(ds, categoricalVars)

    val gmmParameters = new GaussianMixture()
      .setK(K)
      .setMaxIterations(maxIteration)

    val model = gmmParameters.run(snpsRDD)
    model
  }

  def bkmKTuning(ds: DataFrame, categoricalVars : Array[String], kSet : Seq[Int], maxIteration: Int = 10, nReapeat: Int = 10): scala.collection.Map[Int, Double] = {
    kSet.map(k => {
      val purityAvg: Double  = (0 until nReapeat).par.map { _ =>
        val split = ds.randomSplit(Array(0.7, 0.3), 1234)
        val trainingSet = split(0)
        val validationSet = split(1)

        val model = bkm(trainingSet, categoricalVars, k, maxIteration)
        val prediction = predict(model, validationSet, categoricalVars)
        purity(prediction.select("Region", "Predict"))
      }.sum / nReapeat
      (k, purityAvg)
    }).sortBy(_._2).toMap
  }

  /* Bisecting clustering */
  def bkm(ds: DataFrame, categoricalVars : Array[String], K : Int, maxIteration: Int = 10): BisectingKMeansModel = {
    // val infoSampleRegion = ds.select(categoricalVars.head, categoricalVars.tail: _*)
    // val sampleIdRdd : RDD[String] = ds.select("SampleId").rdd.map(row => row.mkString)//toString().substring(1, row.length - 2 ))
    val snpsRDD: RDD[Vector] = dfToRDD(ds, categoricalVars)

    val bkmParameters = new BisectingKMeans()
      .setK(K)
      .setMaxIterations(maxIteration)

    val model = bkmParameters.run(snpsRDD)
    model
  }

  def predict(model: GaussianMixtureModel, ds: DataFrame, categoricalVars : Array[String]) : DataFrame ={
    val sampleIDrdd : RDD[String] = ds.select("SampleId").rdd.map(row => row.mkString)//toString().substring(1, row.length - 2 ))
    val snpsRDD: RDD[Vector] = dfToRDD(ds, categoricalVars)
    val predicted : RDD[(String, Int)] = sampleIDrdd.zip(snpsRDD).map{case(id, snp) => (id, model.predict(snp))}//.map{(id, cluster) => (id, cluster)}
    val clustersDF = sqlContext.createDataFrame(predicted).toDF("SampleId", "Predict")
    ds.join(clustersDF, "SampleId")
  }

  def predict(model: BisectingKMeansModel, ds: DataFrame, categoricalVars : Array[String]) : DataFrame = {
    val sampleIDrdd : RDD[String] = ds.select("SampleId").rdd.map(row => row.mkString)//toString().substring(1, row.length - 2 ))
    val snpsRDD: RDD[Vector] = dfToRDD(ds, categoricalVars)
    val predicted : RDD[(String, Int)] = sampleIDrdd.zip(snpsRDD).map{case(id, snp) => (id, model.predict(snp))} //.map{(id, cluster) => (id, cluster)}
    val clustersDF = sqlContext.createDataFrame(predicted).toDF("SampleId", "Predict")
    ds.join(clustersDF, "SampleId")
  }

  def purity(ds: DataFrame): Double = {
    val sampleCnt = ds.count()
    val labelPredictPair = ds.map(row => (row(1), row(0)))
    val classesCntInCluster = labelPredictPair.map((_, 1L)).reduceByKey(_ + _).map{ case ((k, v), cnt) => (k, (v, cnt))}.groupByKey
    val majorityClassesinCLuster = classesCntInCluster.map{case(_, pairs) => pairs.maxBy(_._2)}
    val correctlyAssignedSum = majorityClassesinCLuster.values.sum()
    return (correctlyAssignedSum / sampleCnt)
  }

/*  private def ri(iter: Iterator[(String, Int)]) : Iterator[Int] ={

  }
  def randIndex(trueLabels: RDD[String], prediction: RDD[Int]): Double = {
    trueLabels.zip(prediction).mapPartitions(ri)

    0
  }*/
}

object Clustering extends Serializable {
  def apply (sc: SparkContext, sqlContext: SQLContext): Clustering = new Clustering (sc, sqlContext)
}

