package com.zsibio

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._
import org.apache.spark.h2o.H2OContext

import _root_.hex.FrameSplitter
import _root_.hex.pca.PCA
import _root_.hex.pca.PCAModel.PCAParameters
import _root_.hex.pca.PCAModel.PCAParameters.Method
import _root_.hex.kmeans.{KMeans, KMeansModel}
import _root_.hex.kmeans.KMeansModel.KMeansParameters

import water.Key
import water.fvec.Frame

object ClusteringH2O {

  def splitDataFrame (df: Frame, ratios: Array[Double]): (Frame, Frame) = {
    val frameSplitter = new FrameSplitter(df, ratios, Array("training", "validation").map(Key.make[Frame](_)), null)
    water.H2O.submitTask(frameSplitter)
    val splits = frameSplitter.getResult
    (splits(0), splits(1))
  }

  def pcaH2O (dfH2O: Frame, nPC: Int = 4, labels: String = "Region", ignoredColumns: Array[String] = Array("SampleId"), pcaMethod: Method = Method.Randomized, ratios: Array[Double] = Array(.7)): Frame = {
    // val dfH2O: Frame = getH2Odf(df, labels)
    var _ratios = ratios

    if (pcaMethod == Method.GLRM) _ratios = Array(.5)
    val (training, validation) = splitDataFrame(dfH2O, _ratios)

    val pcaParameters = new PCAParameters()
    pcaParameters._train = training._key
    // pcaParameters._valid = validation._key
    pcaParameters._response_column = labels
    pcaParameters._ignored_columns = ignoredColumns
    pcaParameters._k = if (nPC != 0) nPC else math.min(training.numRows.toInt, training.numCols - 2 - 1)
    pcaParameters._use_all_factor_levels = true
    pcaParameters._pca_method = pcaMethod
    pcaParameters._max_iterations = 100
    // pcaParameters._transform = DataInfo.TransformType.NORMALIZE

    val pcaObject = new PCA(pcaParameters)
    val pcaModel = pcaObject.trainModel.get
    val prediction = pcaModel.score(dfH2O)
    val pcaDF = prediction

    if (nPC == 0){
      val totalVariancePerc : Double = .5
      val pcaImportance = pcaModel._output._importance
      val pcaCumVariance = pcaImportance.getCellValues.toList(2).toList
      val pcaEigenvectors = pcaModel._output._eigenvectors
      val intPcaCumVariance = pcaCumVariance.map(p => p.get().asInstanceOf[Double])
      val numberPC = intPcaCumVariance.filter(x => x <= totalVariancePerc).size
      pcaDF.remove(Array.range(numberPC, pcaEigenvectors.getColDim))
      pcaDF.update()
    }

    pcaDF.add(Array("SampleId", "Region"), Array(dfH2O.vec("SampleId").toCategoricalVec(), dfH2O.vec("Region").toCategoricalVec()))
    pcaDF.update()
    pcaDF
    // asDataFrame(h2oContext.asH2OFrame(pcaDS))(sqlContext)
  }

  def kmeansH2O(df: Frame, k: Int, labels: String = "Region", ignoredColumns: Array[String] = Array("SampleId")): KMeansModel = {
    val kmeansParameters = new KMeansParameters()
    kmeansParameters._train = df._key
    kmeansParameters._response_column = labels
    kmeansParameters._ignored_columns =  ignoredColumns
    kmeansParameters._k = k

    val kmeans = new KMeans(kmeansParameters)
    val kmeansModel = kmeans.trainModel().get()
    kmeansModel
  }

  def kMeansPredict(model: KMeansModel, df: Frame) : Frame ={
    val predictionDF = model.score(df)
    predictionDF.add("prediction", predictionDF.vec("predict").toCategoricalVec)
    predictionDF.update()
    predictionDF
  }

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setAppName("popgenClusteringWithH2O")

    /*if (args.length < 2) {
       println("There is no input files.")
       println("agrs(0) is a chromosome (ADAM format)")
       println("agrs(1) is a snp panel")
       println("agrs(2) is a number for scaling the data; if nothing then 0")
       exit(1)
     }

     val chr = args(0)
     val snpPanel = args(1)
     val panelFile = "hdfs:///popgen/ALL.panel"
     val dsX = if (args.length == 2) 0 else args(2).toInt
     val dataObj = new LoadData(chr, snpPanel, panelFile, dsX, sc, sqlContext)*/

    if (args.length < 1) {
      println("There is no input files.")
      println("agrs(0) is a chromosome (parquet format)")
      println("agrs(1) is a number of columns to select")
      println("agrs(2) is a number for scaling the data; if nothing then 1")
      exit(1)
    }

    val sc = new SparkContext(conf)
    val sqlContext = new SQLContext(sc)
    val h2oContext = H2OContext.getOrCreate(sc)
    import h2oContext._

    val chr = args(0)
    val dsX = if (args.length == 2) 1 else args(2).toInt
    val nSelectCols = args(1).toInt
    val dataObj = new LoadQCdataFrame(chr, nSelectCols, dsX, sc, sqlContext)

    val timeObj = new Time()

    /*
      PCA + kMeans
     */

    val pcaDF = dataObj.dfExploded//.cache()

    val h2oDF = h2oContext.asH2OFrame(pcaDF)
    h2oDF.replace(h2oDF.find("Region"), h2oDF.vec("Region").toCategoricalVec()).remove()
    h2oDF.update()

    val nPC = 5
    var t0 = System.currentTimeMillis()
    val resultPcaDF: Frame = pcaH2O(h2oDF, nPC, "Region", Array("SampleId"), Method.Randomized, Array(1))
    var t1 = System.currentTimeMillis()
    val pcaTime = timeObj.formatTimeDiff(t0, t1)

    val k = 5
    t0 = System.currentTimeMillis()
    val kmeansModel = kmeansH2O(resultPcaDF, k)
    t1 = System.currentTimeMillis()
    val kmeansTime = timeObj.formatTimeDiff(t0, t1)

    val predictionDF: Frame = kMeansPredict(kmeansModel, resultPcaDF)
    val predictionSqlDF = asDataFrame(h2oContext.asH2OFrame(predictionDF))(sqlContext)

    println(s"PCA H2O time: $pcaTime")
    println(s"kmeans H2O time: $kmeansTime")
  }
}