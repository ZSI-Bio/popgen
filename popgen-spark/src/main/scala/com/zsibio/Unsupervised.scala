package com.zsibio

import hex.{DataInfo, FrameSplitter}
import hex.kmeans.{KMeans, KMeansModel}
import hex.kmeans.KMeansModel.KMeansParameters
import hex.pca.PCA
import hex.pca.PCAModel.PCAParameters
import hex.pca.PCAModel.PCAParameters.Method
import org.apache.spark.SparkContext
import org.apache.spark.h2o.H2OContext
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{DataFrame, SQLContext}
import water.Key
import water.fvec.Frame


case class OutputParameters(dimentionalityRedMethod: String, clusterMethod: String, trainingFrame: DataFrame, SSW: Double, SSB: Double){
  def this(clusterMethod: String) = this(clusterMethod, null, null, 0, 0)
  def this() = this(null, null, null, 0, 0)
}

trait UnsupervisedMethods {
  protected def getH2ODataFrame (schemaRDD: DataFrame) : Frame
  def splitDataFrame (dataSet: Frame, ratios: Array[Double]): (Frame, Frame)
  def splitDataFrame (dataSet: DataFrame, ratios: Array[Double]): (Frame, Frame)
  def pcaH2O (schemaRDD: DataFrame, pcaMethod: Method, ratios: Array[Double]): DataFrame
  def kMeansH2O(dataSet: Frame, numberClusters: Int, responseColumn: String, ignoredColumns: Array[String]): KMeansModel
  def kMeansH2O(dataSet: DataFrame, numberClusters: Int, responseColumn: String, ignoredColumns: Array[String]): KMeansModel
  def kMeansPredict(model: KMeansModel, ds: DataFrame) : DataFrame
  def kMeansTuning(ds: DataFrame, responseColumn: String, ignoredColumns: Array[String], kSet: Seq[Int], nReapeat: Int): scala.collection.Map[Int, Double]
}

@SerialVersionUID(15L)
class Unsupervised[T] (sc: SparkContext, sqlContext: SQLContext) extends Serializable with UnsupervisedMethods{

  // private var _schemaRDD : DataFrame = schemaRDD
  // def schemaRDD_ (value: DataFrame): Unit = _schemaRDD = value

  private var _dimensionalityRedMethod : String = ""
  private var _clusteringMethod : String = ""
  private var _trainingFrame : DataFrame = null
  private var _ssw : Double = 0
  private var _ssb : Double = 0

  val h2oContext = H2OContext.getOrCreate(sc)
  import h2oContext._

  def getH2ODataFrame (schemaRDD: DataFrame) : water.fvec.Frame = {
    val dataFrame = asH2OFrame(schemaRDD)
    dataFrame.replace(dataFrame.find("Region"), dataFrame.vec("Region").toCategoricalVec()).remove()
    dataFrame.update()
  }

  def splitDataFrame (dataSet: Frame, ratios: Array[Double]) : (Frame, Frame) ={
    val frameSplitter = new FrameSplitter(dataSet, ratios, Array("training", "validation").map(Key.make[Frame](_)), null)
    water.H2O.submitTask(frameSplitter)
    val splits = frameSplitter.getResult
    (splits(0), splits(1))
  }

  def splitDataFrame (schemaRDD: DataFrame, ratios: Array[Double]) : (Frame, Frame) = {
    val dataSet : Frame = getH2ODataFrame(schemaRDD)
    splitDataFrame(dataSet, ratios)
  }

  def pcaH2O (schemaRDD: DataFrame, pcaMethod: Method = Method.GramSVD, ratios: Array[Double] = Array(.7)): DataFrame = {
    _dimensionalityRedMethod = "PCA_" + pcaMethod.toString
    val dataSet : Frame = getH2ODataFrame(schemaRDD)

    var _ratios : Array[Double] = ratios
    if (pcaMethod == Method.GLRM) _ratios = Array(.5)
    val (training, validation) = splitDataFrame(dataSet, _ratios)

    val nFeatures = training.numCols() - 2 // remove SampleId and Region
    val nObservations = training.numRows().toInt

    println(s"nObservations = $nObservations, nFeatures = $nFeatures")

    val pcaParameters = new PCAParameters()
    pcaParameters._train = training._key
    // pcaParameters._valid = validation._key
    pcaParameters._response_column = "Region"
    pcaParameters._ignored_columns = Array("SampleId")
    pcaParameters._k = 20// math.min(nFeatures, nObservations)
    pcaParameters._use_all_factor_levels = true
    pcaParameters._pca_method = pcaMethod
    pcaParameters._max_iterations = 100
//    pcaParameters._transform = DataInfo.TransformType.NORMALIZE


    val pcaObject = new PCA(pcaParameters)
	println ("PCA train")
    val pcaModel = pcaObject.trainModel.get

    val pcaImportance = pcaModel._output._importance
    println(pcaImportance)
    val pcaCumVariance = pcaImportance.getCellValues.toList(2).toList
    val pcaEigenvectors = pcaModel._output._eigenvectors

    val totalVariancePerc : Double = .6

    val intPcaCumVariance = pcaCumVariance.map(p => p.get().asInstanceOf[Double])
    val numberPC = intPcaCumVariance.filter(x => x <= totalVariancePerc).size

    val prediction = pcaModel.score(dataSet)
    val pcaDS = prediction

    pcaDS.remove(Array.range(numberPC, pcaEigenvectors.getColDim))

    pcaDS.update()

    pcaDS.add(Array("SampleId", "Region"), Array(dataSet.vec("SampleId").toCategoricalVec(), dataSet.vec("Region").toCategoricalVec()))
    pcaDS.update()

    asDataFrame(h2oContext.asH2OFrame(pcaDS))(sqlContext)
  }

  def kMeansH2O(dataSet: Frame, numberClusters: Int, responseColumn: String, ignoredColumns: Array[String]): KMeansModel = {
    val kmeansParameters = new KMeansParameters()
    kmeansParameters._train = dataSet._key
    kmeansParameters._response_column = "Region"
    kmeansParameters._ignored_columns = Array("SampleId")
    kmeansParameters._k = numberClusters

    val kmeans = new KMeans(kmeansParameters)
    val kmeansModel = kmeans.trainModel().get()
    kmeansModel
  }

  def kMeansH2O(dataSet: DataFrame, numberClusters: Int, responseColumn: String = "Region", ignoredColumns: Array[String] = Array("SampleId")): KMeansModel = {
    val dataSetH2O : Frame = getH2ODataFrame(dataSet)
    kMeansH2O(dataSetH2O, numberClusters, responseColumn, ignoredColumns)
  }

  def kMeansPredict(model: KMeansModel, ds: DataFrame) : DataFrame ={
    val dsH2O : Frame = getH2ODataFrame(ds)
    val kmeansPrediction = model.score(dsH2O)

    val predicted = dsH2O
    predicted.add("Predict", kmeansPrediction.vec("predict").toCategoricalVec)
    predicted.update()

    _trainingFrame = asDataFrame(h2oContext.asH2OFrame(predicted))(sqlContext)

    return _trainingFrame

  }

  def kMeansTuning(ds: DataFrame, responseColumn: String, ignoredColumns: Array[String], kSet: Seq[Int], nReapeat: Int = 10): scala.collection.Map[Int, Double] = {
    kSet.map(k => {
      val purityAvg: Double  = (0 until nReapeat).par.map { _ =>
        val split = ds.randomSplit(Array(.7, 0.3), 1234)
        val trainingSet = split(0)
        val validationSet = split(1)

        val model = kMeansH2O(trainingSet, k, responseColumn, ignoredColumns)
        val prediction = kMeansPredict(model, validationSet)

        Clustering(sc, sqlContext).purity(prediction.select("Region", "Predict"))
      }.sum / nReapeat
      (k, purityAvg)
    }).sortBy(_._2).toMap
  }


  def getOutputParameters : OutputParameters ={
    new OutputParameters(_dimensionalityRedMethod, _clusteringMethod, _trainingFrame, _ssw,  _ssb)
  }

}

object Unsupervised extends Serializable{
  def apply[T](sc: SparkContext, sqlContext: SQLContext): Unsupervised[T] = new Unsupervised[T](sc, sqlContext)
}
