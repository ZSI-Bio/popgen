package com.zsibio

import org.apache.spark.SparkContext
import org.apache.spark.sql.{DataFrame, Row, SQLContext}
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.linalg.{Vector, Vectors}
import org.apache.spark.ml.feature._
import org.apache.spark.ml.{Pipeline, PipelineModel}
import org.apache.spark.ml.tuning.{CrossValidator, CrossValidatorModel, ParamGridBuilder}
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator
import org.apache.spark.ml.classification.DecisionTreeClassifier
import org.apache.spark.ml.classification.RandomForestClassifier
import org.apache.spark.mllib.evaluation.{MulticlassMetrics, MultilabelMetrics}
import org.apache.sysml.api.ml.SVM


class Classification (sc: SparkContext, sqlContext: SQLContext) extends Serializable {

  var _prediction : DataFrame = null
  var _error : Double = Double.NaN
  var _trainingError : Double = Double.NaN
  var _testingError : Double = Double.NaN
  var _evaluator : MulticlassClassificationEvaluator = null
  var model : PipelineModel = null
  var _precisionByLabel : List[Double] = Nil
  var _recallByLabel : List[Double] = Nil
  var _fScoreByLabel : List[Double] = Nil

  private def transformDF(ds: DataFrame, labels: String) : DataFrame ={
    val classVar : RDD[String] = ds.select(labels).rdd.map(x => x.mkString) // Vectors.dense(x.toSeq.toArray.map(x => x.asInstanceOf[Double])))
    val rddX : RDD[Vector] = ds.drop(labels).rdd.map(row => Vectors.dense(row.toSeq.toArray.map(x => x.asInstanceOf[Double]))).cache()
    val transformedDS : DataFrame = sqlContext.createDataFrame(classVar.zip(rddX)).toDF("label", "features")
    transformedDS
  }

  def svm(ds: DataFrame, labels: String = "Region", cv: Boolean = true, nFolds: Int = 10,  cost: Array[Double] = Array(0.01, 0.1, 10), icpt: Array[Int] = Array(0), tol : Array[Double] = Array(0.01), maxIter: Array[Int]= Array(10)) : PipelineModel ={
    val labelIndexer = new StringIndexer()
      .setInputCol(labels)
      .setOutputCol("label")
      .fit(ds)

    val colNames = ds.drop(labels).columns
    val assembler = new VectorAssembler()
      .setInputCols(colNames)
      .setOutputCol("features")

    val svm = new SVM("svm", sc, isMultiClass = true)

    val paramGrid = new ParamGridBuilder()
      .addGrid(svm.regParam, cost)
      .addGrid(svm.icpt, icpt)
      .addGrid(svm.maxOuterIter, maxIter)
      .addGrid(svm.tol, tol).build() // No parameter search

    val labelConverter = new IndexToString()
      .setInputCol("prediction")
      .setOutputCol("predictedLabel")
      .setLabels(labelIndexer.labels)

    _evaluator = new MulticlassClassificationEvaluator()
      .setLabelCol("label")
      .setPredictionCol("prediction")
      // "f1", "precision", "recall", "weightedPrecision", "weightedRecall"
      //.setMetricName("scores")

    val pipeline = new Pipeline().setStages(Array(labelIndexer, assembler, svm))//, labelConverter))

    if (cv == true){
      val crossValidator = new CrossValidator()
        .setEstimator(pipeline)
        .setEvaluator(_evaluator)
        .setEstimatorParamMaps(paramGrid)
        .setNumFolds(nFolds)
      model = crossValidator.fit(ds).bestModel.asInstanceOf[PipelineModel]
    }
    else model = pipeline.fit(ds)

    model
  }

  def decisionTrees(ds: DataFrame, labels: String = "Region", cv: Boolean = true, nFolds: Int = 10, bins: Array[Int] = Array(10, 15, 20), impurity: Array[String] = Array("entropy", "gini"), depth: Array[Int] = Array(4, 6, 8)) : PipelineModel ={
    val labelIndexer = new StringIndexer()
      .setInputCol(labels)
      .setOutputCol("label")
      .fit(ds)

    val colNames = ds.drop(labels).columns
    val assembler = new VectorAssembler()
      .setInputCols(colNames)
      .setOutputCol("features")

    val dt = new DecisionTreeClassifier()
      .setLabelCol("label")
      .setFeaturesCol("features")

    val labelConverter = new IndexToString()
      .setInputCol("prediction")
      .setOutputCol("predictedLabel")
      .setLabels(labelIndexer.labels)

    val pipeline = new Pipeline()
      .setStages(Array(labelIndexer, assembler, dt, labelConverter))

    val paramGrid = new ParamGridBuilder()
      .addGrid(dt.maxBins, bins)
      .addGrid(dt.maxDepth, depth)
      .addGrid(dt.impurity, impurity)
      .build()

    _evaluator = new MulticlassClassificationEvaluator()
      .setLabelCol("label")
      .setPredictionCol("prediction")

    if (cv == true) {
      val crossValidator = new CrossValidator()
        .setEstimator(pipeline)
        .setEvaluator(_evaluator)
        .setEstimatorParamMaps(paramGrid)
        .setNumFolds(nFolds)
      model = crossValidator.fit(ds).bestModel.asInstanceOf[PipelineModel]
    }
    else model = pipeline.fit(ds)

    model
  }

  def randomForest (ds: DataFrame, labels: String = "Region", cv: Boolean = true, nFolds: Int = 10, bins: Array[Int] = Array(10, 15, 20), impurity: Array[String] = Array("entropy", "gini"), depth: Array[Int] = Array(4, 6, 8)) : PipelineModel ={
    val labelIndexer = new StringIndexer()
      .setInputCol(labels)
      .setOutputCol("label")
      .fit(ds)

    val colNames = ds.drop(labels).columns
    val assembler = new VectorAssembler()
      .setInputCols(colNames)
      .setOutputCol("features")

    val rf = new RandomForestClassifier()
      .setLabelCol("label")
      .setFeaturesCol("features")

    val labelConverter = new IndexToString()
      .setInputCol("prediction")
      .setOutputCol("predictedLabel")
      .setLabels(labelIndexer.labels)

    val pipeline = new Pipeline()
      .setStages(Array(labelIndexer, assembler, rf, labelConverter))

    val paramGrid = new ParamGridBuilder()
      .addGrid(rf.maxBins, bins)
      .addGrid(rf.maxDepth, depth)
      .addGrid(rf.impurity, impurity)
      .build()

    _evaluator = new MulticlassClassificationEvaluator()
      .setLabelCol("label")
      .setPredictionCol("prediction")

    if (cv == true) {
      val crossValidator = new CrossValidator()
        .setEstimator(pipeline)
        .setEvaluator(_evaluator)
        .setEstimatorParamMaps(paramGrid)
        .setNumFolds(nFolds)
      model = crossValidator.fit(ds).bestModel.asInstanceOf[PipelineModel]
    }
    else model = pipeline.fit(ds)

    model
  }

  def predict(model: CrossValidatorModel, ds: DataFrame, labels: String = "Region") : Unit = {
    _prediction = model.transform(ds).drop("features")
    _error = 1 - _evaluator.evaluate(_prediction)
    getMetrics(_prediction)
  }

  def predict(model: PipelineModel, ds: DataFrame) : Unit = {
    _prediction = model.transform(ds).drop("features")
    _error = 1 - _evaluator.evaluate(_prediction)
    getMetrics(_prediction)
  }

  def getMetrics(ds: DataFrame) : Unit ={
    val prediction = ds.select("prediction").rdd.map({case Row(pred: Double) => pred})//map(row => row.getAs[Double]("prediction"))
    val trueLabels = ds.select("label").rdd.map({case Row(lab: Double) => lab})//map(row => row.getAs[Double]("label"))
    val predictionAndLabels = prediction.zip(trueLabels)
    val metrics = new MulticlassMetrics(predictionAndLabels)

    _precisionByLabel = metrics.labels.map(label => metrics.precision(label)).toList
    _recallByLabel = metrics.labels.map(label => metrics.recall(label)).toList
    _fScoreByLabel = metrics.labels.map(label => metrics.fMeasure(label)).toList
  }
}
