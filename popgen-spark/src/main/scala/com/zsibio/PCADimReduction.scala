package com.zsibio

/**
  * Created by anastasiia on 1/15/17.
  */

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.sql._
import org.apache.spark.sql.{DataFrame, SQLContext}
import org.apache.spark.ml.feature._
import org.apache.spark.ml.Pipeline
import org.apache.spark.mllib.linalg._
import org.apache.spark.mllib.linalg.distributed.{IndexedRow, IndexedRowMatrix, RowMatrix}
import org.apache.spark.ml.feature.VectorSlicer
import breeze.linalg.{DenseMatrix => BDM, DenseVector => BDV, svd => brzSvd}
import breeze.linalg.accumulate
import java.util.Arrays


class PCADimReduction (sc: SparkContext, sqlContext: SQLContext) extends Serializable {

  def pcaML(ds: DataFrame, nPC: Int, labels: String = "Region", varianceTreshold : Double = 0.7, outputCol : String) : DataFrame ={
    val colNames = ds.drop(labels).drop("SampleId").columns
    val assembler = new VectorAssembler()
      .setInputCols(colNames)
      .setOutputCol("features")

    val standardizer = new StandardScaler()
      .setInputCol("features")
      .setOutputCol("normFeatures")
      .setWithMean(false)
      .setWithStd(true)

    val pca = new PCA()
      .setInputCol("normFeatures")
      .setOutputCol("allPcaFeatures")
      .setK(nPC)

    val pipeline = new Pipeline()
      .setStages(Array(assembler, standardizer, pca))

    val model = pipeline.fit(ds)
    val pcaFeaturesDF = model.transform(ds).select("Region", "SampleId", "allPcaFeatures")

    val finalNumPC = explainedVariance(pcaFeaturesDF, nPC, varianceTreshold, "allPcaFeatures")
    println(s"Final number of the PCs: $finalNumPC")
    val slicer = new VectorSlicer()
      .setInputCol("allPcaFeatures")
      .setOutputCol("pcaFeatures")
      .setIndices(Array.range(0, finalNumPC))

    val pcaDF = slicer.transform(pcaFeaturesDF)
    pcaDF
  }

  def explainedVariance(pcaResultDS: DataFrame, nPC: Int = 20, varianceTreshold : Double = 0.7, inputCol : String) : Int ={
    val pcaFeaturesDS : DataFrame = pcaResultDS.select(inputCol)
    val featuresDense : RDD[DenseVector] = pcaFeaturesDS.rdd.map(row => row(0).asInstanceOf[DenseVector])
    // val featuresVector : RDD[IndexedRow] = featuresDense.map{row => Vectors.dense(row.toArray)}.zipWithIndex().map{case(v, idx) => IndexedRow(idx, v)}
    // val featuresMat : IndexedRowMatrix = new IndexedRowMatrix(featuresVector)
    // val svd: SingularValueDecomposition[IndexedRowMatrix, Matrix] = featuresMat.computeSVD(20, computeU = false)

    val features : RDD[Array[Double]] = featuresDense.map{_.toArray}
    val featuresVector : RDD[Vector] = features.map{row => Vectors.dense(row)}
    // val featuresVector : RDD[Vector] = pcaFeaturesDS.rdd.map({case Row(row: Array[Double]) => Vectors.dense(row(0))})
    val featuresMat : RowMatrix = new RowMatrix(featuresVector)
    val svd: SingularValueDecomposition[RowMatrix, Matrix] = featuresMat.computeSVD(nPC, computeU = false)
    val eigenValues = svd.s.toArray

    val variance = eigenValues.map(value => value / eigenValues.sum)
    variance.foreach(println)
    val cumVariance = variance.map{var x : Double = 0; value => x += value; x}
    val finalNumPC = cumVariance.filter(x => x <= varianceTreshold).size
    return finalNumPC
  }

}
