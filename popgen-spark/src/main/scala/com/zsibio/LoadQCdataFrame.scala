package com.zsibio

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._

import org.bdgenomics.formats.avro._

import scala.collection.mutable.WrappedArray

class LoadQCdataFrame (val chr: String, var nSelectCols : Int = 0, val dsX : Int = 0, val sc: SparkContext, val sqlContext: SQLContext) extends Serializable{

  val df = sqlContext.read.parquet(chr).repartition(200)
  var cols = Array("")
  val ncols = df.columns.length
  if (ncols >= nSelectCols)
    cols = df.columns.take(ncols)
  else cols = df.columns.take(nSelectCols)

  val reducedDF = df.select(cols.head, cols.tail: _*)

  def dfExploded = reducedDF.withColumn("tmp", explode(array((0 until dsX).map(lit): _*))).drop("tmp")
}