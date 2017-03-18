package com.zsibio.popgen

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._

import org.bdgenomics.formats.avro._

import scala.collection.mutable.WrappedArray

class LoadData (val chr: String, val snpPanel: String, val panelFile: String, val dsX : Int = 0, val sc: SparkContext, val sqlContext: SQLContext) extends Serializable{

  val  ds = sqlContext.read.parquet(chr).repartition(260)
  ds.registerTempTable("gts")

  val variantsDF = sqlContext.read .format("com.databricks.spark.csv").option("header", "true").option("delimiter", ",").load(snpPanel).repartition(100)
  variantsDF.registerTempTable("variants")

  private val genotypesDF = sqlContext.sql(
    s"""
        select
          sampleId,
          concat(variant.contig.contigName, ':', variant.start) as variantId,
          sum(case when alleles[0] = 'Alt' and alleles[1] = 'Alt' then 2 when alleles[0] = 'Alt' or alleles[1] = 'Alt' then 1  else 0 end) as altCount
          from gts g, variants v  where
          g.variant.contig.contigName = v.contigName and g.variant.start = v.start and
            (alleles[0] <> 'OtherAlt' or alleles[1] <> 'OtherAlt')
          group by sampleId, variant.contig.contigName, variant.start
      """
  ).repartition(400)

  private val sampleToData : RDD[(String, (String, Int))]= genotypesDF.map({case Row(sampleId : String, variantId : String, count : Long) => (sampleId, (variantId, count.toInt))})
  private val groupedSampleData : RDD[(String, Iterable[(String, Int)])] = sampleToData.groupByKey()
  private val variantsRDD : RDD[(String, Array[(String, Int)])] = groupedSampleData.mapValues(it => it.toArray.sortBy(_._1))

  def extract(file: String, superPop: String = "super_pop", filter: (String, String) => Boolean) = {
    sc.textFile(file).map(line => {
      val tokens = line.split("\t").toList
      if (superPop == "pop") {
        tokens(0) -> tokens(1)
      } else {
        tokens(0) -> tokens(2)
      }
    }
    ).collectAsMap.filter(tuple => filter(tuple._1, tuple._2))
  }

  val panel = extract(panelFile, "super_pop", (sampleID: String, pop: String) => Array("AFR", "EUR", "AMR", "EAS", "SAS").contains(pop))

  def header = StructType(
    Array(StructField("SampleId", StringType)) ++
      Array(StructField("Region", StringType)) ++
      variantsRDD.first()._2.map(variant => {
        StructField(variant._1, DoubleType)
      }))

  def rowRDD: RDD[Row] = variantsRDD.map {
    case (sampleId, variants) =>
      val region: Array[String] = Array(panel.getOrElse(sampleId, "Unknown"))
      val alternateCounts: Array[Double] = variants.map(_._2.toDouble)
      Row.fromSeq(Array(sampleId) ++ region ++ alternateCounts)
  }

  def df = sqlContext.createDataFrame(rowRDD, header)

  def dfExploded = df.withColumn("tmp", explode(array((0 until dsX).map(lit): _*))).drop("tmp")
}