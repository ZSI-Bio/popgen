package com.zsibio

/**
  * Created by anastasiia on 1/18/17.
  */

import org.apache.spark.{SparkContext}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.bdgenomics.formats.avro.{Genotype, GenotypeAllele}
import scala.collection.JavaConverters._

@SerialVersionUID(15L)
class PopulationGe (sc: SparkContext, sqlContext: SQLContext, genotypes: RDD[Genotype], panel: scala.collection.Map[String, String],
                    missingRate: Double = 0.0, var infFreq: Double = 0.05, var supFreq: Double = 1.0) extends Serializable{

  val time = new Time()
  private var _infFreq     : Double = infFreq
  private var _supFreq     : Double = supFreq
  private val _missingRate : Double = missingRate
  var _dataSet: DataFrame = null
  var _numberOfRegions : Int = 0
  //var freq : RDD[(String, Int, Double)] = inputVarFreq

  def infFreq_ (value: Double): Unit = _infFreq = value
  def supFreq_ (value: Double): Unit = _supFreq = value

  def variantId(genotype: Genotype): String = {
    val name = genotype.getVariant.getContig.getContigName
    val start = genotype.getVariant.getStart
    val end = genotype.getVariant.getEnd
    s"$name:$start:$end"
  }

  def altCount(genotype: Genotype): Int = {
    genotype.getAlleles.asScala.count(_ != GenotypeAllele.Ref)
  }
  val sampleCount = genotypes.map(_.getSampleId).distinct.count.toInt

  def getVariantsDF() : DataFrame={
    val variantsById : RDD[(String, Iterable[Genotype])] = genotypes.keyBy(g => variantId(g)).groupByKey.cache
    val variantsSize :  RDD[(String, Int)] = genotypes.map(g => (variantId(g), g.getAlleles.size)).reduceByKey((x,y) => x + y)
    //val variantsSize :  RDD[(String, Int)] = variantsById.map{ case (k, it) => (k, it.size)}
    val variantsFreq : RDD[(String, Int)] = genotypes.map(g => (variantId(g), altCount(g))).reduceByKey((x, y) => x + y)

    val variantsSizeDF = sqlContext.createDataFrame(variantsSize).toDF("VariantId", "VariantSize")
    val variantsFreqDF = sqlContext.createDataFrame(variantsFreq).toDF("VariantId", "VariantFrequencies")
    val variantsDF = variantsSizeDF.join(variantsFreqDF, "VariantId")

    variantsDF
  }

  def getSortedSampleData (variantsDF: DataFrame) : RDD[(String, Array[(String, Int)])] = {
    val filteredVariantsDF = variantsDF.filter(variantsDF("VariantSize") >= (sampleCount * (1 - _missingRate))
      && variantsDF("VariantFrequencies") >= (_infFreq * sampleCount) && variantsDF("VariantFrequencies") <= (_supFreq * sampleCount)).select("VariantId").cache()

    val filteredVariantsId : Array[String] = filteredVariantsDF.rdd.map(row => row.mkString).collect
    val finalGts = genotypes.filter{g => filteredVariantsId contains variantId(g)}

    val sampleToData : RDD[(String, (String, Int))] = finalGts.map { g => (g.getSampleId, (variantId(g), altCount(g))) }.distinct
    val groupedSampleData : RDD[(String, Iterable[(String, Int)])] = sampleToData.groupByKey()
    val sortedSampleData : RDD[(String, Array[(String, Int)])] = groupedSampleData.mapValues(it => it.toArray.sortBy(_._1)).cache()
    sortedSampleData
  }

/*  def getDataSet(sortedVariantsBySampleId: RDD[(String, Array[(String, Int)])], prunnedSnpIdSet: List[String] = null) : DataFrame ={
    val header = StructType(
      Array(StructField("SampleId", StringType)) ++
        Array(StructField("Region", StringType)) ++
        sortedVariantsBySampleId.keys.collect.map(variant => StructField(variant, DoubleType))
    )

    val variants = sortedVariantsBySampleId.keys.collect
    val samples = sortedVariantsBySampleId.first._1
    val alternateCounts = sortedVariantsBySampleId.

    val rowRDD: RDD[Row] = sortedVariantsBySampleId.map {
      case (variantId, sortedVariants) =>
        val region: Array[String] = sortedVariants.map{case(sampleId, _) => panel.getOrElse(sampleId, "Unknown")}
        val alternateCounts: Array[Double] = sortedVariants.map(_._2.toDouble)
        Row.fromSeq(sortedVariants.map(_._1) ++ region ++ alternateCounts)
    }

    val dataSet: DataFrame = sqlContext.createDataFrame(rowRDD, header)//.toDF(header.fieldNames : _*)
    var outputDataSet: DataFrame = null

    if (prunnedSnpIdSet == null) outputDataSet = dataSet
    else{
      val columnsToSelect: List[String] = List("SampleId", "Region") ++ prunnedSnpIdSet
      outputDataSet = dataSet.select(columnsToSelect.head, columnsToSelect.tail: _*)
    }

    _dataSet = outputDataSet
    _numberOfRegions = outputDataSet.select("Region").distinct().count().toInt


    return outputDataSet
  }*/

  def getDataSet(sortedVariantsBySampleId: RDD[(String, Array[(String, Int)])], prunnedSnpIdSet: List[String] = null) : DataFrame ={
    val header = StructType(
      Array(StructField("SampleId", StringType)) ++
        Array(StructField("Region", StringType)) ++
        sortedVariantsBySampleId.first()._2.map(variant => {
          StructField(variant._1, DoubleType)
        }))

    val rowRDD: RDD[Row] = sortedVariantsBySampleId.map {
      case (sampleId, sortedVariants) =>
        val region: Array[String] = Array(panel.getOrElse(sampleId, "Unknown"))
        val alternateCounts: Array[Double] = sortedVariants.map(_._2.toDouble)
        Row.fromSeq(Array(sampleId) ++ region ++ alternateCounts)
    }

    val dataSet: DataFrame = sqlContext.createDataFrame(rowRDD, header)//.toDF(header.fieldNames : _*)
    var outputDataSet: DataFrame = null

    if (prunnedSnpIdSet == null) outputDataSet = dataSet
    else{
      val columnsToSelect: List[String] = List("SampleId", "Region") ++ prunnedSnpIdSet
      outputDataSet = dataSet.select(columnsToSelect.head, columnsToSelect.tail: _*)
    }

    _dataSet = outputDataSet
    _numberOfRegions = outputDataSet.select("Region").distinct().count().toInt

    return outputDataSet
  }

  def getDataSet(variants: RDD[(String, Array[Double])]) : DataFrame ={
    val pc = Array.fill(variants.first()._2.length)("PC")
    val s = (1 until (variants.first()._2.length + 1))
    val headerPC = (pc, s).zipped.par.map{case(pc, s) => pc + s}

    val header = StructType(
      Array(StructField("SampleId", StringType)) ++
        Array(StructField("Region", StringType)) ++
        headerPC.map(pc => {StructField(pc, DoubleType)})
    )

    val rowRDD: RDD[Row] = variants.map {
      case (sampleId, variant) =>
        val region: Array[String] = Array(panel.getOrElse(sampleId, "Unknown"))
        Row.fromSeq(Array(sampleId) ++ region ++ variant)
    }

    val outputDataSet: DataFrame = sqlContext.createDataFrame(rowRDD, header)//.toDF(header.fieldNames : _*)

    _dataSet = outputDataSet
    _numberOfRegions = outputDataSet.select("Region").distinct().count().toInt

    return outputDataSet
  }

}
