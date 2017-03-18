package com.zsibio

import java.io.File

import org.apache.spark.broadcast.Broadcast
import org.apache.spark.ml.PipelineModel
import org.apache.spark.ml.classification.{DecisionTreeClassificationModel, RandomForestClassificationModel}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import org.bdgenomics.adam.rdd.ADAMContext._
import org.bdgenomics.formats.avro._
import org.apache.spark.sql.{DataFrame, Row, SQLContext}
import org.apache.spark.sql.hive.HiveContext
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._

import scala.util.Random
import scala.collection.JavaConverters._
import scala.collection.mutable.WrappedArray

case class Genotypes(id: String, genotype: Int)
case class TSNP(snpIdx: Long, snpId: String, snpPos: Int, snp: Vector[Int]){}

case class ClassificationMetrics(error: Double, precisionByLabels: List[Double], recallByLabels: List[Double], fScoreByLabels: List[Double])

class Parameters(
                  chrFile : String,
                  panelFile : String,
                  pop : String,
                  popSet : Array[String],
                  transformWithR : Boolean,
                  missingRate : Double,
                  isFrequencies : Boolean,
                  infFreq : Double,
                  supFreq : Double,
                  isLDPruning : Boolean,
                  ldMethod : String,
                  ldTreshold : Double,
                  ldMaxBp : Int,
                  ldMaxN : Int,
                  isOutliers : Boolean,
                  isDimRed : Boolean,
                  dimRedMethod : String,
                  pcaMethod : String,
                  mdsMethod : String,
                  isClustering : Boolean,
                  isClassification : Boolean,
                  clusterMethod : String,
                  classificationMethod : String,
                  cvClassification : Boolean,
                  cvClustering : Boolean,
                  nRepeatClassification : Int,
                  nRepeatClustering : Int,
                  clusterFileOutput: String,
                  classTrainOutput : String,
                  classTestOutput : String,
                  chrFreqFile : String,
                  chrFreqFileOutput : String
                ){
  val _chrFile = chrFile
  val _panelFile = panelFile
  val _pop = pop
  val _popSet = popSet
  val _missingRate = missingRate
  val _isFrequencies : Boolean = isFrequencies
  var _infFreq : Double = infFreq
  var _supFreq : Double = supFreq
  val _isLDPruning : Boolean = isLDPruning
  val _ldMethod : String = ldMethod
  val _ldTreshold : Double = ldTreshold
  val _ldMaxBp : Int = ldMaxBp
  val _ldMaxN : Int = ldMaxN
  val _isOutliers : Boolean = isOutliers
  val _isDimRed : Boolean = isDimRed
  val _dimRedMethod : String =dimRedMethod
  val _pcaMethod : String = pcaMethod
  val _mdsMethod : String = mdsMethod
  val _isClustering : Boolean = isClustering
  val _isClassification : Boolean = isClassification
  val _clusterMethod : String = clusterMethod
  val _classificationMethod : String = classificationMethod
  val _cvClassification : Boolean = cvClassification
  val _cvClustering : Boolean = cvClustering
  val _nRepeatClassification : Int = nRepeatClassification
  val _nRepeatClustering : Int = nRepeatClustering
  val _chrFreqFile : String = chrFreqFile
  val _chrFreqFileOutput : String = chrFreqFileOutput
  val _classTestOutput : String = classTestOutput
  val _classTrainOutput : String = classTrainOutput
  val _clusterFileOutput : String = clusterFileOutput
  val _tranformWithR : Boolean = transformWithR
}

object PopulationStratification {

  implicit class RDDOps[T](rdd: RDD[Seq[T]]) {
    def transpose(): RDD[Seq[T]] = {
      rdd.zipWithIndex.flatMap {
        case (row, rowIndex) => row.zipWithIndex.map {
          case (number, columnIndex) => columnIndex -> (rowIndex, number)
        }
      }.groupByKey.sortByKey().values.map {
        indexedRow => indexedRow.toSeq.sortBy(_._1).map(_._2)
      }
    }
  }

  implicit class dfWriter(dataSet: DataFrame) {
    def writeTo(filename: String, isHeader: String = "true"): Unit = {
      val tmpParquetDir = "Posts.tmp.parquet"
      dataSet.write
        .format("com.databricks.spark.csv")
        .option("header", isHeader)
        .save(tmpParquetDir)

      val dir = new File(tmpParquetDir)
      val tmpTsvFile = tmpParquetDir + File.separatorChar + "part-00000"
      (new File(tmpTsvFile)).renameTo(new File(filename))
      dir.listFiles.foreach( f => f.delete )
      dir.delete
    }

    def writeToCsv(filename: String, isHeader: String = "true"): Unit = {
      dataSet.write
        .format("com.databricks.spark.csv")
        .option("header", isHeader)
        .save(filename)
    }
  }

  implicit class RDDPart[T](rdd: RDD[T]) {
    def partitionBy(f: T => Boolean): (RDD[T], RDD[T]) = {
      val passes = rdd.filter(f)
      val fails = rdd.filter(e => !f(e))
      (passes, fails)
    }
  }

  def computeAvgMetrics (classificationMetrics: List[ClassificationMetrics], isPrint: Boolean = true) : Unit ={
    val avgError = classificationMetrics.map(metrics => metrics.error).sum / classificationMetrics.size
    val avgPrecisionByLabel = classificationMetrics.map(metrics => metrics.precisionByLabels).transpose.map(precisions => precisions.sum / precisions.size)
    val avgRecallByLabel = classificationMetrics.map(metrics => metrics.recallByLabels).transpose.map(recalls => recalls.sum / recalls.size)
    val avgFScoreByLabel = classificationMetrics.map(metrics => metrics.fScoreByLabels).transpose.map(fScores => fScores.sum / fScores.size)

    if(isPrint){
      println(s"Average error: $avgError")
      println(s"Average precision for each label:"); avgPrecisionByLabel.foreach{precision => print(s" $precision")}
      println(s"\nAverage recall for each label:"); avgRecallByLabel.foreach(recall => print(s" $recall"))
      println(s"\nAverage F-Measure for each label:"); avgFScoreByLabel.foreach(f => print(s" $f"))
    }
  }

  def variantId(genotype: Genotype): String = {
    val name = genotype.getVariant.getContig.getContigName
    val start = genotype.getVariant.getStart
    val end = genotype.getVariant.getEnd
    s"$name:$start:$end"
  }

  def altCount(genotype: Genotype): Int = {
    genotype.getAlleles.asScala.count(_ != GenotypeAllele.Ref)
  }

  def prun(rdd: RDD[(String, Array[(String, Int)])], snpIdSet: List[String]): RDD[(String, Array[(String, Int)])] =
    rdd.map{ case (sample, sortedVariants) => (sample, sortedVariants.filter(varinat => snpIdSet contains (varinat._1)))}

  def main(args: Array[String]): Unit = {

    val conf = new SparkConf()
      .setAppName("PopStrat")
      .set("spark.ext.h2o.repl.enabled", "false")
      //.set("spark.ext.h2o.topology.change.listener.enabled", "false")
      .set("spark.driver.maxResultSize", "0")
    val sc = new SparkContext(conf)
   // val sqlContext = new SQLContext(sc)
    val sqlContext: SQLContext = new HiveContext(sc) 
    import sqlContext.implicits._

    val time = new Time()
    var (t0, t1) : (Long, Long) = (0, 0)
    var ds : DataFrame = null
    var trainingDS : DataFrame = null
    var testDS : DataFrame = null

    var parametersFile : String = null
    var parameters : Parameters = null
    var timeResult : List[String] = Nil
    var variantsDF : DataFrame = null

    if (args.length == 0) {
      println ("Teher is no file with input parameters")
      exit(1)
    }
    else if (args.length == 1) {

      parametersFile = args(0)

      val params = sc.textFile(parametersFile).map(line => line.split(",").map(elem => elem.trim))
      val keys = params.map(x => x(0)).collect
      val values = params.map(x => x(1)).collect
      val paramsMap = keys.zip(values).toMap

      if (paramsMap.get("missing_rate").getOrElse(null).toDouble < 0. || paramsMap.get("inf_freq").getOrElse(null).toDouble < 0. || paramsMap.get("sup_freq").getOrElse(null).toDouble < 0.
        || paramsMap.get("ld_treshold").getOrElse(null).toDouble < 0. || paramsMap.get("ld_max_bp").getOrElse(null).toInt < 0 || paramsMap.get("ld_max_n").getOrElse(null).toInt < 0){
        println ("Some of parameters are negative")
        sys.exit(-1)
      }

      if (paramsMap.get("missing_rate").getOrElse(null).toDouble > 1. || paramsMap.get("ld_treshold").getOrElse(null).toDouble > 1.){
        println ("Missing rate or ld treshold is great than 1")
        sys.exit(-1)
      }

      val ldMethods = Set("composite", "corr", "r", "dprime")
      if (ldMethods.contains(paramsMap.get("ld_method").getOrElse(null)) == false){
        println ("There is no such method for calculating the linkage disequilibrium")
        sys.exit(-1)
      }

      val pcaMethods = Set("GramSVD", "GLRM", "Power", "Randomized")
      if (pcaMethods.contains(paramsMap.get("pca_method").getOrElse(null)) == false){
        println ("There is no such method for PCA")
        sys.exit(-1)
      }

      parameters = new Parameters(
        paramsMap.get("chr_file").getOrElse(null), paramsMap.get("panel_file").getOrElse(null),
        paramsMap.get("population").getOrElse(null), paramsMap.get("population_set").getOrElse(null).split("/"),
        paramsMap.get("transformWithR").getOrElse(null).toBoolean,
        paramsMap.get("missing_rate").getOrElse(null).toDouble, paramsMap.get("frequencies").getOrElse(null).toBoolean,
        paramsMap.get("inf_freq").getOrElse(null).toDouble, paramsMap.get("sup_freq").getOrElse(null).toDouble,
        paramsMap.get("ld_pruning").getOrElse(null).toBoolean, paramsMap.get("ld_method").getOrElse(null),
        paramsMap.get("ld_treshold").getOrElse(null).toDouble, paramsMap.get("ld_max_bp").getOrElse(null).toInt,
        paramsMap.get("ld_max_n").getOrElse(null).toInt, paramsMap.get("outliers").getOrElse(null).toBoolean,
        paramsMap.get("dim_reduction").getOrElse(null).toBoolean, paramsMap.get("dim_red_method").getOrElse(null),
        paramsMap.get("pca_method").getOrElse(null), paramsMap.get("mds_method").getOrElse(null),
        paramsMap.get("clustering").getOrElse(null).toBoolean, paramsMap.get("classification").getOrElse(null).toBoolean,
        paramsMap.get("clustering_method").getOrElse(null), paramsMap.get("classification_method").getOrElse(null),
        paramsMap.get("cv_classification").getOrElse(null).toBoolean, paramsMap.get("cv_clustering").getOrElse(null).toBoolean,
        paramsMap.get("n_reapeat_classification").getOrElse(null).toInt, paramsMap.get("n_reapeat_clustering").getOrElse(null).toInt,
        paramsMap.get("cluster_df_file").getOrElse(null),
        paramsMap.get("class_train_file").getOrElse(null),
        paramsMap.get("class_test_file").getOrElse(null),
        paramsMap.get("chr_frequencies_file").getOrElse(null),
        paramsMap.get("chr_frequencies_file_output").getOrElse(null)
      )

      if (parameters._isFrequencies == false) {
        parameters._infFreq = 0.
        parameters._supFreq = 1.
      }
    }

    /*        val gtsForSample: RDD[Genotype] = sc.loadGenotypes(genotypeFile)
            val start = 16050000
            val end   = 16053000
            val sampledGts = gtsForSample.filter(g => (g.getVariant.getStart >= start && g.getVariant.getEnd <= end) )
            sampledGts.adamParquetSave("/home/anastasiia/1000genomes/chr22-sample_3000.adam")*/

    def extract(file: String, superPop: String = "super_pop", filter: (String, String) => Boolean) = {
      sc.textFile(file).map(line => {
        val tokens = line.split("\t").toList
        if (superPop == "pop")
          tokens(0) -> tokens(1)
        else tokens(0) -> tokens(2)
      }).collectAsMap.filter(tuple => filter(tuple._1, tuple._2))
    }


    val panel = extract(parameters._panelFile, parameters._pop, (sampleID: String, pop: String) => parameters._popSet.contains(pop))
    val bpanel = sc.broadcast(panel)

    /* val allGenotypes: RDD[Genotype] = sc.loadGenotypes(parameters._chrFile)

    val genotypes: RDD[Genotype] = allGenotypes.filter(genotype => {
      bpanel.value.contains(genotype.getSampleId)
    })*/


    if (parameters._chrFreqFile == "null") {
      val df = sqlContext.read.parquet(parameters._chrFile)
      df.registerTempTable("gts")

      val makeGenotypes = udf((id: String, genotype: WrappedArray[String]) =>
        Genotypes(id, {if(genotype(0) == "Alt" && genotype(1) == "Alt") 2 else if (genotype(0) == "Alt" || genotype(1)== "Alt")  1 else 0} ))

      val df3 = df.withColumn("genotypes", makeGenotypes(col("sampleId"), col("alleles")))
      df3.registerTempTable("gts")
      val sampleCnt =  sqlContext.sql("select count (distinct sampleId) from gts").first.getLong(0).toInt
      val df4 = sqlContext.sql(s"""
        select
          variant.contig.contigName,
          variant.start,
          count(*) as nonmissing,
          variant.referenceAllele as ref,
          variant.alternateAllele as alt,
          sum(case when alleles[0] = 'Alt' and alleles[1] = 'Alt' then 2 when alleles[0] = 'Alt' or alleles[1] = 'Alt' then 1  else 0 end) / ${sampleCnt} as frequency,
          collect_set( concat(genotypes.id,":",cast(genotypes.genotype as string) )) as genotype
        from gts
          where length(variant.alternateAllele) = 1 and length(variant.referenceAllele) = 1
        group by variant.contig.contigName, variant.start, variant.referenceAllele, variant.alternateAllele
          having count(distinct variant.alternateAllele)  = 1 and count(*) = ${sampleCnt}
      """)

      val schema = df4.schema
      val rows = df4.rdd.zipWithUniqueId().map{case (r: Row, id: Long) => Row.fromSeq(id +: r.toSeq)}
      variantsDF = sqlContext.createDataFrame(rows, StructType(StructField("snpIdx", LongType, false) +: schema.fields))
      variantsDF.write.parquet(parameters._chrFreqFileOutput)
      variantsDF.repartition(1).writeToCsv(parameters._chrFreqFileOutput + ".csv", "true")
    }
    else if (parameters._tranformWithR == false) {
      variantsDF = sqlContext.read.parquet(parameters._chrFreqFile).repartition(400) //sqlContext.read.format("com.databricks.spark.csv").option("header", "true").option("inferSchema", "true").load(parameters._chrFreqFile).repartition(400)
      variantsDF.registerTempTable("variants")
      ds = sqlContext.read.parquet(parameters._chrFile).repartition(260)
      ds.registerTempTable("gts")

      val sampleCnt = sqlContext.sql("select count (distinct sampleId) from gts").first.getLong(0).toInt //ds.count

/*      val filteredDF = sqlContext.sql(
        s"""
          select snpIdx, contigName, start, nonmissing, ref, alt, frequency, genotype
          from variants
            where frequency >= ${parameters._infFreq} and frequency <= ${parameters._supFreq}
              and nonmissing >= $sampleCnt * (1 - ${parameters._missingRate})
      """)

      t0 = System.currentTimeMillis()
      filteredDF.show(20)
      t1 = System.currentTimeMillis()
      val fsFreqTime = time.formatTimeDiff(t0, t1)
      println(s"Feature selection. Frequencies: $fsFreqTime")
      timeResult ::= (s"Feature selection. Frequencies: $fsFreqTime")

      val rddForLdPrun: RDD[TSNP] = filteredDF.map { case Row(r0: Long, r1: String, r2: Long, r3: Long, r4: String, r5: String, r6: Double, r7: WrappedArray[String]) =>
        TSNP(r0.toInt, s"${r1}:${r2}", r2.toInt, r7.map(_.split(':')).map(r => (r(0), r(1).toInt)).sortBy(r => r._1).map(_._2.toInt).toArray.toVector)
      }
      val selectedVariantsList: Array[String] = filteredDF.select(concat($"contigName", lit(":"), $"start")).map { case Row(r: String) => r }.toArray :+ "SampleId" :+ "Region"
      ds = ds.select(selectedVariantsList.head, selectedVariantsList.tail:_*)*/

      val genotypesDF = sqlContext.sql(
        s"""
          select
            sampleId,
            concat(variant.contig.contigName, ':', variant.start) as variantId,
            sum(case when alleles[0] = 'Alt' and alleles[1] = 'Alt' then 2 when alleles[0] = 'Alt' or alleles[1] = 'Alt' then 1  else 0 end) as altCount
          from gts g,variants v
          where v.frequency >= ${parameters._infFreq} and v.frequency <= ${parameters._supFreq} and
              g.variant.contig.contigName=v.contigName and g.variant.start=v.start and
                (alleles[0] <> 'OtherAlt' or alleles[1] <> 'OtherAlt')
          group by sampleId, variant.contig.contigName, variant.start
      """).repartition(400)

      // genotypesDF.groupBy("sampleId").pivot("variantId", selectedVariantsList).sum("altCount")
      // val panelDF = panel.toSeq.toDF("sampleId", "Region")//sqlContext.read.load(parameters._panelFile).toDF("sample", "pop", "super_pop", "gender").select("sample", parameters._pop).toDF("sampleId", "Region")
      // ds = genotypesDF.join(panelDF, "sampleId")

      val sampleToData: RDD[(String, (String, Int))] = genotypesDF.map({ case Row(sampleId: String, variantId: String, count: Long) => (sampleId, (variantId, count.toInt)) })
      val groupedSampleData: RDD[(String, Iterable[(String, Int)])] = sampleToData.groupByKey()
      val variantsRDD: RDD[(String, Array[(String, Int)])] = groupedSampleData.mapValues(it => it.toArray.sortBy(_._1))

      val header = StructType(
        Array(StructField("SampleId", StringType)) ++
          Array(StructField("Region", StringType)) ++
          variantsRDD.first()._2.map(variant => {
            StructField(variant._1, DoubleType)
          }))

      val rowRDD: RDD[Row] = variantsRDD.map {
        case (sampleId, variants) =>
          val region: Array[String] = Array(bpanel.value.getOrElse(sampleId, "Unknown"))
          val alternateCounts: Array[Double] = variants.map(_._2.toDouble)
          Row.fromSeq(Array(sampleId) ++ region ++ alternateCounts)
      }

      ds = sqlContext.createDataFrame(rowRDD, header)
      // ds.write.parquet(parameters._chrFile + "parquet")

      /**
        * Variables for prunning data. Used for both ldPruning and Outliers detection cases
        */

      var variantsRDDprunned: RDD[(String, Array[(String, Int)])] = variantsRDD
      var prunnedSnpIdSet: List[String] = null

      /**
        * LDPruning
        */

      if (parameters._isLDPruning == true) {
        println("LD Pruning")
        t0 = System.currentTimeMillis()

        val ldSize = 256
        val seq = sc.parallelize(0 until ldSize * ldSize)
        val ldPrun = new LDPruning(sc, sqlContext, seq)

        val listGeno: RDD[TSNP] = null //rddForLdPrun
        val prunnedSnpIdSet = ldPrun.performLDPruning(listGeno, parameters._ldMethod, parameters._ldTreshold, parameters._ldMaxBp, parameters._ldMaxN)
        variantsRDDprunned = prun(variantsRDDprunned, prunnedSnpIdSet)

        t1 = System.currentTimeMillis()
        val fsLdTime = time.formatTimeDiff(t0, t1)
        println(s"Feature selection. LD Pruning: $fsLdTime")
        timeResult ::= (s"Feature selection. LD Pruning: $fsLdTime")
      }

      /**
        * Outliers detection
        */

      if (parameters._isOutliers == true) {
        println("Outliers Detections")
        val samplesForOutliers: RDD[Seq[Int]] = variantsRDDprunned.map { case (_, sortedVariants) => sortedVariants.map(_._2) }
        val n: Int = samplesForOutliers.count().toInt
        val randomindx: RDD[List[Int]] = sc.parallelize((1 to n / 2).map(unused => Random.shuffle(0 to n - 1).take(n / 2).toList))
        val rddWithIdx: RDD[(Seq[Int], Long)] = samplesForOutliers.zipWithIndex()
        val subRDDs: RDD[Array[Seq[Int]]] = randomindx.map(idx => rddWithIdx.filter { case (_, sampleidx) => idx.contains(sampleidx) }.map(_._1).collect)

        /*    def computeMeanVar(iter: Iterator[Array[Seq[Int]]]) : Iterator[Array[(Double, Double)]] = {
        var meanVar: List[Array[(Double, Double)]] = List()
        while(iter.hasNext){
          val iterCurr = iter.next
          val meanstddev = iterCurr.map{row =>
            val mean = row.sum / n.toDouble
            val variance = row.map(score => (score - mean) * (score - mean))
            val stddev = Math.sqrt(variance.sum / (n.toDouble - 1))
          (mean, stddev)
          }
            meanVar ::= meanstddev
          }
          meanVar.iterator
        }
        val meanVar : RDD[Array[(Double, Double)]] = subRDDs.mapPartitions(computeMeanVar)*/

        val meanVar: RDD[Array[(Double, Double)]] = subRDDs.map { currRDD =>
          val meanStd: Array[(Double, Double)] = currRDD.map { row =>
            val mean = row.sum / n.toDouble
            val variance = row.map(score => (score - mean) * (score - mean))
            val stddev = Math.sqrt(variance.sum / (n.toDouble - 1))
            (mean, stddev)
          }
          meanStd
        }

        meanVar.foreach(x => println(x.toList))
        variantsRDDprunned = prun(variantsRDDprunned, prunnedSnpIdSet)
      }

      /**
        * Dimensionality Reduction
        */

      if (parameters._isDimRed == true) {
        println("Dimensionality Reduction")
        t0 = System.currentTimeMillis()

        parameters._dimRedMethod match {
          case "pcaH2O" => {
            println("pcaH2O")
            val unsupervised = new Unsupervised(sc, sqlContext)
            ds = unsupervised.pcaH2O(ds)
          }
          case "PCA" => {
            println(" PCA")
            val pcaDimRed = new PCADimReduction(sc, sqlContext)
            var nPC = math.min(ds.count, ds.columns.length - 2).toInt
            //  nPC = if (nPC > 30) 30 else (nPC - 1)
            val variantion = 0.7
            ds = pcaDimRed.pcaML(ds, nPC, "Region", variantion, "pcaFeatures")
          }
          case "MDS" => {
            println(" MDS")
            // var snpsMDS : RDD[Seq[Int]] = rddForLdPrun.map(_.snp.toSeq)
            var snpsMDS: RDD[Seq[Double]] = ds.drop("sampleId").drop("Region").map(_.toSeq.map(_.asInstanceOf[Double]))

            val mds = new MDSReduction(sc, sqlContext)
            val pc: RDD[Array[Double]] = sc.parallelize(mds.computeMDS(snpsMDS, "classic"))
            println("mds: ")
            println(pc.count, pc.first.length)

            val samplesSet: RDD[String] = ds.select("sampleId").sort("sampleId").map {
              _.toString
            }
            val mdsRDD: RDD[(String, Array[Double])] = samplesSet.zip(pc)

            /* convert mdsRDD to DF for further usage */
            val mdsPC = Array.fill(mdsRDD.first()._2.length)("PC")
            val s = (1 until (mdsRDD.first()._2.length + 1))
            val headerPC = (mdsPC, s).zipped.par.map { case (pc, s) => pc + s }

            val header = StructType(
              Array(StructField("SampleId", StringType)) ++
                Array(StructField("Region", StringType)) ++
                headerPC.map(pc => {
                  StructField(pc, DoubleType)
                }))

            val rowRDD: RDD[Row] = mdsRDD.map {
              case (sampleId, variant) =>
                val region: Array[String] = Array(panel.getOrElse(sampleId, "Unknown"))
                Row.fromSeq(Array(sampleId) ++ region ++ variant)
            }

            ds = sqlContext.createDataFrame(rowRDD, header)
          }
        }
        t1 = System.currentTimeMillis()
        val dimRedTime = time.formatTimeDiff(t0, t1)
        println(s"Dimensionality reduction. ${parameters._dimRedMethod}: $dimRedTime")
        timeResult ::= (s"Dimensionality reduction. ${parameters._dimRedMethod}: $dimRedTime")
      }
    }else if (parameters._tranformWithR == true){
      ds = sqlContext.read
        .format("com.databricks.spark.csv")
        .option("header", "true")
        .option("inferSchema", "true") // Automatically infer data types
        .option("delimiter", ";")
        .load(parameters._chrFile)


      val schema = StructType(
        Array(StructField("SampleId", StringType)) ++
          Array(StructField("Region", StringType)) ++
          ds.columns.filter(c=>c != "Region" && c!="SampleId").map(variant => {
            StructField(variant, DoubleType)
          }))

      ds = sqlContext.read
        .format("com.databricks.spark.csv")
        .option("header", "true")
        .schema(schema)
        .option("delimiter", ";")
        .load(parameters._chrFile)
    }

    val seeds = 1234
    var split = ds.randomSplit(Array(0.7, 0.3), seeds)
    trainingDS = ds //split(0)
    testDS = split(1)

    /**
      * Clustering
      */  

    if (parameters._isClustering == true) {
      println("Clustering")
      var trainPrediction: DataFrame = null
      var testPrediction: DataFrame = null
      val clustering = new Clustering(sc, sqlContext)
      val setK : Seq[Int] = Seq(1, 2, 3, 4, 5)
      var k = parameters._popSet.length
      var avgPurity = 0.0
      t0 = System.currentTimeMillis()

      parameters._clusterMethod match {
        case "kmeans_h2o" => {
          val unsupervised = new Unsupervised(sc, sqlContext)
          if (parameters._cvClustering == true){
            val kTuning = unsupervised.kMeansTuning(trainingDS, responseColumn = "Region", ignoredColumns = Array("SampleId"), kSet = setK, nReapeat = parameters._nRepeatClustering)
            println("K estimation: ")
            kTuning.foreach{case(k, purity) => println(s"k = ${k}, purity = ${purity}")}
            k = kTuning.maxBy(_._2)._1
          }
          avgPurity = sc.parallelize(0 until parameters._nRepeatClustering).map{ _ =>
            val kMeansModel = unsupervised.kMeansH2O(ds, k)
            trainPrediction = unsupervised.kMeansPredict(kMeansModel, ds)
            clustering.purity(trainPrediction.select("Region", "Predict"))
          }.sum / parameters._nRepeatClustering
          // testPrediction = unsupervised.kMeansPredict(kMeansModel, testDS)
        }

        case "kmeans" => {
          /* if (parameters._cvClustering == true) {
            val kTuning = clustering.kmeansML(trainingDS, "region", "SampleId", k)
            println("K estimation: ")
            kTuning.foreach { case (k, purity) => println(s"k = ${k}, purity = ${purity}") }
            k = kTuning.maxBy(_._2)._1
          }*/
          avgPurity = (0 until parameters._nRepeatClustering).par.map{ _ =>
            val kmeansModel = clustering.kmeansML(ds, "Region", "SampleId", k)
            trainPrediction = clustering.predict(kmeansModel, ds)
            clustering.purity(trainPrediction.select("Region", "Predict"))
          }.sum / parameters._nRepeatClustering
          // testPrediction = clustering.predict(gmmModel, testDS, Array("SampleId", "Region"))
        }

        case "gmm" => {
          if (parameters._cvClustering == true) {
            val kTuning = clustering.gmmKTuning(trainingDS, Array("SampleId", "Region"), setK, nReapeat = parameters._nRepeatClustering)
            println("K estimation: ")
            kTuning.foreach { case (k, purity) => println(s"k = ${k}, purity = ${purity}") }
            k = kTuning.maxBy(_._2)._1
          }
          avgPurity = (0 until parameters._nRepeatClustering).par.map{ _ =>
            val gmmModel = clustering.gmm(ds, Array("SampleId", "Region"), k)
            trainPrediction = clustering.predict(gmmModel, ds, Array("SampleId", "Region"))
            clustering.purity(trainPrediction.select("Region", "Predict"))
          }.sum / parameters._nRepeatClustering
          // testPrediction = clustering.predict(gmmModel, testDS, Array("SampleId", "Region"))
        }

        case "bkm" => {
          if (parameters._cvClustering == true) {
            val kTuning = clustering.bkmKTuning(trainingDS, Array("SampleId", "Region"), setK, nReapeat = parameters._nRepeatClustering)
            println("K estimation: ")
            kTuning.foreach { case (k, purity) => println(s"k = ${k}, purity = ${purity}") }
            k = kTuning.maxBy(_._2)._1
          }
          avgPurity = (0 until parameters._nRepeatClustering).par.map{ _ =>
            val bkmModel = clustering.bkm(ds, Array("SampleId", "Region"), k)
            trainPrediction = clustering.predict(bkmModel, ds, Array("SampleId", "Region"))
            clustering.purity(trainPrediction.select("Region", "Predict"))
          }.sum / parameters._nRepeatClustering
          // testPrediction = clustering.predict(bkmModel, testDS, Array("SampleId", "Region"))
        }
      }

      // var purity = clustering.purity(trainPrediction.select("Region", "Predict"))
      println($"Average purity: ", avgPurity)
      /*      purity = clustering.purity(testPrediction.select("Region", "Predict"))
            println($"Test purity: ", purity)*/

      t1 = System.currentTimeMillis()
      val clusterTime = time.formatTimeDiff(t0, t1)
      println(s"Clustering. ${parameters._clusterMethod} : $clusterTime")
      timeResult ::= (s"Clustering. ${parameters._clusterMethod} : $clusterTime")

      timeResult.foreach(println)

      if (parameters._clusterFileOutput != "null")
        trainPrediction.select("SampleId", "Region", "Predict", "label", "features").repartition(1).writeToCsv(parameters._clusterFileOutput )
      else
        trainPrediction.select("SampleId", "Region", "Predict", "label", "features").repartition(1).writeToCsv(s"${parameters._chrFile}" + s"_${parameters._dimRedMethod}" + s"_${parameters._clusterMethod}")
    }

    /**
      * Classification
      */

    split = ds.drop("SampleId").randomSplit(Array(0.7, 0.3), seeds)
    trainingDS = split(0)
    testDS  = split(1)

    if (parameters._isClassification == true) {
      println("Classification")
      val splitList = (0 until parameters._nRepeatClassification).map(_ => ds.drop("SampleId").randomSplit(Array(0.7, 0.3), seeds))
      val trainDfList = splitList.map(s => s(0))
      val testDfList = splitList.map(s => s(1))

      var trainingPrediction: DataFrame = null
      var testPrediction: DataFrame = null
      var trainingAvgError : Double = Double.NaN
      var testAvgError : Double = Double.NaN
      val classification = new Classification(sc, sqlContext)
      var pipelineModelsList : List[PipelineModel] = Nil

      t0 = System.currentTimeMillis()

      parameters._classificationMethod match {
        case "svm" => {
          println("Start svm")
          pipelineModelsList = trainDfList.map {trainingDf  =>
            val svmModel = classification.svm(trainingDf, cv = parameters._cvClassification, cost = Array(0.01, 0.1, 1, 10), maxIter = Array(1))
            svmModel
          }.toList

          //          val dtStage = svmModel.stages(2).asInstanceOf[SVMModel]
          //          println(s"The best  learned classification SVM model: ${dtStage.mloutput}")
        }

        case "dt" => {
          println("Start dt")
          pipelineModelsList = trainDfList.map {trainingDf  =>
            val dtModel = classification.decisionTrees(trainingDS,  cv = parameters._cvClassification)//, "Region", 10, Array(1.0))
          val dtStage = dtModel.stages(2).asInstanceOf[DecisionTreeClassificationModel]
            println(s"The best  learned classification tree model: ${dtStage.toDebugString}")
            dtModel
          }.toList
        }

        case "rf" => {
          println("Start rf")
          pipelineModelsList = trainDfList.map { trainingDf =>
            val rfModel = classification.randomForest(trainingDS, cv = parameters._cvClassification)
            val dtStage = rfModel.stages(2).asInstanceOf[RandomForestClassificationModel]
            println(s"The best  learned classification random forest model: ${dtStage.toDebugString}")
            rfModel
          }.toList
        }
      }

      /**
        * Training prediction
        */
      var classificationMetrics = pipelineModelsList.zip(trainDfList).map { case (model, trainingDf) =>
        classification.predict(model, trainingDf)
        new ClassificationMetrics(classification._error, classification._precisionByLabel, classification._recallByLabel, classification._fScoreByLabel)
      }

      println(s"Classification algorithm was repeated ${parameters._nRepeatClassification} times")
      println("\nTraining evaluation: ")
      computeAvgMetrics(classificationMetrics)
      println()

      if (parameters._classTrainOutput != "null")
        classification._prediction.select("Region", "prediction", "label", "features").repartition(1).writeToCsv(parameters._classTrainOutput)
      else
        classification._prediction.select("Region", "prediction", "label", "features").repartition(1).writeToCsv(s"${parameters._chrFile}" +
          s"_${parameters._dimRedMethod}" + s"_${parameters._classificationMethod}_train_df")

      /**
        * Test prediction
        */
      classificationMetrics = pipelineModelsList.zip(testDfList).map { case (model, testDf) =>
        classification.predict(model, testDf)
        new ClassificationMetrics(classification._error, classification._precisionByLabel, classification._recallByLabel, classification._fScoreByLabel)
      }
      println("\nTest evaluation: ")
      computeAvgMetrics(classificationMetrics)

      t1 = System.currentTimeMillis()
      val classTime = time.formatTimeDiff(t0, t1)
      println(s"\nClassification. ${parameters._classificationMethod} : $classTime")
      timeResult ::= (s"Classification. ${parameters._classificationMethod} : $classTime")

      timeResult.foreach(println)

      if (parameters._classTestOutput != "null")
        classification._prediction.select("Region", "prediction", "label", "features").repartition(1).writeToCsv(parameters._classTestOutput)
      else
        classification._prediction.select("Region", "prediction", "label", "features").repartition(1).writeToCsv(s"${parameters._chrFile}" +
          s"_${parameters._dimRedMethod}" + s"_${parameters._classificationMethod}_test_df")
    }

    timeResult.foreach(println)

  }
}


