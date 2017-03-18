package com.zsibio

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SQLContext
import org.apache.spark.mllib.linalg.{Vector, Vectors}
import org.apache.spark.mllib.stat.{MultivariateStatisticalSummary, Statistics}

import scala.util.Random

trait OutliersDetectionMethods{
  def findOutliers(rdd: RDD[Seq[Int]], samplesID: RDD[String]) : List[String]
}

@SerialVersionUID(15L)
class OutliersDetection[T] (sc: SparkContext, sqlContext: SQLContext) extends Serializable with OutliersDetectionMethods{

  def findOutliers(rdd: RDD[Seq[Int]], samplesID: RDD[String]) : List[String] = {
    val n: Int = rdd.count().toInt
    val randomindx : IndexedSeq[List[Int]]  = (1 to n/2).map(unused => Random.shuffle(0 to n - 1).take(n/2).toList)
    val rddWithIdx : RDD[(Seq[Int], Long)]  = rdd.zipWithIndex()
    val subRDDs : IndexedSeq[RDD[Seq[Int]]] = randomindx.map(idx => rddWithIdx.filter{case(_, sampleidx) => idx.contains(sampleidx)}.map(_._1))
    println(subRDDs.length, subRDDs(0).count)
    // subRDDs(0).map{case(seq, id) => println(id, seq)}
    val meanVar = subRDDs.par.map{currRDD =>
      val rdd: RDD[Vector]  = currRDD.map(row => Vectors.dense(row.toArray.map(x => x.asInstanceOf[Double])))
      val summary: MultivariateStatisticalSummary = Statistics.colStats(rdd)
      val means = summary.mean
      val variances = summary.variance
      (means.toArray.toList, variances.toArray.toList)
    }

    meanVar.foreach(println)

    return Nil
  }
}

object OutliersDetection extends Serializable{
  def apply[T](sc: SparkContext, sqlContext: SQLContext): OutliersDetection [T] = new OutliersDetection [T](sc, sqlContext)
}