package com.zsibio

import org.apache.spark.SparkContext
import org.apache.spark.sql.{DataFrame, SQLContext}
import org.apache.spark.rdd.RDD
import smile.mds.MDS
import smile.mds.IsotonicMDS
import smile.mds.SammonMapping
import org.apache.spark.mllib.linalg.{Vector, Vectors}
import org.apache.spark.mllib.linalg.distributed.{MatrixEntry, RowMatrix}
import breeze.linalg.{DenseVector, norm, sum}

trait MDSMethods{
  def computeMinkowski(v1: Vector, v2: Vector, p: Int) : Double
  def computeEuclidean(v1: Vector, v2: Vector): Double
  def computeManhattan(v1: Vector, v2: Vector): Double
  def computeHamming(v1: Vector, v2: Vector): Double
  protected def calcDistance(method: String, v1: Vector, v2: Vector, p: Int) : Double
  // def computeMDS(ds: DataFrame, mdsMethod: String, distance: String, p: Int): Array[Array[Double]]
  def computeMDS(ds: RDD[Seq[Double]], mdsMethod: String, distance: String, p: Int): Array[Array[Double]]
}

@SerialVersionUID(15L)
class MDSReduction[T] (sc: SparkContext, sqlContext: SQLContext) extends Serializable with MDSMethods {

  private val _variance : Double =  .7
  private val _npc: Int = 20
  private var _distance : String = ""
  private var _p : Int = 2

  def computeMinkowski(v1: Vector, v2: Vector, p: Int) : Double = {
    val b1 = new DenseVector(v1.toArray)
    val b2 = new DenseVector(v2.toArray)
    norm((b1 - b2), p)
  }

  def computeEuclidean(v1: Vector, v2: Vector): Double = {
    computeMinkowski(v1, v2, 2)
  }

  def computeManhattan(v1: Vector, v2: Vector): Double = {
    computeMinkowski(v1, v2, 1)
  }

  def computeHamming(v1: Vector, v2: Vector): Double = {
    (v1.toArray, v2.toArray).zipped.par.map{case(el1, el2) => el1 != el2 }.filter(_ == true).size
  }

  protected def calcDistance(method: String, v1: Vector, v2: Vector, p: Int = 2) : Double = {
    method match {
      case "minkowski" => return computeMinkowski(v1, v2, p)
      case "euclidean" => return computeEuclidean(v1, v2)
      case "manhattan" => return computeManhattan(v1, v2)
      case "hamming"   => return computeHamming(v1, v2)
    }
    return Double.NaN
  }

  private def mds(proximity: Array[Array[Double]], k: Int, add: Boolean = false): MDS = new MDS(proximity, k, add)
  private def isomds(proximity: Array[Array[Double]], k: Int, tol: Double = 0.0001, maxIter: Int = 200): IsotonicMDS = new IsotonicMDS(proximity, k, tol, maxIter)
  private def sammon(proximity: Array[Array[Double]], k: Int, lambda: Double = 0.2, tol: Double = 0.0001, maxIter: Int = 100): SammonMapping = new SammonMapping(proximity, k, lambda, tol, maxIter)

  def computeClassic(proximity: Array[Array[Double]], k: Int, add: Boolean = false) : Array[Array[Double]] ={
    val model = mds(proximity, k, add)
    val variance = model.getProportion
    val cumVariance = variance.map{var s: Double = 0; d => {s += d; s}}
    val numberPC = cumVariance.filter(value => value <= _variance).length
    val pc = model.getCoordinates.map(arr => arr.slice(0, numberPC))
    pc
  }

  def computeMDSds(ds: DataFrame, mdsMethod: String, distance: String = "euclidean", p: Int = 2): Array[Array[Double]] ={
    println("MDS Start")
    val infoSampleRegion = ds.select("SampleId", "Region")
    val snps = ds.drop("SampleId").drop("Region")
    val snpsRDD: RDD[Vector] = snps.rdd.map(row => Vectors.dense(row.toSeq.toArray.map(x => x.asInstanceOf[Double])))
    val matrix: Array[Double] = snpsRDD.cartesian(snpsRDD).map{case(vec1, vec2) => calcDistance(distance, vec1, vec2, p)}.toArray
    val proximityMatr: Array[Array[Double]] = matrix.grouped(math.sqrt(matrix.length).toInt).toArray

    val pc = mdsMethod match {
      case "classic"  => computeClassic(proximityMatr, _npc)
    /*  case "isotonic" => isomds(proximityMatr, proximityMatr.size)
      case "sammon"   => sammon(proximityMatr, proximityMatr.size) */
    }
    pc
  }

  def computeMDS(ds: RDD[Seq[Double]], mdsMethod: String, distance: String = "euclidean", p: Int = 2): Array[Array[Double]] ={
    _distance = distance
    _p = p
    val snps: RDD[Vector] = ds.map(row => Vectors.dense(row.toArray))
    val matrix: Array[Double] = snps.cartesian(snps).map{case(vec1, vec2) => calcDistance(distance, vec1, vec2, p)}.toArray
/*    val rowMatrix = new RowMatrix(snps)
    val matrix = rowMatrix.columnSimilarities().toBlockMatrix().toLocalMatrix().toArray*/
    val proximityMatr: Array[Array[Double]] = matrix.grouped(math.sqrt(matrix.length).toInt).toArray
    println(proximityMatr.length, proximityMatr(1).length)
    // proximityMatr.foreach(x => println(x.toList))

    val pc = mdsMethod match {
      case "classic"  => computeClassic(proximityMatr, _npc)
     /* case "isotonic" => isomds(proximityMatr, proximityMatr.size)
      case "sammon"   => sammon(proximityMatr, proximityMatr.size) */
    }
    pc
  }

}

object MDSReduction extends Serializable{
  def apply[T](sc: SparkContext, sqlContext: SQLContext): MDSReduction[T] = new MDSReduction[T](sc, sqlContext)
}
