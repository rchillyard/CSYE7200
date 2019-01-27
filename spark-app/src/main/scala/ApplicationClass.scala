import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import scala.language.postfixOps

/**
  * Created by scalaprof on 10/19/16.
  */
class ApplicationClass {

  def doSomethingUseful(): Unit = {
    val sc: SparkContext = ContextServer.sc
    println("I have a SparkContext")

    val matrix: Array[Array[Int]] = Array.ofDim[Int](3,3)
    for (i <- 0 to 2; j <- 0 to 2) matrix(i)(j) = i+j
    val identity: Array[Array[Int]] = Array.ofDim[Int](3,3)
    for (i <- 0 to 2; j <- 0 to 2) identity(i)(j) = if (i==j) 1 else 0
    val rddM: RDD[Array[Int]] = sc.parallelize(matrix)
    val rddI: RDD[Array[Int]] = sc.parallelize(identity)

    showRDD(rddM)
    showRDD(transpose(rddM))
//    showRDD(multiply(rddM,rddI))

    def byColumnAndRow(rdd: RDD[Array[Int]]) = {
      rdd.zipWithIndex.flatMap {
        case (row, rowIndex) => row.zipWithIndex.map {
          case (number, columnIndex) => columnIndex -> (rowIndex, number)
        }
      }
    }

    def transpose(rdd: RDD[Array[Int]]): RDD[Array[Int]] = {
      // Split the matrix into one number per line.
      val byColumnAndRow: RDD[(Int, (Long, Int))] = byColumnAndRow(rdd)
      byColumnAndRow foreach println

      // Build up the transposed matrix. Group and sort by column index first.
      val byColumn = byColumnAndRow.groupByKey.sortByKey().values
      // Then sort by row index.
      val transposed: RDD[Array[Int]] = byColumn.map {
        indexedRow => indexedRow.toSeq.sortBy(_._1).map(_._2).toArray
      }
      transposed
    }

    def multiply(rdd1: RDD[Array[Int]], rdd2: RDD[Array[Int]]): RDD[Array[Int]] = {
      def dotProduct(row: Array[Int], col: Array[Int]): Int = {
        println()
        (row zip col) map ( t => t._1 * t._2) sum
      }

//      def junk(row: Array[Int], col: Array[Int]): Array[Int] = {
//        val xx = row zipWithIndex
//        val yy = col zipWithIndex
////        for ((x,i) <- xx; (y,j) <- yy) yield
//        Array.empty
//      }


      val x1: RDD[(Int, (Long, Int))] = byColumnAndRow(rdd1)
      val x2: RDD[(Int, (Long, Int))] = byColumnAndRow(rdd2)

//      for ((r, (i, c)) <- x1) yield x2 reduceByKey{((x: Long,a: Int),(y: Long,b: Int)) => (x, a+b)}

      val transposed: RDD[Array[Int]] = transpose(rdd2)

//      def doIt(row: Array[Int]): Array[Int] = for (col <- rdd2) yield calculateRow()

      def calculateRow(row: Array[Int]): RDD[Int] = transposed.map(col => dotProduct(row, col))

      val xx: RDD[(Long, Array[Int])] = for ((row,i) <- rdd1 zipWithIndex) yield (i, row)
      val yy: RDD[(Long, Array[Int])] = for ((col,j) <- transposed zipWithIndex) yield (j, col)

      for ((i, row) <- xx) yield

//      val z: RDD[Array[Int]] = rdd1.map[Array[Int]]{ row: Array[Int] => doIt(row)}
//      z
      null
    }

    def showRDD(rdd: RDD[Array[Int]]): Unit = showArrayArray(rdd.collect)

    def showArrayArray[X](z: Array[Array[X]]): Unit = for (i <- z.indices) showArray(s"$i", z(i))

    def showArray[X](w: String, xs: Array[X]): Unit = for (j <- xs.indices) println(s"$w,$j: ${xs(j)}")

  }
}
