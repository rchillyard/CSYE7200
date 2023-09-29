package edu.neu.coe.csye7200.csv.tableParser

import com.phasmidsoftware.parse.TableParser
import com.phasmidsoftware.table.Table
import org.apache.spark.sql.{Dataset, Encoder, SparkSession}

import scala.io.Codec
import scala.util.Try

trait TableDatasetParser[T] {

  def parseResource(resource: String)(implicit spark: SparkSession, ev: TableParser[Table[T]], encoder: Encoder[T], codec: Codec): Try[Dataset[T]] = {
    Table.parseResource(resource, getClass) map {
      mt =>
        println(s"Movie table has ${mt.size} rows")
        spark.createDataset(mt.toSeq)
    }
  }
}