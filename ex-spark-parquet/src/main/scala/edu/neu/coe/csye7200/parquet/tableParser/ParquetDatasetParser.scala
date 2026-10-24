package edu.neu.coe.csye7200.parquet.tableParser

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper
import com.phasmidsoftware.tableparser.parquet.{ParquetCellConverter, ParquetTableParser}
import java.nio.file.Paths
import org.apache.spark.sql.{Dataset, Encoder, SparkSession}
import scala.io.Codec
import scala.reflect.ClassTag
import scala.util.Try

/**
 * A class for parsing Parquet files into typed Spark Datasets.
 *
 * @tparam T the type of the case class representing rows of the dataset.
 *           This must extend `Product` and have an implicit `ClassTag`.
 * @constructor Creates a new `ParquetDatasetParser` instance.
 * @param columnHelper an implicit instance of `ColumnHelper[T]` used for parsing column data.
 * @param encoder      an implicit `Encoder[T]` needed for converting the parsed data into a Spark Dataset.
 */
class ParquetDatasetParser[T <: Product : ClassTag](
                                                           implicit columnHelper: ColumnHelper[T],
                                                           encoder: Encoder[T]
                                                   ) {
  val converterMap: Map[String, ParquetCellConverter[Any]] = Map.empty
  val groupedHelpers: Map[String, (ClassTag[_], ColumnHelper[_])] = Map.empty

  def parseResource(resource: String)(implicit spark: SparkSession, codec: Codec): Try[Dataset[T]] = {
    val taxiParquet = Paths.get(getClass.getResource(resource).toURI)
    val cm = converterMap
    val gh = groupedHelpers
    val parser = new ParquetTableParser[T] {
      val helper: ColumnHelper[T] = columnHelper
      override val converterMap: Map[String, ParquetCellConverter[Any]] = cm
      override val groupedHelpers: Map[String, (ClassTag[_], ColumnHelper[_])] = gh
    }
    parser.parseParquet(taxiParquet).map { mt =>
      spark.createDataset(mt.toSeq)
    }
  }
}