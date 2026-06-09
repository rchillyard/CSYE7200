package edu.neu.coe.csye7200.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper
import com.phasmidsoftware.tableparser.parquet.ParquetCellConverter
import edu.neu.coe.csye7200.parquet.tableParser.ParquetDatasetParser
import org.apache.spark.sql.SparkSession
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

/**
 * The `YellowTaxiAnalyzer` object serves as an entry point to analyze yellow taxi trip data
 * by leveraging Spark for distributed data processing. It includes a custom implementation for
 * parsing Parquet files containing taxi trip data and converting them into structured datasets.
 *
 * This application:
 * - Creates an implicit `SparkSession` for running Spark operations.
 * - Configures Spark to suppress `INFO` and `WARN` logs for clarity.
 * - Implements a custom parser for processing nested structures in `YellowTaxiTripGrouped` data.
 * - Reads a resource file containing sample taxi trip data in Parquet format.
 * - Prints the record count and displays the first few records.
 *
 * The key components of the parser include:
 * - `converterMap`: A mapping of string column identifiers to their respective `ParquetCellConverter`
 * for handling data transformation from Parquet cells to appropriate types.
 * - `groupedHelpers`: Provides helper metadata such as `ClassTag` and `ColumnHelper` for each nested
 * structure in the Parquet dataset.
 *
 * NOTE: The parser demonstrates handling complex, nested Parquet structures, which requires
 * overriding the default mappings for converting and grouping data.
 */
object YellowTaxiAnalyzer extends App {

  implicit val spark: SparkSession = SparkSession
          .builder()
          .appName("YellowTaxiAnalyzer")
          .master("local[*]")
          .getOrCreate()

  spark.sparkContext.setLogLevel("ERROR") // We want to ignore all of the INFO and WARN messages.

  import YellowTaxiTripGrouped._
  import spark.implicits._

  /**
   * Parser implementation for handling YellowTaxiTripGrouped Parquet files.
   * Actually, this method only reads one file (not a Parquet "dataset").
   *
   * This parser facilitates the conversion of parquet data into structured datasets
   * of type `YellowTaxiTripGrouped`. It uses predefined mappings and helpers to
   * ensure efficient parsing and type conversion of parquet cells.
   *
   * The `converterMap` provides a mapping between column keys (as strings) and their
   * respective `ParquetCellConverter` for data transformation.
   *
   * The `groupedHelpers` defines mappings between column keys and their
   * corresponding metadata such as `ClassTag` and `ColumnHelper` for parsing support.
   *
   * NOTE that this is much more complex than a flat Parquet file would require
   * because we must override converterMap and groupedHelpers to support the
   * nested structure of the YellowTaxiTripGrouped case class.
   */
  val parser = new ParquetDatasetParser[YellowTaxiTripGrouped] {
    override val converterMap: Map[String, ParquetCellConverter[Any]] = Map(
      "ids" -> TripIdentifiers.converter.asInstanceOf[ParquetCellConverter[Any]],
      "timing" -> TripTiming.converter.asInstanceOf[ParquetCellConverter[Any]],
      "geo" -> TripGeography.converter.asInstanceOf[ParquetCellConverter[Any]],
      "fare" -> FareBreakdown.converter.asInstanceOf[ParquetCellConverter[Any]],
      "metrics" -> TripMetrics.converter.asInstanceOf[ParquetCellConverter[Any]]
    )
    override val groupedHelpers: Map[String, (ClassTag[_], ColumnHelper[_])] = Map(
      "ids" -> (implicitly[ClassTag[TripIdentifiers]], TripIdentifiers.helper),
      "timing" -> (implicitly[ClassTag[TripTiming]], TripTiming.helper),
      "geo" -> (implicitly[ClassTag[TripGeography]], TripGeography.helper),
      "fare" -> (implicitly[ClassTag[FareBreakdown]], FareBreakdown.helper),
      "metrics" -> (implicitly[ClassTag[TripMetrics]], TripMetrics.helper)
    )
  }

  parser.parseResource("/taxi_sample.parquet") match {
    case Failure(exception) =>
      throw exception
    case Success(d) =>
      println(s"Yellow Taxi Dataset has ${d.count()} records")
      d.show(10)
  }
}