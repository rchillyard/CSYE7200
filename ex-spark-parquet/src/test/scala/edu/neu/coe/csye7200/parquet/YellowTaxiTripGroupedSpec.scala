package edu.neu.coe.csye7200.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper
import com.phasmidsoftware.tableparser.core.table.{HeadedTable, Table}
import com.phasmidsoftware.tableparser.parquet.{ParquetCellConverter, ParquetTableParser}
import java.nio.file.{Path, Paths}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.reflect.ClassTag
import scala.util.Success

class YellowTaxiTripGroupedSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val taxiParquet: Path =
    Paths.get(getClass.getResource("/taxi_sample.parquet").toURI)

  private val parser = new ParquetTableParser[YellowTaxiTripGrouped] {
    val helper = YellowTaxiTripGrouped.helper
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

  private var table: Table[YellowTaxiTripGrouped] = _

  // The first row in the Parquet file has vendorId=2, puLocationId=75, doLocationId=237.
  // We find it by key rather than relying on ParIterable ordering.
  private def firstRow: YellowTaxiTripGrouped =
    table.content.toSeq
            .find(r => r.geo.puLocationId.contains(75) && r.geo.doLocationId.contains(237))
            .getOrElse(fail("Expected row with puLocationId=75, doLocationId=237 not found"))

  override def beforeAll(): Unit =
    table = parser.parseParquet(taxiParquet).get

  behavior of "ParquetTableParser with grouped YellowTaxiTripGrouped"

  it should "parse taxi_sample.parquet into 1000 rows" in {
    table.size shouldBe 1000
  }

  it should "produce a table with 19 columns in the header" in {
    val result = parser.parseParquet(taxiParquet)
    result match {
      case Success(t: HeadedTable[_]) =>
        t.header.size shouldBe 19
    }
  }

  it should "correctly populate TripIdentifiers for the first row" in {
    val row = firstRow
    row.ids.vendorId shouldBe Some(2)
    row.ids.ratecodeId shouldBe Some(1L)
    row.ids.storeAndFwdFlag shouldBe Some("N")
  }

  it should "correctly populate TripTiming for the first row" in {
    val row = firstRow
    row.timing.tpepPickupDatetime shouldBe defined
    row.timing.tpepDropoffDatetime shouldBe defined
    row.timing.tpepPickupDatetime.get.isBefore(row.timing.tpepDropoffDatetime.get) shouldBe true
  }

  it should "correctly populate TripGeography for the first row" in {
    val row = firstRow
    row.geo.puLocationId shouldBe Some(75)
    row.geo.doLocationId shouldBe Some(237)
  }

  it should "correctly populate FareBreakdown for the first row" in {
    val row = firstRow
    row.fare.fareAmount shouldBe Some(10.0)
    row.fare.tipAmount shouldBe Some(4.5)
    row.fare.improvementSurcharge shouldBe Some(1.0)
    row.fare.congestionSurcharge shouldBe Some(2.5)
    row.fare.airportFee shouldBe Some(0.0)
  }

  it should "correctly populate TripMetrics for the first row" in {
    val row = firstRow
    row.metrics.passengerCount shouldBe Some(1L)
    row.metrics.tripDistance shouldBe Some(1.76)
    row.metrics.paymentType shouldBe Some(1L)
    row.metrics.totalAmount shouldBe Some(19.5)
  }

  it should "produce 1000 non-empty totalAmount values" in {
    val totalAmounts = table.content.toSeq.flatMap(_.metrics.totalAmount)
    totalAmounts should have size 1000
  }

  it should "have consistent totalAmount for the known first row" in {
    firstRow.metrics.totalAmount shouldBe Some(19.5)
  }
}