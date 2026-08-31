/*
 * Copyright (c) 2019. Phasmid Software
 */

package edu.neu.coe.csye7200.csv

import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.table.{HeadedTable, Header, Table}
import scala.util.matching.Regex

/**
 * This class represents a generic type: names and phone numbers.
 * Created by scalaprof on 7/10/24.
 *
 *
 */
case class Generic(name: String, phone: String)

object GenericParser extends CellParsers {

//  def camelCaseColumnNameMapper(w: String): String = w.replaceAll("([A-Z0-9])", "_$1")
//
//  implicit val movieColumnHelper: ColumnHelper[Movie] = columnHelper(camelCaseColumnNameMapper _,
//    "title" -> "movie_title",
//    "imdb" -> "movie_imdb_link")
//  implicit val reviewsColumnHelper: ColumnHelper[Reviews] = columnHelper(camelCaseColumnNameMapper _,
//    "facebookLikes" -> "movie_facebook_likes",
//    "numUsersReview" -> "num_user_for_reviews",
//    "numUsersVoted" -> "num_voted_users",
//    "numCriticReviews" -> "num_critic_for_reviews",
//    "totalFacebookLikes" -> "cast_total_facebook_likes")
//  implicit val formatColumnHelper: ColumnHelper[Format] = columnHelper(camelCaseColumnNameMapper _)
//  implicit val productionColumnHelper: ColumnHelper[Production] = columnHelper(camelCaseColumnNameMapper _)
//  implicit val principalColumnHelper: ColumnHelper[Principal] = columnHelper(camelCaseColumnNameMapper _, Some("$x_$c"))
//  implicit val ratingParser: CellParser[Rating] = cellParser(Rating.apply: String => Rating)
//  implicit val formatParser: CellParser[Format] = cellParser4(Format)
//  implicit val productionParser: CellParser[Production] = cellParser4(Production)
//  implicit val nameParser: CellParser[Name] = cellParser(Name.apply)
//  implicit val principalParser: CellParser[Principal] = cellParser2(Principal)
//  implicit val reviewsParser: CellParser[Reviews] = cellParser7(Reviews)
//  implicit val attributesParser: CellParser[AttributeSet] = cellParser(AttributeSet.apply: String => AttributeSet)
//  implicit val optionalPrincipalParser: CellParser[Option[Principal]] = cellParserOption
//  implicit val movieParser: CellParser[Movie] = cellParser11(Movie)

  implicit val genericParser: CellParser[Generic] = cellParser2(Generic)

  implicit object GenericConfig extends DefaultRowConfig {
    override val string: Regex = """[^,]*""".r
    override val delimiter: Regex = "\t".r
    override val listEnclosure: String = ""
  }

  implicit val parser: StandardRowParser[Generic] = StandardRowParser.create[Generic]

  implicit object GenericTableParser extends StringTableParser[Table[Generic]] {
    protected def builder(rows: Iterable[Generic], header: Header): Table[Generic] = HeadedTable(rows, header)

    type Row = Generic

    val maybeFixedHeader: Option[Header] = None

    override val forgiving: Boolean = true

    val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

    protected def builder(rows: Iterator[Generic], header: Header): Table[Row] = HeadedTable(rows, header)

    override val headerRowsToRead: Int = 1
  }

}
