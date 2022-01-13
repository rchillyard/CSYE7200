/*
 * Copyright (c) 2019. Phasmid Software
 */

package edu.neu.coe.csye7200.csv

import com.phasmidsoftware.parse._
import com.phasmidsoftware.table.{HeadedTable, Header, Table}
import scala.util.Try
import scala.util.matching.Regex

/**
  * This class represents a Movie from the IMDB data file on Kaggle.
  * Although the limitation on 22 fields in a case class has partially gone away, it's still convenient to group the different attributes together into logical classes.
  *
  * Created by scalaprof on 9/12/16.
  *
  * Common questions in this assignment:
  * 1. Where is main method?
  * In most case, you don't need to run main method for assignments.
  * Unit tests are provided to test your implementation.
  * In this assignment, you will find the `object Movie extends App`,
  * the `App` trait can be used to quickly turn objects into executable programs.
  * You can read the official doc of Scala for more details.
  *
  * 2. How to understand the whole program in this assignment?
  * I won't suggest you to understand the whole program in this assignment,
  * there are some advanced features like `implicit` which hasn't been covered in class.
  * You should be able to understand it before midterm.
  * I will suggest you only focus on each TO BE IMPLEMENTED in the assignments.
  *
  */
case class Movie(title: String, format: Format, production: Production, reviews: Reviews, director: Principal, actor1: Principal, actor2: Principal, actor3: Option[Principal], genres: AttributeSet, plotKeywords: AttributeSet, imdb: String)

/**
  * The movie format (including language and duration).
  *
  * @param color       whether filmed in color
  * @param language    the native language of the characters
  * @param aspectRatio the aspect ratio of the film (optional)
  * @param duration    its length in minutes (optional)
  */
case class Format(color: String, language: String, aspectRatio: Option[Double], duration: Option[Int]) {
  override def toString: String = {
    s"$color,$language,$aspectRatio,$duration"
  }
}

/**
  * The production: its country, year, and financials
  *
  * @param country   country of origin
  * @param budget    (optional) production budget in US dollars
  * @param gross     (optional) gross earnings (?)
  * @param titleYear the year the title was registered (?)
  */
case class Production(country: String, budget: Option[Int], gross: Option[Int], titleYear: Option[Int]) {
  def isKiwi: Boolean = this match {
    case Production("New Zealand", _, _, _) => true
    case _ => false
  }
}

/**
  * Information about various forms of review, including the content rating.
  */
case class Reviews(imdbScore: Double, facebookLikes: Int, contentRating: Rating, numUsersReview: Option[Int], numUsersVoted: Int, numCriticReviews: Option[Int], totalFacebookLikes: Int)

/**
  * A cast or crew principal
  *
  * @param name          name
  * @param facebookLikes number of FaceBook likes
  */
case class Principal(name: Name, facebookLikes: Int) {
  override def toString = s"$name ($facebookLikes likes)"
}

/**
  * A name of a contributor to the production
  *
  * @param first  first name
  * @param middle middle name or initial
  * @param last   last name
  * @param suffix suffix
  */
case class Name(first: String, middle: Option[String], last: String, suffix: Option[String]) {
  override def toString: String = {
    case class Result(r: StringBuffer) {
      def append(s: String): Unit = r.append(" " + s)

      override def toString: String = r.toString
    }
    val r: Result = Result(new StringBuffer(first))
    middle foreach {
      r.append
    }
    r.append(last)
    suffix foreach {
      r.append
    }
    r.toString
  }
}

/**
  * The US rating.
  * NOTE: this definition does not cover all of the ratings in the IMDB movie dataset.
  * That's OK--this is just an exemplar.
  */
case class Rating(code: String, age: Option[Int]) {
  override def toString: String = code + (age match {
    case Some(x) => "-" + x
    case _ => ""
  })
}

object MovieParser extends CellParsers {

  def camelCaseColumnNameMapper(w: String): String = w.replaceAll("([A-Z0-9])", "_$1")

  implicit val movieColumnHelper: ColumnHelper[Movie] = columnHelper(camelCaseColumnNameMapper _,
    "title" -> "movie_title",
    "imdb" -> "movie_imdb_link")
  implicit val reviewsColumnHelper: ColumnHelper[Reviews] = columnHelper(camelCaseColumnNameMapper _,
    "facebookLikes" -> "movie_facebook_likes",
    "numUsersReview" -> "num_user_for_reviews",
    "numUsersVoted" -> "num_voted_users",
    "numCriticReviews" -> "num_critic_for_reviews",
    "totalFacebookLikes" -> "cast_total_facebook_likes")
  implicit val formatColumnHelper: ColumnHelper[Format] = columnHelper(camelCaseColumnNameMapper _)
  implicit val productionColumnHelper: ColumnHelper[Production] = columnHelper(camelCaseColumnNameMapper _)
  implicit val principalColumnHelper: ColumnHelper[Principal] = columnHelper(camelCaseColumnNameMapper _, Some("$x_$c"))
  implicit val ratingParser: CellParser[Rating] = cellParser(Rating.apply: String => Rating)
  implicit val formatParser: CellParser[Format] = cellParser4(Format)
  implicit val productionParser: CellParser[Production] = cellParser4(Production)
  implicit val nameParser: CellParser[Name] = cellParser(Name.apply)
  implicit val principalParser: CellParser[Principal] = cellParser2(Principal)
  implicit val reviewsParser: CellParser[Reviews] = cellParser7(Reviews)
  implicit val attributesParser: CellParser[AttributeSet] = cellParser(AttributeSet.apply: String => AttributeSet)
  implicit val optionalPrincipalParser: CellParser[Option[Principal]] = cellParserOption
  implicit val movieParser: CellParser[Movie] = cellParser11(Movie)

  implicit object MovieConfig extends DefaultRowConfig {
    override val string: Regex = """[^,]*""".r
    override val delimiter: Regex = """,""".r
    override val listEnclosure: String = ""
  }

  implicit val parser: StandardRowParser[Movie] = StandardRowParser[Movie]

  implicit object MovieTableParser extends StringTableParser[Table[Movie]] {
    protected def builder(rows: Iterable[Movie], header: Header): Table[Movie] = HeadedTable(rows, header)

    type Row = Movie

    val maybeFixedHeader: Option[Header] = None

    override val forgiving: Boolean = true

    val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

  }

}

object Name {
  // NOTE: this regex will not parse all names in the Movie database correctly. Still, it gets most of them.
  private val rName =
    """^([\p{L}\-']+\.?)\s*(([\p{L}\-]+\.?)\s)?([\p{L}\-']+\.?)(\s([\p{L}\-]+\.?))?$""".r

  def apply(name: String): Name = name match {
    case rName(first, _, null, last, _, null) => apply(first, None, last, None)
    case rName(first, _, middle, last, _, null) => apply(first, Some(middle), last, None)
    case rName(first, _, null, last, _, suffix) => apply(first, None, last, Some(suffix))
    case rName(first, _, middle, last, _, suffix) => apply(first, Some(middle), last, Some(suffix))
    case _ => throw new Exception(s"""parse error in Name: '$name'""")
  }
}

object Rating {
  /**
    * Alternative apply method for the Rating class such that a single String is decoded
    *
    * @param s a String made up of a code, optionally followed by a dash and a number, e.g. "R" or "PG-13"
    * @return a Rating
    */
  def apply(s: String): Rating =
    s match {
      case rRating(code, _, null) => apply(code, None)
      case rRating(code, _, age) => apply(code, Try(age.toInt).toOption)
      case _ => throw new Exception(s"""parse error in Rating: '$s'""")
    }

  private val rRating = """^(\w*)(-(\d\d))?$""".r

}
