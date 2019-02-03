package edu.neu.coe.csye7200.asstmd

import scala.collection.mutable
import scala.io.Source
import scala.util.Try

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
  * I will suggest you only focus on TO BE IMPLEMENTEDs in the assignments.
  *
  */
case class Movie(title: String, format: Format, production: Production, reviews: Reviews, director: Principal, actor1: Principal, actor2: Principal, actor3: Principal, genres: Seq[String], plotKeywords: Seq[String], imdb: String)

/**
  * The movie format (including language and duration).
  *
  * @param color       whether filmed in color
  * @param language    the native language of the characters
  * @param aspectRatio the aspect ratio of the film
  * @param duration    its length in minutes
  */
case class Format(color: Boolean, language: String, aspectRatio: Double, duration: Int) {
  override def toString = {
    val x = if (color) "Color" else "B&W"
    s"$x,$language,$aspectRatio,$duration"
  }
}

/**
  * The production: its country, year, and financials
  *
  * @param country   country of origin
  * @param budget    production budget in US dollars
  * @param gross     gross earnings (?)
  * @param titleYear the year the title was registered (?)
  */
case class Production(country: String, budget: Int, gross: Int, titleYear: Int) {
  def isKiwi = this match {
    case Production("New Zealand", _, _, _) => true
    case _ => false
  }
}

/**
  * Information about various forms of review, including the content rating.
  */
case class Reviews(imdbScore: Double, facebookLikes: Int, contentRating: Rating, numUsersReview: Int, numUsersVoted: Int, numCriticReviews: Int, totalFacebookLikes: Int)

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
  * @param first         first name
  * @param middle        middle name or initial
  * @param last          last name
  * @param suffix        suffix
  */
case class Name(first: String, middle: Option[String], last: String, suffix: Option[String]) {
  override def toString = {
    case class Result(r: StringBuffer) { def append(s: String): Unit = r.append(" "+s); override def toString = r.toString}
    val r: Result = Result(new StringBuffer(first))
    middle foreach {r.append}
    r.append(last)
    suffix foreach {r.append}
    r.toString
  }
}

/**
  * The US rating
  */
case class Rating(code: String, age: Option[Int]) {
  override def toString = code + (age match {
    case Some(x) => "-" + x
    case _ => ""
  })
}

object Movie extends App {

  trait IngestibleMovie extends Ingestible[Movie] {
    //Hint: Think of the return type of method. Also, you need the apply method which is similar as a construction method in java.
    //The source file is a csv file which is separated by ","
    // 11 points
    def fromString(w: String): Try[Movie] = ??? // TO BE IMPLEMENTED
  }

  implicit object IngestibleMovie extends IngestibleMovie

  val ingester = new Ingest[Movie]()
  if (args.length > 0) {
    val source = Source.fromFile(args.head)
    val kiwiMovies: Iterator[Try[Movie]] =
      for (my <- ingester(source)) yield
        for (m <- my; if m.production.isKiwi) yield m
    kiwiMovies foreach { _ foreach { println(_) } }
    source.close()
  }

  /**
    * Form a list from the elements explicitly specified (by position) from the given list
    *
    * @param list    a list of Strings
    * @param indices a variable number of index values for the desired elements
    * @return a list of Strings containing the specified elements in order
    */
  def elements(list: Seq[String], indices: Int*): List[String] = {
    val x = mutable.ListBuffer[String]()
    // Hint: form a new list which is consisted by the elements in list in position indices. Int* means array of Int.
    // 6 points
    // TO BE IMPLEMENTED
    ???
    x.toList
  }

  /**
    * Alternative apply method for the Movie class
    *
    * @param ws a sequence of Strings
    * @return a Movie
    */
  def apply(ws: Seq[String]): Movie = {
    // we ignore facenumber_in_poster since I have no idea what that means.
    val title = ws(11)
    val format = Format(elements(ws, 0, 19, 26, 3))
    val production = Production(elements(ws, 20, 22, 8, 23))
    val reviews = Reviews(elements(ws, 25, 27, 21, 18, 12, 2, 13))
    val director = Principal(elements(ws, 1, 4))
    val actor1 = Principal(elements(ws, 10, 7))
    val actor2 = Principal(elements(ws, 6, 24))
    val actor3 = Principal(elements(ws, 14, 5))
    val plotKeywords = ws(16).split("""\|""").toList
    val genres = ws(9).split("""\|""").toList
    val imdb = ws(17)
    Movie(title, format, production, reviews, director, actor1, actor2, actor3, genres, plotKeywords, imdb)
  }
}

object Format {
  def apply(params: List[String]): Format = params match {
    case color :: language :: aspectRatio :: duration :: Nil => apply(color == "Color", language, aspectRatio.toDouble, duration.toInt)
    case _ => throw new Exception(s"logic error in Format: $params")
  }
}

object Production {
  def apply(params: List[String]): Production = params match {
    case country :: budget :: gross :: titleYear :: Nil => apply(country, budget.toInt, gross.toInt, titleYear.toInt)
    case _ => throw new Exception(s"logic error in Production: $params")
  }
}

object Reviews {
  def apply(params: List[String]): Reviews = params match {
    case imdbScore :: facebookLikes :: contentRating :: numUsersReview :: numUsersVoted :: numCriticReviews :: totalFacebookLikes :: Nil => apply(imdbScore.toDouble, facebookLikes.toInt, Rating(contentRating), numUsersReview.toInt, numUsersVoted.toInt, numCriticReviews.toInt, totalFacebookLikes.toInt)
    case _ => throw new Exception(s"logic error in Reviews: $params")
  }
}

object Name {
  // this regex will not parse all names in the Movie database correctly. Still, it gets most of them.
  val rName = """^([\p{L}\-\']+\.?)\s*(([\p{L}\-]+\.)\s)?([\p{L}\-\']+\.?)(\s([\p{L}\-]+\.?))?$""".r

  def apply(name: String): Name = name match {
    case rName(first, _, null, last, _, null) => apply(first, None, last, None)
    case rName(first, _, middle, last, _, null) => apply(first, Some(middle), last, None)
    case rName(first, _, null, last, _, suffix) => apply(first, None, last, Some(suffix))
    case rName(first, _, middle, last, _, suffix) => apply(first, Some(middle), last, Some(suffix))
    case _ => throw new Exception(s"parse error in Name: $name")
  }
}

object Principal {
  def apply(params: List[String]): Principal = params match {
    case name :: facebookLikes :: Nil => apply(name, facebookLikes.toInt)
    case _ => throw new Exception(s"logic error in Principal: $params")
  }

  def apply(name: String, facebookLikes: Int): Principal = apply(Name(name),facebookLikes)
}

object Rating {
  // Hint: This regex matches three patterns: (\w*), (-(\d\d)), (\d\d), for example "PG-13", the first one matches "PG", second one "-13", third one "13".
  val rRating = """^(\w*)(-(\d\d))?$""".r

  /**
    * Alternative apply method for the Rating class such that a single String is decoded
    *
    * @param s a String made up of a code, optionally followed by a dash and a number, e.g. "R" or "PG-13"
    * @return a Rating
    */
  // Hint: This should similar to apply method in Object Name. The parameter of apply in case match should be same as case class Rating
  // 13 points
  def apply(s: String): Rating = ??? // TO BE IMPLEMENTED

}
