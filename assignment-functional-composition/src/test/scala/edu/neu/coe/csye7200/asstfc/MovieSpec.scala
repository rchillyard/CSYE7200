package edu.neu.coe.csye7200.asstfc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.{Codec, Source}
import scala.util._

/**
  * Created by scalaprof on 9/13/16.
  */
class MovieSpec extends AnyFlatSpec with Matchers {

  val phi: Double = (math.sqrt(5) + 1) / 2

  behavior of "Name"

  it should "work for String" in {
    val x = Name.parse("Tom Brady")
    x should matchPattern {
      case Success(Name("Tom", None, "Brady", None)) =>
    }
    Name.parse("Noémie Lenoir") should matchPattern {
      case Success(Name("Noémie", None, "Lenoir", None)) =>
    }
    Name.parse("J.J. Abrams") should matchPattern {
      case Success(Name("J.", Some("J."), "Abrams", None)) =>
    }
    Name.parse("Robert Downey Jr.") should matchPattern {
      case Success(Name("Robert", None, "Downey", Some("Jr."))) =>
    }
  }
  it should "work for Name" in {
    val x = Name("Tom", None, "Brady", None)
    x should matchPattern {
      case Name("Tom", None, "Brady", None) =>
    }
  }

  behavior of "Principal"

  it should "work for List[String]" in {
    Principal.parse(List("Tom Brady", "1")) should matchPattern {
      case Success(Principal(Name("Tom", None, "Brady", None), 1)) =>
    }
    Principal.parse(List("Noémie Lenoir", "2")) should matchPattern {
      case Success(Principal(Name("Noémie", None, "Lenoir", None), 2)) =>
    }
    Principal.parse(List("J.J. Abrams", "3")) should matchPattern {
      case Success(Principal(Name("J.", Some("J."), "Abrams", None), 3)) =>
    }
    Principal.parse(List("Robert Downey Jr.", "4")) should matchPattern {
      case Success(Principal(Name("Robert", None, "Downey", Some("Jr.")), 4)) =>
    }
  }

  behavior of "Rating"

  it should "work for String, Int" in {
    val x = Rating("PG", Some(13))
    x should matchPattern {
      case Rating("PG", Some(13)) =>
    }
  }
  it should "work for PG-13" in {
    val x = Rating.parse("PG-13")
    x should matchPattern {
      case Success(Rating("PG", Some(13))) =>
    }
  }
  it should "work for R" in {
    val x = Rating.parse("R")
    x should matchPattern {
      case Success(Rating("R", None)) =>
    }
  }
  it should "work for PG-0-" in {
    val x = Rating.parse("PG-0-")
    x should matchPattern {
      case Failure(_) =>
    }
  }
  it should "work for PG-XX" in {
    val x = Rating.parse("PG-XX")
    x should matchPattern {
      case Failure(_) =>
    }
  }

  behavior of "Format"

  it should "work for Boolean, String, Double, Int" in {
    val x = Format(color = true, "Swahili", phi, 129)
    x should matchPattern {
      case Format(true, "Swahili", `phi`, 129) =>
    }
  }
  it should "work for List[String]" in {
    val x = Format.parse(List("Color", "Swahili", phi.toString, "129"))
    x should matchPattern {
      case Success(Format(true, "Swahili", `phi`, 129)) =>
    }
  }

  behavior of "Production"

  it should "work for String, Int" in {
    val x = Production("Kenya", 1000000, 1000001, 2016)
    x should matchPattern {
      case Production("Kenya", 1000000, 1000001, 2016) =>
    }
  }
  it should "work for List[String]" in {
    val x = Production.parse(List("Kenya", "1000000", "1000001", "2016"))
    x should matchPattern {
      case Success(Production("Kenya", 1000000, 1000001, 2016)) =>
    }
  }
  it should "define isKiwi properly" in {
    Production("Kenya", 1000000, 1000001, 2016).isKiwi shouldBe false
    Production("New Zealand", 1000000, 1000001, 2016).isKiwi shouldBe true
  }

  behavior of "Reviews"

  it should "work for List[String]" in {
    val x = Reviews.parse(List("8.14", "42", "PG-13", "7", "10", "12", "99"))
    x should matchPattern {
      case Success(Reviews(8.14, 42, Rating("PG", Some(13)), 7, 10, 12, 99)) =>
    }
  }

  behavior of "Movie.getMoviesFromCountry"

  it should "work for the sample file" in {
    val ingester = new Ingest[Movie]()
    implicit val codec: Codec = Codec.UTF8
    val source = Source.fromResource("movie_metadata.csv")
    val msy = Movie.getMoviesFromCountry("New Zealand", ingester(source))
    msy should matchPattern { case Success(_) => }
    msy.get.size shouldBe 4
    source.close()
  }

  behavior of "Movie.testSerializationAndDeserialization"

  it should "work for the sample file" in {
    val ingester = new Ingest[Movie]()
    implicit val codec: Codec = Codec.UTF8
    val source = Source.fromResource("movie_metadata.csv")
    val msy = Movie.getMoviesFromCountry("New Zealand", ingester(source))
    val by = for (ms <- msy) yield Movie.testSerializationAndDeserialization(ms)
    by match {
      case Success(true) =>
      case Failure(x) => fail("problem", x)
      case _ => fail("unknown problem")
    }
    source.close()
  }
}
