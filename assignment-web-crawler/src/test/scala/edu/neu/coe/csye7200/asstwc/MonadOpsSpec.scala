package edu.neu.coe.csye7200.asstwc

import java.util.NoSuchElementException
import org.scalatest.concurrent._
import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/**
  * TODO implement me properly
  */
class MonadOpsSpec extends flatspec.AnyFlatSpec with should.Matchers with Futures with ScalaFutures {

  import MonadOps._
  import scala.concurrent.ExecutionContext.Implicits.global

  behavior of "LiftFuture"
  it should "work" in {
    val g: Future[Int] => Future[String] = liftFuture(_.toString)
    whenReady(g(Future(1))) { y => y should matchPattern { case "1" => } }
  }

  behavior of "AsFuture"
  it should "work" in {
    val xf = asFuture(Try(1))
    whenReady(xf) { x => x should matchPattern { case 1 => } }
  }

  behavior of "SequenceForgivingWithLogging"
  it should "work" in {
    val sb = new StringBuilder
    val xys: Seq[Try[Int]] = Seq(Try(1), Success(2), Failure(WebCrawlerException("dummy")))
    val xsy: Try[Seq[Int]] = sequenceForgivingWithLogging(xys)(e => sb.append(e.getLocalizedMessage))
    xsy should matchPattern { case Success(_) => }
    xsy.get.size shouldBe 2
    sb.toString shouldBe "dummy"
  }

  behavior of "SequenceWithLogging"
  it should "work" in {
    val sb = new StringBuilder
    val xys: Seq[Try[Int]] = Seq(Try(1), Success(2), Failure(WebCrawlerException("dummy1")), Success(3), Failure(WebCrawlerException("dummy2")))
    val xsy: Try[Seq[Int]] = sequenceWithLogging(xys)(e => sb.append(e.getLocalizedMessage))
    xsy should matchPattern { case Failure(WebCrawlerException("dummy1", null)) => }
    sb.toString shouldBe "dummy2"
  }

  behavior of "SequenceForgiving"
  it should "work" in {
    val xys: Seq[Try[Int]] = Seq(Try(1), Success(2), Failure(WebCrawlerException("dummy")))
    val xsy: Try[Seq[Int]] = sequenceForgiving(xys)
    xsy should matchPattern { case Success(_) => }
    xsy.get.size shouldBe 2
  }

  behavior of "LiftTry"
  it should "work" in {
    val g: Try[Int] => Try[String] = liftTry(_.toString)
    g(Try(1)) should matchPattern { case Success("1") => }
  }

  behavior of "Zip"
  "zip(Option,Option)" should "succeed" in {
    val (one, two, none) = (Some(1), Some(2), None)
    zip(one, two) should matchPattern { case Some((1, 2)) => }
    zip(none, two) should matchPattern { case None => }
    zip(one, none) should matchPattern { case None => }
  }

  "zip(Try,Try)" should "succeed" in {
    val (one, two, fail) = (Success(1), Success(2), Failure(new NoSuchElementException))
    zip(one, two) should matchPattern { case Success((1, 2)) => }
    zip(fail, two) should matchPattern { case Failure(_) => }
    zip(one, fail) should matchPattern { case Failure(_) => }
  }

  "zip(Future,Future)" should "succeed" in {
    val one = Future(1)
    val two = Future(2)
    val fail = Future.failed(new NoSuchElementException)
    whenReady(zip(one, two)) { x => x should matchPattern { case (1, 2) => } }
    zip(fail, two).failed.futureValue shouldBe a[NoSuchElementException]
    zip(one, fail).failed.futureValue shouldBe a[NoSuchElementException]
  }

  behavior of "OptionToTry"
  it should "work1" in {
    optionToTry(Some(1), WebCrawlerException("junk")) should matchPattern { case Success(1) => }
    optionToTry(None, WebCrawlerException("junk")) should matchPattern { case Failure(WebCrawlerException("junk", null)) => }
  }

  it should "work2" in {
    optionToTry(Some(1)) should matchPattern { case Success(1) => }
    val xy = optionToTry(None)
    xy should matchPattern { case Failure(_) => }
    xy.toEither.isLeft shouldBe true
  }

  behavior of "Sequence"

  it should "work1" in {
    sequence(Seq(Some(1), Some(2))) shouldBe Some(Seq(1, 2))
    sequence(Seq(Some(1), None)) shouldBe None
  }

  it should "work2" in {
    sequence(Success(1)) shouldBe Right(1)
    sequence(Failure(WebCrawlerException("junk"))) should matchPattern { case Left(WebCrawlerException("junk", null)) => }
  }

  it should "work3" in {
    sequence(Seq(Success(1))) shouldBe Success(Seq(1))
    sequence(Seq(Success(1), Failure(WebCrawlerException("junk")))) shouldBe Failure(WebCrawlerException("junk"))
  }

  it should "work4" in {
    val xys: LazyList[Success[Int]] = LazyList.continually(Success(1)).take(3)
    val xsy: Try[LazyList[Int]] = sequence(xys)
    xsy should matchPattern { case Success(_) => }
    xsy.get.size shouldBe 3
  }

  it should "work5" in {
    sequence(Right(1)) should matchPattern { case Some(1) => }
    sequence(Left(WebCrawlerException("junk"))) should matchPattern { case None => }
  }

  behavior of "mapFuture"
  //  it should "work4" in {
  //    val xsf: Seq[Future[Either[Throwable, Int]]] = mapFuture(Seq(Future(1), Future(2)))
  //    whenReady(xsf) { xs => xs should matchPattern { case 1 => } }
  //  }

  //  behavior of "FlattenRecover"
  //  it should "work" in {
  //    flatt
  //  }

  behavior of "LiftOption"
  it should "work" in {
    val g: Option[Int] => Option[String] = liftOption(_.toString)
    g(Option(1)) should matchPattern { case Some("1") => }
  }

  behavior of "Map2"
  it should "work" in {
    map2(Success(2), Success("A"))((x, y) => y * x) shouldBe Success("AA")
  }

  behavior of "Flatten"

  it should "work1" in {
    val xf: Future[Int] = flatten(Success(Future(1)))
    whenReady(xf) { x => x should matchPattern { case 1 => } }
  }

  it should "work2" in {
    val xf: Future[Int] = flatten(Future(Success(1)))
    whenReady(xf) { x => x should matchPattern { case 1 => } }
  }

  it should "work3" in {
    val xf: Future[Seq[Int]] = flatten(Seq(Future(Seq(1, 2)), Future(Seq(3, 4))))
    whenReady(xf) { x => x should matchPattern { case Seq(1, 2, 3, 4) => } }
  }

  //  it should "work4" in {}

}
