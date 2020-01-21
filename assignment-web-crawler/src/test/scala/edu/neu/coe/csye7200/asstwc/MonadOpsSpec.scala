package edu.neu.coe.csye7200.asstwc

import org.scalatest.concurrent._
import org.scalatest.{FlatSpec, Matchers}

/**
 * TODO implement me preperly
 */
class MonadOpsSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  //    "lift(Future[Try[T]])" should "succeed for http://www.google.com" in {
  //      val uyf = Future(Try(new URL("http://www.google.com")))
  //      val uf = flatten(uyf)
  //      whenReady(uf) { u => u should matchPattern { case _: URL => } }
  //    }
  //
  //    "lift(Try[Future[T]])" should "succeed for http://www.google.com" in {
  //      val ufy = Try(Future(new URL("http://www.google.com")))
  //      val uf = flatten(ufy)
  //      whenReady(uf) { u => u should matchPattern { case _: URL => } }
  //    }
  //
  //    "sequence(Seq[Future[T]])" should "succeed for http://www.google.com, etc." in {
  //      val ws = List("http://www.google.com", "http://www.microsoft.com")
  //      val ufs = for {w <- ws; uf = Future(new URL(w))} yield uf
  //      whenReady(Future.sequence(ufs)) { us => Assertions.assert(us.length == 2) }
  //    }
  //
  //    behavior of "sequence(Seq[Try[T]])"
  //    it should "succeed for http://www.google.com, etc." in {
  //      val ws = List("http://www.google.com", "http://www.microsoft.com")
  //      val uys = for {w <- ws; url = Try(new URL(w))} yield url
  //      sequence(uys) match {
  //        case Success(us) => Assertions.assert(us.length == 2)
  //        case _ => Failed
  //      }
  //    }
  //    it should "fail for www.google.com, etc." in {
  //      val ws = List("www.google.com", "http://www.microsoft.com")
  //      val uys = for {w <- ws; uy = Try(new URL(w))} yield uy
  //      sequence(uys) match {
  //        case Failure(_) => Succeeded
  //        case _ => Failed
  //      }
  //    }
  //    it should "succeed for empty list" in {
  //      val uys = for {w <- List[String](); uy = Try(new URL(w))} yield uy
  //      sequence(uys) match {
  //        case Success(us) => Assertions.assert(us.isEmpty)
  //        case _ => Failed
  //      }
  //    }
  //
  //    "sequence(Seq[Option[T]])" should "succeed for 1, 2, ..." in {
  //      val ws = List("1", "2", "")
  //      val xos: Seq[Option[Int]] = for {w <- ws; xo = Try(w.toInt).toOption} yield xo
  //      println(sequence(xos))
  //      sequence(xos) match {
  //        case Some(_) => Failed("failure")
  //        case _ =>
  //      }
  //    }
  //
  //    behavior of "sequence"
  //    it should "convert Future[Option[X]] to Option[Future[X]]" in {
  //      val xof: Future[Some[Int]] = Future(Some(1))
  //      val xfo: Option[Future[Int]] = sequence(xof)
  //      xfo should matchPattern { case Some(_) => }
  //      whenReady(xfo.get) { x =>
  //        x should matchPattern { case 1 => }
  //      }
  //    }
  //
  //    it should "convert Option[Future[X]] to Future[[OptionX]]" in {
  //      val xfo = Some(Future(1))
  //      val xof = sequence(xfo)
  //      whenReady(xof) { xo =>
  //        xo should matchPattern { case Some(_) => }
  //        xo.get shouldBe 1
  //      }
  //    }
  //
  //    behavior of "flatten"
  //    it should "succeed" in {
  //      val ifs: Seq[Future[Seq[Int]]] = Seq(Future(Seq(1, 2)))
  //      whenReady(flatten(ifs)) { x => println(x); x should matchPattern { case Seq(1, 2) => } }
  //    }
  //    it should "succeed for http://www.google.com, etc." in {
  //      val ws = List("http://www.google.com", "http://www.microsoft.com")
  //      val ufs = for {w <- ws; uf = Future(new URL(w))} yield uf
  //      val usfs = List(Future.sequence(ufs))
  //      whenReady(flatten(usfs)) { us => Assertions.assert(us.length == 2) }
  //    }
  //    it should "succeed for empty list" in {
  //      val ws = List[String]()
  //      val urls = for {w <- ws; uf = Future(new URL(w))} yield uf
  //      val usfs = List(Future.sequence(urls))
  //      whenReady(flatten(usfs)) { us => Assertions.assert(us.isEmpty) }
  //    }
  //    it should "succeed for option map" in {
  //      val map: Map[String, Option[String]] = Map("a" -> Some("A"), "b" -> None)
  //      val flat: Map[String, String] = flatten(map)
  //      flat.size shouldBe 1
  //    }
  //
  //    "sequence" should "succeed for http://www.google.com, www.microsoft.com" in {
  //      val ws = Seq("http://www.google.com", "http://www.microsoft.com", "www.microsoft.com")
  //      val ufs = for {w <- ws; uf = Future(new URL(w))} yield uf
  //      val uefs = for {uf <- ufs} yield sequence(uf)
  //      val uesf = Future.sequence(uefs)
  //      whenReady(uesf) { ues => Assertions.assert(ues.length == 3) }
  //      whenReady(uesf) { ues => (ues.head, ues(1)) should matchPattern { case (Right(_), Right(_)) => } }
  //      whenReady(uesf) { ues => ues(2) should matchPattern { case Left(_) => } }
  //    }
  //
  //    "sequence(Future=>Future(Either))" should "succeed for http://www.google.com, www.microsoft.com" in {
  //      val ws = Seq("http://www.google.com", "http://www.microsoft.com", "www.microsoft.com")
  //      val uefs = for {w <- ws; uf = Future(new URL(w))} yield sequence(uf)
  //      for {uef <- uefs} whenReady(uef) { case Right(_) => true; case Left(_) => true; case _ => Assertions.fail() }
  //    }
  //
  //    "Sequence[Either]" should "succeed" in {
  //      val l: Either[Throwable, Int] = Left(new RuntimeException("bad"))
  //      val r: Either[Throwable, Int] = Right(99)
  //      sequence(l) should matchPattern { case None => }
  //      sequence(r) should matchPattern { case Some(99) => }
  //    }
  //
  //    "zip(Option,Option)" should "succeed" in {
  //      val (one, two, none) = (Some(1), Some(2), None)
  //      zip(one, two) should matchPattern { case Some((1, 2)) => }
  //      zip(none, two) should matchPattern { case None => }
  //      zip(one, none) should matchPattern { case None => }
  //    }
  //
  //    "zip(Try,Try)" should "succeed" in {
  //      val (one, two, fail) = (Success(1), Success(2), Failure(new NoSuchElementException))
  //      zip(one, two) should matchPattern { case Success((1, 2)) => }
  //      zip(fail, two) should matchPattern { case Failure(_) => }
  //      zip(one, fail) should matchPattern { case Failure(_) => }
  //    }
  //
  //    "zip(Future,Future)" should "succeed" in {
  //      val one = Future(1)
  //      val two = Future(2)
  //      val fail = Future.failed(new NoSuchElementException)
  //      whenReady(zip(one, two)) { x => x should matchPattern { case (1, 2) => } }
  //      zip(fail, two).failed.futureValue shouldBe a [NoSuchElementException]
  //      zip(one, fail).failed.futureValue shouldBe a [NoSuchElementException]
  //    }
  //
  //    "optionToTry" should "succeed for Map" in {
  //      val map = Map("a" -> "A", "b" -> "B")
  //      optionToTry(map.get("a")) should matchPattern { case Success("A") => }
  //      optionToTry(map.get("x")) should matchPattern { case Failure(_) => }
  //    }
  //
  //    "lift" should "succeed" in {
  //      def double(x: Int) = 2 * x
  //
  //      Success(1) map double should matchPattern { case Success(2) => }
  //      Failure(new Exception("bad")) map double should matchPattern { case Failure(_) => }
  //    }
  //
  //    "map2" should "succeed" in {
  //      val one = Success(1)
  //      val two = Success(2)
  //
  //      def sum(x: Int, y: Int) = x + y
  //
  //      map2(one, two)(sum) should matchPattern { case Success(3) => }
  //      map2(one, Failure(new Exception("bad")))(sum) should matchPattern { case Failure(_) => }
  //    }
  //
  //    "asFuture" should "succeed" in {
  //      whenReady(asFuture(Success(1))) { x => x should matchPattern { case 1 => } }
  //      //    whenReady(toFuture(Failure[Int](new Exception("bad")))) { x => p shouldBe new Exception("bad")}
  //    }
  //
  //    behavior of "flattenRecover"
  //    ignore should "succeed for http://www.htmldog.com/examples/, www.microsoft.com" in {
  //      val sb = new StringBuffer("caught exception: ")
  //      val ws: Seq[String] = Seq("http://www.htmldog.com/examples/", "www.microsoft.com")
  //      //    val wsfs: Seq[Future[Seq[String]]] = for (w <- ws; uf = Future(new URL(w))) yield for (u <- uf; s <- Future(Source.fromURL(u))) yield s.mkString.split("\n").toSeq
  //      val wsfs: Seq[Future[Seq[String]]] = outerForLoop(ws)
  //      val wsXefs: Seq[Future[Either[Throwable, Seq[String]]]] = for (wsf <- wsfs) yield sequence(wsf)
  //      val wsf: Future[Seq[String]] = flattenRecover(Future.sequence(wsXefs), e => sb.append(e.toString))
  //      whenReady(wsf, timeout(Span(6, Seconds))) { ws => assert(ws.size > 320) }
  //      sb.toString shouldBe "caught exception: java.net.MalformedURLException: no protocol: www.microsoft.com"
  //    }
  //
  //
  //    private def outerForLoop(ws: Seq[String]) = {
  //      //    for (w <- ws; uf = Future(new URL(w))) yield innerForLoop(uf)
  //      ws.map{ w => innerForLoop(Future(new URL(w)))}
  //      //    ws.map { case x$1@w => (x$2, x$1) }.map { case (uf, w) => innerForLoop(uf) }
  //    }
  //
  //    private def innerForLoop(uf: Future[URL]) = {
  //      uf.flatMap(u => Future(Source.fromURL(u)).map(s => s.mkString.split("\n").toSeq))
  //    }

  behavior of "LiftFuture"
  it should "work" in {}

  behavior of "AsFuture"

  it should "work" in {}

  behavior of "SequenceForgiving"

  it should "work" in {}

  behavior of "LiftTry"
  it should "work" in {}

  behavior of "SequenceWithLogging"

  it should "work" in {}

  behavior of "Zip"

  it should "work" in {}

  behavior of "OptionToTry"

  it should "work1" in {}

  it should "work2" in {}

  behavior of "SequenceForgivingWithLogging"

  it should "work" in {}

  behavior of "Sequence"

  it should "work1" in {}

  it should "work2" in {}

  it should "work3" in {}

  it should "work4" in {}

  it should "work5" in {}

  it should "work6" in {}

  it should "work" in {}

  behavior of "FlattenRecover"

  it should "work" in {}

  behavior of "LiftOption"

  it should "work" in {}

  behavior of "Map2"

  it should "work" in {}

  behavior of "Flatten"

  it should "work1" in {}

  it should "work2" in {}

  it should "work3" in {}

  it should "work4" in {}

}
