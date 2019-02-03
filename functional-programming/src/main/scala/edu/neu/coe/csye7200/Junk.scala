package edu.neu.coe.csye7200

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

object ConcurrencyExample extends App {

  type pass = Int
  type fail = Int

  val time = System.currentTimeMillis()

  //use recursion to process each Future in the list
  def segregate[T](fs: List[Future[T]]): Future[(pass, fail)] = {
    def go(fs: List[Future[T]], r: Future[(pass, fail)]): Future[(pass, fail)] = fs match {
      case Nil => r
      case l :: ls =>
        val fx = l.transform({ _ => (1, 0) }, identity).recoverWith[(pass, fail)]({ case _: Exception => Future(0, 1) })
        for (x <- fx; t <- r; g <- go(ls, Future(t._1 + x._1, t._2 + x._2))) yield g
    }

    go(fs, Future((0, 0)))
  }

  //hardcoded future
  val futures2 = List(Future(1), Future(2), Future(throw new Exception("error")))

  val result = segregate(futures2)
  result onComplete {
    case Success(v) => println(s"successes: ${v._1}, failures: ${v._2}")
    case Failure(v) => v.printStackTrace()
  }

  Await.result(result, 1000 millis)
}

//noinspection NotImplementedCode
object MoreJunk {
  def getUser(id: Int): Future[Option[String]] = ???

  def someFunction1(s: String): Int = ???

  val r: Future[Option[Int]] = for {
    maybeUser <- getUser(1)
  } yield {
    maybeUser.map(someFunction1)
  }
}
