package edu.neu.coe.csye7200

/**
  * Created by scalaprof on 12/31/16.
  */
class Junk2 {

}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

object ConcurrencyExample2 extends App {

  type pass = Int
  type fail = Int

  val time = System.currentTimeMillis()

  //use recursion to process each Future in the list
  def segregate(l: List[Future[Int]]): Future[(pass, fail)] = {
    def go(l: List[Future[Int]], t: (pass, fail)): Future[(pass, fail)] = {
      l match {
        case Nil => Future {
          t
        }
        //l is List of Future[Int]. flatMap each successful Future
        //recover each failed Future
        case _l :: ls =>
          // This looks very suspicious!
          _l flatMap (_ => go(ls, (t._1 + 1, t._2)))
          _l.recoverWith({ case _ => go(ls, (t._1, t._2 + 1)) }).asInstanceOf[Future[(pass, fail)]]
      }
    }

    go(l, (0, 0))
  }

  //hardcoded future
  val futures2: List[Future[Int]] = List(Future {
    1
  }, Future {
    2
  }, Future {
    throw new Exception("error")
  })


  val result = segregate(futures2)
  result onComplete {
    case Success(v) => println("pp:" + v)
    case Failure(v) => println("fp:" + v)
  }

  Await.result(result, 1000 millis)
}
