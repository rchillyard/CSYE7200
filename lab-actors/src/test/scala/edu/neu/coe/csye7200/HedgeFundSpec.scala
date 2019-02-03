package edu.neu.coe.csye7200

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.neu.coe.csye7200.actors.SymbolQuery
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{FlatSpec, Matchers, _}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * This specification really tests much of the HedgeFund app but because it particularly deals with
  * processing data from the YQL (Yahoo Query Language) using JSON, we call it by its given name.
  */
class HedgeFundSpec extends FlatSpec with Matchers with
  Futures with ScalaFutures with TryValues with Inside {

  behavior of "SymbolQuery"

  it should "work" in {
    import scala.concurrent.duration._
    implicit val system: ActorSystem = ActorSystem("HedgeFund")
    implicit val timeout: Timeout = Timeout(30.seconds)
    val ay = HedgeFund.startup(ConfigFactory.load())
    ay should matchPattern { case Success(_) => }
    val qf = ay match {
      case Success(q) => q ? SymbolQuery("MSFT", List("name", "symbol", "price", "GF", "t", "l"))
      case Failure(x) => Future.failed(x)
    }
    whenReady(qf, org.scalatest.concurrent.PatienceConfiguration.Timeout(Span(10, Seconds))) { q => println(q) }
  }

}
