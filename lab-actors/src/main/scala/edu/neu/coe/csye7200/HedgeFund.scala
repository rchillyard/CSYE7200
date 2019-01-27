package edu.neu.coe.csye7200

import akka.actor.{ActorRef, ActorSystem, Props}
import com.typesafe.config.{Config, ConfigFactory}
import edu.neu.coe.csye7200.actors.{ExternalLookup, HedgeFundBlackboard, PortfolioUpdate}
import edu.neu.coe.csye7200.model.{GoogleOptionQuery, GoogleQuery, Query, YQLQuery}
import edu.neu.coe.csye7200.portfolio.{Portfolio, PortfolioParser}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration
import scala.io.Source
import scala.language.implicitConversions
import scala.util._

/**
  * @author robinhillyard
  *
  *         TODO migrate entire package from spray http to akka http
  */
object HedgeFund {

  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load()
    println(s"""${config.getString("name")}, ${config.getString("appVersion")}""")
    implicit val system: ActorSystem = ActorSystem("HedgeFund")
    startup(config)
    Thread.sleep(10000)
    Await.ready(system.terminate(), FiniteDuration(1, "second"))
  }

  def startup(config: Config)(implicit system: ActorSystem): Try[ActorRef] = {
    val engine: Option[Query] = config.getString("engine") match {
      case "YQL" => Some(YQLQuery(config.getString("format"), diagnostics = false))
      case "Google" => Some(GoogleQuery("NASDAQ"))
      case _ => None
    }
    engine match {
      case Some(x) =>
        getPortfolio(config) match {
          case Some(portfolio) =>
            val blackboard = system.actorOf(Props.create(classOf[HedgeFundBlackboard]), "blackboard")
            val symbols = getSymbols(config, portfolio)
            blackboard ! ExternalLookup(x.getProtocol, x.createQuery(symbols))
            val optionEngine = new GoogleOptionQuery
            symbols foreach {
              s => blackboard ! ExternalLookup(optionEngine.getProtocol, optionEngine.createQuery(List(s)))
            }
            blackboard ! PortfolioUpdate(portfolio)
            Success(blackboard)

          case None => Failure(new Exception(s"configuration has errors--see logs"))
        }

      case _ => Failure(new Exception("initialization engine not defined"))
    }
  }

  import scala.language.postfixOps

  def getSymbols(config: Config, portfolio: Portfolio): List[String] = {
    // TODO add in the symbols from the portfolio
    config.getString("symbols") split "\\," toList
  }

  def getPortfolio(config: Config): Option[Portfolio] = {
    val filename = config.getString("portfolio")
    implicit val clazz: Class[_] = getClass
    val json = for (s <- getSource(filename)) yield s.mkString
    json map PortfolioParser.decode
  }

  /**
    * Get a Source corresponding to filename and, optionally, clazz.
    *
    * NOTE: we try two different ways of getting the file:
    * (1) where file is a pure filename relative to the filing system;
    * (2) where file is the name of a resource relative to the given class (or current class if clazz == null)
    *
    * NOTE: that all of this is going away in 2.12 because there is a fromResource method in Source there.
    *
    * @param filename the filename to be used as the Source
    * @param clazz    in the case that filename cannot be opened, we will use filename as the name of a resource
    *                 relative to the given class.
    * @return an optional Source
    */
  def getSource(filename: String)(implicit clazz: Class[_] = null): Option[Source] = {
    def getSource(clazz: Class[_]): Source = Source.fromURL(clazz.getResource(filename))

    val getSourceOptional: Option[Class[_]] => Option[Source] = _ map getSource
    Try(Source.fromFile(filename)).recoverWith { case e: Throwable => logger.warn(e.getLocalizedMessage); Failure(e) }.toOption orElse
      getSourceOptional(Option(clazz))
  }

  val logger: Logger = LoggerFactory.getLogger(getClass)

}
