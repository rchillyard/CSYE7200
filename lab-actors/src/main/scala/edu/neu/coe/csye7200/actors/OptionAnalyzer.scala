package edu.neu.coe.csye7200.actors

import akka.actor.ActorRef
import akka.event.LoggingAdapter
import edu.neu.coe.csye7200.HedgeFund
import edu.neu.coe.csye7200.model.{MapUtils, Model}
import edu.neu.coe.csye7200.oldrules.{Candidate, Predicate, SimpleRule}

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/**
  * @author robinhillyard
  */
class OptionAnalyzer(blackboard: ActorRef) extends BlackboardActor(blackboard) {

  // This is mutable state information.
  // When this actor is terminated and reborn, the rules/properties will be re-read from their respective files
  var rules: Map[String, Predicate] = Map[String, Predicate]()
  var properties: List[Map[String, Any]] = List[Map[String, Any]]()

  override def receive: PartialFunction[Any, Unit] = {
    case CandidateOption(model, identifier, put, optionDetails, chainDetails) =>
      log.info("Option Analysis of identifier: {}", identifier)
      val candidate = OptionCandidate(put, model, identifier, optionDetails, chainDetails)
      if (applyRules(put, candidate)) {
        log.debug("Qualifies: sending confirmation message to blackboard")
        val attributes = MapUtils.flatten[String, Any](List("underlying") map { k => k -> candidate(k) } toMap)
        log.debug(s"attributes: $attributes")
        blackboard ! Confirmation(identifier, model, attributes)
      } else
        log.debug(s"$identifier does not qualify")

    case m => super.receive(m)
  }

  override def preStart() {
    rules = OptionAnalyzer.getRules(log)
    properties = OptionAnalyzer.getProperties
  }

  def getProperty(key: String, value: Any, property: String): Option[Any] =
    getProperties(key, value) match {
      case Some(m) => m.get(property);
      case None => None
    }

  def getProperties(key: String, value: Any): Option[Map[String, Any]] =
    properties find { p =>
      p.get(key) match {
        case Some(`value`) => true;
        case _ => false
      }
    }

  def applyRules(put: Boolean, candidate: Candidate): Boolean = {
    candidate("underlying") match {
      case Some(u) =>
        val candidateWithProperties = candidate ++ (getProperties("Id", u) match {
          case Some(p) => p;
          case _ => Map()
        })
        val key = if (put) "put" else "call"
        rules.get(key) match {
          case Some(r) => r(candidateWithProperties) match {
            case Success(b) => b
            case Failure(e) => log.error("rules problem: {}", e); false
          }
          case None => log.error(s"rules problem: $key doesn't define a rule"); false
        }
      case _ => println(s"underlying is not defined for option: $candidate"); false
    }
  }
}

object OptionAnalyzer {

  import java.io.File

  import com.typesafe.config._

  def getRules(log: LoggingAdapter): Map[String, Predicate] = {
    val userHome = System.getProperty("user.home")
    val sRules = "rules.txt"
    val sUserRules = s"$userHome/$sRules"
    val sSysRules = s"/$sRules"
    val co = Try(ConfigFactory.parseURL(getClass.getResource(sSysRules))).toOption

    // TODO re-establish the following more general code...
    //    val userRules = new File(sUserRules)
    //    if (userRules.exists) println(s"exists: $userRules")
    //    val co: Option[Config] = if (userRules.exists) Some(ConfigFactory.parseFile(userRules)) else getConfig(sSysRules, log)
    co match {
      case Some(config) =>
        println(s"config: ${config.entrySet()}")
        List("put", "call") map { k => {
          val r = config.getString(k)
          log.info(s"rule: $k -> $r")
          k -> SimpleRule(r)
        }
        } toMap
      case None => log.warning(s"unable to read rules configuration: $sUserRules or $sSysRules"); Map()
    }
  }

  /**
    * Get a Config corresponding to filename and, optionally, clazz.
    *
    * NOTE: see also getSource in HedgeFund (these methods are similar).
    *
    * NOTE: we try two different ways of getting the file:
    * (1) where file is a pure filename relative to the filing system;
    * (2) where file is the name of a resource relative to the given class (or current class if clazz == null)
    *
    * @param filename the filename to be used for the Config
    * @param clazz    in the case that filename cannot be opened, we will use filename as the name of a resource
    *                 relative to the given class.
    * @return an optional Config
    */
  def getConfig(filename: String, log: LoggingAdapter)(implicit clazz: Class[_] = null): Option[Config] = {
    def getConfig(clazz: Class[_]): Config = {
      println(s"getConfig: $clazz, $filename")
      val z = ConfigFactory.parseURL(clazz.getResource(filename))
      println(s"config: $z")
      z
    }

    val getConfigOptional: Option[Class[_]] => Option[Config] = _ map getConfig
    Try(ConfigFactory.parseFile(new File(filename))).recoverWith { case e: Throwable => println(s"bad"); log.warning(e.getLocalizedMessage); Failure(e) }.toOption orElse
      getConfigOptional(Option(clazz))
  }


  def getProperties: List[Map[String, String]] = {
    val sProperties = "properties.txt"
    val sSysProperties = s"/$sProperties"
    implicit val clazz: Class[_] = getClass
    val so = HedgeFund.getSource(sSysProperties)
    so match {
      case Some(s) =>
        val src = s.getLines
        //    val src = Source.fromFile(sSysProperties).getLines
    // First line is the header
    val headerLine = src.take(1).next
    val columns = headerLine.split(",")
    src map { l => columns zip l.split(",") toMap } toList
      case None =>
        System.err.println(s"problem getting properties: $sSysProperties"); List()
    }
  }
}

/**
  * @author robinhillyard
  *
  *         CONSIDER combining optionDetails and chainDetails in the caller
  */
case class OptionCandidate(put: Boolean, model: Model, id: String, optionDetails: Map[String, String], chainDetails: Map[String, Any]) extends Candidate {

  val details: Map[String, Any] = Map("put" -> put) ++ chainDetails ++ optionDetails

  def identifier: String = id

  // CONSIDER getting rid of the identifier case since we now have a method for that
  def apply(s: String): Option[Any] = s match {
    case "identifier" => Some(id)
    case _ => model.getKey(s) match {
      case Some(x) => details.get(x)
      case _ => None
    }
  }

  def ++(m: Map[String, Any]) = OptionCandidate(put, model, identifier, optionDetails, chainDetails ++ m)

  override def toString = s"OptionCandidate: identifier=$identifier; details=$details"
}
