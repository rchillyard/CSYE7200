package edu.neu.coe.csye7200.actors

import akka.actor.ActorRef
import edu.neu.coe.csye7200.model.{GoogleOptionModel, Model}
import spray.http._

import scala.util._

/**
  * TODO create a super-type for this kind of actor
  *
  * @author robinhillyard
  */
class JsonGoogleOptionParser(blackboard: ActorRef) extends BlackboardActor(blackboard) {

  val model: Model = new GoogleOptionModel

  override def receive: PartialFunction[Any, Unit] = {
    case ContentMessage(entity) =>
      log.debug("JsonGoogleOptionParser received ContentMessage")
      JsonGoogleOptionParser.decode(entity) match {
        case Right(optionChain) => processOptionChain(optionChain)
        case Left(message) => log.warning("Decoding error: " + message)
      }
    case m => super.receive(m)
  }

  def processOptionChain(optionChain: OptionChain): Unit = {
    val chainMap = Map("expiry" -> optionChain.expiry, "expirations" -> optionChain.expirations, "underlying_id" -> optionChain.underlying_id, "underlying_price" -> optionChain.underlying_price)
    optionChain.puts foreach {
      processPut(_, chainMap)(put = true)
    }
    optionChain.puts foreach {
      processPut(_, chainMap)(put = false)
    }
  }

  def processPut(optionDetails: Map[String, String], chainDetails: Map[String, Any])(put: Boolean): Unit = model.getKey("identifier") match {
    case Some(s) =>
      optionDetails.get(s) match {
        case Some(x) =>
          blackboard ! CandidateOption(model, x, put, optionDetails, chainDetails)
        case None => log.warning(s"logic error: details does not support key: $s")
      }
    case None => log.warning(s"logic error: model ${model.getKey("name")} does not support key: identifier")
  }
}

case class YMD(y: Int, m: Int, d: Int) {

  import com.github.nscala_time.time.Imports._

  def asDate(ymd: YMD): DateTime = ymd match {
    case YMD(a, b, c) => new DateTime(a, b, c)
  }
}

case class OptionChain(expiry: YMD, expirations: Seq[YMD], puts: Seq[Map[String, String]], calls: Seq[Map[String, String]], underlying_id: String, underlying_price: Double)

object JsonGoogleOptionParser {

  import spray.httpx.SprayJsonSupport._
  import spray.httpx.unmarshalling._
  import spray.json.{DefaultJsonProtocol, _}

  object MyJsonProtocol extends DefaultJsonProtocol with NullOptions {
    implicit val ymdFormat: RootJsonFormat[YMD] = jsonFormat3(YMD)
    implicit val optionChainFormat: RootJsonFormat[OptionChain] = jsonFormat6(OptionChain)
  }

  import MyJsonProtocol._

  /**
    * This version of decode is a little more complex than usual because the Google
    * interface doesn't wrap attribute names in double quotes.
    *
    * @param entity the entity extracted from the Http Response
    * @return the deserialized version
    */
  def decode(entity: HttpEntity): Deserialized[OptionChain] = {
    //    import spray.httpx.unmarshalling._
    val contentType = ContentType(MediaTypes.`application/json`, HttpCharsets.`UTF-8`)
    entity match {
      case HttpEntity.NonEmpty(`contentType`, y) =>
        HttpEntity(contentType, fix(y)).as[OptionChain]
      case HttpEntity.NonEmpty(s, _) =>
        Left(MalformedContent(s"entity content type: $s"))
      case _ => Left(MalformedContent("logic error"))
    }
  }

  def fix(data: HttpData): Array[Byte] = fix(data.asString).getBytes

  def fix(s: String): String = """([^,{:\s]+):""".r.replaceAllIn(s, """"$1":""")

}
