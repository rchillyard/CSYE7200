package edu.neu.coe.csye7200.actors

import akka.actor.ActorRef

import scala.collection.mutable

/**
  * @author robinhillyard
  */
class MarketData(blackboard: ActorRef) extends BlackboardActor(blackboard) {

  /**
    * see definition of get(String)
    */
  val instruments: mutable.Map[String, Map[String, String]] = scala.collection.mutable.Map[String, Map[String, String]]()

  override def receive: PartialFunction[Any, Unit] = {
    case KnowledgeUpdate(model, identifier, update) =>
      log.debug("update to identifier: {}", identifier)
      instruments.put(identifier, update)
      // for a stock, we don't need additional attributes
      blackboard ! Confirmation(identifier, model, Map())

    // CONSIDER allowing key to be null in which case all attributes returned
    // Or allow key to be a list and always return a map of values
    case SymbolQuery(identifier, keys) =>
      println(s"symbol query received re: identifier: $identifier and keys $keys")
      log.debug("symbol query received re: identifier: {} and keys {}", identifier, keys)
      val attributes: List[Option[(String, String)]] = instruments.get(identifier) match {
        case Some(a) => keys map { k =>
          a.get(k) match {
            case Some(v) => Some(k -> v)
            case None => None
          }
        }
        case None => List()
      }
      import scala.language.postfixOps
      val x = attributes flatten
      val y = x toMap;
      log.debug(s"creating QueryResponse: $identifier $y")
      sender ! QueryResponse(identifier, y)

    case OptionQuery(key, value) =>
      log.debug("option query received re: key: {} and value {}", key, value)
      val optInstr = instruments find { case (_, v) => v.get(key) match {
        case Some(`value`) => true;
        case _ => false
      }
      }
      optInstr match {
        case Some((x, m)) => sender ! QueryResponse(x, m)
        case _ => log.warning("no match found for key: {}, value: {}", key, value); sender ! QueryResponse(null, null)
      }

    case m => super.receive(m)
  }

  /**
    * The key to the instruments collection is an "identifier".
    * In the case of stocks and similar instruments with a (ticker) symbol (or CUSIP),
    * then identifier is the symbol.
    * In the case of options, the identifier is option id.
    * //   * @param key the key (see above)
    * //   * @return the instruments value corresponding to key
    */
  //  private def get(key: String) = instruments.get(key)
}
