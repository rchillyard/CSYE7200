package com.phasmidsoftware

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, StatusCode}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Merge, Source}
import akka.util.ByteString
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/**
  * Trait to define methods involved with creating a Flow of Ts.
  *
  * @tparam Progenitor the processor input type.
  * @tparam Element    the type which is the intermediate data type.
  */
trait Flow[Progenitor, Element] {
  def getElements(n: Int = Int.MaxValue, responseMap: Map[String, Any]): List[Element]

  def getElementsFromURL(n: Int, url: String, header: Map[String, String], timeout: Int): List[Element] =
    getElements(n, Flow.invokeHttpRequest(url, header, timeout))

  def getElementsFromURLJava(n: Int, url: String, header: java.util.Map[String, String], timeout: Int): java.util.List[Element] = {
    import scala.collection.JavaConverters._
    import scala.language.implicitConversions

    val mutableMap: mutable.Map[String, String] = header.asScala
    val accounts: List[Element] = getElementsFromURL(n, url, mutableMap.toMap, timeout)
    val buffer: mutable.Buffer[Element] = accounts.toBuffer
    buffer.asJava
  }

  /**
    * Method to process the entities of an element using merged stream.
    *
    * @param progenitor the progenitor.
    * @param element    the account.
    * @return a Future [ Seq [ Status ] ].
    */
  def processEntitiesForElementWithSinkMerged(progenitor: Progenitor)(element: Element): Future[Seq[Status]]

  /**
    * Method to process the entities of an element using separate streams and then sequencing them together.
    *
    * @param progenitor the progenitor.
    * @param element    the account.
    * @return a Future [ Seq [ Status ] ].
    */
  def processEntitiesForAccountWithSinkSeparately(progenitor: Progenitor)(element: Element): Future[Seq[Status]]

  /**
    * Process a list of Elements, by applying the processor function which takes a Progenitor and returns an Element => Future[ Seq[ Status ] ].
    *
    * @param processor the processor.
    * @param elements  the elements.
    */
  def processElements(processor: Progenitor => Element => Future[Seq[Status]])(elements: List[Element]): Unit

  def processElementsJava(processor: Element => java.util.List[Status], elements: java.util.List[Element], datasets: java.util.List[SourceDataSet]): Unit

  def processFailures(xsf: Future[Seq[Status]])(implicit executor: ExecutionContext): Future[Seq[Status]] = xsf recoverWith[Seq[Status]] { case f => Future.successful(Seq(Status(f))) }

  def close(): Unit
}

object Flow extends LazyLogging {

  /**
    * A method which transforms a (materialized) Query to a future of a (materialized) Seq[Result].
    * The transformation is undertaken by a stream flow source to sink.
    * The stages in this flow are:
    * <ol>
    * <li>Evaluate and link: take a Query object and the function evaluateAndLink and invoke Source.unfoldAsync on them (which generates a Source[Payload]).
    * The Source.unfoldAsync method feeds back a new request which is the eventual, optional result of invoking evaluateAndLink which generates the next element in the stream.
    * Once the optional results proves to be None, the Source is finished.</li>
    * <li>Process: take the resulting stream from the first stage (a Source[Payload]) and invoke mapAsync with the processFunc.
    * The process func is the function which does something useful with the payloads that arrive.
    * An example might be to write the payload out to S3 or to transform it into some other format.</li>
    * <li>Sink: take the stream which arrives (a Source[Result]) and aggregate the elements together into a Seq[Result], using a fold based on simply adding each element at the end.
    * </ol>
    *
    * CONSIDER recasting this as a Flow.
    *
    * @param query           the initial Query value to be used.
    * @param evaluateAndLink a function Query => Future[ Option[ (Query, Payload) ] ].
    * @param processFunc     a function Payload => Future[Result].
    * @tparam Query   the query type. An example of a Query type in the Clever domain might be a tuple of HttpRequest and AccountRequestPair.
    * @tparam Payload the payload type. An example of Payload is a JSON string (CONSIDER decoding the JSON).
    * @tparam Result  the result type. An example of Result type is a Status object.
    * @return a Future [ Seq [Result] ] which is a sequence of all of the results which were returned from the processFunc method.
    */
  def flowLinkProcessFold[Query, Payload, Result](evaluateAndLink: Query => Future[Option[(Query, Payload)]], processFunc: Payload => Future[Result])(query: Query)(implicit akkaPak: AkkaPak): Future[Seq[Result]] =
    Source.unfoldAsync[Query, Payload](query)(evaluateAndLink)
      .log("flowLinkProcessFold: stream error in evaluate and link stage")
      .mapAsync(2)(processFunc)
      .log("Stream error in process stage")
      .runFold[Seq[Result]](Nil)((a, b) => a :+ b)(akkaPak.mat)

  /**
    * A method which transforms a (materialized) sequence of Query instances to a future of a (materialized) Seq[Result].
    * The transformation is undertaken by a stream flow source to sink.
    * The stages in this flow are:
    * <ol>
    * <li>Evaluate and link: take a Query object and the function evaluateAndLink and invoke Source.unfoldAsync on them (which generates a Source[Payload]).
    * The Source.unfoldAsync method feeds back a new request which is the eventual, optional result of invoking evaluateAndLink which generates the next element in the stream.
    * Once the optional results proves to be None, the Source is finished.</li>
    * <li>Merge: the sources resulting from the first stage are merged together (on a first come first served basis) into a single stream, i.e. Source[Payload].
    * <li>Process: take the resulting stream from the first stage (a Source[Payload]) and invoke mapAsync with the processFunc.
    * The process func is the function which does something useful with the payloads that arrive.
    * An example might be to write the payload out to S3 or to transform it into some other format.</li>
    * <li>Sink: take the stream which arrives (a Source[Result]) and aggregate the elements together into a Seq[Result], using a fold based on simply adding each element at the end.
    * </ol>
    *
    * CONSIDER recasting this as a Flow.
    *
    * @param queries         a sequence of initial Query values.
    * @param evaluateAndLink a function Query => Future[ Option[ (Query, Payload) ] ].
    * @param processFunc     a function Payload => Future[Result].
    * @tparam Query   the query type. An example of a Query type in the Clever domain might be a tuple of HttpRequest and AccountRequestPair.
    * @tparam Payload the payload type. An example of Payload is a JSON string (CONSIDER decoding the JSON).
    * @tparam Result  the result type. An example of Result type is a Status object.
    * @return a Future [ Seq [Result] ] which is a sequence of all of the results which were returned from the processFunc method.
    */
  def flowLinkMergeProcessFold[Query, Payload, Result]
  (evaluateAndLink: Query => Future[Option[(Query, Payload)]], processFunc: Payload => Future[Result])
  (queries: Seq[Query])
  (implicit akkaPak: AkkaPak): Future[Seq[Result]] =
    (for (query <- queries) yield Source.unfoldAsync[Query, Payload](query)(evaluateAndLink).log("flowLinkMergeProcessFold: stream error in evaluate and link stage"))
      .foldLeft(Source.empty[Payload])(mergeSources)
      .log("flowLinkMergeProcessFold: stream error in merge stage")
      .mapAsync(2)(processFunc)
      .log("flowLinkMergeProcessFold: stream error in process stage")
      .runFold[Seq[Result]](Nil)((a, b) => a :+ b)(akkaPak.mat)

  /**
    * Method to decode an HttpResponse (in particular, the response.entity.dataBytes) into a Payload.
    *
    * @param converter the function which will convert a ByteString into a Payload.
    * @param predicate the function to determine if we should attempt to convert the given HttpResponse into a Payload.
    * @param r         an HttpResponse.
    * @param akkaPak   (implicit)
    * @tparam Payload the payload type.
    * @return a Future[Payload].
    */
  def extractPayloadFromResponse[Payload](predicate: HttpResponse => Boolean, converter: ByteString => Payload)
                                         (r: HttpResponse)
                                         (implicit akkaPak: AkkaPak):
  Future[Payload] = {
    implicit val (materializer: ActorMaterializer, ec: ExecutionContext, sys: ActorSystem) = AkkaPak.unapply(akkaPak).get
    if (predicate(r))
      r.entity.dataBytes.runFold(ByteString(""))(_ ++ _) map converter
    else {
      r.entity.discardBytes()
      val status: StatusCode = r.status
      Future.failed(HttpException(status.intValue(), s"HttpResponse was not OK because: ${status.defaultMessage()}"))
    }
  }

  /**
    * Method to determine if an HttpResponse has OK status.
    * As a side-effect, a warning log message is appended if the response's status is not OK.
    *
    * @param r the response.
    * @return true if the status is success.
    */
  def statusIsSuccess(r: HttpResponse): Boolean = {
    val result = r.status.isSuccess()
    if (!result) logger.warn(s"HttpResponse is not OK: $r")
    result
  }

  /**
    * Method to take an HttpRequest/Friend pair, submit the request (using the submit function provided),
    * then invoke extractPayloadFromResponse to unpack the HttpResponse, using the unpack function and the statusIsSuccess guard function.
    * Finally, any failures have their exceptions transformed into a FlowException with added context.
    * The friend object is used to add context to any exceptions.
    *
    * @param submit            a function which takes an HttpRequest and yields a Future[HttpResponse].
    * @param unpack            a function which take a ByteString and unpacks it into a Payload.
    * @param requestWithFriend a tuple of HttpRequest and Friend.
    * @param akkaPak           (implicit)
    * @tparam Friend  the type of the friend object.
    * @tparam Payload the type of the payload object.
    * @return a Future[Payload].
    */
  def submitQueryAndExtractPayload[Payload, Friend](submit: HttpRequest => Future[HttpResponse])
                                                   (unpack: ByteString => Payload)
                                                   (requestWithFriend: (HttpRequest, Friend))
                                                   (implicit akkaPak: AkkaPak):
  Future[Payload] = {
    val (request, friend) = requestWithFriend
    val improveException: Throwable => Throwable = {
      case FlowException(m, c) => FlowException(s"$m: with additional information: friend=$friend, request=$request", c)
      case HttpException(s, m) => HttpException(s, s"$m: with additional information: friend=$friend, request=$request")
      case x => x
    }
    implicit val (materializer: ActorMaterializer, ec: ExecutionContext, sys: ActorSystem) = AkkaPak.unapply(akkaPak).get
    (submit(request) flatMap extractPayloadFromResponse(statusIsSuccess, unpack))
      .transform(identity[Payload], improveException)
  }

  /**
    * A method which is used as the evaluateAndLink function in the flows defined by flowLinkProcessFold
    * and flowLinkMergeProcessFold (above).
    * It invokes submitQueryAndExtractPayload and uses the result from that to "chain" a new request.
    * The particular use case for this is Clever, where a request provides the address of the next batch of records.
    *
    * @param submit            a function which takes an HttpRequest and yields a Future[HttpResponse].
    * @param unpack            a function which take a ByteString and unpacks it into a Payload.
    * @param requestWithFriend a tuple of HttpRequest and Friend.
    * @param feedback          a function which takes an Friend and a Payload and yields an optional appropriate HttpRequest.
    * @param akkaPak           (implicit)
    * @tparam Friend  the type of the friend object.
    * @tparam Payload the type of the payload object.
    * @return a Future [ Option [ (HttpRequest, Friend), (Payload, Friend) ] ].
    */
  def submitQueryAndExtractPayloadWithChaining[Friend, Payload](submit: HttpRequest => Future[HttpResponse])
                                                               (feedback: (Friend, Payload) => Option[(HttpRequest, Friend)], unpack: ByteString => Payload)
                                                               (requestWithFriend: (HttpRequest, Friend))
                                                               (implicit akkaPak: AkkaPak):
  Future[Option[((HttpRequest, Friend), (Payload, Friend))]] = {
    implicit val (materializer: ActorMaterializer, ec: ExecutionContext, sys: ActorSystem) = AkkaPak.unapply(akkaPak).get
    val (_, friend) = requestWithFriend
    submitQueryAndExtractPayload(submit)(unpack)(requestWithFriend)
      .map(p => zip(feedback(friend, p), Some(p -> friend)))
  }

  /**
    * The standard submission method for an HttpRequest.
    *
    * @param req    the HttpRequest.
    * @param system (implicit)
    * @return an HttpResponse.
    */
  def submitStandard(req: HttpRequest)(implicit system: ActorSystem): Future[HttpResponse] = Http().singleRequest(req)

  /**
    * Method to get the next HttpRequest when requests are linked, as they are in the Clever datasets.
    *
    * @param generator a function to generate an optional HttpRequest based on the (Friend, Payload) pair.
    * @param payload   the payload object which has been decoded from a previous HttpResponse.
    * @param friend    a "friend" object which helps bind the various requests and responses together. A kind of identifier.
    * @tparam Friend  the type of a friend.
    * @tparam Payload the type of the payload
    * @return an optional pair of HttpRequest and Friend. A None result simply means that we are at the end of the chain--no more requests are needed.
    */
  def getNextRequest[Friend, Payload](generator: (Friend, Payload) => Option[HttpRequest])(payload: Payload, friend: Friend): Option[(HttpRequest, Friend)] =
    generator(friend, payload) map (h => h -> friend)

  /**
    * Method to merge two sources together.
    *
    * @param s1 first Source[T].
    * @param s2 second Source[T]
    * @tparam T the underlying type of the sources.
    * @return a new Source[T] which includes the elements of both sources.
    */
  def mergeSources[T](s1: Source[T, NotUsed], s2: Source[T, NotUsed]): Source[T, NotUsed] = Source.combine(s1, s2)(Merge(_))

  /**
    * Method to zip two optional objects together. Not sure why this isn't in the Option object.
    *
    * TODO make private.
    *
    * @param to an Option[T].
    * @param uo an Option[U].
    * @tparam T the underlying type of to.
    * @tparam U the underlying type of uo.
    * @return an Option[(T, U)].
    */
  def zip[T, U](to: Option[T], uo: Option[U]): Option[(T, U)] = to match {
    case Some(t) => uo match {
      case Some(u) => Some(t -> u)
      case _ => None
    }
    case _ => None
  }

  /**
    * Invoke an HttpRequest to return a Map[String,Any].
    *
    * CONSIDER genericizing this method.
    *
    * @return a Map[String, Any]
    */
  def invokeHttpRequest(url: String, header: Map[String, String], timeout: Int): Map[String, Any] = {
    import scalaj.http.{Http, HttpOptions, HttpRequest}

    val httpRequest = header.toSeq.foldLeft[HttpRequest](Http(url).option(HttpOptions.readTimeout(timeout))) { (h, kV) => h.header(kV._1, kV._2) }
    jsonStrToMap(httpRequest.asString.body)
  }

  import org.json4s.DefaultFormats
  import org.json4s.jackson.JsonMethods.parse

  def jsonStrToMap(jsonStr: String): Map[String, Any] = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
    parse(jsonStr).extract[Map[String, Any]]
  }
}

case class AkkaPak(mat: ActorMaterializer, ec: ExecutionContext, sys: ActorSystem)

/**
  * Case class to describe the status of an element's flow.
  * NOTE: it's OK to have cause with default of null: the underlying log methods are written in Java and expect this.
  *
  * @param code      code (0 is OK)
  * @param isSuccess true or false depending on success.
  * @param attribute undefined.
  */
case class Status(code: Int, isSuccess: Boolean, attribute: String = "", cause: Throwable = null) extends LazyLogging {
  /**
    * Method to log any failures and return 1 for a success, 0 for a failure.
    *
    * @return
    */
  lazy val logIt: Int = if (!isSuccess) {
    logger.debug(s"""Status: failure: code=$code, attribute="$attribute"""", cause)
    0
  }
  else 1
}

object Status {

  def apply(): Status = Status(0, isSuccess = true)

  def apply(x: Throwable): Status = x match {
    case HttpException(status, msg) => apply(status, isSuccess = false, msg)
    case _ => apply(-1, isSuccess = false, "", x)
  }

  trait HasStatusHttpResponse extends HasStatus[HttpResponse] {
    override def status(t: HttpResponse): Status = Status(t.status.intValue(), t.status.isSuccess())
  }

  implicit object HasStatusHttpResponse extends HasStatusHttpResponse

}

/**
  * Typeclass to apply status to other types.
  *
  * @tparam T the destination type.
  */
trait HasStatus[T] {
  def status(t: T): Status
}


case class FlowException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

case class HttpException(status: Int, msg: String) extends Exception(msg)
