package edu.neu.coe.csye7200.mapreduce

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.Config

import scala.collection.immutable
import scala.concurrent.{Future, _}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util._

class Master[K1, V1, K2, V2, V3 >: V2](config: Config, f: (K1, V1) => (K2, V2), g: (V3, V2) => V3) extends MasterBase[K1, V1, K2, V2, V3](config, f, g, Master.zero) with ByReduce[K1, V1, K2, V2, V3]

class Master_Fold[K1, V1, K2, V2, V3](config: Config, f: (K1, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3) extends MasterBase[K1, V1, K2, V2, V3](config, f, g, z) with ByFold[K1, V1, K2, V2, V3]

class Master_First[V1, K2, V2, V3 >: V2](config: Config, f: (Unit, V1) => (K2, V2), g: (V3, V2) => V3) extends MasterSeqBase[V1, K2, V2, V3](config, f, g, Master.zero) with ByReduce[Unit, V1, K2, V2, V3]

class Master_First_Fold[V1, K2, V2, V3](config: Config, f: (Unit, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3) extends MasterSeqBase[V1, K2, V2, V3](config, f, g, z) with ByFold[Unit, V1, K2, V2, V3]

//noinspection ScalaUnusedSymbol
trait ByReduce[K1, V1, K2, V2, V3 >: V2] {
  def mapperProps(f: (K1, V1) => (K2, V2), config: Config): Props =
    if (config.getBoolean("forgiving")) Props.create(classOf[Mapper_Forgiving[K1, V1, K2, V2]], f) else Props.create(classOf[Mapper[K1, V1, K2, V2]], f)

  def reducerProps(f: (K1, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3): Props = Props.create(classOf[Reducer[K2, V2, V3]], g)
}

//noinspection ScalaUnusedSymbol
trait ByFold[K1, V1, K2, V2, V3] {
  def mapperProps(f: (K1, V1) => (K2, V2), config: Config): Props =
    if (config.getBoolean("forgiving")) Props.create(classOf[Mapper_Forgiving[K1, V1, K2, V2]], f) else Props.create(classOf[Mapper[K1, V1, K2, V2]], f)

  def reducerProps(f: (K1, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3): Props = Props.create(classOf[Reducer_Fold[K2, V2, V3]], g, z)
}

/**
  * Abstract class MasterSeqBase
  *
  * This version of the MasterBase class (which it extends) take a different type of message: to wit, a Seq[V1].
  * That is to say, there is no K1 type.
  *
  * @author scalaprof
  * @tparam V1 input type: the message which this actor responds to is of type Seq[X].
  * @tparam K2 key type: mapper groups things by this key and reducer processes said groups.
  * @tparam V2 transitional type -- used internally
  * @tparam V3 output type: the message which is sent on completion to the sender is of type Response[K2,V3]
  * @param f the mapper function which takes a V1 and creates a key-value tuple of type (K2,V2)
  * @param g the reducer function which combines two values (an V3 and a V2) into one V3
  */
abstract class MasterSeqBase[V1, K2, V2, V3](config: Config, f: (Unit, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3) extends MasterBase[Unit, V1, K2, V2, V3](config, f, g, z) {

  import context.dispatcher

  override def receive: PartialFunction[Any, Unit] = {
    case v1s: Seq[V1] =>
      log.info(s"received Seq[V]: with ${v1s.length} elements")
      val caller = sender()
      doMapReduce(Incoming.sequence[Unit, V1](v1s)).onComplete {
        case Success(v3XeK2m) => caller ! Response(v3XeK2m)
        case Failure(x) => caller ! akka.actor.Status.Failure(x)
      }
    case q =>
      log.warning(s"received unknown message type: $q")
  }
}

/**
  * @author scalaprof
  * @tparam K1 key type: input may be organized by this key (may be "Unit").
  * @tparam V1 input type: the message which this actor responds to is of type Map[K1,V1]
  * @tparam K2 key type: mapper groups things by this key and reducer processes said groups.
  * @tparam V2 transitional type -- used internally
  * @tparam V3 output type: the message which is sent on completion to the sender is of type Response[K2,V3]
  * @param f the mapper function which takes a K1,V1 pair and creates a key-value tuple of type (K2,V2)
  * @param g the reducer function which combines two values (an V3 and a V2) into one V3
  */
abstract class MasterBase[K1, V1, K2, V2, V3](config: Config, f: (K1, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3) extends Actor with ActorLogging {
  implicit val timeout: Timeout = Timeout(5 seconds)
  val name: String = config.getString("name")
  val mapper: ActorRef = context.actorOf(mapperProps(f, config), s"mpr")
  val reducers: immutable.IndexedSeq[ActorRef] = for (i <- 1 to config.getInt("reducers")) yield context.actorOf(reducerProps(f, g, z), s"rdcr-$i")

  import context.dispatcher

  def mapperProps(f: (K1, V1) => (K2, V2), config: Config): Props

  def reducerProps(f: (K1, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3): Props

  override def postStop(): Unit = {
    log.debug("has shut down")
  }

  override def receive: Receive = {
    case v1K1m: Map[K1, V1] =>
      log.info(s"received Map[K1,V1]: with ${v1K1m.size} elements")
      maybeLog("received", v1K1m)
      val caller = sender()
      doMapReduce(Incoming.map[K1, V1](v1K1m)).onComplete {
        case Success(v3XeK2m) =>
          maybeLog("response", v3XeK2m)
          caller ! Response(v3XeK2m)
        case Failure(x) =>
          log.warning(s"no response--failure:", x)
          caller ! akka.actor.Status.Failure(x)
      }
    case q =>
      log.warning(s"received unknown message type: $q")
  }

  //noinspection ScalaUnusedSymbol
  def doMapReduce(i: Incoming[K1, V1]): Future[Map[K2, Either[Throwable, V3]]] = for {
    v2sK2m <- doMap(i)
    z = maybeLog("shuffle", v2sK2m)
    v3XeK2m <- doDistributeReduceCollate(v2sK2m)
  } yield v3XeK2m

  def maybeLog(w: String, z: Any): Unit = if (log.isDebugEnabled) log.debug(s"$w: $z")
  private def doMap(i: Incoming[K1, V1]): Future[Map[K2, Seq[V2]]] = {
    val reply = mapper ? i
    if (config.getBoolean("forgiving"))
      reply.mapTo[(Map[K2, Seq[V2]], Seq[Throwable])] map { case (v2sK2m, xs) => for (x <- xs) log.warning("mapper exception:", x); v2sK2m }
    else {
      val v2sK2mtf = reply.mapTo[Try[Map[K2, Seq[V2]]]]
      Master.flatten(v2sK2mtf)
    }
  }

  private def doDistributeReduceCollate(v2sK2m: Map[K2, Seq[V2]]): Future[Map[K2, Either[Throwable, V3]]] = {
    if (v2sK2m.isEmpty) log.warning("mapper returned empty map" + (if (config.getBoolean("forgiving")) ""
    else ": see log for problem and consider using Mapper_Forgiving instead"
    ) )
    maybeLog("doDistributeReduceCollate", v2sK2m)
    val rs = LazyList.continually(reducers.to(LazyList)).flatten
    val v2sK2s = for ((k2, v2s) <- v2sK2m.toSeq) yield (k2, v2s)
    val v3XeK2fs = for (((k2, v2s), a) <- v2sK2s zip rs) yield (a ? Intermediate(k2, v2s)).mapTo[(K2, Either[Throwable, V3])]
    // CONSIDER using traverse
    for (v3XeK2s <- Future.sequence(v3XeK2fs)) yield v3XeK2s.toMap
  }
}

case class Response[K, V](left: Map[K, Throwable], right: Map[K, V]) {
  override def toString = s"left: $left; right: $right"

  def size: Int = right.size
}

object Response {
  def apply[K, V](vXeKm: Map[K, Either[Throwable, V]]): Response[K, V] = {
    val t = Master.toMap(Master.sequenceLeftRight(vXeKm))
    new Response(t._1, t._2)
  }
}

object Master {
  def zero[V3](): V3 = 0.asInstanceOf[V3]

  // CONSIDER moving all these to MonadOps
  /**
    * Method sequence which applied to a Try[X] returns an Either[Throwable,X].
    *
    * @param xt : Try[X]
    * @return : Either[Throwable,X]
    */
  def sequence[X](xt: Try[X]): Either[Throwable, X] = xt match {
    case Success(s) => Right(s);
    case Failure(e) => Left(e)
  }

  /**
    * Method sequence which, applied to a Seq[Try [X] ], returns a Try[Seq [X] ]
    *
    * @param xts : Seq[Try [X] ]
    * @return : Try[Seq [X] ]
    */
  def sequence[X](xts: Seq[Try[X]]): Try[Seq[X]] = xts.foldLeft(Try(Seq[X]())) { (xst, xt) => for (xs <- xst; x <- xt) yield xs :+ x }

  def flatten[X](xyf: Future[Try[X]])(implicit executor: ExecutionContext): Future[X] = {
    def convert[W](wy: Try[W]): Future[W] = {
      val wp = Promise[W]()
      wy match {
        case Success(y) => wp complete Success(y)
        case Failure(e) => wp complete Failure(e)
      }
      wp.future
    }

    for (xy <- xyf; x <- convert(xy)) yield x
  }

  def sequence[K, V, X](vXeKm: Map[K, Either[X, V]]): (Map[K, X], Map[K, V]) = toMap(sequenceLeftRight(vXeKm))

  // TODO remove the get invocation here
  def sequenceLeft[K, V, X](vXeKs: Seq[(K, Either[X, V])]): Seq[(K, X)] = for ((k, e) <- vXeKs) yield (k, e.swap.toOption.get)

  // TODO remove the get invocation here
  def sequenceRight[K, V, X](vXeKs: Seq[(K, Either[X, V])]): Seq[(K, V)] = for ((k, e) <- vXeKs) yield (k, e.toOption.get)

  def tupleMap[L1, L2, R1, R2](fl: L1 => L2, fr: R1 => R2)(t: (L1, R1)): (L2, R2) = (fl(t._1), fr(t._2))

  def partition[K, V, X](vXeKm: Map[K, Either[X, V]]): (Seq[(K, Either[X, V])], Seq[(K, Either[X, V])]) = vXeKm.toSeq.partition({ case (_, v) => v.isLeft })

  def toMap[K, V, X](t: (Seq[(K, X)], Seq[(K, V)])): (Map[K, X], Map[K, V]) = (t._1.toMap, t._2.toMap)

  def sequenceLeftRight[K, V, X](vXeKm: Map[K, Either[X, V]]): (Seq[(K, X)], Seq[(K, V)]) = tupleMap[Seq[(K, Either[X, V])], Seq[(K, X)], Seq[(K, Either[X, V])], Seq[(K, V)]](sequenceLeft, sequenceRight)(partition(vXeKm))
}
