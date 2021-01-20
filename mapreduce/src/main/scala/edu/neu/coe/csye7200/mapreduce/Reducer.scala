package edu.neu.coe.csye7200.mapreduce

import akka.actor.{Actor, ActorLogging}
//import scala.collection.mutable.HashMap
import scala.util._

/**
  * This actor performs the reduce operation on the received sequence of V2 objects,
  * resulting in an V3 object.
  * The incoming "Intermediate" message combines both the current K2 key and the sequence vs of V2 objects.
  * The reply message is a tuple of (K2,Either[Throwable,V3])
  *
  * Intermediate is a convenience incoming message wrapper. It has the advantage of not suffering type erasure.
  *
  * @author scalaprof
  * @tparam K2 key type
  * @tparam V2 value type
  * @tparam V3 the aggregation of V2 objects (in this form, must be super-type of V2)
  * @param g a function which takes a V3 (the accumulator) and a V2 (the value) and combines them into a V3
  */
class Reducer[K2, V2, V3 >: V2](g: (V3, V2) => V3) extends ReducerBase[K2, V2, V3] {
  def getValue(vs: Seq[V2]): V3 = vs.reduceLeft(g)
}

/**
  * This actor performs the reduce operation on the received sequence of V2 objects,
  * resulting in an V3 object.
  * The incoming "Intermediate" message combines both the current K2 key and the sequence vs of V2 objects.
  * The reply message is a tuple of (K2,Either[Throwable,V3])
  *
  * Intermediate is a convenience incoming message wrapper. It has the advantage of not suffering type erasure.
  *
  * @author scalaprof
  * @tparam K2 key type
  * @tparam V2 value type
  * @tparam V3 the aggregation of V2 objects
  * @param g a function which takes a V3 (the accumulator) and a V2 (the value) and combines them into a V3
  * @param z a function which provides an initial value for V3 (this allows us to use Fold rather than Reduce methods)
  */
class Reducer_Fold[K2, V2, V3](g: (V3, V2) => V3, z: => V3) extends ReducerBase[K2, V2, V3] {
  def getValue(vs: Seq[V2]): V3 = vs.foldLeft(z)(g)
}

abstract class ReducerBase[K2, V2, V3] extends Actor with ActorLogging {

  override def receive: Receive = {
    case i: Intermediate[K2, V2] =>
      log.info(s"received $i")
      log.debug(s"with elements ${i.vs}")
      sender() ! (i.k, Master.sequence(Try(getValue(i.vs))))
    case q =>
      log.warning(s"received unknown message type: $q")
  }

  override def postStop(): Unit = {
    log.debug("has shut down")
  }

  def getValue(vs: Seq[V2]): V3
}


case class Intermediate[K2, V2](k: K2, vs: Seq[V2]) {
  override def toString = s"Intermediate: with k=$k and ${vs.size} elements"
}
