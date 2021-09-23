/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.cache

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait Cache[K, V] extends (K => Future[V]) {

  /**
    * This function will take a key value (K) and yield a Future[V].
    * It is expected to perform this function by doing a web lookup, whatever.
    */
  val fulfill: K => Future[V]

  def expire(k: K): Unit

  def empty(): Unit
}

case class MyCache[K, V](fulfill: K => Future[V]) extends Cache[K, V] {

  private def put(k: K, v: V): Unit = cache += ((k, v))

  override def apply(k: K): Future[V] =
    if (cache.contains(k)) Future(cache(k)) else for (v <- fulfill(k); _ = put(k, v)) yield v

  def expire(k: K): Unit = cache -= k

  val cache: mutable.Map[K, V] = mutable.Map.empty

  def empty(): Unit = cache.empty
}
