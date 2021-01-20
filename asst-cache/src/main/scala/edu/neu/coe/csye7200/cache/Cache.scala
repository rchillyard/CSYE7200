/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.cache

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait Cache[K, V] extends (K => Future[V])

trait ExpiringKey[K] {

  /**
    * Method to expire a key.
    *
    * @param k the key to expire.
    */
  def expire(k: K): Unit
}

trait Fulfillment[K, V] {

  /**
    * Function which fulfills a value for a key.
    * In the context of a Cache, we can invoke this function to get a value corresponding to the given key.
    *
    */
  val fulfill: K => Future[V]
}

case class FulfillingCache[K, V](fulfill: K => Future[V]) extends Cache[K, V] with ExpiringKey[K] with Fulfillment[K, V] {

  private def put(k: K, v: V): Unit = cache += (k -> v)

  def apply(k: K): Future[V] = if (cache.contains(k)) Future(cache(k)) else for (v <- fulfill(k); _ = put(k, v)) yield v

  def expire(k: K): Unit = cache -= k

  private val cache: mutable.Map[K, V] = mutable.Map.empty

  def empty(): Unit = cache.empty
}

object CacheFactory {

  def createCache[K, V](fulfill: K => Future[V]): Cache[K, V] = FulfillingCache(fulfill)

  def lookupStock(k: String): Future[Double] = Future(MockStock.lookupStock(k))

  def createStockCache: Cache[String, Double] = createCache(lookupStock)
}
