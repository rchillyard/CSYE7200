package edu.neu.coe.csye7200.mapreduce

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.Config
import scala.annotation.unused
import scala.collection.immutable
import scala.concurrent.duration._
import scala.concurrent._
import scala.language.postfixOps
import scala.util._

/**
 * Master is a parameterized class designed for processing and transforming data using two functions.
 * It extends MasterBase and mixes in the ByReduce trait to provide additional functionality.
 *
 * @param config Configuration object used to initialize and control the behavior of the Master.
 * @param f      Transformation function that takes a key-value pair of type (K1, V1)
 *               and produces a new key-value pair of type (K2, V2).
 * @param g      Reduction function that combines two values into one, of type (V3, V2) => V3.
 * @tparam K1 Type of the key in the input data.
 * @tparam V1 Type of the value in the input data.
 * @tparam K2 Type of the key in the transformed data.
 * @tparam V2 Type of the value in the transformed data.
 * @tparam V3 Type of the result produced after reduction.
 */
class Master[K1, V1, K2, V2, V3 >: V2](config: Config, f: (K1, V1) => (K2, V2), g: (V3, V2) => V3) extends
        MasterBase[K1, V1, K2, V2, V3](config, f, g, Master.zero)
        with ByReduce[K1, V1, K2, V2, V3]

/**
 * Represents a specialized implementation of the MasterBase class that performs
 * operations using folding mechanisms. The class processes input key-value pairs
 * and applies transformations and aggregations defined by the provided functions.
 *
 * @tparam K1 The type of the input key.
 * @tparam V1 The type of the input value associated with the input key.
 * @tparam K2 The type of the output key after the transformation function is applied.
 * @tparam V2 The type of the output value after the transformation function is applied.
 * @tparam V3 The type of the aggregated result produced by the folding mechanism.
 * @param config The configuration object containing the required settings for the operation.
 * @param f      A transformation function that takes an input key-value pair of type (K1, V1)
 *               and produces an output key-value pair of type (K2, V2).
 * @param g      A folding function that aggregates results. It takes the current aggregated result
 *               of type V3 and a value of type V2 to produce a new aggregated result of type V3.
 * @param z      A zero-value function that initializes the aggregated result of type V3.
 */
class Master_Fold[K1, V1, K2, V2, V3](config: Config, f: (K1, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3) extends
        MasterBase[K1, V1, K2, V2, V3](config, f, g, z)
        with ByFold[K1, V1, K2, V2, V3]

/**
 * Master_First is a specific implementation of MasterSeqBase and ByReduce that handles map-reduce operations
 * for a sequence of input values, where the inputs do not have an associated key type (Unit is used as the key type).
 * This class performs the map-reduce computation using a strict reducer function and leveraging Master.zero
 * to initialize the accumulator.
 *
 * @tparam V1 The input type that represents each element in the sequence being processed.
 * @tparam K2 The key type used for grouping values during the map step.
 * @tparam V2 The intermediate type produced by the mapper function and consumed by the reducer function.
 * @tparam V3 The output type produced by the reducer function, must be a supertype of V2.
 * @param config The configuration used to initialize actor properties and map-reduce logic.
 * @param f      The mapper function that transforms each input of type V1 into a key-value pair of type (K2, V2).
 * @param g      The reducer function that combines an accumulated value of type V3 with a new input of type V2 into a new value of type V3.
 */
class Master_First[V1, K2, V2, V3 >: V2](config: Config, f: (Unit, V1) => (K2, V2), g: (V3, V2) => V3) extends
        MasterSeqBase[V1, K2, V2, V3](config, f, g, Master.zero)
        with ByReduce[Unit, V1, K2, V2, V3]

/**
 * Master_First_Fold is a concrete implementation of MasterSeqBase and ByFold.
 * It performs map-reduce operations using provided mapper and reducer functions
 * while working with sequential collections of input values.
 *
 * @tparam V1 the type of input value received by the mapper
 * @tparam K2 the type of key by which the data is grouped
 * @tparam V2 the intermediate type produced by the mapper and consumed by the reducer
 * @tparam V3 the final output type produced by the reducer
 * @param config the configuration settings for the actor
 * @param f      the mapper function transforming a pair (Unit, V1) into a key-value tuple of type (K2, V2)
 * @param g      the reducer function combining a V3 accumulator and a V2 value into a single V3
 * @param z      a zero function producing the initial value for the reducer's V3 accumulator
 */
class Master_First_Fold[V1, K2, V2, V3](config: Config, f: (Unit, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3) extends
        MasterSeqBase[V1, K2, V2, V3](config, f, g, z)
        with ByFold[Unit, V1, K2, V2, V3]

/**
 * Trait ByReduce defines methods to generate Props configurations for mapper and reducer actors.
 * It provides utility methods to create actor properties for both strict and forgiving mappers,
 * as well as reducers that perform reduction operations.
 *
 * @tparam K1 input key type for the mapper
 * @tparam V1 input value type for the mapper
 * @tparam K2 output key type from the mapper
 * @tparam V2 intermediate value type after mapping
 * @tparam V3 aggregated value type after reduction, must be a super-type of V2
 */
//noinspection ScalaUnusedSymbol
trait ByReduce[K1, V1, K2, V2, V3 >: V2] {
  def mapperProps(f: (K1, V1) => (K2, V2), config: Config): Props =
    if (config.getBoolean("forgiving"))
      Props.create(classOf[Mapper_Forgiving[K1, V1, K2, V2]], f)
    else
      Props.create(classOf[Mapper[K1, V1, K2, V2]], f)

  // TODO f is never used.
  // TODO z is never used.
  def reducerProps(@unused f: (K1, V1) => (K2, V2), g: (V3, V2) => V3, @unused z: () => V3): Props =
    Props.create(classOf[Reducer[K2, V2, V3]], g)
}

/**
 * Defines a trait for functionality that combines mapping and reducing operations.
 * This is achieved by providing factory methods for creating actor properties (`Props`) for mappers and reducers.
 *
 * @tparam K1 the type of the input key for the mapper
 * @tparam V1 the type of the input value for the mapper
 * @tparam K2 the type of the output key for the mapper
 * @tparam V2 the type of the intermediate value for the mapper and reducer
 * @tparam V3 the type of the final reduced value
 */
//noinspection ScalaUnusedSymbol
trait ByFold[K1, V1, K2, V2, V3] {
  /**
   * Creates `Props` for a mapper actor based on the provided mapping function and configuration.
   * Depending on the configuration, it returns a `Props` for either a forgiving mapper or a strict mapper.
   *
   * @param f      a function that defines the mapping logic, transforming a `(K1, V1)` pair into a `(K2, V2)` pair
   * @param config the configuration object that determines whether to create a forgiving or strict mapper
   * @return a `Props` object to create the mapper actor
   */
  def mapperProps(f: (K1, V1) => (K2, V2), config: Config): Props =
    if (config.getBoolean("forgiving"))
      Props.create(classOf[Mapper_Forgiving[K1, V1, K2, V2]], f)
    else
      Props.create(classOf[Mapper[K1, V1, K2, V2]], f)

  /**
   * Creates `Props` for a reducer actor based on the provided reducing function and initial value.
   * TODO f is never used.
   *
   * @param f an unused function that would define the mapping logic, transforming a `(K1, V1)` pair into a `(K2, V2)` pair
   * @param g a reducing function that combines a `V3` (the accumulator) and a `V2` (the value) into a `V3`
   * @param z a zero function that produces the initial value for `V3`
   * @return a `Props` object to create the reducer actor
   */
  def reducerProps(@unused f: (K1, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3): Props =
    Props.create(classOf[Reducer_Fold[K2, V2, V3]], g, z)
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
abstract class MasterSeqBase[V1, K2, V2, V3](config: Config, f: (Unit, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3) extends
        MasterBase[Unit, V1, K2, V2, V3](config, f, g, z) {

  import context.dispatcher

  /**
   * Handles incoming messages for the actor.
   * Recognizes specific messages of type Seq[V1], logs their size, and processes them through a map-reduce computation.
   * Responds back to the sender with either the successful map-reduce results or a failure.
   * Logs a warning if receiving unknown message types.
   *
   * @return A PartialFunction that processes a sequence of V1 elements or logs warnings for unknown message types.
   */
  override def receive: PartialFunction[Any, Unit] = {
    case v1s: Seq[V1]@unchecked =>
      log.info(s"received Seq[V]: with ${v1s.length} elements")
      val caller = sender()
      doMapReduce(Incoming.sequence[Unit, V1](v1s)).onComplete {
        case Success(v3XeK2m) =>
          caller ! Response(v3XeK2m)
        case Failure(x) =>
          caller ! akka.actor.Status.Failure(x)
      }
    case q =>
      log.warning(s"received unknown message type: $q")
  }
}

/**
 * An abstract base class representing a master actor in a map-reduce architecture. This actor coordinates the
 * mapping, reduction, and collation of data using mapper and reducer actors. It initializes and manages
 * child actors and implements the map-reduce logic.
 *
 * @tparam K1 The type of the input keys for the map operation.
 * @tparam V1 The type of the input values for the map operation.
 * @tparam K2 The type of the intermediate keys resulting from the map operation.
 * @tparam V2 The type of the intermediate values resulting from the map operation.
 * @tparam V3 The type of the final, reduced values.
 * @param config Configuration object that provides settings for the master, such as actor properties
 *               and the number of reducer actors.
 * @param f      A mapping function that transforms key-value pairs of type `(K1, V1)` into intermediate
 *               key-value pairs of type `(K2, V2)`.
 * @param g      A reduction function that combines cumulative values of type `V3` with intermediate values
 *               of type `V2` to produce aggregated results of type `V3`.
 * @param z      A function returning a zero-initialized value of type `V3`, used as the starting value
 *               for reduction operations.
 */
abstract class MasterBase[K1, V1, K2, V2, V3](config: Config, f: (K1, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3) extends
        Actor with ActorLogging {
  given timeout: Timeout = Timeout(5 seconds)
  val name: String = config.getString("name")
  val mapper: ActorRef = context.actorOf(mapperProps(f, config), s"mpr")
  val reducers: immutable.IndexedSeq[ActorRef] = for (i <- 1 to config.getInt("reducers")) yield context.actorOf(reducerProps(f, g, z), s"rdcr-$i")

  import context.dispatcher

  /**
   * Creates an Akka `Props` object to configure and instantiate a mapper actor.
   *
   * @param f      A function that takes a key-value pair of type `(K1, V1)` and transforms it into a new key-value pair of type `(K2, V2)`.
   * @param config Configuration settings of type `Config` to be used for the actor initialization.
   * @return An Akka `Props` object for creating a mapper actor with the specified transformation function and configuration.
   */
  def mapperProps(f: (K1, V1) => (K2, V2), config: Config): Props

  /**
   * Creates properties required for an actor to function as a reducer in a map-reduce system.
   * The reducer combines intermediate results into a final output using the provided functions.
   *
   * @param f A function that processes input key-value pairs of types `(K1, V1)`
   *          and produces intermediate key-value pairs of types `(K2, V2)`.
   * @param g A reduction function that combines a cumulative value of type `V3`
   *          with a value of type `V2` to produce a new cumulative value of type `V3`.
   * @param z A zero-value generator function that provides an initial cumulative value of type `V3`.
   *          The zero value is used as a starting point for the reduction operation.
   * @return A `Props` object that encapsulates configuration settings for the reducer actor.
   */
  def reducerProps(f: (K1, V1) => (K2, V2), g: (V3, V2) => V3, z: () => V3): Props

  /**
   * Called when the actor is stopped to perform cleanup or final actions.
   * Logs a debug message indicating the actor has been shut down.
   *
   * @return Unit (does not return a value, intended for side effects only).
   */
  override def postStop(): Unit =
    log.debug("has shut down")

  /**
   * Handles different incoming messages and performs appropriate actions:
   * - If a message is a map `Map[K1, V1]`, it logs the received data, processes it using a map-reduce
   * operation, and sends the resulting response back to the sender.
   * - If a message is of an unknown type, it logs a warning.
   *
   * @return A partial function that matches incoming messages, processes them,
   *         and defines the behavior of the actor for those messages.
   */
  override def receive: Receive = {
    case v1K1m: Map[K1, V1]@unchecked =>
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

  /**
   * Logs a debug message if debug logging is enabled.
   *
   * @param w A string that serves as a key or label for the log message.
   * @param z The object or data to include in the log message, typically representing context or value information.
   * @return Unit (this method does not return a value but performs side-effect logging).
   */
  def maybeLog(w: String, z: Any): Unit =
    if (log.isDebugEnabled) log.debug(s"$w: $z")

  /**
   * Processes the given `Incoming` data using a mapping operation and returns a future containing
   * a map of grouped results. The behavior of the method depends on the configuration setting
   * for error handling.
   *
   * @param i The input data of type `Incoming[K1, V1]`, containing a sequence of key-value pairs to be processed.
   * @return A future containing a map where each key of type `K2` is associated with a sequence of
   *         values of type `V2`. If the "forgiving" configuration is enabled, errors are logged,
   *         and processing continues. Otherwise, failures result in the future being failed.
   */
  private def doMap(i: Incoming[K1, V1]): Future[Map[K2, Seq[V2]]] = {
    val reply = mapper ? i
    if (config.getBoolean("forgiving")) {
      // TODO sort out compile info message...
      reply.mapTo[(Map[K2, Seq[V2]], Seq[Throwable])] map {
        case (v2sK2m, xs) =>
          for (x <- xs) log.warning("mapper exception:", x); v2sK2m
      }
    } else {
      val v2sK2mtf = reply.mapTo[Try[Map[K2, Seq[V2]]]]
      Master.flatten(v2sK2mtf)
    }
  }

  /**
   * Distributes the given map of K2 keys and corresponding sequences of V2 values to a set of reducers,
   * performs reductions on those values, and collates the results into a final map.
   *
   * @param v2sK2m A map where each key of type K2 is associated with a sequence of values of type V2.
   *               This map represents the intermediate results from a prior mapping phase.
   * @return A future containing a map where each key of type K2 is associated with either:
   *         - A successfully reduced value of type V3, or
   *         - A failure represented as a `Throwable`.
   */
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

/**
 * Case class representing a response object that separates successful computations from failed ones.
 *
 * @param left  a mapping of keys to the associated Throwable indicating failed computations
 * @param right a mapping of keys to their corresponding values indicating successful computations
 * @tparam K the type of keys in the maps
 * @tparam V the type of values in the `right` map
 */
case class Response[K, V](left: Map[K, Throwable], right: Map[K, V]) {
  override def toString = s"left: $left; right: $right"

  /**
   * Returns the size of the `right` map, which represents the number of key-value pairs it contains.
   *
   * @return the number of key-value pairs in the `right` map
   */
  def size: Int = right.size
}

/**
 * Companion object for the Response class. Provides a factory method to create a Response instance
 * by separating successful computations from failed ones, encapsulated in a map.
 */
object Response {
  /**
   * Creates a `Response` instance by separating the input map into two maps:
   * one containing keys associated with `Left` (failures) and their corresponding `Throwable`,
   * and another containing keys associated with `Right` (successful computations) and their corresponding values.
   *
   * @param vXeKm a map where each key is associated with an `Either` value. The `Left` contains a `Throwable`
   *              indicating a failure, and the `Right` contains a value of type `V` indicating a success.
   * @return a `Response` object containing two maps: one for the failures (keys mapped to `Throwable`) and one
   *         for the successes (keys mapped to values of type `V`).
   */
  def apply[K, V](vXeKm: Map[K, Either[Throwable, V]]): Response[K, V] = {
    val t = Master.toMap(Master.sequenceLeftRight(vXeKm))
    new Response(t._1, t._2)
  }
}

/**
 * The `Master` object provides utility methods for working with `Try`, `Either`, `Future`,
 * and collections, enabling transformations and compositions of these functional constructs.
 */
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
    case Success(s) =>
      Right(s);
    case Failure(e) =>
      Left(e)
  }

  /**
    * Method sequence which, applied to a Seq[Try [X] ], returns a Try[Seq [X] ]
    *
    * @param xts : Seq[Try [X] ]
    * @return : Try[Seq [X] ]
    */
  def sequence[X](xts: Seq[Try[X]]): Try[Seq[X]] =
    xts.foldLeft(Try(Seq[X]())) { (xst, xt) => for (xs <- xst; x <- xt) yield xs :+ x }

  /**
   * A method to flatten a Future\[Try\[X]] into a Future[X].
   * This combines the effects of a Future and a Try into a single Future.
   *
   * @param xyf      the input Future containing a Try, where the Try may either be a Success containing the value of type X
   *                 or a Failure containing an exception.
   * @param executor the implicit ExecutionContext used for asynchronous computation.
   * @return a Future containing the value of type X if the original Try was a Success, or a failed Future if the original Try was a Failure.
   */
  def flatten[X](xyf: Future[Try[X]])(using executor: ExecutionContext): Future[X] = {
    def convert[W](wy: Try[W]): Future[W] = {
      val wp = Promise[W]()
      wy match {
        case Success(y) =>
          wp complete Success(y)
        case Failure(e) =>
          wp complete Failure(e)
      }
      wp.future
    }

    for (xy <- xyf; x <- convert(xy)) yield x
  }

  /**
   * Transforms a map with values of type `Either[X, V]` into two separate maps:
   * one containing the keys with their corresponding `X` values if the `Either`
   * was `Left[X]`, and the other containing the keys with their corresponding `V`
   * values if the `Either` was `Right[V]`.
   *
   * @param vXeKm a map where each key is associated with a value of type `Either[X, V]`
   * @return a tuple containing two maps:
   *         - the first map contains entries from the original map with keys and
   *           `X` values from `Left[X]`.
   *         - the second map contains entries from the original map with keys and
   *           `V` values from `Right[V]`.
   */
  def sequence[K, V, X](vXeKm: Map[K, Either[X, V]]): (Map[K, X], Map[K, V]) =
    toMap(sequenceLeftRight(vXeKm))

  /**
   * Transforms a sequence of key-value pairs where the value is wrapped in an Either by extracting the left value of the Either.
   *
   * @param vXeKs a sequence of pairs, where each pair consists of a key of type K and a value of type Either[X, V].
   *              The Either contains a value on the left of type X or a value on the right of type V.
   * @return a sequence of pairs where each pair consists of a key of type K and the left value of the Either (of type X).
   *         Only the left projections (X values) from the input sequence are included in the result.
   */// TODO remove the get invocation here
  def sequenceLeft[K, V, X](vXeKs: Seq[(K, Either[X, V])]): Seq[(K, X)] =
    for ((k, e) <- vXeKs) yield (k, e.swap.toOption.get)

  /**
   * Transforms a sequence of key-value pairs where the value is wrapped in an Either.
   * Extracts the Right value from each Either and discards the Left values.
   *
   * @param vXeKs the sequence of tuples containing a key of type K and a value wrapped in an Either[X, V].
   * @return a sequence of tuples containing the key of type K and the extracted Right value of type V.
   */// TODO remove the get invocation here
  def sequenceRight[K, V, X](vXeKs: Seq[(K, Either[X, V])]): Seq[(K, V)] =
    for ((k, e) <- vXeKs) yield (k, e.toOption.get)

  /**
   * Maps a tuple of two elements `(L1, R1)` into another tuple `(L2, R2)` using two mapping functions.
   *
   * @param fl a function to map the first element of the tuple from type `L1` to type `L2`
   * @param fr a function to map the second element of the tuple from type `R1` to type `R2`
   * @param t  the input tuple of type `(L1, R1)` to be transformed
   * @return a tuple of type `(L2, R2)` where the first element is the result of applying `fl` to the first element of `t`,
   *         and the second element is the result of applying `fr` to the second element of `t`
   */
  def tupleMap[L1, L2, R1, R2](fl: L1 => L2, fr: R1 => R2)(t: (L1, R1)): (L2, R2) =
    (fl(t._1), fr(t._2))

  /**
   * Partitions a map into two sequences based on whether the value of `Either` in the map is `Left` or `Right`.
   *
   * @param vXeKm a map where each key is associated with a value of type `Either[X, V]`.
   * @return a tuple containing two sequences:
   *         - the first sequence contains all key-value pairs from the map where the value is a `Left`.
   *         - the second sequence contains all key-value pairs from the map where the value is a `Right`.
   */
  def partition[K, V, X](vXeKm: Map[K, Either[X, V]]): (Seq[(K, Either[X, V])], Seq[(K, Either[X, V])]) =
    vXeKm.toSeq.partition({ case (_, v) => v.isLeft })

  /**
   * Converts a tuple of two sequences of key-value pairs into a tuple of two maps.
   *
   * @param t a tuple where the first element is a sequence of key-value pairs of type (K, X)
   *          and the second element is a sequence of key-value pairs of type (K, V).
   * @return a tuple where the first map is constructed from the first sequence with keys of type K
   *         and values of type X, and the second map is constructed from the second sequence with
   *         keys of type K and values of type V.
   */
  def toMap[K, V, X](t: (Seq[(K, X)], Seq[(K, V)])): (Map[K, X], Map[K, V]) =
    (t._1.toMap, t._2.toMap)

  /**
   * Groups the elements in the given map into two sequences based on the nature of the `Either` values.
   * Left values are extracted to produce the first sequence, and Right values are used for the second sequence.
   *
   * @param vXeKm a map where the values are of type `Either[X, V]`; keys are used in the resulting sequences.
   * @return a tuple of two sequences:
   *         - the first sequence contains pairs of keys from the map and their associated Left values,
   *         - the second sequence contains pairs of keys from the map and their associated Right values.
   */
  def sequenceLeftRight[K, V, X](vXeKm: Map[K, Either[X, V]]): (Seq[(K, X)], Seq[(K, V)]) =
    tupleMap[Seq[(K, Either[X, V])], Seq[(K, X)], Seq[(K, Either[X, V])], Seq[(K, V)]](sequenceLeft, sequenceRight)(partition(vXeKm))
}