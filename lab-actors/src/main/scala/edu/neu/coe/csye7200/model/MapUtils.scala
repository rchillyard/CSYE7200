package edu.neu.coe.csye7200.model

/**
  * @author robinhillyard
  */
object MapUtils {

  import scala.language.postfixOps

  def flatten[K, V](x: Map[K, Option[V]]): Map[K, V] =
  // TODO fix this warning!
    x filter { case (_, v) => v match {
      case Some(_) => true;
      case _ => false
    }
    } map {
      case (k, Some(v)) => k -> v
      case z => throw new Exception(s"Unmatched: $z")
    }
}
