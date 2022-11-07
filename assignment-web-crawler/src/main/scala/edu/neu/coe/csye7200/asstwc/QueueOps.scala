package edu.neu.coe.csye7200.asstwc

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object QueueOps {

    implicit class RichQueue[X](q: Queue[X]) {
        def dequeueN(n: Int): (Seq[X], Queue[X]) = {
            @tailrec
            def inner(r: Seq[X], z1: Queue[X], m: Int): (Seq[X], Queue[X]) = m match {
                case 0 => (r, z1)
                case _ =>
                    z1.dequeueOption match {
                        case None => r -> z1
                        case Some(x -> z2) => inner(r :+ x, z2, m - 1)
                    }
            }

            inner(Nil, q, n)
        }
    }

}
