package edu.neu.coe.csye7200.asstwc

import edu.neu.coe.csye7200.asstwc.QueueOps.RichQueue
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.collection.immutable.Queue

class QueueOpsSpec extends AnyFlatSpec with should.Matchers {

    behavior of "RichQueue"

    it should "dequeueN(1) empty" in {
        val queue = Queue()
        val (x, z) = queue.dequeueN(1)

        x.size shouldBe 0
        z.isEmpty shouldBe true
    }

    it should "dequeueN(1)" in {
        val queue = Queue(1, 2, 3)
        val (w, y) = queue.dequeue
        val (x, z) = queue.dequeueN(1)

        x.size shouldBe 1
        x.head shouldBe w
        z shouldBe y
    }

    it should "dequeueN(2) A" in {
        val queue = Queue(1, 2)
        val (x, z) = queue.dequeueN(2)
        (x, z) should matchPattern { case Seq(1, 2) -> _ => }
        val (y, w) = z.dequeueN(2)
        (y, w) should matchPattern { case Nil -> _ => }
    }

    it should "dequeueN(2) B" in {
        val queue = Queue(1, 2, 3)
        val (x, z) = queue.dequeueN(2)
        (x, z) should matchPattern { case Seq(1, 2) -> _ => }
        val (y, w) = z.dequeueN(2)
        (y, w) should matchPattern { case Seq(3) -> Queue() => }
    }

}
