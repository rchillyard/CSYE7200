package edu.neu.coe.csye7200.labactors.ticketagency

import akka.actor.testkit.typed.Effect.Spawned
import akka.actor.testkit.typed.scaladsl.BehaviorTestKit
import akka.actor.typed.scaladsl.Behaviors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TicketAgencySpec extends AnyFlatSpec with should.Matchers {

  behavior of "TicketAgencySpec"

  private val childActor = Behaviors.receiveMessage[String] { _ =>
    Behaviors.same[String]
  }

  ignore should "apply" in {
    val testKit: BehaviorTestKit[TicketAgency.Release] = BehaviorTestKit(TicketAgency())
    testKit.run(TicketAgency.Release(Nil))
testKit.expectEffect(Spawned(Clients(), "client"))

  }

}
