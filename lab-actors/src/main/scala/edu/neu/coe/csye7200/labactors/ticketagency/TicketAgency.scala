package edu.neu.coe.csye7200.labactors.ticketagency

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors

case class Clients(id: Long, card: CreditCard)
case class Ticket(row: Int, seat: Int, price: Int)
case class CreditCard(name: String, number: Long)
case class Transaction(ts: Seq[Ticket], customer: Clients, total: Int)
case class Pool(ts: Seq[Ticket], tentative: Seq[Transaction], xs: Seq[Transaction])

trait PoolMessage {
  val from: ActorRef[Confirmation]
}

trait Confirmation

final case class Buyer(creditCard: CreditCard, replyTo: ActorRef[Authenticated])
final case class Authenticated(id: String, from: ActorRef[Buyer])
case class Availability(ts: Seq[Ticket], from: ActorRef[Confirmation]) extends PoolMessage
case class Hold(x: Transaction, from: ActorRef[Confirmation]) extends PoolMessage
case class Confirm(x: Transaction, from: ActorRef[Confirmation]) extends PoolMessage
case class Stop(from: ActorRef[Confirmation]) extends PoolMessage
case class Query(replyTo: ActorRef[Clients])
case object OK extends Confirmation
case class Failed(msg: String) extends Confirmation

object Clients {

  def apply(): Behavior[Buyer] = Behaviors.receive { (context, message) =>
    context.log.info("Hello {}!", message.creditCard)
    //#greeter-send-messages
    message.replyTo ! Authenticated(message.creditCard.name, context.self)
    //#greeter-send-messages
    Behaviors.same
  }
}

object Pool {

  def apply(ts: Seq[Ticket]): Behavior[PoolMessage] = {
    update(Pool(ts, Nil, Nil))
  }

  private def update(pool: Pool): Behavior[PoolMessage] =
    Behaviors.receive { (context, message) =>
      context.log.info("update {}", pool)
      message match {
        case Availability(ts, from) =>
          val ts1: Seq[Ticket] = pool.ts
          val ts2: Seq[Ticket] = ts1.diff(ts)
          from ! OK
          Behaviors.same

        case Hold(x, from) =>
          val p = Pool(pool.ts.diff(x.ts), pool.tentative :+ x, pool.xs)
          from ! OK
          update(p)

        case Stop(from) =>
          from ! OK
          Behaviors.stopped
      }
    }
}

//#greeter-main
object TicketAgency {

  final case class Release(tickets: Seq[Ticket])

  def apply(): Behavior[Release] =
    Behaviors.setup { context =>
      //#create-actors
      val client = context.spawn(Clients(), "client")
      //#create-actors

      Behaviors.receiveMessage { message =>
        val replyTo: ActorRef[PoolMessage] = context.spawn( Pool(message.tickets), "ticketAgency")
//        client ! Buyer(message.name, replyTo)

        Behaviors.same
      }
    }
}
//#greeter-main

//#main-class
//object MainProgram extends App {
//  //#actor-system
//  val agency: ActorSystem[TicketAgency.Release] = ActorSystem(TicketAgency(), "TicketAgency")
//  //#actor-system
//
//  //#main-send-messages
//  val tickets = for (row <- 1 to 10; seat <- 1 to 10) yield Ticket(row, seat, if (row < 5) 100 else 80)
//  agency ! TicketAgency.Release(tickets)
//  agency ? Availability(Seq[Ticket](Ticket(1, 1, 100)))
//  //#main-send-messages
//}
//#main-class
//#full-example
