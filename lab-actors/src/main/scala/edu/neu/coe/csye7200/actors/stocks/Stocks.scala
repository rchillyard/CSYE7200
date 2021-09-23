package edu.neu.coe.csye7200.actors.stocks

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import edu.neu.coe.csye7200.actors.stocks.Bank.{CreatePortfolio, PortfolioCreated}
import edu.neu.coe.csye7200.actors.stocks.PortfolioActor.{Buy, PortfolioCommand}
import java.util.UUID
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object Stocks extends App {

  final case class Start(clientName: String)

  def apply(): Behavior[Start] =
    Behaviors.setup { context =>
      context.log.info("Creation of the bank")
      val actorBank = context.spawn(Bank(), s"Bank")

      Behaviors.receiveMessage { message =>
        val name = message.clientName
        context.log.info(s"Start a new client: $name")
        context.spawn(BankClientUsingTheTellPattern(actorBank), name)
        Behaviors.same
      }
    }

    val system: ActorSystem[Stocks.Start] = ActorSystem(Stocks(), "Stocks")
    system ! Start("Alice")
    system ! Start("Bob")
}

object BankClientUsingTheAskPattern {
  def apply(bank: ActorRef[CreatePortfolio]): Behavior[Unit] =
    Behaviors.setup { context =>
      implicit val timeout: Timeout = 3.seconds
      context.ask(bank, CreatePortfolio) {
        case Success(message) =>
          context.log.info("Portfolio received")
          message.portfolio ! Buy("AAPL", 100L)
        case Failure(_) => context.log.info("Portfolio received")
      }
      Behaviors.ignore[Unit]
    }
}

object BankClientUsingTheTellPattern {
  def apply(bank: ActorRef[CreatePortfolio]): Behavior[PortfolioCreated] =
    Behaviors.setup { context =>
      bank ! CreatePortfolio(context.self)
      Behaviors.receiveMessage {
        case PortfolioCreated(portfolio) =>
          portfolio ! Buy("AAPL", 100L)
          Behaviors.empty
      }
    }
}

object Bank {

  final case class CreatePortfolio(client: ActorRef[PortfolioCreated])

  final case class PortfolioCreated(portfolio: ActorRef[PortfolioCommand])

  def apply(): Behavior[CreatePortfolio] =
    Behaviors.receive { (context, message) =>
      val replyTo = context.spawn(PortfolioActor(), UUID.randomUUID().toString)
      message.client ! PortfolioCreated(replyTo)
      Behaviors.same
    }
}

object PortfolioActor {

  sealed trait PortfolioCommand

  final case class Buy(stock: String, quantity: Long) extends PortfolioCommand

  final case class Sell(stock: String, quantity: Long) extends PortfolioCommand

  def apply(): Behavior[PortfolioCommand] = {
    portfolio(Portfolio(Map.empty))
  }

  private def portfolio(stocks: Portfolio): Behavior[PortfolioCommand] = {
    Behaviors receiveMessage {
      case Buy(stock, qty) =>
        portfolio(stocks.buy(stock, qty))
      case Sell(stock, qty) =>
        portfolio(stocks.sell(stock, qty))
    }
  }
}

case class Portfolio(stocks: Map[String, Stock]) {
  def buy(name: String, qty: Long): Portfolio = {
    val actuallyOwned: Stock = stocks.getOrElse(name, Stock(name, 0))
    copy(stocks + (name -> actuallyOwned.buy(qty)))
  }

  def sell(name: String, qty: Long): Portfolio = {
    val maybeStock = stocks.get(name)
    maybeStock.fold(this) {
      actuallyOwned =>
        copy(stocks + (name -> actuallyOwned.sell(qty)))
    }
  }
}

case class Stock(name: String, owned: Long) {
  def buy(qty: Long): Stock = copy(name, owned + qty)

  def sell(qty: Long): Stock =
    if (qty <= owned)
      copy(name, owned - qty)
    else
      this
}
