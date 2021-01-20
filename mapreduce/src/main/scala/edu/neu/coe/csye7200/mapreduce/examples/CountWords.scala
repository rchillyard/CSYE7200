package edu.neu.coe.csye7200.mapreduce.examples

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.neu.coe.csye7200.mapreduce._

import java.net.URI
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util._

/**
  * @author scalaprof
  */
object CountWords extends App {
  val config = ConfigFactory.load()
  private implicit val system: ActorSystem = ActorSystem("CountWords")
  implicit val timeout: Timeout = Timeout(10 seconds)

  import system.dispatcher

  def mapper1(w: String): (URI, String) = {
    val u = MockURI(w); (u.getServer, u.content)
  }

  def mapper2(w: URI, gs: Seq[String]): (URI, Int) = (w, (for (g <- gs) yield g.split("""\s+""").length) reduce (_ + _))

  def reducer(a: Seq[String], v: String) = a :+ v

  def init = Seq[String]()

  def adder(x: Int, y: Int): Int = x + y

  val props1 = Props.create(classOf[Master_First_Fold[String, URI, String, Seq[String]]], config, mapper1 _, reducer _, init)
  val master1 = system.actorOf(props1, s"WC-1-mstr")
  val props2 = Props.create(classOf[Master[URI, Seq[String], URI, Int, Int]], config, mapper2 _, adder _)
  val master2 = system.actorOf(props2, s"WC-2-mstr")
  val ws = if (args.length > 0) args.toSeq else Seq("http://www.bbc.com/doc1", "http://www.cnn.com/doc2", "http://default/doc3", "http://www.bbc.com/doc2", "http://www.bbc.com/doc3")
  val wsUrf = master1.ask(ws).mapTo[Response[URI, Seq[String]]]
  val iUrf = for (wsUr <- wsUrf; iUr <- master2.ask(wsUr.right).mapTo[Response[URI, Int]]) yield iUr
  iUrf.onComplete {
    case Success(iUr) =>
      val n = iUr.right.values.sum
      println(s"total words: $n")
      if (iUr.left.nonEmpty)
        for ((k, x) <- iUr.left) Console.err.println(s"exception thrown for key $k: $x")
      system.terminate()
    case Failure(x) => Console.err.println(s"Map/reduce error: ${x.getLocalizedMessage}"); system.terminate()
  }
  // there are 556 words in total between the three extracts
  val bbcText =
    """The US military has delivered more than 45 tonnes of ammunition to rebels fighting the jihadist group Islamic State (IS) in north-eastern Syria.
C-17 transport aircraft, accompanied by fighter escorts, dropped pallets of supplies overnight in Hassakeh province, a Pentagon spokesman said.
The consignment reportedly comprised small arms, ammunition and grenades.
It comes days after the US abandoned a $500m (£326m) plan to train thousands of "moderate" rebels to fight IS.
The money will instead be used to provide much-needed ammunition and some weapons to commanders of rebel groups already established on the ground."""
  val cnnText =
    """(CNN) Vladimir Putin just confirmed what many suspected -- that Russian airstrikes in Syria are meant to bolster President Bashar al-Assad's regime.
But exactly how they're doing that remains a point of contention: Are Russians really focused on pummeling ISIS, or are they targeting Syrian rebels demanding an end to the Assad dynasty?
"Our task is to stabilize the legitimate government and to create conditions for a political compromise ... by military means, of course," Putin told the state-run Russia 24 TV.
"The units of international terrorists and their ilk have no desire to negotiate with the Syrian government, who is almost sieged in its own capital."
Russia has said it's coordinating with the Syrian regime to target ISIS and other terrorists. Al-Assad has used the term "terrorists" to describe Syrians who seek his ouster."""
  val defaultText =
    """U.S. forces airdropped small arms ammunition and other supplies to Syrian Arab rebels, barely two weeks after Russia raised the stakes in the long-running civil war by intervening on the side of President Bashar al-Assad.
One military official said the drop, by Air Force C-17 cargo planes in northern Syria on Sunday, was part of a revamped U.S. strategy announced last week to help rebels in Syria battling Islamic State militants.
Last week, Washington shelved a program to train and equip "moderate" rebels opposed to Assad who would join the fight against Islamic State.[:nL1N1221MR]
The only group on the ground to have success against Islamic State while cooperating with the U.S.-led coalition is a Kurdish militia, the YPG, which has carved out an autonomous zone in northern Syria and advanced deep into Islamic State's stronghold Raqqa province.
On Monday, the YPG announced a new alliance with small groups of Arab fighters, which could help deflect criticism that it fights only on behalf of Kurds. Washington has indicated it could direct funding and weapons to Arab commanders on the ground who cooperate with the YPG.
Syrian Arab rebels said they had been told by Washington that new weapons were on their way to help them launch a joint offensive with their Kurdish allies on the city of Raqqa, the de facto Islamic State capital.
The U.S. military confirmed dropping supplies to opposition fighters vetted by the United States but would say no more about the groups that received the supplies or the type of equipment in the airdrop.[:nL1N12C115]
The Russian intervention in the four-year Syrian war has caught U.S. President Barack Obama's administration off guard. Washington has been trying to defeat Islamic State while still calling for Assad's downfall.
DANGEROUS CONSEQUENCES
Russian President Vladimir Putin was rebuffed in his bid to gain support for his country's bombing campaign, with Saudi sources saying they had warned the Kremlin leader of dangerous consequences and Europe issuing its strongest criticism yet."""

  def getMockContent(u: URI) = {
    u.getHost match {
      case "www.bbc.com" => bbcText
      case "www.cnn.com" => cnnText
      case _ => defaultText
    }
  }
}

case class MockURI(url: String) {
  def get: URI = new URI(url)

  def content: String = CountWords.getMockContent(get)

  def getServer: URI = new URI(get.getHost)
}
