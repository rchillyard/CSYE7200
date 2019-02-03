package edu.neu.coe.csye7200.actors

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit._
import edu.neu.coe.csye7200.model.Model
import org.scalatest._
import org.scalatest.tagobjects.Slow
import spray.http._

import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

/**
  * This specification tests much of the HedgeFund app but because it particularly deals with
  * processing data from the YQL (Yahoo Query Language) using JSON, we call it by its given name.
  */
class JsonGoogleOptionParserSpec(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("JsonGoogleParserSpec"))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  import scala.language.postfixOps

  val json: String = Source.fromFile(getClass.getResource("/googleOptionExample.json").getPath) mkString

  "json read" in {
    import spray.json._
    //noinspection ScalaUnusedSymbol
    val obj = JsonGoogleOptionParser.fix(json).parseJson
    // TODO may have to work on this
    //    obj should startWith "\"{\\\"puts\\\":[{\\\"e\\\":\\\"OPRA\\\",\\\"s\\\":\\\"MSFT150731P00042500\\\","
    //    obj shouldBe "{\"puts\":[{\"e\":\"OPRA\",\"s\":\"MSFT150731P00042500\",\"name\":\"\",\"cs\":\"chr\",\"strike\":\"42.50\",\"a\":\"0.02\",\"oi\":\"600\",\"cid\":\"444471331186724\",\"b\":\"-\",\"vol\":\"2\",\"p\":\"0.01\",\"c\":\"-0.02\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"-66.67\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00043000\",\"name\":\"\",\"cs\":\"chb\",\"strike\":\"43.00\",\"a\":\"0.08\",\"oi\":\"917\",\"cid\":\"48566598978694\",\"b\":\"0.01\",\"vol\":\"-\",\"p\":\"0.04\",\"c\":\"0.00\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"0.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00043500\",\"name\":\"\",\"cs\":\"chb\",\"strike\":\"43.50\",\"a\":\"0.09\",\"oi\":\"1075\",\"cid\":\"317521652120681\",\"b\":\"0.01\",\"vol\":\"-\",\"p\":\"0.04\",\"c\":\"0.00\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"0.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00044000\",\"name\":\"\",\"cs\":\"chr\",\"strike\":\"44.00\",\"a\":\"0.08\",\"oi\":\"3481\",\"cid\":\"318632935618046\",\"b\":\"0.02\",\"vol\":\"41\",\"p\":\"0.03\",\"c\":\"-0.03\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"-50.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00044500\",\"name\":\"\",\"cs\":\"chr\",\"strike\":\"44.50\",\"a\":\"0.04\",\"oi\":\"15263\",\"cid\":\"334270811795914\",\"b\":\"0.02\",\"vol\":\"507\",\"p\":\"0.04\",\"c\":\"-0.07\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"-63.64\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00045000\",\"name\":\"\",\"cs\":\"chr\",\"strike\":\"45.00\",\"a\":\"0.12\",\"oi\":\"6687\",\"cid\":\"766357859319307\",\"b\":\"0.05\",\"vol\":\"164\",\"p\":\"0.05\",\"c\":\"-0.15\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"-75.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00045500\",\"name\":\"\",\"cs\":\"chr\",\"strike\":\"45.50\",\"a\":\"0.11\",\"oi\":\"1688\",\"cid\":\"1009150702314985\",\"b\":\"0.08\",\"vol\":\"619\",\"p\":\"0.10\",\"c\":\"-0.37\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"-78.72\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00046000\",\"name\":\"\",\"cs\":\"chr\",\"strike\":\"46.00\",\"a\":\"0.23\",\"oi\":\"1926\",\"cid\":\"395752839645339\",\"b\":\"0.21\",\"vol\":\"319\",\"p\":\"0.20\",\"c\":\"-0.48\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"-70.59\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00046500\",\"name\":\"\",\"cs\":\"chr\",\"strike\":\"46.50\",\"a\":\"0.45\",\"oi\":\"1809\",\"cid\":\"848786281002239\",\"b\":\"0.41\",\"vol\":\"79\",\"p\":\"0.44\",\"c\":\"-0.56\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"-56.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00047000\",\"name\":\"\",\"cs\":\"chr\",\"strike\":\"47.00\",\"a\":\"0.80\",\"oi\":\"1263\",\"cid\":\"314945793092592\",\"b\":\"0.75\",\"vol\":\"22\",\"p\":\"1.34\",\"c\":\"-0.50\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"-27.17\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00047500\",\"name\":\"\",\"cs\":\"chb\",\"strike\":\"47.50\",\"a\":\"1.26\",\"oi\":\"468\",\"cid\":\"433502897182453\",\"b\":\"1.12\",\"vol\":\"-\",\"p\":\"2.34\",\"c\":\"0.00\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"0.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00048000\",\"name\":\"\",\"cs\":\"chr\",\"strike\":\"48.00\",\"a\":\"1.75\",\"oi\":\"125\",\"cid\":\"1120728705516728\",\"b\":\"1.57\",\"vol\":\"13\",\"p\":\"2.16\",\"c\":\"-0.39\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"-15.29\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00048500\",\"name\":\"\",\"cs\":\"chr\",\"strike\":\"48.50\",\"a\":\"2.22\",\"oi\":\"110\",\"cid\":\"311156152315979\",\"b\":\"2.05\",\"vol\":\"2\",\"p\":\"2.23\",\"c\":\"-0.67\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"-23.10\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00049000\",\"name\":\"\",\"cs\":\"chb\",\"strike\":\"49.00\",\"a\":\"2.75\",\"oi\":\"29\",\"cid\":\"421931139223341\",\"b\":\"2.55\",\"vol\":\"-\",\"p\":\"2.52\",\"c\":\"0.00\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"0.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00049500\",\"name\":\"\",\"cs\":\"chb\",\"strike\":\"49.50\",\"a\":\"3.25\",\"oi\":\"133\",\"cid\":\"1084399842973100\",\"b\":\"3.00\",\"vol\":\"-\",\"p\":\"3.04\",\"c\":\"0.00\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"0.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00050000\",\"name\":\"\",\"cs\":\"chb\",\"strike\":\"50.00\",\"a\":\"3.75\",\"oi\":\"1\",\"cid\":\"858443498919320\",\"b\":\"3.50\",\"vol\":\"-\",\"p\":\"3.27\",\"c\":\"0.00\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"0.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00050500\",\"name\":\"\",\"strike\":\"50.50\",\"a\":\"4.35\",\"oi\":\"0\",\"cid\":\"190021227352667\",\"b\":\"3.95\",\"vol\":\"-\",\"p\":\"-\",\"c\":\"-\",\"expiry\":\"Jul 31, 2015\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00051000\",\"name\":\"\",\"cs\":\"chb\",\"strike\":\"51.00\",\"a\":\"4.75\",\"oi\":\"0\",\"cid\":\"915373215253370\",\"b\":\"4.50\",\"vol\":\"-\",\"p\":\"6.35\",\"c\":\"0.00\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"0.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00051500\",\"name\":\"\",\"strike\":\"51.50\",\"a\":\"5.45\",\"oi\":\"0\",\"cid\":\"1073118539519446\",\"b\":\"5.00\",\"vol\":\"-\",\"p\":\"-\",\"c\":\"-\",\"expiry\":\"Jul 31, 2015\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731P00070000\",\"name\":\"\",\"strike\":\"70.00\",\"a\":\"23.80\",\"oi\":\"0\",\"cid\":\"797422466483546\",\"b\":\"22.40\",\"vol\":\"-\",\"p\":\"-\",\"c\":\"-\",\"expiry\":\"Jul 31, 2015\"}],\"calls\":[{\"e\":\"OPRA\",\"s\":\"MSFT150731C00044000\",\"name\":\"\",\"cs\":\"chg\",\"strike\":\"44.00\",\"a\":\"2.48\",\"oi\":\"542\",\"cid\":\"368508470381252\",\"b\":\"1.30\",\"vol\":\"13\",\"p\":\"2.30\",\"c\":\"+0.80\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"53.33\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00044500\",\"name\":\"\",\"cs\":\"chg\",\"strike\":\"44.50\",\"a\":\"1.97\",\"oi\":\"1515\",\"cid\":\"551849860555675\",\"b\":\"1.84\",\"vol\":\"295\",\"p\":\"1.65\",\"c\":\"+0.60\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"57.14\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00045000\",\"name\":\"\",\"cs\":\"chg\",\"strike\":\"45.00\",\"a\":\"1.51\",\"oi\":\"4253\",\"cid\":\"1050846974293430\",\"b\":\"1.36\",\"vol\":\"108\",\"p\":\"1.30\",\"c\":\"+0.71\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"120.34\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00045500\",\"name\":\"\",\"cs\":\"chg\",\"strike\":\"45.50\",\"a\":\"0.97\",\"oi\":\"1463\",\"cid\":\"1058479642432632\",\"b\":\"0.92\",\"vol\":\"728\",\"p\":\"0.99\",\"c\":\"+0.61\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"160.53\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00046000\",\"name\":\"\",\"cs\":\"chg\",\"strike\":\"46.00\",\"a\":\"0.56\",\"oi\":\"3568\",\"cid\":\"990111920462491\",\"b\":\"0.54\",\"vol\":\"2421\",\"p\":\"0.56\",\"c\":\"+0.36\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"180.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00046500\",\"name\":\"\",\"cs\":\"chg\",\"strike\":\"46.50\",\"a\":\"0.27\",\"oi\":\"3275\",\"cid\":\"925132083369959\",\"b\":\"0.26\",\"vol\":\"1653\",\"p\":\"0.28\",\"c\":\"+0.19\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"211.11\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00047000\",\"name\":\"\",\"cs\":\"chg\",\"strike\":\"47.00\",\"a\":\"0.12\",\"oi\":\"3271\",\"cid\":\"895704908033484\",\"b\":\"0.11\",\"vol\":\"942\",\"p\":\"0.12\",\"c\":\"+0.09\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"300.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00047500\",\"name\":\"\",\"cs\":\"chg\",\"strike\":\"47.50\",\"a\":\"0.06\",\"oi\":\"1560\",\"cid\":\"1002576560303353\",\"b\":\"0.04\",\"vol\":\"93\",\"p\":\"0.06\",\"c\":\"+0.04\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"200.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00048000\",\"name\":\"\",\"cs\":\"chg\",\"strike\":\"48.00\",\"a\":\"0.02\",\"oi\":\"2340\",\"cid\":\"82698616824055\",\"b\":\"0.01\",\"vol\":\"20\",\"p\":\"0.02\",\"c\":\"+0.01\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"100.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00048500\",\"name\":\"\",\"cs\":\"chb\",\"strike\":\"48.50\",\"a\":\"0.05\",\"oi\":\"577\",\"cid\":\"165182678652869\",\"b\":\"-\",\"vol\":\"-\",\"p\":\"0.02\",\"c\":\"0.00\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"0.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00049000\",\"name\":\"\",\"cs\":\"chb\",\"strike\":\"49.00\",\"a\":\"0.06\",\"oi\":\"836\",\"cid\":\"370192518128284\",\"b\":\"-\",\"vol\":\"4\",\"p\":\"0.02\",\"c\":\"0.00\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"0.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00049500\",\"name\":\"\",\"cs\":\"chb\",\"strike\":\"49.50\",\"a\":\"0.04\",\"oi\":\"4444\",\"cid\":\"269359231393921\",\"b\":\"0.01\",\"vol\":\"-\",\"p\":\"0.03\",\"c\":\"0.00\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"0.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00050000\",\"name\":\"\",\"cs\":\"chb\",\"strike\":\"50.00\",\"a\":\"0.01\",\"oi\":\"4531\",\"cid\":\"249743315162646\",\"b\":\"-\",\"vol\":\"-\",\"p\":\"0.01\",\"c\":\"0.00\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"0.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00050500\",\"name\":\"\",\"cs\":\"chb\",\"strike\":\"50.50\",\"a\":\"0.11\",\"oi\":\"629\",\"cid\":\"395277552848133\",\"b\":\"-\",\"vol\":\"-\",\"p\":\"0.03\",\"c\":\"0.00\",\"expiry\":\"Jul 31, 2015\",\"cp\":\"0.00\"},{\"e\":\"OPRA\",\"s\":\"MSFT150731C00070000\",\"name\":\"\",\"strike\":\"70.00\",\"a\":\"0.09\",\"oi\":\"0\",\"cid\":\"439295571919603\",\"b\":\"-\",\"vol\":\"-\",\"p\":\"-\",\"c\":\"-\",\"expiry\":\"Jul 31, 2015\"}],\"underlying_price\":46.419998,\"underlying_id\":\"358464\",\"expirations\":[{\"y\":2015,\"m\":7,\"d\":31},{\"y\":2015,\"m\":8,\"d\":7},{\"y\":2015,\"m\":8,\"d\":14},{\"y\":2015,\"m\":8,\"d\":21},{\"y\":2015,\"m\":8,\"d\":28},{\"y\":2015,\"m\":9,\"d\":4},{\"y\":2015,\"m\":9,\"d\":18},{\"y\":2015,\"m\":10,\"d\":16},{\"y\":2015,\"m\":11,\"d\":20},{\"y\":2016,\"m\":1,\"d\":15},{\"y\":2016,\"m\":4,\"d\":15},{\"y\":2016,\"m\":6,\"d\":17},{\"y\":2017,\"m\":1,\"d\":20}],\"expiry\":{\"y\":2015,\"m\":7,\"d\":31}}"
  }

  "json conversion" in {
    val contentType = ContentType(MediaTypes.`application/json`, HttpCharsets.`UTF-8`)
    val entity = HttpEntity(contentType, json.getBytes())
    val ok = JsonGoogleOptionParser.decode(entity) match {
      case Right(x) =>
        x.puts.length should equal(20)
        val puts = x.puts
        puts.head.get("s") should matchPattern { case Some("MSFT150731P00042500") => }

      case Left(x) =>
        fail("decoding error: " + x)
    }
    ok shouldBe Succeeded
  }

  "send back" taggedAs Slow in {
    val blackboard = system.actorOf(Props.create(classOf[MockGoogleOptionBlackboard], testActor), "blackboard")
    val entityParser = _system.actorOf(Props.create(classOf[EntityParser], blackboard))
    val contentType = ContentType(MediaTypes.`application/json`, HttpCharsets.`UTF-8`)
    val entity = HttpEntity(contentType, json.getBytes())
    entityParser ! EntityMessage("json:GO", entity)
    val msg = expectMsgClass(5.seconds, classOf[QueryResponse])
    println("msg received: " + msg)
    msg should matchPattern {
      case QueryResponse(_, _) =>
    }
  }
}

import akka.pattern.ask

import scala.concurrent.Await


class MockGoogleOptionUpdateLogger(blackboard: ActorRef) extends UpdateLogger(blackboard) {
  override def processOption(identifier: String, model: Model, attributes: Map[String, Any]): Unit = {
    val keys = model mapKeys List("underlying", "strikePrice", "expiry")
    println(s"$keys")
    val future = blackboard ? OptionQuery(identifier, keys)
    val result = Await.result(future, timeout.duration).asInstanceOf[QueryResponse]
    blackboard ! result
  }
}

class MockGoogleOptionBlackboard(testActor: ActorRef) extends Blackboard(Map(classOf[KnowledgeUpdate] -> "marketData", classOf[SymbolQuery] -> "marketData", classOf[OptionQuery] -> "marketData", classOf[CandidateOption] -> "optionAnalyzer", classOf[Confirmation] -> "updateLogger"),
  Map("marketData" -> classOf[MarketData], "optionAnalyzer" -> classOf[OptionAnalyzer], "updateLogger" -> classOf[MockGoogleOptionUpdateLogger])) {

  override def receive: PartialFunction[Any, Unit] = {
    case msg: Confirmation => msg match {
      // Cut down on the volume of messages
      case Confirmation("MSFT150731P00045000", _, _) => super.receive(msg)
      case _ =>
    }
    case msg: QueryResponse => testActor forward msg
    case msg => super.receive(msg)
  }
}
