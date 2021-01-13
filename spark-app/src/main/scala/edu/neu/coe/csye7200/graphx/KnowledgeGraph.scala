package edu.neu.coe.csye7200.graphx

import java.time._

import org.apache.spark._
import org.apache.spark.graphx._

abstract class Knowledge[A](what: String, term: String, value: A, lang: String = "en")

case class Concept(term: String, value: String, lang: String = "en") extends Knowledge[String]("concept",term,value,lang)

case class NamedEntity(term: String, value: String, what: String, lang: String = "en") extends Knowledge[String](what,term,value,lang)

case class Quantity[A](term: String, value: A, lang: String = "en") extends Knowledge[String]("quantity",term,value.toString,lang)

case class Relationship(which: String, tense: String)

case class Triple(from: Long, to: Long, relationship: Relationship)

object KnowledgeGraph extends App {
  implicit val time = java.time.LocalDate.now
  val conf = new SparkConf().setAppName("news").setMaster("local[*]")
  val sc = new SparkContext(conf)

  def createGraph(knowledge: List[Knowledge[String]], tripls: List[Triple]) = {
    val vertices: List[(VertexId,Knowledge[String])] = for (k <- knowledge; i <- 0 to knowledge.length) yield (i.toLong+1,k)
  
    val vArray = vertices.toArray
  
    val nodes = sc.parallelize(vArray)
  
    val pairs = for (r <- triplesDoc1) yield Edge(r.from.toLong,r.to.toLong,r.relationship)
  
    val eArray = pairs.toArray
  
    val arcs = sc.parallelize(eArray)
  
    Graph(nodes, arcs)
  }  

  val knowledgeDoc1: List[Knowledge[String]] = List(
      Quantity[Instant]("November 10th",Instant.parse("2015-11-10T10:15:30.00Z")),
      NamedEntity("Senator John Kerry","John Kerry, U.S. Senator and Secretary of State","person"),
      NamedEntity("President Vladimir Putin","Vladimir Putin, President of Russia","person"),
      NamedEntity("Syria","Syria","nation"),
      Concept("invitation to meet","meet"),
      Concept("subject","the subject of"),
      Quantity[Instant]("December 1st",Instant.parse("2015-12-01T10:15:30.00Z")),
      NamedEntity("Washington","Washington D.D.","city")
    )

  val triplesDoc1 = List(
      Triple(4,0,Relationship("timestamp","past")),
//      Triple(4,2,Relationship("invitee","future")),
//      Triple(4,1,Relationship("invited","past")),
//      Triple(1,4,Relationship("make","past")),
//      Triple(2,1,Relationship("meet with","future")),
//      Triple(4,6,Relationship("timestamp","future")),
//      Triple(4,5,Relationship("propose","past")),
//      Triple(4,7,Relationship("at","future")),
      Triple(5,3,Relationship("is","future"))
    )
  
  val doc1 = createGraph(knowledgeDoc1,triplesDoc1)
  
  println(doc1.triplets.collect)

  
}
