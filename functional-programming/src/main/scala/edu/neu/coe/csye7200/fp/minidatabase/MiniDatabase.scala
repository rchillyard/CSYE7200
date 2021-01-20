package edu.neu.coe.csye7200.fp.minidatabase

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * @author scalaprof
  */
object MiniDatabase {
  def load(wo: Option[String]): List[Entry] = {
    val sy = Try(wo match {
      case Some(w) => Source.fromFile(w)
      case None => Source.fromResource("minidatabase.csv")
    })
    sy match {
      case Success(s) =>
        println(s)
        val result = s.getLines().toList.map(e => Entry(e.split(",")))
        s.close()
        result
      case Failure(x) =>
        System.err.println(x.getLocalizedMessage)
        Nil
    }
  }

  def measure(height: Height): String = height match {
    case Height(8, _) => "giant"
    case Height(7, _) => "very tall"
    case Height(6, _) => "tall"
    case Height(5, _) => "normal"
    case Height(_, _) => "short"
  }

  def main(args: Array[String]): Unit = {
    val wo = args.headOption
    val db = load(wo)
    print(db)
  }
}

case class Entry(name: Name, social: Social, dob: Date, height: Height, weight: Int)

case class Height(feet: Int, in: Int) {
  def inches: Int = feet * 12 + in
}

object Entry {
  def apply(name: String, social: String, dob: String, height: String, weight: String): Entry =
    Entry(Name(name), Social(social), Date(dob), Height(height), weight.toInt)

  def apply(entry: Seq[String]): Entry = apply(entry.head, entry(1), entry(2), entry(3), entry(4))
}

object Height {
  private val rHeightFtIn = """^\s*(\d+)\s*(?:ft|\')(\s*(\d+)\s*(?:in|\"))?\s*$""".r
  private val rHeightFt = """^\s*(\d+)\s*(?:ft|\')$""".r

  def apply(ft: String, in: String) = new Height(ft.toInt, in.toInt)

  def apply(ft: Int) = new Height(ft, 0)

  def apply(height: String): Height = height match {
    case rHeightFt(ft) => Height(ft.toInt)
    case rHeightFtIn(ft, _, in) => Height(ft, in)
    case _ => throw new IllegalArgumentException(height)
  }
}

case class Name(first: String, middle: String, last: String)

case class Social(area: Int, group: Int, serial: Int)

case class Date(year: Int, month: Int, day: Int)

object Name {
  private val rName3 = """^(\w+)\s+(\w.*)\s+(\w+)$""".r
  private val rName2 = """^(\w+)\s+(\w+)$""".r
  private val rName1 = """^(\w+)$""".r

  def apply(name: String): Name = name match {
    case rName1(s) => Name(null, null, s)
    case rName2(f, l) => Name(f, null, l)
    case rName3(f, m, l) => Name(f, m, l)
  }
}

object Date {
  private val rDate1 = """^(\w+)\s+(\d+)\w\w\s(\d{4})$""".r
  private val rDate2 = """^(\d+)\/(\d+)\/(\d+)$""".r

  def apply(year: String, month: String, day: String): Date = Date(year.toInt, month.toInt, day.toInt)

  def apply(date: String): Date = date match {
    // TODO implement this properly
//    case rDate1(d,m,y) => val mo = months.get(m); apply(y,m,d)
    case rDate2(d, m, y) => apply(y, m, d)
  }
}

object Social {
  private val rSsn = """^(\d{3})\-(\d{2})\-(\d{4})$""".r

  def apply(ssn: String): Social = ssn match {
    case rSsn(x, y, z) => Social(x.toInt, y.toInt, z.toInt)
  }
}
