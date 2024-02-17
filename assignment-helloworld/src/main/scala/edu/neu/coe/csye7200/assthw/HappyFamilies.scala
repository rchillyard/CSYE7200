package edu.neu.coe.csye7200.assthw

import java.util.Scanner
import scala.util.{Failure, Success, Try}

case class Family(father: Person, mother: Person, children: Seq[Person]) {
  def birth(child: Person): Family =
  // TO BE IMPLEMENTED 
???
}

case class Person(name: String, gender: Gender, age: Option[Int]) {
  def marry(person: Person): Try[Family] =
  // TO BE IMPLEMENTED 
???
}

case class Gender(maybeMale: Option[Boolean]) {
  def join(other: Gender): Option[Match] =
  // TO BE IMPLEMENTED 
???

  override def toString: String = maybeMale match {
    case Some(true) => "M"
    case Some(false) => "F"
    case _ => "-"
  }
}

object Gender {
  val male: Gender = Gender(Some(true))
  val female: Gender = Gender(Some(false))
  val none: Gender = Gender(None)

  def apply(w: String): Gender = w match {
    case "M" => male
    case "F" => female
    case _ => none
  }
}

trait Match

case object MaleFemale extends Match

case object FemaleMale extends Match

case object Illegal extends Match

case class GenderException(w: String) extends Exception(w)

object HappyFamilies extends App {

  def maybeAge(w: String): Option[Int] = w.toIntOption

  val scanner = new Scanner(System.in)
  System.err.print("Hello, what is your name? ")
  val name = scanner.next()
  System.err.print("and what is your gender (M or F)? ")
  val gender = Gender(scanner.next())
  System.err.print("and what is your age? ")
  val age = maybeAge(scanner.next())
  System.err.print(s"Hello, $name, I believe you are $gender and $age years old")
  val person1 = Person(name, gender, age)
}
