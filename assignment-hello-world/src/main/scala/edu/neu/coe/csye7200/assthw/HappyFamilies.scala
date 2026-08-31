package edu.neu.coe.csye7200.assthw

import java.util.Scanner
import scala.util.{Failure, Success, Try}

case class Family(father: Person, mother: Person, children: Seq[Person]):
  def birth(child: Person): Family =
  // TO BE IMPLEMENTED 
    ???

case class Person(name: String, gender: Gender, age: Option[Int]):
  def marry(person: Person): Try[Family] =
  // TO BE IMPLEMENTED 
    ???

case class Gender(maybeMale: Option[Boolean]):
  def join(other: Gender): Option[Match] =
  // TO BE IMPLEMENTED 
    ???

  override def toString: String = maybeMale match {
    case Some(true) => "M"
    case Some(false) => "F"
    case _ => "-"
  }


object Gender:
  val male: Gender = Gender(Some(true))
  val female: Gender = Gender(Some(false))
  val none: Gender = Gender(None)

  def apply(w: String): Gender = w match {
    case "M" => male
    case "F" => female
    case _ => none
  }

trait Match

case object MaleFemale extends Match

case object FemaleMale extends Match

case object Illegal extends Match

case class GenderException(w: String) extends Exception(w)

/**
 * The `HappyFamilies` object provides a basic example demonstrating user interaction and manipulation of person-related information.
 * It captures user input such as name, gender, and age, and creates a `Person` instance based on this data.
 *
 * It you'd like to run it as a main program, simply extend "App."
 *
 * The object performs the following functionalities:
 *  - Reads user input for name, gender, and age.
 *  - Converts the age input into an `Option[Int]` using the `maybeAge` utility function.
 *  - Prints personalized messages capturing the user's details.
 *  - Constructs a `Person` instance using the provided information.
 *
 * This object demonstrates interaction via the console and simple functional programming concepts. It operates with the `Gender` helper
 * which maps input strings to specific gender values and utilizes the `Person` case class for representing a person's attributes.
 */
object HappyFamilies:

  def maybeAge(w: String): Option[Int] = w.toIntOption

//  val scanner = new Scanner(System.in)
//  System.err.print("Hello, what is your name? ")
//  val name: String = scanner.next()
//  System.err.print("and what is your gender (M or F)? ")
//  private val gender = Gender(scanner.next())
//  System.err.print("and what is your age? ")
//  private val age = maybeAge(scanner.next())
//  System.err.println(s"Hello, $name, I believe you are $gender and $age years old")
//  val person1 = Person(name, gender, age)
//  System.out.println(s"person1: $person1")

