package edu.neu.coe.csye7200.assthw

import java.util.Scanner
import scala.util.{Failure, Success, Try}

/**
 * Case class to represent a family.
 *
 * @param father a male Person.
 * @param mother a female Person.
 * @param children a list of children.
 */
case class Family(father: Person, mother: Person, children: Seq[Person]) {
  /**
   * Method to create a new Family based on this Family and which includes the given child.
   *
   * @param child the child to be born.
   * @return a new Family.
   */
  def birth(child: Person): Family = ??? // TODO (10) implement
}

/**
 * Case class to represent a Person.
 *
 * @param name name.
 * @param gender gender.
 * @param age optional age.
 */
case class Person(name: String, gender: Gender, age: Option[Int]) {
  /**
   * Method to marry this Person to the given person.
   *
   * @param person the person this is to marry.
   * @return a Family wrapped in Try.
   */
  def marry(person: Person): Try[Family] = gender.join(person.gender) match {
    // TODO (15) add cases for (1) male/female, (2) female/male and (3) same-sex, which should fail with GenderException("Illegal")
    case _ => Failure(new NoSuchElementException)
  }
}

/**
 * Case class to represent Gender.
 *
 * @param maybeMale Some of true/false (for male/female); or None (non-binary or unknown).
 */
case class Gender(maybeMale: Option[Boolean]) {
  /**
   * Method to join this Gender to other, as required by Person.marry.
   *
   * @param other the other Gender.
   * @return in the case of male/female then Some(MaleFemale); for female/male then Some(FemaleMale); else Some(Illegal).
   */
  def join(other: Gender): Option[Match] =
  // TODO (20) Construct a for comprehension based on maybeMale and other.maybeMale and then, inside the yielded expression,
  // match on the two resulting Booleans, as described in the scaladoc above.
  // In the default case, return Illegal.
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

