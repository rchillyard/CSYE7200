package edu.neu.coe.csye7200.lab99.interviewQuestions

/**
 * Class to map integers into Strings where:
 * factors of 3 result in "fizz";
 * factors of 5 result in "buzz";
 * factors of 15 result in "fizzbuzz".
 *
 * NOTE: there are no (mutable) variables in any of this code.
 * NOTE: we can easily adapt the code to a different set of fizz-buzz factors, see for example Cicadas.
 *
 * See, for example, an explanation of the FizzBuzz game (and interview question) here:
 * https://youtu.be/QPZ0pIK_wsc
 */
class FizzBuzz extends (Int => String) {
  /**
   * Method to map an Int into a String depending on the factors of the number.
   *
   * @param n the number to test.
   * @return a String: if n is a factor of 3 then "fizz",
   *         else if n is a factor of 5 then "buzz",
   *         else if n is a factor of both 3 and 5, then "fizzbuzz",
   *         otherwise a String representation of n.
   */
  def apply(n: Int): String = if (n > 0) factors(n)(3, 5) match {
    case List(5, 3) => s"$fizz$buzz"
    case List(5) => buzz
    case List(3) => fizz
    case _ => n.toString
  }
  else s"invalid n: $n"

  private val fizz = "fizz"
  private val buzz = "buzz"

  /**
   * Method to prepend y to xs if y divides n.
   *
   * @param n  the number to test.
   * @param xs a list of factors.
   * @param y  a potential factor.
   * @return y prepended to xs if y is a factor of n; otherwise xs.
   */
  private def addIfFactor(n: Int)(xs: List[Int], y: Int) = n % y match {
    case 0 => y +: xs;
    case _ => xs
  }

  /**
   * Method to get the factors of n, but only from the given list ys.
   *
   * @param n  the number to test.
   * @param ys a comma-separated list of potential factors.
   * @return a list of all the factors of n which are also in ys.
   */
  private def factors(n: Int)(ys: Int*) = ys.foldLeft(List[Int]())(addIfFactor(n))
}

/**
 * Create an executable object called FizzBuzz.
 */
object FizzBuzz extends App {
  // We need this so that we can end the expression for result with a post-fix operation.

  import scala.language.postfixOps

  // Let's deal with the first 100 numbers.
  private val n = 100

  // construct a new instance of FizzBuzz
  val fizzBuzz = new FizzBuzzBrief

  // create a list of the first n numbers (starting from 1) and map those numbers
  // into Strings according to the rules of FizzBuzz.
  private val result = LazyList from 1 map fizzBuzz take n toList

  // print the Strings, one to a line.
  result foreach println
}

/**
 * This class is an alternative version of FizzBuzz where we don't use so much extraction (and comments),
 * but rather inline everything as lambdas.
 *
 * See comments re: FizzBuzz.
 */
class FizzBuzzBrief extends (Int => String) {
  def apply(n: Int): String =
    if (n > 0) List(3, 5).foldLeft(List[Int]()) {
      (xs, y) =>
        n % y match {
          case 0 => y +: xs;
          case _ => xs
        }
    } match {
      case List(5, 3) => "fizzbuzz"
      case List(5) => "buzz"
      case List(3) => "fizz"
      case _ => n.toString
    }
    else s"invalid n: $n"
}

/**
 * Class to map integers representing years (CE) into Strings according to the emergence of Cicada broods.
 *
 * See https://en.wikipedia.org/wiki/Periodical_cicadas
 */
case class Cicadas(cicadas: Cicada*) extends (Int => (Int, List[String])) {
  /**
   * Method to map an Int into a String depending on the emergence(s) in that year.
   *
   * @param n the number to test.
   * @return a String
   */
  def apply(n: Int): (Int, List[String]) = n -> cicadas.foldLeft(List[String]())(
    (xs, c) => xs match {
      case Nil if c(n) => List(c.name)
      case _ if c(n) => xs :+ c.name
      case _ => xs
    }
  )
}

case class Cicada(start: Int, period: Int, name: String) extends (Int => Boolean) {
  override def apply(n: Int): Boolean = (n - start) % period == 0
}

object Cicadas {
  val broodX: Cicada = Cicada(2004, 17, "X") // Great Eastern brood: https://en.wikipedia.org/wiki/Brood_X
  val broodXIX: Cicada = Cicada(2011, 13, "XIX") // Great Southern brood: https://en.wikipedia.org/wiki/Brood_XIX

  // construct a new instance of Cicadas
  val cicadas: Cicadas = Cicadas(
    Cicada(2012, 17, "I") // Blue Ridge brood
    , Cicada(2013, 17, "II")  // East coast brood: https://en.wikipedia.org/wiki/Brood_II
    , Cicada(2014, 17, "III") // Iowan brood
    , Cicada(2015, 17, "IV") // Kansan brood
    , Cicada(2016, 17, "V") // https://en.wikipedia.org/wiki/Brood_V
    , Cicada(2000, 17, "VI")
    , Cicada(2001, 17, "VII") // Onandaga brood
    , Cicada(2002, 17, "VIII")
    , Cicada(2003, 17, "IX") // https://en.wikipedia.org/wiki/Brood_IX
    , broodX
    , Cicada(2007, 17, "XIII") // Northern Illinois brood: https://en.wikipedia.org/wiki/Brood_XIII
    , Cicada(2008, 17, "XIV") // https://en.wikipedia.org/wiki/Brood_XIV
    , broodXIX
    , Cicada(2001, 13, "XXII") // Baton Rouge brood: https://en.wikipedia.org/wiki/Brood_XXII
    , Cicada(2002, 13, "XXIII") // Lower Mississippi River Valley brood
  )
}

object CicadasMain extends App {
  // We need this so that we can end the expression for result with a post-fix operation.
  import scala.language.postfixOps

  // Let's deal with the first 50 emergence years.
  private val n = 50

  // create a lazy list of years (starting from 2000) and map those numbers
  // into Strings according to the emergences of Cicadas, or which we will take the first n.
  private val result = LazyList from 2000 map Cicadas.cicadas filterNot (_._2.isEmpty) map { case (x, ws) => s"$x: ${ws.mkString(",")}" } take n toList

  // print the Strings, one to a line.
  result foreach println
}
