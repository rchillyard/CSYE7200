/*
 * Copyright (c) 2025. Phasmid Software
 */

package edu.neu.coe.csye7200.exam

import java.io.PrintWriter
import scala.util.{Failure, Random, Success, Try}

@main
def main(args: String*): Unit = doMain(new SetGame, PrintWriter(System.out, true))

/**
 * Represents a game of Set, a card game where players identify groups of three cards
 * (sets) that satisfy specific attribute-based rules. The class provides functionality
 * for generating a full deck of cards, dealing a hand of cards, and identifying valid sets.
 * See [[https://en.wikipedia.org/wiki/Set_(card_game)]].
 *
 * @constructor Creates a new instance of the Set game.
 * @param size The count of cards dealt in a hand, defaulting to 12.
 * @param random The random generator used for shuffling and dealing cards, defaulting to `new Random`.
 */
class SetGame(size: Int = 12, random: Random = new Random) {

  /**
   * Generates a complete deck of cards for the game of Set.
   * The deck contains all possible combinations of the four attributes:
   * count, shape, color, and shading. Each attribute has three possible
   * values, resulting in a total of 81 distinct cards in the deck.
   *
   * Combines every value from the `Count`, `Shape`, `Color`, and `Shading` enumerations
   * to create individual `Card` objects with all unique configurations.
   *
   * @return A sequence of 81 unique `Card` objects representing the full deck.
   */
  val allCards: Seq[Card] = for {
    count <- Count.values
    shape <- Shape.values
    color <- Color.values
    shading <- Shading.values
  } yield Card(count, shape, color, shading)

  val initialHand: Seq[Card] = hand

  /**
   * Deals a random selection of `size` (usually 12) cards from the full deck.
   * The method shuffles the complete sequence of cards and selects
   * the first cards in the newly randomized order.
   * HINT: use `random` to do the shuffling.
   *
   * @return A sequence of `size` randomly selected `Card` objects.
   */
  def hand: Seq[Card] = ???

  /**
   * Identifies all possible valid sets from a given sequence of cards.
   * A valid set consists of exactly three cards that satisfy the rules of the "Set" game:
   * for each attribute (shape, color, count, and shading), the values must either all be the same
   * or all be different across the three cards.
   *
   * The method generates all possible combinations of three cards from the input sequence,
   * evaluates each combination to check if it forms a valid set, and returns the collection of valid sets.
   *
   * @param cards A sequence of `Card` objects to search for valid sets.
   * @return A sequence of valid `Set` objects found within the given cards.
   */
  def sets(cards: Seq[Card]): Seq[Set] =
    (for {
      i <- cards.indices
      j <- Range(???, ???)
      k <- Range(???, ???)
    } yield Set(Seq(cards(i), cards(j), cards(k)))).filter(???)
}

/**
 * Represents a set of three cards in the game of Set.
 * A valid `Set` consists of exactly three cards, where for each attribute
 * (shape, color, count, and shading), the values must either all be the same
 * or all be different across the three cards.
 *
 * @constructor Creates a `Set` with a specified sequence of three cards.
 *              Ensures the sequence contains exactly three cards.
 * @param cards The sequence of three `Card` objects that make up the set.
 */
case class Set(cards: Seq[Card]) {
  require(cards.distinct.size == 3)

  // Lens functions:
  val count: Card => Int = ???
  val shape: Card => Int = ???
  val color: Card => Int = ???
  val shading: Card => Int = ???

  /**
   * Checks if the three cards in the set form a valid `Set`.
   * A valid `Set` is formed when, for each attribute (shape, color, count, shading),
   * the values are either all the same or all different across the three cards.
   *
   * @return `true` if the cards form a valid `Set`, `false` otherwise.
   */
  def isSet: Boolean =
    attributesMatch(count) && attributesMatch(shape) && attributesMatch(color) && attributesMatch(shading)

  /**
   * Checks whether the values of a particular attribute across the three cards
   * are either all the same or all different.
   *
   * @param f A lens function that extracts the specific attribute from a card
   *          (e.g., shape, color, count, or shading).
   * @return `true` if the values of the specified attribute on the three cards
   *         are either all identical or all unique, otherwise `false`.
   */
  def attributesMatch(f: Card => Int): Boolean = {
    ??? // Hint: use `distinct.size == 1` to check for uniqueness
  }

  override def toString: String = s"{${cards.mkString(", ")}}"
}

/**
 * Companion object for the `Set` class, providing utility methods
 * to construct instances of `Set` and work with string representations
 * of card sets.
 */
object Set {
  /**
   * Constructs a `Set` object from three `Card` instances.
   * A valid `Set` contains exactly three cards where, for each attribute
   * (shape, color, count, and shading), the values are either all the same
   * or all different across the three cards.
   *
   * @param card1 The first card in the set.
   * @param card2 The second card in the set.
   * @param card3 The third card in the set.
   * @return A `Set` object containing the three specified `Card` instances.
   */
  def apply(card1: Card, card2: Card, card3: Card): Set = Set(Seq(card1, card2, card3))

  /**
   * Parses a string representation of a set and converts it into a `Set` object.
   * The input string must be enclosed in curly braces and contain three colon-separated
   * card representations. Each card representation must be in the format of
   * "attribute1:attribute2:attribute3:attribute4", where the attributes correspond
   * to the properties of a `Card` (e.g., count, shape, color, shading).
   *
   * @param w The string to parse, representing a set of cards in the specified format.
   * @return A `Set` object containing three `Card` instances derived from the input.
   * @throws IllegalArgumentException If the input string does not match the expected format.
   */
  def apply(w: String): Set = w match {
    case setRegex(s) =>
      val Array(a, b, c) = ???
      Set(Card(a), Card(b), Card(c))
    case _ =>
      throw new IllegalArgumentException(s"Invalid set: $w")
  }

  /**
   * Regular expression pattern used for parsing a set represented as a
   * string enclosed in curly braces. The content inside the braces is captured using a group.
   * Expected format: "{content}", where "content" can be any string.
   */
  private val setRegex = """\{([^\}]*)\}""".r
}

/**
 * Represents a card with four attributes: count, shape, color, and shading.
 *
 * Each attribute is modeled as its own enumeration (`Count`, `Shape`, `Color`, `Shading`),
 * encapsulating the specific values allowed in the domain of the card game.
 *
 * This class provides a structured way to represent and work with cards, ensuring
 * strict type checking and domain-specific constraints.
 *
 * The fields of this class store the respective attributes of a card:
 * - count: The number of elements depicted on the card.
 * - shape: The specific geometric shape depicted on the card (e.g., Oval, Diamond, Squiggle).
 * - color: The color used to render the shape (e.g., Red, Green, Purple).
 * - shading: The style in which the shape is filled (e.g., Solid, Striped, Open).
 *
 * @constructor Creates a `Card` instance by specifying its four attributes.
 * @param count   The count attribute of the card.
 * @param shape   The shape attribute of the card.
 * @param color   The color attribute of the card.
 * @param shading The shading attribute of the card.
 */
case class Card(count: Count, shape: Shape, color: Color, shading: Shading):
  override def toString: String = s"$count:$shape:$color:$shading"

/**
 * Factory object for constructing instances of `Card` from various inputs.
 */
object Card {

  /**
   * Constructs a Card instance by parsing a serialized string representation.
   *
   * The input string should represent the attributes of a Card in the following format:
   * "count:shape:color:shading". The method splits the string based on the colon delimiter
   * and attempts to create a Card using the parsed values. If the input string is invalid or
   * parsing fails, an error message is logged, and null is returned.
   *
   * @param w The serialized string representation of a Card, using colon-delimited attributes.
   * @return A Card instance if parsing and construction succeed, or null if an error occurs.
   */
  def apply(w: String): Card = {
    val Array(count, shape, color, shading) = ???
    fromStrings(count, shape, color, shading) match {
      case Success(card) => card
      case Failure(exception) =>
        System.err.println(s"cannot create a Card from $w because: $exception")
        null
    }
  }

  /**
   * Constructs a `Card` instance from string representations of its attributes: count, shape, color, and shading.
   *
   * This method attempts to parse the given string values into their respective enumerations (`Count`, `Shape`, `Color`, and `Shading`)
   * and creates a `Card` object. Any parsing errors will result in a failure encapsulated in a `Try`.
   *
   * @param numberStr  The string representation of the count attribute of the card.
   * @param shapeStr   The string representation of the shape attribute of the card.
   * @param colorStr   The string representation of the color attribute of the card.
   * @param shadingStr The string representation of the shading attribute of the card.
   * @return A `Try[Card]` containing the constructed `Card` if all strings were successfully parsed, or a failure if parsing failed.
   */
  def fromStrings(numberStr: String, shapeStr: String, colorStr: String, shadingStr: String): Try[Card] = Try {
    Card(Count.valueOf(numberStr), Shape.valueOf(shapeStr), Color.valueOf(colorStr), Shading.valueOf(shadingStr))
  }
}

/**
 * A trait representing a generic attribute with an integer mapping.
 *
 * Implementations of this trait are expected to provide a concrete
 * definition of the `toInt` method, which maps the attribute to a
 * corresponding integer value. This may be used for comparison,
 * serialization, or other domain-specific purposes.
 */
trait Attribute:
  def toInt: Int

/**
 * An enumeration representing colors used as attribute values in the context of SetGame.
 *
 * Each color is associated with a unique integer ID for mapping and comparison purposes.
 * This enum extends the `Attribute` trait, providing an implementation for representing
 * the color as its corresponding integer value.
 *
 * @param toInt An integer representation of the color.
 */
enum Color(val toInt: Int) extends Attribute:
  case Red extends Color(1)
  case Green extends Color(2)
  case Purple extends Color(3)

/**
 * Enum representation of shapes used in the Set game.
 *
 * Each shape is associated with a unique integer value which allows for easy comparison
 * and mapping between attributes in the game.
 *
 * @param toInt Integer representation of the shape.
 */
enum Shape(val toInt: Int) extends Attribute:
  case Oval extends Shape(1)
  case Diamond extends Shape(2)
  case Squiggle extends Shape(3)

/**
 * Represents the shading attribute of a card in a game.
 *
 * The `Shading` enum defines three distinct shading types:
 * - Solid
 * - Striped
 * - Open
 *
 * Each shading type has an associated integer value:
 * - 1 for `Solid`
 * - 2 for `Striped`
 * - 3 for `Open`
 *
 * This mapping between shading types and integer values allows for
 * comparison, serialization, or other domain-specific operations.
 *
 * @param toInt An integer representation of the shading type.
 */
enum Shading(val toInt: Int) extends Attribute:
  case Solid extends Shading(1)
  case Striped extends Shading(2)
  case Open extends Shading(3)

/**
 * Enumeration representing the count attribute of a card in a card game,
 * such as the "Set" game. Each value represents the count of elements
 * depicted on the card and is mapped to an integer value.
 *
 * This enum extends the `Attribute` trait, which means it provides a
 * method to convert the count value into its corresponding integer.
 *
 * Values:
 * - `One`: Represents a count of one element. Mapped to 1.
 * - `Two`: Represents a count of two elements. Mapped to 2.
 * - `Three`: Represents a count of three elements. Mapped to 3.
 */
enum Count(val toInt: Int) extends Attribute:
  case One extends Count(1)
  case Two extends Count(2)
  case Three extends Count(3)

/**
 * Executes the main functionality for the `SetGame` instance, printing the total number of cards in the game
 * and analyzing the initial hand to determine and display possible valid sets.
 *
 * @param game   An instance of `SetGame` representing the current game state.
 * @param stream A `PrintStream` to output the game details and results.
 * @return This method does not return a value. It prints game information to the provided `stream`.
 */
private[exam] def doMain(game: SetGame, stream: PrintWriter): Unit = {
  stream.println(s"There are ${game.allCards.size} cards in total.")
  stream.println(s"Example hand: ${game.initialHand.mkString(", ")}")
  val sets = game.sets(game.initialHand)
  stream.println(s"There are ${sets.size} possible sets: ${sets.mkString(", ")}")
}