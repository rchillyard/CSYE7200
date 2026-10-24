package edu.neu.coe.csye7200.shuntingyard

/**
 * Case class representing a parenthesis, which can either be open or closed.
 *
 * @param open A boolean indicating whether the parenthesis is open (true) or closed (false).
 */
trait Parenthesis

/**
 * Represents an open parenthesis.
 */
case object Open extends Parenthesis {
  override def toString: String = "("
}

/**
 * Represents a closing parenthesis.
 */
case object Close extends Parenthesis {
  override def toString: String = ")"
}

/**
 * Companion object for the Parenthesis case class.
 * Provides a method to construct a Parenthesis instance based on a string input.
 */
object Parenthesis {
  /**
   * Constructs a `Parenthesis` instance based on the input string.
   * The input is expected to be either "(" or ")" to signify an open or closed parenthesis respectively.
   *
   * @param w a string representing a parenthesis, where "(" indicates an open parenthesis and ")" indicates a closed one.
   * @return a `Parenthesis` instance with a boolean value `true` for open parenthesis and `false` for closed parenthesis.
   * @throws MatchError if the input string is not "(" or ")".
   */
  def apply(w: String): Parenthesis = w match {
    case "(" => Open
    case ")" => Close
  }
}

/**
 * Trait representing an Operator, typically used in mathematical or computational contexts.
 *
 * This trait defines a contract for any class or object that implements it to provide
 * a specific `operate` method, which takes two integer operands and returns an integer result.
 */
trait Operator {
  /**
   * Applies an operation between two integers and returns the result.
   *
   * @param x the first integer operand.
   * @param y the second integer operand.
   * @return the result of applying the operation to the given operands.
   */
  def operate(x: Int, y: Int): Int
}

/**
 * Companion object for the Operator trait.
 *
 * Provides utility methods for creating specific Operator instances based on a string.
 */
object Operator {
  /**
   * Creates an Operator instance based on the provided string representation.
   *
   * This method maps specific string inputs to their corresponding Operator case objects.
   *
   * @param s a string representation of the operator ("+" for Plus, "*" for Times).
   * @return the corresponding Operator instance (e.g., `Plus` or `Times`) if the input matches a valid operator.
   */
  def apply(s: String): Operator = s match {
    case "+" => Plus
    case "*" => Times
  }
}

/**
 * Represents the addition operator in a mathematical operation.
 *
 * This object extends the `Operator` trait and specifically implements the `operate`
 * method to perform the addition of two integers.
 *
 * The `Plus` operator is used to compute the sum of two integers.
 */
case object Plus extends Operator {
  /**
   * Applies an operation on two integer operands and returns the result.
   *
   * @param x the first integer operand.
   * @param y the second integer operand.
   * @return the result of applying the operation, which in this case is the sum of x and y.
   */
  def operate(x: Int, y: Int): Int = x + y
}

/**
 * A case object representing the multiplication operator.
 *
 * This object extends the `Operator` trait and provides
 * an implementation of the `operate` method to multiply two integers.
 */
case object Times extends Operator {
  /**
   * Method to perform an operation on two integers and return the result.
   *
   * @param x the first integer operand.
   * @param y the second integer operand.
   * @return the result of multiplying the two integer operands.
   */
  def operate(x: Int, y: Int): Int = x * y
}
