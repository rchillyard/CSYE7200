package edu.neu.coe.csye7200.numerics

/**
 * Represents absolute temperature in Kelvin.
 *
 * This class provides methods to convert Kelvin temperatures into other temperature scales,
 * including Celsius, Fahrenheit, and Rankine.
 *
 * NOTE that the constructor is private to ensure that only valid temperatures can be created.
 * NOTE that copy could still cause problems with invalid temperatures.
 *
 * @param kelvin the temperature in Kelvin
 */
case class AbsoluteTemperature private (kelvin: Double) extends AnyVal:
  import AbsoluteTemperature.*

  def toCelsius: Double = kelvin + absoluteZeroCelsius
  def toFahrenheit: Double = kelvin * scaleFactor + absoluteZeroFahrenheit
  def toRankine: Double = kelvin * scaleFactor
  override def toString: String = s"AbsoluteTemperature: $kelvin K"

/**
 * Provides factory methods to create instances of `AbsoluteTemperature` from different temperature scales.
 *
 * The `AbsoluteTemperature` object defines constants and conversion methods for converting
 * temperatures from Celsius, Fahrenheit, and Rankine scales to Kelvin.
 *
 * Constants:
 * - `absoluteZeroCelsius`: Absolute zero temperature in Celsius.
 * - `absoluteZeroFahrenheit`: Absolute zero temperature in Fahrenheit.
 * - `scaleFactor`: Conversion factor between Kelvin and Rankine/Fahrenheit.
 *
 * Methods:
 * - `fromCelsius(celsius: Double)`: Converts a temperature in Celsius to an instance of `AbsoluteTemperature`.
 * - `fromFahrenheit(fahrenheit: Double)`: Converts a temperature in Fahrenheit to an instance of `AbsoluteTemperature`.
 * - `fromRankine(rankine: Double)`: Converts a temperature in Rankine to an instance of `AbsoluteTemperature`.
 */
object AbsoluteTemperature:
  private val absoluteZeroCelsius = -273.15
  private val absoluteZeroFahrenheit = -459.67
  private val scaleFactor = 1.8
  def apply(kelvin: Double): AbsoluteTemperature =
    require(kelvin >= 0, "Absolute temperature cannot be negative")
    new AbsoluteTemperature(kelvin)
  def fromCelsius(celsius: Double): AbsoluteTemperature = apply(celsius - absoluteZeroCelsius)
  def fromFahrenheit(fahrenheit: Double): AbsoluteTemperature = apply((fahrenheit - absoluteZeroFahrenheit) / scaleFactor)
  def fromRankine(rankine: Double): AbsoluteTemperature = apply(rankine / scaleFactor)

/**
 * Converts temperature values from Celsius, Fahrenheit, or Rankine into an instance
 * of `AbsoluteTemperature` and prints the result. The conversion is determined
 * by the input arguments where the first argument represents the value and
 * the second represents the temperature scale ("C" for Celsius, "F" for Fahrenheit,
 * or "R" for Rankine).
 *
 * @param args A variable-length argument list. The first argument is the temperature
 *             value as a string, and the second argument specifies the temperature
 *             scale ("C", "F", or "R"). Any other input will display usage instructions.
 * @return Unit. Outputs the result of the conversion to the console or provides
 *         usage instructions for invalid input.
 */
@main def convert(args: String*): Unit =
  args match
    case Seq(x, "C") => println(s"${args.mkString}: ${AbsoluteTemperature.fromCelsius(x.toDouble)}")
    case Seq(x, "F") => println(s"${args.mkString}: ${AbsoluteTemperature.fromFahrenheit(x.toDouble)}")
    case Seq(x, "R") => println(s"${args.mkString}: ${AbsoluteTemperature.fromRankine(x.toDouble)}")
    case _ => println("Usage: convert <C|F|R> <value>")

