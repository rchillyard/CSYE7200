package edu.neu.coe.csye7200

trait Base {
  val name: String
}

trait Organelle

trait Organism {
  def genotype: Seq[Base]
}

trait Eukaryote extends Organism {
  def organelles: Seq[Organelle]
}

trait Animal extends Eukaryote {
  def female: Boolean

  def organelles: Seq[Organelle] = Nil
}

trait Vertebrate extends Animal {
  def vertebra: Int

  def sound: Sound
}

trait Sound {
  def sound: Seq[Byte]
}

trait Voice extends Sound with (() => String) {
  def sound: Seq[Byte] = apply().getBytes
}

trait Bear extends Mammal {
  def sound: Sound = Growl

  def growl: String
}

case object Woof extends Voice {
  def apply(): String = "Woof!"
}

case object Growl extends Sound {
  def sound: Seq[Byte] = "growl".getBytes
}

trait Mammal extends Vertebrate {
  def vertebra: Int = 33
}

trait Pet extends Animal {
  def name: String
}

trait Dog extends Mammal with Pet {
  def sound: Sound = Woof

  def genotype: Seq[Base] = Nil
}

case class Chihuahua(name: String, female: Boolean, color: String) extends Dog

case class Pets[+X <: Pet with Mammal, -Y <: Sound](xs: Seq[X]) {

  import scala.language.postfixOps

  def identify(s: String): X = xs find (_.name == s) get

  def sounders(y: Y): Seq[X] = xs filter (_.sound == y)
}

object Pets extends App {

  def create[X <: Pet with Mammal, Y <: Sound](xs: X*): Pets[X, Y] = Pets(xs)

  // This method takes a Chihuahua and returns it as a Dog which works because Chihuahua is a subtype of Dog.
  // All of the required properties of Dog are specified by any instance of Chihuahua
  def asDog(x: Chihuahua): Dog = x

  // Here we create a new type X which involves both covariant and contravariant types.
  // Then we create a function of type X based on asDog
  type X[+S, -T] = T => S
  val x: X[Dog, Chihuahua] = asDog

  val bentley = Chihuahua("Bentley", female = false, "black")
  val gingerSnap = Chihuahua("GingerSnap", female = true, "ginger")
  val ralphie = Chihuahua("Ralphie", female = true, "white")
  // List[Chihuahua] is a subtype of Seq[Dog] because A is covariant in Seq[A] and because List is a subtype of Seq
  val dogs: Seq[Dog] = List(bentley, gingerSnap, ralphie)
  // Pets[Chihuahua, Sound] is a subtype of Pets[Dog,Voice] because Chihuahua is a subtype of Dog (and covariant)
  // while Sound is a supertype of Voice (and contravariant)
  val pets: Pets[Dog, Voice] = Pets.create[Chihuahua, Sound](bentley, gingerSnap, ralphie)
  // Dog is a subtype of Mammal: all of the required properties of Mammal are specified by any instance of Dog
  val m: Mammal = asDog(bentley)
  val ps = pets.sounders(Woof)
  println(ps.mkString(","))
  println(x(bentley))
}
