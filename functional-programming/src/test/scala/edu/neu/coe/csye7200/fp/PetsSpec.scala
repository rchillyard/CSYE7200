package edu.neu.coe.csye7200.fp

import edu.neu.coe.csye7200.Pets._
import edu.neu.coe.csye7200._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author scalaprof
  */
class PetsSpec extends AnyFlatSpec with Matchers {

  behavior of "Pets"

  it should "create Pets correctly" in {
    val bentley = Chihuahua("Bentley", female = false, "black")
    val gingerSnap = Chihuahua("GingerSnap", female = true, "ginger")
    val ralphie = Chihuahua("Ralphie", female = true, "white")
    // Pets[Chihuahua, Sound] is a subtype of Pets[Dog,Voice] because Chihuahua is a subtype of Dog (and covariant)
    // while Sound is a supertype of Voice (and contravariant)
    val pets: Pets[Dog, Voice] = Pets.create[Chihuahua, Sound](bentley, gingerSnap, ralphie)
    // Dog is a subtype of Mammal: all of the required properties of Mammal are specified by any instance of Dog
    val _: Mammal = asDog(bentley)
    val ps: Seq[Dog] = pets.sounders(Woof)
    ps.size shouldBe 3
    ps.head shouldBe bentley
  }

}
