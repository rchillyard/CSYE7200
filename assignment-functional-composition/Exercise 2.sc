case class Complex(real: Double, imag: Double)
val z = Complex(1,0)
z match {
  case Complex(r,i) => println(s"$r i$i")
//  case _ => println(s"exception: $z")
}
val l = List(1,2,3)
l match {
  case h :: t => println(s"head: $h; tail: $t")
  case Nil => println(s"empty")
}