import edu.neu.coe.csye7200.MonadOps._


case class V(v: Int)

case class X(x: Int)

case class Y(y: Int)

case class Z(v: Int, x: Int, y: Int)

val maybeOne = map0()(() => 1)
val maybeTwo = map1(maybeOne)(_ * 2)
val maybeThree = map2(maybeOne, maybeTwo)(_ + _)
val maybeSix = map3(maybeOne, maybeTwo, maybeThree)(_ + _ + _)

for (one <- maybeOne; two <- maybeTwo; three <- maybeThree) yield Z(one, two, three)

maybeOne.flatMap((one: Int) => maybeTwo.map((two: Int) => Z.apply(one, two, 0)))
