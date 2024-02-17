def f(x: Int) = x * x
f(9)
def f(x: Int) = {println(x); x * x}
f(9)
val y = f(9)
lazy val z = f(9)
z + 19
z+20
f{println("hello"); 9}
def f(x: () => Int) = x() * x()
f{() => println("hello"); 9}
def f(x: => Int) = x * x
f{println("hello"); 9}
val g = {println(9); 9*9}