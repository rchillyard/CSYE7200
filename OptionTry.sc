import scala.util.{Failure, Success, Try}
// Option/Try worksheet

val s: String = "1"

val xo = s.toIntOption
val xy = Try(s.toInt)
xy.toOption
xy.toEither

def transformOptTry[X](xyo: Option[Try[X]]): Try[Option[X]] = xyo match {
  case None => Failure(new NoSuchElementException)
  case Some(xy) => xy match {
    case Success(x) => Success(Some(x))
    case Failure(x) => Failure(x)
  }
}

def transformTryOpt[X](xoy: Try[Option[X]]): Option[Try[X]] = xoy match {
  case Failure(x) => None
  case Success(xo) => xo match {
    case Some(x) => Some(Success(x))
    case None => None
  }
}

transformOptTry(Some(Failure(new RuntimeException("hello"))))
transformTryOpt(Success(Some(1)))