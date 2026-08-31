import org.apache.spark.SparkContext

/**
  * Created by scalaprof on 10/19/16.
  */
object MainProgram extends App { // TODO upgrade to Scala 3

  ContextServer.getContextServer().sparkContext = new SparkContext()

  new ApplicationClass().doSomethingUseful
}
