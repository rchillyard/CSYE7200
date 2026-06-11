import org.apache.spark.SparkContext

/**
  * Created by scalaprof on 10/19/16.
  */
object MainProgram extends App {

  ContextServer.getContextServer().sparkContext = new SparkContext()

  new ApplicationClass().doSomethingUseful
}
