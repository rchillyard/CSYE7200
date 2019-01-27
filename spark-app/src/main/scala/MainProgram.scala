import org.apache.spark.{SparkConf, SparkContext}

/**
  * Created by scalaprof on 10/19/16.
  *
  * FIXME this doesn't work.
  */
object MainProgram extends App {

  val conf = new SparkConf()
    .setMaster("local[2]")
    .setAppName("CountingSheep")
//  val sc = new SparkContext(conf)
  ContextServer.getContextServer().sparkContext = new SparkContext(conf)

//  val sparkConf = new SparkConf().setAppName("SOME APP NAME").setMaster("local[2]").set("spark.executor.memory", "1g")
  new ApplicationClass().doSomethingUseful
}
