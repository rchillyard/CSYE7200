import org.apache.spark.SparkContext

/**
  * Created by scalaprof on 10/19/16.
  */
class ContextServer {

 var sparkContext: SparkContext = _


//  var hiveContext: HiveContext
}

object ContextServer {

  var contextServer: ContextServer = _

  def getContextServer(): ContextServer = {
    if (contextServer==null)
      contextServer = new ContextServer
    contextServer
  }
  def sc: SparkContext = getContextServer().sparkContext
}
