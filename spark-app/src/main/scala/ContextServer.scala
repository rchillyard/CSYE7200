import org.apache.spark.SparkContext

/**
  * Created by scalaprof on 10/19/16.
  */
class ContextServer {

 var sparkContext: SparkContext = null


//  var hiveContext: HiveContext
}

object ContextServer {

  var contextServer: ContextServer = null

  def getContextServer() = {
    if (contextServer==null)
      contextServer = new ContextServer
    contextServer
  }
  def sc = getContextServer().sparkContext
}
