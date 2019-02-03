package edu.neu.coe.csye7200.http

import edu.neu.coe.csye7200.model.{GoogleQuery, YQLQuery}
import org.scalatest.{Inside, Matchers, WordSpecLike}

/**
  * This specification really tests much of the HedgeFund app but because it particularly deals with
  * processing data from the YQL (Yahoo Query Language) using JSON, we call it by its given name.
  */
class QuerySpec extends WordSpecLike with Matchers with Inside {

  "YQL tech query" in {
    val symbols = List("YHOO", "AAPL", "GOOG", "MSFT")
    val uri = YQLQuery("json", diagnostics = true).createQuery(symbols)
    println(uri.toString)
    uri.toString shouldEqual "https://query.yahooapis.com/v1/public/yql?format=json&callback=&q=select+*+from+yahoo.finance.quotes+where+symbol+in+(%22YHOO%22,%22AAPL%22,%22GOOG%22,%22MSFT%22)&diagnostics=true&env=http://datatables.org/alltables.env"
  }

  "Google tech query" in {
    val symbols = List("AAPL", "YHOO")
    val uri = GoogleQuery("NASDAQ").createQuery(symbols)
    println(uri.toString)
    // TODO this is actually incorrect (and so is code being tested)--fix it
    uri.toString shouldEqual "https://finance.google.com/finance/info?q=NASDAQ:AAPL,YHOO&client=ig"
  }

}
