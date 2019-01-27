package edu.neu.coe.csye7200.model

import spray.http.Uri

/**
  * @author robinhillyard
  */
trait Query {
  def createQuery(symbols: List[String]): Uri

  def getProtocol: String
}
