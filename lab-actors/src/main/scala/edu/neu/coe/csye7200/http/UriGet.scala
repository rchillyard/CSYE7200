package edu.neu.coe.csye7200.http

import spray.http.Uri
import spray.http.Uri.{Authority, NamedHost, Path}

import scala.language.postfixOps

/**
  * @author robinhillyard
  */
class UriGet {
  def get(host: String, path: String, queryParams: Map[String, String], scheme: String = "https"): Uri =
    Uri(scheme, Authority(NamedHost(host)), Path(path)) withQuery queryParams
}
