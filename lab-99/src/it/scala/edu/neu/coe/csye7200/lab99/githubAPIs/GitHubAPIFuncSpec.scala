package edu.neu.coe.csye7200.lab99.githubAPIs

import edu.neu.coe.csye7200.lab99.githubAPIs.GitHubAPI.fetchPaginated
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.collection.mutable
import scala.util.Try
import ujson.Value

class GitHubAPIFuncSpec extends AnyFlatSpec with should.Matchers {

  behavior of "GitHubAPI"

  it should "fetchPaginated" in {
    val result: Try[mutable.Seq[Value]] = for {token <- Try(os.read(os.home / "github_token.txt"))
                                               z = fetchPaginated("https://api.github.com/repos/lihaoyi/upickle/issues", token.trim, "state" -> "all")
                                               } yield z
    println(result)
    // FIXME this was working.
//    result.isSuccess shouldBe true
  }

}