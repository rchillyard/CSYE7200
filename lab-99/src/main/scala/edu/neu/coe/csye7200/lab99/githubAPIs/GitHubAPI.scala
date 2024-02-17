package edu.neu.coe.csye7200.lab99.githubAPIs

import edu.neu.coe.csye7200.lab99.githubAPIs.Tries.{tryEquals, tryNotEquals}
import requests.Response
import scala.annotation.unused
import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import ujson.Value

object GitHubAPI extends App {

  // reading the github token stored in a file called github_token.txt in the user's home directory
  Try(os.read(os.home / "github_token.txt")).transform(token => invokeAPI(token.trim), ex => showException(ex))

  private def invokeAPI(token: String) = {
    // Just a test to see lihaoyi's first issue
    val resp1: Response = postIssue(token)
    print(resp1)

    // Retrieving all of uPickle's issues
    @unused
    val resp2: Try[Value] = for (z <- getIssuesJson(token); q <- Try(ujson.read(z))) yield q
//    print(resp2)

    val triedIssues: Try[Seq[IssueData]] = getIssues(token)
    triedIssues foreach { is => println(s"read ${is.length} issues") }

    // Calling the fetchPaginated function to retrieve all pages.
    val issues: mutable.Seq[Value] = fetchPaginated("https://api.github.com/repos/lihaoyi/upickle/issues", token, "state" -> "all")
    print(issues.length)

    // filtering only the non pull request issues in the response
    val nonPullRequests = issues.filter(!_.obj.contains("pull_request"))

    print(nonPullRequests.length)

    // parsing the values.
    val issueData = for (issue <- nonPullRequests) yield (
            issue("number").num.toInt,
            issue("title").str,
            issue("body").str,
            issue("user")("login").str
    )
    print(issueData)
    Success(())
  }

  case class IssueData(number: Int, title: String, body: String, user: User, url: String, repository_url: String,
                       labels_url: String, comments_url: String, events_url: String, html_url: String, id: Long, node_id: String,
                       locked: Boolean, assignees: Seq[String],
                       comments: Int, created_at: String, updated_at: String, closed_at: String, author_association: String,
//                       labels: Seq[String], state: String, pull_request: PullRequest, draft: Boolean
                      )

  case class User(login: String, id: Long, node_id: String, avatar_url: String, gravatar_id: String,
                  url: String, html_url: String, followers_url: String, following_url: String, gists_url: String,
                  starred_url: String, subscriptions_url: String, organizations_url: String, repos_url: String,
                  events_url: String, received_events_url: String, `type`: String, site_admin: Boolean)

  case class PullRequest(url: String, html_url: String, diff_url: String, patch_url: String)

  implicit val UserRW: upickle.default.ReadWriter[User] = upickle.default.macroRW[User]
  implicit val PullRequestRW: upickle.default.ReadWriter[PullRequest] = upickle.default.macroRW[PullRequest]
  implicit val IssueDataRW: upickle.default.ReadWriter[IssueData] = upickle.default.macroRW[IssueData]

  def getIssues(token: String): Try[Seq[IssueData]] =
    for {json <- getIssuesJson(token)
         _ = println(s"# json: ${json.length}")
         issues <- Try(upickle.default.read[Seq[IssueData]](json))
         } yield issues

  def getIssuesJson(token: String): Try[String] =
    for {
      response <- Try(requestIssues(token))
      _ <- tryEquals(response.statusCode, 200, "invalid status")
      _ <- tryEquals(response.headers("content-type"), List("application/json; charset=utf-8"), "bad content type")
      json <- tryNotEquals(response.text(), "", "empty json")
    } yield json

  def postIssue(token: String): Response = {
    requests.post(
      "https://api.github.com/repos/lihaoyi/test/issues",
      data = ujson.Obj("title" -> "hello"),
      headers = Map("Authorization" -> s"token $token")
    )
  }

  /**
   * This function iterates over the total number of pages and retrieves the data.
   *
   * @param url    String
   * @param params The parameters that are required to make a successful request
   */
  def fetchPaginated(url: String, token: String, params: (String, String)*): mutable.Seq[Value] = {
    var done = false
    var page = 1
    val responses = collection.mutable.Buffer.empty[ujson.Value]
    while (!done) {
      println("page " + page + "...")
      val resp = requests.get(
        url,
        params = Map("page" -> page.toString) ++ params,
        headers = Map("Authorization" -> s"token $token")
      )
      val parsed = ujson.read(resp.text()).arr
//      val text: String = resp.text()
//      val issueData = upickle.default.read[IssueData](text)
//      println(s"issueData: $issueData")
      if (parsed.isEmpty) done = true
      else responses.appendAll(parsed)
      page += 1
    }
    responses
  }

  private def requestIssues(token: String) = requests.get(
    "https://api.github.com/repos/lihaoyi/upickle/issues",
    params = Map("state" -> "all"),
    headers = Map("Authorization" -> s"token $token")
  )

  private def showException(ex: Throwable) = {
    println(ex)
    Success(())
  }

}


object Tries {
  def tryMatch[X](f: (X, X) => Boolean)(x: => X, expected: X, message: String): Try[X] =
    if (f(x, expected)) Success(x) else Failure(new Exception(s"$message: $x did not equal $expected"))

  def tryEquals[X](x: => X, expected: X, message: String): Try[X] = tryMatch[X](_ == _)(x, expected, message)

  def tryNotEquals[X](x: => X, expected: X, message: String): Try[X] = tryMatch[X](_ != _)(x, expected, message)
}