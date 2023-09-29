package edu.neu.coe.csye7200.lab99.githubAPIs

import edu.neu.coe.csye7200.lab99.githubAPIs.GitHubAPI.{IssueData, PullRequest, User, getIssues, getIssuesJson}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Success, Try}

class GitHubAPISpec extends AnyFlatSpec with should.Matchers {

  implicit val UserRW: upickle.default.ReadWriter[User] = upickle.default.macroRW[User]
  implicit val PullRequestRW: upickle.default.ReadWriter[PullRequest] = upickle.default.macroRW[PullRequest]
  implicit val IssueDataRW: upickle.default.ReadWriter[IssueData] = upickle.default.macroRW[IssueData]

  behavior of "GitHubAPI"

  it should "postIssue" in {
    // let's not do this for now
  }

  it should "get PR" in {
    val json =
      """{
        |"url":"https://api.github.com/repos/com-lihaoyi/upickle/pulls/442",
        |"html_url":"https://github.com/com-lihaoyi/upickle/pull/442",
        |"diff_url":"https://github.com/com-lihaoyi/upickle/pull/442.diff",
        |"patch_url":"https://github.com/com-lihaoyi/upickle/pull/442.patch",
        |"merged_at":null
        |}""".stripMargin
    val pr: PullRequest = upickle.default.read[PullRequest](json)
    pr.url shouldBe """https://api.github.com/repos/com-lihaoyi/upickle/pulls/442"""
  }

  it should "get user" in {
    val json =
      """{
       "login":"scala-steward",
       "id":43047562,
       "node_id":"MDQ6VXNlcjQzMDQ3NTYy",
       "avatar_url":"https://avatars.githubusercontent.com/u/43047562?v=4",
       "gravatar_id":"",
       "url":"https://api.github.com/users/scala-steward",
       "html_url":"https://github.com/scala-steward",
       "followers_url":"https://api.github.com/users/scala-steward/followers",
       "following_url":"https://api.github.com/users/scala-steward/following{/other_user}",
       "gists_url":"https://api.github.com/users/scala-steward/gists{/gist_id}",
       "starred_url":"https://api.github.com/users/scala-steward/starred{/owner}{/repo}",
       "subscriptions_url":"https://api.github.com/users/scala-steward/subscriptions",
       "organizations_url":"https://api.github.com/users/scala-steward/orgs",
       "repos_url":"https://api.github.com/users/scala-steward/repos",
       "events_url":"https://api.github.com/users/scala-steward/events{/privacy}",
       "received_events_url":"https://api.github.com/users/scala-steward/received_events",
       "type":"User",
       "site_admin":false
       }""".stripMargin
    val user: User = upickle.default.read[User](json)
    println(user)
  }

  it should "get issueData" in {
    val json =
      """{
        |"url":"https://api.github.com/repos/com-lihaoyi/upickle/issues/442",
        |"repository_url":"https://api.github.com/repos/com-lihaoyi/upickle",
        |"labels_url":"https://api.github.com/repos/com-lihaoyi/upickle/issues/442/labels{/name}",
        |"comments_url":"https://api.github.com/repos/com-lihaoyi/upickle/issues/442/comments",
        |"events_url":"https://api.github.com/repos/com-lihaoyi/upickle/issues/442/events",
        |"html_url":"https://github.com/com-lihaoyi/upickle/pull/442",
        |"id":1584267049,
        |"node_id":"PR_kwDOASLO2c5J80TJ",
        |"number":442,
        |"title":"Update circe-core, circe-generic, ... to 0.14.4",
        |"user":{"login":"scala-steward","id":43047562,"node_id":"MDQ6VXNlcjQzMDQ3NTYy","avatar_url":"https://avatars.githubusercontent.com/u/43047562?v=4","gravatar_id":"","url":"https://api.github.com/users/scala-steward","html_url":"https://github.com/scala-steward","followers_url":"https://api.github.com/users/scala-steward/followers","following_url":"https://api.github.com/users/scala-steward/following{/other_user}", "gists_url":"https://api.github.com/users/scala-steward/gists{/gist_id}","starred_url":"https://api.github.com/users/scala-steward/starred{/owner}{/repo}","subscriptions_url":"https://api.github.com/users/scala-steward/subscriptions","organizations_url":"https://api.github.com/users/scala-steward/orgs","repos_url":"https://api.github.com/users/scala-steward/repos","events_url":"https://api.github.com/users/scala-steward/events{/privacy}","received_events_url":"https://api.github.com/users/scala-steward/received_events","type":"User","site_admin":false},
        |"labels":[],
        |"state":"open",
        |"locked":false,
        |"assignee":null,
        |"assignees":[],
        |"milestone":null,
        |"comments":0,
        |"created_at":"2023-02-14T14:29:35Z",
        |"updated_at":"2023-02-14T14:29:35Z",
        |"closed_at":null,
        |"author_association":"CONTRIBUTOR",
        |"active_lock_reason":null,
        |"draft":false,
        |"pull_request":{"url":"https://api.github.com/repos/com-lihaoyi/upickle/pulls/442","html_url":"https://github.com/com-lihaoyi/upickle/pull/442","diff_url":"https://github.com/com-lihaoyi/upickle/pull/442.diff","patch_url":"https://github.com/com-lihaoyi/upickle/pull/442.patch","merged_at":null},
        |"body":"Updates \n* [io.circe:circe-core](https://github.com/circe/circe)\n* [io.circe:circe-generic](https://github.com/circe/circe)\n* [io.circe:circe-parser](https://github.com/circe/circe)\n\n from 0.14.3 to 0.14.4.\n[GitHub Release Notes](https://github.com/circe/circe/releases/tag/v0.14.4) - [Version Diff](https://github.com/circe/circe/compare/v0.14.3...v0.14.4)\n\n\nI'll automatically update this PR to resolve conflicts as long as you don't change it yourself.\n\nIf you'd like to skip this version, you can just close this PR. If you have any feedback, just mention me in the comments below.\n\nConfigure Scala Steward for your repository with a [`.scala-steward.conf`](https://github.com/scala-steward-org/scala-steward/blob/71a67d63f74149774a2a6e021dfb354771078b87/docs/repo-specific-configuration.md) file.\n\nHave a fantastic day writing Scala!\n\n<details>\n<summary>Adjust future updates</summary>\n\nAdd this to your `.scala-steward.conf` file to ignore future updates of this dependency:\n```\nupdates.ignore = [ { groupId = \"io.circe\" } ]\n```\nOr, add this to slow down future updates of this dependency:\n```\ndependencyOverrides = [{\n  pullRequests = { frequency = \"30 days\" },\n  dependency = { groupId = \"io.circe\" }\n}]\n```\n</details>\n\nlabels: library-update, early-semver-minor, semver-spec-patch, version-scheme:early-semver, commit-count:1","reactions":{"url":"https://api.github.com/repos/com-lihaoyi/upickle/issues/442/reactions","total_count":0,"+1":0,"-1":0,"laugh":0,"hooray":0,"confused":0,"heart":0,"rocket":0,"eyes":0},
        |"timeline_url":"https://api.github.com/repos/com-lihaoyi/upickle/issues/442/timeline",
        |"performed_via_github_app":null,
        |"state_reason":null
        |}""".stripMargin
    val issue: IssueData = upickle.default.read[IssueData](json)
    println(issue)
  }

  // TODO fix this
  ignore should "getIssuesJson" in {
    val triedIssueData: Try[IssueData] = for {token <- Try(os.read(os.home / "github_token.txt"))
                                              json <- getIssuesJson(token.trim)
                                              _ = println(json.length)
                                              issues <- Try(upickle.default.read[Seq[IssueData]](json))
                                              first = issues.head
                                              } yield first
    triedIssueData should matchPattern { case Success(_) => }
  }

  // TODO fix this.  getIssues simply invokes getIssuesJson which, as we see above, works just fine.
  ignore should "getIssues" in {
    val triedIssueData: Try[IssueData] = for {token <- Try(os.read(os.home / "github_token.txt"))
                                              json <- getIssuesJson(token.trim)
                                              _ = println(json.length)
                                              issues <- Try(upickle.default.read[Seq[IssueData]](json))
                                              issues2 <- getIssues(token.trim)
                                              first = issues.head
                                              } yield first
    triedIssueData should matchPattern { case Success(_) => }
  }
}