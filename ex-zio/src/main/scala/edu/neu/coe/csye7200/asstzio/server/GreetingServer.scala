package edu.neu.coe.csye7200.asstzio.server

import java.time.LocalDateTime
import zio.*
import zio.http.*

object GreetingServer extends ZIOAppDefault {
  
  val routes =
    Routes(
      Method.GET / Root -> handler(Response.text("Greetings at your service")),
      Method.GET / "greet" -> handler { (req: Request) =>
        val name = req.queryOrElse[String]("name", "World")
        Response.text(s"Hello $name!")
      }
    )

  def run = Server.serve(routes).provide(Server.default)
}
