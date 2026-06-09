package edu.neu.coe.csye7200.asstzio

import zio.*


object HelloZIO extends ZIOAppDefault:
  def run =
    for {
      _    <- Console.printLine("What's your name?")
      name <- Console.readLine
      _    <- Console.printLine(s"Hello $name!")
    } yield ()
