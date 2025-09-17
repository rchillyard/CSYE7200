package edu.neu.coe.csye7200.labsorted.leetcode

import edu.neu.coe.csye7200.labsorted.leetcode.Solution.solve

/**
 * The `solve` method of the `Solution` object is where a candidate writes their code.
 * The method takes a list of Strings and returns a list of Strings representing lines of the output (to be tested).
 *
 * We use List instead of Array because in the Scala world (or any functional programming language),
 * we do not like arrays because they are mutable.
 * List is used just about everywhere!
 *
 * The tests include expected output verification using the provided `shouldBe` helper function to compare
 * computed values with expectations.
 */
object Solution {

  /**
   * This is where you write your code to solve the problem given.
   *
   * @param input a list of strings.
   * @return a list of strings.
   */
  def solve(input: List[String]): List[String] = {

    // Problem-specific code is placed here.
    val noMeetings: MeetingSchedule = MeetingSchedule.create()
    val lunchMeeting: Meeting = Meeting(1300, 1400)
    val scrum: Meeting = Meeting(1330, 1450)
    val justLunchMeeting: MeetingSchedule = noMeetings :+ lunchMeeting
    val scumeAndLunch: MeetingSchedule = justLunchMeeting :+ scrum

    val leetCodeMeetings1: MeetingSchedule = MeetingSchedule.parse(input.take(2))
    val leetCodeMeetings2: MeetingSchedule = MeetingSchedule.parse(input.slice(2, 5))

    // The following are (optional) assertions that the solver wants to assert before returning the result.
//    import edu.neu.coe.csye7200.labsorted.leetcode.ScalaTemplate.TestableInt
    import edu.neu.coe.csye7200.labsorted.leetcode.ScalaTemplate.*
    noMeetings.totalRooms shouldBe 0
    justLunchMeeting.totalRooms shouldBe 1
    scumeAndLunch.totalRooms shouldBe 2
    (scumeAndLunch :+ Meeting(1345, 1500)).totalRooms shouldBe 3
    ((Meeting(1310, 1320) +: scumeAndLunch) :+ Meeting(1345, 1500)).totalRooms shouldBe 3
    leetCodeMeetings1.totalRooms shouldBe 2
    leetCodeMeetings2.totalRooms shouldBe 2

    println("returning result...")

    // This is where the solver returns the result that will be checked by the ScalaTemplate.
    List(leetCodeMeetings1.totalRooms.toString, leetCodeMeetings2.totalRooms.toString)
  }
}

/**
 * The ScalaTemplate object demonstrates testing and verification of a solution's logic
 * using implicit classes for enhanced testing and the `solve` function to process input data.
 *
 * It extends the `App` trait to allow immediate execution of code within the object.
 * Specifically, it verifies expected results against actual outputs using custom `shouldBe` assertions and
 * showcases an example of how a candidate's solution can be evaluated for correctness.
 * NOTE that, because we extend `App`, we don't need to explicitly enclose the code inside a method called `main`.
 *
 * This object includes:
 * - Hardcoded test cases for demonstration purposes, such as meeting schedules.
 * - Implicit classes to test values like integers, strings, and lists.
 * - Printing statements to display results and indicate test status.
 *
 * The focus is on validating the logic present in the `solve` function, which computes the required results based on meeting schedules.
 */
object ScalaTemplate extends App {

  // NOTE the code here should basically remain unchanged.
  // The only thing here is that we have hardcode the input instead of getting it from args.
  // The real ScalaTemplate will get the input from args.
  println("Hello, world!")

  // NOTE: normally, we would just read the input strings from the command line (and those show up as an array of Strings called args, exactly like in Java).
//  val input: Seq[String] = args to List
  // NOTE: Instead, we will just hard-code the list of Strings for illustration purposes.
  val input: List[String] = List("05:00 - 08:00", "06:00 - 08:00", "09:00 - 10:00", "04:00 - 09:00", "04:00 - 17:00")
  val actual: List[String] = solve(input)
  // NOTE: this line is also obviously problem-specific
  actual shouldBe List("2", "2")
  
  println("Test completed")

  extension (actual: Int)
    def shouldBe(expected: Int): Unit = if (actual != expected) System.err.println(s"$actual should be $expected")

  extension (actual: String) {
    def shouldBe(expected: String): Unit = if (actual != expected) System.err.println(s"$actual should be $expected")
  }
  extension (actual: List[String]) {
    def shouldBe(expected: List[String]): Unit = actual zip expected foreach { case (a, e) => a shouldBe e }
  }
}