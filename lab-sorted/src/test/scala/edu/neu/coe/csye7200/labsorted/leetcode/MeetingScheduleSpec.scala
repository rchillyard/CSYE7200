package edu.neu.coe.csye7200.labsorted.leetcode

import edu.neu.coe.csye7200.labsorted.lbsort.Comparison
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MeetingScheduleSpec extends AnyFlatSpec with should.Matchers with PrivateMethodTester {

  behavior of "MeetingSchedule"

  private val noMeetings: MeetingSchedule = MeetingSchedule.create()
  private val lunchMeeting: Meeting = Meeting(1300, 1400)
  private val scrum: Meeting = Meeting(1330, 1450)
  private val oneMeeting: MeetingSchedule = noMeetings :+ lunchMeeting
  private val twoMeetings: MeetingSchedule = oneMeeting :+ scrum
  private val leetCodeMeetings1: MeetingSchedule = MeetingSchedule.create(Meeting(500, 800), Meeting(600, 800)) // Leetcode test
  private val leetCodeMeetings2: MeetingSchedule = MeetingSchedule.create(Meeting(900, 1000), Meeting(400, 900), Meeting(400, 1700)) // Leetcode test

  it should "get totalRooms" in {
    noMeetings.totalRooms shouldBe 0
    oneMeeting.totalRooms shouldBe 1
    twoMeetings.totalRooms shouldBe 2
    (twoMeetings :+ Meeting(1345, 1500)).totalRooms shouldBe 3
    ((Meeting(1310, 1320) +: twoMeetings) :+ Meeting(1345, 1500)).totalRooms shouldBe 3
    leetCodeMeetings1.totalRooms shouldBe 2
    leetCodeMeetings2.totalRooms shouldBe 2
  }

  it should "get rooms" in {
    val decorateDepth = PrivateMethod[Seq[Int]](Symbol("rooms"))
    noMeetings invokePrivate decorateDepth() shouldBe Seq(0)
    oneMeeting invokePrivate decorateDepth() shouldBe Seq(0, 1, 0)
    twoMeetings invokePrivate decorateDepth() shouldBe Seq(0, 1, 2, 1, 0)
    leetCodeMeetings2 invokePrivate decorateDepth() shouldBe Seq(0, 1, 2, 1, 2, 1, 0)
  }

  it should "get transitions" in {
    val decTransitions = PrivateMethod[Seq[Transition]](Symbol("transitions"))
    noMeetings invokePrivate decTransitions() shouldBe Nil
    oneMeeting invokePrivate decTransitions() shouldBe Seq(Transition(TransitionTime(1300), start = true), Transition(TransitionTime(1400), start = false))
    twoMeetings invokePrivate decTransitions() shouldBe Seq(Transition(TransitionTime(1300), start = true), Transition(TransitionTime(1400), start = false), Transition(TransitionTime(1330), start = true), Transition(TransitionTime(1450), start = false))
  }

  it should "get orderedTransitions" in {
    val decOrderedTransitions = PrivateMethod[Seq[Transition]](Symbol("orderedTransitions"))
    noMeetings invokePrivate decOrderedTransitions() shouldBe Nil
    oneMeeting invokePrivate decOrderedTransitions() shouldBe Seq(Transition(TransitionTime(1300), start = true), Transition(TransitionTime(1400), start = false))
    twoMeetings invokePrivate decOrderedTransitions() shouldBe Seq(Transition(TransitionTime(1300), start = true), Transition(TransitionTime(1330), start = true), Transition(TransitionTime(1400), start = false), Transition(TransitionTime(1450), start = false))
    leetCodeMeetings1 invokePrivate decOrderedTransitions() shouldBe Seq(Transition(TransitionTime(500), start = true), Transition(TransitionTime(600), start = true), Transition(TransitionTime(800), start = false), Transition(TransitionTime(800), start = false))
    leetCodeMeetings2 invokePrivate decOrderedTransitions() shouldBe Seq(Transition(TransitionTime(400), start = true), Transition(TransitionTime(400), start = true), Transition(TransitionTime(900), start = false), Transition(TransitionTime(900), start = true), Transition(TransitionTime(1000), start = false), Transition(TransitionTime(1700), start = false))
  }

  behavior of "comparers"
  it should "compare booleans" in {
    val comparer = Transition.comparerBoolean
    comparer((false, true)).toInt shouldBe -1
    comparer((false, false)).toInt shouldBe 0
    comparer((true, false)).toInt shouldBe 1
  }
  it should "compare transition times" in {
    val comparer = Transition.comparerTransitionTime
    comparer((TransitionTime(900), TransitionTime(1000))).toInt shouldBe -1
  }
  it should "compare transitions" in {
    val comparer = Transition.comparer
    comparer((Transition.start(900), Transition.start(1000))) shouldBe Comparison.less
    comparer((Transition.start(1000), Transition.start(1000))) shouldBe Comparison.same
    comparer((Transition.stop(1000), Transition.start(1000))) shouldBe Comparison.less
    comparer((Transition.start(1000), Transition.stop(1000))) shouldBe Comparison.more
  }
}