package edu.neu.coe.csye7200.labsorted.leetcode

import edu.neu.coe.csye7200.labsorted.lbsort.Comparer
import edu.neu.coe.csye7200.labsorted.leetcode.TransitionTime.OrderingTime

/**
  * Meeting schedule class.
  *
  * This is the solution to a LeetCode (premium) question: https://leetcode.com/problems/meeting-rooms-ii.
  *
  * The problem is to find the number of meeting rooms required to accommodate a list of meetings.
  *
  * What invariants do we have?
  * (1) the total rooms (i.e. vacant rooms plus engaged rooms) is a non-negative constant;
  * (2) the numbers of vacant rooms and engaged rooms are non-negative.
  *
  * Our solution takes a list of Meetings which is then converted to a list of meeting transitions.
  * The transitions are ordered, from which we can derive the number of engaged rooms (the "rooms") before and after each transition.
  * From the list of depths, we can yield the maximum rooms which must be the required number of rooms, such that the vacant rooms never goes negative.
  *
  * @param ms a list of Meetings (in any order).
  */
case class MeetingSchedule(ms: Seq[Meeting]) {
  /**
    * Method to concatenate two MeetingSchedules.
    *
    * @param m another MeetingSchedule.
    * @return a new MeetingSchedule with all meetings from each of this and m.
    */
  def ++(m: MeetingSchedule): MeetingSchedule = MeetingSchedule(ms ++ m.ms)

  /**
    * Method to add a meeting to a MeetingSchedule.
    * Order is not significant so this and +: are effectively equivalent.
    *
    * @param m a Meeting.
    * @return a new MeetingSchedule containing the existing meetings together with the additional meeting.
    */
  def :+(m: Meeting): MeetingSchedule = MeetingSchedule(ms :+ m)


  /**
    * Method to add a meeting to a MeetingSchedule.
    * Order is not significant so this and +: are effectively equivalent.
    *
    * @param m a Meeting.
    * @return a new MeetingSchedule containing the existing meetings together  with the additional meeting.
    */
  def +:(m: Meeting): MeetingSchedule = MeetingSchedule(m +: ms)

  private lazy val transitions: Seq[Transition] = ms flatMap {
    m => Seq(Transition(m.start, start = true), Transition(m.stop, start = false))
  }

  private lazy val orderedTransitions: Seq[Transition] = transitions.sorted

  private lazy val rooms: Seq[Int] = orderedTransitions.scanLeft(0)((d, t) => if (t.start) d + 1 else d - 1)

  /**
    * Method to yield the total rooms required (the invariant) chosen such that the vacant rooms is always at least 0.
    */
  lazy val totalRooms: Int = rooms.max

  val f: Int => Double = _ * 2
  val g: Double => Double = _ + 1
  val h: Double => String = _.toString
  val fgh: Int => String = x => h(g(f(x)))
  val p = f andThen g andThen h
}

object MeetingSchedule {
  def create(ms: Meeting*): MeetingSchedule = MeetingSchedule(ms)
}

/**
  * A Meeting Transition time in hours and minutes.
  *
  * @param hours   a number between 0 and 23, inclusive.
  * @param minutes a number between 0 and 59 inclusive.
  */
case class TransitionTime(hours: Int, minutes: Int) {
  /**
    * Method to get the actual number of minutes elapsed since midnight.
    *
    * @return the number of minutes after midnight of a transition.
    */
  def ticks: Int = 60 * hours + minutes

  override def toString: String = (hours * 100 + minutes).toString
}

object TransitionTime {
  def apply(t: Int): TransitionTime = TransitionTime(t / 100, t % 100)

  implicit object OrderingTime extends Ordering[TransitionTime] {
    def compare(x: TransitionTime, y: TransitionTime): Int = x.ticks.compare(y.ticks)
  }
}

/**
  * A meeting transition at time t which is either starting or stopping.
  *
  * @param t     the transition time.
  * @param start true if the meetings starts at this time, otherwise false if it stops.
  */
case class Transition(t: TransitionTime, start: Boolean) {
  override def toString: String = s"""$t: ${if (start) "start" else "stop"}"""
}

object Transition {

  /**
    * Method to instantiate a new Transition, based on the time ("Zulu" time) and a Boolean (start/stop).
    *
    * @param t     the transition time in Zulu time.
    * @param start true if this is the start of a meeting, false otherwise.
    * @return a Transition.
    */
  def apply(t: Int, start: Boolean): Transition = Transition(TransitionTime(t), start)

  /**
    * Convenient method to create a start Transition.
    *
    * @param t the time, as an Int.
    * @return a Transition.
    */
  def start(t: Int): Transition = apply(t, start = true)

  /**
    * Convenient method to create a stop Transition.
    *
    * @param t the time, as an Int.
    * @return a Transition.
    */
  def stop(t: Int): Transition = apply(t, start = false)

  // NOTE that we need to sort transitions first by time, and then by boolean (false should come before true)
  val comparerTransitionTime: Comparer[TransitionTime] = OrderingTime
  val comparerBoolean: Comparer[Boolean] = implicitly[Ordering[Boolean]]
  val comparerTime: Comparer[Transition] = comparerTransitionTime.unMap(_.t)
  val comparerStart: Comparer[Transition] = comparerBoolean.unMap(_.start)
  val comparer: Comparer[Transition] = comparerTime orElse comparerStart
  implicit val ordering: Ordering[Transition] = comparer.toOrdering
}

case class Meeting(start: TransitionTime, stop: TransitionTime)

object Meeting {
  def apply(start: Int, stop: Int): Meeting = Meeting(TransitionTime(start), TransitionTime(stop))
}

