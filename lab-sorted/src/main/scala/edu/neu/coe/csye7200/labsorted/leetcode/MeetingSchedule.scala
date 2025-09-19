package edu.neu.coe.csye7200.labsorted.leetcode

import edu.neu.coe.csye7200.labsorted.lbsort.Comparer
import edu.neu.coe.csye7200.labsorted.leetcode.Solution.solve
import edu.neu.coe.csye7200.labsorted.leetcode.TransitionTime.*

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

/**
 * Object MeetingSchedule provides methods to manipulate and construct MeetingSchedule instances.
 */
object MeetingSchedule {
  /**
   * Creates a MeetingSchedule instance from a variable number of Meeting instances.
   *
   * @param ms a variable number of Meeting objects to include in the MeetingSchedule.
   * @return a new MeetingSchedule containing the specified meetings.
   */
  def create(ms: Meeting*): MeetingSchedule = MeetingSchedule(ms)

  /**
   * Parses a list of strings representing meetings into a MeetingSchedule.
   * Each string in the input list is parsed using the Meeting.parse method,
   * and the resulting valid Meeting objects are aggregated into a MeetingSchedule.
   *
   * @param meetings a list of strings, where each string represents a meeting in the format "hh:mm - hh:mm".
   * @return a MeetingSchedule containing all valid Meeting objects parsed from the input list of strings.
   */
  def parse(meetings: List[String]): MeetingSchedule =
    MeetingSchedule(meetings.flatMap(Meeting.parse))
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
  /**
   * Creates a TransitionTime instance representing a transition time in hours and minutes.
   *
   * @param t an integer representation of time in the format hhmm (e.g., 1230 for 12:30).
   * @return a TransitionTime instance with hours and minutes extracted from the input integer.
   */
  def apply(t: Int): TransitionTime = TransitionTime(t / 100, t % 100)

  /**
   * Defines an implicit ordering for instances of TransitionTime based on their elapsed minutes
   * since midnight, represented by the `ticks` method of the TransitionTime class.
   * This ordering allows TransitionTime instances to be compared using their `ticks` values.
   */
  given TransitionTimeOrdering: Ordering[TransitionTime] with {
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

/**
 * Companion object for the Transition case class, providing factory methods
 * and comparers for sorting Transition instances.
 */
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
  val comparerTransitionTime: Comparer[TransitionTime] = TransitionTime.TransitionTimeOrdering
  val comparerBoolean: Comparer[Boolean] = implicitly[Ordering[Boolean]]
  val comparerTime: Comparer[Transition] = comparerTransitionTime.unMap(_.t)
  val comparerStart: Comparer[Transition] = comparerBoolean.unMap(_.start)
  val comparer: Comparer[Transition] = comparerTime orElse comparerStart
  given ordering: Ordering[Transition] = comparer.toOrdering
}

/**
 * Represents a meeting with a defined start and stop time.
 *
 * @constructor Creates a Meeting with the specified start and stop times.
 * @param start the start time of the meeting, represented as a TransitionTime.
 * @param stop  the stop time of the meeting, represented as a TransitionTime.
 */
case class Meeting(start: TransitionTime, stop: TransitionTime)

/**
 * Companion object for the Meeting case class.
 *
 * Provides utility methods to create and parse Meeting objects.
 */
object Meeting {
  def apply(start: Int, stop: Int): Meeting =
    Meeting(TransitionTime(start), TransitionTime(stop))

  /**
   * Parses a string representing a meeting schedule into an Option[Meeting].
   * The string should be in the format "hh:mm - hh:mm", where `hh` represents hours
   * (0-23) and `mm` represents minutes (0-59).
   *
   * @param string the input string to parse, representing the meeting time range.
   * @return an Option containing a Meeting object if the input string is valid;
   *         otherwise, None.
   */
  def parse(string: String): Option[Meeting] = {
    val regex = """(\d{2}):(\d{2}) - (\d{2}):(\d{2})""".r
    string match {
      case regex(s1, s2, e1, e2) =>
        for (x1 <- s1.toIntOption;x2 <- s2.toIntOption;y1 <- e1.toIntOption;y2 <- e2.toIntOption)
          yield Meeting(TransitionTime(x1, x2), TransitionTime(y1, y2))
      case _ => None
    }
  }
}