package edu.neu.coe.csye7200.asstwc.fp

case class Timer(private val start: Long) {
  /**
   * Get the current lap time and a new Timer starting at this moment.
   *
   * @return (Long, Timer)
   */
  def lap: (Long, Timer) = {
    val millis = System.currentTimeMillis()
    millis - start -> Timer(millis)
  }
}

object Timer {
  def apply: Timer = new Timer(System.currentTimeMillis())
}