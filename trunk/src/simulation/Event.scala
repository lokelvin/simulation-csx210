package simulation

/** Event represents a future event in a simulation.
 *  @author mepcotterell@gmail.com
 *  @param entity the Entity associated with this event
 *  @param scheduler the Scheduler implicitly associated with this event
 */
abstract class Event (entity: Entity) (implicit scheduler: Scheduler) extends Ordered[Event] {

  // the time when the event was originally scheduled
  var timeScheduled: Int = 0
    
  // the event time
  var time: Int = 0
  
  /** The current time in the simulation
   *  @author mepcotterell@gmail.com
   */
  def clock = scheduler.clock
  
  // mandatory implementation of Ordered.compare
  def compare (that: Event) = (that.time - this.time)
  
  /** The event logic
   *  @author mepcotterell@gmail.com
   */
  def occur: Unit
  
  /** Schedules a future event using the implicitly associated Scheduler
   *  @author mepcotterell@gmail.com
   *  @param event the future event
   *  @param delay the delay between now and when the event should occur
   */
  def schedule (event: Event, delay: Double) = scheduler.schedule(event, delay.toInt)
  
} // abstract class Event