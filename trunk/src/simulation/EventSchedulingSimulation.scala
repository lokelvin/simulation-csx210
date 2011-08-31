package simulation

/** Provides useful variables and functions for event scheduling simulation
 *  @author mepcotterell@gmail.com
 */
trait EventSchedulingSimulation extends Simulation {

  println("Setting up the environment for an event scheduling simulation [trait EventSchedulingSimulation]")
  
  // implicitly available event scheduler
  implicit val scheduler = Scheduler()
  
  /** The current time in the simulation
   *  @author mepcotterell@gmail.com
   */
  def clock = scheduler.clock
  
  /** Schedules a future event
   *  @author mepcotterell@gmail.com
   *  @param event the future event
   *  @param delay the delay between now and when the event should occur
   */
  def schedule (event: Event, delay: Int) = scheduler.schedule(event, delay)
  
  /** Runs the simulation
   *  @author mepcotterell@gmail.com
   */
  def simulate = scheduler.simulate
  
  
} // trait EventScheduling