package simulation
package event

import scala.collection.mutable.PriorityQueue

/** Event Scheduler
 *  @author mepcotterell@gmail.com
 */
case class Scheduler () {
  
  // "System" clock
  var clock: Int = 0
  
  // The future events list
  val futureEvents = PriorityQueue.empty[Event]

  //are we simulating?
  var simulating = true
  
  /** Schedules a future event
   *  @author mepcotterell@gmail.com
   *  @param event the future event
   *  @param delay the delay between now and when the event should occur
   */
  def schedule (event: Event, delay: Int) {
    
    // set the time the event was scheduled
    event.timeScheduled = clock
      
    // set the event time
    event.time = clock + delay

    //set the delay time
    event.delay = delay
    
    // add the event to the list
    futureEvents.enqueue(event)
    
    // print a nice message
    Console.BLUE
    println("%10s %10s Scheduled %s for t = %s".format(clock, "[action]", event, event.time))
    Console.RESET

  } // def schedule
  
  /** Cancels a future event
   *  @author mepcotterell@gmail.com
   *  @param event the event to be canceled.
   */
  def cancel (event: Event) = futureEvents.dropWhile(_ == event)

  /** Runs the simulation
   *  @author mepcotterell@gmail.com
   */
  def simulate (implicit trace: () => String = () => "") {
    
    // a flag that marks if the scheduler is simulating
    simulating = true
    
    while (simulating && !futureEvents.isEmpty) {
      
      // the next event 
      val event = futureEvents.dequeue()
      
      // update the "System" clock
      clock = event.time
      
      // print a nice message
      Console.GREEN
      if (!trace().isEmpty()) {
    	  println("%10s %10s %s".format(clock, "[trace]", trace()))
      }
      println("%10s %10s Handling %s".format(clock, "[action]", event))
      Console.RESET
      
      // let the event occur
      event.occur
      
    } // whiled
    
  } // def simulate
  
} // case class Scheduler

/** Test application for the Scheduler class
 *  @author mepcotterell@gmail.com
 */
object SchedulerTestApp extends App {
  
  import scala.util.Random
  val r = new Random
  
  implicit val sched = Scheduler()
  
  case class Person (name: String) extends Entity
  
  case class Arrival (e: Entity) extends Event (e) {   
    def occur {
    	schedule(Departure(e), r.nextInt(9))
    }
  }
  
  case class Departure (e: Entity) extends Event (e) {   
    def occur {
    	
    }
  }
 
  sched.schedule(Arrival(Person("Bob")), r.nextInt(9))
  sched.schedule(Arrival(Person("Bill")), r.nextInt(9))
  sched.schedule(Arrival(Person("Beth")), r.nextInt(9))

  
  sched.simulate()
}