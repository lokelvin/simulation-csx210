package simulation

import scala.collection.mutable.PriorityQueue

case class Scheduler () {
  
  var clock: Int = 0
  val futureEvents = PriorityQueue.empty[Event]
  
  
  
  def schedule (event: Event, delay: Int) {
    event.time = clock + delay
    futureEvents.enqueue(event)
    println("\tScheduled %s for t = %s".format(event, event.time))
  }
  
  def cancel (event: Event) = futureEvents.dropWhile(_ == event)

  def simulate {
    
    var simulating = true
    
    while (simulating && !futureEvents.isEmpty) {
      val event = futureEvents.dequeue
      clock = event.time
      println("t = %s".format(clock))
      println("\tHandling %s".format(event))
      event.occur
    }
    
  }
  
}

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

  
  sched.simulate
}