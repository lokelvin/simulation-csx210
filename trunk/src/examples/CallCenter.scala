package examples

import simulation._

/** An example simulation of a Call Center using event scheduling.
 *  @author mepcotterell@gmail.com
 */
object CallCenter extends App {

  // the start time of the simulation
  val tStart = 0;
  
  // the end time of the simulation
  val tStop = 100;
  
  // The arrival rate
  val λ = 9.0
  
  // The service rate
  val μ = 2.0
  
  // the random number generator
  val r = new scala.util.Random
  
  // the total number of calls
  var nCalls: Int = 0
  
  // the event scheduler
  implicit val sched = Scheduler()
  
  /** Random distribution
   *  @author mepcotterell@gmail.coms
   */
  def Rand (p: Double) = r.nextInt((1.0 / p).toInt)
  
  /** Represents a person in the simulation
   *  @author mepcotterell@gmail.com
   */
  case class Person extends Entity
  
  /** Represents the operator in the Call Center
   *  @author mepcotterell@gmail.com
   */
  case class Operator extends Entity {
    
    // Indicates whether or not the Operator is idle
    var idle = true
    
  } // case class Operator
  
  val operator = Operator()
  
  /** The MakeCall Event
   *  @author mepcotterell@gmail.com
   *  @param person the Person associated with the event
   */
  case class MakeCall (person: Person) extends Event (person) {
    def occur {
      
      // increase the total number of calls
      nCalls += 1
      
      if (operator.idle) {
        schedule(HangUp(person), Rand(1.0 / μ))
        operator.idle = false
      } // if
      
      if (clock <= tStop) {
        schedule(MakeCall(Person()), Rand(1.0 / λ))
      } // if
      
    } // def occur
  } // case class MakeCall
  
  /** The HangUp Event
   *  @author mepcotterell@gmail.com
   *  @param person the Person associated with the event
   */
  case class HangUp (person: Person) extends Event (person) {
    def occur {
            
      if (operator.idle) {
        operator.idle = true
      } // if
      
    } // def occur
  } // case class HangUp
  
  // schedule the first event
  sched.schedule(MakeCall(Person()), Rand(1.0 / μ))
  
  // run the simulation
  sched.simulate
  
} // object CallCenter