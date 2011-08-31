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
    
    def startService {
      println("\tOperator is starting service...")
      this.idle = false
    }
    
    def stopService {
      println("\tOperator is stopping service...")
      this.idle = true
    }
    
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
      
        // schedule a hangup
        schedule(HangUp(person), Rand(1.0 / μ))
        
        // tell the operator to stop service
        operator.startService
      
      } // if
      
      if (clock <= tStop) {
        
        // schedule a new call
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
      
      // tell the operator to stop service
      operator.stopService
      
    } // def occur
  } // case class HangUp
  
  // schedule the first event
  sched.schedule(MakeCall(Person()), tStart + Rand(1.0 / λ))
  
  // run the simulation
  sched.simulate
  
  // print out some information
  println("The total number of calls was %s".format(nCalls))
  
} // object CallCenter