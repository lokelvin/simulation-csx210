package examples
package event

import simulation._

/** An example simulation of a Call Center using event scheduling.
 *  @author mepcotterell@gmail.com
 */
object CallCenter extends App with EventSchedulingSimulation {

  // Assign values to the Simulation variables
  tStart = 0
  tStop  = 100
  λ      = 9.0
  μ      = 2.0
  
  // the total number of calls
  var nCalls: Int = 0
  
  /** Represents a person in the simulation
   *  @author mepcotterell@gmail.com
   */
  case class Person() extends Entity
  
  /** Represents the operator in the Call Center
   *  @author mepcotterell@gmail.com
   */
  case class Operator() extends Entity {
    
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
      
        // schedule a hangup
        schedule(HangUp(person), Rand(1.0 / μ))
        
        // tell the operator to stop service
        operator.idle = false
      
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
      operator.idle = true
      
    } // def occur
  } // case class HangUp
  
  // schedule the first event
  schedule(MakeCall(Person()), tStart + Rand(1.0 / λ))
  
  // run the simulation
  simulate
  
  // print out some information
  println
  println("The total number of calls was %s".format(nCalls))

  
} // object CallCenter