package examples.event

import simulation._
import simulation.event._

/** An example simulation of a Bank using event scheduling.
 *  @author mepcotterell@gmail.com
 */
object Bank extends App with EventSchedulingSimulation {

  // Assign values to the Simulation variables
  tStart = 0
  tStop  = 60
  λ      = 10.0
  μ      = 4.0
  
  // total number of tellers in the bank
  val N = 2
  
  // the number of customers in the bank
  var L = 0
  
  // the total number of customers serviced
  var nServiced = 0
  
  // the service times
  var serviceTimes = scala.collection.mutable.ListBuffer.empty[Double]
  
  // the wait times
  var waitTimes = scala.collection.mutable.ListBuffer.empty[Double]
  
  /** Represents a customer in the bank
   *  @author mepcotterell@gmail.com
   */
  case class Customer extends Entity

  /** The Arrival Event
   *  @author mepcotterell@gmail.com
   *  @param person the Customer associated with the event
   */
  case class Arrival (customer: Customer) extends Event (customer) {
    def occur {
           
      if (clock <= tStop) {
        
        // schedule a new arrival
        schedule(Arrival(Customer()), Rand(1.0 / λ))
        
      } // if
      
      if (L < N) {
        
        // the service time
        val delay = Rand(1.0 / μ)
        
        // add to the list of service times
        serviceTimes += delay
        
        // add to the list of wait times
        waitTimes += 0
        
        // schedule a departure
        schedule(Departure(customer), delay)
        
      } // if
      
      // increase the number of customers in the bank
      L += 1
      
    } // def occur
  } // case class MakeCall
  
  /** The Departure Event
   *  @author mepcotterell@gmail.com
   *  @param person the Customer associated with the event
   */
  case class Departure (customer: Customer) extends Event (customer) {
    def occur {
      
      // increase the number of customers serviced
      nServiced += 1
      
      if (L > N) {
      
        // the service time
        val delay = Rand(1.0 / μ)
        
        // add to the list of service times
        serviceTimes += delay
        
        // add to the list of wait times
        waitTimes += clock - timeScheduled
          
        // schedule a departure
        schedule(Departure(customer), delay)
      
      } // if
      
      // decrease the number of customers in the bank
      L -= 1
      
    } // def occur
  } // case class HangUp
  
  // schedule the first event
  schedule(Arrival(Customer()), tStart + Rand(1.0 / λ))
  
  // run the simulation
  simulate
  
  // compute the average service time
  val avgServiceTime = serviceTimes.reduceLeft(_+_) / serviceTimes.size
  
  // compute the average wait time
  val avgWaitTime = waitTimes.reduceLeft(_+_) / waitTimes.size
  
  // print out some information
  println
  println("The total number of customers serviced was %s".format(nServiced))
  println("The average service time was %s".format(avgServiceTime))
  println("The average wait time was %s".format(avgWaitTime))

} // object Bank