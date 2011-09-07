package project2

import simulation._
import simulation.event._
import scala.collection.mutable.Queue

/** @see Grocery Store Checkout Counter: Example 3.3: Page 77
 *  @author mepcotterell@gmail.com
 */
object CheckoutCounter extends App with EventSchedulingSimulation {
 // Assign values to the Simulation variables
  tStart = 0
  tStop  = 60
  λ      = 8.0
  μDist = Map[Int,Double](1->0.10,2->0.30,3->0.60,4->0.85,5->0.95,6->1.0)

  //The waiting queue
  var waitQ : Queue[Person] = new Queue[Person]

  //the max # of customers in the queue
  var waitMax = 0

  //the total time waited
  var waitTotal = 0

  //the max length of the wait queue
  var qMax = 0

  // the total number of customers
  var nCustomers: Int = 1

  // the amount of time the cashier has been busy, total
  var busyTime = 0

  /** Represents a person in the simulation
   *  @author mepcotterell@gmail.com
   */
  case class Person(personNum: Int) extends Entity {
    var timeArrived = 0
    var timeWaited = 0
  }

  /** Represents the operator in the Call Center
   *  @author mepcotterell@gmail.com
   */
  case class Server() extends Entity {

    // Indicates whether or not the Operator is idle
    var idle = true

  } // case class Operator

  val cashier = Server()

/**
   * Enqueues the person and performs some menial tasks
   * @author rbedger@gmail.com
   */
  def enQ(waitQ : Queue[Person],person : Person) {
     waitQ.enqueue(person)
     if (waitQ.length > qMax)
        qMax = waitQ.length
  }

  /**
   * Dequeues the first person in line and performs some menial tasks
   * @return the person dequeued
   * @author rbedger@gmail.com
   */
  def dQ(waitQ : Queue[Person]) = {
    val dQ = waitQ.dequeue()
    dQ.timeWaited = clock - dQ.timeArrived
    if (dQ.timeWaited > waitMax)
      waitMax = dQ.timeWaited
    waitTotal += dQ.timeWaited
    println("\tPerson %d waited %d time units".format(dQ.personNum,dQ.timeWaited))
    dQ
  }


  /** The Arrival Event
   *  @author mepcotterell@gmail.com
   *  @param person the Person associated with the event
   */
  case class Arrival (person: Person) extends Event (person) {
    def occur {

      // increase the total number of calls
      nCustomers += 1
      //set the time the person arrive
      person.timeArrived = clock
      if (cashier.idle) {

        // schedule a hangup
        schedule(Departure(person), DiscreteRand(μDist))

        // tell able to stop service
        cashier.idle = false

      } // if
      else {
         enQ(waitQ,person)
      } //else

        // schedule a new call
        schedule(Arrival(Person(nCustomers+1)), Rand(1/λ))

    } // def occur
  } // case class Arrival

  /** The Departure Event
   *  @author mepcotterell@gmail.com
   *  @param person the Person associated with the event
   */
  case class Departure (person: Person) extends Event (person) {
    def occur {
      busyTime = busyTime + delay
      // tell the operator to stop service
      cashier.idle = true
      if (waitQ.length > 0) {
        schedule(Departure(dQ(waitQ)),DiscreteRand(μDist))
        cashier.idle = false;
      }

    } // def occur
  } // case class Departure

  /**The class which represents the end of the simulation
   * @author rbedger@gmail.com
   */
  case class Stop() extends Event(null)  {
    def occur {
      scheduler.simulating = false
    }
  }

  // schedule the second arrival
  schedule(Arrival(Person(2)), tStart + 1)
  //schedule the first departure
  schedule(Departure(Person(1)),tStart + 4)
  //schedule the stopping event
  schedule(Stop(),tStop)
  // run the simulation
  simulate

  // print out some information
  println()
  println("The total number of customers was %s".format(nCustomers))
  println("The total time the cashier was busy was %d units of time".format(busyTime))
  println("Server utilization is %3.2f".format(busyTime.toDouble/tStop))
  println("Max queue length was %d customers".format(qMax))


}