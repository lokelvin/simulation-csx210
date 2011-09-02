package project2

import simulation._

/** @see Grocery Store Checkout Counter: Example 3.3: Page 77
 *  @author mepcotterell@gmail.com
 */
object CheckoutCounter extends App with EventSchedulingSimulation {
 // Assign values to the Simulation variables
  tStart = 0
  tStop  = 60
  //λ      = 9.0
  λDist = Map[Int,Double](1->0.25,2->0.65,3->0.85,4->1.0)
  //μ      = 8.0
  μDist = Map[Int,Double](2->0.30,3->0.58,4->0.83,5->1.0)

  // the total number of calls
  var nCustomers: Int = 0

  /** Represents a person in the simulation
   *  @author mepcotterell@gmail.com
   */
  case class Person() extends Entity

  /** Represents the operator in the Call Center
   *  @author mepcotterell@gmail.com
   */
  case class Server() extends Entity {

    // Indicates whether or not the Operator is idle
    var idle = true

  } // case class Operator

  val cashier = Server()

  /** The Arrival Event
   *  @author mepcotterell@gmail.com
   *  @param person the Person associated with the event
   */
  case class Arrival (person: Person) extends Event (person) {
    def occur {

      // increase the total number of calls
      nCustomers += 1

      if (cashier.idle) {

        // schedule a hangup
        schedule(Departure(person), DiscreteRand(μDist))

        // tell able to stop service
        cashier.idle = false

      } // if

      if (clock <= tStop) {

        // schedule a new call
        schedule(Arrival(Person()), DiscreteRand(λDist))

      } // if

    } // def occur
  } // case class Arrival

  /** The Departure Event
   *  @author mepcotterell@gmail.com
   *  @param person the Person associated with the event
   */
  case class Departure (person: Person) extends Event (person) {
    def occur {

      // tell the operator to stop service
      cashier.idle = true

    } // def occur
  } // case class Departure

  // schedule the first event
  schedule(Arrival(Person()), tStart + DiscreteRand(λDist).toInt)

  // run the simulation
  simulate

  // print out some information
  println
  println("The total number of customers was %s".format(nCustomers))


}