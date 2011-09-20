package project4

import collection.mutable.Queue
import simulation.Entity
import simulation.process.{SimActor, Model, ProcessInteractionSimulation}

/**
 * Created by IntelliJ IDEA.
 * User: robert
 * Date: 9/18/11
 * Time: 12:26 AM
 * To change this template use File | Settings | File Templates.
 */

object CheckoutCounter extends App with ProcessInteractionSimulation {
  class Director() extends Model

  //the director
  implicit val director = new Director

  //the number of servers
  val N_SERVERS = 1

  //the waiting line
  var waitQ : Queue[SimActor] =  new Queue[SimActor]

  //simulation variables
  tStart = 0
  tStop  = 60
  λ      = 8.0
  μDist = Map[Int,Double](1->0.10,2->0.30,3->0.60,4->0.85,5->0.95,6->1.0)

  //number of customers that have been through the system
  var nCustomers = 0

  //number of customers in the system
  var L = 0

  //the cashier
  case class Cashier(serviceTime : Map[Int,Double]) extends Entity {
    var idle = true
  }

  val cashier = Cashier(μDist)

  //the stopping "event"/process which kills the system
  case class Stopper() extends SimActor {
    def act() {
      director.simulating = false
      director ! "resume directing"
    }
  }

  //the Customer
  case class Customer(customerNumber : Int) extends SimActor {
    //to be set upon arrival
    var arrivalTime = 0
    //who is my cashier?
    var myCashier : Cashier = null

    //my script
    actions.push("leave","checkout","arrive")

    /**
     * Claim a cashier
     * @param cashier the cashier to be claimed
     */
    def useCashier(cashier : Cashier) {
      cashier.idle = false
      myCashier = cashier
      director.schedule(this,DiscreteRand(cashier.serviceTime).toInt,actions.top)
    }

    /**
     * Release my cashier
     */
    def releaseCashier() {
      myCashier.idle = true
      //if there are still people in line, give the cashier to them immediately
      if ( L > N_SERVERS ) {
        val actor = waitQ.dequeue().asInstanceOf[Customer]
        println("%s dequeued for %s, waited %d".format(actor,actor.actions.top,director.clock-actor.arrivalTime))
        director.schedule(actor,0,actor.actions.top)
      }
    }

    /**
     * Wait for messages from the director to continue through the loop
     */
    def act() {
      var waitOnDirector = true
      while (true) {
        waitOnDirector = true
        //go through your script
        actions.pop match {
            //arrival
          case "arrive" => {
            //set the arrival time
            arrivalTime = director.clock
            //increment the number of customers through the system
            nCustomers = nCustomers + 1
            //schedule another arrival
            director.schedule(Customer(nCustomers),Rand(1/λ),"arrive")
            //increment the number of customers in the system
            L = L + 1
            //if there are people in line, get in line
            if (L > N_SERVERS) {
              waitQ.enqueue(this)
            }
            else {
              waitOnDirector = false
            }
          }
            //checking out
          case "checkout" => {
            assert(cashier.idle,"Error: Cashier should be idle upon entry of 'Checkout' state")
            useCashier(cashier)
          }
            //leaving the store
          case "leave" => {
            //give up the cashier
            releaseCashier()
            //decrement the number of customers in the system
            L = L - 1
            //relinquish control
            director ! "resume directing"
            //kill yourself
            exit()
          }
        }
        if (waitOnDirector)
        {
          //relinquish control
          director ! "resume directing"
          //wait for messages
          receive {
            //the normal case
            case "resume acting" => {
              println("%10s %10s Person %d is about to %s".format(director.clock,"[action]",this.customerNumber,this.actions.top))
            }
              //the quit case
            case "quit" =>
            {
              //kill yourself
              exit()
            }
          }
        }
    }
  }
  }

  //tell the director about your wait queues
  director.waitQueues.enqueue(waitQ)
  //schedule the first arrival at time 0
  val actor = Customer(nCustomers)
  director.schedule(actor,0,actor.actions.top)
  //schedule the stopper class at time 60
  director.schedule(Stopper(),60,"Stop simulation")
  //run
  director.start()

  while (!director.finished) {

    }
  println("Print some statistics here")
}