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

  implicit val director = new Director

  val N_SERVERS = 1

  var waitQ : Queue[Customer] =  new Queue[Customer]

  tStart = 0
  tStop  = 60
  λ      = 8.0
  μDist = Map[Int,Double](1->0.10,2->0.30,3->0.60,4->0.85,5->0.95,6->1.0)

  var nCustomers = 0

  var L = 0

  case class Cashier(serviceTime : Map[Int,Double]) extends Entity {
    var idle = true
  }

  val cashier = Cashier(μDist)

  case class Customer(customerNumber : Int) extends SimActor {
    var arrivalTime = 0
    var myCashier : Cashier = null

    actions.push("leave","checkout","arrive")

    def useCashier(cashier : Cashier) {
      cashier.idle = false
      myCashier = cashier
      director.schedule(this,DiscreteRand(cashier.serviceTime).toInt,actions.top)
    }

    def releaseCashier() {
      myCashier.idle = true
      if ( L > N_SERVERS ) {
        val actor = waitQ.dequeue()
        println("%s dequeued for %s, waited %d".format(actor,actor.actions.top,director.clock-actor.arrivalTime))
        director.schedule(actor,0,actor.actions.top)
      }
    }

    def act() {
      while (true) {
        actions.pop match {
          case "arrive" => {
            arrivalTime = director.clock
            nCustomers = nCustomers + 1
            director.schedule(Customer(nCustomers),Rand(1/λ),"arrive")
            L = L + 1
            if (L > N_SERVERS) {
              waitQ.enqueue(this)
            }
            else {
              actions.pop
              assert(cashier.idle,"Error: Cashier should be idle upon entry into else block of 'Arrive' state")
              useCashier(cashier)
            }
          }
          case "checkout" => {
            assert(cashier.idle,"Error: Cashier should be idle upon entry of 'Checkout' state")
            useCashier(cashier)
          }
          case "leave" => {
            releaseCashier()
            L = L - 1
            director ! "resume directing"
            exit()
          }
        }
        director ! "resume directing"
        receive { case "resume acting" => println("\t\t%d [action] Person %d is about to %s".format(director.clock,this.customerNumber,this.actions.top))}
      }
    }
  }

  val actor = Customer(nCustomers)
  director.schedule(actor,0,actor.actions.top)
  director.start()
}