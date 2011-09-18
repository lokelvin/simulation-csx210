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

  val N_SERVERS = 2

  var waitQ : Queue[Customer] =  new Queue[Customer]

  tStart = 0
  tStop  = 60
  λ      = 8.0
  μDist = Map[Int,Double](1->0.10,2->0.30,3->0.60,4->0.85,5->0.95,6->1.0)

  var nCustomers = 0

  var L = 0

  case class Cashier() extends Entity {
    var idle = true
  }

  val cashier = Cashier()

  case class Customer(customerNumber : Int) extends SimActor {

    def act() {

    }
  }

}