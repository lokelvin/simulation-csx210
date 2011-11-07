package project4

import collection.mutable.Queue

import simulation.Entity
import simulation.process.{SimActor, Model, ProcessInteractionSimulation}
import simulation.stat._

/**
 * Created by IntelliJ IDEA.
 * User: robert
 * Date: 9/18/11
 * Time: 12:26 AM
 * To change this template use File | Settings | File Templates.
 */

object CheckoutCounterExp extends App with ProcessInteractionSimulation {
  class Director() extends Model

  //the director
  implicit val director = new Director

  //the number of servers
  val N_SERVERS = 1

  //the waiting line
  var waitQ =  new Queue[Customer]

  //simulation variables
  tStart = 0
  tStop  = 2000
  
  import scalation.random._
  
  λ      = 8.0
  μ      = 10.0
  
  val ρ = λ / μ

  def log (x: Double): Double = java.lang.Math.log(x)
  
  def exp (mean: Double) = -mean * log(util.Random.nextDouble)
  
  //number of customers that have been through the system
  var nCustomers = 0

  //number of customers in the system
  var L = 0
  def LQ = waitQ.size
  def LS = if (L > N_SERVERS) N_SERVERS else L
  
  def getClock = director.clock
  
  var waitTimes = 0.
  var serviceTimes = 0.
  var servedCustomers = 0
  
  
  //the cashier
  case class Cashier(serviceRate : Double) extends Entity {
    var idle = true
  }

  val cashier = Cashier(μ)

  //the stopping "event"/process which kills the system
  case class Stopper() extends SimActor {
    def act() {
      
      director.simulating = false
      
      for (i <- 1 to 10000) {}
      
      println
      println("STATISTICS")
      println
      println("-------------------------------------")
      println("| %10s | %20s |".format("STAT", "MEAN"))
      println("-------------------------------------")
      println("| %10s | %20s |".format("LQ", λ * (waitTimes / servedCustomers)))
      println("| %10s | %20s |".format("LS", λ * (serviceTimes / servedCustomers)))
      println("| %10s | %20s |".format("L", λ * ((waitTimes / servedCustomers) + (serviceTimes / servedCustomers))))
      println("-------------------------------------")
      println("| %10s | %20s |".format("WQ", (waitTimes / servedCustomers)))
      println("| %10s | %20s |".format("WS", (serviceTimes / servedCustomers)))
      println("| %10s | %20s |".format("W", (waitTimes / servedCustomers) + (serviceTimes / servedCustomers)))
      println("-------------------------------------")
   
      println
      println("MARKVOVIAN CALCULATED STATISTICS")
      println("-------------------------------------")
      println("| %10s | %20s |".format("STAT", "MEAN"))
      println("-------------------------------------")
      
      println("| %10s | %20s |".format("LQ", (ρ * ρ) / (1 - ρ) ))
      println("| %10s | %20s |".format("LS", ρ                 ))
      println("| %10s | %20s |".format("L",  ρ / (1 - ρ)       ))
      
      println("-------------------------------------")
      
      println("| %10s | %20s |".format("WQ", (ρ / μ) / (1 - ρ) ))
      println("| %10s | %20s |".format("WS", (1 / μ)           ))
      println("| %10s | %20s |".format("W",  (1 / μ) / (1 - ρ) ))
      
      println("-------------------------------------")
      
      println
      println("VARIABLES")
      println("----------------------------")
      println("| %s = %20s |".format("λ", λ ))
      println("| %s = %20s |".format("μ", μ ))
      println("| %s = %20s |".format("ρ", ρ ))
      println("----------------------------")
      

      director ! "resume directing"
  
    }
  }

  //the Customer
  case class Customer(customerNumber : Int) extends SimActor {
    //to be set upon arrival
    var arrivalTime = 0.0
    //who is my cashier?
    var myCashier : Cashier = null

    /**
     * Claim a cashier
     * @param cashier the cashier to be claimed
     */
    def useCashier(cashier : Cashier) {
      cashier.idle = false
      myCashier = cashier

      val stime = exp(1.0/cashier.serviceRate)
      val waitTime = director.clock - arrivalTime

      serviceTimes += stime
      waitTimes += waitTime
      servedCustomers += 1
      
      director.schedule(this, stime)//,actions.top)
    }

    /**
     * Release my cashier
     */
    def releaseCashier() {
      myCashier.idle = true

      //if there are still people in line, give the cashier to them immediately
      if ( L > N_SERVERS ) {
        val actor = waitQ.dequeue()
//        println("%s dequeued waited %s".format(actor,director.clock-actor.arrivalTime))
        director.schedule(actor, 0)
      }
    }

    /**
     * Wait for messages from the director to continue through the loop
     */
    def act() {
            arrivalTime = director.clock             //set the arrival time
            
            nCustomers +=  1 //increment the number of customers through the system
            
            director.schedule(Customer(nCustomers), exp(1.0/λ)) //schedule another arrival
            
            L += 1 //increment the number of customers in the system
            
            //if there are people in line, get in line
            if (L > N_SERVERS) {
              waitQ.enqueue(this)
              yieldToDirector()
            }
            
//            assert(cashier.idle,"Error: Cashier should be idle upon entry of 'Checkout' state")
            
            useCashier(cashier)

            yieldToDirector()

            releaseCashier()
            
            L -= 1 //decrement the number of customers in the system
            
            director ! "resume directing" //relinquish control
            
        }
   }
  

  

  val actor = Customer(nCustomers)    //schedule the first arrival at time 0
  director.schedule(actor,0)

  director.schedule(Stopper(),tStop) //schedule the stopper class

  director.start() //run

 }