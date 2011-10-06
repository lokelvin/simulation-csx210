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
  var waitQ : Queue[SimActor] =  new Queue[SimActor]

  //simulation variables
  tStart = 0
  tStop  = 1000
  
  import scalation.random._
  
  λ      = 8.0
  μ      = 10.0
  
  val ρ = λ / μ

   def log(x: Double): Double = java.lang.Math.log(x)
  def exp(n:Double) = -(1/n)*log(scala.util.Random.nextDouble())

  //def λDistribution = exp(1.0 / λ)
  //def μDistribution = exp(1.0 / μ)

  //number of customers that have been through the system
  var nCustomers = 0

  //number of customers in the system
  var L = 0
  def LW = waitQ.size
  def LS = if (L > N_SERVERS) N_SERVERS else L
  
  def getClock = director.clock
  
  // Duration Statistics
  val WQ_STAT = DurationStatistic(() => (LW, getClock))
  val WS_STAT = DurationStatistic(() => (LS, getClock))
  val W_STAT  = DurationStatistic(() => (L, getClock)) 
  
  val LQ_STAT = Statistic[Double]()
  val LS_STAT = Statistic[Double]()
  val L_STAT  = Statistic[Double]()

  def takeSamples {

        WS_STAT.takeSample
        WQ_STAT.takeSample
        W_STAT.takeSample

        LQ_STAT.takeSample(LW)
        LS_STAT.takeSample(LS)
        L_STAT.takeSample(L)
  }
  //the cashier
  case class Cashier(serviceTime : Double) extends Entity {
    var idle = true
  }

  val cashier = Cashier(μ)

  //the stopping "event"/process which kills the system
  case class Stopper() extends SimActor {
    def act() {
      
      director.simulating = false
      director ! "resume directing"
      
      for (i <- 1 to 10000) {}
      
      println
      println("STATISTICS")
      println("----------------------------------------------------------------------------")
      println("| %10s | %10s | %10s | %10s | %20s |".format("STAT", "MIN", "MAX", "SAMPLES", "MEAN"))
      println("----------------------------------------------------------------------------")
      
      println("| %10s | %10s | %10s | %10s | %20s |".format("LQ", LQ_STAT.min, LQ_STAT.max, LQ_STAT.n, LQ_STAT.mean))
      println("| %10s | %10s | %10s | %10s | %20s |".format("LS", LS_STAT.min, LS_STAT.max, LS_STAT.n, LS_STAT.mean))
      println("| %10s | %10s | %10s | %10s | %20s |".format("L", "n/a", "n/a", "n/a", L_STAT.mean))
      
      println("----------------------------------------------------------------------------")
      println("| %10s | %10s | %10s | %10s | %20s |".format("WQ", WQ_STAT.min, WQ_STAT.max, WQ_STAT.n, WQ_STAT.mean))
      println("| %10s | %10s | %10s | %10s | %20s |".format("WS", WS_STAT.min, WS_STAT.max, WS_STAT.n, WS_STAT.mean))
      println("| %10s | %10s | %10s | %10s | %20s |".format("W", "n/a", "n/a", "n/a", W_STAT.mean))
      println("----------------------------------------------------------------------------")
   
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
      
      
      
  
    }
  }

  //the Customer
  case class Customer(customerNumber : Int) extends SimActor {
    //to be set upon arrival
    var arrivalTime = 0.0
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
      director.schedule(this, exp(1/cashier.serviceTime).toInt,actions.top)
    }

    /**
     * Release my cashier
     */
    def releaseCashier() {
      myCashier.idle = true
      //if there are still people in line, give the cashier to them immediately
      if ( L > N_SERVERS ) {
        val actor = waitQ.dequeue().asInstanceOf[Customer]
        println("%s dequeued for %s, waited %s".format(actor,actor.actions.top,director.clock-actor.arrivalTime))
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
            director.schedule(Customer(nCustomers), exp(1/λ).toInt, "arrive")
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
        takeSamples
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
  director.schedule(Stopper(),tStop,"Stop simulation")
  //run
  director.start()

  while (!director.finished) {

    }
  println("Print some statistics here")
}