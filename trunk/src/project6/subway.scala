package project6

import collection.mutable.Queue

import simulation.Entity
import simulation.process.{SimActor, Model, ProcessInteractionSimulation}
import simulation.stat._
import java.util.ArrayList
import java.awt.geom.Point2D
import javax.swing.{JLabel, JPanel}
import util.control.Breaks._

/**
 * Created by IntelliJ IDEA.
 * User: robert
 * Date: 9/18/11
 * Time: 12:26 AM
 * To change this template use File | Settings | File Templates.
 */

object Subway extends App with ProcessInteractionSimulation {
  class Director() extends Model

  //the director
  implicit val director = new Director
  
  //rates
  λ        = 50.0
  val μLin = IndexedSeq(30.0)
  val μReg = IndexedSeq(35.0)
  
  //The animation entities
  val source =  new Source()
  val line1 = new Service(200,50, μLin.length)
  val line1Q = new WaitQueue(line1)
  val source2Line = new Path(source,line1Q)
  val register = new Service(400,60, μReg.length)
  val registerQ = new WaitQueue(register)
  val line2Register = new Path(line1,registerQ)
  val split = new Split(500,150)
  val register2Split = new Path(register,split)
  val door = new Source(600,150)
  val split2Door = new Path(split,door)
  val lobby = new Lobby(250,150,60,100)
  val split2Lobby = new Path(split,lobby)
  val lobby2Split = new Path(lobby,split)

  //the waiting lines
  var waitQLin =  new ArrayList[SimCustomer]
  var waitQReg =  new ArrayList[SimCustomer]

  //simulation variables
  tStart = 0
  tStop  = 8
 
  def log (x: Double): Double = java.lang.Math.log(x)
  
  def exp (mean: Double) = -mean * log(util.Random.nextDouble)
  
  //number of customers that have been through the system
  var nCustomers = 0

  def getClock = director.clock
  
  var waitTimesLine = 0.
  var serviceTimesLine = 0.
  var servedCustomersLine = 0

  var waitTimesReg = 0.
  var serviceTimesReg = 0.
  var servedCustomersReg = 0
  
  
  //the cashier
  case class Server(serviceRate : Double, waitQ : ArrayList[SimCustomer], val serverNo : Int, val service : Service) extends Entity {
    var idle = true
    override def toString = "Server(%d)".format(serverNo)
  }

  case class Cashier(serviceRate : Double, waitQ : ArrayList[SimCustomer], val serverNo : Int, val service : Service) extends Entity {
    var idle = true
    override def toString = "Cashier(%d)".format(serverNo)
  }

  var lineWorkers = for (i <- 0 until μLin.length) yield Server(μLin(i), waitQLin, i, line1)
  var cashiers = for (i <- 0 until μReg.length) yield Cashier(μReg(i), waitQReg, i, register)
  
  //the stopping "event"/process which kills the system
  case class Stopper() extends SimActor {
    def act() {
      
      director.simulating = false
      
      val LQ_LIN = λ * (waitTimesLine / servedCustomersLine)
      val LS_LIN = λ * (serviceTimesLine /  servedCustomersLine)
      val L_LIN  = LQ_LIN + LS_LIN
      
      val WQ_LIN = (waitTimesLine /  servedCustomersLine)
      val WS_LIN = (serviceTimesLine / servedCustomersLine)
      val W_LIN  = WQ_LIN + WS_LIN
      
      val LQ_REG = λ * (waitTimesReg / servedCustomersReg)
      val LS_REG = λ * (serviceTimesReg /  servedCustomersReg)
      val L_REG  = LQ_REG + LS_REG
      
      val WQ_REG = (waitTimesReg /  servedCustomersReg)
      val WS_REG = (serviceTimesReg / servedCustomersReg)
      val W_REG  = WQ_REG + WS_REG
      
      val totalTime = director.clock
      
      val wq1 = ( waitTimesLine /  servedCustomersLine)
      val wq2 = (waitTimesReg / servedCustomersReg)
      val nc  = nCustomers
      
      val ne  = lineWorkers.length + cashiers.length
      
      def payroll (ne: Int): Double = 7.5 * ne * (totalTime + 1)
      def net (nc: Double): Double = 5.0 * nc - math.max (wq1, wq2) * nc
      def profit (ne: Int, nc: Double): Double = net(nc) - payroll(ne)
      
      println("%.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.2f, %.2f, %.2f".format(LQ_LIN, LS_LIN, L_LIN, WQ_LIN, WS_LIN, W_LIN, LQ_REG, LS_REG, L_REG, WQ_REG, WS_REG, W_REG, totalTime, payroll(ne), net(nc), profit(3, nc)))

      director ! "resume directing"
      
      System.exit(0)
  
    }
  }

  //the Customer
  case class SimCustomer(customerNumber : Int) extends SimActor {
    
    //val customer : Customer = new Customer(source)

    //to be set upon arrival
    var arrivalTime = 0.0
    //who is my server?
    var myServer : Server = null
    var myCashier : Cashier = null

    /**
     * Claim a cashier
     * @param cashier the cashier to be claimed
     */
    def useLineWorker(server : Server) {
      server.idle = false
      myServer = server



      val stime = exp(1.0/myServer.serviceRate)
      val waitTime = director.clock - arrivalTime

      serviceTimesLine += stime
      waitTimesLine += waitTime
      servedCustomersLine += 1
      
      director.schedule(this, stime)
    }

    def useCashier(cashier: Cashier)    {
        cashier.idle = false
        myCashier = cashier

        val stime = exp(1.0/cashier.serviceRate)
        val waitTime = director.clock - arrivalTime

        serviceTimesReg += stime
        waitTimesReg += waitTime
        servedCustomersReg += 1

        director.schedule(this, stime)
    }

    /**
     * Release my cashier
     */
    def releaseServer() {
      myServer.idle = true

      //if there are still people in line, give the cashier to them immediately
      if (!myServer.waitQ.isEmpty) {
        val actor = myServer.waitQ.remove(0)
        director.schedule(actor, 0)
      }


    }

    def releaseCashier()   {
        myCashier.idle = true

        //if there are still people in line, give the cashier to them immediately
        if (!myCashier.waitQ.isEmpty) {
          val actor = myCashier.waitQ.remove(0)
          director.schedule(actor, 0)
        }


    }


    /**
     * Wait for messages from the director to continue through the loop
     */
    def act() {
            arrivalTime = director.clock             //set the arrival time
            
            nCustomers +=  1 //increment the number of customers through the system
            
            director.schedule(SimCustomer(nCustomers), exp(1.0/λ)) //schedule another arrival

            //if there are people in line, get in line
            if ((for (worker <- lineWorkers) yield !worker.idle).reduceLeft(_&&_)) {
              waitQLin.add(this)
              println("%10.6f %10s Customer %s is waiting in waitQLin".format(director.clock, "[event]", this))
              yieldToDirector()
            }

            breakable { 
              for (worker <- lineWorkers) {
            	  if (worker.idle) {
            	      println("%10.6f %10s Customer %s is using %s".format(director.clock, "[service]", this, worker))
            		  useLineWorker(worker)
            		  break
            	  }
              }
              println("stuck")
            }
            
            yieldToDirector()
            releaseServer()

            //if there are people in line, get in line
            if ((for (cashier <- cashiers) yield !cashier.idle).reduceLeft(_&&_)) {
              waitQReg.add(this)
              println("%10.6f %10s Customer %s is waiting in waitQReg".format(director.clock, "[event]", this))
              yieldToDirector()
            }

            breakable { 
              for (cashier <- cashiers) {
            	  if (cashier.idle) {
            		  println("%10.6f %10s Customer %s is using %s".format(director.clock, "[service]", this, cashier))
            		  useCashier(cashier)
            		  break
            	  }
              }
              println("stuck")
            }

            yieldToDirector()
            releaseCashier()

            println("%10.6f %10s Customer %s is waiting in departing".format(director.clock, "[event]", this))
            
             //println(director.clock+": "+this+" exiting")
            director ! "resume directing" //relinquish control
            exit()
        }
   }



  val actor = SimCustomer(nCustomers)    //schedule the first arrival at time 0
  director.schedule(actor,0)

  director.schedule(Stopper(),tStop) //schedule the stopper class

  director.start() //run

 }