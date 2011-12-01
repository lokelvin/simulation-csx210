package project6

import collection.mutable.Queue

import simulation.Entity
import simulation.process.{SimActor, Model, ProcessInteractionSimulation}
import simulation.stat._
import java.util.ArrayList
import java.awt.geom.Point2D
import javax.swing.{JLabel, JPanel}

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

  //the number of servers
  def N_SERVERS  = N_SERVERS_LINE+N_SERVERS_REG
  val N_SERVERS_LINE = 2
  val N_SERVERS_REG = 1

  //The animation entities
  val source =  new Source()
  val line1 = new Service(200,50,N_SERVERS_LINE)
  val line1Q = new WaitQueue(line1)
  val source2Line = new Path(source,line1Q)
  val register = new Service(400,60,N_SERVERS_REG)
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
  var waitQLine =  new ArrayList[SimCustomer]
  var waitQReg =  new ArrayList[SimCustomer]

  //simulation variables
  tStart = 0
  tStop  =4380
 
  //rates
  λ      = 10.0

  val μ_1     = 7.0
  val μ_2     = 6.0

  def μ_12 = μ_1+μ_2

  def λ2      =  μ_12
  val μ_3     = 14.0
  
  val ρ1 = λ / μ_12
  val ρ2 = λ2 / μ_3


  //Statistics
  def factorial(factorial : Double) : Double = {
    var iFactorial = factorial
    if (factorial.intValue()==0)
      return 1
        for (j <- 1 to factorial.intValue())
          iFactorial *= j
        return iFactorial
      }

  def P0 : Double = {
    var sum = 0.0
    for(n <- 0 to 1)
    {
       sum+= math.pow(N_SERVERS_LINE*ρ1,n)/factorial(n)
    }
    sum += (math.pow(N_SERVERS_LINE*ρ1,N_SERVERS_LINE)*(1/factorial(N_SERVERS_LINE))*(1/1-ρ1))
    return math.pow(sum,-1)
  }

  def probAllSrvBsy : Double = (math.pow(N_SERVERS_LINE*ρ1,N_SERVERS_LINE)*P0)/(factorial(N_SERVERS_LINE)*(1-ρ1))

  def L_LINE : Double = N_SERVERS_LINE*ρ1 + LQ_LINE
  def W_LINE : Double = L_LINE/λ
  def WQ_LINE : Double = W_LINE-(1/(μ_12/2))
  def LQ_LINE : Double = (ρ1*probAllSrvBsy)/(1-ρ1)

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
  }

  case class Cashier(serviceRate : Double, waitQ : ArrayList[SimCustomer], val service : Service) extends Entity {
    var idle = true
  }

  val lineWorker1 = Server(μ_1,waitQLine,1,line1)
  val lineWorker2 = Server(μ_2,waitQLine,2,line1)
  val cashier = Cashier(μ_3,waitQReg,register)

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
      
      def payroll (ne: Int): Double = 7.5 * ne * totalTime
      def net (nc: Double): Double = 5.0 * nc - wq1 * nc - wq2 * nc
      def profit (ne: Int, nc: Double): Double = net(nc) - payroll(ne)
      
      println("%.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.2f, %.2f, %.2f".format(LQ_LIN, LS_LIN, L_LIN, WQ_LIN, WS_LIN, W_LIN, LQ_REG, LS_REG, L_REG, WQ_REG, WS_REG, W_REG, totalTime, payroll(3), net(nc), profit(3, nc)))

      director ! "resume directing"
  
    }
  }

  //the Customer
  case class SimCustomer(customerNumber : Int) extends SimActor {
    val customer : Customer = new Customer(source)

    //to be set upon arrival
    var arrivalTime = 0.0
    //who is my server?
    var myServer : Server = null

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

    def useCashier()    {
        cashier.idle = false



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
        cashier.idle = true

        //if there are still people in line, give the cashier to them immediately
        if (!cashier.waitQ.isEmpty) {
          val actor = cashier.waitQ.remove(0)
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
            if (!lineWorker1.idle && !lineWorker2.idle) {
              waitQLine.add(this)
              yieldToDirector()
            }

            if (lineWorker1.idle)
              useLineWorker(lineWorker1)
            else if (lineWorker2.idle)
              useLineWorker(lineWorker2)
            else
            {
              println("You shouldn't be here! 1")
              exit()
            }

            yieldToDirector()

            releaseServer()



            if (!cashier.idle)
            {
              waitQReg.add(this)

              yieldToDirector()

            }


            if (cashier.idle)
              useCashier()
            else
            {
              println("You shouldn't be here!")
              exit()
            }

            yieldToDirector()

            releaseCashier()



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