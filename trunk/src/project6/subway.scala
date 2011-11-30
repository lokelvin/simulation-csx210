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
  tStop  =10000
  
  import scalation.random._


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
      
      //for (i <- 1 to 10000) {}
      
      println
      println("STATISTICS FOR LINE")
      println
      println("-------------------------------------")
      println("| %10s | %20s |".format("STAT", "MEAN"))
      println("-------------------------------------")
      println("| %10s | %20s |".format("LQ", λ * (waitTimesLine / servedCustomersLine)))
      println("| %10s | %20s |".format("LS", λ * (serviceTimesLine /  servedCustomersLine)))
      println("| %10s | %20s |".format("L", λ * (( waitTimesLine /  servedCustomersLine) + ( serviceTimesLine /  servedCustomersLine))))
      println("-------------------------------------")
      println("| %10s | %20s |".format("WQ", ( waitTimesLine /  servedCustomersLine)))
      println("| %10s | %20s |".format("WS", ( serviceTimesLine / servedCustomersLine)))
      println("| %10s | %20s |".format("W", ( waitTimesLine /  servedCustomersLine) + ( serviceTimesLine /  servedCustomersLine)))
      println("-------------------------------------")
   
      println
      println("MARKVOVIAN CALCULATED STATISTICS FOR LINE")
      println("-------------------------------------")
      println("| %10s | %20s |".format("STAT", "MEAN"))
      println("-------------------------------------")
      
      println("| %10s | %20s |".format("LQ", LQ_LINE ))
      println("| %10s | %20s |".format("LS", L_LINE-LQ_LINE                ))
      println("| %10s | %20s |".format("L",  L_LINE       ))
      
      println("-------------------------------------")
      
      println("| %10s | %20s |".format("WQ", WQ_LINE ))
      println("| %10s | %20s |".format("WS", W_LINE-WQ_LINE           ))
      println("| %10s | %20s |".format("W",  W_LINE ))
      
      println("-------------------------------------")


      println
      println("STATISTICS FOR REGISTER")
      println
      println("-------------------------------------")
      println("| %10s | %20s |".format("STAT", "MEAN"))
      println("-------------------------------------")
      println("| %10s | %20s |".format("LQ", λ2 * (waitTimesReg / servedCustomersReg)))
      println("| %10s | %20s |".format("LS", λ2 * (serviceTimesReg / servedCustomersReg)))
      println("| %10s | %20s |".format("L", λ2 * ((waitTimesReg / servedCustomersReg) + (serviceTimesReg / servedCustomersReg))))
      println("-------------------------------------")
      println("| %10s | %20s |".format("WQ", (waitTimesReg / servedCustomersReg)))
      println("| %10s | %20s |".format("WS", (serviceTimesReg / servedCustomersReg)))
      println("| %10s | %20s |".format("W", (waitTimesReg / servedCustomersReg) + (serviceTimesReg / servedCustomersReg)))
      println("-------------------------------------")

      println
      println("MARKVOVIAN CALCULATED STATISTICS FOR REGISTER")
      println("-------------------------------------")
      println("| %10s | %20s |".format("STAT", "MEAN"))
      println("-------------------------------------")

      println("| %10s | %20s |".format("LQ", (ρ2 * ρ2) / (1 - ρ2) ))
      println("| %10s | %20s |".format("LS", ρ2                 ))
      println("| %10s | %20s |".format("L",  ρ2 / (1 - ρ2)       ))

      println("-------------------------------------")

      println("| %10s | %20s |".format("WQ", (ρ2 / μ_3) / (1 - ρ2) ))
      println("| %10s | %20s |".format("WS", (1 / μ_3)           ))
      println("| %10s | %20s |".format("W",  (1 / μ_3) / (1 - ρ2) ))
      println("-------------------------------------")

      
      println
      println("VARIABLES")
      println("----------------------------")
      println("| %s = %20s |".format("λ", λ ))
      println("| %s = %20s |".format("μ1", μ_1 ))
      println("| %s = %20s |".format("μ2", μ_2 ))
      println("| %s = %20s |".format("ρ1", ρ1 ))
      println("| %s = %20s |".format("λ2", λ2 ))
      println("| %s = %20s |".format("μ3", μ_3 ))
      println("| %s = %20s |".format("ρ2", ρ2 ))
      println("----------------------------")
      

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

      if (ANIMATING)
        customer.setPosition(server.service.getCenterOfServer(customer.size,server.serverNo))

      val stime = exp(1.0/myServer.serviceRate)
      val waitTime = director.clock - arrivalTime

      serviceTimesLine += stime
      waitTimesLine += waitTime
      servedCustomersLine += 1
      
      director.schedule(this, stime)
    }

    def useCashier()    {
        cashier.idle = false

        if (ANIMATING)
          customer.setPosition(cashier.service.getCenterOfServer(customer.size,1))

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

      if (ANIMATING)
        customer.setPosition(myServer.service.getCenter(customer.size))
    }

    def releaseCashier()   {
        cashier.idle = true

        //if there are still people in line, give the cashier to them immediately
        if (!cashier.waitQ.isEmpty) {
          val actor = cashier.waitQ.remove(0)
          director.schedule(actor, 0)
        }

        if (ANIMATING)
          customer.setPosition(cashier.service.getCenter(customer.size))
    }


    /**
     * Wait for messages from the director to continue through the loop
     */
    def act() {
            arrivalTime = director.clock             //set the arrival time
            
            nCustomers +=  1 //increment the number of customers through the system
            
            director.schedule(SimCustomer(nCustomers), exp(1.0/λ)) //schedule another arrival

            if (ANIMATING)
            {
              customerList.add(customer)
              customer.move(source2Line)
            }
            
            //if there are people in line, get in line
            if (!lineWorker1.idle && !lineWorker2.idle) {
              waitQLine.add(this)
              if (ANIMATING)
                line1Q.enterQueue()
              yieldToDirector()
              if (ANIMATING)
                line1Q.leaveQueue(waitQLine)
            }

            if (lineWorker1.idle)
              useLineWorker(lineWorker1)
            else if (lineWorker2.idle)
              useLineWorker(lineWorker2)
            else
            {
              println("You shouldn't be here!")
              exit()
            }

            yieldToDirector()

            releaseServer()

            if (ANIMATING)
              customer.move(line2Register)

            if (!cashier.idle)
            {
              waitQReg.add(this)
              if (ANIMATING)
                registerQ.enterQueue()
              yieldToDirector()
              if (ANIMATING)
                registerQ.leaveQueue(waitQReg)
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

            if (ANIMATING)
            {
              customer.move(register2Split)

              if (util.Random.nextDouble() < .25 )
              {
                customer.move(split2Lobby)
                if (lobby.enterLobby(customer))
                {
                  director.schedule(this,2.5)
                  yieldToDirector()
                  lobby.leaveLobby(customer)
                }
                customer.move(lobby2Split)
                customer.move(split2Door)
              }
              else
              {
                customer.move(split2Door)
              }
              customerList.remove(customer)
            }



             //println(director.clock+": "+this+" exiting")
            director ! "resume directing" //relinquish control
            exit()
        }
   }


  var ANIMATING  = true
  if (args.length != 1)
  {
    println("Usage: ./Subway true|false")
    exit()
  }
  if (args(0).matches("false"))
    ANIMATING = false

  var customerList = new ArrayList[Customer]()
  if (ANIMATING) {
      val animator = new Animator("Subway",List(source2Line,line2Register, split2Door, register2Split, split2Lobby, lobby2Split,source,line1,line1Q,register,registerQ,
          door,split,lobby),customerList)
  }

  val actor = SimCustomer(nCustomers)    //schedule the first arrival at time 0
  director.schedule(actor,0)

  director.schedule(Stopper(),tStop) //schedule the stopper class

  director.start() //run

 }