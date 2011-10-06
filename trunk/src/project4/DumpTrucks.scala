package project4

import simulation._
import simulation.process._
import simulation.stat._
import scala.collection.mutable.Queue

/** An example simulation of a Dump Truck simulation using event scheduling.
 *  @author mepcotterell@gmail.com
 */
object DumpTrucks extends App with ProcessInteractionSimulation {

  // Assign values to the Simulation variables
  tStart = 0
  tStop  = 60
  
  class Director() extends Model

  // the director for this simulation
  implicit val director = new Director()

  def getClock = director.clock
  
  // The number of loading docks
  val N_LOAD = 2
  
  // The number of weigh scales/stations
  val N_WEIGH = 1

  // number of customers in the system
  def L = L_LOAD + L_WEIGH + loadQ.size + weighQ.size
  
  // the number of customers being loaded
  var L_LOAD = 0
  
  // the number of customers being weighed
  var L_WEIGH = 0
  
  // the waiting line
  var loadQ : Queue[SimActor] = new Queue[SimActor]
  
  // the weighing queue
  var weighQ : Queue[SimActor] = new Queue[SimActor]

  // Duration Statistics
  val LW_L_STAT = DurationStatistic(() => (loadQ.size, getClock))
  val LS_L_STAT = DurationStatistic(() => (L_LOAD, getClock))
  
  val LW_W_STAT = DurationStatistic(() => (weighQ.size, getClock))
  val LS_W_STAT = DurationStatistic(() => (L_WEIGH, getClock))
  
  // The service distributions
  val μLoadingDist 	= Map[Int, Double](  5 -> 0.30,  10 -> 0.80,  15 -> 1.00)
  val μWeighingDist = Map[Int, Double]( 12 -> 0.70,  16 -> 1.00)
  val μTravelDist 	= Map[Int, Double]( 40 -> 0.40,  60 -> 0.70,  80 -> 0.90, 100 -> 1.00)

  // the loader
  case class Loader (serviceTime : Map[Int,Double]) extends Entity {
    var idle = true
  }
  
  val loaderA = Loader(μLoadingDist)
  val loaderB = Loader(μLoadingDist)
  
  // the weigher
  case class Weigher (serviceTime : Map[Int,Double]) extends Entity {
    var idle = true
  }
  
  val weigher = Weigher(μWeighingDist)
  
  // the stopping "event"/process which kills the system
  case class Stopper() extends SimActor {
    def act() {
      director.simulating = false
      director ! "resume directing"
      
      for (i <- 1 to 1000000000) {}
      
      println
      println("STATISTICS")
      println("----------------------------------------------------------------------------")
      println("| %10s | %10s | %10s | %10s | %20s |".format("STAT", "MIN", "MAX", "SAMPLES", "MEAN"))
      println("----------------------------------------------------------------------------")
      println("| %10s | %10s | %10s | %10s | %20s |".format("LQ_LOAD", LW_L_STAT.min, LW_L_STAT.max, LW_L_STAT.n, LW_L_STAT.mean))
      println("| %10s | %10s | %10s | %10s | %20s |".format("LS_LOAD", LS_L_STAT.min, LS_L_STAT.max, LS_L_STAT.n, LS_L_STAT.mean))
      println("| %10s | %10s | %10s | %10s | %20s |".format("L_LOAD", "n/a", "n/a", "n/a", LW_L_STAT.mean + LS_L_STAT.mean))
      println("| %10s | %10s | %10s | %10s | %20s |".format("LQ_WEIGH", LW_W_STAT.min, LW_W_STAT.max, LW_W_STAT.n, LW_W_STAT.mean))
      println("| %10s | %10s | %10s | %10s | %20s |".format("LS_WEIGH", LS_W_STAT.min, LS_W_STAT.max, LS_W_STAT.n, LS_W_STAT.mean))
      println("| %10s | %10s | %10s | %10s | %20s |".format("L_WEIGH", "n/a", "n/a", "n/a", LW_W_STAT.mean + LS_W_STAT.mean))
      println("----------------------------------------------------------------------------")
      
    }
  }
  
  /** Dump Truck Actor
   *  @author mepcotterell@gmail.com
   */
  case class DumpTruck (id: Int) extends SimActor {
    
    // to be set upon arrival
    var arrivalTime = 0
    
    // who is my loader
    var myLoader: Loader = null
    
    // who is my weigher
    var myWeigher: Weigher = null
    
    /**
     * Claim a loader
     * @param cashier the cashier to be claimed
     */
    def useLoader {
      
      L_LOAD += 1
      
      if (loaderA.idle) {
        myLoader = loaderA
      } else {
        myLoader = loaderB
      } 
      
      myLoader.idle = false
      director.schedule(this, DiscreteRand(μLoadingDist).toInt, actions.top)
      
      LW_L_STAT.takeSample
      LS_L_STAT.takeSample
      LW_W_STAT.takeSample
      LS_W_STAT.takeSample
      
    }
    
    /**
     * Release a loader
     */
    def releaseLoader {
      
      L_LOAD -= 1
      myLoader.idle = true
      
      // if there are still people in line, give the cashier to them immediately
      if (loadQ.size > 0) {
        val actor = loadQ.dequeue
        println("%s dequeued for %s, waited %d".format(actor, actor.actions.top, director.clock-actor.asInstanceOf[DumpTruck].arrivalTime))
        director.schedule(actor, 0, actor.actions.top)
      }
      
      LW_L_STAT.takeSample
      LS_L_STAT.takeSample
      LW_W_STAT.takeSample
      LS_W_STAT.takeSample
      
    }
    
    /**
     * Claim a weigher
     * @param cashier the cashier to be claimed
     */
    def useWeigher {
      
      L_WEIGH += 1   
      weigher.idle = false
      myWeigher = weigher
      director.schedule(this, DiscreteRand(μWeighingDist).toInt, actions.top)
      
      LW_L_STAT.takeSample
      LS_L_STAT.takeSample
      LW_W_STAT.takeSample
      LS_W_STAT.takeSample
      
    }
    
    /**
     * Release a weigher
     */
    def releaseWeigher {
      
      L_WEIGH -= 1
      myWeigher.idle = true
      
      //if there are still people in line, give the cashier to them immediately
      if (weighQ.size > 0) {
        val actor = weighQ.dequeue
        println("%s dequeued for %s, waited %d".format(actor, actor.actions.top, director.clock-actor.asInstanceOf[DumpTruck].arrivalTime))
        director.schedule(actor,0,actor.actions.top)
      }
      
      LW_L_STAT.takeSample
      LS_L_STAT.takeSample
      LW_W_STAT.takeSample
      LS_W_STAT.takeSample
      
    }
    
    def travel {

      releaseWeigher
      director.schedule(this, DiscreteRand(μTravelDist).toInt, actions.top)
      
      LW_L_STAT.takeSample
      LS_L_STAT.takeSample
      LW_W_STAT.takeSample
      LS_W_STAT.takeSample
      
    }
    
    
    // my script
    actions.push("weigh", "arriveWeigh", "load", "arriveLoad",
    			 "weigh", "arriveWeigh", "load", "arriveLoad",
    			 "weigh", "arriveWeigh", "load", "arriveLoad",
    			 "weigh", "arriveWeigh", "load", "arriveLoad",
    			 "weigh", "arriveWeigh", "load", "arriveLoad",
    			 "weigh", "arriveWeigh", "load", "arriveLoad",
    			 "weigh", "arriveWeigh", "load", "arriveLoad",
    			 "weigh", "arriveWeigh", "load", "arriveLoad",
    			 "weigh", "arriveWeigh", "load", "arriveLoad",
    			 "weigh", "arriveWeigh", "load", "arriveLoad",
    			 "weigh", "arriveWeigh", "load", "arriveLoad",
    			 "weigh", "arriveWeigh", "load", "arriveLoad",
    			 "weigh", "arriveWeigh", "load", "arriveLoad")
    
    def act() {
      
      var waitOnDirector = true
      
      loop {
        
        waitOnDirector = true
        
        // go through your script
        actions.pop match {
          
          // arrival at loading station
          case "arriveLoad" => {
            
            println("%s received message arriveLoad".format(this))
            
            // set the arrival time
            arrivalTime = director.clock
            
            // if there are people in line, get in line
            if (L_LOAD >= N_LOAD) {
              println ("%s saw other trucks in the queue and decided to get into the queue.".format(this))
              loadQ.enqueue(this)
              waitOnDirector = true
            } else {
              println ("%s saw NO other trucks in the queue and decided to get loaded.".format(this))
              waitOnDirector = false
            }
            
            
            
          }
          
          // load
          case "load" => {
            println("%s received message Load".format(this))
            useLoader
          }
          
          // arrive at weighing station
          case "arriveWeigh" => {
           
            println("%s received message arriveWeigh".format(this))
            
            releaseLoader
            
            // set the arrival time
            arrivalTime = director.clock
            
            // if there are people in line, get in line
            if (L_WEIGH >= N_WEIGH) {
              weighQ.enqueue(this) 
              waitOnDirector = true
            } else {
              waitOnDirector = false
            }
            
          }
          
          // weigh
          case "weigh" => {
            println("%s received message weigh".format(this))
            useWeigher
          }
          
          // travel 
          case "travel" => {
            releaseWeigher
            println("%s received message travel".format(this))
            travel
          }
          
          
        } // actions.pop match
        
        if (waitOnDirector) {
          
          // relinquish control
          director ! "resume directing"
          
          // wait for messages
          receive {
            
            // the normal case
            case "resume acting" => {
            	println("%10s %10s %s is about to %s".format(director.clock,"[action]", this, this.actions.top))
            }
            
            // the quit case
            case "quit" =>
            {
              println("%10s %10s %s died.".format(director.clock,"[action]", this))
              // kill yourself
              exit()
            }
            
          } // if (waitOnDirector) 
          
        } // recieve
        
      } // loop  
      
    } // def act()
      
  } // case class DumpTruck (id: Int)
  
  // The six dump trucks
  val DT1 = DumpTruck(1)
  val DT2 = DumpTruck(2)
  val DT3 = DumpTruck(3)
  val DT4 = DumpTruck(4)
  val DT5 = DumpTruck(5)
  val DT6 = DumpTruck(6)  
  
  // tell the director about your wait queues
  director.waitQueues.enqueue(loadQ, weighQ)
  
  // schedule the first arrival
  director.schedule(DT1, 0, DT1.actions.top)
  director.schedule(DT2, 0, DT2.actions.top)
  director.schedule(DT3, 0, DT3.actions.top)
  director.schedule(DT4, 0, DT4.actions.top)
  director.schedule(DT5, 0, DT5.actions.top)
  director.schedule(DT6, 0, DT6.actions.top)
  

  //schedule the stopper class at time 60
  director.schedule(Stopper(), tStop, "Stop simulation")
  
  //run
  director.start
  
  

}