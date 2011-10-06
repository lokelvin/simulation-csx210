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
      
      if (loaderA.idle == true) {
        myLoader = loaderA
      } else if (loaderB.idle == true) {
        myLoader = loaderB
      }
      
      myLoader.idle = false
      director.schedule(this, DiscreteRand(μLoadingDist).toInt, actions.top)
      
    }
    
    /**
     * Release a loader
     */
    def releaseLoader {
      
      L_LOAD -= 1
      myLoader.idle = true
      
      //if there are still people in line, give the cashier to them immediately
      if (L > N_LOAD) {
        val actor = loadQ.dequeue
        println("%s dequeued for %s, waited %d".format(actor, actor.actions.top, director.clock-actor.asInstanceOf[DumpTruck].arrivalTime))
        director.schedule(actor,0,actor.actions.top)
      }
      
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
    }
    
    /**
     * Release a weigher
     */
    def releaseWeigher {
      
      L_WEIGH -= 1
      myWeigher.idle = true
      
      //if there are still people in line, give the cashier to them immediately
      if (L > N_WEIGH) {
        val actor = weighQ.dequeue
        println("%s dequeued for %s, waited %d".format(actor, actor.actions.top, director.clock-actor.asInstanceOf[DumpTruck].arrivalTime))
        director.schedule(actor,0,actor.actions.top)
      }
    }
    
    def travel {
      releaseWeigher
      director.schedule(this, DiscreteRand(μTravelDist).toInt, actions.top)
    }
    
    def act() {
      
      var waitOnDirector = true
      
      loop {
        
        waitOnDirector = true
        
        // go through your script
        actions.pop match {
          
          // arrival at loading station
          case "arriveLoad" => {
            
            // set the arrival time
            arrivalTime = director.clock
            
            // if there are people in line, get in line
            if (L_LOAD > N_LOAD) loadQ.enqueue(this) else waitOnDirector = false
            
          }
          
          // load
          case "load" => {
            useLoader
          }
          
          // arrive at weighing station
          case "arriveWeigh" => {
           
            releaseLoader
            
            // set the arrival time
            arrivalTime = director.clock
            
            // if there are people in line, get in line
            if (L_WEIGH > N_WEIGH) weighQ.enqueue(this) else waitOnDirector = false
            
          }
          
          // weigh
          case "weigh" => useWeigher
          
          // travel 
          case "travel" => {
            releaseWeigher
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
            	println("%10s %10s Person %d is about to %s".format(director.clock,"[action]", this.id, this.actions.top))
            }
            
            // the quit case
            case "quit" =>
            {
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
  
  // To initialize the simulation, we assume that, at time 0, five trucks are
  // the loaders (LQ = 3, L = 2) and one is at the scale (WQ = 0, W = 1)
  L_LOAD   = 2
  L_WEIGH  = 1  
 
  // Enqueue the three trucks in the loader queue
  loadQ.enqueue(DT4, DT5, DT6)
  
  // tell the director about your wait queues
  director.waitQueues.enqueue(loadQ, weighQ)
  
  // schedule the first arrival
  director.schedule(DT1, 0, DT1.actions.top)

  //schedule the stopper class at time 60
  director.schedule(Stopper(), tStop, "Stop simulation")
  
  //run
  director.start
  
  println("Print some statistics here")

}