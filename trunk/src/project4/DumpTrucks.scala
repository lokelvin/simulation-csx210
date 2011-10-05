package project4

import simulation._
import simulation.process._
import simulation.stat._
import scala.collection.mutable.Queue
import project4.CallCenter.Director

/** An example simulation of a Dump Truck simulation using event scheduling.
 *  @author mepcotterell@gmail.com
 */
object DumpTrucks extends App with ProcessInteractionSimulation {

  class Director() extends Model

  // the director for this simulation
  implicit val director = new Director()

  // The number of loading docks
  val N_LOAD = 2
  
  // The number of weigh scales/stations
  val N_WEIGH = 1

  // the waiting line
  var loadQ : Queue[DumpTruck] = new Queue[DumpTruck]
  
  // the weighing queue
  var weighQ : Queue[DumpTruck] = new Queue[DumpTruck]
  
  // The service distributions
  val μLoadingDist 	= Map[Int, Double](  5 -> 0.30,  10 -> 0.80,  15 -> 1.00)
  val μWeighingDist = Map[Int, Double]( 12 -> 0.70,  16 -> 1.00)
  val μTravelDist 	= Map[Int, Double]( 40 -> 0.40,  60 -> 0.70,  80 -> 0.90, 100 -> 1.00)
  
  case class Loader () extends SimActor {
    var idle = true
    def act() {}
  }
  
  case class Weigher (id: Int) extends SimActor {
    var idle = true
    def act() {}
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
    def useLoader(loader : Loader) {
      loader.idle = false
      myLoader = loader
      director.schedule(this, DiscreteRand(μLoadingDist).toInt, actions.top)
    }
    
    def act() {
      
      var waitOnDirector = true
      
      loop {
        
        waitOnDirector = true
        
        // go through your script
        actions.pop match {
          
          // arrival at loading station
          case "arriveLoad" => {
            
          }
          
          // load
          case "load" => {
            
          }
          
          // arrive at weighing station
          case "arriveWeigh" => {
            
          }
          
          case "weigh" => {
            
          }
          
          
        } // actions.pop match
        
        if (waitOnDirector) {
          
          // relinquish control
          director ! "resume directing"
          
          // wait for messages
          receive {
          
            // the normal case
            case "resume acting" => {
              println("%10s %10s Person %d is about to %s".format(director.clock,"[action]",this.customerNumber,this.actions.top))
            }
            
            // the quit case
            case "quit" =>
            {
              // kill yourself
              exit()
            }
            
          } // if (waitOnDirector) 
          
       } // loop  
      
    } // def act()
      
  } // case class DumpTruck (id: Int)
  
  
  
  println("Print some statistics here")

}