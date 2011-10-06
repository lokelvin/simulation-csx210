package project4

import simulation._
import simulation.process._
import simulation.stat._
import scala.collection.mutable.Queue

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
  

  class Loader () extends SimActor {
    var idle = true
    def act () {}
  }
  
  class Weigher (id: Int) extends SimActor {
    var idle = true
    def act() {
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
    def useLoader(loader : Loader) {
      loader.idle = false
      myLoader = loader
      //director.schedule(this, DiscreteRand(μLoadingDist).toInt, actions.top)
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
  
  
  
  println("Print some statistics here")

}