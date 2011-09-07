package project2

import simulation._
import simulation.event._
import scala.collection.mutable.Queue

/** @see Dump Trucks for Hauling Coal: Example 3.5: Page 81
 *  @author mepcotterell@gmail.com
 */
object DumpTrucks extends App with EventSchedulingSimulation {

  // Assign values to the Simulation variables
  tStart = 0
  tStop  = 60
  
  // The number of trucks in the loader queue at time t
  var LQ = 0
  
  // The number of trucks (0, 1, or 2) being loaded at time t
  var L = 0
  
  // The number of trucks in the weigh queue
  var WQ = 0
  
  // The number of trucks (0 or 1) being weighed
  var W = 0
  
  // The service distributions
  val μLoadingDist 	= Map[Int, Double](  5 -> 0.30,  10 -> 0.80,  15 -> 1.00)
  val μWeighingDist = Map[Int, Double]( 12 -> 0.70,  16 -> 1.00)
  val μTravelDist 	= Map[Int, Double]( 40 -> 0.40,  60 -> 0.70,  80 -> 0.90, 100 -> 1.00)
  
  /** Dump Truck Entity
   */
  case class DumpTruck (id: Int) extends Entity
  
  // The six dump trucks
  val DT1 = DumpTruck(1)
  val DT2 = DumpTruck(2)
  val DT3 = DumpTruck(3)
  val DT4 = DumpTruck(4)
  val DT5 = DumpTruck(5)
  val DT6 = DumpTruck(6)
  
  // The loading queue
  var loaderQ = Queue.empty[DumpTruck]
  
  // The weighing queue
  var weighQ = Queue.empty[DumpTruck]
  
  /** A truck arrives at the loader queue.
   */
  case class Arrival (truck: DumpTruck) extends Event (truck) {
    def occur {
      
    } // def occur
  } // case class Arrival
  
  /** A truck ends loading.
   */
  case class DepartureFromLoadingQ (truck: DumpTruck) extends Event (truck) {
    def occur {
      
    } // def occur
  } // case class DepartureFromLoadingQ
  
  /** A truck ends weighing.
   */
  case class DepartureFromWeighingQ (truck: DumpTruck) extends Event (truck) {
    def occur {
      
    } // def occur
  } // case class DepartureFromWeighingQ
  
}