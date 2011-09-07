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
  
  // The loading queue
  var loaderQ = Queue.empty[DumpTruck]
  
  // The weighing queue
  var weighQ = Queue.empty[DumpTruck]
  
  // The number of trucks in the loader queue at time t
  def LQ = loaderQ.size
  
  // The number of trucks (0, 1, or 2) being loaded at time t
  var L = 0
  
  // The number of trucks in the weigh queue
  def WQ = weighQ.size
  
  // The number of trucks (0 or 1) being weighed
  var W = 0
  
  // The total busy time of both loaders from time tStart
  var BL = 0.0
  
  // The total busy time of the scale from time tStart
  var BS = 0.0
  
  
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
  
  /** A truck arrives at the loader queue.
   */
  case class Arrival (truck: DumpTruck) extends Event (truck) {
    def occur {
      
      if (clock <= tStop) {
     
        // if there is a free space on the loading deck then schedule a 
        // departure from the loading station
	    if (L < 2) {
	      
	      val delay = DiscreteRand(μLoadingDist).toInt
	      schedule(DepartureFromLoadingStation(truck), delay)
	      L  = L + 1
	      BL = BL + delay
	      
	    // else go into the loading queue
	    } else {
	        
	      loaderQ += truck
	      
	    } // if
    
      } // if
      
    } // def occur
  } // case class Arrival
  
  /** A truck ends loading.
   */
  case class DepartureFromLoadingStation (truck: DumpTruck) extends Event (truck) {
    def occur {
     
      if (clock <= tStop) {
        
	    // This truck is no longer being loaded
	    L = L - 1
	    
	    // if there is a free space on the scale then schedule a 
        // departure from the weighing station
	    if (W < 1) {
	       
	      val delay = DiscreteRand(μTravelDist).toInt
	      schedule(DepartureFromWeighingStation(truck), delay)
	      W  = W + 1
	      BS = BS + delay
	    
	    // else go into the loading queue
	    } else {
	    
	      weighQ += truck
	      
	    } // if
	      
	    // Since we just sent someone away from the loading station, we should
	    // check the loading queue. If there is a truck in the loading queue
	    // schedule a departure from the loading station.
	    if (LQ > 0) {
	        
	        val otherTruck = loaderQ.dequeue
	        val delay = DiscreteRand(μLoadingDist).toInt
	        schedule(DepartureFromLoadingStation(otherTruck), delay)
	        
	        // We just increased the number of trucks loading
		    L  = L + 1
		    
		    // update our cumulative statistic
		    BL = BL + delay
	        
	      } // if
	      
      } // if
      
    } // def occur
  } // case class DepartureFromLoadingQ
  
  /** A truck ends weighing.
   */
  case class DepartureFromWeighingStation (truck: DumpTruck) extends Event (truck) {
    def occur {
      
      if (clock <= tStop) {
        
	    // This truck is no longer being weighed
	    W = W - 1
	      
	    // Send the truck off for traveling
	    schedule(Arrival(truck), DiscreteRand(μTravelDist).toInt)
	    
	    // Since we just sent someone away from the weighing station, we should
	    // check the weighing queue. If there is a truck in the weighing queue
	    // schedule a departure from the weighing station.
	    if (WQ > 0) {
	      
	      val otherTruck = weighQ.dequeue
	      val delay = DiscreteRand(μTravelDist).toInt
	      schedule(DepartureFromWeighingStation(otherTruck), delay)
	      
	      // We just increased the number of trucks on the scale
		  W  = W + 1
		  
		  // update our cumulative statistic
		  BS = BS + delay
	    
	    } // 
	      
      } // if
      
    } // def occur
  } // case class DepartureFromWeighingQ
  
  
  // To initialize the simulation, we assume that, at time 0, five trucks are
  // the loaders (LQ = 3, L = 2) and one is at the scale (WQ = 0, W = 1)
  L  = 2
  W  = 1  
 
  // Enqueue the three trucks in the loader queue
  loaderQ.enqueue(DT4, DT5, DT6)
  
  // Schedule truck 1 departing from the weigh station
  schedule(DepartureFromWeighingStation(DT1), DiscreteRand(μWeighingDist).toInt)
  
  // Schedule truck 2 and 3 departing from the loading stations
  schedule(DepartureFromLoadingStation(DT2), DiscreteRand(μLoadingDist).toInt)
  schedule(DepartureFromLoadingStation(DT3), DiscreteRand(μLoadingDist).toInt)

  // run the simulation
  simulate
  
  // print out some information
  println
  println("The number of trucks in the loading queue at the end of the simulation was %s".format(LQ))
  println("The number of trucks being loaded at the end of the simulation was %s".format(L))
  println("The number of trucks in the weighing queue at the end of the simulation was %s".format(WQ))
  println("The number of trucks on the scale at the end of the simulation was %s".format(W))
  println
  println("The average loader utilization was %s".format((BL / L) / clock))
  println("The average scale utilization was %s".format((BS / W) / clock))
  
}