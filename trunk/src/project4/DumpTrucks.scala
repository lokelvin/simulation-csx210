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

  //the director for this simulation
  implicit val director = new Director

  // The number of loading docks
  val N_LOAD = 2
  
  // The number of weigh scales/stations
  val N_WEIGH = 1

  println("Print some statistics here")

}