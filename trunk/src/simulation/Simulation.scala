package simulation

/** Provides useful variables and functions for simulations.
 *  @author mepcotterell@gmail.com
 */
trait Simulation {

  println("Setting up the environment for a Simulation")
  
  // the start time of the simulation
  var tStart: Int = 0
  
  // the end time of the simulation
  var tStop: Int = 100
  
  // The arrival rate
  var λ: Double = 1.0
  
  // The service rate
  var μ: Double = 1.0
  
  // the random number generator
  private val r = new scala.util.Random
  
  /** Random distribution
   *  @author mepcotterell@gmail.coms
   */
  def Rand (p: Double) = 1 + r.nextInt((1.0 / p).toInt - 1)
  
  /** Runs the simulation
   *  @author mepcotterell@gmail.com
   */
  def simulate: Unit

} // trait Simulation
