package simulation

/** Provides useful variables and functions for simulations.
 *  @author mepcotterell@gmail.com
 */
trait Simulation {

  println("Setting up the environment for a simulation [trait Simulation]")
  
  // the start time of the simulation
  var tStart: Int = 0
  
  // the end time of the simulation
  var tStop: Int = 100
  
  // The arrival rate
  var λ: Double = 1.0
  
  // The service rate
  var μ: Double = 1.0

  //The second service rate
  var μ2: Double = 1.0

  //The Interarrival distribution
  var λDist : Map[Int,Double] = Map[Int,Double](1->1.0)

  //The Service Distribution
  var μDist: Map[Int,Double] = Map[Int,Double](1->1.0)

  //The second service Distribution
  var μ2Dist: Map[Int,Double] = Map[Int,Double](1->1.0)
  
  // the random number generator
  private val r = new scala.util.Random
  
  /** Random distribution
   *  @author mepcotterell@gmail.coms
   */
  def Rand (p: Double) = 1 + r.nextInt((1.0 / p).toInt - 1)

  /**Discrete random distribution
   *  @author rbedger@gmail.com
   */
  def DiscreteRand(map: Map[Int,Double]):Double = {
    val randNum : Double = r.nextDouble()
    for ((key, value) <- map) if (randNum < value) return key
    return -1
  } //DiscreteRand
  
  /** Runs the simulation
   *  @author mepcotterell@gmail.com
   */
  //def simulate (implicit trace: () => String = () => "") : Unit

} // trait Simulation
