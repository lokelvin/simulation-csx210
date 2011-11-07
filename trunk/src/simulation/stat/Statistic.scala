package simulation
package stat

/** Used to accumulate values and collect statistics. Based on Dr. Miller's
 *  ScalaTion Statistic code.
 *  @see http://cs.uga.edu/~jam/scalation/src/scalation/stat/Statistic.scala
 *  @author mepcotterell@gmail.com
 *  @param f a function that returns some numeric value
 */
case class Statistic [A: Numeric] () {
  
  protected val numeric = implicitly[Numeric[A]]
  import numeric._
  
  implicit def makeFractional (n: A) = n.toDouble
  
  /** The number of samples
   */
  var n = 0
  
  /** The sum of the samples collected
   */
  var sum = 0.
  
  /** The max sample value collected
   */
  var max = zero
  
  /** The min sample value collected
   */
  var min = zero
  
  /** Return the average of the statistic
   */
  def mean = sum / n
  
  /** Accumulate the next value and update the accumulators. Based on 
   *  Dr. Miller's ScalaTion Statistic code.
   *  @see http://cs.uga.edu/~jam/scalation/src/scalation/stat/Statistic.scala
   */
  def takeSample (sample: A) {
    
    // increase the number of samples taken
    n     += 1
    
    // increase the sum
    sum   += sample
   
    // update the min and max
    if (sample < min) min = sample
    if (sample > max) max = sample
    
  } // def takeSample
  
} // case class Statistic

/** Used to collect values that are present for some duration of time and 
 *  collect statistics. Based on Dr. Miller's ScalaTion Statistic code.
 *  @see http://cs.uga.edu/~jam/scalation/src/scalation/stat/Statistic.scala
 *  @author mepcotterell@gmail.com
 *  @param f a function that returns some tuple containing a sample and the clock
 */
case class DurationStatistic [A: Numeric] (getValues: () => (A, Double)) extends Statistic [A] {
  
  /** The last time a statistic was collected
   */
  var lastTime = 0.
  
  /** Return the average of the statistic
   */
  override def mean = sum / lastTime
  
  /** Accumulate the next value weighted by its time duration and update 
   *  accumulators. Based on Dr. Miller's ScalaTion Statistic code.
   *  @see http://cs.uga.edu/~jam/scalation/src/scalation/stat/Statistic.scala
   *  @param x  the value to accumulate
   *  @param t  the time of the observation
   */
  def takeSample {
	
    // get the current (value, clock time) tuple
    val (x, t) = getValues()


    // how long did this sample last
    val duration = t - lastTime
    
    // update lastTime for next time
    lastTime	 = t
    
    // increase the number of samples taken
    n     += 1
    
    // increase the sum
    sum   += x * duration
   
    // update the min and max
    if (x < min) min = x
    if (x > max) max = x
    
  } // override def takeSample
  
} // class TimeStatistic