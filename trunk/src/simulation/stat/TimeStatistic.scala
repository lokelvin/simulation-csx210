package simulation
package stat

/** Used to collect values and collect statistics. Based on Dr. Miller's
 *  ScalaTion Statistic code.
 *  @see http://cs.uga.edu/~jam/scalation/src/scalation/stat/Statistic.scala
 *  @author mepcotterell@gmail.com
 *  @param f a function that returns some tuple containing a sample and the clock
 */
case class TimeStatistic (getValues: () => (Double, Double)) {

  /** The last time a statistic was collected
   */
  var lastTime = 0.
  
  /** The number of samples
   */
  var n = 0
  
  /** The sum of the samples collected
   */
  var sum = 0.
  
  /** The max sample value collected
   */
  var max = 0.
  
  /** The min sample value collected
   */
  var min = 0.
  
  /** Return the average of the statistic
   */
  def mean = sum / lastTime
  
  /** Accumulate the next value weighted by its time duration and update 
   *  accumulators. Based on Dr. Miller's ScalaTion Statistic code.
   *  @see http://cs.uga.edu/~jam/scalation/src/scalation/stat/Statistic.scala
   *  @param x  the value to accumulate
   *  @param t  the time of the observation
   */
  def accumulate {
	
    // get the current values
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
    
  } // def accumulate
  
} // class TimeStatistic