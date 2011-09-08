package simulation
package stat

/** Used to collect values and collect statistics. Based on Dr. Miller's
 *  ScalaTion Statistic code.
 *  @see http://cs.uga.edu/~jam/scalation/src/scalation/stat/Statistic.scala
 *  @author mepcotterell@gmail.com
 */
case class TimeStatistic () {

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
  def apply (x: Double, t: Double) {
	  
    val duration = t - lastTime
    lastTime	 = t
    
    n     += 1
    sum   += x * duration
   
    if (x < min) min = x
    if (x > max) max = x
    
  } // def accumulate
  
} // class TimeStatistic