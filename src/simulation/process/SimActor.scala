package simulation
package process

import scala.actors._
import scala.actors.Actor._

abstract class SimActor (implicit model: Model) extends Actor with Ordered[SimActor] {

  var actTime = 0
  
    // mandatory implementation of Ordered.compare
  def compare (that: SimActor) = (that.actTime - this.actTime)
  
} // SimActor