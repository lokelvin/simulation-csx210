package simulation
package process

import scala.actors._
import scala.actors.Actor._

abstract class SimActor (implicit model: Model) extends Actor with Ordered[SimActor] {

  var actTime = 0.0
  var yetToAct = true
  var actions : scala.collection.mutable.Stack[String] = new scala.collection.mutable.Stack[String]()
  def simulating = {model.simulating}
  
    // mandatory implementation of Ordered.compare
  def compare (that: SimActor) = (that.actTime - this.actTime).toInt

  def nowActing = {yetToAct = false}

  def yieldToDirector() {
    model ! "resume directing"
    receive {case "resume acting" => {println("I'll resume")}}
  }
  
} // SimActor