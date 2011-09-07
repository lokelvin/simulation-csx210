package simulation
package process

import scala.actors._
import scala.actors.Actor._
import scala.collection.mutable.PriorityQueue

abstract class Model extends Actor {

  // The future events list
  val futureEvents = PriorityQueue.empty[SimActor]
  
  
} // class Model