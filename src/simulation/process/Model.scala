package simulation
package process

import scala.actors._
import scala.actors.Actor._
import scala.collection.mutable.PriorityQueue
import event.Event

abstract class Model extends Actor {

  var clock = 0
  // The future events list
  val futureEvents = PriorityQueue.empty[SimActor]
  var simulating = true

  def schedule (actor : SimActor, delay: Int) {

    actor.actTime = clock + delay
    // add the event to the list
    futureEvents.enqueue(actor)

    // print a nice message
    Console.BLUE
    println("%10s %10s Scheduled %s for t = %s".format(clock, "[action]", actor, actor.actTime))
    Console.RESET

  } // def schedule

  def act() {
    while (simulating && !futureEvents.isEmpty) {
      val actor = futureEvents.dequeue()
      clock = actor.actTime
      if (actor.yetToAct) {
        actor.nowActing
        actor.start
      }
      else {
        actor ! "resume acting"
      }
      receive {case "resume directing" => {println("Director received message!")}}
    }
  }
  
} // class Model