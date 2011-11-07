package simulation
package process

import scala.actors._
import scala.actors.Actor._
import event.Event
import collection.mutable.{Queue, PriorityQueue}

abstract class Model extends Actor {
  //the system clock
  var clock : Double = 0.0
  // The future events list
  val futureEvents = PriorityQueue.empty[SimActor]
  //are we simulating?
  var simulating = true

  var finished = false


  /**
   * Schedules a new actor on the FEL
   * @param actor the actor to schedule
   * @param delay the distance in the future to schedule this actor
   * @param nextState the state the actor will be entering
   */
  def schedule (actor : SimActor, delay: Double) {//), nextState : String) {
    //Set the actor's activation time
    actor.actTime = clock + delay
    // add the event to the list
    futureEvents.enqueue(actor)

    // print a nice message
    Console.BLUE
    println("%10s %10s Scheduled %s for t = %s".format(clock, "[action]", actor, actor.actTime))
    Console.RESET

  } // def schedule

  /**
   * Pull actors off of the FEL while the FEL is not empty and we are still simulating
   */
  def act() {
    while (simulating && !futureEvents.isEmpty) {
      //the next actor to act
      val actor = futureEvents.dequeue()
      //advance the clock
      clock = actor.actTime
      //either start or resume the actor
      if (actor.yetToAct) {
        actor.nowActing
        actor.start
      }
      else {
        actor ! "resume acting"
      }
      //wait for the actor to rescind control
      receive {case "resume directing" => {}}
    }

    //kill child processes so we can quit
    while (!futureEvents.isEmpty) {
      futureEvents.dequeue() ! "quit"
    }
    finished = true
  }
  
} // class Model