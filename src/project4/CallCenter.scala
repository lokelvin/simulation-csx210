package project4

import simulation._
import simulation.process._
import simulation.stat._
import scala.collection.mutable.Queue
import project4.CallCenter.Director

/** An example simulation of a Call Center using event scheduling.
 *  @author mepcotterell@gmail.com
 */
object CallCenter extends App with ProcessInteractionSimulation {

  class Director() extends Model
  implicit val director = new Director

  case class caller() extends SimActor() {
    actions.push("leave","usePhone","arrive")
    def act() {
         while (true) {
           actions.pop match {
             case "arrive" => {
               director.schedule(caller(),10)
               println("Im here!")
               director.schedule(this,5)
             }
             case "usePhone" => {
               println("Im using the phone!")
               director.schedule(this,5)
             }
             case "leave" => {
               println("Bye!")
             }
           }
          director ! "resume directing"
          receive {case "resume acting" => {println("I'll resume")}}
          }
         }
    }



    director.schedule(caller(),0)
    director.start

}