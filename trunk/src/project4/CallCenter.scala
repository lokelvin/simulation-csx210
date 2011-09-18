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

  val N_SERVERS = 2

  var N_CALLS = 0

  var waitQ : Queue[caller] =  new Queue[caller]

  // Assign values to the Simulation variables
  tStart = 0
  tStop  = 100
  λDist = Map[Int,Double](1->0.25,2->0.65,3->0.85,4->1.0)
  μDist = Map[Int,Double](2->0.30,3->0.58,4->0.83,5->1.0)
  μ2Dist = Map[Int,Double](3->0.35,4->0.60,5->0.80,6->1.0)

  var L = 0

  case class Operator() extends Entity {
    var idle = true
  }

  var able = Operator()
  var baker = Operator()

  case class caller(callerNumber : Int) extends SimActor() {
    var arrivalTime : Int = 0

    actions.push("leave","usePhone","arrive")

    var myOperator : Operator = null

    def useOperator(operator : Operator) {
      operator.idle = false
      this.myOperator = operator
    }

    def releaseOperator() {
      myOperator.idle = true
      println("L value = %d".format(L))
      if (L > N_SERVERS) {
        val actor = waitQ.dequeue
        director.schedule(actor,0,actor.actions.top)
        println("Caller %d dequeued for %s, waited %d".format(actor.callerNumber,actor.actions.top,director.clock-actor.arrivalTime))
      }
      L = L - 1
    }

    def act() {
         while (true) {
           actions.pop match {
             case "arrive" => {
               arrivalTime = director.clock
               N_CALLS = N_CALLS + 1
               director.schedule(caller(N_CALLS),DiscreteRand(λDist).toInt,"arrive")
               L = L + 1
               if (L > N_SERVERS) {
                 waitQ.enqueue(this)
               }
               else {
                 actions.pop
                 if (able.idle){
                   useOperator(able)
                   director.schedule(this,DiscreteRand(μDist).toInt,actions.top)
                 }
                 else if (baker.idle){
                   useOperator(baker)
                   director.schedule(this,DiscreteRand(μ2Dist).toInt,actions.top)
                 }

               }
             }
             case "usePhone" => {
                if (able.idle){
                   useOperator(able)
                   director.schedule(this,DiscreteRand(μDist).toInt,actions.top)
                 }
                 else if (baker.idle){
                   useOperator(baker)
                   director.schedule(this,DiscreteRand(μ2Dist).toInt,actions.top)
                 }
             }
             case "leave" => {
               releaseOperator()
               director !"resume directing"
               exit()
             }
           }
          director ! "resume directing"
          receive {case "resume acting" => {println("\t\t%d [action] Person %d is %s".format(director.clock,this.callerNumber,this.actions.top))}}
          }
         }
    }


    val actor = caller(N_CALLS)
    director.schedule(actor,0,actor.actions.top)
    director.start

}