package project2

import simulation._
import simulation.event._
import scala.collection.mutable.Queue

/** An example simulation of a Call Center using event scheduling.
 *  @author mepcotterell@gmail.com
 */
object CallCenter extends App with EventSchedulingSimulation {

  // Assign values to the Simulation variables
  tStart = 0
  tStop  = 100
  λDist = Map[Int,Double](1->0.25,2->0.65,3->0.85,4->1.0)
  μDist = Map[Int,Double](2->0.30,3->0.58,4->0.83,5->1.0)
  μ2Dist = Map[Int,Double](3->0.35,4->0.60,5->0.80,6->1.0)
  
  // the total number of calls
  var nCalls: Int = 0

  //the waiting queue
  var waitQ : Queue[Person] = new Queue[Person]

  //the highest waiting time
  var waitMax = 0

  //the total time waited
  var waitTotal = 0

  //the max length of the waitQ
  var qMax = 0

  
  /** Represents a person in the simulation
   *  @author mepcotterell@gmail.com
   */
  case class Person(personNum : Int) extends Entity {
    var timeArrived = 0
    var timeWaited = 0
  }
  
  /** Represents the operator in the Call Center
   *  @author mepcotterell@gmail.com
   */
  case class Operator() extends Entity {
    
    // Indicates whether or not the Operator is idle
    var idle = true
    
  } // case class Operator
  
  val able = Operator()
  val baker = Operator()
  
  /** The MakeCall Event
   *  @author mepcotterell@gmail.com
   *  @param person the Person associated with the event
   */
  case class ArrivalEvent (person: Person) extends Event (person) {
    def occur {
      // increase the total number of calls
      nCalls += 1
      // set the time the Person arrived
      person.timeArrived = clock
      if (able.idle) {
        //put them in the waiting queue
        if (waitQ.length > 0){
          enQ(waitQ,person)
        }
        // or schedule a hangup
        else
          schedule(AbleEndSrv(person), DiscreteRand(μDist))
        
        // tell able to stop service
        able.idle = false
      
      } // if
      else if (baker.idle) {
        if (waitQ.length > 0)
          enQ(waitQ,person)
        // schedule a hangup
        else
          schedule(BakerEndSrv(person), DiscreteRand(μ2Dist))

        //tell baker to stop service
        baker.idle = false;
      }
      else  {
          enQ(waitQ, person)
      }


      if (clock <= tStop) {
        
        // schedule a new call
        schedule(ArrivalEvent(Person(nCalls+1)), DiscreteRand(λDist))
        
      } // if
      
    } // def occur
  } // case class MakeCall

  /**
   * Enqueues the person and performs some menial tasks
   * @author rbedger@gmail.com
   */
  def enQ(waitQ : Queue[Person],person : Person) {
     waitQ.enqueue(person)
     if (waitQ.length > qMax)
        qMax = waitQ.length
  }

  /**
   * Dequeues the first person in line and performs some menial tasks
   * @return the person dequeued
   * @author rbedger@gmail.com
   */
  def dQ(waitQ : Queue[Person]) = {
    val dQ = waitQ.dequeue()
    dQ.timeWaited = clock - dQ.timeArrived
    if (dQ.timeWaited > waitMax)
      waitMax = dQ.timeWaited
    waitTotal += dQ.timeWaited
    println("\tPerson %d waited %d time units".format(dQ.personNum,dQ.timeWaited))
    dQ
  }

  /** The AbleEndSrv Event
   *  @author mepcotterell@gmail.com
   *  @param person the Person associated with the event
   */
  case class AbleEndSrv (person: Person) extends Event (person) {
    def occur {
      //print a nice message
      println("\t\tAble is dropping Person %d".format(person.personNum))
      // tell the operator to stop service
      able.idle = true

      if (waitQ.length > 0) {
        schedule(AbleEndSrv(dQ(waitQ)),DiscreteRand(μDist))
        able.idle = false;
      }
    } // def occur
  } // case class AbleEndSrv

  /**
   * The BakerEndSrv Event
   */
  case class BakerEndSrv (person: Person) extends Event (person) {
      def occur {
        //print a nice message
        println("\t\tBaker is dropping Person %d".format(person.personNum))
        //tell the operator to stop service
        baker.idle = true;
        if (waitQ.length > 0) {

          schedule(BakerEndSrv(dQ(waitQ)),DiscreteRand(μDist))

          baker.idle = false
        }
      } //def occur
  }//case class BakerEndSrv
  
  // schedule the first event
  //why do we need to cast it?
  schedule(ArrivalEvent(Person(nCalls+1)), tStart + DiscreteRand(λDist).toInt)

  // run the simulation
  simulate
  
  // print out some information
  println()
  println("The total number of calls was %s".format(nCalls))
  println("The max time any caller waited was %s units of time".format(waitMax))
  println("The average waiting time for all callers was %3.2f units of time".format(waitTotal.toDouble/nCalls))
  println("The max # of callers in the wait queue was %d".format(qMax))


  
} // object CallCenter