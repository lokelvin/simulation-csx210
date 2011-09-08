package project2

import simulation._
import simulation.event._
import simulation.stat._
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
  var waitQueue : Queue[Person] = new Queue[Person]

  //the highest waiting time
  var waitMax = 0

  //the total time waited
  var waitTotal = 0

  //the max length of the waitQ
  var qMax = 0

  //the number of callers waiting
  var LQ = 0

  //the number of callers being serviced
  var L = 0

  //Able's total busy time
  var BA = 0

  //Baker's total busy time
  var BB = 0

  
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
        if (waitQueue.length > 0){
          enQ(waitQueue,person)
        }
        // or schedule a hangup
        else {
          val delay =  DiscreteRand(μDist)
          schedule(AbleEndSrv(person), delay)
          L = L + 1
          BA = BA + delay.toInt
        }
        
        // tell able to stop service
        able.idle = false
      
      } // if
      else if (baker.idle) {
        if (waitQueue.length > 0) {
          enQ(waitQueue,person)
        }
        // schedule a hangup
        else   {
          val delay =  DiscreteRand(μ2Dist)
          schedule(BakerEndSrv(person), delay)
          L = L + 1
          BB = BB + delay.toInt
        }

        //tell baker to stop service
        baker.idle = false;
      }
      else  {
          enQ(waitQueue, person)

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
     LQ = waitQueue.length
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
    LQ = LQ - 1
    //println("\tPerson %d waited %d time units".format(dQ.personNum,dQ.timeWaited))
    dQ
  }

  /** The AbleEndSrv Event
   *  @author mepcotterell@gmail.com
   *  @param person the Person associated with the event
   */
  case class AbleEndSrv (person: Person) extends Event (person) {
    def occur {
      //print a nice message
      //println("\t\tAble is dropping Person %d".format(person.personNum))
      // tell the operator to stop service
      able.idle = true
      L = L - 1
      if (waitQueue.length > 0) {
        val delay = DiscreteRand(μDist)
        schedule(AbleEndSrv(dQ(waitQueue)),delay)
        BA = BA + delay.toInt
        L = L + 1
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
        //println("\t\tBaker is dropping Person %d".format(person.personNum))
        //tell the operator to stop service
        baker.idle = true;
        L = L - 1
        if (waitQueue.length > 0) {
          val delay = DiscreteRand(μDist)
          schedule(BakerEndSrv(dQ(waitQueue)),delay)
          BB = BB + delay.toInt
          L = L + 1
          baker.idle = false
        }
      } //def occur
  }//case class BakerEndSrv

  implicit def trace () = "LQ = %s, L = %s, BA = %s, BB = %s".format(LQ, L, BA, BB)

  // schedule the first event
  schedule(ArrivalEvent(Person(nCalls+1)), tStart + DiscreteRand(λDist).toInt)

  // run the simulation
  simulate
  
  // print out some information
  println()
  println("The total number of calls was %s".format(nCalls))
  println("The max time any caller waited was %s units of time".format(waitMax))
  println()
  println("Able's utilization was %3.2f".format(BA.toDouble/clock))
  println("Baker's utilization was %3.2f".format(BB.toDouble/clock))
  println()
  println("The average waiting time for all callers was %3.2f units of time".format(waitTotal.toDouble/nCalls))
  println("The max # of callers in the wait queue was %d".format(qMax))


  
} // object CallCenter