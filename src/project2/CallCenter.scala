package project2

import simulation._

/** An example simulation of a Call Center using event scheduling.
 *  @author mepcotterell@gmail.com
 */
object CallCenter extends App with EventSchedulingSimulation {

  // Assign values to the Simulation variables
  tStart = 0
  tStop  = 100
  //λ      = 9.0
  λDist = Map[Int,Double](1->0.25,2->0.65,3->0.85,4->1.0)
  //μ      = 2.0
  μDist = Map[Int,Double](2->0.30,3->0.58,4->0.83,5->1.0)
  μ2Dist = Map[Int,Double](3->0.35,4->0.60,5->0.80,6->1.0)
  
  // the total number of calls
  var nCalls: Int = 0
  
  /** Represents a person in the simulation
   *  @author mepcotterell@gmail.com
   */
  case class Person() extends Entity
  
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
      
      if (able.idle) {
      
        // schedule a hangup
        schedule(AbleEndSrv(person), DiscreteRand(μDist))
        
        // tell able to stop service
        able.idle = false
      
      } // if
      else if (baker.idle) {
        //schedule a hangup
        schedule(BakerEndSrv(person), DiscreteRand(μ2Dist))
        //tell baker to stop service
        baker.idle = false;
      }
      
      if (clock <= tStop) {
        
        // schedule a new call
        schedule(ArrivalEvent(Person()), DiscreteRand(λDist))
        
      } // if
      
    } // def occur
  } // case class MakeCall
  
  /** The AbleEndSrv Event
   *  @author mepcotterell@gmail.com
   *  @param person the Person associated with the event
   */
  case class AbleEndSrv (person: Person) extends Event (person) {
    def occur {
      
      // tell the operator to stop service
      able.idle = true
      
    } // def occur
  } // case class AbleEndSrv

  /**
   * The BakerEndSrv Event
   */
  case class BakerEndSrv (person: Person) extends Event (person) {
      def occur {
        //tell the operator to stop service
        baker.idle = true;
      } //def occur
  }//case class BakerEndSrv
  
  // schedule the first event
  //why do we need to cast it?
  schedule(ArrivalEvent(Person()), tStart + DiscreteRand(λDist).toInt)

  // run the simulation
  simulate
  
  // print out some information
  println()
  println("The total number of calls was %s".format(nCalls))

  
} // object CallCenter