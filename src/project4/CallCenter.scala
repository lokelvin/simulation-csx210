//package project4
//
//import simulation._
//import simulation.process._
//import simulation.stat._
//import scala.collection.mutable.Queue
//import project4.CallCenter.Director
//
///** An example simulation of a Call Center using event scheduling.
// *  @author mepcotterell@gmail.com
// */
//object CallCenter extends App with ProcessInteractionSimulation {
//
//  class Director() extends Model
//
//  //the director for this simulation
//  implicit val director = new Director
//
//  //the number of servers available
//  val N_SERVERS = 2
//
//  //the number of calls that have come in
//  var N_CALLS = 0
//
//  //the waiting line
//  var waitQ : Queue[SimActor] =  new Queue[SimActor]
//
//  // Assign values to the Simulation variables
//  tStart = 0
//  tStop  = 100
//  λDist = Map[Int,Double](1->0.25,2->0.65,3->0.85,4->1.0)
//  μDist = Map[Int,Double](2->0.30,3->0.58,4->0.83,5->1.0)
//  μ2Dist = Map[Int,Double](3->0.35,4->0.60,5->0.80,6->1.0)
//
//  //the number of callers in the system at time t
//  var L = 0
//  def LW = waitQ.size
//  def LS = if (L > N_SERVERS) N_SERVERS else L
//  
//  def getClock = director.clock
//  
//  // Duration Statistics
//  val WQ_STAT = DurationStatistic(() => (LW, getClock))
//  val WS_STAT = DurationStatistic(() => (LS, getClock))
//  val W_STAT  = DurationStatistic(() => (L, getClock)) 
//  
//  // Stats
//  val LQ_STAT = Statistic[Double]()
//  val LS_STAT = Statistic[Double]()
//  val L_STAT  = Statistic[Double]()
//  
//  //a telephone operator
//  case class Operator(serviceTime : Map[Int,Double]) extends Entity {
//    var idle = true
//  }
//
//  //the two operators in this simulation
//  var able = Operator(μDist)
//  var baker = Operator(μ2Dist)
//
//  //a caller in this simulation
//  case class caller(callerNumber : Int) extends SimActor() {
//    //to be set upon arrival
//    var arrivalTime : Double = 0.0
//
//    //the 'script' for this actor
//    actions.push("leave","usePhone","arrive")
//
//    //who is serving this caller?
//    var myOperator : Operator = null
//
//    /**
//     * Claim an operator
//     */
//    def useOperator(operator : Operator) {
//      operator.idle = false
//      this.myOperator = operator
//      director.schedule(this,DiscreteRand(myOperator.serviceTime).toInt,actions.top)
//    }
//
//    /**
//     * Release my current operator
//     */
//    def releaseOperator() {
//      myOperator.idle = true
//      if (L > N_SERVERS) {
//        val actor : caller = waitQ.dequeue.asInstanceOf[caller]
//        director.schedule(actor,0,actor.actions.top)
//        println("Caller %d dequeued for %s, waited %d".format(actor.callerNumber,actor.actions.top,director.clock-actor.arrivalTime))
//      }
//
//    }
//
//    /**
//     *  Wait for messages from the director to continue through the loop
//     */
//    def act() {
//      var waitOnDirector = true
//         while (true) {
//           
//           WS_STAT.takeSample
//           WQ_STAT.takeSample
//           W_STAT.takeSample
//           
//           LQ_STAT.takeSample(LW)
//           LS_STAT.takeSample(LS)
//           L_STAT.takeSample(L)
//           
//           waitOnDirector = true
//           actions.pop match {
//             //the arrival
//             case "arrive" => {
//               //Set the arrival time
//               arrivalTime = director.clock
//               //increment the number of calls
//               N_CALLS = N_CALLS + 1
//               //schedule another arrival
//               director.schedule(caller(N_CALLS),DiscreteRand(λDist).toInt,"arrive")
//               //Increment the number of people in service
//               L = L + 1
//               //are there servers available?
//               if (L > N_SERVERS) {
//                 waitQ.enqueue(this)
//               }
//               else {
//                 waitOnDirector = false
//               }
//             }
//             //using the telephone
//             case "usePhone" => {
//               assert(able.idle || baker.idle,"Error: Able or Baker should be idle at 'usePhone'")
//                if (able.idle){
//                   useOperator(able)
//                 }
//                 else if (baker.idle){
//                   useOperator(baker)
//                 }
//             }
//             //leaving
//             case "leave" => {
//               //give up the operator
//               releaseOperator()
//               //decrement the number of people in the system
//               L = L - 1
//               //relinquish control
//               director !"resume directing"
//               //kill yourself
//               exit()
//             }
//           }
//           if (waitOnDirector)
//           {
//             //relinquish control
//            director ! "resume directing"
//            receive {
//              //normal resume case
//              case "resume acting" => {
//                println("%10s %10s Person %d is %s".format(director.clock,"[action]",this.callerNumber,this.actions.top))
//                if (director.clock > tStop) {
//                  director.simulating = false
//                  
//                  for (i <- 1 to 1000000000) {}
//      
//			      println
//			      println("STATISTICS")
//			      println("----------------------------------------------------------------------------")
//			      println("| %10s | %10s | %10s | %10s | %20s |".format("STAT", "MIN", "MAX", "SAMPLES", "MEAN"))
//			      println("----------------------------------------------------------------------------")
//			      
//			      println("| %10s | %10s | %10s | %10s | %20s |".format("LQ_LOAD", LQ_STAT.min, LQ_STAT.max, LQ_STAT.n, LQ_STAT.mean))
//			      println("| %10s | %10s | %10s | %10s | %20s |".format("LS_LOAD", LS_STAT.min, LS_STAT.max, LS_STAT.n, LS_STAT.mean))
//			      println("| %10s | %10s | %10s | %10s | %20s |".format("L_LOAD", "n/a", "n/a", "n/a", L_STAT.mean))
//			      
//			      println("----------------------------------------------------------------------------")
//			      
//			      println("| %10s | %10s | %10s | %10s | %20s |".format("WQ", WQ_STAT.min, WQ_STAT.max, WQ_STAT.n, WQ_STAT.mean))
//			      println("| %10s | %10s | %10s | %10s | %20s |".format("WS", WS_STAT.min, WS_STAT.max, WS_STAT.n, WS_STAT.mean))
//			      println("| %10s | %10s | %10s | %10s | %20s |".format("W", "n/a", "n/a", "n/a", W_STAT.mean))
//			      
//			      println("----------------------------------------------------------------------------")
//       
//                }
//              }
//              //quit case
//              case "quit" => {
//                //kill yourself
//                exit()
//              }
//            }
//           }
//          }
//         }
//    }
//
//    //tell the director about your wait queues
//    director.waitQueues.enqueue(waitQ)
//    //set up the first caller
//    val actor = caller(N_CALLS)
//    //schedule the first caller
//    director.schedule(actor,0,actor.actions.top)
//    //start the simulation
//    director.start
//    while (!director.finished) {
//
//    }
//  println("Print some statistics here")
//
//}