package project6

import java.lang.{ProcessBuilder => JPB}

object Replications extends App 
{
  
  val r = Runtime.getRuntime
  for (i <- 1 to 100) {
    try {
      r.exec("scala -cp bin project6.subway")
    } catch {
      case e: Exception => println(e) 
    }
  }
}
