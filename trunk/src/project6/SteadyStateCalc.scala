package project6

import math.pow

object SteadyStateCalc extends App {

  def sum (r: Range, f: Int => Double) = (r.map(n => f(n))).reduceLeft(_+_)
  def fac (n: Int): Double = if (n > 1) (1 to n).reduceLeft(_+_) else 1
  
  // STEADY-STATE PARAMETERS FOR M/M/c QUEUE
  
  val λ = 2.0
  val μ = 3.0 / 2.0
  
  def ρ (c: Int) = λ / (c * μ)
  
  def P0 (c: Int) = 1.0 / ( sum (0 to c-1, n => pow(c * ρ(c), n) / fac(n)) + (pow(c * ρ(c), c) * (1.0 / fac(c) * (1.0 / (1 - ρ(c))))) )
  
  println(P0(2))
  
}