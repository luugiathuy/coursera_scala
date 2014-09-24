package week2

import math.abs

object session3 {
  val tolerate = 0.0001                           //> tolerate  : Double = 1.0E-4
  def isCloseEnough(x: Double, y: Double) =
  	abs((x - y) / x) / x < tolerate           //> isCloseEnough: (x: Double, y: Double)Boolean
  	
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  	def iterate (guess: Double): Double = {
  		val next = f(guess)
  		if (isCloseEnough(next, guess)) next
  		else iterate(next)
  	}
  	iterate(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  
  fixedPoint(x => 1 + x/2)(1)                     //> res0: Double = 1.999755859375
  
  def sqrt(x: Double) = fixedPoint(y => (y + x/y) / 2)(1)
                                                  //> sqrt: (x: Double)Double
  sqrt(2)                                         //> res1: Double = 1.4142135623746899
}