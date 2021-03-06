package week1

object session {
  1 + 3                                           //> res0: Int(4) = 4
  def abs(x: Double): Double = if (x < 0) -x else x
                                                  //> abs: (x: Double)Double
  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess / x - 1) < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  sqrt(2.0)                                       //> res1: Double = 1.4142156862745097
  sqrt(4.0)                                       //> res2: Double = 2.000609756097561
  sqrt(1e-6)                                      //> res3: Double = 0.0010000001533016628
  sqrt(1e60)                                      //> res4: Double = 1.0000788456669446E30
  sqrt(0.001)                                     //> res5: Double = 0.03162278245070105
  sqrt(0.1e-20)                                   //> res6: Double = 3.1633394544890125E-11
  sqrt(1.0e20)                                    //> res7: Double = 1.0000021484861237E10
  sqrt(1.0e50)                                    //> res8: Double = 1.0000003807575104E25

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)              //> gcd: (a: Int, b: Int)Int
    
  gcd(14, 21)                                     //> res9: Int = 7

	def factorial(n: Int): Int = {
		def loop(acc: Int, n: Int): Int =
			if (n == 0) acc else loop(acc * n, n - 1)
		loop(1, n)
	}                                         //> factorial: (n: Int)Int
	
	factorial(4)                              //> res10: Int = 24
}