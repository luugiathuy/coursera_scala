package week2

object session2 {
	def sum(f: Int => Int) (a: Int, b: Int): Int =
		if (a > b) 0 else f(a) + sum(f)(a + 1, b)
                                                  //> sum: (f: Int => Int)(a: Int, b: Int)Int
		
	sum(x => x * x)(3, 5)                     //> res0: Int = 50
}