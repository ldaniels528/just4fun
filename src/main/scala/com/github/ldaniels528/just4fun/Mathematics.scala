package com.github.ldaniels528.just4fun

/**
 * Mathematics Algorithms
 * @author lawrence.daniels@gmail.com
 */
object Mathematics {

  def main(args: Array[String]): Unit = {
    {
      val number = 7
      System.out.println(s"isPrime($number) = ${isPrime(number)}")
    }

    {
      val times = 10
      (1 to times) map (n => (n, fibonacci(n))) foreach { case (x, y) =>
        System.out.println(s"fib($x) = $y")
      }
    }

    {
      val number = 2000
      System.out.println(s"sqrt($number) = ${sqrt(number)}")
    }

    {
      val number = 32
      System.out.println(s"isPowerOf2($number) = ${isPowerOf2(number)}")
    }
  }

  def fibonacci(number: Long): Long = {
    number match {
      case n if n <= 1 => 0
      case 2 => 1
      case n => fibonacci(n - 1) + fibonacci(n - 2)
    }
  }

  def isPowerOf2(n: Int): Boolean = (n & (n - 1)) == 0

  def isPrime(number: Long): Boolean = {
    var n = 2L
    while (n <= number / n) {
      if (number % n == 0) return false
      n += 1
    }
    true
  }

  def sqrt(number: Double): Double = {
    val error = 1e-10

    def initialFactor(number: Double) = {
      var factor = 1
      while(factor * factor < number) {
        factor *= 10
      }
      factor
    }

    def sqrtR(number: Double, approx: Double, factor: Double): Double = {
      if (factor <= error) approx
      else {
        var n: Double = approx
        while (n * n < number && factor >= error) n += factor
        if (n * n == number) n else sqrtR(number, n - factor, factor * 0.1)
      }
    }

    sqrtR(number, approx = 0, factor = initialFactor(number))
  }

}
