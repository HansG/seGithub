package exa

import cats.Eval

object CatsTry {


  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }


  def factorial(n: BigInt): Eval[BigInt] =
    if(n == 1) {
      Eval.now(n)
    } else {
      factorial(n - 1).map(_ * n)
    }




}
