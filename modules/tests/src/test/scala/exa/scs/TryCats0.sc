import cats.data.State

import scala.collection.mutable
import scala.collection.mutable.Stack


def op(sym: String): Option[(Int, Int) => Int] =
  sym match {
    case "+" =>  Some(_ + _)
    case "-" =>  Some(_ - _)
    case "*" =>  Some(_ * _)
    case "/" =>  Some(_ / _)
    case _ => None
  }


type CS = State[Stack[Int], Int]

def step(sym : String): CS =  State[Stack[Int], Int] {
  (stk : Stack[Int]) =>
    val stk1 = op(sym).map(op => op(stk.pop(), stk.pop()))
      .map(re => stk.push(re)).getOrElse(stk.push(sym.toInt))
    (stk1, stk1.top)
}

def calc(syms: String) = syms.split(' ').foldLeft {
  State[Stack[Int], Int]( stk => (stk, 0))
} {
  (st, sym) =>  st.flatMap( _ => step(sym))
}


calc("1 2 + 3 *").runA(Stack()).value
val biggerProgram = for {
  _ <- calc("1 2 +")
  _ <- calc("3 4 +")
  ans <- calc("*")
} yield ans
biggerProgram.runA(Stack()).value