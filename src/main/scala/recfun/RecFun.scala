package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def count(flag: Int, list: List[Char]): Boolean = {
      if (flag < 0) false
      else if (list.isEmpty && flag == 0) true
      else if (list.isEmpty && flag > 0) false
      else list.head match {
        case '(' => count(flag+1, list.tail)
        case ')' => count(flag-1, list.tail)
        case _ => count(flag, list.tail)
      }
    }
    count(0, chars)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (m, l) if m == 0 || l.isEmpty => 0
      case (m, l) =>
        val newMoney = m - l.head
        if (newMoney == 0) 1 + countChange(m, l.tail)
        else if (newMoney < 0) countChange(m, l.tail)
        else countChange(newMoney, l) + countChange(m, l.tail)
    }
  }

//import RecFun._
//@main def run(): Unit = println(countChange(4, List(1,2)))