package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface :

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c == 0 || c == r then
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =

    def nesting(ch: Char): Int =
      ch match
        case '(' => 1
        case ')' => -1
        case _ => 0

    @tailrec
    def loop(restChars: List[Char], nestingLevel: Int): Boolean =
      restChars.isEmpty && nestingLevel == 0 ||
        (restChars.nonEmpty && nestingLevel >= 0) &&
          loop(restChars.tail, nestingLevel + nesting(restChars.head))

    loop(chars, 0)


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 && coins.nonEmpty then
      1
    else if money < 0 || coins.isEmpty then
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)

