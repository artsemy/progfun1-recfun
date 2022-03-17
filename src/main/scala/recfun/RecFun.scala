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
    @tailrec
    def loop(nestingLevel: Int, restChars: List[Char]): Boolean =
      if nestingLevel < 0 then
        false
      else if restChars.isEmpty && nestingLevel == 0 then
        true
      else if restChars.isEmpty && nestingLevel > 0 then
        false
      else restChars.head match
        case '(' => loop(nestingLevel + 1, restChars.tail)
        case ')' => loop(nestingLevel - 1, restChars.tail)
        case _ => loop(nestingLevel, restChars.tail)

    loop(0, chars)


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 || coins.isEmpty then
      0
    else
      val newMoney = money - coins.head
      if newMoney == 0 then
        1 + countChange(money, coins.tail)
      else if newMoney < 0 then
        countChange(money, coins.tail)
      else
        countChange(newMoney, coins) + countChange(money, coins.tail)

