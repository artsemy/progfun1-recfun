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
    def loop(restChars: List[Char], nestingLevel: Int): Boolean =
      (restChars, nestingLevel) match
        case (Nil, nl) => nl == 0
        case (_, -1) => false // nestingLevel != -1 // impossible
        case ('(' :: tail, nl) => loop(tail, nl + 1)
        case (')' :: tail, nl) => loop(tail, nl - 1)
        case (ch :: tail, nl) => loop(tail, nl)

    loop(chars, 0)


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

