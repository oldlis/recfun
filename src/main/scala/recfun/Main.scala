package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println(pascal(350, 400))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def isIncorrectIndex(c: Int, r: Int): Boolean = (c < 0 || r < 0 || c > r)

    //C(r,c) = r!/(c!*(r-c)!)
    def getValue(c: Int, r: Int): Int = {
      val dividend = factorial(r)
      val divisor = factorial(c) * factorial(r - c)

      //if value is out of Int value interval
      if (divisor == 0) throw new TooBigPascalValueException("Element(" + c + "," + r + ") can't be calculated because of too big value!")

      dividend / divisor
    }

    if (isIncorrectIndex(c, r)) throw new NoSuchElementException("Element(" + c + "," + r + ") does not exist!")
    getValue(c, r)
  }

  def factorial(n: Int): Int = {
    def rfactorial(accum: Int, n: Int): Int = if (n == 0) accum else rfactorial(n * accum, n - 1)
    rfactorial(1, n)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def rbalance(openedBrackets: Int, chars: List[Char]): Boolean = {
      if (openedBrackets < 0) return false
      if (chars.isEmpty) {
        return if (openedBrackets == 0) true else false
      }

      chars.head match {
        case '(' => rbalance(openedBrackets + 1, chars.tail)
        case ')' => rbalance(openedBrackets - 1, chars.tail)
        case _ => rbalance(openedBrackets, chars.tail)
      }
    }

    rbalance(0, chars);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sortWith(_ < _);

    if (money == 0) 1
    else if (money < 0 || sortedCoins.isEmpty) 0
    else {
      countChange(money, sortedCoins.tail) + countChange(money - sortedCoins.head, sortedCoins)
    }
  }
}