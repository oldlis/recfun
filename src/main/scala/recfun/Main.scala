package recfun

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
  def balance(chars: List[Char]): Boolean = ???

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}