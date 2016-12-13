package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || r == 0 || (c == r))
        1
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      lazy val OpeningParenthesis = '('
      lazy val ClosingParenthesis = ')'

      def balanceCorr (char: Char) : Int = {
        if (char == OpeningParenthesis) 1
        else if (char == ClosingParenthesis)
          -1
        else
          0
      }

      @tailrec
      def balancePoll(chars: List[Char], accBalance: Int) : Boolean = {
        if (chars.isEmpty)
          accBalance == 0
        else {
          (accBalance >= 0) && balancePoll(chars.tail, accBalance + balanceCorr(chars.head))
        }
      }

      balancePoll(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money<=0 || coins.isEmpty)
        0
      else if (money == coins.head)
        1 + countChange(money, coins.tail)
      else
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
