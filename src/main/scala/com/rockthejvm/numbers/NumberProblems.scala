package com.rockthejvm.numbers

import scala.annotation.tailrec


object NumberProblems extends App {

  def isPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeTailrec(currentDivisor: Int): Boolean = {
      if (currentDivisor > Math.sqrt(Math.abs(n))) true
      else n % currentDivisor != 0 && isPrimeTailrec(currentDivisor + 1)
    }
    isPrimeTailrec(2)
  }

}

