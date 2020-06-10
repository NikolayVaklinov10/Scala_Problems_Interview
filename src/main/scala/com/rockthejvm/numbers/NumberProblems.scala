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

  // the constituent prime divisors
  def decompose(n: Int): List[Int] = {
    assert(n > 0)


    @tailrec
    def decomposeTailrec(remaining: Int, currentDivisor: Int, accumulator: List[Int]): List[Int] = {
      if (currentDivisor > Math.sqrt(n)) remaining :: accumulator
      else if (remaining % currentDivisor == 0) decomposeTailrec(remaining / currentDivisor, currentDivisor, currentDivisor :: accumulator)
      else decomposeTailrec(remaining, currentDivisor + 1, accumulator)
    }
    decomposeTailrec(n, 2, List())
  }

  def testisPrime() = {
    println(isPrime(2))
    println(isPrime(15))
    println(isPrime(2003))
    println(isPrime(23352352))
    println(isPrime(453453453))
    println(isPrime(1))
  }

  def testDecompose() = {
    println(decompose(2))
    println(decompose(15))
    println(decompose(2003))
    println(decompose(23352352))
    println(decompose(453453453))
    println(decompose(1))
  }

  testisPrime()
  testDecompose()





}

