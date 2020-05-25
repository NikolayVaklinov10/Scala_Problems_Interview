package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  // reverse the list
  def reverse: RList[T]

  // run-length encoding
  def rle: RList[(T,Int)]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  /**
   * Medium problem
   */
  // run-length encoding
  override def rle: RList[(Nothing, Int)] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T]{
  override def isEmpty: Boolean = false

  override def reverse: RList[T] = {
    def reverseTailrec(remainingList: RList[T], result: RList[T]): RList[T] = {
      if (remainingList.isEmpty) result
      else reverseTailrec(remainingList.tail, remainingList.head :: result)
    }

    reverseTailrec(this, RNil)
  }

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  override def rle: RList[(T, Int)] = {
    @tailrec
    def rleTailrec(remaining: RList[T],currentTuple: (T, Int) ,accumulator: RList[(T, Int)]): RList[(T, Int)] = {
      if (remaining.isEmpty && currentTuple._2 == 0) accumulator
      else if (remaining.isEmpty) currentTuple :: accumulator
      else if (remaining.head == currentTuple._1) rleTailrec(remaining.tail, currentTuple.copy(_2 = currentTuple._2 + 1), accumulator)
      else rleTailrec(remaining.tail, (remaining.head, 1), currentTuple :: accumulator)
    }
    rleTailrec(this.tail, (this.head, 1), RNil).reverse
  }
}


object RLEproblem extends App {

  /**
   * Medium difficulty problem
   *
   */
  // run length encoding
  println((1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 5 :: 5 :: 5 :: RNil).rle)



}
