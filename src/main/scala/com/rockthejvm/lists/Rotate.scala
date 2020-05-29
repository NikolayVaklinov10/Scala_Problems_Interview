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

  // concatenate another list in this one
  def ++[S >: T](anotherList: RList[S]): RList[S]

  // rotations by a number of positions to the left
  def rotate(k: Int): RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def ::[S >: Nothing](elem: S): RList[S] = ???

  override def toString: String = "[]"

  /**
   * Medium problem
   */
  // run-length encoding
  override def rle: RList[(Nothing, Int)] = RNil

  override def reverse: RList[Nothing] = RNil

  // append another list
  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  // rotations by a number of positions to the left
  override def rotate(k: Int): RList[Nothing] = RNil
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

  // reverse this list into a new list
  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    def concatTailrec(remainingList: RList[S], acc:RList[S]): RList[S] = {
      if (remainingList.isEmpty) acc
      else concatTailrec(remainingList.tail, remainingList.head :: acc)
    }
    concatTailrec(anotherList, this.reverse).reverse
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

  // rotations by a number of positions to the left
  override def rotate(k: Int): RList[T] = {

    @tailrec
    def rotateTailrec(remaining:RList[T], rotationLeft: Int,buffer: RList[T]): RList[T] = {
      if(remaining.isEmpty && rotationLeft == 0) this // [1,2,3].rotate(3)
      else if (remaining.isEmpty) rotateTailrec(this, rotationLeft, RNil)
      else if (rotationLeft == 0) remaining ++ buffer.reverse
      else rotateTailrec(remaining.tail, rotationLeft - 1, remaining.head :: buffer)
    }
    rotateTailrec(this, k, RNil)
  }

}


object Rotate extends App {

  val oneToTen = RList.from(1 to 10)

  def testRotate()={
    for {
      i <- 1 to 20
    } println(oneToTen.rotate(i))
  }

}
