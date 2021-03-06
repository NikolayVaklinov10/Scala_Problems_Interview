package com.rockthejvm.lists

import scala.annotation.tailrec
import scala.util.Random

sealed abstract class RList[+T]{
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  // get element at a given index
  def apply(index: Int): T

  // the size of the list
  def length: Int

  // random sample
  def sample(k: Int): RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  // get element at a given index
  override def apply(index: Int): Nothing = throw new NoSuchElementException

  // the size of the list
  override def length: Int = 0

  // random samples
  override def sample(k: Int): RList[Nothing] = RNil

}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {

  override def apply(index: Int): T = {
    /*
      [1,2,3,4,5].apply(2) = applyTailrec([1,2,3,4,5], 0)
      = applyTailrec([2,3,4,5], 1)
      = applyTailrec([3,4,5], 2)
      = 3
      Complexity of this algorithm?
      O(min(N, index))
     */
    @tailrec
    def applyTailrec(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index) remaining.head
      else applyTailrec(remaining.tail, currentIndex + 1)
    }

    if (index < 0) throw new NoSuchElementException
    else applyTailrec(this, 0)
  }

  override def isEmpty: Boolean = false


  // random samples
  override def sample(k: Int): RList[T] = {
    val random = new Random(System.currentTimeMillis())
    val maxIndex = this.length

    /*
  [1,2,3,4,5].sample(3) = sampleTailrec(3, [])
  = sampleTailrec(2, [2])
  = sampleTailrec(1, [4,2])
  = sampleTailrec(0, [4,4,2])
  = [4,4,2]
  Complexity: O(N * K)
 */
    @tailrec
    def sampleTailrec(nRemaining: Int, accumulator: RList[T]): RList[T] = {
      if (nRemaining == 0) accumulator
      else {
        val index = random.nextInt(maxIndex)
        val newNumber = this(index)
        sampleTailrec(nRemaining - 1, newNumber :: accumulator)
      }
    }

    /*
  Complexity: O(N * K)
 */
    def sampleElegant: RList[T] =
      RList.from((1 to k).map(_ => random.nextInt(maxIndex)).map(index => this (index)))

    if (k < 0) RNil
    else sampleElegant
  }

  // the length code
  override def length: Int = {
    @tailrec
    def lengthTailrec(remaining: RList[T], accumulator: Int): Int = {
      if (remaining.isEmpty) accumulator
      else lengthTailrec(remaining.tail, accumulator + 1)
    }
    lengthTailrec(this,0)
  }
}


object RandomSample extends App {

  val aSmallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  val aLargeList = RList.from(1 to 10000)
  val oneToTen = RList.from(1 to 10)

  // random samples
  println(aLargeList.sample(10))
}
