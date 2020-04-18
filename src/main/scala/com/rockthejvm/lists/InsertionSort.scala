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

  // sorting the list in the order defined by the Ordering object
  def sorted[S >: T](ordering: Ordering[S]): RList[S]
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

  // sorting the list in the order defined by the Ordering object
  override def sorted[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil

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

  /**
   * Hard problems
   */
  override def sorted[S >: T](ordering: Ordering[S]): RList[S] = {

    /*
     insertSorted(4, [], [1,2,3,5]) =
     insertSorted(4, [1], [2,3,5]) =
     insertSorted(4, [2,1], [3,5]) =
     insertSorted(4, [3,2,1], [5]) =
     [3,2,1].reverse + (4 :: [5]) =
     [1,2,3,4,5]
     Complexity: O(N)
    */
    @tailrec
    def insertSorted(element: T, before: RList[S], after: RList[S]): RList[S] = {
      if (after.isEmpty || ordering.lteq(element, after.head)) before.reverse ++ (element :: after)
      else insertSorted(element, after.head :: before, after.tail)
    }

    /*
      [3,1,4,2,5].sorted = insertSortTailrec([3,1,4,2,5], []) =
        = insertSortTailrec([1,4,2,5], [3])
        = insertSortTailrec([4,2,5], [1,3])
        = insertSortTailrec([2,5], [1,3,4])
        = insertSortTailrec([5], [1,2,3,4])
        = insertSortTailrec([], [1,2,3,4,5])
        = [1,2,3,4,5]
        Complexity: O(N^2)
     */
    @tailrec
    def insertSortTailrec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc
      else insertSortTailrec(remaining.tail, insertSorted(remaining.head, RNil, acc))
    }

    insertSortTailrec(this, RNil)
  }

}


object InsertionSort extends App {

  val aSmallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  val aLargeList = RList.from(1 to 10000)
  val oneToTen = RList.from(1 to 10)

  def testHardFunctions() = {
    val anUnorderedList = 3 :: 1 :: 2 :: 4 :: 5 :: RNil
    val ordering = Ordering.fromLessThan[Int](_ < _)



    // insertion sort
    println(anUnorderedList.sorted(ordering))
    println(aLargeList.sample(10).sorted(ordering))

  }

}
