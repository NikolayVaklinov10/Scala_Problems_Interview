package com.rockthejvm.lists

import scala.annotation.tailrec
import scala.util.Random

sealed abstract class RList[+T] {
  /**
   * Standard functions
   */
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  /**
   * Easy problems
   */
  // get element at a given index
  def apply(index: Int): Int

  // the size of the list
  def length: Int

  // reverse the list
  def reverse: RList[T]

  // concatenate another list to this one
  def ++[S >: T](anotherList: RList[S]): RList[S]

  // remove an element at a given index, return a NEW list
  def removeAt(index: Int): RList[T]

  // the big 3
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  // random sample
  def sample(k: Int): RList[T]

  // merge sort
  def mergeSort[S >: T](ordering: Ordering[S]): RList[S]

}

case object RNil extends RList[Nothing]{
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  /**
   * Easy problems
   */
  override def apply(index: Int): Nothing = throw new NoSuchElementException

  // the size of the list
  override def length: Int = 0

  // reverse the empty list
  override def reverse: RList[Nothing] = RNil

  // append another list
  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  // remove an element
  override def removeAt(index: Int): RList[Nothing] = RNil

  // the big 3
  override def map[S](f: Nothing => S): RList[S] = RNil
  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil
  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  // random samples
  override def sample(k: Int): RList[Nothing] = RNil

  // merge sort
  override def mergeSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T]{
  override def isEmpty: Boolean = false
  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.isEmpty}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  /**
   * Easy problems
   */
  // get element at a given index
  override def apply(index: Int): Int = {
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

  // the size of the list
  override def length: Int = {
    /*
    [1,2,3,4,5].lengthTailrec([1,2,3,4,5], 0)
    = lengthTailrec([2,3,4,5], 1)
    = lengthTailrec([3,4,5], 2)
    = lengthTailrec([4,5], 3)
      = lengthTailrec([5], 4)
      = lengthTailrec([], 5)
      = 5
      Complexity: O(N)
     */
    @tailrec
    def lengthTailrec(remainingList: RList[T], accumulator: Int): Int = {
      if (remainingList.isEmpty) accumulator
      else lengthTailrec(remainingList.tail, accumulator + 1)
    }

    lengthTailrec(this, 0)
  }

  // reverse this list into a new list
  override def reverse: RList[T] = {
    /*
    [1,2,3,4].reverse = reverseTailrec([1,2,3,4], RNil)
      = reverseTailrec([2,3,4], [1])
      = reverseTailrec([3,4], [2,1])
      = reverseTailrec([4], [3,2,1])
      = reverseTailrec([], [4,3,2,1])
      = [4,3,2,1]
      Complexity: O(N)
     */
    def reverseTailrec(remainingList: RList[T], result: RList[T]): RList[T] = {
      if (remainingList.isEmpty) result
      else reverseTailrec(remainingList.tail, remainingList.head :: result)
    }

    reverseTailrec(this, RNil)
  }

  // append another list
  def ++[S >: T](anotherList: RList[S]): RList[S] = {
    /*
      [1,2,3] ++ [4,5] = concatTailrec([4,5], [3,2,1])
      = concatTailrec([5], [4,3,2,1])
      = concatTailrec([], [5,4,3,2,1])
      = [5,4,3,2,1]
      Complexity: O(M + N)
      length of this list = N
      length of the other list = M
     */
    @tailrec
    def concatTailrec(remainingList: RList[S], acc: RList[S]): RList[S] = {
      if (remainingList.isEmpty) acc
      else concatTailrec(remainingList.tail, remainingList.head :: acc)
    }
    concatTailrec(anotherList, this.reverse).reverse
  }

  // merge sort
  override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = {
    /*
    merge([1,3], [2,4,5,6,7], []) =
      merge([3], [2,4,5,6,7], [1]) =
      merge([3], [4,5,6,7], [2,1]) =
      merge([], [4,5,6,7], [3,2,1]) =
      [1,2,3] ++ [4,5,6,7] =
      [1,2,3,4,5,6,7]
     */
    @tailrec
    def merge(listA: RList[S], listB: RList[S], accumulator: RList[S]): RList[S] = {
      if (listA.isEmpty) accumulator.reverse ++ listB
      else if (listB.isEmpty) accumulator.reverse ++ listA
      else if (ordering.lteq(listA.head, listB.head)) merge(listA.tail, listB, listA.head :: accumulator)
      else merge(listA, listB.tail, listB.head :: accumulator)
    }
    /*
      [3,1,2,5,4] => [[3],[1],[2],[5],[4]]
      mst([[3],[1],[2],[5],[4]], []) =
      = mst([[2],[5],[4]], [[1,3]])
      = mst([[4]], [[2,5], [1,3]])
      = mst([], [[4], [2,5], [1,3]]) =
      = mst([[4], [2,5], [1,3]], [])
      = mst([[1,3]], [[2,4,5]])
      = mst([], [[1,3], [2,4,5]])
      = mst([[1,3], [2,4,5]], [])
      = mst([], [[1,2,3,4,5]])
      = [1,2,3,4,5]
      Complexity: O(n * log(n))
      complexity(n) = 2 * complexity(n/2) + n
     */





  }






}


object MergeSort extends App {

}
