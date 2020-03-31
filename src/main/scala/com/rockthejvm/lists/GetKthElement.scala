package com.rockthejvm.lists

//import scala.annotation.tailrec
//
//sealed abstract class RList[+T] {
//  def head: T
//  def tail: RList[T]
//  def isEmpty: Boolean
//
//  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
//
//  def apply(index: Int): T
//}
//
//case object RNil extends RList[Nothing] {
//  override def head: Nothing = throw new NoSuchElementException
//  override def tail: RList[Nothing] = throw new NoSuchElementException
//  override def isEmpty: Boolean = true
//
//  override def toString: String = "[]"
//
//  override def apply(index: Int): Nothing = throw new NoSuchElementException
//}
//
//case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
//  override def isEmpty: Boolean = false
//
//  override def toString: String = {
//    @tailrec
//    def toStringTailrec(remaining: RList[T], result: String): String = {
//      if (remaining.isEmpty) result
//      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
//      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
//    }
//
//    "[" + toStringTailrec(this, "") + "]"
//  }
//
//  override def apply(index: Int): T = {
//    def applyTailrec(remaining: RList[T], currentInt: Int): T = {
//      if (currentInt == index) remaining.head
//      else applyTailrec(remaining.tail , currentInt + 1)
//    }
//    applyTailrec(this, 0)
//  }
//
//}
//
//object GetKthElement extends App{
//
//  val aSmallList = 1 :: 2 :: 3 :: RNil //
//
//  println(aSmallList.apply(0))
////  println(aSmallList.apply(2))
////  println(aSmallList.apply(90))
//
//
//}
