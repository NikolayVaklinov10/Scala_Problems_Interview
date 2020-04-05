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
//  def length: Int
//}
//
//case object RNil extends RList[Nothing] {
//  override def head: Nothing = throw new NoSuchElementException
//  override def tail: RList[Nothing] = throw new NoSuchElementException
//  override def isEmpty: Boolean = true
//
//  override def toString: String = "[]"
//
//  override def length: Int = 0
//}
//
//case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
//  override def isEmpty: Boolean = false
//
//  override def length: Int = {
//    @tailrec
//    def lengthTailrec(remaining: RList[T], accumulator: Int): Int = {
//      if (remaining.isEmpty) accumulator
//      else lengthTailrec(remaining.tail, accumulator + 1)
//    }
//    lengthTailrec(this,0)
//  }
//
//
//}
//
//object Length extends App {
//
//  val aSmallList = 1 :: 2 :: 3 :: RNil
//
//  // test length method
//  println(aSmallList.length)
//
//
//}


