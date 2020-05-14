package com.rockthejvm.graphs

import scala.annotation.tailrec


object GraphProblems extends App {

  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob", "Mary"),
    "Mary" -> Set("Bob", "Charlie")
  )

  /**
   * Easy problems
   */

  // number of nodes this node `node` is associated (adjacent) to
  def outDegree[T](graph: Graph[T], node: T): Int = {
    if (graph.contains(node)) graph(node).size
    else 0
  }

  // number of nodes connected to `node`
  def inDegree[T](graph: Graph[T], node: T): Int = {
    graph.values.count(_.contains(node))
  }

  /**
   * Medium difficulty problems
   */

    def isPath[T](graph: Graph[T], start: T, end: T):Boolean = {
      @tailrec
      def isPathTailrec(remaining: List[T], consideredNodes: Set[T]): Boolean = {
        if (remaining.isEmpty) false
        else {
          val node = remaining.head
          if (node == end) true
          else if (consideredNodes.contains(node)) isPathTailrec(remaining.tail, consideredNodes)
          else isPathTailrec(remaining.tail ++ graph(node), consideredNodes + node)
        }
      }

      isPathTailrec(List(start), Set())
    }

    def findPath[T](graph: Graph[T], start: T, end: T): List[T] = {

      @tailrec
      def findPathTailrec(remaining: List[(T, List[T])], consideredNodes: Set[T]): List[T] = {
        if (remaining.isEmpty) List()
        else {
          val (node, currentPath) = remaining.head
          if (node == end) currentPath.reverse
          else if (consideredNodes.contains(node)) findPathTailrec(remaining.tail, consideredNodes)
          else {
            val neighbours = graph(node)
            val tuples = neighbours.map(n => (n, n :: currentPath))
            findPathTailrec(remaining.tail ++ tuples, consideredNodes + node)
          }
        }
      }
      findPathTailrec(List((start, List(start))), Set())
    }

  def findCycle[T](graph: Graph[T], node: T): List[T] = findPath(graph, node, node)


  // testing the degrees two function code
  def testDegrees(): Unit = {
    println(outDegree(socialNetwork, "Alice")) // 3
    println(inDegree(socialNetwork, "David")) // 2
  }

  // testing the findPath code
  def testFindPath(): Unit = {
    println(findPath(socialNetwork, "Charlie", "Mary"))
    println(findPath(socialNetwork, "Alice", "Mary"))
    println(findPath(socialNetwork, "Bob", "Mary"))
  }

  // testing the cycle function
  def testCycles(): Unit = {
    println(findCycle(socialNetwork, "Alice")) // List
  }


}
