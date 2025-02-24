package graphs

import cats.instances.double

object Traversal2:
  /**
    * Perform a depth-first search of a graph and return
    * the dfs tree (as a graph) showing the edges followed.
    * Uses the imperative stack-based approach.
    * 
    * This version only puts edges on the stack.
    *
    * @param g the input graph
    * @return the dfs tree
    */
  def depthFirst[T](g: Graph[T]): Graph[T] = {
    import scala.collection.mutable.Stack

    val stack: Stack[Edge[T]] = Stack.empty

    var visited: List[T] = Nil
    var tree: List[Edge[T]] = Nil

    def visitNode(node: T): Unit = {
      visited = node :: visited
      for edge <- g.outgoing(node) do
        stack.push(edge)
    }

    for start <- g.nodes do
      if !visited.contains(start) then
        visitNode(start)

      while stack.nonEmpty do
        val edge = stack.pop()
        if !visited.contains(edge.to) then
          tree = edge :: tree
          visitNode(edge.to)
      end while
    end for
    
    Graph.Edges(g.nodes, tree)
  }

  @main def traverse2(): Unit = {
    import doodle.core.*
    import doodle.image.*
    import doodle.image.syntax.all.*
    import doodle.image.syntax.core.*
    import doodle.core.font.*
    import doodle.java2d.*
    import cats.effect.unsafe.implicits.global

    val demo = Graph.Pairs(
      List("A", "B", "C", "D", "E", "F"),
      List("A"->"B", "A"->"C", "B"->"C", "B"->"D",
        "D"->"A", "E"->"C", "E"->"F", "F"->"D", "F"->"F")
    )

    val dfsTree = depthFirst(demo)

    showGraph(demo, new CircleNodeLayout(demo.nodes, 50), new DefaultNodeImage(20)).draw()
    showGraph(dfsTree, new CircleNodeLayout(dfsTree.nodes, 50), new DefaultNodeImage(20)).draw()
  }
