package search

/**
 * An ImplicitGraph represents a graph-like structure on which we
 * can search for goal nodes. The graph is not stored as a collection
 * of explicit nodes in a data structure; rather, it is traversed one
 * node at a time. Each node can be checked to see if it is a goal,
 * and a list of neighboring nodes can be obtained. Note that this is
 * not necessarily a list of all neighboring nodes, since we assume
 * that some form of pruning has been applied to guide the traversal.
 * In particular, since it may not be feasible to maintain a collection
 * of visited nodes, we expect that the neighbors will only include
 * unvisited nodes.
 */
trait ImplicitGraph:
  type Node
  def neighbors(n: Node): List[Node]
  def isGoal(n: Node): Boolean

/**
 * Depth-first search on an ImplicitGraph. Does no tracing of visited
 * nodes, so it relies on an appropriate pruning strategy when listing
 * neighbors of nodes.
 */
object DFS:
  def findOne(g: ImplicitGraph, start: g.Node): Option[g.Node] = {
    import scala.util.boundary, boundary.break // enable break from loop

    if g.isGoal(start)
    then Some(start)
    else
      boundary:
        for n <- g.neighbors(start) do {
          val result = findOne(g, n)
          if result.isDefined
          then break(result)
        }
        None
  }
  
  def findAll(g: ImplicitGraph, start: g.Node): List[g.Node] = {
    type Node = g.Node

    def aux(n: Node, accum: List[Node]): List[Node] = {
      var result = if g.isGoal(n) then n :: accum else accum
      for n2 <- g.neighbors(n) do
        result = aux(n2, result)
      result
    }

    aux(start, Nil)
  }

/**
 * Example of searching for solutions to the 8 queens problem:
 * put 8 queens on an 8x8 chessboard so that none of them
 * attack each other (horizontally, vertically, or diagonally).
 */
object EightQueens extends ImplicitGraph:
  type Node = List[(Int, Int)]

  def neighbors(n: Node): List[Node] = {
    // Prune the potential list to only include unattacked queens
    // on the next empty row; assume that the rows have been
    // filled in starting at 1, so n is of the form
    // List((k, qk), ..., (2, q2), (1, q1))
    val row = n.length + 1
    def isSafe(col: Int): Boolean = {
      n.forall {
        case (r, c) =>
          c != col &&                   // vertical attack
          row - r != math.abs(col - c)  // diagonal attack
      }
    }
    val cols = (1 to 8).filter(isSafe).toList
    cols.map(col => (row, col) :: n)
  }

  def isGoal(n: Node): Boolean = n.length == 8

  @main
  def solve(): Unit = {
    println("First Solution:")
    println(DFS.findOne(EightQueens, Nil))
    println("Number of Solutions:")
    println(DFS.findAll(EightQueens, Nil).length)
  }