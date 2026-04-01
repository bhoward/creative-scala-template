package search

trait ImplicitGraph:
  type Node
  def neighbors(n: Node): List[Node]
  def isGoal(n: Node): Boolean

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