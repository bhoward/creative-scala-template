package graphs

object Traversal:
  enum StackItem[T]:
    case VisitNode(node: T)
    case VisitEdge(edge: Edge[T])

  /**
    * Perform a depth-first search of a graph and return
    * the dfs tree (as a graph) showing the edges followed.
    * Uses the imperative stack-based approach.
    *
    * @param g the input graph
    * @return the dfs tree
    */
  def depthFirst[T](g: Graph[T]): Graph[T] = {
    import scala.collection.mutable.Stack
    import StackItem.*

    val stack: Stack[StackItem[T]] = Stack.empty

    var visited: List[T] = Nil
    var tree: List[Edge[T]] = Nil

    for start <- g.nodes do
      if !visited.contains(start) then
        stack.push(VisitNode(start))

        while stack.nonEmpty do
          stack.pop() match
            case VisitNode(node) =>
              for edge <- g.outgoing(node) do
                stack.push(VisitEdge(edge))
              visited = node :: visited

            case VisitEdge(edge) =>
              if !visited.contains(edge.to) then
                tree = edge :: tree
                stack.push(VisitNode(edge.to))
        end while
    end for
    
    Graph.Edges(g.nodes, tree)
  }

  @main def traverse() = {
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
