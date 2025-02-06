package graphs

object Traversal:
  enum StackItem[T]:
    case Visit(node: T)
    case Edge(node1: T, node2: T)

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
    var tree: List[(T, T)] = Nil

    for start <- g.nodes do
      if !visited.contains(start) then
        stack.push(Visit(start))

        while stack.nonEmpty do
          stack.pop() match
            case Visit(node) =>
              for node2 <- g.neighbors(node) do
                stack.push(Edge(node, node2))
              visited = node :: visited

            case Edge(node1, node2) =>
              if !visited.contains(node2) then
                tree = (node1, node2) :: tree
                stack.push(Visit(node2))
        end while
    end for
    
    Graph.Pairs(g.nodes, tree)
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
