package graphs

trait Edge[T]:
  def from: T
  def to: T
  def weight: Double = 1

object Edge:
  case class Unweighted[T](from: T, to: T) extends Edge[T]
  case class Weighted[T](from: T, to: T, _weight: Double) extends Edge[T]:
    override def weight: Double = _weight

trait Graph[T]:
  def nodes: List[T]
  def edges: List[Edge[T]]
  def pairs: List[(T, T)] = edges.map(e => (e.from, e.to))
  def outgoing(node: T): List[Edge[T]] = edges.filter(_.from == node)
  def incoming(node: T): List[Edge[T]] = edges.filter(_.to == node)
  def neighbors(node: T): List[T] = outgoing(node).map(_.to)

  // Need to override at least one of these:
  def adjacent(node1: T, node2: T): Boolean = weight(node1, node2) < Double.PositiveInfinity
  def weight(node1: T, node2: T): Double = if adjacent(node1, node2) then 1 else Double.PositiveInfinity

  def asEdges = Graph.Edges(nodes, edges)
  def asPairs = Graph.Pairs(nodes, pairs)
  def asAdjList = Graph.AdjList(nodes, neighbors)
  def asAdjMatrix = Graph.AdjMatrix(nodes, adjacent)
  def asWeighted = Graph.Weighted(nodes, weight)

object Graph:
  case class Edges[T](nodes: List[T], edges: List[Edge[T]]) extends Graph[T]:
    override def weight(node1: T, node2: T): Double = {
      edges.find(e => e.from == node1 && e.to == node2) match
        case None => Double.PositiveInfinity
        case Some(edge) => edge.weight
    }

  case class Pairs[T](nodes: List[T], _pairs: List[(T, T)]) extends Graph[T]:
    override def pairs: List[(T, T)] = _pairs

    def edges: List[Edge[T]] = pairs.map {
      case (from, to) => Edge.Unweighted(from, to)
    }

    override def adjacent(node1: T, node2: T): Boolean = pairs.contains(node1 -> node2)

    override def toString: String = pairs.mkString("{", ",", "}")

  case class AdjList[T](nodes: List[T], adj: T => List[T]) extends Graph[T]:
    def edges: List[Edge[T]] = {
      for
        node1 <- nodes
        node2 <- adj(node1)
      yield Edge.Unweighted(node1, node2)
    }
    
    override def outgoing(node: T): List[Edge[T]] = neighbors(node).map(Edge.Unweighted(node, _))

    override def neighbors(node: T): List[T] = adj(node)
    
    override def adjacent(node1: T, node2: T): Boolean = adj(node1).contains(node2)

    override def toString: String =
      nodes
        .map(node => s"$node:${adj(node).mkString("{", ",", "}")}")
        .mkString("[", "; ", "]")

  case class AdjMatrix[T](nodes: List[T], matrix: (T, T) => Boolean) extends Graph[T]:
    def edges: List[Edge[T]] = {
      for
        node1 <- nodes
        node2 <- nodes
        if matrix(node1, node2)
      yield Edge.Unweighted(node1, node2)
    }
    
    override def outgoing(node: T): List[Edge[T]] = {
      for
        node2 <- nodes
        if matrix(node, node2)
      yield Edge.Unweighted(node, node2)
    }

    override def incoming(node: T): List[Edge[T]] = {
      for
        node1 <- nodes
        if matrix(node1, node)
      yield Edge.Unweighted(node1, node)
    }
    
    override def neighbors(node: T): List[T] = {
      for
        node2 <- nodes
        if matrix(node, node2)
      yield node2
    }
    
    override def adjacent(node1: T, node2: T): Boolean = matrix(node1, node2)

    override def toString: String =
      nodes
        .map(n1 => s"$n1:${nodes.map(n2 => matrix(n1, n2).compare(false)).mkString}")
        .mkString("[", "; ", "]")

  case class Weighted[T](nodes: List[T], _weight: (T, T) => Double) extends Graph[T]:
    override def edges: List[Edge[T]] = {
      for
        node1 <- nodes
        node2 <- nodes
        if weight(node1, node2) < Double.PositiveInfinity
      yield
        Edge.Weighted(node1, node2, weight(node1, node2))
    }

    override def outgoing(node: T): List[Edge[T]] = {
      for
        node2 <- nodes
        if weight(node, node2) < Double.PositiveInfinity
      yield
        Edge.Weighted(node, node2, weight(node, node2))
    }

    override def incoming(node: T): List[Edge[T]] = {
      for
        node1 <- nodes
        if weight(node1, node) < Double.PositiveInfinity
      yield
        Edge.Weighted(node1, node, weight(node1, node))
    }

    override def weight(node1: T, node2: T): Double = _weight(node1, node2)
    
object GraphDemo:
  // Example 1
  val demo1 = Graph.Pairs(
    List(1, 2, 3, 4),
    List(1->1, 1->2, 1->3, 1->4, 2->2, 2->4, 3->3, 4->4)
  )

  println(demo1)
  println(demo1.asAdjList)
  println(demo1.asAdjMatrix)

  // Example 2
  enum Demo2Nodes:
    case A, B, C, D
  import Demo2Nodes.*

  val demo2 = Graph.AdjList(
    List(A, B, C, D),
    {
      case A => List(B, D)
      case B => List(A, C)
      case C => List(B)
      case D => List(A)
    }
  )

  println(demo2.asPairs)
  println(demo2)
  println(demo2.asAdjMatrix)

  // Example 3
  val demo3 = Graph.AdjMatrix(
    List("red", "green", "yellow"),
    {
      case ("red", "green") => true
      case ("green", "yellow") => true
      case ("yellow", "red") => true
      case _ => false
    }
  )

  println(demo3.asPairs)
  println(demo3.asAdjList)
  println(demo3)

  // Example 4
  val demo4 = Graph.Weighted(
    (1 to 12).toList,
    (a, b) => if b % a == 0 then 1 else Double.PositiveInfinity
  )

  println(demo4.asEdges)
  println(demo4.asPairs)
  println(demo4.asAdjList)
  println(demo4.asAdjMatrix)

import doodle.core.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.image.syntax.core.*
import doodle.core.font.*
import doodle.java2d.*
import doodle.reactor.*
import scala.concurrent.duration.*
import cats.effect.unsafe.implicits.global

trait NodeLayout[T]:
  def location(node: T): Point

class CircleNodeLayout[T](nodes: List[T], radius: Double) extends NodeLayout[T]:
  def location(node: T): Point = {
    val i = nodes.indexOf(node)
    Point(radius, 90.degrees - 1.turns * i / nodes.length)
  }

class GridNodeLayout[T](nodes: List[T], columns: Int, spacing: Double) extends NodeLayout[T]:
  private val shift = (columns - 1) * spacing / 2

  def location(node: T): Point = {
    val i = nodes.indexOf(node)
    val row = i / columns
    val col = i % columns
    Point(col * spacing - shift, -row * spacing)
  }

trait NodeImage[T]:
  def apply(node: T): Image
  def radius(node: T): Double

class DefaultNodeImage[T](diameter: Double) extends NodeImage[T]:
  def apply(node: T): Image = {
    Image.text(node.toString) `on` Image.circle(diameter)
  }
  def radius(node: T): Double = diameter / 2

class ColoredNodeImage[T](diameter: Double, nodeColor: T => Color)
extends DefaultNodeImage[T](diameter):
  override def apply(node: T): Image = {
    super.apply(node).fillColor(nodeColor(node))
  }

def showGraph[T](
  g: Graph[T],
  layout: NodeLayout[T],
  nodeImage: NodeImage[T]
): Image = {
  val arrowLength = 5
  val arrowAngle = 30.degrees
  val loopOut = 240.degrees
  val loopIn = 300.degrees

  val nodeImages = g.nodes.map { case node =>
    val point = layout.location(node)
    nodeImage(node).at(point)
  }

  val edgeImages = g.pairs.map { case (node1, node2) =>
    val point1 = layout.location(node1)
    val point2 = layout.location(node2)
    if node1 == node2 then
      val radius = nodeImage.radius(node1)
      val p1 = point1 + Vec(radius, loopOut)
      val p2 = point1 + Vec(radius, loopIn)
      val c1 = point1 + Vec(3 * radius, loopOut)
      val c2 = point1 + Vec(3 * radius, loopIn)
      val p3 = p2 + Vec(arrowLength, loopIn + arrowAngle)
      val p4 = p2 + Vec(arrowLength, loopIn - arrowAngle)
      Image.path(OpenPath.empty
        .moveTo(p1).curveTo(c1, c2, p2)
        .moveTo(p2).lineTo(p3)
        .moveTo(p2).lineTo(p4)
      )
    else
      val radius1 = nodeImage.radius(node1)
      val radius2 = nodeImage.radius(node2)
      val v = (point2 - point1).normalize
      val p1 = point1 + v * radius1
      val p2 = point2 + v * -radius2
      val p3 = p2 + v.rotate(arrowAngle) * -arrowLength
      val p4 = p2 + v.rotate(-arrowAngle) * -arrowLength
      Image.path(OpenPath.empty
        .moveTo(p1).lineTo(p2)
        .moveTo(p2).lineTo(p3)
        .moveTo(p2).lineTo(p4)
      )
  }

  nodeImages.foldLeft(Image.empty)(_ `on` _) `on`
    edgeImages.foldLeft(Image.empty)(_ `on` _)
}

@main def demo(): Unit = {
  import GraphDemo.*

  showGraph(demo1, new CircleNodeLayout(demo1.nodes, 50), new DefaultNodeImage(20)).draw()
  showGraph(demo2, new GridNodeLayout(demo2.nodes, 3, 50), new DefaultNodeImage(20)).draw()

  def nodeColor(node: String): Color = node match
    case "red" => Color.red
    case "yellow" => Color.yellow
    case "green" => Color.green
    case _ => Color.white
  showGraph(demo3, new CircleNodeLayout(demo3.nodes, 50), new ColoredNodeImage(40, nodeColor)).draw()

  showGraph(demo4, new CircleNodeLayout(demo4.nodes, 100), new DefaultNodeImage(20)).draw()
}
