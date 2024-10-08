package graphs

trait Graph[T]:
  def nodes: List[T]
  def pairs: List[(T, T)]
  def neighbors(node: T): List[T]
  def adjacent(node1: T, node2: T): Boolean
  def asPairs = Graph.Pairs(nodes, pairs)
  def asAdjList = Graph.AdjList(nodes, neighbors)
  def asAdjMatrix = Graph.AdjMatrix(nodes, adjacent)

object Graph:
  case class Pairs[T](nodes: List[T], pairs: List[(T, T)]) extends Graph[T]:
    def neighbors(node: T): List[T] = {
      for
        (node1, node2) <- pairs
        if node1 == node
      yield node2
    }

    def adjacent(node1: T, node2: T): Boolean = pairs.contains(node1 -> node2)

    override def toString: String = pairs.mkString("{", ",", "}")

  case class AdjList[T](nodes: List[T], adj: T => List[T]) extends Graph[T]:
    def pairs: List[(T, T)] = {
      for
        node1 <- nodes
        node2 <- adj(node1)
      yield (node1, node2)
    }
    
    def neighbors(node: T): List[T] = adj(node)
    
    def adjacent(node1: T, node2: T): Boolean = adj(node1).contains(node2)

    override def toString: String =
      nodes
        .map(node => s"$node:${adj(node).mkString("{", ",", "}")}")
        .mkString("[", "; ", "]")

  case class AdjMatrix[T](nodes: List[T], matrix: (T, T) => Boolean) extends Graph[T]:
    def pairs: List[(T, T)] = {
      for
        node1 <- nodes
        node2 <- nodes
        if matrix(node1, node2)
      yield (node1, node2)
    }
    
    def neighbors(node: T): List[T] = {
      for
        node2 <- nodes
        if matrix(node, node2)
      yield node2
    }
    
    def adjacent(node1: T, node2: T): Boolean = matrix(node1, node2)

    override def toString: String =
      nodes
        .map(n1 => s"$n1:${nodes.map(n2 => matrix(n1, n2).compare(false)).mkString}")
        .mkString("[", "; ", "]")

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
}
