package dijkstra

import scala.annotation.tailrec
import scala.collection.mutable

final case class DirectedEdge(from: Int, to: Int, weight: Double)

final case class EdgeWeightedDigraph(adj: mutable.LinkedHashMap[Int, List[DirectedEdge]] = mutable.LinkedHashMap())

object EdgeWeightedDigraphOps {

  implicit class EdgeWeightedDigraphOps(g: EdgeWeightedDigraph) {
    def addEdge(e: DirectedEdge): EdgeWeightedDigraph = {
      val list = g.adj.getOrElse(e.from, List.empty)
      val adj = g.adj.clone() += (e.from -> (list :+ e))
      EdgeWeightedDigraph(adj)
    }
  }

}

object ShortestPath {

  def run(g: EdgeWeightedDigraph, sourceV: Int): Either[String, ShortestPathCalc] = {
    val size = g.adj.size

    if (sourceV >= g.adj.size) Left(s"Source vertex must in range [0, $size)")
    else {
      val edgeTo = mutable.ArrayBuffer.fill[Option[DirectedEdge]](size)(None)
      val distTo = mutable.ArrayBuffer.fill(size)(Double.PositiveInfinity)

      //init source distance and add to the queue
      distTo(sourceV) = 0.0
      val sourceDist = (sourceV, distTo(sourceV))
      val queue = mutable.PriorityQueue[(Int, Double)](sourceDist)((a, b) => a._2.compareTo(b._2))

      while (queue.nonEmpty) {
        val (minDestV, _) = queue.dequeue()
        val edges = g.adj.getOrElse(minDestV, List.empty)

        edges.foreach { e =>
          if (distTo(e.to) > distTo(e.from) + e.weight) {
            distTo(e.to) = distTo(e.from) + e.weight
            edgeTo(e.to) = Some(e)
            if (!queue.exists(_._1 == e.to)) queue.enqueue((e.to, distTo(e.to)))
          }
        }
      }

      Right(new ShortestPathCalc(edgeTo, distTo))
    }
  }
}

class ShortestPathCalc(edgeTo: Seq[Option[DirectedEdge]], distTo: Seq[Double]) {
  def pathTo(v: Int): Either[String, Seq[DirectedEdge]] = {

    @tailrec
    def go(list: List[DirectedEdge], vv: Int): List[DirectedEdge] =
      edgeTo(vv) match {
        case Some(e) => go(e +: list, e.from)
        case None => list
      }

    hasPath(v).map(b => if (!b) Seq() else go(List(), v))
  }

  def hasPath(v: Int): Either[String, Boolean] =
    distTo.lift(v).map(_ < Double.PositiveInfinity).toRight(s"Vertex $v does not exist")

  def distToV(v: Int): Either[String, Double] =
    distTo.lift(v).toRight(s"Vertex $v does not exist")
}
