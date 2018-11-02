package dijkstra

object ShortestPathTest extends App {
  import dijkstra.EdgeWeightedDigraphOps._

  val g = EdgeWeightedDigraph()
    .addEdge(DirectedEdge(4, 5, 0.35))
    .addEdge(DirectedEdge(5, 4, 0.35))
    .addEdge(DirectedEdge(4, 7, 0.37))
    .addEdge(DirectedEdge(5, 7, 0.28))
    .addEdge(DirectedEdge(7, 5, 0.28))
    .addEdge(DirectedEdge(5, 1, 0.32))
    .addEdge(DirectedEdge(0, 4, 0.38))
    .addEdge(DirectedEdge(0, 2, 0.26))
    .addEdge(DirectedEdge(7, 3, 0.39))
    .addEdge(DirectedEdge(1, 3, 0.29))
    .addEdge(DirectedEdge(2, 7, 0.34))
    .addEdge(DirectedEdge(6, 2, 0.40))
    .addEdge(DirectedEdge(3, 6, 0.52))
    .addEdge(DirectedEdge(6, 0, 0.58))
    .addEdge(DirectedEdge(6, 4, 0.93))

  val sp = ShortestPath.run(g, 0)
  assert(sp.isRight)

  val path = sp.right.get.pathTo(6)
  assert(path.isRight)

  val actual = path.right.get
  assert(actual.nonEmpty)

  val expected = Seq(
    DirectedEdge(0, 2, 0.26), DirectedEdge(2, 7, 0.34), DirectedEdge(7, 3, 0.39), DirectedEdge(3, 6, 0.52)
  )

  assert(actual == expected)
}
