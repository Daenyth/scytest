package scytest.util

import fs2.{Pure, Stream}
import scytest.fixture.FixtureTag
import scytest.util.TagList.TList

import scala.collection.mutable

private[scytest] object HGraph {

  /**
    * A Graph which is:
    * - Directed
    * - Acyclic
    * - Labelled
    * - Each node has a phantom label type, and a corresponding value (of that phantom type) in the graph's context type
    *
    *
    * Terms:
    * Root - Points to no other node
    * Leaf - Is not pointed to by any node
    */
  sealed class Graph private[HGraph] (
      protected[HGraph] val nodes: Map[NodeId, Node],
      protected[HGraph] val edges: Map[NodeId, Set[NodeId]],
      protected[HGraph] val reverseEdges: Map[NodeId, Set[NodeId]]
  ) {
    private val nodeIds: Set[NodeId] = nodes.keySet
    assert(nodeIds == edges.keySet, s"$nodeIds - $edges")
    assert(nodeIds == reverseEdges.keySet)
    assert(edges.values.toSet.flatten.subsetOf(nodeIds))
    assert(reverseEdges.values.toSet.flatten.subsetOf(nodeIds))

    def find[L](label: FixtureTag.Aux[L]): Option[Node.Of[L]] =
      nodes
        .find(kv => kv._2.label == label)
        .map(kv => kv._2.asInstanceOf[Node.Of[L]])

    private[this] val memoLeafFocus: mutable.Map[NodeId, Graph] =
      mutable.Map.empty

    /** A subset of this graph where the only leaf node is `nodeId`, and all other
      *  nodes are either direct or indirect roots of that node
      *
      *  @return Empty graph if nodeId is not present, otherwise the subset
      */
    def focusOnLeaf(nodeId: NodeId): Graph =
      if (memoLeafFocus.contains(nodeId)) memoLeafFocus(nodeId)
      else
        memoLeafFocus.synchronized {
          val built = buildLeafFocus(nodeId)
          memoLeafFocus(nodeId) = built
          built
        }

    private[this] def buildLeafFocus(nodeId: NodeId): Graph = {
      if (!nodes.contains(nodeId)) return Graph.empty

      var included = Set(nodeId)
      var toFind: Set[NodeId] = Set(nodeId)
      while (toFind.nonEmpty) {
        val next = toFind.head
        toFind = toFind - next

        val nextEdges = edges(next)
        toFind = toFind ++ (nextEdges -- included)
        included = included ++ nextEdges
      }

      val newNodes = nodes.filterKeys(included.contains)

      // Optimization to avoid creating new graph instances when possible
      if (newNodes == nodes) return this

      val newEdges = edges.filterKeys(included.contains)
      val newRevEdges = reverseEdges.filterKeys(included.contains)

      new Graph(newNodes, newEdges, newRevEdges) // TODO test (focus + extractLeafs) -> Set(nodeId)
    }

    def isEmpty: Boolean = nodes.isEmpty
    def nonEmpty: Boolean = !isEmpty

    /** Swap the direction of all edges in this graph */
    lazy val reverseDirection: Graph = new Reversed(this)

    /** Unfold of `extractLeafs`, "peeling" back one layer of leafs
      *  at a time and producing them in order, until all nodes have been emitted */
    val unfoldLeafs: Stream[Pure, Set[Node]] =
      Stream.unfold(this) { g =>
        if (g.isEmpty) None
        else Some(g.extractLeafs)
      }

    /** Remove all root nodes, producing a new graph without those nodes, and with edges directed to those removed */
    lazy val extractRoots: (Set[Node], Graph) = {

      val (ns, g) = reverseDirection.extractLeafs
      (ns, g.reverseDirection)
    }

    /** Remove all leaf nodes, producing a new graph without those nodes, and with their edges removed */
    lazy val extractLeafs: (Set[Node], Graph) = {
      val leafIds = reverseEdges.collect { case (k, v) if v.isEmpty => k }.toSet
      val (leafs, nonLeafs) = nodes.partition(kv => leafIds.contains(kv._1))

      val newGraph =
        if (nonLeafs.isEmpty) Graph.empty
        else
          new Graph(
            nonLeafs,
            edges.collect {
              case (from, to) if !leafIds.contains(from) =>
                from -> to.diff(leafIds)
            },
            reverseEdges -- leafIds
          )

      leafs.values.toSet -> newGraph
    }

  }

  /** `g` with all its edges reversed */
  private class Reversed(g: Graph)
      extends Graph(
        nodes = g.nodes,
        edges = g.reverseEdges,
        reverseEdges = g.edges
      ) {
    override lazy val reverseDirection: Graph = g
  }

  object Graph {
    val empty: Graph = new Graph(Map.empty, Map.empty, Map.empty)
    private type EdgeMap = Map[NodeId, Set[NodeId]]

    def newBuilder(): Builder =
      new Builder(Map.empty, Map.empty, Map.empty)

    class Builder private[Graph] (
        var nodes: Map[NodeId, Node],
        var edges: EdgeMap,
        var reverseEdges: EdgeMap
    ) {
      def add[L](label: FixtureTag.Aux[L], edges: TList): Unit = {
        val node = Node(label, NodeId(label))
        this.nodes = nodes.updated(node.id, node)
        this.edges = edges
          .foldLeft(this.edges.updated(node.id, Set.empty)) { (b, tag) =>
            b.updated(node.id, b.getOrElse(node.id, Set.empty) + NodeId(tag))
          }
        reverseEdges = edges
          .foldLeft(reverseEdges.updated(node.id, Set.empty)) { (b, tag) =>
            val vaId = NodeId(tag)
            b.updated(vaId, b.getOrElse(vaId, Set.empty) + node.id)
          }
      }

      // TODO assert cycles fail to build
      def build(): Graph = new Graph(nodes, edges, reverseEdges)
    }
  }

  trait Node {
    type L
    def label: FixtureTag.Aux[L]
    def id: NodeId
  }

  object Node {
    type Of[L0] = Node { type L = L0 }
    def apply[L](label: FixtureTag.Aux[L], id: NodeId): Node =
      new LNode(label, id)
  }

  private class LNode[L0](
      val label: FixtureTag.Aux[L0],
      val id: NodeId
  ) extends Node {
    type L = L0
  }

  final class NodeId private (private val value: Any) {

    override def toString: String = s"NodeId($value)"

    override def equals(obj: Any): Boolean = obj match {
      case other: NodeId => value == other.value
      case _             => false
    }

    override def hashCode(): Int = value.hashCode()
  }

  object NodeId {
    // Unsafe / relies on universal equals/hashcode
    private[HGraph] def apply[T](t: T): NodeId = new NodeId(t)
  }

}
