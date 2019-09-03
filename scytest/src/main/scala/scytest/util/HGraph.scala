package scytest.util

import cats._
import cats.implicits._
import fs2.{Pure, Stream}

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
  class Graph[V[_]] private[HGraph] (
      protected[HGraph] val nodes: Map[NodeId, Node[V]],
      protected[HGraph] val edges: Map[NodeId, Set[NodeId]],
      protected[HGraph] val reverseEdges: Map[NodeId, Set[NodeId]]
  ) {
    private val nodeIds: Set[NodeId] = nodes.keySet
    private val emptyInstance = if (nodes.isEmpty) this else Graph.empty[V]

    assert(nodeIds == edges.keySet, s"$nodeIds - $edges")
    assert(nodeIds == reverseEdges.keySet)
    assert(edges.values.toSet.flatten.subsetOf(nodeIds))
    assert(reverseEdges.values.toSet.flatten.subsetOf(nodeIds))

    /*
    def add[L](node: Node.Of[V, L], edges: Set[NodeId]): Graph[V] = {
      require(
        !nodeIds.contains(node.id),
        s"NodeId(${node.id}) already exists in ${this}"
      )
      require(
        edges.forall(id => nodes.contains(id) && id != node.id),
        "All edges must exist in this graph"
      )

      new Graph[V](
        nodes.updated(node.id, node),
        this.edges.updated(node.id, edges),
        edges.foldLeft(reverseEdges.updated(node.id, Set.empty)) { (re, id) =>
          // re.apply is OK here because map key existence is an invariant
          re.updated(id, re(id) + node.id)
        }
      )
    }
     */

    def find[L](label: V[L]): Option[Node.Of[V, L]] =
      nodes
        .find(kv => kv._2.label == label)
        .map(kv => kv._2.asInstanceOf[Node.Of[V, L]])

    private[this] val memoLeafFocus: mutable.Map[NodeId, Graph[V]] =
      mutable.Map.empty

    /** A subset of this graph where the only leaf node is `nodeId`, and all other
      *  nodes are either direct or indirect roots of that node
      *
      *  @return Empty graph if nodeId is not present, otherwise the subset
      */
    def focusOnLeaf(nodeId: NodeId): Graph[V] =
      if (memoLeafFocus.contains(nodeId)) memoLeafFocus(nodeId)
      else
        memoLeafFocus.synchronized {
          println(s"BUILDING FOCUS $nodeId")
          val built = buildLeafFocus(nodeId)
          memoLeafFocus(nodeId) = built
          built
        }

    private def buildLeafFocus(nodeId: NodeId): Graph[V] = {
      if (!nodes.contains(nodeId)) return emptyInstance

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

      new Graph[V](newNodes, newEdges, newRevEdges) // TODO test (focus + extractLeafs) -> Set(nodeId)
    }

    def isEmpty: Boolean = nodes.isEmpty
    def nonEmpty: Boolean = !isEmpty

    /** Swap the direction of all edges in this graph */
    lazy val reverseDirection: Graph[V] = new Reversed(this)

//    private lazy val rootToLeaf: Iterable[Node[V]] = reverseDirection.leafToRoot
//    private lazy val leafToRoot: Iterable[Node[V]] =
//      unfoldLeafs.flatMap(ns => Stream.emits(ns.toSeq)).compile.toList

//    /** Monadic visit all nodes in a specified order, producing a new graph with each
//      *  node's label adjusted by `f`, but the graph shape remaining the same
//      */
//    def relabelA[F[_]: Applicative, V2[_]](order: VisitOrder)(
//        f: Visitor[F, V, V2]
//    ): F[Graph[V2]] = {
//      val ns = order match {
//        case VisitOrder.LeafsFirst => leafToRoot
//        case VisitOrder.RootsFirst => rootToLeaf
//      }
//
//      ns.toList
//        .traverse { n =>
//          f(n.label).map(label => Node(label, n.id))
//        }
//        .map { ns =>
//          new Graph[V2](
//            ns.map(n => n.id -> n).toMap,
//            edges,
//            reverseEdges
//          )
//        }
//    }

//    /** Modify all label contexts with `f`, producing a new graph with the same shape */
//    def relabel[V2[_]](f: Visitor[cats.Id, V, V2]): Graph[V2] = {
//      val newNodes: Map[NodeId, Node[V2]] = nodes.map {
//        case (nodeId, node) =>
//          nodeId -> Node[V2, node.L](f(node.label), nodeId)
//      }
//      new Graph(newNodes, edges, reverseEdges)
//    }

    /** Unfold of `extractLeafs`, "peeling" back one layer of leafs
      *  at a time and producing them in order, until all nodes have been emitted */
    val unfoldLeafs: Stream[Pure, Set[Node[V]]] =
      Stream.unfold(this) { g =>
        if (g.isEmpty) None
        else Some(g.extractLeafs)
      }

    /** Remove all root nodes, producing a new graph without those nodes, and with edges directed to those removed */
    lazy val extractRoots: (Set[Node[V]], Graph[V]) = {

      val (ns, g) = reverseDirection.extractLeafs
      (ns, g.reverseDirection)
    }

    /** Remove all leaf nodes, producing a new graph without those nodes, and with their edges removed */
    lazy val extractLeafs: (Set[Node[V]], Graph[V]) = {
      println(s"EXTRACT $this")
      val leafIds = reverseEdges.collect { case (k, v) if v.isEmpty => k }.toSet
      val (leafs, nonLeafs) = nodes.partition(kv => leafIds.contains(kv._1))

      val newGraph =
        if (nonLeafs.isEmpty) emptyInstance
        else
          new Graph[V](
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
  private class Reversed[V[_]](g: Graph[V])
      extends Graph[V](
        nodes = g.nodes,
        edges = g.reverseEdges,
        reverseEdges = g.edges
      ) {
    override lazy val reverseDirection: Graph[V] = g
  }

  object Graph {
    def empty[V[_]]: Graph[V] = new Graph[V](Map.empty, Map.empty, Map.empty)
    private type EdgeMap = Map[NodeId, Set[NodeId]]

    def newBuilder[V[_]](tl: TypeAnd[V]): Builder[V] =
      new Builder[V](tl, Map.empty, Map.empty, Map.empty)

    class Builder[V[_]] private[Graph] (
        tl: TypeAnd[V],
        var nodes: Map[NodeId, Node[V]],
        var edges: EdgeMap,
        var reverseEdges: EdgeMap
    ) {
      def add[L](label: V[L], edges: TypeAnd[V]#TList): Unit = {
        val node = Node(label, NodeId(label))
        this.nodes = nodes.updated(node.id, node)
        this.edges = edges
          .asInstanceOf[tl.TList]
          .foldLeft(this.edges.updated(node.id, Set.empty))(
            new tl.LFold[Id, EdgeMap] {
              def apply[A](b: EdgeMap, va: V[A]): EdgeMap =
                b.updated(node.id, b.getOrElse(node.id, Set.empty) + NodeId(va))
            }
          )
        reverseEdges = edges
          .asInstanceOf[tl.TList]
          .foldLeft(reverseEdges.updated(node.id, Set.empty))(
            new tl.LFold[Id, EdgeMap] {
              def apply[A](b: EdgeMap, va: V[A]): EdgeMap = {
                val vaId = NodeId(va)
                b.updated(vaId, b.getOrElse(vaId, Set.empty) + node.id)
              }
            }
          )
      }

      // TODO assert cycles fail to build
      def build(): Graph[V] = new Graph[V](nodes, edges, reverseEdges)
    }
  }

//  /** forall [L], `V[L] => F[G[L]]` */
//  trait Visitor[F[_], V[_], G[_]] {
//    def apply[L](label: V[L]): F[G[L]]
//  }
//
//  /** The order in which a graph visitor  */
//  sealed trait VisitOrder
//
//  object VisitOrder {
//    case object LeafsFirst extends VisitOrder
//    case object RootsFirst extends VisitOrder
//  }

  trait Node[V[_]] {
    type L
    def label: V[L]
    def id: NodeId

  }

  object Node {
    type Of[V[_], L0] = Node[V] { type L = L0 }
    def apply[V[_], L](label: V[L], id: NodeId): Node[V] =
      new LNode(label, id)
  }

  class LNode[V[_], L0](
      val label: V[L0],
      val id: NodeId
  ) extends Node[V] {
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
