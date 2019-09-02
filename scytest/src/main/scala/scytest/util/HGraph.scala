package scytest.util

import cats._
import cats.implicits._

import fs2.Stream

object HGraph {

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

    assert(nodeIds == edges.keySet)
    assert(nodeIds == reverseEdges.keySet)
    assert(edges.values.toSet.flatten.subsetOf(nodeIds))
    assert(reverseEdges.values.toSet.flatten.subsetOf(nodeIds))

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

    def isEmpty: Boolean = nodes.isEmpty
    def nonEmpty: Boolean = !isEmpty

    /** Swap the direction of all edges in this graph */
    lazy val reverseDirection: Graph[V] = new Reversed(this)

    private lazy val rootToLeaf: Iterable[Node[V]] = reverseDirection.leafToRoot
    private lazy val leafToRoot: Iterable[Node[V]] =
      fs2.Stream
        .unfold(this) { g =>
          if (g.isEmpty) None
          else Some(g.extractLeafs)
        }
        .flatMap(ns => Stream.emits(ns.toSeq))
        .compile
        .toList

    /** Monadic visit all nodes in a specified order, producing a new graph with each
      *  node's label adjusted by `f`, but the graph shape remaining the same
      */
    def relabelA[F[_]: Applicative, V2[_]](order: VisitOrder)(
        f: Visitor[F, V, V2]
    ): F[Graph[V2]] = {
      val ns = order match {
        case VisitOrder.LeafsFirst => leafToRoot
        case VisitOrder.RootsFirst => rootToLeaf
      }

      ns.toList
        .traverse { n =>
          f(n.label).map(label => Node(label, n.id))
        }
        .map { ns =>
          new Graph[V2](
            ns.map(n => n.id -> n).toMap,
            edges,
            reverseEdges
          )
        }
    }

    /** Modify all label contexts with `f`, producing a new graph with the same shape */
    def relabel[V2[_]](f: Visitor[cats.Id, V, V2]): Graph[V2] = {
      val newNodes: Map[NodeId, Node[V2]] = nodes.map {
        case (nodeId, node) =>
          nodeId -> Node[V2, node.L](f(node.label), nodeId)
      }
      new Graph(newNodes, edges, reverseEdges)
    }

    /** Remove all root nodes, producing a new graph without those nodes, and with edges directed to those removed */
    lazy val extractRoots: (Set[Node[V]], Graph[V]) = {

      val (ns, g) = reverseDirection.extractLeafs
      (ns, g.reverseDirection)
    }

    /** Remove all leaf nodes, producing a new graph without those nodes, and with their edges removed */
    lazy val extractLeafs: (Set[Node[V]], Graph[V]) = {
      val leafIds = reverseEdges.collect { case (k, v) if v.isEmpty => k }.toSet
      val (leafs, nonLeafs) = nodes.partition(kv => leafIds.contains(kv._1))
      val newGraph = new Graph[V](
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
    override val reverseDirection: Graph[V] = g
  }

  object Graph {
    def empty[V[_]]: Graph[V] = new Graph[V](Map.empty, Map.empty, Map.empty)
  }

  /** forall [L], `V[L] => F[G[L]]` */
  trait Visitor[F[_], V[_], G[_]] {
    def apply[L](label: V[L]): F[G[L]]
  }

  /** The order in which a graph visitor  */
  sealed trait VisitOrder

  object VisitOrder {
    case object LeafsFirst extends VisitOrder
    case object RootsFirst extends VisitOrder
  }

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

  class NodeId

}
