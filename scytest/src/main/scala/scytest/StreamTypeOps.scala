package scytest

import cats.data.{Chain, NonEmptyChain}
import cats.effect.Sync
import fs2.Stream

private[scytest] class StreamTypeOps(s: Stream.type) {

  def chain[F[_]: Sync, A](chain: Chain[A]): Stream[F, A] =
    s.fromIterator[F](chain.iterator)

  def chain[F[_]: Sync, A](chain: NonEmptyChain[A]): Stream[F, A] =
    s.fromIterator[F](chain.iterator)
}
