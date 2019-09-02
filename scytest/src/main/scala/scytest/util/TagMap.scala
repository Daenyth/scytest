package scytest.util

import cats.~>
import scytest.fixture.FixtureTag

private[scytest] class TagMap[V[_]] private[TagMap] (
    private val map: Map[FixtureTag, TagMap.Entry[V]]
) {
  import TagMap.Entry

  def get[T](key: FixtureTag.Aux[T]): V[T] =
    map(key).value.asInstanceOf[V[T]]

  def put[T](key: FixtureTag.Aux[T], value: V[T]): TagMap[V] =
    new TagMap[V](map.updated[Entry[V]](key, Entry(key, value)))

  def modify[T](key: FixtureTag.Aux[T])(f: V[T] => V[T]): TagMap[V] =
    put(key, f(get(key)))

  def mapE[V2[_]](
      f: Entry.Aux[V, ?] ~> Entry.Aux[V2, ?]
  ): TagMap[V2] = {
    val newMap: Map[FixtureTag, Entry[V2]] =
      map.map {
        case (k, e) =>
          k -> f.apply[e.A](e).asInstanceOf[Entry[V2]]
      }
    new TagMap[V2](newMap)
  }

  def ++(other: TagMap[V]): TagMap[V] =
    new TagMap[V](map ++ other.map)

  val keys: Iterable[FixtureTag] = map.keys

  def collectValues[B](f: V[_] => Option[B]): Iterable[(FixtureTag, B)] =
    map.values.map(e => f(e.value).map(b => e.key -> b).toIterable).flatten

}

private[scytest] object TagMap {
  def empty[V[_]] = new TagMap[V](Map.empty[FixtureTag, Entry[V]])
  def of[V[_]](items: Entry[V]*): TagMap[V] =
    items.foldLeft(empty[V])((tm, e) => tm.put(e.key, e.value))

  trait Entry[V[_]] {
    type A
    def key: FixtureTag.Aux[A]
    def value: V[A]

    def map[V2[_]](f: V[A] => V2[A]): Entry.Aux[V2, A]
  }
  object Entry {
    def apply[V[_], A](key: FixtureTag.Aux[A], value: V[A]): Entry.Aux[V, A] =
      new Impl(key, value)

    implicit def fromTuple[V[_], A](kv: (FixtureTag.Aux[A], V[A])): Entry[V] =
      new Impl(kv._1, kv._2)

    type Aux[V[_], A0] = Entry[V] { type A = A0 }
    private class Impl[V[_], A0](
        val key: FixtureTag.Aux[A0],
        val value: V[A0]
    ) extends Entry[V] {
      type A = A0

      def map[V2[_]](f: V[A] => V2[A]): Aux[V2, A0] = Entry(key, f(value))
    }
  }
}
