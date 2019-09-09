package scytest

import scytest.fixture.Fixture
import scytest.util.PolyUtil._
import shapeless._
import shapeless.ops.hlist

trait TestP[F[_]] {
  val name: String
  def fixtures: List[Fixture[F, _]]

  /** The type of the collected FixtureTags this test depends on */
  type DepTags <: HList
  protected val rawDepTags: DepTags

  /** Proof that DepTags actually represents FixtureTags */
  val tagMapper: hlist.Mapper[toTag.type, DepTags]

  /** The dependency tags */
  final val depTags: tagMapper.Out = tagMapper(rawDepTags)

  val tagFn: TagFn[tagMapper.Out]

  final type RunDeps = tagFn.Objs
  def run(dependencies: RunDeps): F[TestResult]
}

final class FixturelessTestP[F[_]](
    override val name: String,
    body: => F[TestResult]
) extends TestP[F] {

  override val fixtures: List[Fixture[F, _]] = Nil

  override type DepTags = HNil
  override protected val rawDepTags: HNil = HNil
  override val tagMapper: hlist.Mapper.Aux[toTag.type, HNil, HNil] = implicitly
  override val tagFn: TagFn[HNil] = implicitly[TagFn[HNil]]
  override def run(dependencies: RunDeps): F[TestResult] = body
}
