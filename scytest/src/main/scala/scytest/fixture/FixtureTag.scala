package scytest.fixture

class FixtureTag(val name: String, val scope: FixtureScope) { type R }

object FixtureTag {
  type Aux[R0] = FixtureTag { type R = R0 }

  def apply[R0](name: String, scope: FixtureScope): FixtureTag.Aux[R0] =
    new FixtureTag(name, scope) {
      type R = R0
    }
  val unit: FixtureTag.Aux[Unit] = apply[Unit]("Unit", FixtureScope.Process)
}
