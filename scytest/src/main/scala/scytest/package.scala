package object scytest { module =>
  private[scytest] implicit def toStreamTypeOps(
      s: fs2.Stream.type
  ): StreamTypeOps =
    new StreamTypeOps(s)
}
