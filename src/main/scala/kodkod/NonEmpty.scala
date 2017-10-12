package kodkod

/** Proof that a collection is not empty.
  */
final case class NonEmpty[L <: Singleton] private()
object NonEmpty {
  private[this] val instance: NonEmpty[Singleton] =
    new NonEmpty[Singleton]

  private[this] val someInstance: Some[NonEmpty[Singleton]] =
    Some(instance)

  private[kodkod] def make[L <: Singleton]: NonEmpty[L] =
    instance.asInstanceOf[NonEmpty[L]]

  private[kodkod] def some[L <: Singleton]: Option[NonEmpty[L]] =
    someInstance.asInstanceOf[Option[NonEmpty[L]]]
}