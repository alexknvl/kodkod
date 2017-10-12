package kodkod

/** Proof that a collection is empty.
  */
final case class Empty[L <: Singleton] private()
object Empty {
  private[this] val instance: Empty[Singleton] =
    new Empty[Singleton]

  private[this] val someInstance: Some[Empty[Singleton]] =
    Some(instance)

  private[kodkod] def make[L <: Singleton]: Empty[L] =
    instance.asInstanceOf[Empty[L]]

  private[kodkod] def some[L <: Singleton]: Option[Empty[L]] =
    someInstance.asInstanceOf[Option[Empty[L]]]
}