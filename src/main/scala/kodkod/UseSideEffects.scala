package kodkod

final case class UseSideEffects private()
object UseSideEffects {
  object Implicits {
    implicit val yes: UseSideEffects = new UseSideEffects
  }
}
