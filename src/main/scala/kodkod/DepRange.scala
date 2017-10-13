package kodkod

import scala.annotation.tailrec


sealed abstract class DepRange[F[_ <: Int with Singleton]](from: Int, until: Int) {
  protected[this] def apply(i: Int): F[i.type]

  def foreach[U](f: Sigma[Int, F] => U)(implicit U: UseSideEffects): Unit = {
    @tailrec def go(i: Int): Unit = if (i < until) {
      val x = i
      f(Sigma(x)(apply(x)))
      go(i + 1)
    }
    go(from)
  }
}
object DepRange {
  def pi[F[_ <: Int with Singleton]](pi: Pi[Int, F])(from: Int, to: Int): DepRange[F] =
    new DepRange[F](from, to) {
      protected[this] def apply(i: Int): F[i.type] = pi.apply(i)
    }
}