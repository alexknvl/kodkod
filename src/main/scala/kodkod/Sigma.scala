package kodkod

import cats.Id

sealed abstract class Sigma[A, F[_ <: A with Singleton]] {
  val first: A
  val second: F[first.type]
}
object Sigma {
  def apply[A, F[_ <: A with Singleton]](a: A)(fa: F[a.type]): Sigma[A, F] =
    new Sigma[A, F] {
      val first = a
      val second = fa.asInstanceOf[F[first.type]]
    }

  def id[A](a: A): Sigma[A, Id] = Sigma[A, Id](a)(a)
}

abstract class Pi[A, F[_ <: A with Singleton]] {
  def apply(a: A): F[a.type]
}
object Pi {
  def id[A]: Pi[A, Id] = new Pi[A, Id] {
    override def apply(a: A): Id[a.type] = a
  }
}