package kodkod

import cats.Monoid
import cats.instances.list._

object ScalaUtils {
  def orMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }
}

trait FreeMonoid[F[_]] {
  def empty[A]: F[A]
  def combine[A](fa: F[A], fb: F[A]): F[A]

  def lift[A](a: A): F[A]

  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

  def liftSeq[A](a: A*): F[A] =
    a.foldLeft(empty[A])((acc, x) => combine[A](acc, lift[A](x)))

  def cons[A](fa: F[A])(a: A): Sigma[F[A], NonEmpty] = {
    val result = combine(lift(a), fa)
    Sigma(result)(NonEmpty.make[result.type])
  }

  def snoc[A](fa: F[A])(a: A): Sigma[F[A], NonEmpty] = {
    val result = combine(fa, lift(a))
    Sigma(result)(NonEmpty.make[result.type])
  }

  def toList[A](fa: F[A]): List[A] =
    foldMap[A, List[A]](fa)(a => List(a))

  def nonEmpty[A](fa: F[A]): Option[NonEmpty[fa.type]] =
    if (foldMap[A, Boolean](fa)(_ => true)(ScalaUtils.orMonoid)) NonEmpty.some[fa.type]
    else None

  def uncons[A](fa: F[A])(implicit P: NonEmpty[fa.type]): (A, F[A]) = {
    val list = toList[A](fa)
    (list.head, liftSeq(list.tail:_*))
  }
  def unsnoc[A](fa: F[A])(implicit P: NonEmpty[fa.type]): (F[A], A) = {
    val list = toList[A](fa)
    (liftSeq(list.init:_*), list.last)
  }
}