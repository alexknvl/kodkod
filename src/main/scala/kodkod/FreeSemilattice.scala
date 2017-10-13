package kodkod

import cats.kernel.Semilattice

trait FreeSemilattice[F[_]] {
  def empty[A]: F[A]

  def combine[A](fa: F[A], fb: F[A]): F[A]

  def lift[A](a: A): F[A]

  def foldMap[A, B: Semilattice](fa: F[A])(f: A => B): B

  def liftSeq[A](a: A*): F[A] =
    a.foldLeft(empty[A])((acc, x) => combine[A](acc, lift[A](x)))
}
