package kodkod

import singleton.ops._

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.{specialized => sp}

/** Specialized immutable array slice.
  */
final class Slice[@sp A] private[kodkod]
(val data: Array[A],
 val start: Int,
 val end: Int
) {
  def size: Int = end - start

  def isEmpty: Option[Empty[this.type]] =
    if (end == start) Empty.some[this.type]
    else None

  def nonEmpty: Option[NonEmpty[this.type]] =
    if (end > start) NonEmpty.some[this.type]
    else None

  def head(implicit P: NonEmpty[this.type]): A =
    data(start)

  def last(implicit P: NonEmpty[this.type]): A =
    data(end - 1)

  def uncons(implicit P: NonEmpty[this.type]): (A, Slice[A]) =
    (data(start), new Slice(data, start + 1, end))

  def unsnoc(implicit P: NonEmpty[this.type]): (Slice[A], A) =
    (new Slice(data, start, end - 1), data(end - 1))

  def map[B](f: A => B)(implicit B: ClassTag[B]): Slice[B] = {
    val res = B.newArray(size)

    @tailrec def go(i: Int): Unit = if (i < end) {
      res(i - start) = f(data(i))
      go(i + 1)
    }
    go(start)

    new Slice(res, 0, size)
  }

  type GoodIndex[x <: Int with Singleton] = Require[x < (end.type - start.type)]

  def indices: DepRange[GoodIndex] =
    DepRange.pi(new Pi[Int, GoodIndex] {
      def apply(a: Int): GoodIndex[a.type] = null.asInstanceOf[GoodIndex[a.type]]
    })(0, end - start)

  def apply(index: Int)(implicit R: Require[index.type < (end.type - start.type)]): A =
    data(start + index)

  def foreach[U](f: A => U)(implicit U: UseSideEffects): Unit = {
    @tailrec def go(i: Int): Unit = if (i < end) {
      f(data(i))
      go(i + 1)
    }
    go(start)
  }

  def filter(f: A => Boolean): Slice[A] = {
    val A = Utils.arrayClassTag(data).asInstanceOf[ClassTag[A]]
    val res = A.newArray(size)

    @tailrec def go(src: Int, dst: Int): Int = if (src < end) {
      if (f(data(src))) {
        res(dst) = data(src)
        go(src + 1, dst + 1)
      } else go(src + 1, dst)
    } else dst

    val len = go(start, 0)
    new Slice(res, 0, len)
  }

  def copy: Slice[A] = {
    val A = Utils.arrayClassTag(data).asInstanceOf[ClassTag[A]]
    val res = A.newArray(size)
    System.arraycopy(data, start, res, 0, size)

    new Slice(res, 0, size)
  }

  override def hashCode: Int =
    Utils.genericHashCode(data, start, end, 1)

  override def toString: String = {
    val sb: StringBuilder = new StringBuilder
    sb.append("Slice(")
    @tailrec def go(i: Int): Unit = if (i < end) {
      sb.append(data(i))
      if (i != end - 1) sb.append(", ")
      go(i + 1)
    }
    go(start)
    sb.append(")")
    sb.toString
  }

  override def equals(obj: Any): Boolean = throw new NotImplementedError()
}
object Slice {
  def apply[A](a: A*)(implicit A: ClassTag[A]): Slice[A] =
    new Slice(a.toArray, 0, a.length)

  def empty[A](implicit A: ClassTag[A]): Slice[A] =
    new Slice(A.newArray(0), 0, 0)
}