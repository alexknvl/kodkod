package kodkod

import scala.annotation.tailrec

/** Non-specialized linear array queue.
  *
  * O(1) cons, snoc, unsnoc, uncons, size
  * O(N) map, copy
  */
final class Buf[+A] private[kodkod]
(private[this] var data: Array[Any],
 private[this] var front: Int,
 private[this] var rear: Int,
 private[this] var count: Int)
{
  @inline private[this] def capacity: Int = data.length
  @inline private[this] def next(i: Int) = (i + 1) % capacity
  @inline private[this] def prev(i: Int) = (i - 1 + capacity) % capacity

  def ensureCapacity(atLeast: Int): Unit = {
    val newCapacity = math.max(16, math.max(capacity, atLeast * 3 / 2))
    val newData = Array.ofDim[Any](newCapacity)

    val tail = math.min(count, data.length - front)
    System.arraycopy(data, front, newData, 0, tail)
    System.arraycopy(data, 0, newData, tail, count - tail)

    data = newData
    front = 0
    rear = count - 1
  }

  def size: Int = count

  def isEmpty: Option[Empty[this.type]] =
    if (count == 0) Empty.some[this.type]
    else None

  def nonEmpty: Option[NonEmpty[this.type]] =
    if (count > 0) NonEmpty.some[this.type]
    else None

  def head(implicit P: NonEmpty[this.type]): A =
    data(front).asInstanceOf[A]

  def last(implicit P: NonEmpty[this.type]): A =
    data(rear).asInstanceOf[A]

  def +:[B >: A](item: B): Buf[B] = {
    if (count == capacity)
      ensureCapacity(count + 1)
    count += 1
    front = prev(front)
    data(front) = item
    this
  }

  def :+[B >: A](item: B): Buf[B] = {
    if (count == capacity)
      ensureCapacity(count + 1)
    count += 1
    rear = next(rear)
    data(rear) = item
    this
  }

  def uncons(implicit P: NonEmpty[this.type]): (A, Buf[A]) = {
    val result = data(front)
    front = next(front)
    count -= 1
    (result.asInstanceOf[A], this)
  }

  def unsnoc(implicit P: NonEmpty[this.type]): (Buf[A], A) = {
    val result = data(rear)
    rear = prev(rear)
    count -= 1
    (this, result.asInstanceOf[A])
  }

  def map[B](f: A => B): Buf[B] = {
    @tailrec def go(counter: Int, i: Int): Unit = if (counter < count) {
      data(i) = f(data(i).asInstanceOf[A]).asInstanceOf[Any]
      go(counter + 1, next(i))
    }
    go(0, front)
    this.asInstanceOf[Buf[B]]
  }

  def copy: Buf[A] =
    new Buf(data.clone(), front, rear, size)

  def foreach[U](f: A => U)(implicit U: UseSideEffects): Unit = {
    @tailrec def go(counter: Int, i: Int): Unit = if (counter < count) {
      f(data(i).asInstanceOf[A])
      go(counter + 1, next(i))
    }
    go(0, front)
  }

  override def hashCode: Int = {
    val tail = math.min(count, data.length - front)
    val r = Utils.hashCode(data.asInstanceOf[Array[Object]], front, tail, 1)
    Utils.hashCode(data.asInstanceOf[Array[Object]], tail, count - tail, r)
  }

  override def toString: String = {
    val sb: StringBuilder = new StringBuilder
    sb.append("Buf(")
    @tailrec def go(i: Int): Unit = if (i < size) {
      sb.append(data(i))
      if (i != size - 1) sb.append(", ")
      go(i + 1)
    }
    go(0)
    sb.append(")")
    sb.toString
  }
}
object Buf {
  def apply[A](a: A*): Buf[A] =
    new Buf(a.toArray[Any], 0, a.length - 1, a.length)

  def empty[A]: Buf[A] =
    ofCapacity(16)

  def ofCapacity[T](capacity: Int): Buf[T] =
    new Buf[T](
      data = Array.ofDim(math.max(0, capacity)),
      front = 0,
      rear = capacity - 1,
      count = 0)
}