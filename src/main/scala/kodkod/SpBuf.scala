package kodkod

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.{specialized => sp}

/** Specialized linear array queue.
  *
  * O(1) cons, snoc, unsnoc, uncons, size
  * O(N) map, copy
  */
final class SpBuf[@sp A] private[kodkod]
(private var data: Array[A],
 private var front: Int,
 private var rear: Int,
 private var count: Int)
{
  private[this] implicit val classTag: ClassTag[A] =
    Utils.arrayClassTag(data).asInstanceOf[ClassTag[A]]

  @inline private[this] def capacity: Int = data.length
  @inline private[this] def next(i: Int) = (i + 1) % capacity
  @inline private[this] def prev(i: Int) = (i - 1 + capacity) % capacity

  def ensureCapacity(atLeast: Int): ConsumesThis[Unit] = {
    val newCapacity = math.max(16, math.max(capacity, atLeast * 3 / 2))
    val newData = Array.ofDim[A](newCapacity)

    val tail = math.min(count, data.length - front)
    System.arraycopy(data, front, newData, 0, tail)
    System.arraycopy(data, 0, newData, tail, count - tail)

    data = newData
    front = 0
    rear = count - 1
  }

  def size: Int = count

  def isEmpty: Option[Empty[this.type]] =
    if (count == 0) Empty.some[this.type] else None

  def nonEmpty: Option[NonEmpty[this.type]] =
    if (count > 0) NonEmpty.some[this.type] else None

  def head(implicit P: NonEmpty[this.type]): A =
    data(front)

  def last(implicit P: NonEmpty[this.type]): A =
    data(rear)

  def +:(item: A): ConsumesThis[SpBuf[A]] = {
    if (count == capacity)
      ensureCapacity(count + 1)
    count += 1
    front = prev(front)
    data(front) = item
    this
  }

  def :+(item: A): ConsumesThis[SpBuf[A]] = {
    if (count == capacity)
      ensureCapacity(count + 1)
    count += 1
    rear = next(rear)
    data(rear) = item
    this
  }

  def uncons(implicit P: NonEmpty[this.type]): ConsumesThis[(A, SpBuf[A])] = {
    val result = data(front)
    front = next(front)
    count -= 1
    (result, this)
  }

  def unsnoc(implicit P: NonEmpty[this.type]): ConsumesThis[(SpBuf[A], A)] = {
    val result = data(rear)
    rear = prev(rear)
    count -= 1
    (this, result)
  }

  def map[B](f: A => B)(implicit B: ClassTag[B]): ConsumesThis[SpBuf[B]] = {
    val newData: Array[B] =
      if (B.runtimeClass eq classTag.runtimeClass) data.asInstanceOf[Array[B]]
      else B.newArray(data.length)

    @tailrec def go(counter: Int, i: Int): Unit = if (counter < count) {
      newData(i) = f(data(i))
      go(counter + 1, next(i))
    }
    go(0, front)

    this.data = newData.asInstanceOf[Array[A]]
    this.asInstanceOf[SpBuf[B]]
  }

  def widen[B >: A](implicit B: ClassTag[B]): ConsumesThis[SpBuf[B]] =
      new SpBuf(data.toArray[B](B), front, rear, size)

  def foreach[U](f: A => U)(implicit U: UseSideEffects): Unit = {
    @tailrec def go(counter: Int, i: Int): Unit = if (counter < count) {
      f(data(i))
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
    sb.append("SpBuf(")
    @tailrec def go(i: Int): Unit = if (i < size) {
      sb.append(data(i))
      if (i != size - 1) sb.append(", ")
      go(i + 1)
    }
    go(0)
    sb.append(")")
    sb.toString
  }

  override def equals(obj: Any): Boolean = throw new NotImplementedError()
}
object SpBuf {
  def apply[A](a: A*)(implicit A: ClassTag[A]): SpBuf[A] =
    new SpBuf(a.toArray, 0, a.length - 1, a.length)

  def empty[A](implicit A: ClassTag[A]): SpBuf[A] =
    ofCapacity(16)

  def ofCapacity[A](capacity: Int)(implicit A: ClassTag[A]): SpBuf[A] =
    new SpBuf[A](
      data = Array.ofDim[A](math.max(0, capacity)),
      front = 0,
      rear = capacity - 1,
      count = 0)

  def copyOf[A](spBuf: SpBuf[A]): SpBuf[A] =
    new SpBuf(spBuf.data.clone(), spBuf.front, spBuf.rear, spBuf.size)
}