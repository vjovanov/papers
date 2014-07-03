package vector
import scala.reflect.ClassTag
/*
 * Vector example should cover all of the functionality of YY. This example is also good for testing inheritance.
 * Pros: Covers every aspect of YY.
 *    - Code generation for inheritance, type params, data etc.
 *    - Virtualization
 *    - Guarded recompilation based on types and based on values.
 * Cons: Too long. Can it be done with our framework?
 * TODO: Can we make it smaller e.g. exclude implementations of apply and size?
 */
trait Vector[T] {
  def size: Int
  def apply(x: Int): T
  def foreach(x:T => Unit): Unit
  def *(v: Vector[T])(implicit num: Numeric[T]): Vector[T]
  def map[U: Numeric](f: T => U): Vector[U]
}

object DenseVector {
  def fill[T: ClassTag](v: T, s: Int): Vector[T] =
    new DenseVector(Seq.fill(s)(v))
}

class DenseVector[T](private[this] val data: Seq[T]) extends Vector[T] {
  def size: Int = data.size
  def apply(x: Int) = data(x)
  def foreach(f:T => Unit): Unit = data foreach f
  def *(that: Vector[T])(implicit num: Numeric[T]): Vector[T] =
    new DenseVector(data.zipWithIndex.map(x => num.times(x._1, that(x._2))))
  def map[U: Numeric](f: T => U): Vector[U] = new DenseVector(data.map(f))
}

class SparseVector[T](val size: Int, val data: List[(Int, T)], default: T) extends Vector[T] {
  def apply(i: Int): T = data.find(x => x._1 == i) match {
    case Some((_, v)) => v
    case None => default
  }

  def foreach(f:T => Unit) = (0 until size) foreach { i =>
    f(data.find(x => x._1 == i) match {
      case Some((_, v)) => v
      case None => default
    })
  }

  def *(that: Vector[T])(implicit num: Numeric[T]): SparseVector[T] =
     new SparseVector(
      size,
      data flatMap {
        data =>
          if (that(data._1) == 0)
            Nil
          else
            List((data._1, num.times(that(data._1), data._2)))
      },
      num.zero
    )
}