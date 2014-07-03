package deepgen.vector

import scala.reflect.ClassTag
import scala.math.Numeric.Implicits._
/*
 * Smaller version of Vector example to show automatic deep generation
 */

object shallow {
  object Vector {
    def fill[T:Numeric](value: T)(size: Int): Vector[T] =
      fromSeq(Seq.fill(size)(value))
    def fromSeq[T:Numeric](seq: Seq[T]): Vector[T] =
      new Vector(seq)
    def range(start: Int, end: Int): Vector[Int] =
      fromSeq(Seq.range(start, end))
  }
  class Vector[T:Numeric](val data: Seq[T]) {
    def +(that: Vector[T]): Vector[T] =
      Vector.fromSeq(data.zip(that.data).map(x => x._1 + x._2))
    def map[S: Numeric](f: T => S): Vector[S] =
      Vector.fromSeq(data.map(x => f(x)))
  }
}

object deep {
  // must be elided
  // begin


  trait Base {
    type Rep[T] = Reps[T]
    trait Reps[T] {
      def atPhase(phase: Phase)(block: => Rep[T]): Rep[T] = block
    }
    trait Phase
    object lowering extends Phase
    case class Const[T](v: T) extends Reps[T]

    trait Numeric[T] {
      def plus(a: Rep[T], b: Rep[T]): Rep[T]
      def zero: Rep[T]
    }

    class NumericOps[T:Numeric](val v: Rep[T]) {
      def +(other: Rep[T]): Rep[T] = implicitly[Numeric[T]].plus(v, other)
    }

    implicit def numerize[T:Numeric](v: Rep[T]) = new NumericOps(v)

    object Seq {
      def fill[T](size: Rep[Int])(value: Rep[T]): Rep[Seq[T]] =
        SeqFill(size, value)
    }

    implicit class SeqRep[T](seq: Rep[Seq[T]]) extends Reps[Seq[T]] {
      def zip[S](that: Rep[Seq[S]]): Rep[Seq[(T, S)]] = SeqZip(seq, that)
      def map[S](f: Rep[T] => Rep[S]): Rep[Seq[S]] = SeqMap(seq, f)
    }

    implicit class Tup2Rep[T1, T2](tup2: Rep[(T1, T2)]) extends Reps[(T1, T2)] {
      def _1: Rep[T1] = ???
      def _2: Rep[T2] = ???
      // HACK!
      // def _1: T1 = ???
      // def _2: T2 = ???
    }

    case class SeqFill[T](size: Rep[Int], value: Rep[T]) extends Reps[Seq[T]]
    case class SeqZip[T, S](seq: Rep[Seq[T]], that: Rep[Seq[S]]) extends Reps[Seq[(T, S)]]
    case class SeqMap[T, S](seq: Rep[Seq[T]], f: Rep[T] => Rep[S]) extends Reps[Seq[S]]
  }
  // end

  trait VectorOps extends Base {
    // must be elided
    // begin
    type Vector[T] = shallow.Vector[T]

    implicit class VectorRep[T:Numeric](v: Rep[Vector[T]]) extends Reps[Vector[T]] {
      val data: Rep[Seq[T]] = vector_data(v)
      def +(that: Rep[Vector[T]]): Rep[Vector[T]] = vector_plus(v, that)
      def map[S: Numeric](f: Rep[T] => Rep[S]): Rep[Vector[S]] = vector_map(v, f)
    }

    object Vector {
      def fromSeq[T:Numeric](seq: Rep[Seq[T]]): Rep[Vector[T]] =
        vector_fromSeq(seq)
      def fill[T:Numeric](value: Rep[T])(size: Rep[Int]): Rep[Vector[T]] =
        vector_fill(value, size)
      def range(start: Rep[Int], end: Rep[Int]): Rep[Vector[Int]] =
        vector_range(start, end)
    }
    // end
    def vector_plus[T:Numeric](a: Rep[Vector[T]], b: Rep[Vector[T]]): Rep[Vector[T]]
    def vector_map[T:Numeric, S: Numeric](v: Rep[Vector[T]], f: Rep[T] => Rep[S]): Rep[Vector[S]]
    def vector_fill[T:Numeric](value: Rep[T], size: Rep[Int]): Rep[Vector[T]]
    def vector_fromSeq[T:Numeric](seq: Rep[Seq[T]]): Rep[Vector[T]]
    def vector_range(start: Rep[Int], end: Rep[Int]): Rep[Vector[Int]]
    def vector_data[T: Numeric](v: Rep[Vector[T]]) = Seq.fill(Const(10))(implicitly[Numeric[T]].zero) /* HACK */
  }

  trait VectorExp extends VectorOps {
    // High level IR nodes definitions
    case class VectorPlus[T:Numeric](a: Rep[Vector[T]], b: Rep[Vector[T]]) extends Reps[Vector[T]]
    case class VectorMap[T:Numeric,S:Numeric](v: Rep[Vector[T]], f: Rep[T] => Rep[S]) extends Reps[Vector[S]]
    case class VectorFill[T:Numeric](value: Rep[T], size: Rep[Int]) extends Reps[Vector[T]]
    case class VectorSeq[T:Numeric](data: Rep[Seq[T]]) extends Reps[Vector[T]]
    case class VectorRange(start: Rep[Int], end: Rep[Int]) extends Reps[Vector[Int]]

    def vector_plus[T:Numeric](a: Rep[Vector[T]], b: Rep[Vector[T]]): Rep[Vector[T]] = VectorPlus(a, b)
    def vector_map[T:Numeric, S: Numeric](v: Rep[Vector[T]], f: Rep[T] => Rep[S]): Rep[Vector[S]] = VectorMap(v, f)
    def vector_fill[T:Numeric](value: Rep[T], size: Rep[Int]): Rep[Vector[T]] = VectorFill(value, size)
    def vector_fromSeq[T:Numeric](seq: Rep[Seq[T]]): Rep[Vector[T]] = VectorSeq(seq)
    def vector_range(start: Rep[Int], end: Rep[Int]): Rep[Vector[Int]] = VectorRange(start, end)
  }

  trait VectorLowLevel extends VectorExp {
    // Low level implementations
    override def vector_plus[T:Numeric](a: Rep[Vector[T]], b: Rep[Vector[T]]): Rep[Vector[T]] =
      VectorPlus(a, b).atPhase(lowering) {
        Vector.fromSeq(a.data.zip[T](b.data).map[T]((x: Rep[(T, T)]) => x._1 + x._2))
      }

    override def vector_map[T:Numeric, S: Numeric](v: Rep[Vector[T]], f: Rep[T] => Rep[S]): Rep[Vector[S]] =
      VectorMap(v, f).atPhase(lowering) {
        Vector.fromSeq(v.data.map(x => f(x)))
      }
    override def vector_fill[T:Numeric](value: Rep[T], size: Rep[Int]): Rep[Vector[T]] =
      VectorFill(value, size).atPhase(lowering) {
        Vector.fromSeq(Seq.fill[T](size)(value))
      }
  }
}