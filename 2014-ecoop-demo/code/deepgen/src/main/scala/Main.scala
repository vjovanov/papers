package deepgen.vector

object Main extends App {
  // DSL scope
  def optiVect[T](b: => T) = b
  def pow(b: Int, e: Int): Int = if (e == 0) 1 else b * pow(b, e - 1)
  implicit def num: Numeric[Int] = ???
  val exp = 10
  optiVect { import shallow._
    val vect = Vector.fill(2)(100).map(x => pow(x, exp))
    vect
  }

  import deep._
  trait VectorDSL extends VectorOps with VectorExp

  // Error message examples
  new VectorDSL {
    implicit def inttoRep(i: Int): Rep[Int] = ???
    implicit def dtoRep(i: Double): Rep[Double] = ???
    implicit def snttoRep(i: String): Rep[String] = ???
    implicit def num: Numeric[Int] = ???
    implicit def num1 : Numeric[Double] = ???

    val s: Rep[Vector[String]] = ???
    val x = Vector.range(1,2)
    // x + 1
  }
}
