package vector

object Main extends App {
  // DSL scope
  def optiVect[T](b: => T) = b
  def pow(b: Int, e: Int): Int = if (e == 0) 1 else b * pow(b, e - 1)
  type Vector[T] = DenseVector[T]

  val exp = 10; val n = 10;
  optiVect {
    if (n > 0) {
     val vect = Vector.range(0, n)
     vect.map(x => pow(x, exp)).sum
    } else -1
  }

 // val exp = 10; val n = 10
 // new VectDSL with PowDSL { def main() = {
 //  __if(hole[Int](n) > lift[Int](-1),{
 //    val v = this.Vector.range(lift[Int](0))(hole[Int]())
 //    vect.map[Int](x: Rep[Int] => pow(x, lift[Int](exp)))
 //  },{
 //    lift[Int](-1)
 //  })}}

}
