trait Numbers {
	type Number = Double
}

// trait TupleOps {
// 	type Tuple[T, U] = scala.Tuple2[T, U]
// }

trait VectorOps {
	self: Numbers => 
  type Vector[T] = scala.collection.immutable.Vector[T]

  def zip[T](v1: Vector[T], v2: Vector[T]) = v1.zip(v2)
  def addV(v1: Vector[Number], v2: Vector[Number]) = v1.zip(v2).map(x => x._1 + x._2)  
}

// snippet
trait MatrixOps {
  self: VectorOps with Numbers  => 
  type Matrix[T] = Vector[Vector[T]]
  
  def add(m1: Matrix[Number], m2: Matrix[Number]) =
  	zip(m1, m2).map(x => addV(x._1, x._2))
}
// end of snippet

object Test extends App with MatrixOps with VectorOps with Numbers {
   println(add(Vector(Vector(1),Vector(2),Vector(3)), Vector(Vector(1),Vector(2),Vector(3))))	
}