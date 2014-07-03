package a
import language.experimental.macros
import reflect.macros.Context

object Pow {
  def evalPow(expr: Double): Double = macro _evalPowImpl
  def _evalPowImpl(c: Context)
    (expr: c.Expr[Double]): c.Expr[Double] = {
    import c.universe._
    println(showRaw(expr.tree))
    expr.tree match {
      case q"scala.math.`package`.pow($b, $e)" =>
        c.Expr(q"${c.eval[Double](expr)}")
      case _ =>
        println("Missed!!!")
        expr
    }
  }
}