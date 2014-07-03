package virtualizations

// How to make a fix evaluator in both cases.
trait Base {
  type Rep[T] = Exp[T]
  sealed trait Exp[T]
  case class App[T, U](t1: Rep[T=>U], t2: Rep[T]) extends Exp[U]
  case class Lam[T, U](f: Rep[T]=>Rep[U]) extends Exp[T=>U]
  case class Fix[T](f: Rep[T => T]) extends Exp[T]
  case class Const[T](t: T) extends Exp[T]
  case class Let[T, U](t: Rep[T], x: Rep[T] => Rep[U]) extends Exp[U]
}

class HOAS extends Base {
  def _app[T, U](t1: Rep[T => U], t2: Rep[T]): Rep[U] = App(t1, t2)
  def _lam[T, U](x: Rep[T] => Rep[U]): Rep[T=>U] = Lam(x)
  def _lift[T](x: T): Rep[T] = Const(x)
  def _fix[T](f: Rep[T => T]): Rep[T] = Fix(f)
  def _let[T, U](x: Rep[T])(f: Rep[T] => Rep[U]): Rep[U] = Let[T, U](x, f)
  def _ifThenElse[T](c: Rep[Boolean], t: => Rep[T], e: => Rep[T]): Rep[T] = ???
  def _succ(t: Rep[Int]): Rep[Int] = ???

  def eval[T](t: Rep[T]): T = t match {
    case App(t1, t2) => eval(t1) apply (eval(t2))
    case lam: Lam[t, u] => x: t => eval(lam.f(Const(x)))
    case Fix(f) => eval(_app(f, _fix(f)))
    case Const(t) => t
  }

  def fix = eval(_fix(
    _lam((f: Rep[Int => Int]) =>
       _lam((y: Rep[Int]) =>
         _app(f, y)
        )
     )
  ))
  println(eval(_app(_lam((x: Rep[Int]) => x), _lift(1))))
}

object shallow {

 val x = (x: Int) => x

 def foo[T](x: T): T = x
 // Inlining
 def foo[T](x: Rep[T]): Rep[T] = x
 foo((x: Int)=> x)
 foo((x: Rep[Int])=> x) // fails

 // case _lam: Rep[T => U]
 // fails for:
 (x: Int => Int) => x(1)
 // gets transformed
 (x: Rep[Int] => Rep[Int]) => x(1)
}

class HOASInlining extends Base {
  def _app[T, U](t1: Rep[T] => Rep[U], t2: Rep[T]): Rep[U] = t1(t2)
  def _lam[T, U](x: Rep[T] => Rep[U]): Rep[T] => Rep[U] = x
  def _lift[T](x: T): Rep[T] = Const(x)
  def _fix[T](f: Rep[T] => Rep[T]): Rep[T] = ??? // this is equivalent to lam in LMS
  def _let[T, U](x: Rep[T])(f: Rep[T] => Rep[U]): Rep[U] = Let[T, U](x, f)
  def _ifThenElse[T](c: Rep[Boolean], t: => Rep[T], e: => Rep[T]): Rep[T] = ???
  def _succ(t: Rep[Int]): Rep[Int] = ???

 // _let[Int => Int, Int]((x: Rep[Int]) => x)(y => y(1))
  def eval[T](t: Rep[T]): T = t match {
    case App(t1, t2) => eval(t1)(t2)
    case lam: Lam[t, u] => x: t => eval(lam.f(Const(x)))
    case Const(t) => t
  }

  println(eval(_app(_lam((x: Rep[Int]) => x), _lift(2))))
}


object Main extends App {
  (new HOAS).fix(1)
}