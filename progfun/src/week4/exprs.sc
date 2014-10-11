package week4

object exprs {

  def showInParens(e: Expr): String = e match {
    case Sum(_, _) => "(" + show(e) + ")"
    case _ => show(e)
  }                                               //> showInParens: (e: week4.Expr)String

  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
    case Prod(l, r) => showInParens(l) + " * " + showInParens(r)
    case Var(x) => x
  }                                               //> show: (e: week4.Expr)String

  show(Sum(Number(1), Number(4)))                 //> res0: String = 1 + 4
  show(Sum(Prod(Number(2), Var("x")), Var("y")))  //> res1: String = 2 * x + y
  show(Prod(Sum(Number(2), Var("x")), Var("y")))  //> res2: String = (2 + x) * y
}