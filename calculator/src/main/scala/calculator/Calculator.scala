package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.transform((a, b) => Signal(eval(b.apply(), List(a), namedExpressions))).toMap
  }

  def eval(expr: Expr, escape: List[String], references: Map[String, Signal[Expr]]): Double = {
    def applyOperation(func: (Double, Double) => Double, opOne: Expr, opTwo: Expr) = {
      val leftOp = eval(opOne, escape, references)
      val rightOp = eval(opTwo, escape, references)
      if (leftOp != Double.NaN || rightOp != Double.NaN) {
        func(leftOp, rightOp)
      } else {
        Double.NaN
      }
    }

    expr match {
      case Literal(v) => v
      case Ref(name) => {
        if (escape.contains(name)) {
          Double.NaN
        } else {
          val refExp = getReferenceExpr(name, references)
//          references.updated(name, eval(refExp, name :: escape, references))
          eval(refExp, name :: escape, references)
        }
      }
      case Plus(a, b) => {
        applyOperation((x, y) => x + y, a, b)
      }
      case Minus(a, b) => {
        applyOperation((x, y) => x - y, a, b)
      }
      case Times(a, b) => {
        applyOperation((x, y) => x * y, a, b)
      }
      case Divide(a, b) => {
        applyOperation((x, y) => if (y != 0) x / y else Double.NaN, a, b)
      }
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
