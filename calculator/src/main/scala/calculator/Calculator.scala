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
    namedExpressions.mapValues( (sig) => {
        Signal(eval(sig(), namedExpressions))
    })
    
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def _eval(expr: Expr, refs: Map[String, Signal[Expr]], refsStack: Set[String]): Double = {      
      expr match {
        case Literal(v) => v
        case Plus(a, b) => _eval(a, refs, refsStack) + _eval(b, refs, refsStack)
        case Minus(a, b) => _eval(a, refs, refsStack) - _eval(b, refs, refsStack)
        case Times(a, b) => _eval(a, refs, refsStack) * _eval(b, refs, refsStack)
        case Divide(a, b) => _eval(a, refs, refsStack) / _eval(b, refs, refsStack)
        case Ref(name) => {
          if ((refsStack contains name) || ! (refs contains name)) Double.NaN
          else  _eval(refs(name)(), refs, refsStack + name)
        }
      }
    }
    _eval(expr, references, Set())
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
