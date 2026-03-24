// Needs classes to hold terms
// Sealed trait can do matching (switch) in functions with classes
sealed trait Expr

case class Num(n: Int) extends Expr
case class Var(n: String) extends Expr
case class Add(a: Expr, b: Expr) extends Expr
case class Sub(a: Expr, b: Expr) extends Expr
case class Mul(a: Expr, b: Expr) extends Expr
case class Div(a: Expr, b: Expr) extends Expr
case class Pow(a: Expr, b: Expr) extends Expr
case class Neg(n: Expr) extends Expr


