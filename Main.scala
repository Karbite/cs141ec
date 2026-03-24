import scala.math._

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

/*
Eval will take in an Expr called n and return an int. It will use
match (switch) statements to go through what n is a class of and
return the right evaluation.
*/
def eval(n: Expr): Int = 
	n match
		case Num(n) => n
		case Var(n) => println("Eval var not allowed")
		case Add(a, b) => eval(a) + eval(b)
		case Sub(a, b) => eval(a) - eval(b)
		case Mul(a, b) => eval(a) * eval(b)
		case Div(a, b) => eval(a) / eval(b)
		case Pow(a, b) => math.pow(eval(a).toDouble, eval(b).toDouble).toInt
		case Neg(n) => -eval(n)


