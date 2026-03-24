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
def eval(e: Expr): Int = 
	e match
		case Num(n) => n
		case Var(n) => println("Eval var not allowed")
		case Add(a, b) => eval(a) + eval(b)
		case Sub(a, b) => eval(a) - eval(b)
		case Mul(a, b) => eval(a) * eval(b)
		case Div(a, b) => eval(a) / eval(b)
		case Pow(a, b) => math.pow(eval(a).toDouble, eval(b).toDouble).toInt
		case Neg(n) => -eval(n)

// Checks for same variables
def same(a: Expr, b: Expr): Boolean =
	a == b

/*
Simplify will just simplify the expression one time by
recursing through all the classes. It will do depth first
search with the first term and backtrack to get the 
simplifications.
*/
def simplify(e: Expr): Expr = 
	e match
		case Num(n) => Num(n)
		case Var(n) => Var(n)
		case Add(a, b) =>
			(simplify(a), simplify(b)) match
				case (Num(0), y) => y
				case (x, Num(0)) => x
				case (Num(x), Num(y)) => Num(x + y)
				case (x, y) => Add(x, y)
		case Sub(a, b) =>
			(simplify(a), simplify(b) match
				case (x, Num(0)) => x
				case (Num(0), y) => y
				case (Num(x), Num(y)) => Num(x - y)
				case (x, Neg(y)) => Add(x, y)
				case (x, y) =>
					if same(x, y) then Num(0)
					else Sub(x, y)
		case Mul(a, b) =>
			(simplify(a), simplify(b)) match
				case (Num(0), _) => Num(0)
				case (_, Num(0)) => Num(0)
				case (Num(x), Num(x)) => Num(x * y)
				case (Num(1), y) => y
				case (x, Num(1)) => x
				case (Num(-1), y) => Neg(y)
				case (x, Num(-1)) => Neg(x)
				case (x, y) => Mul(x, y)
		case Div(a, b) =>
			(simplify(a), simplify(b)) match
				case (Num(0), _) => Num(0)
				case (_, Num(0)) => println("Not possible dividing by 0")
				case (Num(x), Num(y)) => Num(x / y)
				case (x, Num(1)) => x
				case (Neg(x), y) => Neg(Div(x, y))
				case (x, Neg(y)) => Neg(Div(x, y))
				case (x, y) =>
					if same(x, y) then Num(1)
					else Neg(x, y)
		case Pow(a, b) =>
			(simplify(a), simplify(b)) match
				case (_, Num(0)) => Num(1)
				case (Num(0), _) => Num(0)
				case (x, Num(1)) => x
				case (Num(x), Num(y)) => Num(math.pow(x.toDouble, y.toDouble).toInt)
				case (x, y) => Pow(x, y)
		case Neg(n) =>
			simplify(n) match
				case Num(a) => Num(-a) // Number
				case Neg(a) => simplify(a) // Cancel out negative and recurse
				case a => Neg(a) // Variable


def simplifyRecurse(e: Expr): Expr =
	val temp = simplify(e)

	// Needs to keep simplifying until no more simplifications
	if temp == e then // Expression is the same due to no more simplifications
		e
	else // Still found a simplification
		simplify(temp)


