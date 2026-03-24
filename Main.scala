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

var input = ""
var pos = 0

/*
Parsing order should go from (+, -) to (*, /) to pow then to unary
to (numbers, variables, paren)
*/
def parse(s: String): Expr =
	input = input.replaceAll("\\s+", "")
	parse1() // Needs to go from lowest to highest precedence

def parse1(): Expr = // Does + and - first
	val left = parse2() // Needs to get left first
	var result = left

	while pos < input.length && (input(pos) == '+' || input(pos) == '-') do
		val term = input(pos) // Get + or - term
		pos += 1
		val right = parseTerm()

		if term == '+' then
			result = Add(left, right)
		else
			result = Sub(left, right)

	result

def parse2(): Expr = // Does * and /
	val left = parse3()
	var result = left

	while pos < input.length && (input(pos) == '*' || input(pos) == '/') do
		val term = input(pos)
		pos += 1
		val right = parse3()

		if term == '*' then
			result = Mul(left, right)
		then
			result = Div(left, right)

	result

def parse3(): Expr = // Does pow
	val left = parse4()
	var result = left

	while pos < input.length && input(pos) == '^' then 
		// val term = input(pos) 	Not needed, already know
		pos += 1
		val right = parsePower()
		result = Pow(left, right)
	
	result

def parse4(): Expr = // Does unary
	while pos < input.length && input(pos) == '-' then
		pos += 1
		Neg(parse4())
	
	parse5()

def parseNum(): Expr =
	val start = pos

	while pos < input.length && input(pos).isDigit do
		pos += 1
	
	Num(input.substring(start, pos).toInt) // Use substring to get full num

def parseVar(): Expr =
	val start = pos

	while pos < input.length && input(pos).isLetter do
		pos += 1
	
	Var(input.substring(start, pos))

def parse5(): Expr = // Does num, var, and paren
	if pos >= input.length then
		throw new RuntimeException("Out of bounds")
	
	val temp = input(pos)

	if temp.isDigit then
		parseNum()
	else if temp.isLetter then
		parseVar()
	else if temp == '(' then
		pos += 1

		val insideExpr = parse1() // Go back to beginning
		// Pos will be incremented in call so need to check if
		// there is an ending parentheses using pos
		if pos >= input.length || input(pos) == ')' then
			throw new RuntimeException("No ending )")

		pos += 1
		insideExpr
	else
		throw new RuntimeException("Input is not accepted")

/*
Eval will take in an Expr called n and return an int. It will use
match (switch) statements to go through what n is a class of and
return the right evaluation.
*/
def eval(e: Expr): Int = 
	e match
		case Num(n) => n
		case Var(n) => throw new RuntimeException("Eval var not allowed")
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
			(simplify(a), simplify(b)) match
				case (x, Num(0)) => x
				case (Num(0), y) => Neg(y)
				case (Num(x), Num(y)) => Num(x - y)
				case (x, Neg(y)) => Add(x, y)
				case (x, y) =>
					if same(x, y) then Num(0)
					else Sub(x, y)
		case Mul(a, b) =>
			(simplify(a), simplify(b)) match
				case (Num(0), _) => Num(0)
				case (_, Num(0)) => Num(0)
				case (Num(x), Num(y)) => Num(x * y)
				case (Num(1), y) => y
				case (x, Num(1)) => x
				case (Num(-1), y) => Neg(y)
				case (x, Num(-1)) => Neg(x)
				case (x, y) => Mul(x, y)
		case Div(a, b) =>
			(simplify(a), simplify(b)) match
				case (Num(0), _) => Num(0)
				case (_, Num(0)) => throw new RuntimeException("Not possible dividing by 0")
				case (Num(x), Num(y)) => Num(x / y)
				case (x, Num(1)) => x
				case (Neg(x), y) => Neg(Div(x, y))
				case (x, Neg(y)) => Neg(Div(x, y))
				case (x, y) =>
					if same(x, y) then Num(1)
					else Div(x, y)
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
		simplifyRecurse(temp)

def deriv(e: Expr): Expr = 
	e match
		case Num(_) => Num(0)
		case Var("x") => Num(1)
		case Var(_) => Num(0)
		case Neg(x) => Neg(deriv(x))
		case Add(a, b) =>
			Add(deriv(a), deriv(b))
		case Sub(a, b) =>
			Sub(deriv(a), deriv(b))
		case Mul(a, b) => // u'v + uv'
			Add(Mul(deriv(a), b), Mul(a, deriv(b)))
		case Div(a, b) => // (u'v - uv') / v^2
			Div(Sub(Mul(deriv(a), b), Mul(a, deriv(b))), Pow(b, Num(2)))
		case Pow(x, Num(b)) => // ax^b -> a * b * derivation of x * x^(b-1)
			Mul(Num(b), Mul(deriv(x), Pow(x, Num(b - 1))))
		case Pow(_, _) => throw new RuntimeException("Only constant exponents")

def derivation(e: Expr): Expr =
	val temp = simplifyRecurse(e) // Simplify first for easier derivation

	simplifyRecurse(deriv(temp)) // Derive and then simplify the derivation

case class Guest(name: String, isFemale: Boolean, langs: Set[String])

def sameLang(a: Guest, b: Guest): Boolean =
	a.langs.exists(lang => b.langs.contains(lang))

def notFemales(a: Guest, b: Guest): Boolean =
	!(a.isFemale && b.isFemale)

def getPermutations(current: List[Guest], rest: List[Guest]): List[Guest] = 
	if rest.isEmpty then
		if (sameLang(current.head, current.last) &&
		    notFemales(current.head, current.last)) then
			current
		else
			List()
	else
		var i = 0 // Needs to be mutable
		while (i < rest.length) do
			val temp = rest(i)

			if (sameLang(current.last, temp) &&
				notFemales(current.last, temp)) then
				val newCurrent = current ++ List(temp)
				val newRest = rest.slice(0, i) ++ rest.slice(i + 1, rest.length)
				val result = getPermutations(newCurrent, newRest)

				if result.nonEmpty then 
					return result

			i += 1

		List()

def getNames(guests: List[Guest]): List[String] =
	var names = List[String]()
	var i = 0

	while (i < guests.length) do
		names = names ++ List(guests(i).name)
		i += 1
	
	names

def partySeating(guests: List[Guest]): List[String] =
	var i = 0

	while (i < guests.length) do
		val start = guests(i)
		val rest = guests.slice(0, i) ++ guests.slice(i + 1, guests.length)
		val result = getPermutations(List(start), rest)

		if result.nonEmpty then
			return getNames(result)

	List()
