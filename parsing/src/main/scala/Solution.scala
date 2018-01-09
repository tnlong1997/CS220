import hw.parsing._
import scala.util.parsing.combinator._

object ArithEval extends ArithEvalLike {
	def eval (e : Expr) : Double = {
		e match {
			case Num(n) => n
			case Add(n1, n2) => eval(n1) + eval(n2)
			case Sub(n1, n2) => eval(n1) - eval(n2)
			case Mul(n1, n2) => eval(n1) * eval(n2)
			case Div(n1, n2) => eval(n1) / eval(n2)
			case Exponent(n1, n2) => Math.pow(eval(n1), eval(n2))
		}
	}
}

object ArithParser extends ArithParserLike {
	lazy val expr : PackratParser [ Expr ] = add
	lazy val exponent : PackratParser[Expr] = 
		exponent ~ "^" ~ atom ^^ {
			case e ~_~ a => Exponent(e,a)
		} | atom
	lazy val mul : PackratParser [ Expr ] = 
		mul ~ "*" ~ exponent ^^ { 
			case m ~ _ ~ e => Mul(m,e)
		} | mul ~ "/" ~ exponent ^^ { 
			case m ~ _ ~ e => Div(m,e)
		} | exponent
	lazy val add : PackratParser [ Expr ] = 
		add ~ "+" ~ mul ^^ { 
			case a ~ _ ~ m => Add(a,m)
		} | add ~ "-" ~ mul ^^ { 
			case a ~ _ ~ m => Sub(a,m)
		} | mul
	lazy val atom : PackratParser [ Expr ] = 
		number ^^ { 
			case n => Num(n)
		} | "(" ~>expr<~ ")"
}

object ArithPrinter extends ArithPrinterLike {
	def print (e: Expr ): String = {
		e match{
			case Num(n) => n.toString
			case Add(n1, n2) => print(n1).toString + "+" + print(n2).toString
			case Sub(n1, n2) => print(n1).toString + "-" + print(n2).toString
			case Mul(n1, n2) => print(n1).toString + "*" + print(n2).toString
			case Div(n1, n2) => print(n1).toString + "/" + print(n2).toString
			case Exponent(n1, n2) => print(n2).toString + "*" + print(n2).toString
		}
	}
}