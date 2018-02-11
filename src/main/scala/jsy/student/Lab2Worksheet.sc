/*
 * CSCI 3155: Lab 2 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab2.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab2.Parser.parse

// Imports the ast nodes
import jsy.lab2.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab2._

// Call the JavaScripty parser (from the provided library) on a string
val negFourAST = parse("-4")

// Evaluate that JavaScripty expression.
//eval(negFourAST)

// For convenience, we also have an eval function that takes a string,
// which calls the parser and then delegates to your eval function.
//eval("undefined + 1")


def toNumber2(v: Expr): Double = {
  require(isValue(v))
  (v: @unchecked) match {
    case N(n) => n
    case B(b) => if (b) 1 else 0
    case S(str) => try
      {
        str.toDouble
      } catch {
      case e: Exception => Double.NaN
      }
    case _ => Double.NaN
  }
}

//val tmp0= toNumber("10")
val tmp1=B(true)
toNumber2(v = tmp1)
//val tmp2=Var("2")
//toNumber(tmp2)

val test="2"
val test2 = S("2")
toNumber2(test2)
val e = Undefined
toNumber2(e)

val word="True"
word.toLowerCase


val empty1=""
val empty2=" "

empty1.isEmpty
val inf_lower="infinity"
val inf_upper="Infinity"
toNumber2(S(inf_lower))
toNumber2(S(inf_upper))

val dict: Env = Map()

val e1 = N(3)
val e2 = Binary(Plus, Var("x"), N(1))
val e3 = eval(ConstDecl("x", e1, e2))

val e4 = extend(Map(),"x",e1)



//val e5 = eval(e4,eval(e2))
//e3 === N(4))

val f1 = parse("-1/0")
val f2 = eval(f1)