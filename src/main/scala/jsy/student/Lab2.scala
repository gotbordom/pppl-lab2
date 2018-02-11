package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * <Anthony Tracy>
   * 
   * Partner: <Unknown at this Time>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(b) => if (b) 1 else 0
      case S(str) => try str.toDouble catch {case _ : Throwable => Double.NaN}
      case _ => Double.NaN
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => if (n==0) false else true
      case S(s) => if (s.isEmpty) true else false
      case _ => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case _ => pretty(v)
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(n) => N(n)
      case B(b) => B(b)
      case S(s) => S(s)
      case Var(x) => try lookup(env,x) catch {case _ : Throwable => Undefined}
      case Undefined => Undefined
      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      case ConstDecl(s,e1,e2) => eval(extend(env,s,eval(env,e1)),e2)
      case If(e1,e2,e3) => if(toBoolean(eval(env,e1))) eval(env,e2) else eval(env,e3)

      // Looking at all Binary cases:
      case Binary(bop,e1,e2) =>
        // Match for all possible BinaryOp cases:
        bop match {
          case Plus =>
            // match for what type of expressions you have:
            (e1,e2) match {
              // Anything Plus string concatonates:
              case (S(s),_) => S(s+toStr(eval(env,e2)))
              case (_,S(s)) => S(toStr(eval(env,e1))+s)
              // Any boolean plus number is a number:
              case (_,_) => N(toNumber(eval(env,e1))+toNumber(eval(env,e2)))
            }
          case Minus =>
            (e1,e2) match {
               // Strings act like standerd subtraction <- though the NaNs are a pain due to words...
              case (S(s),_) => N(toNumber(eval(env,S(s))) - toNumber(eval(env,e2)))
              case (_,S(s)) => N(toNumber(eval(env,e1)) - toNumber(eval(env,S(s))))
              case (_,_) => N(toNumber(eval(env,e1))-toNumber(eval(env,e2)))
            }
          case Times =>
            (e1,e2) match {
              // Strings act the same as they would in minus... a number acts like a number and a word becomes NaN
              case (S(s),_) => N(toNumber(eval(env,S(s))) * toNumber(eval(env,e2)))
              case (_,S(s)) => N(toNumber(eval(env,e1)) * toNumber(eval(env,S(s))))
              case (_,_) => N(toNumber(eval(env,e1))*toNumber(eval(env,e2)))
            }
          case Div =>
            // This has a weird case "Infinity" or 0... Which scala seems to have a double = Infinity so it seems fine
            (e1,e2) match {
              // Aside from the previous comment about this section, it all works the same as mult and sub.
              case (S(s),_) => N(toNumber(eval(env,S(s))) / toNumber(eval(env,e2)))
              case (_,S(s)) => N(toNumber(eval(env,e1)) / toNumber(eval(env,S(s))))
              case (_,_) => N(toNumber(eval(env,e1))/toNumber(eval(env,e2)))
            }
          // Note about this... the notes say to use === not ==, which are very different in both scala and Javascript's node js
          case Eq => B(toNumber(eval(env,e1)) == toNumber(eval(env,e2)))    // THere will be an error here if I get two words that are different ...
          case Ne => B(toNumber(eval(env,e1)) != toNumber(eval(env,e2)))
          case Lt => B(toNumber(eval(env,e1)) < toNumber(eval(env,e2)))
          case Le => B(toNumber(eval(env,e1)) <= toNumber(eval(env,e2)))
          case Gt => B(toNumber(eval(env,e1)) > toNumber(eval(env,e2)))
          case Ge => B(toNumber(eval(env,e1)) >= toNumber(eval(env,e2)))
          // This is a really weird opperator in javascript...
          case And => if (toBoolean(eval(env,e1))) eval(env,e2) else eval(env,e1)
          case Or => if (!toBoolean(eval(env,e1))) eval(env,e2) else eval(env,e1)
          case Seq => eval(env,e1); eval(env,e2)
          //case _ => Undefined
        }
      // Unary operations:
      case Unary(uop,e1) =>
        uop match {
          case Neg => N(-1*toNumber(eval(env,e1)))
          case Not => B(!toBoolean(eval(env,e1)))
        }
      case _ => Undefined
    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}
