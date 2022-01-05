// Alambek GUlamidinov
package Hw2

import fastparse._
import MultiLineWhitespace._
import scala.collection.immutable.HashMap 

sealed trait Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, body: Expr, expr: Expr, env: Env) extends Val

case class Env(hashmap: HashMap[Var,Val]) {
  def apply(variable: Var): Val = hashmap(variable)
  def exists(v: Var): Boolean = 
    hashmap.exists((a: (Var, Val)) => a._1 == v)
  def add(v: Var, value: Val) = Env(hashmap + (v -> value))
  
}

sealed trait Program
sealed trait Expr extends Program
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Equal(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class Let(name: Var, value: Expr, body: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr
case class LetRec(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr

sealed trait IntExpr
case class IntConst(n: Int) extends IntExpr
case object IntVar extends IntExpr
case class IntAdd(l: IntExpr, r: IntExpr) extends IntExpr
case class IntSub(l: IntExpr, r: IntExpr) extends IntExpr
case class IntMul(l: IntExpr, r: IntExpr) extends IntExpr
case class IntSigma(f: IntExpr, t: IntExpr, b: IntExpr) extends IntExpr
case class IntPi(f: IntExpr, t: IntExpr, b: IntExpr) extends IntExpr
case class IntPow(b: IntExpr, e: IntExpr) extends IntExpr



package object Hw2 {

  

}

object IntInterpreter {
  def evalInt(expr: IntExpr, env: Option[Int]): Int =
    expr match{
        case IntConst(n) => n
        case IntVar => if(env.isEmpty) throw new Exception("1")
                       else env.get // CHANGE THIS
        case IntAdd(l, r) => (evalInt(l, env), evalInt(r, env)) match  {
             case (x: Int, y: Int) => x + y
             case _ => throw new Exception("Type Error")
        }
        case IntSub(l, r) => (evalInt(l, env), evalInt(r, env)) match  {
             case (x: Int, y: Int) => x - y
             case _ => throw new Exception("Type Error")
        } 
        case IntMul(l, r) => (evalInt(l, env), evalInt(r, env)) match  {
             case (x: Int, y: Int) => x * y
             case _ => throw new Exception("Type Error")
        } 
        case IntSigma(f, t, b) => (evalInt(f, env), evalInt(t, env)) match{
            case (v1: Int, v2: Int) => if(v1 <= v2) {
                                                 evalInt(IntSigma(IntAdd(f, IntConst(1)), t, b), env) + evalInt(b, Some(v1))
                                            }
                                             else 0
            case _ => throw new Exception("Type Error")
        }
        case IntPi(f, t, b) => (evalInt(f, env), evalInt(t, env)) match{
            case (v1: Int, v2: Int) => if(v1 <= v2) {
                                                 evalInt(IntPi(IntAdd(f, IntConst(1)), t, b), env) * evalInt(b, Some(v1))
                                            }
                                            else 1
            case _ => throw new Exception("Type Error")
        }
        case IntPow(b, e) => (evalInt(b, env), evalInt(e, env)) match{
            case (v1: Int, v2: Int) => if(v2 > 0) evalInt(IntPow(b, IntSub(e, IntConst(1))), env) * v1
                                       else  1
            case _ => throw new Exception("Type Error")
        }
    }
  def apply(s: String): Int = {
    val parsed = IntParser(s)
    evalInt(parsed, None)
  }
}

object LetRecInterpreter {
  
  def eval(env: Env, expr: Expr): Val =
    expr match{
        case Const(n) => IntVal(n)
        case Var(s) => if(env.exists(Var(s)))
                          env.apply(Var(s))
                       else throw new Exception("1")
        case Add(l, r) => (eval(env, l), eval(env, r)) match{
              case(x: IntVal, y: IntVal) => IntVal(x.n + y.n)
              case _ => throw new Exception("Type Error")
        }
        case Sub(l, r) => (eval(env, l), eval(env, r)) match{
              case(x: IntVal, y: IntVal) => IntVal(x.n - y.n)
              case _ => throw new Exception("Type Error")
        }
        case Equal(l, r) => BoolVal((eval(env, l) == eval(env, r)))  
        case Iszero(c) => eval(env, c) match{
              case(x: IntVal) => BoolVal (x.n == 0)
              case _ => BoolVal(false)
        }
        case Ite(c, t, f) => eval(env, c) match{
              case v: BoolVal => if(v.b) eval(env, t) else eval(env, f)
              case _ => throw new Exception("Type Error")
        }
        case Let(name, value, body) => { 
              val new_env = env.add(name, eval(env, value))
              eval(new_env, body)
        }
        case Paren(expr: Expr) => eval(env, expr)

        case Proc(v, expr) => {
              ProcVal(v, expr, env)
        }
        case PCall(ftn, arg) => ftn match{
              case Proc(v, expr) => {
                        val new_env = env.add(v, (eval(env, arg)))
                        eval(new_env, expr)
              }
              case LetRec(fname, aname, fbody, ibody) =>{
                         val new_env = env.add(fname, eval(env, fbody)) 
                         new_env.add(aname, eval(new_env, arg))
                         eval(new_env, fbody)
              }
              case _ => throw new Exception ("YOU DIDN'T GIVE ANY INFO DESPITE I ASKED")
        }

    }

  
  def apply(program: String): Val = {
    val parsed = LetRecParserDriver(program)
    eval(Env(new HashMap[Var,Val]()), parsed)
  }

}

object LetRecToString {
  def apply(expr: Expr): String = expr match{
        case Const(n) => n.toString
        case Var(s) => s
        case Add(l, r) => (apply(l) + " + " + apply(r))
        case Sub(l, r) => (apply(l) + " - " + apply(r))
        case Equal(l, r) => (apply(l) + " == " + apply(r))  
        case Iszero(c) =>  ("iszero " + apply(c))
        case Ite(c, t, f) => ("if " + apply(c) + " then " + apply(t) + " else " + apply(f))
        case Let(name, value, body) => ("let " + apply(name) + " = " + apply(value) + " in " + apply(body))
        case Paren(expr: Expr) => ("(" + apply(expr) + ")")

        case Proc(v, expr) => ("proc " + apply(v) + " " + apply(expr))
        case PCall(ftn, arg) => (apply(ftn) + " " + apply(arg))
        case LetRec(fname, aname, fbody, ibody) => ("letrec " + apply(fname) + "(" + apply(aname) + ") = " + apply(fbody) + " in " + apply(ibody))
  }
}

object Hw2App extends App {
  
  println("Hello from Hw2!")

  val int_prog = """x + 1"""
  val parsed = IntParser(int_prog)
  println(parsed)
}
