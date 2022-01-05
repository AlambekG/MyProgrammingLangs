//Alambek Gulamidinov StudentID_20192012

package hw3

import scala.collection.immutable.HashMap 
import hw3._


package object hw3 {
  type Env = HashMap[Var,Val]
  type Loc = Int
}

case class Mem(m: HashMap[Loc,Val], top: Loc) {
     def add(value: Val) = {
         val mm = Mem(m.updated(top,value), top + 1)
         (mm, LocVal(top))
     }
     def get(loc: LocVal) = m.get(loc.l)
     def dd():Loc = top
     def ff():HashMap[Loc,Val] = m 
}

sealed trait Val
case class IntVal(n: Int) extends Val
case class IntListVal(n: List[IntVal]) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, body: Expr, env: Env) extends Val
case class LocVal(l: Loc) extends Val


sealed trait Program
sealed trait Expr extends Program
case class ConstI(n: Int) extends Expr
case class ConstB(n: Boolean) extends Expr
case class ConstIL(n: List[IntVal]) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class Rem(l: Expr, r: Expr) extends Expr
case class Cons(l: Expr, r: Expr) extends Expr
case class GTExpr(l: Expr, r: Expr) extends Expr
case class GEQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class ValExpr(name: Var, value: Expr, body: Expr) extends Expr
case class VarExpr(name: Var, value: Expr, body: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class DefExpr(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr







object MiniScalaInterpreter {

  case class Result(v: Val, m: Mem)
  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)
  
  
  def eval(env: Env, mem: Mem, expr: Expr): Result = expr match{
          
          case ConstI(n) => Result(IntVal(n), mem)
          case ConstB(n) => Result(BoolVal(n), mem)
          case ConstIL(n) => Result(IntListVal(n), mem)
          case Var(s) => // mem check
                          env.get(Var(s)) match{
                               case Some(v) => v match{
                                     case a: LocVal => mem.get(a) match{
                                        case Some(v) => Result(v, mem)
                                        case _ => throw new UndefinedSemantics(s"Error1")
                                     }
                                     case _ => Result(v, mem)
                               }
                               case None => throw new UndefinedSemantics(s"Error1")
                          }
          case Add(l, r) =>{
                val x = eval(env, mem, l)
                val y = eval(env, x.m, r)
                (x.v, y.v) match{
                     case (a: IntVal, b: IntVal) => Result(IntVal(a.n + b.n), y.m)
                     case _ => throw new UndefinedSemantics(s"Can't add this expression")
                }
          }
          case Sub(l, r) =>{
                val x = eval(env, mem, l)
                val y = eval(env, x.m, r)
                (x.v, y.v) match{
                     case (a: IntVal, b: IntVal) => Result(IntVal(a.n - b.n), y.m)
                     case _ => throw new UndefinedSemantics(s"Can't subtract this expression")
                }
          }
          case Mul(l, r) =>{
                val x = eval(env, mem, l)
                val y = eval(env, x.m, r)
                (x.v, y.v) match{
                     case (a: IntVal, b: IntVal) => Result(IntVal(a.n * b.n), y.m)
                     case _ => throw new UndefinedSemantics(s"Can't multiply this expression")
                }
          }
          case Div(l, r) =>{
                val x = eval(env, mem, l)
                val y = eval(env, x.m, r)
                if(y.v == IntVal(0)) throw new UndefinedSemantics(s"Cant' divide by 0")
                (x.v, y.v) match{
                     case (a: IntVal, b: IntVal) => Result(IntVal(a.n / b.n), y.m)
                     case _ => throw new UndefinedSemantics(s"Can't divide this expression")
                }
          }
          case Rem(l, r) =>{
                val x = eval(env, mem, l)
                val y = eval(env, x.m, r)
                if(y.v == IntVal(0)) throw new UndefinedSemantics(s"Cant' take remainder from MOD 0")
                (x.v, y.v) match{
                     case (a: IntVal, b: IntVal) => Result(IntVal(a.n % b.n), y.m)
                     case _ => throw new UndefinedSemantics(s"Can't find remainder in this expression")
                }
          }
          case Cons(l, r) =>{
                val x = eval(env, mem, l)
                val y = eval(env, x.m, r)
                (x.v, y.v) match{
                     case (a: IntVal, b: IntListVal) => Result(IntListVal(a::b.n), y.m)
                     case _ => throw new UndefinedSemantics(s"Can't make  Cons operation")
                }
          }
          case GTExpr(l, r) =>{
                val x = eval(env, mem, l)
                val y = eval(env, x.m, r)
                (x.v, y.v) match{
                     case (a: IntVal, b: IntVal) => Result(BoolVal(a.n > b.n), y.m)
                     case _ => throw new UndefinedSemantics(s"Can't compare this expression")
                }
          }
          case GEQExpr(l, r) =>{
                val x = eval(env, mem, l)
                val y = eval(env, x.m, r)
                (x.v, y.v) match{
                     case (a: IntVal, b: IntVal) => Result(BoolVal(a.n >= b.n), y.m)
                     case _ => throw new UndefinedSemantics(s"Can't compare this expression")
                }
          }
          case Iszero(c) => {
               val x = eval(env, mem, c);
               x.v match{
                    case (a:IntVal) => Result(BoolVal(a.n == 0), x.m)
                    case _ => throw new UndefinedSemantics(s"Can't compare to zero")
               }
          }
          case Ite(c, t, f) => {
                val x = eval(env, mem, c);
                x.v match{
                    case (a:BoolVal) => if(a.b == true) eval(env, x.m, t)
                                        else eval(env, x.m, f)
                    case _ => throw new UndefinedSemantics(s"Not Boolean type")
                }
                
          }
          case ValExpr(name, value, body) => {
                   val x = eval(env, mem, value);
                   val new_env = env.updated(name, x.v);
                   eval(new_env, x.m, body);
          }
          case VarExpr(name, value, body) => {
                   val x = eval(env, mem, value);
                   val a = x.m.add(x.v)
                   val new_env = env.updated(name, a._2)
                   eval(new_env, a._1, body);
          }
          
          case Proc(v, expr) => {
                Result(ProcVal(v, expr, env), mem)
          }

          case DefExpr(fname, aname, fbody, ibody) => {
                //println("CHECK")
                val x = RecProcVal(fname, aname, fbody, env)
                val new_env = env.updated(fname, x)
                //println(ibody)
                val y = eval(new_env, mem, ibody)
                //println("LOOKTHERE")
                //println(y)
                y
          }

          case Asn(v, e) => {
               val x = eval(env, mem, e)
               env.get(v) match {
                  case Some(a: LocVal) => Result(x.v, Mem(x.m.ff.updated(a.l, x.v),x.m.dd)  )
                  case _ => throw new UndefinedSemantics(s"Error5")
               }
          }

          case Paren(expr: Expr) => eval(env, mem, expr)
          
          case Block(f, s) => {
              val x = eval(env, mem, f)
              eval(env, x.m, s)
          }
          
          case PCall(ftn, arg) => {
                val x = eval(env, mem, ftn)
                x.v match{
                  case a:ProcVal => {
                              val y = eval(env, x.m, arg) 
                              val new_env = a.env.updated(a.v, y.v)
                              eval(new_env, y.m, a.expr)
                  }
                  case a:RecProcVal =>{
                              //println("YOU ARE THERE")
                              val y = eval(env, x.m, arg) 
                              val new_env = env.updated(a.av, y.v)
                              //println(new_env)
                              new_env.updated(a.fv, a)
                              //println(new_env)
                              
                              eval(new_env, y.m, a.body)
                  }
                  case _ => throw new UndefinedSemantics(s"Error4")
               }
              
          }          
          case _ => throw new UndefinedSemantics(s"There is no such case")
  }
  
  def apply(program: String): Val = {
    val parsed = MiniScalaParserDriver(program)
   // println(parsed)
    eval(new Env(), Mem(new HashMap[Loc,Val],0), parsed).v
  }

}


object Hw3App extends App {
  
  println("Hello from Hw3!")

}