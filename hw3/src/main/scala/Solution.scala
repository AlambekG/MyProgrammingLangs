package hw3

import scala.collection.immutable.HashMap 
import hw3._


package object hw3 {
  type Env = HashMap[Var,Val]
  type Loc = Int
  
}

case class Mem(m: HashMap[Loc,Val], top: Loc) {
  def extended(v: Val): (Mem, LocVal) = {
    val new_mem = Mem(m.updated(top,v), top+1)
    (new_mem,LocVal(top))
  }
  def updated(l: LocVal, new_val: Val): Option[Mem] = {
    m.get(l.l) match {
      case Some(v) => Some(Mem(m.updated(l.l, new_val), top))
      case None => None
    }
  }
  def get(l: LocVal): Option[Val] = m.get(l.l)
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

  def eval(env: Env, mem: Mem, expr: Expr): Result = expr match {
    case ConstI(n) => Result(IntVal(n),mem)
    case ConstB(n) => Result(BoolVal(n),mem)
    case ConstIL(n) => Result(IntListVal(n),mem)
    case Var(s) => env.get(Var(s)) match {
      case Some(v) => v match {
        case r: LocVal => mem.get(r) match {
          case Some(v) => Result(v,mem)
          case _ => throw new UndefinedSemantics(s"location ${r} for variable ${s} not allocated")
        }
        case _ => Result(v,mem)
      }
      case None => throw new UndefinedSemantics(s"variable ${s} undefined")
    }
    case Add(a,b) => {
      val res1 = eval(env,mem,a)
      val res2 = eval(env,res1.m, b)
      (res1.v, res2.v) match {
        case (v1: IntVal, v2: IntVal) => Result(IntVal(v1.n + v2.n), res2.m)
        case _ => throw new UndefinedSemantics("Can only add one integer from another!")
      }
    }
    case Sub(a,b) => {
      val res1 = eval(env,mem,a)
      val res2 = eval(env,res1.m, b)
      (res1.v, res2.v) match {
        case (v1: IntVal, v2: IntVal) => Result(IntVal(v1.n - v2.n), res2.m)
        case _ => throw new UndefinedSemantics("Can only sub one integer from another!")
      }
    }
    case Mul(a,b) => {
      val res1 = eval(env,mem,a)
      val res2 = eval(env,res1.m, b)
      (res1.v, res2.v) match {
        case (v1: IntVal, v2: IntVal) => Result(IntVal(v1.n * v2.n), res2.m)
        case _ => throw new UndefinedSemantics("Can only mul one integer from another!")
      }
    }
    case Div(a,b) => {
      val res1 = eval(env,mem,a)
      val res2 = eval(env,res1.m, b)
      (res1.v, res2.v) match {
        case (_, IntVal(0)) => throw new UndefinedSemantics("Divide by zero")
        case (v1: IntVal, v2: IntVal) => Result(IntVal(v1.n / v2.n), res2.m)
        case _ => throw new UndefinedSemantics("Can only div one integer from another!")
      }
    }
    case Rem(a,b) => {
      val res1 = eval(env,mem,a)
      val res2 = eval(env,res1.m, b)
      (res1.v, res2.v) match {
        case (_, IntVal(0)) => throw new UndefinedSemantics("Remainder by zero")
        case (v1: IntVal, v2: IntVal) => Result(IntVal(v1.n % v2.n), res2.m)
        case _ => throw new UndefinedSemantics("Can only rem one integer from another!")
      }
    }
    case Cons(a,b) => {
      val res1 = eval(env,mem,a)
      val res2 = eval(env,res1.m, b)
      (res1.v, res2.v) match {
        case (v1: IntVal, v2: IntListVal) => Result(IntListVal(v1 :: v2.n), res2.m)
        case _ => throw new UndefinedSemantics("Can only cons with one integer and integer list!")
      }
    }
    case GTExpr(a,b) => {
      val res1 = eval(env,mem,a)
      val res2 = eval(env,res1.m, b)
      (res1.v, res2.v) match {
        case (v1: IntVal, v2: IntVal) => Result(BoolVal(v1.n > v2.n), res2.m)
        case _ => throw new UndefinedSemantics("Can only gt one integer from another!")
      }
    }
    case GEQExpr(a,b) => {
      val res1 = eval(env,mem,a)
      val res2 = eval(env,res1.m, b)
      (res1.v, res2.v) match {
        case (v1: IntVal, v2: IntVal) => Result(BoolVal(v1.n >= v2.n), res2.m)
        case _ => throw new UndefinedSemantics("Can only geq one integer from another!")
      }
    }
    case Iszero(c) => {
      val res = eval(env,mem,c)
      res.v match {
      case x: IntVal => Result(BoolVal(x.n == 0), res.m)
      case _ => throw new UndefinedSemantics("Can only iszero with an integer!")
      }
    }
    case Ite(c, t, f) => eval(env,mem,c) match {
      case Result(v: BoolVal,m) => if (v.b) eval(env,m,t) else eval(env,m,f)
      case _ => throw new UndefinedSemantics("Type Error: condition should be Boolean")
    }
    case ValExpr(name, value, body) => {
      val res = eval(env,mem,value)
      val new_env = env.updated(name, res.v)
      eval(new_env, res.m, body)
    }
    case VarExpr(name, value, body) => {
      val res = eval(env,mem,value)
      val e = res.m.extended(res.v)
      val new_mem = e._1
      val loc = e._2
      val new_env = env.updated(name, e._2)
      eval(new_env, new_mem, body)
    }

    case Paren(expr: Expr) => eval(env,mem,expr)
    case Proc(v,e) => Result(ProcVal(v,e,env),mem)
    case PCall(f,e) => {
      val proc = eval(env,mem,f)
      val arg = eval(env,proc.m,e)
      
      proc.v match {
        case p: ProcVal => {
          val new_env = p.env.updated(p.v, arg.v)
          eval(new_env, arg.m, p.expr)  
        }
        case rp: RecProcVal => {
          val new_env = rp.env.updated(rp.fv, rp).updated(rp.av, arg.v)
          eval(new_env, arg.m, rp.body)
        }
        case _ => throw new Exception("Type Error")
      }
    }
    case DefExpr(fv,av,body,expr) => {
      val rp = RecProcVal(fv,av,body,env)
      val new_env = env.updated(fv, rp)
      eval(new_env,mem,expr)
    }
    case Block(f,s) => {
      val res = eval(env,mem,f)
      eval(env,res.m,s)
    }
    case Asn(n,e) => {
      val res = eval(env,mem,e)
      env.get(n) match {
        case Some(v: LocVal) => res.m.updated(v,res.v) match {
          case Some(new_mem) => Result(res.v, new_mem)
          case _ => throw new UndefinedSemantics(s"location ${v} for variable ${n} not allocated")
        }
        case Some(_) => throw new UndefinedSemantics(s"variable ${n} is not mutable")
        case None => throw new UndefinedSemantics(s"variable ${n} is not defined")
      }
    }
    case _ => throw new Exception(s"unsupported expr: ${expr}")
  }
  
  
  def apply(program: String): Val = {
    val parsed = MiniScalaParserDriver(program)
    eval(new Env(), Mem(new HashMap[Loc,Val],0), parsed).v
  }

}


object Hw3App extends App {
  
  println("Hello from Hw3!")

}