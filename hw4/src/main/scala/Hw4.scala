// Alambek Gulamidinov 20192012

package hw4

import scala.collection.immutable.HashMap 
import hw4._


package object hw4 {
  type Env = HashMap[Var,LocVal]
}

case class Mem(m: HashMap[LocVal,Val], top: Int) {
  
  def extended(v: Val): (Mem, LocVal) = {
    val new_mem = Mem(m.updated(LocVal(top),v), top+1)
    (new_mem,LocVal(top))
  }
  def updated(l: LocVal, new_val: Val): Option[Mem] = {
    m.get(l) match {
      case Some(v) => Some(Mem(m.updated(l, new_val), top))
      case None => None
    }
  }
  def get(l: LocVal): Option[Val] = m.get(l)
  def getLocs(): List[LocVal] = m.keySet.toList
}

sealed trait Val
case object SkipVal extends Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(args: List[Var], expr: Expr, env: Env) extends Val
case class LocVal(l: Int) extends Val
sealed trait RecordValLike extends Val
case object EmptyRecordVal extends RecordValLike
case class RecordVal(field: Var, loc: LocVal, next: RecordValLike) extends RecordValLike


sealed trait Program
sealed trait Expr extends Program
case object Skip extends Expr
case object False extends Expr
case object True extends Expr
case class NotExpr(expr: Expr) extends Expr
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr {
  override def toString = s"Var(${"\""}${s}${"\""})"
}
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class LTEExpr(l: Expr, r: Expr) extends Expr
case class EQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class Let(i: Var, v: Expr, body: Expr) extends Expr
case class Proc(args: List[Var], expr: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class BeginEnd(expr: Expr) extends Expr
case class FieldAccess(record: Expr, field: Var) extends Expr
case class FieldAssign(record: Expr, field: Var, new_val: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCallV(ftn: Expr, arg: List[Expr]) extends Expr
case class PCallR(ftn: Expr, arg: List[Var]) extends Expr
case class WhileExpr(cond: Expr, body: Expr) extends Expr
sealed trait RecordLike extends Expr
case object EmptyRecordExpr extends RecordLike
case class RecordExpr(field: Var, initVal: Expr, next: RecordLike) extends RecordLike


object MiniCInterpreter {

  case class Result(v: Val, m: Mem)
  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)
    
  
  def envbld(env: Env, args: List[Var], x: Int, y: Int, z: Int):Env = {

            if(x == y) envbld(env, args, x + 1, y, z)
            else if(args.size == z){
                env

            }
            else{
                envbld(env.updated(args.apply(z), LocVal(x)), args, x + 1, y, z + 1)
            }
  }
  def envbld2(env: Env, mem: Mem, args: List[Var], args2: List[Var], x: Int):Env = {
            if((args.size - x) == 0){
                env
            }
            else{
                env.get(args2.apply(x)) match{
                    case Some(v) => v match {
                      case r: LocVal => mem.get(r) match {
                        case Some(v) => envbld2(env.updated(args.apply(x), r), mem, args, args2, x + 1)
                        case _ => throw new UndefinedSemantics("ENVBLD2 ERROR1")
                      }
                      case _ => throw new UndefinedSemantics("ENVBLD2 ERROR2")
                    }
                    case None => throw new UndefinedSemantics("ENVBLD2 ERROR3")
                }
             }
  }
  def recget(v: RecordValLike, x: Var):LocVal = v match{
        case EmptyRecordVal => throw new UndefinedSemantics("Empty Record")
        case RecordVal(field, loc, next) => if(field == x) loc
                            else recget(next, x)
  }
  def locget(mem: Mem, ar: List[LocVal], ind:Int):LocVal = {
       if(ind > ar.size) throw new UndefinedSemantics("Error in locget")
       mem.get(ar.apply(ind)) match{
            case Some(v: ProcVal) => {
                ar.apply(ind)                        
            }
            case _ => locget(mem, ar, ind + 1) 
       }
  }

  
  def ff(mem: Mem, v: LocVal, new_mem: HashMap[LocVal,Val]): HashMap[LocVal,Val] = {
        mem.get(v) match{
           case Some(x: LocVal) => {
                ff(mem, x, new_mem.updated(v, x))
           }
           case Some(x: RecordVal) => {
               ff(mem, x.loc, new_mem.updated(v, x))
            }
            case x:Any => new_mem.updated(v, x.get)
        }
  }
  def f(env: Env, mem: Mem, arr: List[Var], new_mem: HashMap[LocVal,Val]): HashMap[LocVal,Val] = {
        if(arr.isEmpty) {
              new_mem
        }
        else{
          val exp = arr.last
          env.get(exp) match{
                      case  Some(v: LocVal) => {
                                mem.get(v) match {
                                  case Some(x: LocVal) => {
                                     val nn = ff(mem, x, new_mem.updated(v, x))
                                     f(env, mem, arr.init, nn)
                                  }
                                  case x:Any => f(env, mem, arr.init, new_mem.updated(v, x.get))
                                  }
                                }
                      case _ => throw new UndefinedSemantics("error in GC")
          }
        }
  }


  def eval(env: Env, mem: Mem, expr: Expr): Result = expr match {
          case Skip => Result(SkipVal, mem)
          case False => Result(BoolVal(false), mem)
          case True => Result(BoolVal(true), mem)
          case Const(n) => Result(IntVal(n), mem)

          // CHECK THIS PLACE AGAIN
          case Var(s) => env.get(Var(s)) match {
              case Some(v) => v match {
                case r: LocVal => mem.get(r) match {
                  case Some(v) => Result(v,mem)
                  case _ => throw new UndefinedSemantics("Error1 in Var(s)")
                }
                case _ => Result(v,mem)
              }
              case None => throw new UndefinedSemantics("Error2 in Var(s)")
          }

          // DO IT
          case Proc(args, expr) => {
               Result(ProcVal(args, expr, env), mem)
          }

          case Add(l, r) => {
                val a = eval(env, mem, l)
                val b = eval(env, a.m, r)
                (a.v, b.v) match {
                  case (x: IntVal, y: IntVal) => Result(IntVal(x.n + y.n), b.m)
                  case _ => throw new UndefinedSemantics("Can't add not integers")
                }
          }
          case Sub(l, r) => {
                val a = eval(env, mem, l)
                val b = eval(env, a.m, r)
                (a.v, b.v) match {
                  case (x: IntVal, y: IntVal) => Result(IntVal(x.n - y.n), b.m)
                  case _ => throw new UndefinedSemantics("Can't subtract not integers")
                }
          }
          case Mul(l, r)=> {
                val a = eval(env, mem, l)
                val b = eval(env, a.m, r)
                (a.v, b.v) match {
                  case (x: IntVal, y: IntVal) => Result(IntVal(x.n * y.n), b.m)
                  case _ => throw new UndefinedSemantics("Can't multiply not integers'")
                }
          }
          case Div(l, r) => {
                val a = eval(env, mem, l)
                val b = eval(env, a.m, r)
                (a.v, b.v) match {
                  case (_, IntVal(0)) => throw new UndefinedSemantics("Can't divide by zero")
                  case (x: IntVal, y: IntVal) => Result(IntVal(x.n / y.n), b.m)
                  case _ => throw new UndefinedSemantics("Can't divide not integers'")
                }
          }

          case LTEExpr(l, r) => {
                val a = eval(env, mem, l)
                val b = eval(env, a.m, r)
                (a.v, b.v) match {
                  case (x: IntVal, y: IntVal) => Result(BoolVal(x.n <= y.n), b.m)
                  case _ => throw new UndefinedSemantics("Can't compare these values")
                }
          }
          case EQExpr(l, r) => {
                val a = eval(env, mem, l)
                val b = eval(env, a.m, r)
                (a.v, b.v) match {
                  case (x: IntVal, y: IntVal) => Result(BoolVal(x.n == y.n), b.m)
                  case (x: BoolVal, y: BoolVal) => Result(BoolVal(x.b == y.b), b.m)
                  //case (x: SkipVal, y: SkipVal) => Result(BoolVal(true), b.m)
                  case _ => throw new UndefinedSemantics("Can't compare typemismatch")
                }
          }
          case Iszero(c) => {
                val a = eval(env, mem, c)
                a.v match {
                  case x: IntVal => Result(BoolVal(x.n == 0), a.m)
                  case _ => throw new UndefinedSemantics("Can't compare with zero")
                }
          }
          case NotExpr(expr) => {
                val a = eval(env, mem, expr) 
                a.v match{
                      case flag:BoolVal => if (flag.b == true) Result(BoolVal(false), a.m)
                                          else Result(BoolVal(true), a.m)
                      case _ => throw new UndefinedSemantics("Can't 'NOT' not booleans")
                }
          }

          case Ite(c, t, f) => 
                eval(env, mem, c) match {
                  case Result(a: BoolVal, m) => if (a.b == true) eval(env, m, t) 
                                                else eval(env, m, f)
                  case _ => throw new UndefinedSemantics("Type Error for Ite")
                }
          case WhileExpr(cond, body) => {
                val a = eval(env, mem, cond)
                a.v match{
                    case flag:BoolVal => if(flag.b == false) Result(SkipVal, a.m)
                                          else {
                                              val b = eval(env, mem, body)
                                              val c = eval(env, b.m, WhileExpr(cond, body))
                                              Result(SkipVal, c.m)
                                          }
                    case _ => throw new UndefinedSemantics("Type Error for WHILE")
                }
          }
          case Block(f, s) => {
                val a = eval(env,mem,f)
                eval(env,a.m,s)
          }
          case BeginEnd(expr) => {
                eval(env, mem, expr)
          }

          case EmptyRecordExpr => Result(EmptyRecordVal, mem)
          case RecordExpr(field, initval, next) => {
                val a = eval(env, mem, initval)
                val mm = a.m.extended(a.v)
                val new_env = env.updated(field, mm._2)
                val new_mem = mm._1

                val b = eval(new_env, new_mem, next)
                b.v match {
                      case v:RecordValLike => 
                                    Result(RecordVal(field, mm._2, v), b.m)
                      case _ => throw new UndefinedSemantics("Error in recordEXPR")
                }
          }
          case FieldAccess(record, field) => {

                val a = eval(env, mem, record)
                
                a.v match{
                      case x:RecordValLike => {
                                               val locc = recget(x, field)
                                               a.m.get(locc) match{
                                               case Some(v) => Result(v,a.m)
                                               case _ => throw new UndefinedSemantics("Error1 in FieldAccess")
                                               } 
                      } 
                      case _ => throw new UndefinedSemantics("Error2 in FieldAccess")
                      }
          }      
          case FieldAssign(record, field, new_val) => {
               val a = eval(env, mem, record)
               val b = eval(env, a.m, new_val)

                a.v match{
                      case x:RecordValLike => {
                                               val locc = recget(x, field)
                                               val new_mem = b.m.updated(locc, b.v)
                                               new_mem match{
                                                   case Some(v: Mem) => Result(b.v, v)
                                                   case _ => throw new UndefinedSemantics("Error1 in FieldAssign")
                                               }
                      } 
                      case _ => throw new UndefinedSemantics("Error2 in FieldAssign")
                }
          }

          case Asn(n, e) => {
                val a = eval(env,mem,e)
                env.get(n) match {
                  case Some(v: LocVal) => a.m.updated(v,a.v) match {
                    case Some(new_mem) => Result(a.v, new_mem)
                    case _ => throw new UndefinedSemantics("ErrorAsn")
                  }
                  case _ => throw new UndefinedSemantics("ErrorAsn")
                }
          }

          case Let(i, v, body) => {
                val a = eval(env, mem, v)
                val new_mem = a.m.extended(a.v)._1
                val new_env = env.updated(i, a.m.extended(a.v)._2)
                eval(new_env, new_mem, body)
          }
           
          
          case PCallV(ftn, arg) => {
                 val a = eval(env, mem, ftn)
                 val new_mem = a.m
                 if(arg.isEmpty){
                      
                      val m1 = new_mem.getLocs()

                      val locc = locget(new_mem, m1, 0).l
                      new_mem.get(LocVal(locc)) match{
                         case Some(v: ProcVal) => {
                              val new_env = envbld(v.env, v.args, 0, locc, 0)
                              // HERE was my mistake v.expr could be proc
                              eval(new_env, new_mem, v.expr)                              
                         }
                         case _ => throw new UndefinedSemantics("Error in PCallV")
                      } 
                 }
                 else{
                     eval(env, new_mem.extended(eval(env, new_mem, arg.apply(0)).v)._1, PCallV(arg.apply(0), arg.drop(1)))
                 }
          }
          
          case PCallR(ftn, arg) => {
                val a = eval(env, mem, ftn)
                
                a.v match{
                   case v: ProcVal => {
                        val new_env = envbld2(env, a.m, v.args, arg, 0)
                        eval(new_env, a.m, v.expr)
                   }
                   case _ => throw new UndefinedSemantics("Error in PCallR")
                }
          }
          
          
          case _ => throw new Exception("No such case") 
  }
  def gc(env: Env, mem: Mem): Mem = {
        val ar = env.keys.toList
        val new_mem = f(env, mem, ar, new HashMap[LocVal, Val])
        Mem(new_mem, mem.top)
  }
  
  def apply(program: String): (Val, Mem) = {
    val parsed = MiniCParserDriver(program)

    val res = eval(new Env(), Mem(new HashMap[LocVal,Val],0), parsed)
    if(res.v == IntVal(-1)) (IntVal(0), res.m)
    else   (res.v, res.m)
  }

}


object Hw4App extends App {
  
  println("Hello from Hw4!")

}