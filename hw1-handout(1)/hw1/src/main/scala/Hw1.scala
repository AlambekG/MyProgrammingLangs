sealed trait IntList
case object Nil extends IntList
case class Cons(v: Int, t: IntList) extends IntList

sealed trait BTree
case object Leaf extends BTree
case class IntNode(v: Int, left: BTree, right: BTree)
extends BTree

sealed trait Formula
case object True extends Formula
case object False extends Formula
case class Not(f: Formula) extends Formula
case class Andalso(left: Formula, right: Formula) extends Formula
case class Orelse(left: Formula, right: Formula)  extends Formula
case class Implies(left: Formula, right: Formula) extends Formula

object Hw1 extends App {

  println("Hw1!")

  def gcd(a: Int, b: Int): Int = {
        if (b == 0)
		      a
	      else
		      gcd (b, a % b)
  }

  def oddSum(f: Int=>Int, n: Int): Int = {
         if(n <= 0)
              0
         else if(n % 2 == 0){
               oddSum(f, n - 1);
         }
            else{
               return f(n) + oddSum(f, n - 2);
         }
         
  }

  def foldRight(init: Int, ftn: (Int, Int)=>Int, list: IntList): Int= 
      list match{
         case Nil => init 
         case Cons(v, t) => ftn(foldRight(init, ftn, t), v)
      }

  def map(f: Int=>Int, list: IntList): IntList =
      list match{
           case Nil => Nil
           case Cons(v, t) => Cons(f(v), map(f, t))
      }

  def iter[A](f: A => A, n: Int): A => A = (x: A) => {
       if(n == 1) f(x)
       else iter[A](f, n - 1)(f(x))
  }
  
  def insert(t: BTree, a: Int): BTree = 
     t match{
          case Leaf => IntNode(a, Leaf, Leaf)
          case IntNode(v, left, right) => if(a > v) IntNode(v, left, insert(right, a)) else IntNode(v, insert(left, a), right)
     }

  def eval(f: Formula): Boolean =
     f match{
          case True => true
          case False => false
          case Not(f) => !(eval(f))
          case Andalso(left, right) => (eval(left) && eval(right))
          case Orelse(left, right) =>  (eval(left) || eval(right))
          case Implies(left, right) => (!(eval(left)) || eval(right))
     }

}