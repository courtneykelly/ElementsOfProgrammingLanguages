package Assignment3.TurtleStandalone
import Assignment3.TurtleEDSL.Assignment3Embedded.TurtleDSL
import Assignment3.TurtleEDSL.Assignment3Embedded.Testing
import Assignment3.TurtleEDSL.Assignment3Embedded.TurtleDSLImpl
import Assignment3.GraphicsCanvas._
import java.awt.Color
import java.util.Random
import scala.collection.immutable.Set
import scala.collection.immutable.ListMap

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object Assignment3Standalone {
  type Variable = String
  type Location = Int
  type Env[A] = Map[Variable,A]
  type Store[A] = Map[Location,A]

  // Arithmetic expressions

  abstract class Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Minus(e1: Expr, e2: Expr) extends Expr
  case class Times(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr

  // Booleans
  case class Eq(e1: Expr, e2:Expr) extends Expr
  case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr
  case class GreaterThan(e1: Expr, e2: Expr) extends Expr
  case class LessThan(e1: Expr, e2: Expr) extends Expr

  // Variables and let-binding
  case class Var(x: Variable) extends Expr
  case class Let(x: Variable, e1: Expr, e2: Expr) extends Expr
  case class LetFun(f: Variable, arg: Variable, ty: Type, e1:Expr, e2:Expr)
      extends Expr
  case class LetRec(f: Variable, arg: Variable, xty: Type, ty: Type, e1:Expr, e2:Expr)
      extends Expr
  case class LetPair(x: Variable, y: Variable, ePair: Expr, eBody: Expr) extends Expr

  // Pairs
  case class Pair(e1: Expr, e2: Expr) extends Expr
  case class Fst(e: Expr) extends Expr
  case class Snd(e: Expr) extends Expr

  // Functions
  case class Lambda(x: Variable, ty: Type, e: Expr) extends Expr
  case class Rec(f: Variable, x: Variable, tyx:Type, ty: Type, e: Expr) extends Expr
  case class Apply(e1: Expr, e2: Expr) extends Expr

  // e1 ; e2
  case class Sequence(e1: Expr, e2: Expr) extends Expr

  // References
  case class Loc(loc: Location) extends Expr
  case class CreateRef(e: Expr) extends Expr
  case class Deref(e: Expr) extends Expr
  case class Assign(e1: Expr, e2: Expr) extends Expr

  // Lists
  case class EmptyList(ty: Type) extends Expr
  case class Cons(e: Expr, e2: Expr) extends Expr
  case class ListCase(l: Expr, e1: Expr, x: Variable, y: Variable, e2: Expr) extends Expr

  // Turtle-y things
  case class Forward(e: Expr) extends Expr
  case class Backward(e: Expr) extends Expr
  case class Right(e: Expr) extends Expr
  case class Left(e: Expr) extends Expr
  case class PenUp() extends Expr
  case class PenDown() extends Expr
  case class SetCol(e: Expr) extends Expr
  case class RandCol(e: Expr) extends Expr

  // Looping
  case class While(pred: Expr, body: Expr) extends Expr
  case class DoWhile(body: Expr, pred: Expr) extends Expr

  // Case which lifts a value into an expression
  //case class ValueExpr(v: Value) extends Expr

  // Values
  abstract class Value extends Expr
  case object UnitV extends Value
  case class NumV(n: Int) extends Value
  case class BoolV(n: Boolean) extends Value
  case class ColorV(c: Color) extends Value
  case class ListV(l: List[Value]) extends Value
  case class LocV(loc: Location) extends Value
  case class PairV(fst: Value, snd: Value) extends Value
  case class FunV(x: Variable, e: Expr) extends Value
  case class RecV(f: Variable, x: Variable, e: Expr) extends Value


    // Types
  abstract class Type
  case object UnitTy extends Type
  case object IntTy extends Type
  case object BoolTy extends Type
  case object ColorTy extends Type
  case class ListTy(ty1: Type) extends Type
  case class RefTy(ty1: Type) extends Type
  case class PairTy(ty1: Type, ty2: Type) extends Type
  case class FunTy(ty1: Type, ty2: Type) extends Type

  object Gensym {
    private var id = 0
    def gensym(s: Variable): Variable = {
      val fresh_s = s + "_" + id
      id = id + 1
      fresh_s
    }
  }

  object Genloc {
    private var id = 0
    def genloc(): Location = {
      val newloc = id
      id = id + 1
      newloc
    }
  }

  val rand = new Random(System.currentTimeMillis())

  /****************
   *  Exercise 3  *
   ****************/

  // Typechecker
  // typing: calculate the return type of e, or throw an error
  def tyOf(ctx: Env[Type], e: Expr): Type = {
    def valueTy(v: Value): Type = v match {
      case UnitV => UnitTy
      case NumV(_) => IntTy
      case BoolV(_) => BoolTy
      case ColorV(_) => ColorTy
      case ListV(_) => sys.error("Impossible case: ListTy(xs) only introduced at runtime")
      case LocV(_) => sys.error("Impossible case: Locations are only introduced at runtime, and have no type")
      case PairV(_, _) => sys.error("Impossible case: PairV is only introduced at runtime")
      case FunV(_, _) => sys.error("Impossible case: FunV is only introduced at runtime")
      case RecV(_, _, _) => sys.error("Impossible case: FunV is only introduced at runtime")
    }

    e match {
      // Values
      case v: Value => valueTy(v)
      case _ => e match {

        // Arithmetic
        case NumV(n) => IntTy
        case Plus(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
          case (IntTy, IntTy) => IntTy
          case _ => sys.error("non-integer arguments to -") 
        }
        case Minus(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
          case (IntTy, IntTy) => IntTy
          case _ => sys.error("non-integer arguments to +") 
        }
        case Times(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
          case (IntTy, IntTy) => IntTy
          case _ => sys.error("non-integer arguments to *") 
        }
        case Div(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
          case (IntTy, IntTy) => IntTy
          case _ => sys.error("non-integer arguments to /")
        }

        //  Booleans
        case BoolV(b) => BoolTy
        case Eq(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
          case (IntTy, IntTy) => BoolTy
          case (BoolTy, BoolTy) => BoolTy
          case (ColorTy, ColorTy) => BoolTy
          case _ => sys.error("arguments to Eq must be of the same type")
        }
        case LessThan(e1, e2) => (tyOf(ctx,e1)) match {
          case IntTy => (tyOf(ctx,e2)) match {
            case IntTy => BoolTy
            case _ => sys.error("Second argument to LessThan must be an Integer")
          }
          case _ => sys.error("First argument to LessThan must be an Integer")
        }
        case GreaterThan(e1, e2) => (tyOf(ctx,e1)) match {
          case IntTy => (tyOf(ctx,e2)) match {
            case IntTy => BoolTy
            case _ => sys.error("Second argument to GreaterThan must be an Integer")
          }
          case _ => sys.error("First argument to GreaterThan must be an Integer")
        }
        case IfThenElse(e,e1,e2) =>
          (tyOf(ctx,e),tyOf(ctx,e1),tyOf(ctx,e2)) match {
            case (BoolTy,a,b) => if (a == b) {
              a
            }
            else {
              sys.error("types of branches must be equal")
            }
            case (_,a,b) => sys.error("type of conditional must be boolean")
          }

        // Variables and let-binding
        case Var(x) => ctx(x)
        case Let(x,e1,e2) => tyOf(ctx + (x -> (tyOf(ctx,e1))), e2)
        case LetPair(x,y,e1,e2) => tyOf(ctx, e1) match {
          case PairTy(a,b) => tyOf(ctx + (x -> a) + (y -> b), e2)
          case _ => sys.error("Let pair's first argument must be a pair")
        }

        // Sequencing
        case Sequence(e1, e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
          case (a,b) => b
          case _ => sys.error("type mismatch for Sequence") 
        }

        // While
        case While(pred, body) => (tyOf(ctx,pred)) match {
          case BoolTy => (tyOf(ctx,body)) match {
            case UnitTy => UnitTy
            case _ => sys.error("body of while must be of type Unit")
          } 
          case _ => sys.error("pred of while must be a boolean")
        }
        case DoWhile(body, pred) => (tyOf(ctx,pred)) match {
          case BoolTy => (tyOf(ctx,body)) match {
            case UnitTy => UnitTy
            case _ => sys.error("body of while must be of type Unit")
          } 
          case _ => sys.error("pred of while must be a boolean")
        }

        // References
        case CreateRef(e) => RefTy
        case Deref(e) => tyOf(ctx,e) match {
          case RefTy(t) => t
          case _ => sys.error("...")
        }
        case Assign(e1, e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
          case (RefTy(t1),t2) => 
            if (t1 == t2) {UnitTy}
            else {sys.error("type mismatch for Assign")}
          case _ => sys.error("type mismatch for Assign")
        }

        // Lists 
        case EmptyList(t) => ListTy(t)
        /*
        case Cons(e1, e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
          case (a,b) => b 
          case _ => sys.error("type mismatch for Cons")
        }
        */
        case ListCase

        // Turtle constructs
        case ColorV(c) => ColorTy
        case Forward(e) => (tyOf(ctx,e)) match {
          case IntTy => UnitTy
          case _ => sys.error("Forward's argument must be an Integer")
        }
        case Backward(e) => (tyOf(ctx,e)) match {
          case IntTy => UnitTy
          case _ => sys.error("Backward's argument must be an Integer")
        }
        case Right(e)=> (tyOf(ctx,e)) match {
          case IntTy => UnitTy
          case _ => sys.error("Right's argument must be an Integer")
        }
        case Left(e) => (tyOf(ctx,e)) match {
          case IntTy => UnitTy
          case _ => sys.error("Left's argument must be an Integer")
        }
        case PenUp() => UnitTy
        case PenDown() => UnitTy
        case SetCol(e) => (tyOf(ctx,e)) match {
          case ColorTy => UnitTy
          case _ => sys.error("SetCol's argument must be a Color")
        }
        case RandCol(e) => (tyOf(ctx,e)) match {
          case ListTy(ColorTy) => UnitTy
          case _ => sys.error("RandCol's argument must be a List of type Color")
        }
      
      }
    }
  }


  // Swapping (provided)
  def swapVar(x: Variable, y: Variable, z: Variable): Variable =
    if (x == y) {
      z
    } else if (x == z) {
      y
    } else {
      x
    }

  def swap(e: Expr, y: Variable, z: Variable): Expr = {


    def go(e: Expr): Expr = e match {
      
      // Values are closed
      case v: Value => v
      case Plus(t1,t2) => Plus(go(t1),go(t2))
      case Minus(t1,t2) => Minus(go(t1),go(t2))
      case Times(t1,t2) => Times(go(t1),go(t2))
      case Div(t1,t2) => Div(go(t1),go(t2))

      case Eq(t1,t2) => Eq(go(t1),go(t2))
      case GreaterThan(t1, t2) => GreaterThan(go(t1), go(t2))
      case LessThan(t1, t2) => LessThan(go(t1), go(t2))
      case IfThenElse(t,t1,t2) => IfThenElse(go(t),go(t1),go(t2))

      case Var(x) => Var(swapVar(x,y,z))
      case Let(x,t1,t2) => Let(swapVar(x,y,z),go(t1),go(t2))
      case LetFun(f,x,ty,t1,t2) => LetFun(swapVar(f,y,z),swapVar(x,y,z),ty,go(t1),go(t2))
      case LetRec(f,x,xty,ty,t1,t2) => LetRec(swapVar(f,y,z),swapVar(x,y,z),xty,ty,go(t1),go(t2))
      case LetPair(x1, x2, t1, t2) =>
        LetPair(swapVar(x1, y, z), swapVar(x2, y, z), go(t1), go(t2))

      case Lambda(x,ty,t) => Lambda(swapVar(x,y,z),ty,go(t))
      case Apply(t1,t2) => Apply(go(t1),go(t2))
      case Rec(f,x,xty,ty,t) => Rec(swapVar(f,y,z), swapVar(x,y,z), xty,ty,go(t))

      case Sequence(t1, t2) => Sequence(go(t1), go(t2))
      
      // References
      case Loc(l) => Loc(l)
      case CreateRef(t) => CreateRef(go(t))
      case Deref(t) => Deref(go(t))
      case Assign(t1, t2) => Assign(go(t1), go(t2))

      // Pairs
      case Pair(t1, t2) => Pair(go(t1), go(t2))
      case Fst(t) => Fst(go(t))
      case Snd(t) => Snd(go(t))

      // Lists
      case EmptyList(t) => EmptyList(t)
      case Cons(t1, t2) => Cons(go(t1), go(t2))
      case ListCase(l, t1, consVar1, consVar2, t2) =>
        ListCase(go(l), go(t1), swapVar(consVar1, y, z), swapVar(consVar2, y, z), go(t2))

      // Turtle constructs
      case Forward(t) => Forward(go(t))
      case Backward(t) => Backward(go(t))
      case Right(t) => Right(go(t))
      case Left(t) => Left(go(t))
      case SetCol(t) => SetCol(go(t))
      case RandCol(t) => RandCol(go(t))
      case PenUp() => PenUp()
      case PenDown() => PenDown()

      // Looping
      case While(p, b) => While(go(p), go(b))
      case DoWhile(b, p) => DoWhile(go(b), go(p))
    }
    go(e)
  }

  /****************
   *  Exercise 4  *
   ****************/
  def subst(e1:Expr, e2:Expr, x: Variable): Expr = {

    e1 match {
      
      // Values are closed so substitution has no effect
      case v: Value => v
      case _ => e1 match {
      
        // Arithmetic
        case NumV(e2) => NumV(e2)
        case Plus(t1,t2) => Plus(subst(t1,e2,x),subst(t2,e2,x))
        case Minus(t1,t2) => Minus(subst(t1,e2,x),subst(t2,e2,x))
        case Times(t1,t2) => Times(subst(t1,e2,x),subst(t2,e2,x))
        case Div(t1,t2) => Div(subst(t1,e2,x), subst(t2,e2,x))

        // Booleans
        case BoolV(b) => BoolV(b)
        case Eq(t1,t2) => Eq(subst(t1,e2,x),subst(t2,e2,x))
        case GreaterThan(t1,t2) => GreaterThan(subst(t1,e2,x),subst(t2,e2,x))
        case LessThan(t1,t2) => LessThan(subst(t1,e2,x),subst(t2,e2,x))
        case IfThenElse(t0,t1,t2) =>
          IfThenElse(subst(t0,e2,x),subst(t1,e2,x),subst(t2,e2,x))

        case Var(y) =>
          if (x == y) {
            e2
          } else {
            Var(y)
          }
        case Let(y,t1,t2) => {
          val z = Gensym.gensym(y);
          Let(z,subst(t1,e2,x),subst(swap(t2,y,z),e2,x))
        }

        // Pairs
        case Pair(t1,t2) => Pair(subst(t1,e2,x),subst(t2,e2,x))
        case Fst(t0) => Fst(subst(t0,e2,x))
        case Snd(t0) => Snd(subst(t0,e2,x))

        // Functions
        case Lambda(y,ty,t0) => {
          val z = Gensym.gensym(y);
          Lambda(z,ty,subst(swap(t0,y,z),e2,x))
        }
        case Apply(t1,t2) => Apply(subst(t1,e2,x),subst(t2,e2,x))
        case Rec(f,y,ty1,ty2,t0) => {
          val g = Gensym.gensym(f);
          val z = Gensym.gensym(y);
          Rec(g,z,ty1,ty2,subst(swap(swap(t0,f,g),y,z),e2,x))
        }

        // Syntactic sugar
        case LetPair(y1,y2,t1,t2) => {
          val y1z = Gensym.gensym(y1);
          val y2z = Gensym.gensym(y2);
          LetPair(y1z,y2z,subst(t1,e2,x),
            subst(swap(swap(t2,y1z,y1), y2z, y2), e2,x))
        }

        case LetFun(f,y,ty,t1,t2) => {
          val fz = Gensym.gensym(f);
          val yz = Gensym.gensym(y);
          LetFun(fz,yz,ty,subst(swap(t1,yz,y),e2,x),
            subst(swap(t2,fz,f), e2,x))
        }

        case LetRec(f,y,ty1,ty2,t1,t2) => {
          val fz = Gensym.gensym(f);
          val yz = Gensym.gensym(y);
          LetRec(fz,yz,ty1,ty2,subst(swap(swap(t1,fz,f),yz,y),e2,x),
            subst(swap(t2,fz,f), e2,x))
        }

        // Sequencing 
        case Sequence(t1, t2) => Sequence(subst(t1,e2,x),subst(t2,e2,x))

        // References
        case Loc(l) => Loc(l)
        case CreateRef(t) => CreateRef(subst(t,e2,x))
        case Deref(t) => Deref(subst(t,e2,x))
        case Assign(t1, t2) => Assign(subst(t1,e2,x),subst(t2,e2,x))

        // Lists
        case EmptyList(t) => EmptyList(t)
        case Cons(t1, t2) => Cons(subst(t1,e2,x),subst(t2,e2,x))
        //case ListCase(l, t1, consVar1, consVar2, t2) => 

        // Turtle constructs
        case Forward(t) => Forward(subst(t,e2,x))
        case Backward(t) => Backward(subst(t,e2,x))
        case Right(t) => Right(subst(t,e2,x))
        case Left(t) => Left(subst(t,e2,x))
        case SetCol(t) => SetCol(subst(t,e2,x))
        case RandCol(t) => RandCol(subst(t,e2,x))

        // Looping
        case While(p, b) => While(subst(p,e2,x), subst(b,e2,x))
        case DoWhile(b, p) => DoWhile(subst(b,e2,x), subst(p,e2,x))
      
      }
    }
  }

  /****************
   *  Exercise 5  *
   ****************/
  // Desugaring
  def desugar(e: Expr): Expr = e match {
    
    case Plus(e1,e2) => Plus(desugar(e1),desugar(e2))
    case Minus(e1,e2) => Minus(desugar(e1),desugar(e2))
    case Times(e1,e2) => Times(desugar(e1),desugar(e2))
    case Div(e1,e2) => Div(desugar(e1), desugar(e2))

    case Eq(e1,e2) => Eq(desugar(e1),desugar(e2))
    case GreaterThan(e1,e2) => GreaterThan(desugar(e1), desugar(e2))
    case LessThan(e1,e2) => LessThan(desugar(e1), desugar(e2))
    case IfThenElse(cond,e1,e2) => {
      IfThenElse(desugar(cond),desugar(e1),desugar(e2))
    }

    case Let(x,e1,e2) => Let(x,desugar(e1),desugar(e2))
    case LetFun(f,arg,ty,e1,e2) =>
      Let(f,Lambda(arg,ty,desugar(e1)),desugar(e2))
    case LetRec(f,arg,xty,ty,e1,e2) => {
      Let(f,
        Rec(f,arg,xty,ty,desugar(e1)),
        desugar(e2))
    }
    case LetPair(x,y,e1,e2) => {
      val p = Gensym.gensym("p")
      Let(p,desugar(e1),subst(subst(desugar(e2),Fst(Var(p)),x),Snd(Var(p)),y))
    }

    case Pair(e1,e2) => Pair(desugar(e1),desugar(e2))
    case Fst(e) => Fst(desugar(e))
    case Snd(e) => Snd(desugar(e))

    case Lambda(x,ty,e) => Lambda(x,ty,desugar(e))
    case Apply(e1,e2) => Apply(desugar(e1),desugar(e2))
    case Rec(f,x,tyx,ty,e) => Rec(f,x,tyx,ty,desugar(e))

    // Sequencing 
    case Sequence(e1, e2) => Sequence(desugar(e1), desugar(e2))

    // References
    case Loc(l) => Loc(l)
    case CreateRef(e) =>  CreateRef(desugar(e))
    case Deref(e) => Deref(desugar(e))
    case Assign(e1, e2) => Assign(desugar(e1), desugar(e2))

    // Lists
    case EmptyList(e) => EmptyList(e)
    case Cons(e1, e2) => Cons(desugar(e1), desugar(e2))
    //case ListCase(l, t1, consVar1, consVar2, t2) =>

    // Turtle constructs
    case Forward(e) => Forward(desugar(e))
    case Backward(e) => Backward(desugar(e))
    case Right(e) => Right(desugar(e))
    case Left(e) => Left(desugar(e))
    case SetCol(e) => SetCol(desugar(e))
    case RandCol(e) => RandCol(desugar(e))
    case PenUp() => PenUp()
    case PenDown() => PenDown()

    // Looping
    case While(p, b) => While(desugar(p), desugar(b))
    case DoWhile(b, p) => DoWhile(desugar(b), desugar(p))

    case e => e // Num, bool, str, var
    
  }
  
  class Eval(tg: TurtleDSL) {
    import tg._

    // State used to keep track of canvas, co-ords, angle, pen state
    private final case class TurtleState(
      store: Store[Value],
      graphics: TurtleGraphics
    )

    // Some potentially-useful helper methods
    // Any methods whose types mention TurtleState need to be private
    private def updateVarMapping(locName: Location, v: Value, ts: TurtleState) = {
      val vmap = ts.store
      ts.copy (store = vmap + (locName -> v))
    }

    private def getVarMapping(locName: Location, ts: TurtleState): Value = {
      ts.store(locName)
    }

    private def addGraphics(tg: TurtleGraphics, ts: TurtleState) = {
      val graphics = ts.graphics
      ts.copy (graphics = graphics <> tg)
    }

    private def evalBinArgs(st: TurtleState, e1: Expr, e2: Expr): (TurtleState, Value, Value) = {
      val (st1, v1) = eval(st, e1)
      val (st2, v2) = eval(st1, e2)
      (st2, v1, v2)
    }

    /****************
     *  Exercise 6  *
     ****************/

    private def eval(state: TurtleState, expr: Expr): (TurtleState, Value) = expr match { 
      case v: Value => (state,v)
      case _ => expr match {

        // Arithmetic
        case NumV(n) => (state, NumV(n))
        case Plus(e1,e2) => {
          val (state1,val1) = eval(state,e1)
          val (state2, val2) = eval(state1,e2)
          (val1, val2) match {
            case (NumV(t1), NumV(t2)) => (state2, NumV(t1+t2))
          }
        }
        case Minus(e1,e2) => 
          (state, subtract(eval(state,e1),eval(state,e2)))
        case Times(e1,e2) =>
          (state, multiply(eval(state,e1),eval(state,e2)))
        case Div(e1,e2) =>
          (state, divide(eval(state,e1), eval(state, e2)))
          
        // Booleans
        case BoolV(b) => (state, BoolV(b))
        case Eq(e1,e2) =>
          (state, equal(eval(state,e1),eval(state,e2)))
        case LessThan(e1,e2) => 
          (state, equal(eval(state,e1),eval(state,e2)))
        case GreaterThan(e1,e2) =>
          (state, equal(eval(state,e1),eval(state,e2)))
        case IfThenElse(e,e1,e2) =>
          eval(state,e) match {
            case (state, BoolV(true)) => (state, eval(state,e1))
            case (state, BoolV(false)) => (state, eval(state,e2)
            case _ =>  sys.error("conditional must evaluate to a boolean")
          }
        
        // Variables + let
        /*
        case Var(x) =>
          env(x)
        */
        case Let(x,e1,e2) =>
          (state, eval(state + (x -> eval(env,e1)),e2)
          
        // Pairs
        case Pair(e1,e2) =>
          (state, PairV(eval(state,e1), eval(state,e2)))
        case Fst(e) => eval(env,e) match {
          case PairV(x,_) => (state, x)
          case _ => sys.error("first must be applied to a pair")
        }
        case Snd(e) => eval(env,e) match {
          case PairV(_,y) => (state, y)
          case _ => sys.error("second must be applied to a pair")
        }

        // Functions
        /*
        case Apply(e1,e2) =>
          eval(env,e1) match {
            case ClosureV(lamEnv,x,lamBody) =>
              eval(lamEnv + (x -> eval(env,e2)),lamBody)
            case RecV(recEnv,f,x,recBody) =>
              eval(recEnv
                + (f -> RecV(recEnv,f,x,recBody))
                + (x -> eval(env,e2)),
                recBody)
            case _ => sys.error("first argument of application must be a function")
          }
        */

        // Sequencing 
        case Sequence(e1, e2) => 

        // References
        //case Loc(l) => 
        case CreateRef(e) =>  
        case Deref(e) => 
        case Assign(e1, e2) => 

        // Lists
        //case EmptyList(e) => 
        case Cons(e1, e2) => 
        case ListCase(l, t1, consVar1, consVar2, t2) =>

        // Turtle constructs
        case Forward(e) => {
          val (state1, val1) = eval(state,e)
          (val1) match {
            case NumV(t) => (addGraphics(forward(t), state1), UnitV)
          }
        }
        case Backward(e) => 
        case Right(e) => 
        case Left(e) => 
        case SetCol(e) => 
        case RandCol(e) => 
        case PenUp() => 
        case PenDown() => 

        // Looping
        case While(p, b) => 
        case DoWhile(b, p) => 

      }
    }

    def evalExpr(expr: Expr): Value = {
      val initState = TurtleState(ListMap(),empty)
      val (_endState, endResult) = eval(initState, expr)
      endResult
    }

    // Interpreter
    def run(expr: Expr, width: Integer, height: Integer): GraphicsCanvas = {
      val initState = TurtleState(ListMap(),empty)
      val (endState,_endResult) = eval(initState, expr)
      return draw(endState.graphics,width,height)
    }
  }



  /////////////////// BEGIN SUPPORT CODE
  ///////////////////
  // You should not change anything below this comment
  ///////////////////
  // Parser

  class CWParser extends StandardTokenParsers with PackratParsers {

    type P[+A] = PackratParser[A]

    def parseStr(input: String): Expr = {
      phrase(expression)(new lexical.Scanner(input)) match {
        case Success(ast, _) => ast
        case e: NoSuccess => sys.error(e.msg)
      }
    }

    def parse(input: String): Expr = {
      val source = scala.io.Source.fromFile(input)
      val lines = try source.mkString finally source.close()
      parseStr(lines)
    }

    lexical.reserved += ("let", "in", "rec", "if", "then", "else",
      "int","bool","true","false","fun","color",
      "Red","Green", "Blue", "Pink","Black", "forward", "backward", "right","left","penUp",
      "penDown","setCol","randCol","ref", "case", "list", "ref", "while", "do",
      "fst", "snd", "unit"
    )
    lexical.delimiters += ("=","*", "\\", "+", "-", "(", ")", "==", ":", ":=", ".",
      "->", "=>", "{", "}", "|", "::", "[", "]", "|->", ">", "<", "!", "()", ";", "/", ","
    )


    // Order of precedence (loosest binding first)
    // ;
    // :=, ::
    // <, ==, >
    // +, -
    // *
    // !
    lazy val binaryOp: P[Expr] = {
      sequence
    }

    lazy val sequence: P[Expr] = {
      expression~";"~expression ^^ {
        case e1~";"~e2 => Sequence(e1, e2)
      } | assignOrCons
    }

    lazy val assignOrCons: P[Expr] = {
      ((expression <~ ":=") ~ binRel) ^^ {
        case e1~e2 => Assign(e1, e2)
      } | ((expression <~ "::") ~ expression) ^^ {
        case e1~e2 => Cons(e1, e2)
      } | binRel
    }

    lazy val binRel: P[Expr] =
      expression ~ "==" ~ summation ^^ {
        case e1~"=="~e2 => Eq(e1,e2)
      } | expression ~ "<" ~ summation ^^ {
        case e1~"<"~e2 => LessThan(e1,e2)
      } | expression ~ ">" ~ summation ^^ {
        case e1~">"~e2 => GreaterThan(e1,e2)
      } | summation

    lazy val summation: P[Expr] =
      summation ~ "+" ~ prod ^^ {
        case e1~"+"~e2 => Plus(e1,e2)
      } | summation ~ "-" ~ prod ^^ {
        case e1~"-"~e2 => Minus(e1,e2)
      } | prod

    lazy val prod: P[Expr] =
      prod ~ "*" ~ unaryOp ^^ {
        case e1~"*"~e2 => Times(e1,e2)
      } | prod ~ "/" ~ unaryOp ^^ {
        case e1~"/"~e2 => Div (e1,e2)
      } | unaryOp

    lazy val pRed: P[Expr] = "Red" ^^^ ColorV(Color.RED)

    lazy val pGreen: P[Expr] = "Green" ^^^ ColorV(Color.GREEN)
    lazy val pBlue: P[Expr] = "Blue" ^^^ ColorV(Color.BLUE)
    lazy val pPink: P[Expr] = "Pink" ^^^ ColorV(Color.PINK)
    lazy val pBlack: P[Expr] = "Black" ^^^ ColorV(Color.BLACK)
    lazy val colour: P[Expr] =
      pRed | pGreen | pBlue | pPink | pBlack

    lazy val whileLoop: P[Expr] =
      ("while" ~ "(") ~>
      expression~
      ((")" ~ "{") ~> expression) <~ "}" ^^ {
      case pred~body => While(pred, body)
    }

    lazy val doWhileLoop: P[Expr] =
      "do" ~> ("{" ~> expression <~ "}") ~ ("while" ~> "(" ~> expression <~ ")") ^^ {
      case body~pred => DoWhile(body, pred)
    }

    lazy val lambda: P[Expr] =
      ("\\" ~> ident) ~ (":" ~> typ) ~ ("." ~> expression) ^^ {
        case arg~ty~body => Lambda(arg,ty,body)
      }

    lazy val rec: P[Expr] =
      ("rec" ~> ident) ~
        ("(" ~> ident) ~ (":" ~> typ) ~ ((")" ~ ":") ~> typ) ~
        ("." ~> expression) ^^ {
          case recArg~funArg~funType~recType~body =>
            Rec(recArg,funArg,funType,recType,body)
        }

    lazy val ifExpr: P[Expr] =
      ("if" ~> expression) ~
        ("then" ~> expression) ~
        ("else" ~> expression) ^^ {
          case cond~e1~e2 => IfThenElse(cond,e1,e2)
        }

    lazy val letExpr: P[Expr] =
      ("let" ~> ident) ~ ("=" ~> expression) ~ ("in" ~> expression) ^^ {
        case binder~e1~e2 => Let(binder,e1,e2)
      }

    lazy val letFun: P[Expr] =
      ("let" ~ "fun" ~> ident) ~ ("(" ~> ident) ~
        (":" ~> typ <~ ")") ~ ("=" ~> expression) ~
        ("in" ~> expression) ^^ {
          case fun~binder~ty~e1~e2 => LetFun(fun,binder,ty,e1,e2)
        }

    lazy val letRec: P[Expr] = {
      ("let" ~ "rec" ~> ident) ~ ("(" ~> ident) ~
        (":" ~> typ <~ ")") ~ (":" ~> typ) ~ ("=" ~> expression) ~
        ("in" ~> expression ) ^^ {
          case fun~binder~xty~ty~e1~e2 => LetRec(fun,binder,xty,ty,e1,e2)
        }
      }

    lazy val letPair: P[Expr] =
      ("let" ~ "(") ~> ident ~ ("," ~> ident <~ ")") ~
        ("=" ~> expression) ~ ("in" ~> expression) ^^ {
          case x~y~e1~e2 => LetPair(x,y,e1,e2)
        }

    lazy val emptyList: P[Expr] =
      (("[" ~ "]") ~> ":" ~> typ) ^^ EmptyList

    lazy val emptyListCase: P[Expr] =
      ((("[" ~ "]") ~ "|->") ~> expression)

    lazy val consListCase: P[(Variable, Variable, Expr)] = {
      (ident<~"::")~(ident <~ "|->")~expression ^^ {
        case i1~i2~e => (i1, i2, e)
      }
    }

    lazy val listCase: P[Expr] = {
        (("case"~> expression <~"{")~
          emptyListCase~("|" ~> consListCase <~ "}")) ^^ {
           case scrutinee~ec~cc =>
            cc match {
              case (i1, i2, cc) => ListCase(scrutinee, ec, i1, i2, cc)
            }
        }
    }

    lazy val unaryOp: P[Expr] =
      createRef | deref | fact

    lazy val createRef: P[Expr] =
      ("ref"~>fact) ^^ CreateRef

    lazy val deref: P[Expr] =
      ("!"~>fact) ^^ Deref

    lazy val typ: P[Type] =
      funTyp

    lazy val funTyp: P[Type] =
      typ ~ "->" ~ funTyp ^^ {
        case t1~"->"~t2 => FunTy(t1,t2)
      } | pairTyp

    lazy val pairTyp: P[Type] =
      primitiveType ~ "*" ~ pairTyp ^^ {
        case t1~"*"~t2 => PairTy(t1,t2)
      } | listTyp

    lazy val listTyp: P[Type] =
      "list" ~> ("[" ~> typ <~ "]") ^^ {
      case innerTy => ListTy(innerTy)
    } | refTyp

    lazy val refTyp: P[Type] =
      "ref" ~> ("[" ~> typ <~ "]") ^^ {
      case innerTy => RefTy(innerTy)
    } | primitiveType

    lazy val primitiveType: P[Type] =
      "bool" ^^^ BoolTy | "int" ^^^ IntTy | "color" ^^^ ColorTy |
      "unit" ^^^ UnitTy | "("~>typ<~")"

    lazy val application: P[Expr] = {
      fact ~ fact ^^ {
        case e1~e2 => Apply(e1,e2)
      }
    }

    lazy val forward: P[Expr] =
      ("forward" ~> fact) ^^ {
        case e => Forward(e)
      }

    lazy val backward: P[Expr] =
      ("backward" ~> fact) ^^ {
        case e => Backward(e)
      }

    lazy val right: P[Expr] =
      ("right" ~> fact) ^^ {
        case e => Right(e)
      }

    lazy val left: P[Expr] =
      ("left" ~> fact) ^^ {
        case e => Left(e)
      }

    lazy val setCol: P[Expr] =
      ("setCol" ~> fact) ^^ {
        case e => SetCol(e)
      }

    lazy val randCol: P[Expr] =
      ("randCol" ~> fact) ^^ {
        case e => RandCol(e)
      }

    lazy val penUp: P[Expr] = "penUp" ^^^ PenUp()
    lazy val penDown: P[Expr] = "penDown" ^^^ PenDown()

    lazy val turtleConst: P[Expr] =
      penUp |
      penDown |
      setCol |
      randCol |
      forward |
      backward |
      right |
      left

    lazy val expression: P[Expr] = simpleExpr

    lazy val simpleExpr: P[Expr] = (
        lambda |
        rec |
        letExpr |
        letFun |
        letRec |
        letPair |
        ifExpr |
        binaryOp |
        listCase |
        whileLoop |
        doWhileLoop |
        emptyList |
        fact
    )

    lazy val pairLit: P[Expr] =
      "(" ~> expression ~ "," ~ expression <~ ")" ^^ {
        case t1~","~t2 => Pair(t1,t2)
      }

    lazy val pairOp: P[Expr] =
      ("fst" ~ "(") ~> expression <~ ")" ^^ (x => Fst(x)) |
        ("snd" ~ "(") ~> expression <~ ")" ^^ (x => Snd(x))

    lazy val operations: P[Expr] = (
      application |
      pairOp |
      turtleConst
    )

    lazy val fact: P[Expr] = (
        operations |
        pairLit |
        colour |
        (ident ^^ Var) |
        (numericLit ^^ { x => NumV(x.toInt) }) |
        ("()" ^^^ (UnitV)) |
        ("true" ^^^ BoolV(true)) |
        ("false" ^^^ BoolV(false)) |
        "("~>expression<~")"
    )
  }


  val parser = new CWParser
  object Main {
    def typecheck(ast: Expr):Type =
      tyOf(Map.empty,ast);

    def createCanvasAndRun(w: Integer, h: Integer,
        ast: Expr, outputFilename: String, test: Boolean) = {
      if(test) {
        println("Rendering using Testing...")
        val canvas = new Eval(Testing).run(ast, w, h)
        canvas.saveToFile(outputFilename)
      } else {
        println("Rendering using TurtleDSLImpl...")
        val canvas = new Eval(TurtleDSLImpl).run(ast, w, h)
        canvas.saveToFile(outputFilename)
      }
    }

    def showResult(ast: Expr, width: Integer, height: Integer, outputFilename: String, test: Boolean) {
      println("AST:  " + ast.toString + "\n")
      try {
        print("Type Checking...");
        val ty = typecheck(ast);
        println("Done!");
        println("Type of Expression: " + ty.toString + "\n") ;
      } catch {
          case e:Throwable => println("Error: " + e)
      } 
      try{
        println("Desugaring...");
        val core_ast = desugar(ast);
        println("Done!");
        println("Desugared AST: " + core_ast.toString + "\n") ;
        try {
          println("Evaluating...");
          createCanvasAndRun(width, height, core_ast, outputFilename,test)
        } catch {
          case e:Throwable => println("Error: " + e)
        }
      } catch {
        case e: Throwable =>  println("Error: " + e)
          println("Evaluating original AST...");
          createCanvasAndRun(width, height, ast, outputFilename,test)
      }
   
    }
  }

  val FILENAME = "filename"
  val OUTPUT = "output"
  val WIDTH = "width"
  val HEIGHT = "height"
  val TEST = "test"

  val defaultArgs = ListMap (
    FILENAME -> "",
    WIDTH -> "2000",
    HEIGHT -> "2000",
    OUTPUT -> "output.png",
    TEST -> "false"
  )

  def main( args:Array[String] ):Unit = {
    val argList = args.toList

    def readArgs(argList: List[String], optMap: ListMap[String, String]):
      ListMap[String, String] = argList match {
        case Nil => optMap
        case "-o" :: outputName :: tail =>
          readArgs(tail, optMap + (OUTPUT -> outputName))
        case "-w" :: w :: tail =>
          readArgs(tail, optMap + (WIDTH -> w))
        case "-h" :: h :: tail =>
          readArgs(tail, optMap + (HEIGHT -> h))
        case "-t" :: tail =>
          readArgs(tail, optMap + (TEST -> "true"))
        case fn :: _ => optMap + (FILENAME -> fn)
    }

    if (args.length == 0) {
      print("Usage: [-o output_filename] [-w width] [-h height] [-t] filename\n")
    } else {
      print("Parsing...");
      val argMap = readArgs(args.toList, defaultArgs)
      val ast = parser.parse(argMap(FILENAME))
      println("Done!");
      Main.showResult(ast, argMap(WIDTH).toInt, argMap(HEIGHT).toInt, argMap(OUTPUT), argMap(TEST).toBoolean)
    }
  }
}
// vim: set ts=2 sw=2 et sts=2:
