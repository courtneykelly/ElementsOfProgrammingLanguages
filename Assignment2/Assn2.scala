// Version 1.0

import scala.collection.immutable.Set

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object Assn2 {
  type Variable = String
  type Env[A] = Map[Variable,A]

  // Arithmetic expressions

  abstract class Expr
  case class Num(n: Integer) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Minus(e1: Expr, e2: Expr) extends Expr
  case class Times(e1: Expr, e2: Expr) extends Expr

  // Booleans
  case class Bool(n: Boolean) extends Expr
  case class Eq(e1: Expr, e2:Expr) extends Expr
  case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr
  
   // Strings
  case class Str(s: String) extends Expr
  case class Length(e: Expr) extends Expr
  case class Index(e1: Expr, e2: Expr) extends Expr
  case class Concat(e1: Expr, e2: Expr) extends Expr

  // Variables and let-binding
  case class Var(x: Variable) extends Expr
  case class Let(x: Variable, e1: Expr, e2: Expr) extends Expr
  case class LetFun(f: Variable, arg: Variable, ty: Type, e1:Expr, e2:Expr)
      extends Expr
  case class LetRec(f: Variable, arg: Variable, xty: Type, ty: Type, e1:Expr, e2:Expr)
      extends Expr
  case class LetPair(x: Variable,y: Variable, e1:Expr, e2:Expr) extends Expr

  // Pairing
  case class Pair(e1: Expr, e2: Expr) extends Expr
  case class First(e: Expr) extends Expr
  case class Second(e: Expr) extends Expr

  // Functions
  case class Lambda(x: Variable, ty: Type, e: Expr) extends Expr
  case class Apply(e1: Expr, e2: Expr) extends Expr
  case class Rec(f: Variable, x: Variable, tyx:Type, ty: Type, e: Expr) extends Expr

  // Values
  abstract class Value
  case class NumV(n: Integer) extends Value
  case class BoolV(n: Boolean) extends Value
  case class StringV(s: String) extends Value
  case class PairV(v1: Value, v2: Value) extends Value
  case class ClosureV(env: Env[Value], x: Variable, e: Expr) extends Value
  case class RecV(env: Env[Value], f:Variable, x: Variable, e: Expr) extends Value

  // Types
  abstract class Type
  case object IntTy extends Type
  case object BoolTy extends Type
  case object StringTy extends Type
  case class PairTy(ty1: Type, ty2: Type) extends Type
  case class FunTy(ty1: Type, ty2: Type) extends Type


  // ======================================================================
  // Part 1: Interpretation
  // ======================================================================

  // ======================================================================
  // Exercise 1: Primitive operations
  // ======================================================================

  
  object Value {
    // utility methods for operating on values
    def add(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => NumV (v1 + v2)
      case _ => sys.error("arguments to addition are non-numeric")
    }

    def subtract(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => NumV (v1 - v2)
      case _ => sys.error("arguments to addition are non-numeric")
    }

    // Multiplication
    def multiply(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 * v2)
      case _ => sys.error("arguments to multiplication are non-numeric")
    }

    // Equality Testing
    def eq(v1: Value, v2: Value): Value = (v1,v2) match {
      // Two Booleans
      case (BoolV(b1), BoolV(b2)) => BoolV(b1 == b2)

      // Two Numerics
      case (NumV(n1), NumV(n2)) => BoolV(n1 == n2)

      // Two Strings
      case (StringV(s1), StringV(s2)) => BoolV(s1 == s2)

      // Everything else (= error)
      case _ => sys.error("Only equality testing for booleans, strings, and numerics supported")
    }

    // String Length
    def length(v: Value): Value = v match {
      case StringV(s) => NumV(s.length)
      case _ => sys.error("Argument passed to length function was not a string!")
    }

    // String Indexing
    def index(v1: Value, v2: Value): Value = (v1,v2) match {
      case (StringV(s), NumV(n)) => StringV((s.apply(n)).toString)
      case _ => sys.error("incorrect types passed to index function")
    }

    // String Concatenation
    def concat(v1: Value, v2: Value): Value = (v1,v2) match {
      case (StringV(s1), StringV(s2)) => StringV(s1.concat(s2))
      case _ => sys.error("incorrect types passed to concat function")
    }
  }
  
  // ======================================================================
  // Exercise 2: Evaluation
  // ======================================================================

  def eval (env: Env[Value], e: Expr): Value = e match {
    // Arithmetic
    case Num(n) => NumV(n)
    case Plus(e1,e2) => 
      Value.add(eval(env,e1),eval(env,e2))
    case Minus(e1,e2) => 
      Value.subtract(eval(env,e1),eval(env,e2))
    case Times(e1,e2) =>
      Value.multiply(eval(env,e1),eval(env,e2))
      
    // Booleans
    case Bool(b) => BoolV(b)
    case Eq(e1,e2) => 
      Value.eq(eval(env,e1), eval(env,e2))
    case IfThenElse(e,e1,e2) =>
      eval(env,e) match {
        case BoolV(true) => eval(env,e1)
        case BoolV(false) => eval(env,e2)
        case _ => sys.error("the expression didn't evaluate to a Boolean!")
      }

    // Strings
    case Str(s) => StringV(s)
    case Length(e) =>
      Value.length(eval(env,e))
    case Index(e1,e2) =>
      Value.index(eval(env,e1), eval(env,e2))
    case Concat(e1,e2) =>
      Value.concat(eval(env,e1), eval(env,e2))

    // Variable (evaluation) && let case??
    case Var(x) => env(x)
    case Let(x,e1,e2) => eval(env + (x -> eval(env,e1)), e2)

    // Pairs
    case Pair(e1,e2) => PairV(eval(env,e1), eval(env,e2))
    case First(e) => 
      eval(env,e) match {
        case PairV(x,_) => x
        case _ => sys.error("could not get first element of the pair")
      }
    case Second(e) =>
      eval(env,e) match {
        case PairV(_,y) => y
        case _ => sys.error("coult not get second element of the pair")
      }

    // Other Functions
    // ==============================================================
    //                      DO NOT UNDERSTAND
    // I was able to answer these questions in this section by looking 
    // at last year's assignment for help. 
    // ==============================================================
    case Lambda(x,_,e) => ClosureV(env,x,e)
    case Rec(f,x,_,_,e) => RecV(env,f,x,e)
    case Apply(e1,e2) =>
      eval(env,e1) match {
        case ClosureV(closEnv, x, closExp) =>
          eval(closEnv + (x -> eval(env,e2)), closExp)
        case RecV(recEnv, f, x, recExp) =>
          eval(recEnv + (f -> RecV(recEnv,f,x,recExp)) + (x -> eval(env,e2)), recExp)
        case _ => sys.error("the first argument passed to Apply must be a function!")
      }
  }


  // ======================================================================
  // Part 2: Typechecking
  // ======================================================================

  // ======================================================================
  // Exercise 3: Typechecker
  // ======================================================================

  // typing: calculate the return type of e, or throw an error
  def tyOf(ctx: Env[Type], e: Expr): Type = e match {

    // Arithmetic
    case Num(n) => IntTy
    case Plus(e1,e2) => 
      (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (IntTy, IntTy) => IntTy
        case _ => sys.error("non-integer arguments to +") 
      }
    case Minus(e1,e2) =>
      (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (IntTy, IntTy) => IntTy
        case _ => sys.error("non-integer arguments to -") 
      }
    case Times(e1,e2) =>
      (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (IntTy, IntTy) => IntTy
        case _ => sys.error("non-integer arguments to *") 
      }

    // Booleans
    case Bool(b) => BoolTy
    case Eq(e1,e2) =>
      (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (b1, b2) =>
          if ( b1 == b2 ) {BoolTy}
          else {sys.error("Types of Eq must be equal!!")}
      }
    case IfThenElse(e,e1,e2) =>
      (tyOf(ctx,e),tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (BoolTy, b1, b2) => 
          if ( b1 == b2 ) {BoolTy}
          else {sys.error("Types of Eq must be equal!!")}
        case (_, b1, b2) => sys.error("The expression didn't evaluate to Boolean!")
      }

    // Strings
    case Str(s) => StringTy
    case Length(e) => 
      tyOf(ctx,e) match {
        case StringTy => IntTy
        case _ => sys.error("the Length function can only be applied to a string!")
      }
    case Index(e1,e2) =>
      (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (StringTy, IntTy) => StringTy
        case (s1,s2) => sys.error("the Index function can only be applied to a string and int")
      }
    case Concat(e1,e2) =>
      (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (StringTy, StringTy) => StringTy
        case _ => sys.error("the Concat function can only be applied to two strings")
      }

    // Variables and let-binding
    case Var(x) => ctx(x)
    case Let(x,e1,e2) => tyOf(ctx + (x -> (tyOf(ctx,e1))), e2)
    case LetPair(x,y,e1,e2) =>
      (tyOf(ctx,e1)) match {
        case PairTy(a,b) => tyOf( ctx + (x -> a) + (y -> b), e2)
        case _ => sys.error("the first argument passed to LetPair must be a pair")
      }
    case LetFun(f,x,ty,e1,e2) => {
      val ty2 = tyOf(ctx + (x -> ty), e1);
      tyOf(ctx + (f -> FunTy(ty, ty2)), e2);
    }
    case LetRec(f,x,ty1,ty2,e1,e2) => {
      val fty = FunTy(ty1,ty2);
      if ( tyOf(ctx + (x -> ty1) + (f -> fty), e1) == ty2) {
        tyOf(ctx + (f -> fty), e2)
      }
      else {
        sys.error("the type of the passed recursive function does not match")
      }
    }

    // Pairs
    case Pair(e1,e2) => PairTy(tyOf(ctx,e1), tyOf(ctx,e2))
    case First(e) =>
      (tyOf(ctx,e)) match {
        case PairTy(p1, p2) => p1
        case _ => sys.error("the argument passed to the First function must be a pair")
      }
    case Second(e) =>
      (tyOf(ctx,e)) match {
        case PairTy(p1, p2) => p2
        case _ => sys.error("the argument passed to the Second function must be a pair")
      }

    // Functions
    // ==============================================================
    //                      DO NOT UNDERSTAND
    // I was able to answer these questions by looking at last year's
    // assignment for help. 
    // ============================================================== 
    case Lambda(x,ty,e) => FunTy( ty, tyOf(ctx + (x -> ty), e) )
    case Rec(f,x,xty,ty,e) =>
      tyOf(ctx + (f -> FunTy(xty,ty)) + (x -> xty), e) match {
        case body =>
          if (ty == body) {FunTy(xty,ty)}
          else {sys.error("Types do not match!")}
      }
    case Apply(e1,e2) =>
      (tyOf(ctx,e1), tyOf(ctx,e2)) match {
        case (FunTy(f1,f2), f3) => 
          if (f1 == f3) {f2}
          else {sys.error("Types do not match!")}
        case (f1, f2) => sys.error("Types passed to Apply function not recognized by Giraffe") 
      }

  }


    // ======================================================================
  // Part 3: Syntactic transformation
  // ======================================================================

  // ======================================================================
  // Exercise 4: Capture-avoiding substitution
  // ======================================================================

  // This object provides a method to generate a "fresh" variable name
  object Gensym {
    private var id = 0
    def gensym(s: Variable): Variable = {
      val fresh_s = s + "_" + id
      id = id + 1
      fresh_s
    }
  }

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
      case Num(n) => Num(n)
      case Plus(t1,t2) => Plus(go(t1),go(t2))
      case Minus(t1,t2) => Minus(go(t1),go(t2))
      case Times(t1,t2) => Times(go(t1),go(t2))

      case Bool(b) => Bool(b)
      case Eq(t1,t2) => Eq(go(t1),go(t2))
      case IfThenElse(t,t1,t2) => IfThenElse(go(t),go(t1),go(t2))

      case Str(s) => Str(s)
      case Length(t) => Length(go(t))
      case Index(t1,t2) => Index(go(t1),go(t2))
      case Concat(t1,t2) => Concat(go(t1),go(t2))

      case Var(x) => Var(swapVar(x,y,z))
      case Let(x,t1,t2) => Let(swapVar(x,y,z),go(t1),go(t2))
      case LetFun(f,x,ty,t1,t2) => LetFun(swapVar(f,y,z),swapVar(x,y,z),ty,go(t1),go(t2))
      case LetRec(f,x,xty,ty,t1,t2) => LetRec(swapVar(f,y,z),swapVar(x,y,z),xty,ty,go(t1),go(t2))
      case LetPair(x1,x2,t1,t2) => LetPair(swapVar(x1,y,z),swapVar(x2,y,z),go(t1),go(t2))

      case Pair(t1,t2) => Pair(go(t1),go(t2))
      case First(t) => First(go(t))
      case Second(t) => Second(go(t))


      case Lambda(x,ty,t) => Lambda(swapVar(x,y,z),ty,go(t))
      case Apply(t1,t2) => Apply(go(t1),go(t2))
      case Rec(f,x,xty,ty,t) => Rec(swapVar(f,y,z), swapVar(x,y,z), xty,ty,go(t))

    }
    go(e)
  }

  def subst(e1:Expr, e2:Expr, x: Variable): Expr =
    e1 match {
      // Arithmetic
      case Num(e) => Num(e)
      case Plus(t1,t2) => Plus(subst(t1,e2,x),subst(t2,e2,x))
      case Minus(t1,t2) => Minus(subst(t1,e2,x),subst(t2,e2,x))
      case Times(t1,t2) => Times(subst(t1,e2,x),subst(t2,e2,x))

      // Booleans
      case Bool(b) => Bool(b)
      case Eq(b1,b2) => Eq(subst(b1,e2,x), subst(b2,e2,x))
      case IfThenElse(b, b1, b2) => IfThenElse(subst(b,e2,x), subst(b1,e2,x), subst(b2,e2,x)) 

      // Strings
      case Str(s) => Str(s)
      case Length(e) => Length(subst(e,e2,x))
      case Index(s1,s2) => Index(subst(s1,e2,x), subst(s2,e2,x))
      case Concat(s1,s2) => Concat(subst(s1,e2,x), subst(s2,e2,x))

      // Variables + let-binding
      case Var(y) =>
        if (x == y) {e2} 
        else {Var(y)}
      case Let(y,t1,t2) => {
        val z = Gensym.gensym(y);
        val fresh_t2 = swap(t2,y,z);
        Let(z,subst(t1,e2,x),subst(fresh_t2,e2,x))
      }

      // Pairs 
      case Pair(t1,t2) => Pair(subst(t1,e2,x), subst(t2,e2,x))
      case First(e) => First(subst(e,e2,x))
      case Second(e) => Second(subst(e,e2,x))

      // Functions 
      // ==================================================================
      //                      DO NOT UNDERSTAND
      // I was able to write the basic strucutre on my own, just not the 
      // execution, I especially was unsure how to use the Gensym variables
      // I was able to answer these questions by looking at last year's
      // assignment for help. 
      // ==================================================================
      
      // Lambda and Rec are incomplete!!
      // sorry :( 
      case Lambda(y,a,e) =>
        if (x == y) {Lambda(y,a,e)}
        else {
          /*
          if () { // couldn't figure out how to see if e2 contains y ??
            val z = Gensym.gensym(y)
            Lambda(z, a, subst( subst(e,Var(z),y), e2, x))
          }
          else {
          */
            Lambda(y, a, subst(e, e2, x))
          //}
        }
      case Rec(f,y,a,b,e) =>
        if ( x==f || x==y ) {
          Rec(f,y,a,b,e)
        }
        else {
          /*
          if () { // couldn't figure out how to see if e2 contains y, or f ??
            val yz = Gensym.gensym(y)
            val fz = Gensym.gensym(f)
            Rec(fz, yz, a, b, subst(subst(subst(e, Var(yz), y), Var(fz), f), e2, x))
          }
          else {
          */
            Rec(f, y, a, b, subst(e, e2, x))
          //}
        }

      case LetPair(y1,y2,t1,t2) => {
        val y1z = Gensym.gensym(y1);
        val y2z = Gensym.gensym(y2);
        LetPair(y1z,y2z,subst(t1,e2,x),
          subst(subst(subst(t2,Var(y1z),y1), Var(y2z), y2), e2,x))
      }

      case LetFun(f,y,ty,t1,t2) => {
        val fz = Gensym.gensym(f);
        val yz = Gensym.gensym(y);
        LetFun(fz,yz,ty,subst(subst(t1,Var(yz),y),e2,x),
          subst(subst(t2,Var(fz),f), e2,x))
      }

      case LetRec(f,y,ty1,ty2,t1,t2) => {
        val fz = Gensym.gensym(f);
        val yz = Gensym.gensym(y);
        LetRec(fz,yz,ty1,ty2,subst(subst(subst(t1,Var(fz),f),Var(yz),y),e2,x),
          subst(subst(t2,Var(fz),f), e2,x))
      }

    }

  // ======================================================================
  // Exercise 5: Desugaring let fun, let rec and let pair
  // ======================================================================

  def desugar(e: Expr): Expr = e match {

    case Num(n) => Num(n)
    case Plus(e1,e2) => Plus(desugar(e1),desugar(e2))
    case Minus(e1,e2) => Minus(desugar(e1),desugar(e2))
    case Times(e1,e2) => Times(desugar(e1),desugar(e2))

    // Booleans
    case Eq(e1,e2) => Eq( desugar(e1), desugar(e2) )
    case IfThenElse(conditional,e1,e2) => IfThenElse( desugar(conditional), desugar(e1), desugar(e2) )

    // Strings
    case Length(e) => Length( desugar(e) )
    case Index(e1,e2) => Index( desugar(e1), desugar(e2) )
    case Concat(e1,e2) => Concat( desugar(e1), desugar(e2) )

    // let-binding
    case Let(x,e1,e2) => Let( x, desugar(e1), desugar(e2) ) // don't need to desugar x! 

    // Pairs
    case Pair(e1,e2) => Pair( desugar(e1), desugar(e2) )
    case First(e) => First( desugar(e) )
    case Second(e) => Second( desugar(e) )

    // Functions
    //  ===================================================================================
    //  Let-Pair was the only Function in this section I couldn't get on my own. I am still
    //  confused by the use of the p Gensym variable as used in Last Year's assignment.
    //  However, I left it because it fixed the bug in my code.
    //  ===================================================================================
    case LetPair(x,y,e1,e2) => {
      val p = Gensym.gensym("p") // ??
      Let(p, desugar(e1), subst(subst(desugar(e2), First(Var(p)),x), Second(Var(p)),y))
    }
    case LetFun(f,arg,ty,e1,e2) => {
      Let(f, Lambda(arg, ty, desugar(e1)), desugar(e2))
    }
    case LetRec(f,arg,xty,ty,e1,e2) => {
      Let(f, Rec(f, arg, xty, ty, desugar(e1)), desugar(e2))
    }

    case Lambda(x,ty,e) => Lambda(x, ty, desugar(e))
    case Apply(e1,e2) => Apply( desugar(e1), desugar(e2) )
    case Rec(f,x,xty,ty,e) => Rec(f, x, xty, ty, desugar(e) )

    // single bool, num, str, and var cases
    case e => e


  }


  // ======================================================================
  // Some simple programs
  // ======================================================================

  // The following examples illustrate how to embed Giraffe source code into
  // Scala using multi-line comments, and parse it using parser.parseStr.

  // Example 1: the swap function
  def example1: Expr = parser.parseStr("""
    let fun swap(x:int * int) = (snd(x), fst(x)) in 
    swap(42,17)
    """)

  // Example 2: the factorial function, yet again
  def example2: Expr = parser.parseStr("""
    let rec fact(n:int):int = 
      if (n == 0) then 1 else n * fact(n - 1) in 
    fact(5)
    """)

  // Example 3: exponentiation
  def example3: Expr = parser.parseStr("""
    let rec power(input: int * int):int =
          let (x,n) = input in
          if (n == 0) then 1 else
          x * power(x,n-1)
        in
        power(2,10)
    """)

  // Example 4: check whether two strings have the same last character
  def example4: Expr = parser.parseStr("""
    let fun sameLastChar(input: str * str) = 
      let (s1,s2) = input in 
      index(s1,length(s1)-1) == index(s2,length(s2)-1)
    in sameLastChar("abcz","abcdefghijklmnopqrstuvwxyz")
    """)

  // Example 5: String slice
  def example5: Expr = parser.parseStr("""
    let rec slice(input: str * (int * int)) : str = 
      let (s,p) = input in
      let (base,len) = p in
      if (len == 0) then ""
      else concat(index(s,base), slice (s,(base + 1, len - 1)))
    in slice("abcdefghijklmnopqrstuvwxyz", (10, 10))""")

  // Example 6: Integer comparison
  def example6: Expr = parser.parseStr("""
    let comment = "Assumes nonnegative arguments" in 
    let rec lessthanorequal(nm: int * int) : bool = 
      let (n,m) = nm in 
      if (n == 0) then true 
      else if (m == 0) then false
      else lessthanorequal(n-1,m-1)
    in (lessthanorequal (5,6), lessthanorequal(6,5))""")




  /*======================================================================
   The rest of this file is support code, which you should not (and do not
   need to) change.
   ====================================================================== */

  class GiraffeParser extends StandardTokenParsers with PackratParsers {

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
      "int","str","bool","true","false","fst","snd","concat",
      "index","length","fun"
    )
    lexical.delimiters += ("=","*", "\\", "+", "-", "(", ")", "==", ":", ".",
      "->", ","
    )

    lazy val expression: P[Expr] =
      simpleExpr

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

    lazy val letRec: P[Expr] =
      ("let" ~ "rec" ~> ident) ~ ("(" ~> ident) ~
        (":" ~> typ <~ ")") ~ (":" ~> typ) ~ ("=" ~> expression) ~
        ("in" ~> expression) ^^ {
          case fun~binder~xty~ty~e1~e2 => LetRec(fun,binder,xty,ty,e1,e2)
        }

    lazy val letPair: P[Expr] =
      ("let" ~ "(") ~> ident ~ ("," ~> ident <~ ")") ~
        ("=" ~> expression) ~ ("in" ~> expression) ^^ {
          case x~y~e1~e2 => LetPair(x,y,e1,e2)
        }

    lazy val typ: P[Type] =
      funTyp 

    lazy val funTyp: P[Type] =
      pairTyp ~ "->" ~ funTyp ^^ {
        case t1~"->"~t2 => FunTy(t1,t2)
      } | pairTyp

    lazy val pairTyp: P[Type] =
      primitiveType ~ "*" ~ pairTyp ^^ {
        case t1~"*"~t2 => PairTy(t1,t2)
      } | primitiveType

    lazy val primitiveType: P[Type] =
      "bool" ^^^ BoolTy | "int" ^^^ IntTy | "str" ^^^ StringTy |  "("~>typ<~")"

    lazy val operations: P[Expr] =
      application | 
      ("fst" ~ "(") ~> expression <~ ")" ^^ (x => First(x)) |
        ("snd" ~ "(") ~> expression <~ ")" ^^ (x => Second(x)) |
        ("length" ~ "(") ~> expression <~ ")" ^^ (x => Length(x)) |
        ("concat"  ~ "(") ~> expression ~ ("," ~> expression) <~ ")" ^^ {
          case e1~e2 => Concat(e1,e2)
        } |
        ("index" ~ "(") ~> expression ~ ("," ~> expression) <~ ")" ^^ {
          case e1~e2 => Index(e1,e2)
        }

    lazy val arith: P[Expr] =
      eq

    lazy val prod: P[Expr] =
      prod ~ "*" ~ fact ^^ {
        case e1~"*"~e2 => Times(e1,e2)
      } | fact

    lazy val summation: P[Expr] =
      summation ~ "+" ~ prod ^^ {
        case e1~"+"~e2 => Plus(e1,e2)
      } | summation ~ "-" ~ prod ^^ {
        case e1~"-"~e2 => Minus(e1,e2)
      } | prod

    lazy val eq: P[Expr] =
      simpleExpr ~ "==" ~ summation ^^ {
        case e1~"=="~e2 => Eq(e1,e2)
      } | summation

    lazy val application: P[Expr] =
      fact ~ fact ^^ {
        case e1~e2 => Apply(e1,e2)
      }

    lazy val simpleExpr: P[Expr] = (
      lambda |
        rec |
        letExpr |
        letFun |
        letRec |
        letPair |
        ifExpr |
        arith |
        fact
    )

    lazy val pairLit: P[Expr] =
      "(" ~> expression ~ "," ~ expression <~ ")" ^^ {
        case t1~","~t2 => Pair(t1,t2)
      }

    lazy val fact: P[Expr] = (
      operations |
        pairLit |
        (ident ^^ Var) |
        (numericLit ^^ { x => Num(x.toInt) }) |
        (stringLit ^^ Str) |
        ("true" ^^^ Bool(true)) |
        ("false" ^^^ Bool(false)) |
        "("~>expression<~")"
    )

  }


  val parser = new GiraffeParser

  
  object Main {
    def typecheck(ast: Expr):Type =
      tyOf(Map.empty,ast);

    def evaluate(ast: Expr):Value =
      eval(Map.empty,ast)



    def showResult(ast: Expr) {
      println("AST:  " + ast.toString + "\n")

      try {
        print("Type Checking...");
        val ty = typecheck(ast);
        println("Done!");
        println("Type of Expression: " + ty.toString + "\n") ;
      } catch {
          case e:Throwable => println("Error: " + e)
      }
      try {
        println("Desugaring...");
        val core_ast = desugar(ast);
        println("Done!");
        println("Desugared AST: " + core_ast.toString + "\n") ;

        println("Evaluating...");
        println("Result: " + evaluate(core_ast))
      } catch {
        case e:Throwable => {
          println("Error: " + e)
          println("Evaluating raw AST...");
          println("Result: " + evaluate(ast))
        }
      }
     
    }

    def start(): Unit = {
      println("Welcome to Giraffe! (V1.0, October 5, 2016)");
      println("Enter expressions to evaluate, :load <filename.gir> to load a file, or :quit to quit.");
      println("This REPL can only read one line at a time, use :load to load larger expressions.");
      repl()
    }

    def repl(): Unit = {
      print("Giraffe> ");
      val input = scala.io.StdIn.readLine();
      if(input == ":quit") {
        println("Goodbye!")
      }
      else if (input.startsWith(":load")) {
        try {
          val ast = parser.parse(input.substring(6));
          showResult(ast)
        } catch {
          case e:Throwable => println("Error: " + e)
        }
        repl()
      } else {
        try {
          val ast = parser.parseStr(input);
          showResult(ast)
        } catch {
          case e:Throwable => println("Error: " + e)
        }
        repl()
      }
    }

  }
  def main( args:Array[String] ):Unit = {
    if(args.length == 0) {
      Main.start()
    } else {
      try {
        print("Parsing...");
        val ast = parser.parse(args.head)
        println("Done!");
        Main.showResult(ast)
      } catch {
        case e:Throwable => println("Error: " + e)
      }
    }
  }
}


