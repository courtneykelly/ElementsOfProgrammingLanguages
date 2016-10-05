/*	Courtney Kelly
 *	Elements of Programming Languages
 *	October 4, 2016
 *
 *	Tutorial 1
*/

import scala.collection.immutable.ListMap

/*	1. 	Pattern matching. For this problem, you should use the Scala definition of
 *		LArith abstract syntax trees presented in the lectures:
 *		
 *		abstract class Expr
 *		case class Num(n: Integer) extends Expr
 *		case class Plus(e1: Expr, e2: Expr) extends Expr
 *		case class Times(e1: Expr, e2: Expr) extends Expr
*/

/* 	(a) Write a Scala function evens[A]: List[A] => List[A] that traverses
 *		a list and returns all of the elements in even-numbered positions. For example,
 *		evens(List(’a’,’b’,’c’,’d’,’e’,’f’)) = List(’a’,’c’,’e’).
 *		The solution should use pattern-matching rather than indexing into the
 *		list.
*/
	
def evens[A](l: List[A]): List[A] = l match {
  case Nil => List()
  case x :: Nil => List(x)
  case x :: y :: xs =>  x :: evens(xs)
}

/*	(b) Write a Scala function allplus: Expr => Boolean that traverses a LArith
 *		term and returns true if all of the operations in it are additions, false
 *		otherwise. (For this problem, you may want to use the Scala Boolean
 *		AND operation &&.)
*/

/*	(c) Write Scala function consts: Expr => List[Int] that traverses a LArith
 *		expression and constructs a list containing all of the numerical constants
 *		in the expression. (For this problem, you may want to use the Scala listappend
 *		operation ++.)
*/

/*	(d) Write Scala function revtimes: Expr => Expr that traverses a LArith
 *		expression and reverses the order of all multiplication operations (i.e.
 *		e1 × e2 becomes e2 × e1).
*/

/*	(e) (*) Write a Scala function printExpr: Expr => String that traverses
 *		an expression and converts it into a (fully parenthesised) string. For
 *		example:
 *			scala> printExpr( Times(Plus(Num(1), Num(2)), Times(Num(3), Num(4))))
 *			res0: String = ((1 + 2) * (3 * 4))
*/

/*	2. 	Evaluation derivations. Recall the evaluation rules covered in lectures:
 *		Write out derivation trees for the following expressions:
*/

/*		(a) 6 × 9
*/

/*		(b) 3 × 3 + 4 × 4 == 5 × 5
*/

/*		(c) (?) if 1 + 1 == 2 then 2 + 3 else 2 ∗ 3
*/

/*		(d) (?) (if 1 + 1 == 2 then 3 else 4) + 5
*/

/*	3. 	Typechecking derivations. Recall the typechecking rules covered in lectures:
 *		Write out typing derivations for the following judgments:
*/

/*		(a) ` 6 × 9 : int
*/

/*		(b) (*) ` (if 1 + 1 == 2 then 3 else 4) + 5 : int
*/

/*	4. 	(*) Nondeterminism. Suppose we add the following construct e1e2 to LIf:
 *		Informally, the semantics of e1e2 is that we evaluate either e1 or e2 nondeterministically.
*/

/*		(a) What property of LArith (among those discussed in Lecture 2) is violated
 *		after we add ?
 */

/*		(b) Write a sensible rule for typechecking e1e2.
*/

/*		(c) For each of the following expressions e, list all of the possible values v
 *		such that e ⇓ v is derivable:
*/	

/* 			i. (12) × (34)
*/
			
/*			ii. if (truefalse) then 1 else 2
*/

/* 		(d) Define an expression e and a value v such that there are two different
 *		derivations of the judgment e ⇓ v. (What does it mean for the derivations
 *		to be different?)
*/ 

