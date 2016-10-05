import scala.collection.immutable.ListMap

/* Part 2 */

def incr(x: Int): Int = x + 1
def double(x: Int): Int = x + x
def square(x: Int): Int = x * x

def factorial(n: Int): Int =
  if (n == 0) {1} else {n * factorial(n-1)}

def power(x: Int, n: Int): Int =
  if (n == 0) {1} else {x * power(x,n-1)}

def factorial1(n: Int): Int = {
  val m = n-1 ; if (n == 0) {1} else {n * factorial1(m)}
}


def factorial2(n: Int): Int = {
  val m = n-1;                                // you don't need the semi-colon if it's on a new line
  if (n == 0) {1} else {n * factorial2(m)}    // Scala will try to guess where the semi-colon goes,
}                                             // better to be specific


def factorial3(n: Int): Int = {
  val m = n-1;
  if (n == 0) {
    return 1;
  } 
  else {
    return n * factorial3(m);
  }
}

/* Exercise 1 */
def p(x: Int, y:Int): Int = {
  val a = x * x;
  val b = 2 * x * y;
  val c = y * y * y;
  return a + b + c - 1;
}

/* Exercise 2 */
def sum(n: Int): Int = {
  if (n == 0) {0} else {n + sum(n-1)}
}

/* Part 3 */

/* Exercise 3 */
//def cycle(q:(Int,Int,Int)): (Int,Int,Int) = {}
// alternative form:
def cycle(x: Int,y: Int,z: Int): (Int,Int,Int) = {
  return (y,z,x)
}

/* Part 4 */


// for each number (integer n) , do pattern matching
// look at this number see if it matches any of the expressions listed, if so
// return the expression on the right
def nameFromNum(presidentNum: Int): String = presidentNum match {
  case 41 => "George H. W. Bush"
  case 42 => "Bill Clinton"
  case 43 => "George W. Bush"
  case 44 => "Barack Obama"
  case _ => "I don't know"  // DEFAULT CASE, matches anything
  case n => "I don't know who the " + n + "president is"
}

def numFromName(presidentName: String): Int = presidentName match {
  case "George H. W. Bush" => 41
  case "Bill Clinton" => 42
  case "George W. Bush" => 43
  case "Barack Obama" => 44
}

/* Exercise 4 */
def suffix(n: Int): String = {
  var i = n % 10;
  var j = n % 100;
  if ( (i==1) && (j!=11) ) {
    return "st"
  }
  if ( (i==2) && (j!=12) ) {
    return "nd"
  }
  if ( (i==3) && (j!=13) ) {
    return "rd"
  }
  return "th"
}


abstract class Colour
case class Red() extends Colour
case class Green() extends Colour
case class Blue() extends Colour

/* Exercise 5 */
def favouriteColour(c: Colour): Boolean = c match { 
  case Red() => false
  case Blue() => false
  case Green() => true
}


abstract class Shape
case class Circle(r: Double, x: Double, y: Double) extends Shape
case class Rectangle(llx: Double, lly: Double, w:Double, h:Double) extends Shape

def center(s: Shape): (Double,Double) = s match {
  case Rectangle(llx,lly,w,h) => (llx+w/2, lly+h/2)
  case Circle(r,x,y) => (x,y)
}

/* Exercise 6 */
def boundingBox(s: Shape): Rectangle = s match{
  case Rectangle(llx,lly,w,h) => Rectangle(llx,lly,w,h)
  case Circle(r,x,y) => Rectangle(r-x, r-y, 2*r, 2*r)
}

/* Exercise 7 */
def horizontalOverlap( rectA: Rectangle, rectB: Rectangle ): Boolean = {
  if ( ((rectA.llx + rectA.w) < rectB.llx) || ((rectB.llx + rectB.w) < rectA.llx) ) {
    return false
  }
  else {
    return true
  }
}

def verticalOverlap( rectA: Rectangle, rectB: Rectangle ): Boolean = { 
  if (( rectA.lly < (rectB.lly+rectB.h) ) || ( rectB.lly < (rectA.lly + rectA.h) )) {
    return false
  }
  else {
    return true
  }
}

def mayOverlap(s1: Shape, s2: Shape): Boolean = {
  var RectA = boundingBox(s1)
  var RectB = boundingBox(s2)

  if ( verticalOverlap(RectA,RectB) || horizontalOverlap(RectA,RectB) ) {
    return true
  }
  else {
    return false
  }
}


/* Part 5 */

val anonIncr = {x: Int => x+1} // anonymous version of incr
val anonAdd = {x: Int => {y: Int => x + y}}

/* Exercise 8 */
def compose1[A, B, C](f: A => B, g: B => C)(x:A): C = g(f(x)) 

/* Exercise 9 */
def compose[A, B, C](f: A => B, g: B => C) = {x: A => g(f(x))}

/* Exercise 10 */
def e1(x: Int): String = {
  return x.toString
}

def e2(x: String): Boolean = {
  if (x.length() > 2) {return false}
  else {return true}
}

/*  Solution to Excercise 10
    scala> compose(e1,e2)
    res37: Int => Boolean = <function1>

    scala> compose(e2,e1)
    <console>:15: error: type mismatch;
      found   : Int => String
      required: Boolean => String
        compose(e2,e1)
                   ^
*/

def isEmpty[A](l: List[A]) = l match { 
  case Nil => true
  case x :: y => false
}

def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
  case Nil => l2
  case x :: xs => x :: append(xs, l2)
}

/* Exercise 11 */
def map[A, B](f: A => B, l: List[A]): List[B] = l match {
  case Nil => List()
  case x :: xs => f(x) :: map( f, xs )
}

/* Exercise 12 */
def filter[A](f: A => Boolean, l: List[A]): List[A] = l match {
  case Nil => List()
  case x :: xs => 
    if (f(x)) { x :: filter(f,xs) }
    else { filter(f,xs) }
}

/* Exercise 13 */
def reverse[A](l: List[A]): List[A] = l match {
  case Nil => List()
  case x :: xs => append( reverse(xs) , List(x)) 
}


/* Part 6 */

def empty[K,V]: List[(K,V)] = List()

/* Exercise 14 */
def lookup[K, V](m: List[(K, V)], k: K): V = m match {
  case Nil => sys.error("Map empty!")
  case (key,value) :: map2 => 
    if (key == k) {return value}
    else {lookup(map2,k)}
}

/* Exercise 15 */
def update[K, V](m: List[(K, V)], k: K, v: V): List[(K, V)] = m match {
  case Nil => List((k,v))
  case (key,value) :: map2 =>
    if (key == k) { (k,v) :: map2 }
    else { (key,value) :: update(map2,k,v) }
}

/* Exercise 16 */
def keys[K,V](m: List[(K,V)]): List[K] = {
  map( {x:(K,V) => x._1}, m)
}

/* Exercise 17 */
val presidentListMap = ListMap(
  41 -> "George H. W. Bush"
  42 -> "Bill Clinton"
  43 -> "George Bush"
  44 -> "Barack Obama")

/* Exercise 18 */
def m0_withUpdate = ListMap(1 -> "a", 2 -> "b")

/* Exercise 19 */
def list2map[K,V](l: List[(K,V)]): ListMap[K,V] = l match {
  case Nil => ListMap[K,V]()
  case (key,value) :: list => list2map(list) + (key -> value)
}

/* Exercise 20 */
def election(votes: List[String]): ListMap[String,Int] = votes match {
  case Nil => ListMap[String,Int]()
  case k :: m =>
    val result = election(m)
    if (result.contains(k)) { result + (k -> (result(k)+1)) }
    else { result + (k -> 1) }
}
