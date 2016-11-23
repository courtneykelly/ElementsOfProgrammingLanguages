package Assignment3.TurtleEDSL
import Assignment3.GraphicsCanvas._
import scala.language.implicitConversions
import java.awt.Color
import java.util.Random

object Assignment3Embedded {

  /****************
   *  Exercise 1  *
   ****************/

  trait TurtleDSL {
    type TurtleGraphics

    // Creates a new TurtleGraphics instance
    val empty: TurtleGraphics

    // Do action tg1, then do action tg2
    def append(tg1: TurtleGraphics, tg2: TurtleGraphics): TurtleGraphics

    // Puts the pen up: no lines will be drawn when moving
    def penUp(): TurtleGraphics

    // Puts the pen down: lines will be drawn when moving
    def penDown(): TurtleGraphics

    // Goes forward by `distance`
    def forward(distance: Integer): TurtleGraphics

    // Goes backward by `distance`
    def backward(distance: Integer): TurtleGraphics = {
      forward(-distance)
    }

    // Turns right by `amount` degrees
    def right(amount: Integer): TurtleGraphics

    // Turns left by `amount` degrees
    def left(amount: Integer): TurtleGraphics = {
      right(360 - (amount % 360))
    }

    // Sets the line color to `color`.
    def setColor(color: Color): TurtleGraphics

    // Sets the line colour to a colour randomly picked from `cols`.
    def setRandomColor(cols: List[Color]): TurtleGraphics

    // Calculates an angle difference modulo 360. So:
    // calculateAngleDiff(359, 5) = 4
    // calculateAngleDiff(4, -5) = 359
    def calculateAngleDiff(angle: Integer, diff: Integer): Integer = {
      val diff1 = ((angle+diff) % 360)
      if (diff1>=0) {
        return diff1
      }
      else {
        return 360 + diff1
      }
    }

    // Given initial co-ordinates `x0` and `y0`, an angle `angle`, and a distance `distance`,
    // returns a pair of the calculated new co-ordinates.
    def calculateNewCoords(x0: Integer, y0: Integer,
      angle: Integer, distance: Integer): (Integer, Integer) = {
      var x1 = (x0 + (distance * math.sin( math.toRadians(angle.toDouble) ))).toInt
      var y1 = (y0 - (distance * math.cos( math.toRadians(angle.toDouble) ))).toInt
      return (x1,y1)
    }

    // Saves the turtle graphics given by `tg` to `filename`.
    def draw(tg: TurtleGraphics, width: Integer, height: Integer): GraphicsCanvas

    // Saves the turtle graphics given by `tg` to `filename`.
    def saveToFile(tg: TurtleGraphics, width: Integer, height: Integer, filename: String): Unit = {
      val canvas = draw(tg,width,height)
      canvas.saveToFile(filename)
    }

    // Scala magic to allow <> infix operator. No implementation needed here -- you can ignore this.
    class AppendAssoc(x: TurtleGraphics) {
      def <> (y: TurtleGraphics): TurtleGraphics = append(x,y)
    }
    implicit def tg2AppendAssoc(x: TurtleGraphics): AppendAssoc = new AppendAssoc(x)
  }


  object Testing extends TurtleDSL {
    // This implementation simply prints out each command as it is encountered.
    // It is NOT a good model to use for implementing the EDSL, but might be useful
    // for debugging.
    type TurtleGraphics = Unit

    // no-ops
    val empty = ()
    def append(tg1: TurtleGraphics, tg2: TurtleGraphics) = ()

    def penUp() = println("penUp()")
    def penDown() = println("penDown()")

    def forward(distance: Integer) = println("forward(" + distance + ")")

    def right(amount: Integer) = println("right(" + amount + ")")

    def setColor(col: Color) = println("setColor(" + col + ")")

    def setRandomColor(cols: List[Color]) = println("setRandomColor(" + cols + ")")

    def draw(tg: TurtleGraphics, width: Integer, height: Integer): GraphicsCanvas = {
      println("draw(_, " + width + ", " + height + ")")
      return new GraphicsCanvas(width,height)
    }
  }


   /****************
    *  Exercise 2  *
    ****************/

  object TurtleDSLImpl extends TurtleDSL {
    type TurtleGraphics = TurtleGraphicsAST

    // AST Definition
    abstract class TurtleGraphicsAST
    case class TGEmpty() extends TurtleGraphicsAST
    case class TGAppend(tg1: TurtleGraphics, tg2: TurtleGraphics) extends TurtleGraphicsAST
    case class TGPenUp() extends TurtleGraphicsAST
    case class TGPenDown() extends TurtleGraphicsAST
    case class TGForward(distance: Integer) extends TurtleGraphicsAST
    case class TGRight(amount: Integer) extends TurtleGraphicsAST
    case class TGSetColor(col: Color) extends TurtleGraphicsAST
    case class TGSetRandomColor(cols: List[Color]) extends TurtleGraphicsAST

    final case class TurtleGraphicsState(isPenUp: Boolean, currX: Integer, currY: Integer, angle: Integer )

    // Language Constructs 
    val empty: TurtleGraphics = TGEmpty()
    def append(tg1: TurtleGraphics, tg2: TurtleGraphics): TurtleGraphics = TGAppend(tg1,tg2)
    def penUp(): TurtleGraphics = TGPenUp()
    def penDown(): TurtleGraphics = TGPenDown()
    def forward(distance: Integer): TurtleGraphics = TGForward(distance)
    def right(amount: Integer): TurtleGraphics = TGRight(amount)
    def setColor(col: Color): TurtleGraphics = TGSetColor(col)
    def setRandomColor(cols: List[Color]): TurtleGraphics = TGSetRandomColor(cols)

    def draw(tg: TurtleGraphics, width: Integer, height: Integer): GraphicsCanvas = {
      
      val canvas = new GraphicsCanvas(width, height)

      // define initial state
      val initialState = TurtleGraphicsState(false, width/2, height/2, 0)
      canvas.setLineColor(Color.BLACK)

      // recursive function
      def drawInner(tg: TurtleGraphics, st: TurtleGraphicsState): TurtleGraphicsState = {
        (tg) match {
          case TGEmpty() => st
          case TGAppend(tg1, tg2) => {
            drawInner(tg2,drawInner(tg1,st))
          }
          case TGPenUp() => {
            val updatedState = TurtleGraphicsState(true, st.currX, st.currY, st.angle)
            return updatedState
          }
          case TGPenDown() => {
            val updatedState = TurtleGraphicsState(false, st.currX, st.currY, st.angle)
            return updatedState
          }
          case TGForward(distance)  => {
            val (x1, y1) = calculateNewCoords(st.currX, st.currY, st.angle, distance)
            if (!st.isPenUp) {
              canvas.drawLine(st.currX, st.currY, x1, y1)
            }
            val updatedState = TurtleGraphicsState(st.isPenUp, x1, y1, st.angle)
            return updatedState
          }
          case TGRight(amount) => {
            val newAngle = calculateAngleDiff(st.angle, amount)
            val updatedState = TurtleGraphicsState(st.isPenUp, st.currX, st. currY, newAngle)
            return updatedState
          }
          case TGSetColor(col) => {
            canvas.setLineColor(col)
            return st
          }
          case TGSetRandomColor(cols: List[Color]) => {
            val rand = new Random(System.currentTimeMillis())
            val num = rand.nextInt(cols.length)
            canvas.setLineColor(cols(num))
            return st
          }
          case _ => sys.error("end of draw drawInner")
        }
      }

      val finalState = drawInner(tg, initialState)
      canvas.drawTurtle(finalState.currX, finalState.currY, finalState.angle)
      
      return canvas
    }

  }

  ///////////////////////////////////////////////////////////////////////////
  // Test code - nothing to implement below this point but you may want to //
  // add more tests of your own.                                           //
  ///////////////////////////////////////////////////////////////////////////

  // change the comments to test TurtleDSLImpl instead
  //import Testing._
  import TurtleDSLImpl._

  def square() = {
    forward(100) <> right(90) <> setColor(Color.RED) <> forward(100)  <> right(90) <> forward(100) <> right(90) <> forward(100)
  }

  def squareRec() = {
    def squareRecInner(sides: Integer): TurtleGraphics = {
      if (sides <= 0) { empty } else
        { forward(100) <> right(90) <> squareRecInner(sides - 1) }
    }

    squareRecInner(4)
  }


  def drawSquares(numToDraw: Integer): TurtleGraphics = {
    if (numToDraw <= 0) {
      empty
    } else {
      // Firstly, offset our position and get back into the correct angle
      penUp() <> left(90) <> forward(80) <> left(90) <> forward(80) <> right(180) <> penDown() <>
      // Next, draw the square and recurse
      squareRec() <> drawSquares(numToDraw - 1)
    }
  }

  def circleyThing(): TurtleGraphics = {
    def circleyThingInner(n: Integer): TurtleGraphics = {
      if (n <= 0) { empty } else {
        forward(10) <> right(10) <> circleyThingInner(n-1)
      }
    }
    circleyThingInner(36)
  }


  def spiral(): TurtleGraphics = {
    def lineTurns(len: Integer): TurtleGraphics = {
      val colors = List(Color.RED, Color.GREEN, Color.BLUE, Color.PINK, Color.ORANGE, Color.YELLOW)
      if (len > 500) { empty } else {
        setRandomColor(colors) <> forward(len) <> right(60) <> lineTurns(len + 10)
      }
    }

    forward(15) <>
    lineTurns(20)
  }



  type Filename = String
  val toRun = List(
    (square(), "squareSimple.png"),
    (squareRec(), "squareRec.png"),
    (drawSquares(4), "fourSquares.png"),
    (circleyThing(), "circleyThing.png"),
    (spiral(), "spiral.png")
  )

  def drawFiles(xs: List[(TurtleGraphics, Filename)]) = {
    xs.foreach (p => {
        val (tg, fn) = p
        print("Processing " + fn + "...\n")
        saveToFile(tg, 2000, 2000, fn)
      })
  }

  def main(args: Array[String]): Unit = {
    drawFiles(toRun)
  }
}


// vim: set ts=2 sw=2 et sts=2:
