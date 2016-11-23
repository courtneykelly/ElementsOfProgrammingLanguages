package Assignment3.GraphicsCanvas
import java.awt.Color
import java.awt.image._
import java.io.File
import java.io.IOException
import javax.imageio.ImageIO
import java.awt.geom._
import java.awt.BasicStroke

trait GraphicsCanvasTrait {
  // Draws a line starting at co-ordinates (x0, y0), finishing at (x1, y1).
  def drawLine(x0: Integer, y0: Integer, x1: Integer, y1: Integer): Unit
  // Sets the colour of future lines to be drawn
  def setLineColor(col: Color): Unit
  // Draws a turtle at the location of (x, y), at angle `angle`
  def drawTurtle(x: Integer, y: Integer, angle: Integer): Unit
  // Saves the current image as an image to filename.
  def saveToFile(filename: String): Unit
}

class GraphicsCanvas(val width: Integer, val height: Integer) extends GraphicsCanvasTrait {
  val TURTLE_FILE = "turtle.png"

  // Firstly, set up the canvas
  val bufferedImg = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
  val g2d = bufferedImg.createGraphics()
  // Default colour is black on white
  g2d.setColor(Color.WHITE);
  g2d.fillRect(0, 0, width, height);
  g2d.setColor(Color.BLACK)

  // Method implementations
  def drawLine(x0: Integer, y0: Integer, x1: Integer, y1: Integer): Unit = {
    g2d.drawLine(x0, y0, x1, y1)
  }

  def setLineColor(col: Color): Unit = {
    g2d.setColor(col)
  }

  // Algorithm adapted from http://stackoverflow.com/questions/4787066/how-to-resize-and-rotate-an-image
  // (yes, TAs use StackOverflow too...)
  def rotateImage(image: BufferedImage, angle: Double): BufferedImage = {
      val angleSin = Math.abs(math.sin(angle))
      val angleCos = Math.abs(math.cos(angle))
      val w = image.getWidth()
      val h = image.getHeight()
      val newWidth = Math.floor(w * angleCos + h * angleSin).toInt
      val newHeight = Math.floor(h * angleCos + w * angleSin).toInt
      val resultImg = new BufferedImage(newWidth, newHeight, BufferedImage.TYPE_INT_ARGB)
      val g2d = resultImg.createGraphics()
      g2d.translate((newWidth - w)/2, (newHeight - h)/2)
      g2d.rotate(angle, w/2, h/2)
      g2d.drawRenderedImage(image, null)
      g2d.dispose()
      resultImg
  }

  def drawTurtle(x: Integer, y: Integer, angle: Integer): Unit = {
    try {
      val turtleImg = ImageIO.read(new File(TURTLE_FILE))
      // Scale and rotate image
      val w = turtleImg.getWidth()
      val h = turtleImg.getHeight()
      val scaleFactor = 0.08
      val newW = w * scaleFactor
      val newH = h * scaleFactor
      val scaleImg = new BufferedImage(newW.toInt, newH.toInt, BufferedImage.TYPE_INT_ARGB);
      // Scale:
      val at = AffineTransform.getScaleInstance(scaleFactor, scaleFactor)
      val scaleGraphics = scaleImg.createGraphics()
      scaleGraphics.drawRenderedImage(turtleImg, at)
      // Rotate:
      val angleRads = math.toRadians(angle.toDouble)
      val rotateImg = rotateImage(scaleImg, angleRads)
      // Finally, translate to the correct position and draw to the canvas
      val absoluteX = x - (rotateImg.getWidth() / 2)
      val absoluteY = y - (rotateImg.getHeight() / 2)
      val translateTransform = new AffineTransform()
      translateTransform.translate(absoluteX, absoluteY)
      g2d.drawRenderedImage(rotateImg, translateTransform)
    } catch {
      case (ioe: IOException) => print("Exception occurred while reading turtle image: %s".format(ioe.toString))
    }
  }

  def saveToFile(filename: String) {
    try {
      val outputfile = new File(filename)
      ImageIO.write(bufferedImg, "png", outputfile)
      ()
    } catch {
      case (ioe: IOException) => print("Exception occurred while saving image: %s".format(ioe.toString))
    }
  }

}

// vim: set ts=2 sw=2 et sts=2:
