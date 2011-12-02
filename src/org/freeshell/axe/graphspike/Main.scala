package org.freeshell.axe.graphspike

import java.awt._
import javax.swing._
import org.freeshell.axe.graphspike._

/** Entry point to the application. */
object Main {

  def main(args: Array[String]): Unit = {
    val width = 640
    val height = 480

    val begin = -4.0d
    val end = 4.0d
    val step = 0.05d

    val paddingPercent = 2
    val connect = true
    val drawType = Cross(5)

    val model = new Model(
      for (i <- begin to (end, step)) yield (i, Math.sin(i)),
      (width, height), paddingPercent, connect, drawType)

    println(model)

    makeFrame(model, width, height).setVisible(true)
  }

  private def makeFrame(model: Model, width: Int, height: Int) = {
    val frame = new JFrame {
      setContentPane(new GraphPanel(model))
    }
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize(width, height);
    frame.setLocation(new Point(200, 300))
    frame
  }
}

sealed trait DrawType
case class Cross(w: Int = 3) extends DrawType
case class Circle(w: Int = 3) extends DrawType
case class Dot extends DrawType

/** The data model, just a sequence of number pairs. */
class Model(
  // The numbers to be plotted
  private[this] val numbers: Seq[(Double, Double)],
  // The display viewport width and height
  var viewport: (Int, Int),
  // Percent, how much of the viewport shall be used as padding around the graph
  var padPct: Int = 0,
  var connect: Boolean = false,
  var drawType: DrawType = Dot()) {

  // Some figures regarding our numeric sequence
  lazy val minx = numbers.reduceLeft((tup1, tup2) => if (tup1._1 < tup2._1) tup1 else tup2)._1
  lazy val maxx = numbers.reduceLeft((tup1, tup2) => if (tup1._1 > tup2._1) tup1 else tup2)._1
  lazy val spanx = Math.abs(maxx - minx)

  // FIXME axelk: extract '<' as further abstraction?
  lazy val miny = numbers.reduceLeft((tup1, tup2) => if (tup1._2 < tup2._2) tup1 else tup2)._2
  lazy val maxy = numbers.reduceLeft((tup1, tup2) => if (tup1._2 > tup2._2) tup1 else tup2)._2
  lazy val spany = Math.abs(maxy - miny)

  // Drawing size
  def width = Math.round(viewport._1 - (2 * (padPct * (viewport._1 / 100.0d)))).asInstanceOf[Int]
  def height = Math.round(viewport._2 - (2 * (padPct * (viewport._2 / 100.0d)))).asInstanceOf[Int]

  // Drawing origin is lower left, not upper left (as screen coordinates go)
  def zerox = Math.round(padPct * (viewport._1 / 100.0d)).asInstanceOf[Int]
  def zeroy = Math.round(viewport._2 - (padPct * (viewport._2 / 100.0d))).asInstanceOf[Int]

  // Used to convert between numeric and drawing numbers
  def coeffx = width / spanx
  def coeffy = height / spany

  lazy val zerobased = zerobase(numbers)

  def drawCoords = zerobased.map(translate)

  /** Recalculates the numbers to be zero-based instead of using negative numbers. */
  def zerobase(numbers: Seq[(Double, Double)]) = {
    numbers.map(tup => (Math.abs(minx - tup._1), Math.abs(miny - tup._2)))
  }

  /** Translates a numeric coordinate into the viewport coordinates. */
  def translate(tup: (Double, Double)): (Int, Int) = {
    (zerox + Math.round(tup._1 * coeffx).asInstanceOf[Int],
      zeroy - Math.round(tup._2 * coeffy).asInstanceOf[Int])
  }

  def draw(x: Int, y: Int, g: Graphics): Unit = drawType match {
    case Cross(w) => drawCross(x, y, w, g)
    case Circle(w) => drawArc(x, y, w, g)
    case _ => drawCross(x, y, 1, g)
  }

  def drawCross(x: Int, y: Int, w: Int, g: Graphics) = {
    g.drawLine(x - (w / 2), y, x + (w / 2), y)
    g.drawLine(x, y - (w / 2), x, y + (w / 2))
  }

  def drawArc(x: Int, y: Int, w: Int, g: Graphics) = {
    g.drawArc(x - (w / 2), y - (w / 2), w, w, 0, 360)
  }

  override def toString = {
    "Model[\nNumberOfElements=" + numbers.size +
      ", \nMinX=" + minx +
      ", \nMaxX=" + maxx +
      ", \nSpanX=" + spanx +
      ", \nMinY=" + miny +
      ", \nMaxY=" + maxy +
      ", \nSpanY=" + spany +
      ", \nAllElements=" + numbers +
      ", \nZeroX=" + zerox +
      ", \nZeroY=" + zeroy +
      ", \nWidth=" + width +
      ", \nHeight=" + height +
      ", \nCoeffX=" + coeffx +
      ", \nCoeffY=" + coeffy +
      ", \nDrawCoords=" + drawCoords +
      "]"
  }
}

/** The panel for drawing the graph. */
class GraphPanel(private[this] val model: Model) extends JPanel {

  override def paint(g: Graphics) = {
    g.setColor(Color.RED)
    model.viewport = (getWidth(), getHeight())
    model.drawCoords.foreach(tup => model.draw(tup._1, tup._2, g));
    if (model.connect) {
      model.drawCoords.reduceLeft(
        (tup1, tup2) => {
          g.drawLine(tup1._1, tup1._2, tup2._1, tup2._2)
          tup2
        })
    }
  }

}

