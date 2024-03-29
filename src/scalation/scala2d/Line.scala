
/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller
 * @version 1.0
 * @date    Sun Jan 10 16:17:54 EST 2010
 * @see     LICENSE (MIT style license file).
 */

package scalation.scala2d

import swing.{MainFrame, Panel}

import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.{Dimension, Graphics2D}
import scalation.util.Error

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * The Line class uses Java's Line2D class to create a line.
 * @param p1  the starting point for the line
 * @param p2  the ending point for the line
 */
case class Line (var p1:  R2 = R2 (0., 0.),
                 var p2:  R2 = R2 (0., 0.))
     extends java.awt.geom.Line2D.Double (p1, p2) with CurvilinearShape
{
    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the x-coordinate of the center of the line.
     */
    def getCenterX (): Double =
    {
        (p1.x + p2.x) / 2.
    } // getCenterX

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the y-coordinate of the center of the line.
     */
    def getCenterY (): Double =
    {
        (p1.y + p2.y) / 2.
    } // getCenterY

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set (or reset) the frame/location for the Line as a line.
     * @param _p1  the starting point
     * @param _p2  the ending point
     */
    def setLine (_p1: R2, _p2: R2)
    {
        p1 = _p1; p2 = _p2
        super.setLine (p1.x, p1.y, p2.x, p2.y)
    } // setLine

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set (or reset) the frame/location for the Line as a line. The bend
     * parameter is ignored for this class, since arrows are straight.
     * @param _p1   the starting point
     * @param _p2   the ending point
     * @param bend  the bend or curvature (0. => straight line)
     */
    def setLine (_p1: R2, _p2: R2, bend: Double)
    {
        p1 = _p1; p2 = _p2
        setLine (p1, p2)
    } // setLine
    
} // Line class

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This object is used to test the Line class.
 */
object LineTest extends App
{
    private val line1 = new Line (R2 (200, 200), R2 (300, 200))
    private val line2 = new Line (R2 (200, 200), R2 (300, 300))
    private val line3 = new Line (R2 (200, 200), R2 (200, 300))
    private val line4 = new Line (R2 (200, 200), R2 (100, 300))
    private val line5 = new Line (R2 (200, 200), R2 (100, 200))
    private val line6 = new Line (R2 (200, 200), R2 (100, 100))
    private val line7 = new Line (R2 (200, 200), R2 (200, 100))
    private val line8 = new Line (R2 (200, 200), R2 (300, 100))

    private val canvas = new Panel
    {
        background    = white
        preferredSize = new Dimension (600, 600)

        override def paintComponent (g2d: Graphics2D)
        {
            super.paintComponent (g2d)
            g2d.setPaint (red)
            g2d.draw (line1)
            g2d.setPaint (orange)
            g2d.draw (line2)
            g2d.setPaint (yellow)
            g2d.draw (line3)
            g2d.setPaint (yellowgreen)
            g2d.draw (line4)
            g2d.setPaint (green)
            g2d.draw (line5)
            g2d.setPaint (cyan)
            g2d.draw (line6)
            g2d.setPaint (blue)
            g2d.draw (line7)
            g2d.setPaint (violet)
            g2d.draw (line8)
        } // paintComponent

     } // canvas Panel

    private def top = new MainFrame
    {
        title    = "LineTest"
        contents = canvas
        visible  = true
    } // top MainFrame

    println ("Run LineTest")
    top

} // LineTest object

