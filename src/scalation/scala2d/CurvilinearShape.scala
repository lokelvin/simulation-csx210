
/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller
 * @version 1.0
 * @date    Sat Jan  9 14:06:47 EST 2010
 * @see     LICENSE (MIT style license file).
 */

package scalation.scala2d

import scalation.scala2d.Shapes.Shape
import scalation.util.Error

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * The CurvilinearShape trait provides a general type for line and curves.
 * It is analogous to RectangularShape.
 */
trait CurvilinearShape extends Shape with Error
{
    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the x-coordinate of the center of a line/curve.  This method must
     * be implemented by all classes mixing in this trait.
     */
    def getCenterX (): Double

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the y-coordinate of the center of a line/curve.  This method must
     * be implemented by all classes mixing in this trait.
     */
    def getCenterY (): Double

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set (or reset) the location for a straight line.  This method must
     * be implemented by all classes mixing in this trait.
     * @param p1  the starting point
     * @param p2  the ending point
     */
    def setLine (p1: R2, p2: R2)

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set (or reset) the location for a line/curve using the bend parameter
     * to compute the control point.  This method must be implemented by all
     * classes mixing in this trait.
     * @param _p1   the starting point
     * @param _p2   the ending point
     * @param bend  the bend or curvature (1. => line-length)
     */
    def setLine (p1: R2, pc: R2, bend: Double)

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set (or reset) the location for a line/curve using an explicitly
     * given control point.  This is an optional method.
     * @param _p1  the starting point
     * @param _pc  the control point
     * @param _p2  the ending point
     */
    def setLine (p1: R2, pc: R2, p2: R2)
    {
        flaw ("setFrame (p1, pc, p2)", "this method is not overridden by mixin class")
    } // setFrame

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Return the next point on the CurvilinearShape (one step beyond current point)
     * and adjust from top-left to center coordinates for the object traversing
     * the curve based on its width and height.  Return null if past end point.
     * This is an optional method.
     * @param width   the width of object traversing the curve
     * @param height  the height of object traversing the curve
     */
    def next (width: Double, height: Double): R2 =
    {
        flaw ("next (width, height)", "this method is not overridden by mixin class")
        null
    } // next

} // CurvilinearShape

