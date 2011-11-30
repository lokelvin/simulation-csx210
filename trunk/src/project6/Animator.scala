package project6

import javax.swing.{JPanel, JFrame}
import java.awt.{Color, Graphics2D, Graphics}
import java.awt.geom.{Dimension2D, Point2D, Ellipse2D}
import java.awt.geom.Ellipse2D.Double
import java.awt.geom.Point2D.Double
import java.util.ArrayList

/**
 * Created by IntelliJ IDEA.
 * User: robert
 * Date: 11/29/11
 * Time: 12:58 PM
 * To change this template use File | Settings | File Templates.
 */

class Animator (title: String,components : List[AnimationEntity], customerList:ArrayList[Customer]) extends JFrame (title) with Runnable{

  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  getContentPane.add(new DisplayPanel)
  setLocation(100, 100)
  setSize(800, 350)
  setVisible(true)
  setBackground(Color.black)
  val displayer = new Thread(this)
  displayer.start

  class DisplayPanel extends JPanel
  {
    override def paintComponent( gr : Graphics )
    {
      super.paintComponent(gr)
      implicit val gr2: Graphics2D = gr.asInstanceOf[Graphics2D]
      val iter = components.iterator
      while (iter.hasNext)
        iter.next().draw

      for (i <- 0 to customerList.size()-1)
      {
        try
          customerList.get(i).draw
        catch
        {
          case e: IndexOutOfBoundsException =>
        }
      }
    }
  }

  def run()
  {
       while(true)
       {
         repaint()
       }
  }
}

object DisplayPanel
{
  private var lineQueue : ArrayList[Customer] = new ArrayList[Customer]()
}

object AnimatorTest extends App{
  val source = new Source(5,30)
  val line1 = new Service(200,25,2)
  val line1Q = new WaitQueue(line1)
  val source2Line = new Path(source,line1Q)
  val register = new Service(400,35)
  val registerQ = new WaitQueue(register)
  val line2Register = new Path(line1,registerQ)

  var customerList = new ArrayList[Customer]()
  val animator = new Animator("Subway",List(source,line1,line1Q,source2Line,register,registerQ,line2Register),customerList)
  Thread.sleep(5000)
  val customer = new Customer(source)
  customerList.add(customer)
  customer.move(source2Line)

}