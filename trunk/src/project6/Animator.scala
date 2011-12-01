package project6

import java.awt.{Color, Graphics2D, Graphics}
import java.awt.geom.{Dimension2D, Point2D, Ellipse2D}
import java.awt.geom.Ellipse2D.Double
import java.awt.geom.Point2D.Double
import java.util.{ArrayList}
import javax.swing.{JSlider, JLabel, JPanel, JFrame}
import javax.swing.event.ChangeEvent

/**
 * Created by IntelliJ IDEA.
 * User: robert
 * Date: 11/29/11
 * Time: 12:58 PM
 * To change this template use File | Settings | File Templates.
 */

object Animator
{
  var speed : Int = 50
}

class Animator (title: String,components : List[AnimationEntity], customerList:ArrayList[Customer]) extends JFrame (title) with Runnable with javax.swing.event.ChangeListener{

  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  getContentPane.add(new DisplayPanel)
  setLocation(100, 100)
  setSize(800, 350)
  setVisible(true)
  setBackground(Color.black)

  val lineStatsY = 100
  val lineLabel = new JLabel("Line Statistics")
  val LQ_Line   = new JLabel()
  val LS_Line   = new JLabel()
  val L_Line    = new JLabel()
  val WQ_Line   = new JLabel()
  val WS_Line   = new JLabel()
  val W_Line    = new JLabel()

  val regStatsY = 180
  val regLabel  = new JLabel("Register Statistics")
  val LQ_Reg    = new JLabel()
  val LS_Reg    = new JLabel()
  val L_Reg     = new JLabel()
  val WQ_Reg    = new JLabel()
  val WS_Reg    = new JLabel()
  val W_Reg     = new JLabel()

  val openingTime = 60*8
  val time      = new JLabel()
  val simTime   = new JLabel()

  val displayer = new Thread(this)
  displayer.start

  val slider = new JSlider(javax.swing.SwingConstants.HORIZONTAL,1,100, 10)
  slider.addChangeListener(this)

  def stateChanged(e : ChangeEvent)
  {
    val source : JSlider = e.getSource.asInstanceOf[JSlider]
    if (!source.getValueIsAdjusting)
    {
        Animator.speed = 100/source.getValue.toInt
    }
  }

  class DisplayPanel extends JPanel
  {
    override def paintComponent( gr : Graphics )
    {
      super.paintComponent(gr)
      //Labels
      add(lineLabel)
      lineLabel.setLocation(0,lineStatsY)
      add(LQ_Line)
      LQ_Line.setText("  %2s = %10s".format("LQ",(Subway_Animated.λ * (Subway_Animated.waitTimesLine / Subway_Animated.servedCustomersLine))) )
      LQ_Line.setLocation(0,(lineStatsY+10).toInt)
      add(LS_Line)
      LS_Line.setText("  %2s = %10s".format("LS",Subway_Animated.λ * (Subway_Animated.serviceTimesLine /  Subway_Animated.servedCustomersLine)))
      LS_Line.setLocation(0,(lineStatsY+20).toInt)
      add(L_Line)
      L_Line.setText("    %2s = %10s".format("L",Subway_Animated.λ * (( Subway_Animated.waitTimesLine /  Subway_Animated.servedCustomersLine) + ( Subway_Animated.serviceTimesLine /  Subway_Animated.servedCustomersLine))))
      L_Line.setLocation(0,(lineStatsY+30).toInt)
      add(WQ_Line)
      WQ_Line.setText("%2s = %10s".format("WQ",( Subway_Animated.waitTimesLine /  Subway_Animated.servedCustomersLine)))
      WQ_Line.setLocation(0,(lineStatsY+40).toInt)
      add(WS_Line)
      WS_Line.setText("%2s = %10s".format("WS",( Subway_Animated.serviceTimesLine / Subway_Animated.servedCustomersLine)))
      WS_Line.setLocation(0,(lineStatsY+50).toInt)
      add(W_Line)
      W_Line.setText("  %2s = %10s".format("W",( Subway_Animated.waitTimesLine /  Subway_Animated.servedCustomersLine) + ( Subway_Animated.serviceTimesLine /  Subway_Animated.servedCustomersLine)))
      W_Line.setLocation(0,(lineStatsY+60).toInt)


      add(regLabel)
      regLabel.setLocation(0,regStatsY)
      add(LQ_Reg)
      LQ_Reg.setText("  %2s = %10s".format("LQ",(Subway_Animated.λ2 * (Subway_Animated.waitTimesReg / Subway_Animated.servedCustomersReg))) )
      LQ_Reg.setLocation(0,(regStatsY+10).toInt)
      add(LS_Reg)
      LS_Reg.setText("  %2s = %10s".format("LS",Subway_Animated.λ2 * (Subway_Animated.serviceTimesReg /  Subway_Animated.servedCustomersReg)))
      LS_Reg.setLocation(0,(regStatsY+20).toInt)
      add(L_Reg)
      L_Reg.setText("    %2s = %10s".format("L",Subway_Animated.λ2 * (( Subway_Animated.waitTimesReg /  Subway_Animated.servedCustomersReg) + ( Subway_Animated.serviceTimesReg /  Subway_Animated.servedCustomersReg))))
      L_Reg.setLocation(0,(regStatsY+30).toInt)
      add(WQ_Reg)
      WQ_Reg.setText("%2s = %10s".format("WQ",( Subway_Animated.waitTimesReg /  Subway_Animated.servedCustomersReg)))
      WQ_Reg.setLocation(0,(regStatsY+40).toInt)
      add(WS_Reg)
      WS_Reg.setText("%2s = %10s".format("WS",( Subway_Animated.serviceTimesReg / Subway_Animated.servedCustomersReg)))
      WS_Reg.setLocation(0,(regStatsY+50).toInt)
      add(W_Reg)
      W_Reg.setText("  %2s = %10s".format("W",( Subway_Animated.waitTimesReg /  Subway_Animated.servedCustomersReg) + ( Subway_Animated.serviceTimesReg /  Subway_Animated.servedCustomersReg)))
      W_Reg.setLocation(0,(regStatsY+60).toInt)

      add(time)
      val currentTimeInMin : Int = (Subway_Animated.getClock*60).toInt+openingTime
      time.setText("      t = %d:%s%d %s".format(if ((currentTimeInMin/60)%12 == 0) 12 else (currentTimeInMin/60)%12, if (currentTimeInMin%60 < 10) "0" else "" , currentTimeInMin%60, if ((currentTimeInMin/60)%24 < 12) "AM" else "PM" ))
      time.setLocation(0,0)
      add(simTime)
      simTime.setText("real = %s".format(Subway_Animated.getClock))
      simTime.setLocation(0,time.getAlignmentY.toInt+10)

      add(slider)
      slider.setLocation(150,0)

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