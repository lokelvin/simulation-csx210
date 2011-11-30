package project6

import java.awt.{Color, Graphics2D, Shape}
import util.Random
import java.awt.geom._
import java.awt.geom.Line2D.Double
import java.awt.geom.Point2D.Double
import collection.mutable.Queue
import project6.Subway.SimCustomer
import java.awt.geom.Ellipse2D.Double
import java.awt.geom.Rectangle2D.Double
import java.util.ArrayList

/**
 * Created by IntelliJ IDEA.
 * User: robert
 * Date: 11/29/11
 * Time: 2:23 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class AnimationEntity(x: Int, y: Int) {
      var entity : Shape
      val size : Int
      val color : Color
      var position : Point2D.Double = new Point2D.Double(x,y)
      val center : Point2D.Double  = new Point2D.Double()

  def getCenter(otherSize : Int) : Point2D =
  {
    center.setLocation(((position.getX)+size/2)-otherSize/2,((position.getY)+size/2)-otherSize/2)
    center
  }

  def setPosition(x : Int, y : Int)
  {
    position.setLocation(x,y)
  }

  def setPosition(pos :Point2D)
  {
    position.setLocation(pos)
  }

  def setFrame()

  def draw(implicit gr2:Graphics2D)
  {
    setFrame()
    gr2.setColor(color)
    gr2.fill(entity)
  }
}

class Source(x:Int = 30, y:Int = 30, val size:Int = 30, val color: Color = new Color(205,133,63)) extends AnimationEntity(x,y) {
  var entity : Shape = new RoundRectangle2D.Double()
  def setFrame()
  {
    entity.asInstanceOf[RoundRectangle2D].setFrame(position.x,position.y,size,size)
  }
}

class Customer(source : Source, val size :Int = 10,val color : Color = Color.getHSBColor( Random.nextFloat(), 1.0F, 1.0F )) extends AnimationEntity(source.getCenter(size).getX.toInt,source.getCenter(size).getY.toInt) {
  var entity : Shape = new Ellipse2D.Double()
  val stepSize = 20

  def setFrame()
  {
    entity.asInstanceOf[Ellipse2D.Double].setFrame(position.getX,position.getY,size,size)
  }

  def move(along : Path)
  {
    val center1 : Point2D = along.entity1.getCenter(size)
    val center2 : Point2D = along.entity2.getCenter(size)
    val deltaX  = ((center2.getX-center1.getX)/stepSize)
    val deltaY  = (center2.getY-center1.getY)/stepSize
    for (i <- 0 to stepSize)
    {
        position.setLocation(position.getX+deltaX,position.getY+deltaY)
      Thread.sleep(10)
    }
  }
}

class Service(x:Int, y:Int, val servers : Int = 1,val size:Int = 20, val color: Color = new Color(143,188,143)) extends AnimationEntity(x,y)
{
  var entity : Shape = new Rectangle2D.Double()

  def getCenterOfServer(otherSize: Int, serverNo: Int): Point2D =
  {
    center.setLocation(((position.getX)+size/2)-otherSize/2,((position.getY)+size*serverNo-size/2)-otherSize/2)
    center
  }

  override def getCenter(otherSize: Int) : Point2D =
  {
    center.setLocation(((position.getX)+size/2)-otherSize/2,((position.getY)+size*servers/2)-otherSize/2)
    center
  }

  def setFrame() = entity.asInstanceOf[Rectangle2D].setFrame(position.x,position.y,size,size*servers)
}

class Path(val entity1: AnimationEntity, val entity2: AnimationEntity, val size:Int = 2, val color:Color = Color.black) extends  AnimationEntity(0,0)
{
  var entity : Shape = new Line2D.Double()
  def setFrame() = entity.asInstanceOf[Line2D].setLine( entity1.getCenter(0),entity2.getCenter(0))

  override def draw(implicit gr2: Graphics2D)
  {
    setFrame()
    gr2.setColor(color)
    gr2.draw(entity)
  }
}

class WaitQueue(val forWho: AnimationEntity,val size:Int = 10, val color:Color = Color.yellow) extends AnimationEntity((forWho.getCenter(size*10).getX-forWho.size/2).toInt,(forWho.getCenter(size).getY).toInt)
{
  var entity : Shape = new Rectangle2D.Double()
  val queueMax = 5
  var customersInQueue = 0

  def enterQueue()
  {
    customersInQueue += 1
  }

  def leaveQueue(q : ArrayList[SimCustomer])
  {
     customersInQueue -= 1
    var max = 4
    if (q.size()-1< max)
      max = q.size()-1
    for (i <- 0 to max)
    {
      val customer = q.get(i).customer
      customer.setPosition(customer.position.getX.toInt+customer.size,customer.position.getY.toInt)
    }
  }

  override def getCenter(otherSize : Int) : Point2D =
  {
    var temp : Int = 0
    if (customersInQueue > queueMax)
      temp = 5
    else
      temp = customersInQueue
    center.setLocation((position.getX+entity.getBounds.getWidth-(otherSize*(temp+1))),((position.getY)+size/2)-otherSize/2)
    center
  }

  def setFrame() = entity.asInstanceOf[Rectangle2D].setFrame(position.x,position.y,size*queueMax,size)
}

class Split(x:Int, y:Int,val size: Int = 5, val color : Color = Color.black) extends AnimationEntity(x,y)
{
   var entity : Shape = new Ellipse2D.Double()

  def setFrame()
  {
    entity.asInstanceOf[Ellipse2D.Double].setFrame(position.getX,position.getY,size,size)
  }
}

class Lobby (x: Int, y:Int, val height : Int, val width : Int, val color : Color = new Color(161,122,230))  extends AnimationEntity(x,y)
{
  val size = height * width
  var entity : Shape = new Rectangle2D.Double()

  val seats : Array[Customer] = new Array[Customer]((width/10)*(height/10))

  def enterLobby(customer : Customer) : Boolean =
  {
    for (i <- 0 to ((width/10)*(height/10))-1)
    {
        if (seats(i)==null){
             seats(i) = customer
             customer.setPosition(i%(width/10)*10+position.getX.toInt,i/(width/10)*10+position.getY.toInt)
             return true
        }
    }
    return false
  }

  def leaveLobby(customer : Customer)
  {
    customer.setPosition(getCenter(customer.size))
    for (i <- 0 to ((width/10)*(height/10))-1)
    {
        if (seats(i)==customer){
             seats(i) = null
             return
        }

    }
  }

  override def getCenter(otherSize : Int) : Point2D =
  {
    center.setLocation((position.getX+width/2)-otherSize/2,((position.getY)+height/2)-otherSize/2)
    center
  }

  def setFrame()
  {
    entity.asInstanceOf[Rectangle2D].setFrame(position.getX,position.getY,width,height)
  }
}