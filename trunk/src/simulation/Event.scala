package simulation

abstract class Event (entity: Entity) (implicit scheduler: Scheduler) extends Ordered[Event] {

  var time: Int = 0
  
  def compare (that: Event) = (that.time - this.time)
  
  def schedule (event: Event, delay: Int) = scheduler.schedule(event, delay)
  
  def occur: Unit
  
}