package simulation
package process

case class Message (msg: String)

case class Kill () extends Message ("Kill") 