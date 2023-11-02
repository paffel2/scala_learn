import scala.io.StdIn

object Main extends App {
  class Button(val label: String = "this") {
    def click() = s"button -$label- has been clicked"

  }

  class RoundedButton(label: String) extends Button(label) {
    override def click(): String = s"rounded button -$label- has been clicked"
  }

  abstract class Event {
    def trigger(eventName: String): Unit
  }

  class Listener(val eventName: String, var event: Event) {
    def register(evt: Event) { event = evt }
    def sendNotification() { event.trigger(eventName) }
  }

  val notification: Listener = new Listener(
    "mousedown",
    new Event {
      def trigger(eventName: String): Unit = println(
        s"trigger $eventName event"
      )
    }
  )

  // notification.sendNotification
}

object test extends App {

  trait Event {
    def id: String
    def evType: String
    def operation: String
  }

  case class UserEvent(id: String, evType: String, operation: String)
      extends Event
  case class SystemEvent(id: String, evType: String, operation: String)
      extends Event

  trait Registrable[A <: Event] {
    def update(event: A, status: String): A
  }

  object EventOps {
    val userEvent = new Registrable[UserEvent] {
      def update(event: UserEvent, status: String): UserEvent =
        event.copy(operation = status)
    }

    val systemEvent = new Registrable[SystemEvent] {
      def update(event: SystemEvent, status: String): SystemEvent =
        event.copy(operation = status)
    }
  }

  class EventOps[A <: Event](event: A)(ops: Registrable[A]) {
    def update(status: String) = ops.update(event, status)
  }

  val user1Event = UserEvent("1", "user_event", "account_create")
  val user1Ops = new EventOps(user1Event)(EventOps.userEvent)
  val user1EventUpd = user1Ops.update("permission_add")

  println(s"User1 | Operation: ${user1EventUpd.operation}")
}

object test3 extends App {

  case class Course(title: String, instructor: String)

  object Course {
    def apply(instructor: String): Course = Course("AdvancedScala", instructor)
  }

  val scalaCourse = Course("Scala", "Bob")

  val course = scalaCourse.copy()

  val course1 = new Course("Scala", "Bob")

  val course2 = Course("Alice")

  val course3 = scalaCourse.copy("AdvancedScala")

  val course4 = Course("Scala", "Bob")

  // val course5 = new Course("Scala")
}

object oopExer1 extends App {

  class Logger(val msgNum: Int = 0) {
    def info: Logger = {
      println("INFO New Message")
      new Logger(msgNum + 1)
    }

    def print = println(msgNum)

    def info(n: Int): Logger = {
      if (n <= 0) this
      else this.info.info(n - 1)
    }

  }
}
