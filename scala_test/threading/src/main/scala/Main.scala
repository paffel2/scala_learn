import java.lang.Thread
import java.util.concurrent.atomic._
import javax.annotation.processing.Messager

object Main extends App {
  val thread = new Thread(() => println("thread running"))

  thread.start()
}

object severalTreahds extends App {

  /*case class CustomThread() extends Thread {
    override def run() = println("custom thread running...")
  }

  val thread = CustomThread()

  thread.start()
  thread.join()

  println("custom thread ends work")*/

  def execute(f: => Unit): Thread = {
    val thread1 = new Thread { override def run() = f }
    thread1.start()
    thread1
  }

  val someThread = execute {
    println("someThread running...")
    Thread.sleep(1000)
    println("someThread running...")
    Thread.sleep(1000)
  }

  someThread.join()
  println("someThread has completed its execution")

}

object registration extends App {

  var idCount = 0

  def getNewId(): Int = this.synchronized {
    val userId = idCount + 1
    idCount = userId
    userId
  }

  def registerUsers(userCnt: Int, threadName: String): Unit = {

    val userIds = for (_ <- 0 to userCnt) yield getNewId()
    println(s"Users [ $userIds ] has been registerd by $threadName")
  }

  val thread1 = new Thread(() => registerUsers(5, "thread-1"))
  val thread2 = new Thread(() => registerUsers(5, "thread-2"))

  thread1.start()
  thread2.start()

  thread1.join()
  thread2.join()

  println(idCount)

}

object test1 extends App {

  var greeting = ""

  val greetingThread = new Thread(() => {
    Thread.sleep(1000)
    greeting = "hello"
  })

  greeting = "hi"

  greetingThread.start()
  Thread.sleep(1001)

  println(greeting)

}

object Bank extends App {

  class Account(val id: Int, val name: String, var balance: Int) {
    override def toString: String = s"На счету [ $name ] имеется [ $balance ]"
  }

  def send(fromAcc: Account, toAcc: Account, amount: Int) = {
    def approveSending() = {
      fromAcc.balance -= amount
      toAcc.balance += amount

      println(s"$fromAcc | $toAcc")
    }

    if (fromAcc.id < toAcc.id) fromAcc.synchronized {
      toAcc.synchronized { approveSending() }
    }
    else
      toAcc.synchronized {
        fromAcc.synchronized { approveSending() }
      }
  }

  val alice = new Account(1, "Alice", 100)
  val bob = new Account(2, "Bob", 200)

  val t1 = new Thread { for (_ <- 0 to 5) send(alice, bob, 10) }
  val t2 = new Thread { for (_ <- 0 to 5) send(bob, alice, 20) }

  t1.join()
  t2.join()

}

object atomicExample extends App {

  import java.util.concurrent.atomic._

  val id = new AtomicLong(0L)
  def getId(): Long = id.incrementAndGet()

  def registerUsers(userCnt: Int, threadName: String): Unit = {

    val userIds = for (_ <- 0 to userCnt) yield getId()
    println(s"Users [ $userIds ] has been registerd by $threadName")
  }

  val thread1 = new Thread(() => registerUsers(5, "thread-1"))
  val thread2 = new Thread(() => registerUsers(5, "thread-2"))

  thread1.start()
  thread2.start()
}

object prac1 extends App {

  // исправьте код
  def runThreads(threadNum: Int, i: Int = 1): Thread = new Thread(() => {
    if (i < threadNum) {
      val thread = runThreads(threadNum, i + 1)
      print(s"thread_$i ")
      println(Thread.currentThread().getName()) // получаем имя потока
      thread.start()
      thread.join()
    }
    print(s"thread_$i ")
  })

  runThreads(3, 0).start()
}

object prodCons extends App {
  class Container {
    private var num: Int = 0

    def isEmpty: Boolean = num == 0

    // производитель
    def set(newVal: Int) = num = newVal

    // потребитель
    def get = {
      val result = num // получили значение num
      num = 0 // вернули начальное значение
      result
    }

  }

  def execute(): Unit = {
    val container = new Container

    val consumer = new Thread(() => {
      println("[потребитель] ожидает значения...")

      container.synchronized {
        container.wait() // ждет сигнал от notify
      }

      println("[потребитель] значение получено:" + container.get)
    })

    val producer = new Thread(() => {
      println("[производитель] идут вычисления...")
      Thread.sleep(100)
      val value = 123

      container.synchronized {
        container.set(value)
        println("[производитель] значение готово:" + value)
        container.notify()
      }
    })

    consumer.start()
    producer.start()
  }

  execute()

}

object prac2 extends App {

  class BankAccount(var amount: Int) {
    override def toString: String = s"На счету $amount"
  }

  def payWithCard(account: BankAccount, price: Int, details: String) =
    account.synchronized {

      account.amount = account.amount - price

      println(details)
      println(s"Остаток на счете ${account.amount}")
    }

  for (_ <- 1 to 50) {
    val account = new BankAccount(100)

    val thread1 = new Thread(() => payWithCard(account, 20, "футболка"))
    val thread2 = new Thread(() => payWithCard(account, 50, "ботинки"))

    thread1.start()
    // thread1.join()
    thread2.start()
    // thread2.join()

    Thread.sleep(10)

    if (account.amount != 30) println(s"Остаток на счете: ${account.amount}")
  }
}

object prac5 extends App {
  import java.util.concurrent.atomic._
  // def call[A](a: => A): A = a

  def execute[A, B](a: A, b: B): (A, B, A) = {
    def call[A](a: => A): A = a
    var aVal: A = null.asInstanceOf[A]
    var aVal2: A = null.asInstanceOf[A]
    var bVal: B = null.asInstanceOf[B]
    val f = (new Thread(() => { aVal = call(a) }))
    f.start()
    val s = (new Thread(() => { bVal = call(b) }))
    s.start()
    val t = (new Thread(() => { aVal2 = call(a) }))
    t.start()

    f.join()
    s.join()
    t.join()

    (aVal, bVal, aVal2)

  }

  println(execute(1, "SomeString"))
}

object messages extends App {

  class Message[T] {
    def read(): Unit = this.synchronized {
      new Thread(() => {
        println("No message found")

        this.synchronized {
          this.wait() // ждет сигнал от notify
        }

        println(123)
      }).start()

    }
    def get = vault.head

    def create(m: T): Unit = this.synchronized {
      val c = new Thread(() => {
        this.synchronized {
          vault = List(m)
          println("Message created")
          this.notifyAll()
        }
      })
      c.start()
      c.join()
    }
    var vault: List[T] = List()

  }

  var a = new Message[Int]
  a.read()
  a.create(123)
  val t = Seq("a", "").filter(s => s.nonEmpty)
}
