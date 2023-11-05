import cats._
import cats.implicits._
import cats.syntax._
import cats.instances.list._
import cats.instances.future._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AsyncTesting extends App {

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Int = hosts.getOrElse(hostname, 0)

  }

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    println(s"expected = $expected")
    println(s"actual = $actual")
    assert(actual == expected)
  }

  testTotalUptime()

}
