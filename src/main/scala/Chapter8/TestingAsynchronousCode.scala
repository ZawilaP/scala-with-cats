package Chapter8
import cats.implicits.toFunctorOps
import cats.instances.list._
import cats.syntax.traverse._
import cats.{Applicative, Id}
import scala.concurrent.Future

import scala.language.higherKinds
object TestingAsynchronousCode extends App{

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }
  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }
  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Int =
      hosts.getOrElse(hostname, 0)
  }
  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  def testTotalUptime(): Unit = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    println(actual)
    println(expected)
    assert(actual == expected)
  }

  testTotalUptime()
}
