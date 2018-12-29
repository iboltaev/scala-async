import java.util.{Timer, TimerTask}

import org.scalatest._

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.language.implicitConversions

class CFutureTest extends FlatSpec with Matchers {
  val timer = new Timer

  def schedule[A](delay: Int)(f: => A): Future[A] = {
    val p = Promise[A]
    timer.schedule(new TimerTask {
      override def run(): Unit = {
        p.success(f)
      }
    }, delay)
    p.future
  }

  def timed(i: Int): Future[Int] = {
    schedule(2000) {
      println(s"works: $i")
      i
    }
  }

  def timedCf(i: Int): CFuture[Int] =
    CFuture.fromFuture(timed(i), () => println(s"cancelled $i"))

  def stream = {
    implicit val ctx = CFuture.defaultCtx

    def impl(cf: CFuture[Int]): CFuture[Int] = for {
      i <- cf
      next <- impl(timedCf(i + 1))
    } yield next

    impl(timedCf(0))
  }

  def flatMapped(start: Int): CFuture[Int] = {
    implicit val ctx = CFuture.defaultCtx

    for {
      i1 <- timedCf(start)
      i2 <- timedCf(start + 1)
    } yield i2
  }

  it should "work" in {
    val f = stream

    val f2 = schedule(7000) {
      f.cancel()
    }

    Await.ready(f2, Duration.Inf)
    Await.ready(f.f, Duration.Inf)
  }

  // NOT WORKING!!!
  it should "work 2" in {
    implicit val ctx = CFuture.defaultCtx

    val f = for {
      i1 <- flatMapped(0)
      i2 <- flatMapped(2)
    } yield i2

    val f2 = schedule(7000) {
      f.cancel()
    }

    Await.ready(f2, Duration.Inf)
    Await.ready(f.cf.f, Duration.Inf)
  }
}
