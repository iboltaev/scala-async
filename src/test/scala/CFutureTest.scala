import java.util.concurrent.atomic.AtomicReference
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

  def timedCf(i: Int)(implicit ctx: AtomicReference[Option[() => Unit]]): CFuture[Int] =
    CFuture.fromFuture(timed(i), () => println(s"cancelled $i"))

  def stream(implicit ctx: AtomicReference[Option[() => Unit]]) = {
    /*def impl(cf: CFuture[Int]): CFuture[Int] = cf.flatMap { i =>
      impl(timedCf(i + 1))
    }*/
    def impl(cf: CFuture[Int]): CFuture[Int] = for {
      i <- cf
      next <- impl(timedCf(i + 1))
    } yield next

    impl(timedCf(0))
  }

  it should "work" in {
    implicit val ctx = new AtomicReference[Option[() => Unit]](Some(() => {}))
    val f = stream
    val f2 = schedule(7000) {
      f.cancel()
    }

    Await.ready(f2, Duration.Inf)
    Await.ready(f.f, Duration.Inf)
  }
}
