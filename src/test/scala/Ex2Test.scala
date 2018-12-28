import Ex2._
import org.scalatest._

import scalaz.syntax.monad._

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.language.implicitConversions

class Ex2Spec extends FlatSpec with Matchers {
  it should "work with 'point', 'flatMap' and 'map'" in {
    implicit val m = new FStateMonad[Int]
    val a = for {
      v <- m.point(10)
      v2 = v + 1
    } yield(v2)

    Await.result(a(0), Duration.Inf) should be ((11, 0))
  }

  it should "work with 'gets' and 'puts'" in {
    implicit val m = new FStateMonad[Int]

    val a = for {
      _ <- m.whileM_ (_ < 10) { for {
          i <- gets[Int]
          _ <- puts(i + 1)
        } yield {}
      }
      v1 <- gets[Int]
    } yield v1

    Await.result(a(0), Duration.Inf) should be ((10, 10))
  }

  it should "work with 'whileM_'" in {
    implicit val m = new FStateMonad[Int]
    
    val a = for {
      _ <- m.whileM_(conds(_ < 10), mods(_ + 1))
      v1 <- gets[Int]
    } yield v1

    Await.result(a(0), Duration.Inf) should be ((10, 10))
  }

  it should "work with 'forM_'" in {
    implicit val m = new FStateMonad[Int]
    
    val a = for {
      _ <- m.forM_(_ < 10, _ + 1) {
        m.point("AAAAAA")
      }
      v1 <- gets[Int]
    } yield v1

    Await.result(a(0), Duration.Inf) should be ((10, 10))
  }

}
