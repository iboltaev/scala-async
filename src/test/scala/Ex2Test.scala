import scala.language.implicitConversions

import org.scalatest._

import com.twitter.util._
import com.twitter.conversions.time._

import scalaz._
import syntax.monad._

import Ex2._

class Ex2Spec extends FlatSpec with Matchers {
  it should "work with 'point', 'flatMap' and 'map'" in {
    implicit val m = new FStateMonad[Int]
    val a = for {
      v <- m.point(10)
      v2 = v + 1
    } yield(v2)

    a(0)() should be ((11, 0))
  }

  it should "work with 'gets' and 'puts'" in {
    implicit val m = new FStateMonad[Int]
    val a = for {
      _ <- m.whileM_(gets[Int] map (_ < 10), for {
        i <- gets[Int]
        _ <- puts(i + 1)
      } yield(()))
      v1 <- gets[Int]
    } yield (v1)

    a(0)() should be ((10, 10))
  }

  it should "work with 'whileM_'" in {
    implicit val m = new FStateMonad[Int]
    
    val a = for {
      _ <- m.whileM_(conds(_ < 10), mods(_ + 1))
      v1 <- gets[Int]
    } yield (v1)

    a(0)() should be ((10, 10))
  }

  it should "work with 'forM_'" in {
    implicit val m = new FStateMonad[Int]
    
    val a = for {
      _ <- m.forM_(_ < 10, _ + 1) {
        m.point("AAAAAA")
      }
      v1 <- gets[Int]
    } yield (v1)

    a(0)() should be ((10, 10))
  }

}
