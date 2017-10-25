import scala.language.implicitConversions

import org.scalatest._

import com.twitter.util._
import com.twitter.conversions.time._

import scalaz._
import syntax.monadPlus._

import Ex4._

class Ex4Spec extends FlatSpec with Matchers {
  import Ex3._
  import Ex2._

  def makeStream(l: List[Int]) = genAsyncStream(l)(
    l => if (l.isEmpty) new NoFuture else Future((l.head, l.tail)))

  it should "work with 'foreach'" in {
    implicit val m = new FStateMonad[Int]
    import m._

    val fstate = for {
      _ <- foreach(makeStream(0 :: 1 :: 2 :: Nil)) { v =>
        mods(_ + 1)
      }
      v2 <- gets[Int]
    } yield (v2)

    fstate(0)() should be ((3, 3))
  }

  it should "work with 'foreach' 2" in {
    implicit val m = new FStateMonad[Int]
    import m._

    val fstate = for {
      _ <- foreach(makeStream(0 :: 1 :: 2 :: 3 :: Nil)) { v =>
        for {
          s <- gets[Int]
          _ <- puts(s + v)
        } yield () 
      }
      v2 <- gets[Int]
    } yield v2

    fstate(0)() should be ((6, 6))
  }

  it should "work with 'isEmpty' and 'get'" in {
    case class S (counter: Int, stream: AsyncStream[Int]) // State
    implicit val m2 = new FStateMonad[S]

    val stream = makeStream(0 :: 1 :: 2 :: 3 :: Nil)

    val fstate = for {
      _ <- m2.whileM_(notEmpty(_.stream), for {
        s <- gets[S]
        (el, newStream) <- get[Int, S](s.stream)
        _ <- puts(S(s.counter + el, newStream))
      } yield())
      v <- gets[S]
    } yield(v.counter)

    fstate(S(0, stream))()._1 should be (6)
  }

  it should "use FState as generator" in {
    implicit val m = new FStateMonad[Int]

    val stream = Ex4.genAsyncStream(0) {
      for {
        s <- gets[Int]
        _ <- puts(s + 1)
      } yield(s)
    } take 3

    stream.flatten() should be (0 :: 1 :: 2 :: Nil)
  }

  it should "use FState to generate finite stream" in {
    implicit val m = new FStateMonad[Int]

    val stream = Ex4.genAsyncStream(0) {
      for {
        s <- gets[Int]
        if (s < 3) // yes, we generate finite stream
        _ <- puts(s + 1)
      } yield(s)
    }

    stream.flatten() should be (0 :: 1 :: 2 :: Nil)
  }
}
