import Ex4._

import org.scalatest._
import scalaz._
import scalaz.syntax.monadPlus._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.language.implicitConversions

class Ex4Spec extends FlatSpec with Matchers {
  import Ex2._
  import Ex3._

  def makeStream(l: List[Int]) = genAsyncStream(l) { l =>
    if (l.isEmpty) Future(null)
    else Future((l.head, l.tail))
  }

  it should "work with 'foreach'" in {
    implicit val m = new FStateMonad[Int]
    import m._

    val fstate = for {
      _ <- foreach(makeStream(0 :: 1 :: 2 :: Nil)) { v =>
        mods(_ + 1)
      }
      v2 <- gets[Int]
    } yield v2

    Await.result(fstate(0), Duration.Inf) should be ((3, 3))
  }

  it should "work with 'foreach' 2" in {
    implicit val m = new FStateMonad[Int]

    val fstate = for {
      _ <- foreach(makeStream(0 :: 1 :: 2 :: 3 :: Nil)) { v =>
        for {
          s <- gets[Int]
          _ <- puts(s + v)
        } yield () 
      }
      v2 <- gets[Int]
    } yield v2

    Await.result(fstate(0), Duration.Inf) should be ((6, 6))
  }

  it should "work with 'isEmpty' and 'get'" in {
    case class S (counter: Int, stream: AsyncStream[Int]) // State
    implicit val m2 = new FStateMonad[S]

    val stream = makeStream(0 :: 1 :: 2 :: 3 :: Nil)

    val fstate = for {
      _ <- m2.whileM_(notEmpty(_.stream), for {
        s <- gets[S]
        (el, newStream) <- get[Int, S](s.stream)
        _ <- puts { S(s.counter + el, newStream) }
      } yield())
      v <- gets[S]
    } yield v.counter

    Await.result(fstate(S(0, stream)), Duration.Inf)._1 should be (6)
  }

  it should "use FState as generator" in {
    implicit val m = new FStateMonad[Int]

    val stream = Ex4.genAsyncStream(0) {
      for {
        s <- gets[Int]
        _ <- puts(s + 1)
      } yield s
    } take 3

    Await.result(stream.flatten, Duration.Inf) should be (0 :: 1 :: 2 :: Nil)
  }

  it should "use FState to generate finite stream" in {
    implicit val m = new FStateMonad[Int]

    val stream = Ex4.genAsyncStream(0) {
      for {
        s <- gets[Int]
        if (s < 3) // yes, we generate finite stream
        _ <- puts(s + 1)
      } yield s
    }

    Await.result(stream.flatten, Duration.Inf) should be (0 :: 1 :: 2 :: Nil)
  }
}
