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

  implicit val m = new FStateMonad[Int]

  def makeStream(l: List[Int]) = genAsyncStream(l)(
    l => if (l.isEmpty) new NoFuture else Future((l.head, l.tail)))

  it should "work with 'foreach'" in {
    val fstate = for (
      _ <- foreach(makeStream(0 :: 1 :: 2 :: Nil)) {
        v => m.mods(_ + 1)
      };
      v2 <- gets[Int]
    ) yield (v2)

    fstate(0)() should be ((3, 3))
  }

  it should "work with 'isEmpty' and 'get'" in {
    case class S (counter: Int, stream: AsyncStream[Int]) // State
    implicit val m2 = new FStateMonad[S]

    val stream = makeStream(0 :: 1 :: 2 :: 3 :: Nil)

    val fstate = for(
      _ <- m2.whileM_(notEmpty(_.stream), for(
        s <- gets[S];
        (el, newStream) <- get[Int, S](s.stream);
        _ <- puts(S(s.counter + el, newStream))
      ) yield());
      v <- gets[S]
    ) yield(v.counter)

    fstate(S(0, stream))()._1 should be (6)
  }
}
