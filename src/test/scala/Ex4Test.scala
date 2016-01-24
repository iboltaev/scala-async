import scala.language.implicitConversions

import org.scalatest._

import com.twitter.util._
import com.twitter.conversions.time._

import scalaz._
import syntax.monad._

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
}
