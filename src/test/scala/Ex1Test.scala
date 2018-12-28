import Ex1._
import org.scalatest._

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.language.implicitConversions

class Ex1Spec extends FlatSpec with Matchers {
  it should "work with 'flatMap' and 'map'" in {
    val t = for {
      a <- FState.unit[Int, Int](10);
      b = a + 1
    } yield b

    Await.result(t(0), Duration.Inf) should be((11, 0))
  }

}
