import scala.language.implicitConversions

import org.scalatest._

import com.twitter.util._
import com.twitter.conversions.time._

import Ex1._

class Ex1Spec extends FlatSpec with Matchers {
  it should "work with 'flatMap' and 'map'" in {
    val t = for(
      a <- FState.unit[Int, Int](10);
      b = a + 1
    ) yield(b)

    t(0)() should be((11, 0))
  }

}
