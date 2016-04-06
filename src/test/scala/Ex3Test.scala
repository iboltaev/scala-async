import scala.language.implicitConversions

import org.scalatest._

import com.twitter.util._
import com.twitter.conversions.time._

import scalaz._
import syntax.monadPlus._

import Ex3._

class Ex3Spec extends FlatSpec with Matchers {
  def makeStream(l: List[Int]) = genAsyncStream(l)(
    l => if (l.isEmpty) new NoFuture else Future((l.head, l.tail)))

  def makeInfStream = genAsyncStream(0)(v => Future((v, v + 1)))

  it should "work with 'foldLeft'" in {
    val s2 = makeStream(2 :: 3 :: Nil)
    val f = s2.foldLeft(List[Int]())((list, el) => el :: list)
    f() should be (List(3, 2))
  }

  it should "properly 'concat' streams" in {
    val s1 = makeStream(0 :: 1 :: Nil)
    val s2 = makeStream(2 :: 3 :: Nil)
    val s3 = concat(s1, s2)

    s3.flatten() should be (List(0, 1, 2, 3))
  }

  it should "correctly work as monad" in {
    implicit val m = new AsyncStreamMonad

    val s1 = makeStream(0 :: 1 :: Nil)
    val s2 = makeStream(2 :: 3 :: Nil)

    val s3 = for(
      v <- s1;
      v2 <- s2) yield((v, v2))

    s3.flatten() should be (List((0, 2), (0, 3), (1, 2), (1, 3)))
  }

  it should "work with 'takeWhile'" in {
    val s = makeInfStream takeWhile (_ < 4)
    s.flatten() should be (List(0, 1, 2, 3))
  }

  it should "work with 'flatten'" in {
    val list = 0 :: 1 :: 2 :: 3 :: Nil
    val s = makeStream(list)
    s.flatten() should be (list)
  }

  it should "fold large stream without stackoverflow" in {
    val stream = makeInfStream takeWhile (_ < 1000000)
    stream.flatten() should be (0 to 999999)
  }

}
