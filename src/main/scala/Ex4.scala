import com.twitter.util._

import scalaz._
import syntax.monad._

object Ex4 {

  def foreach[A, S]
    (stream: Ex3.AsyncStream[A])(f: A => Ex2.FState[S, Any]): Ex2.FState[S, Unit] = 
    Ex2.FState(s => {
      stream.foldLeft(Future(s))(
      (futureS, a) => futureS.flatMap(s2 => f(a)(s2).map(_._2))).flatten.map( ((), _) )
    })

  def isEmpty[A, S](stream: Ex3.AsyncStream[A]): Ex2.FState[S, Boolean] =
    Ex2.FState(s => stream.data map (pair => (pair eq null, s)))

  def isEmpty[A, S : Ex2.FStateMonad](f: S => Ex3.AsyncStream[A]): Ex2.FState[S, Boolean] = {
    val m = implicitly[Ex2.FStateMonad[S]]
    m.fconds(s => isEmpty(f(s)))
  }

  def notEmpty[A, S](stream: Ex3.AsyncStream[A]): Ex2.FState[S, Boolean] =
    Ex2.FState(s => stream.data map (pair => (!(pair eq null), s)))

  def notEmpty[A, S : Ex2.FStateMonad](f: S => Ex3.AsyncStream[A]): Ex2.FState[S, Boolean] = {
    val m = implicitly[Ex2.FStateMonad[S]]
    m.fconds(s => notEmpty(f(s)))
  }

  def get[A, S](stream: Ex3.AsyncStream[A]): Ex2.FState[S, (A, Ex3.AsyncStream[A])] =
    Ex2.FState(s => stream.data map (pair => ((pair.first, pair.second), s) ))

  def genAsyncStream[S,A](start: S)(gen: Ex2.FState[S, A]) =
    Ex3.genAsyncStream(start)(gen.f)

  implicit val asMonad = new Ex3.AsyncStreamMonad
}
