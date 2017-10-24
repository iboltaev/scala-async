import com.twitter.util._

import scalaz._
import syntax.monad._

object Ex2 {

  case class FState[S, +A](f: S => Future[(A, S)]) {
    def apply(s: S) = f(s)
  }

  class FStateMonad[S] extends MonadPlus[({ type f[X] = FState[S, X]})#f] {
    type F[X] = FState[S, X]

    // we MUST NOT return 'NoFuture' here at all !!
    override def empty[A]: F[A] = FState((s: S) => Future(null))

    override def point[A](a: => A): F[A] = FState((s: S) => Future((a, s)))

    override def bind[A, B](m: F[A])(f: A => F[B]): F[B] =
      FState((s: S) => m(s) flatMap { pair => {
        if (pair eq null) Future(null)
        else f(pair._1)(pair._2) 
      }})

    override def plus[A](a: F[A],b: => F[A]): F[A] = bind(a)(_ => b)

    def conds(f: S => Boolean): F[Boolean] = bind(gets[S])(vs => point(f(vs)))
    def fconds(f: S => F[Boolean]): F[Boolean] = bind(gets[S])(f)
    def mods(f: S => S): F[S] = bind(gets[S])(vs => puts(f(vs)))

    def forM_[A](cond: S => Boolean, mod: S => S)(action: => F[A]): F[Unit] =
      whileM_(conds(cond), bind(action)(va => mods(mod)))

    def whileM_[A](cond: S => Boolean)(body: => F[A]): F[Unit] =
      whileM_(conds(cond), body)
  }

  def gets[S](): FState[S, S] = FState((s: S) => Future((s, s)))
  def puts[S](news: S): FState[S, S] = FState((_: S) => Future((news, news)))

  def conds[S : FStateMonad](f: S => Boolean) = {
    val m = implicitly[FStateMonad[S]]
    m.conds(f)
  }

  def fconds[S : FStateMonad](f: S => FState[S, Boolean]) = {
    val m = implicitly[FStateMonad[S]]
    m.fconds(f)
  }

  def mods[S : FStateMonad](f: S => S) = {
    val m = implicitly[FStateMonad[S]]
    m.mods(f)
  }
}
