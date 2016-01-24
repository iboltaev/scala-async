import com.twitter.util._

import scalaz._
import syntax.monad._

object Ex2 {

  case class FState[S, +A](f: S => Future[(A, S)]) {
    def apply(s: S) = f(s)
  }

  class FStateMonad[S] extends Monad[({ type f[X] = FState[S, X]})#f] {
    type F[X] = FState[S, X]
    override def point[A](a: => A): F[A] = FState((s: S) => Future((a, s)))
    override def bind[A, B](m: F[A])(f: A => F[B]): F[B] =
      FState((s: S) => m(s) flatMap { pair => f(pair._1)(pair._2) })

    def conds(f: S => Boolean): F[Boolean] = bind(gets[S])(vs => point(f(vs)))
    def mods(f: S => S): F[S] = bind(gets[S])(vs => puts(f(vs)))

    def forM_[A](cond: S => Boolean, mod: S => S)(action: => F[A]): F[Unit] =
      whileM_(conds(cond), bind(action)(va => mods(mod)))
  }

  def gets[S](): FState[S, S] = FState((s: S) => Future((s, s)))
  def puts[S](news: S): FState[S, S] = FState((_: S) => Future((news, news)))

  def conds[S : FStateMonad](f: S => Boolean) = {
    val m = implicitly[FStateMonad[S]]
    m.conds(f)
  }

  def mods[S : FStateMonad](f: S => S) = {
    val m = implicitly[FStateMonad[S]]
    m.mods(f)
  }
}
