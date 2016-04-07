import com.twitter.util._

import scalaz._

object Ex3 {

  class Pair[A, B](firstp: => A, secondp: => B) {
    val first = firstp
    lazy val second = secondp
  }

  object Pair {
    def apply[A, B](first: => A, second: => B) = new Pair[A, B](first, second)
  }

  // 'null' value in Future means stream end; there should be no 'NoFuture'
  case class AsyncStream[A](data: Future[Pair[A, AsyncStream[A]]]) {
    def foldLeft[B](start: B)(f: (B, A) => B): Future[B] = {
      def impl(d: Future[Pair[A, AsyncStream[A]]], acc: Future[B]): Future[B] =
        d flatMap (pair => {
          if (pair eq null) acc
          else impl(pair.second.data, acc map (b => f(b, pair.first)))
        })

      impl(data, Future(start))
    }

    def flatten : Future[List[A]] =
      foldLeft[List[A]](Nil)((list, el) => el :: list) map (_.reverse)

    def takeWhile(p: A => Boolean): AsyncStream[A] =
      new AsyncStream[A](data map (pair => {
        if (pair eq null) null
        else if (!p(pair.first)) null
        else Pair(pair.first, pair.second.takeWhile(p))
      }))

    def take(n: Int): AsyncStream[A] =
      if (n <= 0) nil
      else new AsyncStream[A](data map (pair => {
        if (pair eq null) null
        else Pair(pair.first, pair.second.take(n - 1))
      }))
  }

  // 'gen' must return 'NoFuture' or 'Future(null)' in case of stream end
  def genAsyncStream[S,A](start: S)(gen: S => Future[(A, S)]): AsyncStream[A] =
    new AsyncStream[A](
      gen(start) match {
        case _: NoFuture => Future(null)
        case future => future map (pair => { // Future[Pair[A, AsyncStream]]
          if (pair eq null) null
          else Pair(pair._1, genAsyncStream(pair._2)(gen))
        })})

  def nil[A] = new AsyncStream[A](Future(null))

  def unit[A](a: => A) = new AsyncStream[A](
    Future(Pair(a, nil)))

  def concat[A](s1: AsyncStream[A], s2: AsyncStream[A]): AsyncStream[A] =
    new AsyncStream[A](s1.data flatMap (pair => {
      if (pair eq null) s2.data
      else Future(Pair(pair.first, concat(pair.second, s2)))
    }))


  class AsyncStreamMonad extends MonadPlus[AsyncStream] {
    override def empty[A] = nil[A]

    override def point[A](a: => A): AsyncStream[A] = unit(a)

    override def plus[A](a: AsyncStream[A], b: => AsyncStream[A]) = concat(a, b)

    override def bind[A, B](
      ma: AsyncStream[A])(f: A => AsyncStream[B]): AsyncStream[B] = //new AsyncNil
      new AsyncStream[B](ma.data flatMap (pair => {
        if (pair eq null) Future(null)
        else f(pair.first).data map (
          pair2 => Pair(pair2.first, concat(pair2.second, bind(pair.second)(f))))
      }))
  }

}
