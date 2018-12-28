import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CFuture[A](val f: Future[A])(implicit val ref: AtomicReference[Option[() => Unit]]) {
  def flatMap[B](fn: A => CFuture[B]): CFuture[B] = {
    val newF: Future[B] = f.flatMap { a =>
      val c = ref.get()
      if (c.isEmpty) {
        Future.failed(new Throwable("cancelled"))
      } else {
        val cf = fn(a)
        if (cf.ref ne ref)
          throw new Throwable("different context")
        else
          cf.f
      }
    }

    new CFuture[B](newF)
  }

  def map[B](fn: A => B): CFuture[B] = flatMap { a =>
    new CFuture[B](Future(fn(a)))
  }

  def cancel(): Unit = {
    val c = ref.get()
    if (c.isDefined) {
      if (ref.compareAndSet(c, None)) {
        c.foreach(_())
      } else cancel()
    }
  }
}

object CFuture {
  def fromFuture[A](f: Future[A], cancel: () => Unit)(implicit ctx: AtomicReference[Option[() => Unit]]) = {
    ctx.set(Some(cancel))
    new CFuture[A](f)
  }
}