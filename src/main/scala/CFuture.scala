import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CFuture[A](val f: Future[A])(implicit val ref: CFuture.Ctx) {

  def flatMap[B](fn: A => CFuture[B]): CFuture[B] = {
    val newF: Future[B] = f.flatMap { a =>
      val c = ref.get()
      if (c.isEmpty) {
        Future.failed(new RuntimeException("cancelled"))
      } else {
        val cf = fn(a)
        if (cf.ref ne ref)
          throw new RuntimeException("different context")
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
  type Cancel = () => Unit
  type Ctx = AtomicReference[Option[Cancel]]

  def fromFuture[A](f: Future[A], cancel: () => Unit)(implicit ctx: Ctx) = {
    ctx.set(Some(cancel))
    new CFuture[A](f)
  }

  def withCtx[A](f: AtomicReference[Option[() => Unit]] => CFuture[A]): CFuture[A] = {
    val ctx = new AtomicReference[Option[() => Unit]](Some(() => {}))
    f(ctx)
  }
}