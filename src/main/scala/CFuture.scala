import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import CFuture._

case class CFuture[+A](f: Future[A], c: Cancel)

class CFutureOps[A](val cf: CFuture[A])(implicit val ref: Ctx) {
  setRef(Some(cf.c))

  def setRef(o: Option[Cancel]) = {
    ref.set(o)
  }

  def flatMap[B](fn: A => CFuture[B]): CFutureOps[B] = {
    val newF: Future[B] = cf.f.flatMap { a =>
      val c = ref.get()
      if (c.isEmpty) {
        Future.failed(new RuntimeException("cancelled"))
      } else {
        val cf2 = fn(a)
        setRef(Some(cf2.c))
        cf2.f
      }
    }

    new CFutureOps[B](CFuture(newF, cf.c))(ref)
  }

  def map[B](fn: A => B): CFuture[B] = flatMap { a =>
    new CFuture[B](CFuture(fn(a), cf.c))
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

  def fromFuture[A](f: Future[A], cancel: Cancel) = {
    CFuture[A](f, cancel)
  }

  def withCtx[A](f: AtomicReference[Option[Cancel]] => CFuture[A]): CFuture[A] = {
    val ctx = new AtomicReference[Option[Cancel]](Some(() => {}))
    f(ctx)
  }

  implicit def toOps[A](cf: CFuture[A])(implicit ctx: Ctx): CFutureOps[A] = {
    new CFutureOps[A](cf)(ctx)
  }

  implicit def fromOps[A](cfo: CFutureOps[A])(implicit ctx: Ctx): CFuture[A] = {
    cfo.cf
  }
}