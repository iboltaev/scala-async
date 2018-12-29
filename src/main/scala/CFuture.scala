import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import CFuture._

import scala.annotation.tailrec

sealed trait Cancel extends Function0[Unit] {
  def isContextKill = false
}

case class Regular(fn: () => Unit) extends Cancel {
  override def apply(): Unit = fn()
}

case class ContextKill(reg: Regular)(implicit ref: Ctx) extends Cancel {
  override def apply(): Unit = {
    def cancel(): Unit = {
      println("cancelling contextKill...")
      val c = ref.get()
      if (c.isDefined) {
        if (ref.compareAndSet(c, None)) {
          c.foreach(_())
        } else cancel()
      }
    }

    cancel()
  }
  override def isContextKill = true
}

case class CFuture[+A](f: Future[A], c: Cancel) {
  def cancel(): Unit = c()
}

class CFutureOps[A](val cf: CFuture[A])(implicit val ref: Ctx) {
  setRef(Some(cf.c))

  private def newC(o: Option[Cancel], n: Option[Cancel]): Option[Cancel] = (o, n) match {
    case (_, None) => o
    case (Some(Regular(f1)), Some(ContextKill(reg))) => Some(reg)
    case _ => n
  }

  @tailrec private def setRef(o: Option[Cancel]): Boolean = {
    val c = ref.get()
    if (c.isEmpty) {
      o.foreach(_())
      false
    }
    else if (o.isEmpty) true
    else {
      val success = ref.compareAndSet(c, newC(c, o))
      if (!success) setRef(o)
      else success
    }
  }

  def flatMap[B](fn: A => CFuture[B]): CFutureOps[B] = {
    val newF: Future[B] = cf.f.flatMap { a =>
      val c = ref.get()
      if (c.isEmpty) {
        Future.failed(new RuntimeException("cancelled 1"))
      } else {
        val cf2 = fn(a)
        if (!setRef(Some(cf2.c)))
          Future.failed(new RuntimeException("cancelled 2"))
        else
          cf2.f
      }
    }

    new CFutureOps[B](CFuture(newF, cf.c))(ref)
  }

  def map[B](fn: A => B): CFuture[B] = flatMap { a =>
    new CFuture[B](Future(fn(a)), cf.c)
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
  type Ctx = AtomicReference[Option[Cancel]]

  def fromFuture[A](f: Future[A], cancel: () => Unit) = {
    CFuture[A](f, Regular(cancel))
  }

  def withCtx[A](f: AtomicReference[Option[Cancel]] => CFuture[A]): CFuture[A] = {
    val ctx = new AtomicReference[Option[Cancel]](Some(Regular(() => {})))
    f(ctx)
  }

  implicit def toOps[A](cf: CFuture[A])(implicit ctx: Ctx): CFutureOps[A] = {
    new CFutureOps[A](cf)(ctx)
  }

  implicit def fromOps[A](cfo: CFutureOps[A])(implicit ctx: Ctx): CFuture[A] = {
    val newCancel = (cfo.cf.c, ctx.get) match {
      case (Regular(f1), Some(Regular(f2))) => ContextKill(Regular(f1))
      case _ => cfo.cf.c
    }

    CFuture(cfo.cf.f, newCancel)
  }
}