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

}
