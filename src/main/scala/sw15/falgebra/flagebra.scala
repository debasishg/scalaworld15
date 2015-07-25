package net.debasishg.sw15.falgebra

import scalaz._
import Scalaz._
import scala.language.higherKinds

/**
 * http://stackoverflow.com/questions/16015020/what-does-coalgebra-mean-in-the-context-of-programming
 **/
object FAlgebra {
  def op[T: Monoid](arg: \/[(T, T), T]): T = {
    val m = implicitly[Monoid[T]]
    arg match {
      case -\/((a, b)) => m.append(a, b)
      case \/-(a)      => m.zero
    }
  }
  
  sealed trait MonoidArgument[+T]
  case class Mappend[T](t1: T, t2: T) extends MonoidArgument[T]
  case object Mempty extends MonoidArgument[Nothing]
  
  implicit def MonoidArgumentFunctor: Functor[MonoidArgument] = new Functor[MonoidArgument] {
    def map[A, B](a: MonoidArgument[A])(f: A => B): MonoidArgument[B] = a match {
      case Mappend(x, y) => Mappend(f(x), f(y))
      case Mempty => Mempty
    }
  }
  
  /**
   * An F-algebra has the following components:
   *
   * (a) an endofunctor `F` in category `C`
   * (b) an object `A` in that category
   * (c) a morphism `F[A] => A`, which is the evaluator of the algebra
   */
  abstract class Algebra[F[_]: Functor, A] {
    def op: F[A] => A
  }

  /**
   * Build using the monad (Free monad) and evaluate using the F-algebra
   */
/*
  sealed trait AccountRepoF[+A]
    
  case class Query[+A](no: String, onResult: Account => A) extends AccountRepoF[A]
  case class Store[+A](account: Account, next: A) extends AccountRepoF[A]
  case class Delete[+A](no: String, next: A) extends AccountRepoF[A]
  
  object AccountRepoF {
    implicit val functor: Functor[AccountRepoF] = new Functor[AccountRepoF] {
      def map[A,B](action: AccountRepoF[A])(f: A => B): AccountRepoF[B] = action match {
        case Store(account, next) => Store(account, f(next))
        case Query(no, onResult) => Query(no, onResult andThen f)
        case Delete(no, next) => Delete(no, f(next))
      }
    }
  }

  type AccountRepo[A] = Free[AccountRepoF, A]
*/
}


