package net.debasishg.sw15.algebra
package domain.trade
package dsl

import scalaz._
import Kleisli._
import Scalaz._

/**
clientOrders: ClientOrderSheet => List[Order]
execution: Order => List[Execution]

Generalizing, we see that we have 2 functions f: A => M[B] and g: B => M[C], where M is a monad. We would like to compose them. One obvious way will be to define a mapping from M[B] => B, which is really not an interesting proposition, as it rhows away the effects and takes away the interesting bits from our computation. Instead what we could do is use the fact that M is a Functor (as it's a monad) and use the functor to lift g to a function M[B] => M[M[C]]. But again since M is monad we can flatten the 2 M's into 1 with join combinator. Thus we get M[B] => M[C], and this nicely composes with f.

Now to define the algebra we have to define a combinator that does exactly what we discussed.

scala> def andThen[M[_], A, B, C](f: A => M[B], g: B => M[C])(implicit m: Bind[M]): A => M[C] = {(a: A) => m.join(m.map(f(a))(g))}

scala> def andThen[M[_], A, B, C](f: A => M[B], g: B => M[C])(implicit m: Bind[M]): A => M[C] = {(a: A) => m.bind(f(a))(g)}

case class Kleisli[M[_], A, B](run: A => M[B]) {
  def andThen[C](f: B => M[C])(implicit M: Monad[M]): Kleisli[M, A, C] =
    Kleisli((a: A) => M.flatMap(run(a))(f))

We can use the power of monads to compose not only functions of ordinary types, but also effectful ones. In complex domain models we (almost) always use effects to model domain behaviors and monads give us a potent technique to build our functionality incrementally using effectful function composition.

**/
/*
object TradeDsl {

  import model.TradeModel._

  /**
   * processes client orders in unstructured form and generates a list of Order objects
   *
   * ClientOrderSheet => List[Order]
   */
  def clientOrders: Kleisli[List, ClientOrderSheet, Order] = kleisli(fromClientOrders)

  /**
   * executes an Order in a market on behalf of a specific broker account
   * generates a sequence of Executions
   *
   * Order => List[Execution]
   */
  def execute(market: Market, brokerAccount: Account): Kleisli[List, Order, Execution] = kleisli { order =>
    order.items.map {item =>
      Execution(brokerAccount, item.ins, "e-123", market, item.price, item.qty)
    }
  }

  /**
   * allocates an execution to a List of client accounts
   * generates a List of trades
   *
   * Execution => String \/ Trade
   */
  def allocate(accounts: List[Account]): Kleisli[List, Execution, \/[String, Trade]] = kleisli { execution =>
    val q = execution.quantity / accounts.size
    accounts.map {account =>
      makeTrade(account, execution.instrument, "t-123", execution.market, execution.unitPrice, q)
    }
  }

  def tradeGeneration(market: Market, broker: Account, clientAccounts: List[Account]) = {
    // client orders         executed at market by broker        & allocated to client accounts
    clientOrders      >=>    execute(market, broker)       >=>   allocate(clientAccounts)
  }
}
*/
