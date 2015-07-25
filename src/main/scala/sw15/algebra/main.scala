package net.debasishg.sw15.algebra
package domain.trade
package model

import java.util.Date

import scalaz._
import Scalaz._
import Free._

import TradeModel._

object Main {

  import TradeLifecycleModel._

  val trade = Trade("a-123", "google", "r-123", HongKong, 12.24, 100.5)

  val a = (t: Trade) => BigDecimal(1000)

  val lc = for {
    t <- valuation(trade)
    _ <- confirmation(t, Email)
    i <- accruedInterestCalculation(t)
    _ <- contractNoteGeneration(t)
    _ <- finalization(t) 
    _ <- contractNoteGeneration(t)
  } yield (())

  val inter = BasicTradeLifecycleInterpreter()
  val u = inter.interpret(lc, trade)
}
