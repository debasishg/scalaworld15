package net.debasishg.sw15.algebra
package domain.trade
package model

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Task._
import Free._

import TradeModel._

trait TradeLifecycleInterpreter {
  def apply[A](action: TradeLifecycle[A]): Task[A]
}
  
/**
 * Basic interpreter that uses a global mutable Map to store the state
 * of computation
 */
case class SimpleTradeLifecycleInterpreter() extends TradeLifecycleInterpreter {

  def today = java.util.Calendar.getInstance.getTime

  def step[A](action: TradeLifecycleF[TradeLifecycle[A]]): Task[TradeLifecycle[A]] = action match {

    case Valuation(trade, onResult) =>
      now(onResult(enrich(trade)))

    case Confirmation(trade, mode, next) =>
      now(generateConfirmation(trade, mode)).map(_ => next)

    case AccruedInterestCalculation(trade, onResult) =>
      now(onResult(computeAccruedInterest(trade)))

    case ContractNoteGeneration(trade, next) =>
      now(generateContractNote(trade)).map(_ => next)
  }

  /**
   * Turns the AccountRepo script into a `Task` that executes it in a mutable setting
   */
  def apply[A](action: TradeLifecycle[A]): Task[A] = action.runM(step)
}

case class BasicTradeLifecycleInterpreter() {
  def interpret[A](script: TradeLifecycle[A], t: Trade): Trade = script.fold(_ => t, {

    case Valuation(trade, onResult) =>
      val trd = enrich(trade)
      interpret(onResult(trd), trd)

    case Confirmation(trade, mode, next) =>
      generateConfirmation(trade, mode)
      interpret(next, trade)

    case AccruedInterestCalculation(trade, onResult) =>
      interpret(onResult(computeAccruedInterest(trade)), trade)

    case ContractNoteGeneration(trade, next) =>
      generateContractNote(trade)
      interpret(next, trade)

    case Finalization(trade) =>
      finalizeTrade(trade)
  })
}
