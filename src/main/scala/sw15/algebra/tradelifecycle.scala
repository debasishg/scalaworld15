package net.debasishg.sw15.algebra
package domain.trade
package model

import java.util.Date

import scalaz._
import Scalaz._
import Free._

import TradeModel._

sealed trait DespatchMode
case object Email extends DespatchMode
case object SnailMail extends DespatchMode

case class AccruedInterest(amount: BigDecimal, from: Date, to: Date)

sealed trait TradeLifecycleF[+A]
  
case class Valuation[+A](trade: Trade, onResult: Trade => A) extends TradeLifecycleF[A]
case class Confirmation[+A](trade: Trade, mode: DespatchMode, next: A) extends TradeLifecycleF[A]
case class AccruedInterestCalculation[+A](trade: Trade, onResult: AccruedInterest => A) extends TradeLifecycleF[A]
case class ContractNoteGeneration[+A](trade: Trade, next: A) extends TradeLifecycleF[A]
case class Finalization(trade: Trade) extends TradeLifecycleF[Nothing]

object TradeLifecycleF {
  implicit val functor: Functor[TradeLifecycleF] = new Functor[TradeLifecycleF] {
    def map[A,B](action: TradeLifecycleF[A])(f: A => B): TradeLifecycleF[B] = action match {
      case Valuation(t, onResult) => Valuation(t, onResult andThen f)
      case Confirmation(t, m, next) => Confirmation(t, m, f(next))
      case AccruedInterestCalculation(t, onResult) => AccruedInterestCalculation(t, onResult andThen f)
      case ContractNoteGeneration(t, next) => ContractNoteGeneration(t, f(next))
      case Finalization(t) => Finalization(t)
    }
  }
}

trait TradeLifecycleModel {
  def valuation(trade: Trade): TradeLifecycle[Trade] = 
    liftF(Valuation(trade, identity))
  
  def confirmation(trade: Trade, mode: DespatchMode): TradeLifecycle[Unit] = 
    liftF(Confirmation(trade, mode, ()))
  
  def accruedInterestCalculation(trade: Trade): TradeLifecycle[AccruedInterest] = 
    liftF(AccruedInterestCalculation(trade, identity))
  
  def contractNoteGeneration(trade: Trade): TradeLifecycle[Unit] = 
    liftF(ContractNoteGeneration(trade, ()))
  
  def finalization(trade: Trade): TradeLifecycle[Unit] = 
    liftF(Finalization(trade))
}

object TradeLifecycleModel extends TradeLifecycleModel

