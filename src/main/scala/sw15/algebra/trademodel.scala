package net.debasishg.sw15.algebra
package domain.trade
package model

import java.util.{ Date, Calendar }

import scalaz._
import Scalaz._
import \/._

trait TradeModel {this: RefModel =>

  case class Trade private[model] (account: Account, instrument: Instrument, refNo: String, market: Market,
    unitPrice: BigDecimal, quantity: BigDecimal, tradeDate: Date = today,
    valueDate: Option[Date] = None, taxFees: Option[List[(TaxFeeId, BigDecimal)]] = None, 
    netAmount: Option[BigDecimal] = None)

  sealed trait TaxFeeId
  case object TradeTax extends TaxFeeId
  case object Commission extends TaxFeeId
  case object VAT extends TaxFeeId
  case object Surcharge extends TaxFeeId

  type ValidationStatus[S] = \/[String, S]

  type ReaderTStatus[A, S] = ReaderT[ValidationStatus, A, S]

  object ReaderTStatus extends KleisliInstances with KleisliFunctions {
    def apply[A, S](f: A => ValidationStatus[S]): ReaderTStatus[A, S] = kleisli(f)
  }

  private def validQuantity = ReaderTStatus[Trade, Trade] { trade =>
    if (trade.quantity < 0) left(s"Quantity needs to be > 0 for $trade") else right(trade)
  }

  private def validUnitPrice = ReaderTStatus[Trade, Trade] { trade =>
    if (trade.unitPrice < 0) left(s"Unit Price needs to be > 0 for $trade") else right(trade)
  }

  private def validValueDate = ReaderTStatus[Trade, Trade] { trade =>
    trade.valueDate.map(vd => 
      if (trade.tradeDate after vd) left(s"Trade Date ${trade.tradeDate} must be before value date $vd")
      else right(trade)).getOrElse(right(trade))
  }

  def makeTrade(account: Account, 
    instrument: Instrument, 
    refNo: String, 
    market: Market, 
    unitPrice: BigDecimal, 
    quantity: BigDecimal,
    td: Date = today, 
    vd: Option[Date] = None): ValidationStatus[Trade] = {

    val trd = Trade(account, instrument, refNo, market, unitPrice, quantity, td, vd)
    val s = for {
      _ <- validQuantity
      _ <- validValueDate
      t <- validUnitPrice
    } yield t
    s(trd)
  }

  def enrich(trade: Trade): Trade = trade
  def generateConfirmation(trade: Trade, mode: DespatchMode) = println(s"confirmation generated for $trade")
  def computeAccruedInterest(trade: Trade) = AccruedInterest(BigDecimal(100), today, today)
  def generateContractNote(trade: Trade) = println(s"contract note generated for $trade")
  def finalizeTrade(trade: Trade) = trade
}

object TradeModel extends ExecutionModel with OrderModel with RefModel with TradeModel with ContractNoteModel 
