package net.debasishg.sw15.algebra
package domain.trade
package model

import java.util.{Date, Calendar}

import scalaz._
import Scalaz._

trait OrderModel {this: RefModel =>
  case class LineItem(ins: Instrument, qty: BigDecimal, price: BigDecimal)
  case class Order(no: String, date: Date, customer: Customer, items: List[LineItem])

  case class ClientOrderSheet(details: Map[String, String])

  def fromClientOrders: ClientOrderSheet => List[Order] = { cos =>
    List(Order("123", today, "abc", List.empty[LineItem]))
  }
}
