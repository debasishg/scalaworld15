package net.debasishg.sw15.algebra
package domain.trade

import java.util.{ Date, Calendar }
import scalaz.{ Order => OrderZ, _ }
import Scalaz._
import Kleisli._
import ListT._

import interpreter.{ Trading, TradeModel }
import TradeModel._

object Main {
  val clientOrder = ClientOrder(
    Map("no" -> "o-124", "customer" -> "nomura", "instrument" -> "cisco/100/30-oracle/200/12")
  )
  val trades = Trading.tradeGeneration(NewYork, "b-123", List("c1-123", "c2-123")).run(clientOrder)

  println(s"trades = $trades")
}
