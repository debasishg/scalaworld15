package net.debasishg.sw15.algebra
package domain.trade

import scalaz.Free

package object model {
  type TradeLifecycle[A] = Free[TradeLifecycleF, A]
}


