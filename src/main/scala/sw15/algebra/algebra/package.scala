package net.debasishg.sw15.algebra
package domain

import scalaz._
import Scalaz._

package object trade {
  type StringOr[A] = String \/ A
  type Valid[A] = ListT[StringOr, A]
}



