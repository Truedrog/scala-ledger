package sledger

import cats.{Eq, Order, Show}
import cats.syntax.all._
import cats.derived
import cats.kernel.Comparison
import io.estatico.newtype.macros.newtype
import sledger.data.amount.{Amount, MixedAmount}
import sledger.data.transaction._

import java.time.DayOfWeek

package object Types {

  val isDecimalMark: Char => Boolean = {
    case '.' => true
    case ',' => true
    case _ => false
  }

  
  case class BalanceAssertion(amount: Amount,
                              total: Boolean,
                              inclusive: Boolean,
                              positon: (Int, Int))
  
  sealed trait Status
  case object Unmarked extends Status
  case object Pending extends Status
  case object Cleared extends Status
  object Status {
    implicit val showStatus: Show[Status] = {
      case Unmarked => ""
      case Pending => "!"
      case Cleared => "*"
    }
  }
  
  type AccountName = String
  
  case class Journal()
  
}



