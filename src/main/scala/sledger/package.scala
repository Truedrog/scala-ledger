package sledger
import cats._
import cats.data._
import cats.syntax.all._
import sledger.data.Amounts.{Amount, MixedAmount}

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
  final case object Unmarked extends Status
  final case object Pending extends Status
  final case object Cleared extends Status
  object Status {
    implicit val showStatus: Show[Status] = Show[Status]
      {
        case Unmarked => ""
        case Pending => "!"
        case Cleared => "*"
      }
  }
  

  implicit val stringBuilderSemigroup: Semigroup[StringBuilder] = 
    (x: StringBuilder, y: StringBuilder) => new StringBuilder().append(x).append(y) 
  
  implicit val stringBuilderMonoid: Monoid[StringBuilder] = new Monoid[StringBuilder] {
    override def empty: StringBuilder = new StringBuilder("")

    override def combine(x: StringBuilder, y: StringBuilder): StringBuilder = {
      new StringBuilder().append(x).append(y)
    }
  }
}



