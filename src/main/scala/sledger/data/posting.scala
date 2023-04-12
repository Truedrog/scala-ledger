package sledger.data
import cats._
import cats.data._
import cats.syntax.all._

import sledger.Types.{AccountName, Status, Unmarked}
import sledger.Types.Status.showStatus
import sledger.data.amount.{MixedAmount, nullmixedamout}
import sledger.data.transaction.Transaction

import java.time.DayOfWeek

object posting {
  case class Posting(
                      date1: Option[DayOfWeek],
                      date2: Option[DayOfWeek],
                      status: Status,
                      account: AccountName,
                      amount: MixedAmount,
                      comment: String,
//                      balanceAssertion: Option[BalanceAssertion],
                      transaction: Option[Transaction]
                    )
  object Posting {
    implicit val showPosting: Show[Posting] = Show[Posting] { posting => 
      s"Posting {" +
        s"date = ${posting.date1.toString}" +
        s"date2 = ${posting.date2.toString}" +
        show"status = ${posting.status}"
        show"account = ${posting.account}" +
        show"amount = ${posting.amount}" +
        show"comment = ${posting.comment}" +
        s"transaction = txn"
    }
  }
  def nullsourcepos = ((1, 1), (2, 1))
  def nullposting: Posting = {
    Posting(date1 = None,
      date2 = None, 
      status= Unmarked, 
      account="",
      amount=nullmixedamout,
      comment = "",
//      balanceAssertion = None,
      transaction = None)
  }
  def posting: Posting = nullposting
  
  def renderCommentLines(t: String) = ???
}