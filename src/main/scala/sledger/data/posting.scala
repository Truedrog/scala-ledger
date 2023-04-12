package sledger.data

import sledger.Types.{AccountName, BalanceAssertion, Status, Unmarked}
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
}
