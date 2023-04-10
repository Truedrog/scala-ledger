package sledger.data

import java.time.{DayOfWeek, LocalDate}
import sledger.Types.Status
import sledger.data.posting.Posting
object transaction {
  case class Transaction(
                          index: Int,
                          precedingcomment: String,
                          sourcepos: ((Int, Int), (Int, Int)),
                          date: LocalDate,
                          date2: Option[LocalDate],
                          status: Status,
                          code: String,
                          description: String,
                          comment: String,
                          postings: List[Posting]
                        )
  
  def txnTieKnot(transaction: Transaction): Transaction = {
    transaction.copy(postings = transaction.postings.map(p => postingSetTransaction(transaction,p)))
  }

  def postingSetTransaction(t: Transaction, p:Posting) = p.copy(transaction = Some(t))
}
