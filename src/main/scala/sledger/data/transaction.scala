package sledger.data

import java.time.LocalDate
import sledger.Types.{Status, Unmarked}
import sledger.data.dates.nulldate
import sledger.data.posting.{Posting, nullsourcepos}

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
  def nulltransaction: Transaction = Transaction (
    index = 0,
    sourcepos = nullsourcepos,
    date = nulldate,
    date2 = None,
    status = Unmarked,
    code = "",
    description = "",
    comment = "",
    postings = List(),
    precedingcomment = ""
  )
  def txnTieKnot(transaction: Transaction): Transaction = {
    transaction.copy(postings = transaction.postings.map(p => postingSetTransaction(transaction,p)))
  }

  def postingSetTransaction(t: Transaction, p:Posting): Posting = p.copy(transaction = Some(t))
}
