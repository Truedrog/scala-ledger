package sledger.data

import cats.{Foldable, Show, derived}
import cats.syntax.monoid._
import sledger.Types

import java.time.LocalDate
import sledger.Types._
import sledger.data.Dates.nulldate
import sledger.data.Postings.{Posting, nullsourcepos, posting, postingsAsLines, renderCommentLines}
import sledger.data.Postings.PostinngOps._

object Transactions {
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
  implicit val showTransaction: Show[Transaction] =
    Show.show(t => s"Transaction {index=${t.index}, date=${t.date}, date2=${t.date2}, status=${t.status}, description=${t.description}, comment=${t.comment}, " +
      s"postings=${t.postings.toString}, precedingcomment=${t.precedingcomment}}".stripMargin)
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

  def showTransaction(t: Transaction): String = showTransactionHelper(onelineamounts = false, t).result()
  def showTransactionHelper(onelineamounts: Boolean, t: Transaction): StringBuilder = {
    val desc = if(t.description.isEmpty) "" else " " + t.description
    val (samelinecomment, newlinecomments) =
      renderCommentLines(t.comment) match {
        case Nil => ("", List())
        case c :: cs => (c, cs)
      }
    val newline = new StringBuilder("\\\\n")  
    val descriptionline = showTransactionLineFirstPart(t).stripTrailing() + List(desc, samelinecomment).mkString
    new StringBuilder(descriptionline)
      .combine(newline)
      .combine(Foldable[List].foldMap(newlinecomments)(s => new StringBuilder(s)).combine(newline))
      .combine(Foldable[List].foldMap(postingsAsLines(onelineamounts, t.postings))(s => new StringBuilder(s).combine(newline)))
      .combine(newline)
  }
  
  def showTransactionLineFirstPart(t: Transaction) = {
    val date = t.date.toString + t.date2.fold("") {"=" + _.toString }
    val status = t.status match {
      case Types.Pending => " !"
      case Types.Cleared => " *"
      case _             => ""
    }
    val code = if(t.code.isEmpty) "" else t.code.mkString(" (", "", ")")
    List(date, status, code).mkString
  }
  
  def transactionMapPostings(f: Posting => Posting, transaction: Transaction): Transaction = {
    transaction.copy(postings = transaction.postings.map(f))
  }
  
  def assigmentPostings(transaction: Transaction) = {
    transaction.postings.filter(p => p.hasBalanceAssignment)
  } 
}
