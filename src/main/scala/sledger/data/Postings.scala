package sledger.data

import java.time.DayOfWeek
import cats.{Group => _, _}
import cats.syntax.all._
import sledger.Types.{AccountName, Status, Unmarked}
import sledger.data.Amounts.{MixedAmount, noColour, nullmixedamout, showMixedAmountLinesB}
import sledger.data.Transactions.Transaction
import sledger.text.tabular.Ascii._
import io.github.akiomik.seaw.implicits._
import sledger.Types
import sledger.text.WideString.WideBuilder
import sledger.text.tabular.Tabular.{Group, Header, NoLine}
import utils.Text.fitText
import utils.maximumBound

object Postings {
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
      status = Unmarked,
      account = "",
      amount = nullmixedamout,
      comment = "",
      //      balanceAssertion = None,
      transaction = None)
  }

  def posting: Posting = nullposting

  def showPosting(posting: Posting): String = {
    postingsAsLines(onelineamounts = false, List(posting)).mkString("", "\n", "\n")
  }

  def showPostingLines(posting: Posting): List[String] = {
    postingAsLines(elideamount = false, onelineamounts = false)(posting)._1
  }

  def postingsAsLines(onelineamounts: Boolean, ps: List[Posting]): List[String] = {
    ps.map { p =>
      postingAsLines(elideamount = false,
        onelineamounts = onelineamounts
      )(p)
    }.flatMap(_._1)
  }

  def postingAsLines(elideamount: Boolean,
                     onelineamounts: Boolean,
                     acctwidth: Int = 2,
                     amtwidth: Int = 12)(p: Posting): (List[String], Int, Int) = {
    val pacctstr: Posting => String = p => showAccountName(None, p.account)
    val pstatusprefix: Posting => String = p => p.status match {
      case Types.Unmarked => ""
      case s => s.toString + " "
    }
    val pstatusandacct: Posting => String =
      p => pstatusprefix(p) + pacctstr(p)
    val statusandaccount: String =
      lineIndent(fitText(Some(2 + acctwidth), None, ellipsify = false, rightside = true)(pstatusandacct(p)))
    val thisacctwidth = pacctstr(p).width
    val shownAmounts: List[WideBuilder] = {
      if (elideamount) {
        Monoid[List[WideBuilder]].empty
      } else {
        showMixedAmountLinesB(noColour.copy(displayOneLine = onelineamounts), p.amount)
      }
    }
    val shownAmountsAssertions = shownAmounts //todo maybe add balance assertions
    val thisamtwidth = maximumBound(0)(shownAmounts.map(_.width))
    val (samelinecomments, newlinecomments) =
      renderCommentLines(p.comment) match {
        case Nil => ("", List())
        case c :: cs => (c, cs)
      }

    def pad(amt: WideBuilder): WideBuilder = {
      val w = 12.max(amtwidth) - amt.width
      WideBuilder(new StringBuilder(" " * w), w) |+| amt
    }

    val postingblock = for {amt <- shownAmountsAssertions} yield {
      renderRow(
        TableOpts().copy(tableBorders = false, borderSpaces = false),
        Group(NoLine, List(
          textCell(BottomLeft, statusandaccount),
          textCell(BottomLeft, "  "),
          textCell(BottomLeft, "  "),
          Cell(BottomLeft, List(pad(amt))),
          textCell(BottomLeft, samelinecomments)).map(c => Header(c))
        )).split("\\\\n").toList.map(_.stripTrailing)
    }
    (postingblock.flatMap(a => a ++ newlinecomments), thisacctwidth, thisamtwidth)
  }

  def showAccountName(mwidth: Option[Int], accountName: AccountName): AccountName =
    mwidth.fold(accountName)(accountName.take(_))

  def renderCommentLines(t: String): List[String] = {

    val comment = (t: String) => "; " + t
    val lines = t.linesIterator.toSeq
    lines match {
      case Seq() => Seq().toList
      case Seq(l) => Seq(commentSpace(comment(l))).toList
      case "" +: ls => ls.map(a => lineIndent(comment(a))).toList
      case l +: ls => commentSpace(comment(l)) +: ls.map(a => lineIndent(comment(a))).toList
    }
  }

  def lineIndent(t: String): String = "    " + t

  def commentSpace(t: String): String = "  " + t

}
