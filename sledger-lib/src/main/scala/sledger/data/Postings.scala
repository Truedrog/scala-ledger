package sledger.data

import cats.{Group => _, _}
import cats.syntax.all._
import io.github.akiomik.seaw.implicits._
import sledger._
import sledger.data.AccountNames.AccountName
import sledger.data.Amounts._
import sledger.data.Dates.{PrimaryDate, SecondaryDate, WhichDate, nulldate}
import sledger.data.Transactions.Transaction
import sledger.text.WideString.WideBuilder
import sledger.text.tabular.Ascii._
import sledger.text.tabular.Tabular.{Group, Header, NoLine}
import sledger.utils.Text.fitText
import sledger.utils.maximumBound

import java.time.{DayOfWeek, LocalDate}

object Postings {
  case class Posting(
                      date1: Option[LocalDate],
                      date2: Option[LocalDate],
                      status: Status,
                      account: AccountName,
                      amount: MixedAmount,
                      comment: String,
                      original: Option[Posting],
                      //                      balanceAssertion: Option[BalanceAssertion],
                      transaction: Option[Transaction]
                    ) {

    override def toString: String = {
      s"\nPosting {" +
        s"date = ${this.date1.toString} " +
        s"date2 = ${this.date2.toString} " +
        s"status = ${this.status.toString} " +
        s"account = ${this.account} " +
        s"amount = ${this.amount.toString} " +
        s"comment = ${this.comment} " +
        s"transaction = txn} \n"
    }

    override def equals(o: Any): Boolean = { // for tests

      o match {
        case other@Posting(_, _, _, _, _, _, _, _) =>
          this.date1 == other.date1 &&
            this.date2 == other.date2 &&
            this.status == other.status &&
            this.account == other.account &&
            this.amount == other.amount &&
            this.comment == other.comment
        case _ => false
      }
    }
  }

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
    implicit val eqPosting: Eq[Posting] = (x: Posting, y: Posting) => {
      x.account === y.account && x.amount === y.amount && x.date1 == y.date1 && x.date2 == y.date2 && x.status == y.status
    }
  }

  object PostinngOps {
    implicit class PostingExtensions(p: Posting) {
      def hasAmount: Boolean = !isMissingMixedAmount(p.amount)

//      def hasBalanceAssignment: Boolean = {
//        !p.hasAmount
//      }
    }
  }

  def nullsourcepos = ((1, 1), (2, 1))

  def nullposting: Posting = {
    Posting(date1 = None,
      date2 = None,
      status = Unmarked,
      account = "",
      amount = nullMixedAmount,
      comment = "",
      original = None,
      //      balanceAssertion = None,
      transaction = None)
  }

  def posting: Posting = nullposting

  def postingDate(posting: Posting): LocalDate = {
    val dates = List(posting.date1, posting.transaction.map(_.date))
    Alternative[Option].combineAllK(dates).getOrElse(nulldate)
  }
  
  def postingDate2(posting: Posting): LocalDate = {
    val dates = List(posting.date2, posting.transaction.flatMap(_.date2), posting.date1, posting.transaction.map(_.date))
    Alternative[Option].combineAllK(dates).getOrElse(nulldate)
  }
  
  def postingDateOrDate2(wd: WhichDate, posting: Posting): LocalDate = {
    wd match {
      case PrimaryDate => postingDate(posting)
      case SecondaryDate => postingDate2(posting)
    }
  }

  def post(acc: AccountName, amt: Amount): Posting = posting.copy(amount = mixedAmount(amt), account = acc)

  def sumPostings(postings: List[Posting]): MixedAmount =
    Monoid[MixedAmount].combineAll(postings.map(_.amount))

  def accountNamesFromPostings(postings: List[Posting]): List[AccountName] = {
    postings.map(_.account).distinct
  }

  def originalPosting(posting: Posting): Posting = {
    posting.original.getOrElse(posting)
  }

  def showPosting(posting: Posting): String = {
    postingsAsLines(onelineamounts = false, List(posting)).mkString("", "\n", "\n")
  }

  def showPostingLines(posting: Posting): List[String] = {
    postingAsLines(elideamount = false, onelineamounts = false)(posting)._1
  }

  def postingsAsLines(onelineamounts: Boolean, ps: List[Posting]): List[String] = {
   val xs = ps.map { p =>
      postingAsLines(elideamount = false,
        onelineamounts = onelineamounts
      )(p)
    }
    val accw = maximumBound(0)(xs.map(_._2))
    val amtw = maximumBound(0)(xs.map(_._3))
    ps.map { p =>
      postingAsLines(elideamount = false,
        onelineamounts = onelineamounts, accw, amtw
      )(p)
    }.flatMap(_._1)
  }

  def postingAsLines(elideamount: Boolean,
                     onelineamounts: Boolean,
                       acctwidth: Int = 2,
                     amtwidth: Int = 12)(p: Posting): (List[String], Int, Int) = {
    val pacctstr: Posting => String = p => showAccountName(None, p.account)
    val pstatusprefix: Posting => String = p => p.status match {
      case Unmarked => ""
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

  def postingApplyCommodityStyles(styles: Map[CommoditySymbol, AmountStyle], posting: Posting): Posting =
    posting.copy(amount = styledMixedAmount(styles, posting.amount))
}
