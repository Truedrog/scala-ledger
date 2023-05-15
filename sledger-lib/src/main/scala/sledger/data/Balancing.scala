package sledger.data

import cats._
import cats.syntax.all._
import sledger.data.AccountNames.AccountName
import sledger.data.Amounts._
import sledger.data.Journals.JournalOps._
import sledger.data.Journals.{Journal, journalCommodityStyles}
import sledger.data.Postings.PostinngOps._
import sledger.data.Postings.{Posting, originalPosting, sumPostings}
import sledger.data.Transactions.{Transaction, showTransaction, txnTieKnot}
import sledger.utils.Parse.sourcePosPretty

object Balancing {
  case class BalancingOptions(commodityStyles: Option[Map[CommoditySymbol, AmountStyle]])

  val defBalancingOptions: BalancingOptions = BalancingOptions(commodityStyles = None)//todo remove

  def journalBalanceTransactions(balancingOptions: BalancingOptions, journal: Journal): Either[String, Journal] = {
    val ts = journal.numberTransactions.transactions
    val styles = Some(journalCommodityStyles(journal))
    val bopts = balancingOptions.copy(commodityStyles = styles)
    for {
      bts <- ts.traverse(t => balanceTransaction(bopts, t))
    } yield journal.copy(transactions = bts.sortBy(_.date))
  }

  def balanceTransaction(balancingOptions: BalancingOptions, transaction: Transaction): Either[String, Transaction] = {
    balanceTransactionHelper(balancingOptions, transaction).map(_._1)
  }

  def balanceTransactionHelper(balancingOptions: BalancingOptions,
                               t: Transaction): Either[String, (Transaction, List[(AccountName, MixedAmount)])] = {
    for {
      (t, inferredamtsandaccts) <- transactionInferBalancingAmount(balancingOptions.commodityStyles.getOrElse(Map.empty), t)
      result <- transactionCheckBalanced(balancingOptions, t) match {
        case Nil => Right((txnTieKnot(t), inferredamtsandaccts))
        case errs =>
          Left(transactionBalanceError(t, errs ++ List("Consider adjusting this entry's amounts, or adding missing postings.")))
      }
    } yield result
  }

  def transactionInferBalancingAmount(styles: Map[CommoditySymbol, AmountStyle],
                                      t: Transaction): Either[String, (Transaction, List[(AccountName, MixedAmount)])] = {
    val (amountfullPostings, amountlessPostings) = t.postings.partition(_.hasAmount)
    val realSum = sumPostings(amountfullPostings)

    def inferamount(p: Posting): (Posting, Option[MixedAmount]) = {
      val minferredamt = if (!p.hasAmount) Some(realSum) else None
      minferredamt match {
        case None => (p, None)
        case Some(a) =>
          val amt = styleMixedAmount(styles, maNegate(a))
          (p.copy(amount = amt, original = Some(originalPosting(p))), Some(amt))
      }
    }

    if (amountlessPostings.length > 1) {
      Left(transactionBalanceError(t, List("There can't be more than one real posting with no amount.")))
    } else {
      val postingsAndInferredAmounts = t.postings.map(inferamount)
      val infereredAccountsAndAmounts = postingsAndInferredAmounts
        .foldLeft(List.empty[(AccountName, MixedAmount)]) { case (acc, (p, mb)) =>
          mb match {
            case Some(amt) => (p.account, amt) +: acc
            case None => acc
          }
        }
      Right((t.copy(postings = postingsAndInferredAmounts.map(_._1)), infereredAccountsAndAmounts))
    }
  }

  def transactionCheckBalanced(balancingOptions: BalancingOptions, t: Transaction): List[String] = {
    val (rps, _) = t.postings.foldRight((List.empty[Posting], List.empty[Posting])) { case (p, (l, r)) => (p +: l, r) }
    val canonicalise = balancingOptions.commodityStyles.fold(identity[MixedAmount] _)(s => canonicaliseMixedAmount(s, _))
    val postingBalancingAmount = (p: Posting) => p.amount // todo add prices later?
    val singsOK = (ps: List[Posting]) => {
      val amounts = ps.map(p => canonicalise(p.amount))
      val nonzeros = amounts.filter(ma => !mixedAmountLooksZero(ma))
      val hasNonzeros = nonzeros.length >= 2
      val signsConsistent = if (hasNonzeros) {
        nonzeros.flatMap(isNegativeMixedAmount).distinct.sorted.length > 1
      } else true
      !hasNonzeros || signsConsistent
    }
    val rsignsok = singsOK(rps)
    val rsumcost = Foldable[List].foldMap(rps)(postingBalancingAmount)
    val rsumdisplay = canonicalise(rsumcost)
    val rsumok = mixedAmountLooksZero(rsumdisplay)
    val errs = if (rsumok) "" else if (!rsignsok) "The real postings all have the same sign. Consider negating some of them."
    else s"The real postings sum should be 0 but is: ${showMixedAmountOneLineWithoutPrice(false, rsumcost)}"
    List(errs).filter(_.nonEmpty)
  }

  def transactionBalanceError(t: Transaction, errs: List[String]): String = {
    val s = errs.mkString("", "\n", "\n")
    val (_, _, ex) = makeTransactionErrorExcerpt(t, _ => None)
    f"${sourcePosPretty(t.sourcepos)}%s\n${ex}%s\n\nThis transaction is unbalanced.\n${s}%s"
  }
  
  def makeTransactionErrorExcerpt(t: Transaction, f: Transaction => Option[(Int, Option[Int])]): (Int, Option[(Int, Option[Int])], String) = {
    val (tpos, _) = t.sourcepos._1
    val txntxt = showTransaction(t).stripMargin + "\n"
    val merrcols = f(t)
    val ex = decorateError(tpos, merrcols, txntxt)
    (tpos, merrcols, ex)
  }

  def decorateError(l: Int, mcols: Option[(Int, Option[Int])], txt: String): String = {
    val (ls, ms) = txt.split('\n').toList.splitAt(1)
    val lsPrime = ls map (line => s"$l | $line")
    val lineprefix = " " * (l.toString.length + 1) + "| "
    val colmarkerline = mcols.toList.flatMap {
      case (col, Some(endCol)) =>
        val regionw = endCol - col + 1
        List(s"${lineprefix}${" " * (col - 1)}${"^" * regionw}")
      case (col, None) =>
        List(s"${lineprefix}${" " * (col - 1)}^")
    }
    (lsPrime ++ colmarkerline ++ ms.map(lineprefix + _)).mkString("\n")
  }

  def isTransactionBalanced(bopts: BalancingOptions, transaction: Transaction): Boolean =
    transactionCheckBalanced(bopts, transaction).isEmpty
}
