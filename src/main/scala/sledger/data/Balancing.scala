package sledger.data

import cats._
import cats.data.Reader
import cats.syntax.all._
import sledger.data.AccountNames.AccountName
import sledger.data.Amounts.{AmountStyle, CommoditySymbol, MixedAmount, canonicaliseMixedAmount, isMissingMixedAmount, isNegativeAmount, isNegativeMixedAmount, maNegate, mixedAmountCost, mixedAmountLooksZero, showMixedAmountOneLineWithoutPrice, styleMixedAmount}
import sledger.data.Journals.{Journal, journalCommodityStyles}
import sledger.data.Journals.JournalOps._
import sledger.data.Transactions.{Transaction, txnTieKnot}
import sledger.data.Postings.PostinngOps._
import sledger.data.Postings.{Posting, originalPosting, posting, sumPostings}

import scala.collection.mutable.ListBuffer

object Balancing {
  case class BalancingOptions(commodityStyles: Option[Map[CommoditySymbol, AmountStyle]])

  val defBalancingOptions = BalancingOptions(commodityStyles = None)

  def journalBalanceTransactions(balancingOptions: BalancingOptions, journal: Journal): Either[String, Journal] = {
    val ts = journal.numberTransactions.transactions
    val styles = Some(journalCommodityStyles(journal))
    val bopts = balancingOptions.copy(commodityStyles = styles)
    var balancedtxn: ListBuffer[Transaction] = ListBuffer[Transaction](ts)

    ???
  }

  def balanceTransaction(balancingOptions: BalancingOptions, transaction: Transaction): Either[String, Transaction] = {
    balanceTransactionHelper(balancingOptions, transaction).map(_._1)
  }

  def balanceTransactionHelper(balancingOptions: BalancingOptions,
                               transaction: Transaction): Either[String, (Transaction, List[(AccountName, MixedAmount)])] = {
    for {
      (t, inferredamtsandaccts) <- transactionInferBalancingAmount(balancingOptions.commodityStyles.getOrElse(Map.empty), transaction)
      result <- transactionCheckBalanced(balancingOptions, t) match {
        case Nil => Right((txnTieKnot(t), inferredamtsandaccts))
        case errs =>
          Left(transactionBalanceError(errs ++ List("Consider adjusting this entry's amounts, or adding missing postings.")))
      }
    } yield result
  }

  def transactionInferBalancingAmount(styles: Map[CommoditySymbol, AmountStyle],
                                      transaction: Transaction): Either[String, (Transaction, List[(AccountName, MixedAmount)])] = {
    val (amountfullPostings, amountlessPostings) = transaction.postings.partition(_.hasAmount)
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
      Left(transactionBalanceError(List("This transaction is unbalanced. There can't be more than one real posting with no amount.")))
    } else {
      val postingsAndInferredAmounts = transaction.postings.map(inferamount)
      val infereredAccountsAndAmounts = for {
        (p, Some(amt)) <- postingsAndInferredAmounts
      } yield (p.account, amt)
      Right((transaction.copy(postings = postingsAndInferredAmounts.map(_._1)), infereredAccountsAndAmounts))
    }
  }

  def transactionCheckBalanced(balancingOptions: BalancingOptions, t: Transaction): List[String] = {
    val (rps, _) = t.postings.foldRight((List.empty[Posting], List.empty[Posting])) { case (p, (l, r)) => (p +: l, r) }
    val canonicalise = balancingOptions.commodityStyles.fold(identity[MixedAmount])(s => canonicaliseMixedAmount(s, _))
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
    else s"The real postings' sum should be 0 but is: " + ${
      showMixedAmountOneLineWithoutPrice(false, rsumcost)
    }
    List(errs).filter(_.nonEmpty)
  }

  def transactionBalanceError(errs: List[String]): String = {
    val s = errs.mkString
    f"THis transaction is unbalanced ${s}%s"
  }

}