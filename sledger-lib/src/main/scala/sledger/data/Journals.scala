package sledger.data

import cats._
import sledger.Queries.{Query, matchesPostingExtra}
import sledger.Unmarked
import sledger.data.AccountNames._
import sledger.data.Amounts._
import sledger.data.Balancing.{defBalancingOptions, journalBalanceTransactions}
import sledger.data.Dates._
import sledger.data.Postings._
import sledger.data.Transactions.{Transaction, transactionMapPostings, txnTieKnot}
import sledger.utils.RoseTree._

import java.time.{LocalDate, LocalDateTime, Year}

object Journals {

  case class Journal(
                      parseDefaultYear: Option[Year],
                      parseParentAccounts: List[AccountName],
                      parsedDefaultCommodity: Option[(CommoditySymbol, AmountStyle)],
                      files: List[(String, String)],
                      transactions: List[Transaction],
                      declaredAccountTypes: Map[AccountType, List[AccountName]],
                      inferredCommodities: Map[CommoditySymbol, AmountStyle], // commodities and formats inferred from journal amounts
                      lastReadTime: LocalDateTime,
                      accountTypes: Map[AccountName, AccountType],
                      //                      globalCommodityStyles: Map[CommoditySymbol, AmountStyle]
                    )

  object JournalOps {
    implicit class JournalExtensions(j: Journal) {
      //      def withGlobalCommodityStyles(styles: Map[CommoditySymbol, AmountStyle]): Journal =
      //        j.copy(globalCommodityStyles = styles)

      def withLastReadTime(t: LocalDateTime): Journal =
        j.copy(lastReadTime = t)

      def withFile(f: String, txt: String): Journal =
        j.copy(files = (f, txt) :: j.files)

      def reverse: Journal =
        j.copy(files = j.files.reverse, transactions = j.transactions.reverse)

      def withAccountTypes: Journal = j.copy(accountTypes = journalAccountTypes(j))

      def applyCommodityStyles: Either[CommoditySymbol, Journal] =
        journalApplyCommodityStyles(j)

      def addTransaction(transaction: Transaction): Journal = j.copy(transactions = transaction :: j.transactions)

      def numberTransactions: Journal = j.copy(transactions = j.transactions
        .zipWithIndex
        .map { case (t, i) => t.copy(index = i + 1) })

      def balanceTransactions: Either[String, Journal] = journalBalanceTransactions(defBalancingOptions, j)
    }
  }

  private def journalApplyCommodityStyles(journal: Journal): Either[String, Journal] = {

    journalInferCommodityStyles(journal).map(j => {
      val styles = journalCommodityStyles(j)
      journalMapPostings(p => postingApplyCommodityStyles(styles, p), j)
    })
  }

  def journalCommodityStyles(journal: Journal): Map[CommoditySymbol, AmountStyle] = {
    val inferred = journal.inferredCommodities
    inferred
  }

  def journalPostings(journal: Journal): List[Posting] = journal.transactions.flatMap(_.postings)

  def journalAccountNamesUsed(journal: Journal): List[AccountName] =
    accountNamesFromPostings(journalPostings(journal))

  private def journalAccountNames(journal: Journal): List[AccountName] =
    Foldable[List].foldMap(List(expandAccountNames(journalAccountNamesUsed(journal))))(a => a.distinct)

  def journalAccountType(journal: Journal, an: AccountName): Option[AccountType] =
    accountNameType(journalAccountTypes(journal), an)

  private def journalAccountTypes(journal: Journal): Map[AccountName, AccountType] = {
    def setTypes(mparenttype: Option[(AccountType, Boolean)], tree: Tree[AccountName]): Tree[(AccountName, Option[(AccountType, Boolean)])] = {
      tree match {
        case Node(a, subs) =>
          val declared = journalDeclaredAccountTypes(journal).view.mapValues((_, true)).toMap
          val minferred: Option[(AccountType, Boolean)] = mparenttype.filter(_._2) match {
            case Some(_) => mparenttype
            case None => accountNameInferType(a).map((_, false)).orElse(mparenttype)
          }
          val mtype = declared.get(a).orElse(minferred)
          val subTrees = Functor[List].map(subs)(t => setTypes(mtype, t))
          Node((a, mtype), subTrees)
      }
    }

    val t1 = setTypes(None, accountNameTreeFrom(journalAccountNames(journal)))
    flatten(t1).flatMap {
      case (a, mac) => mac match {
        case Some((accountType, _)) => List((a, accountType))
        case None => List()
      }
    }.toMap
  }

  private def journalDeclaredAccountTypes(journal: Journal): Map[AccountName, AccountType] = {
    (for {
      (accountType, accountNames) <- journal.declaredAccountTypes.toList
      accountName <- accountNames
    } yield accountName -> accountType).toMap
  }

  private def journalStyleInfluencingAmounts(journal: Journal): List[Amount] = {
    val l = journalPostings(journal).flatMap(p => amountsRaw(p.amount)).map(Some(_)).flatMap(_.toList)
//    println("journalStyleInfluencingAmounts", l)
    l
  }

  private def journalInferCommodityStyles(journal: Journal): Either[String, Journal] = {
    commodityStylesFromAmounts(journalStyleInfluencingAmounts(journal)) match {
      case Left(value) => Left(value)
      case Right(cs) => Right(journal.copy(inferredCommodities = {
//        println("journalInferCommodityStyles", cs) 
        cs
      }))
    }
  }

  private def commodityStylesFromAmounts(amounts: List[Amount]): Either[String, Map[CommoditySymbol, AmountStyle]] =
    Right(amounts.foldLeft(Map.empty[CommoditySymbol, AmountStyle]) { (acc, a) =>
      acc.updatedWith(a.commodity) {
        case Some(existing) => Some(canonicalStyle(existing, a.style))
        case None => Some(a.style)
      }
    })

  private def canonicalStyle(a: AmountStyle, b: AmountStyle): AmountStyle = {
    val prec = Order[AmountPrecision].max(a.precision, b.precision)
    val mgrps = a.digitgroups.orElse(b.digitgroups)
    val defdecmark = mgrps match {
      case Some(DigitGroupStyle('.', _)) => ','
      case _ => '.'
    }

    def chooseDecimalMark(mgrps: Option[DigitGroupStyle], asdecimalpoint: Option[Char], defdecmark: Char): Option[Char] = {
      mgrps match {
        case Some(DigitGroupStyle(mark, _)) if asdecimalpoint.contains(mark) => None
        case _ => asdecimalpoint.orElse(Some(defdecmark))
      }
    }

    val decmark = chooseDecimalMark(mgrps, a.decimalpoint.orElse(b.decimalpoint), defdecmark)
    a.copy(precision = prec, decimalpoint = decmark, digitgroups = mgrps)
  }

  def journalMapPostings(f: Posting => Posting, journal: Journal): Journal = {
    journal.copy(transactions = journal.transactions.map(t => transactionMapPostings(f, t)))
  }

  val nulljournal: Journal = Journal(
    parseDefaultYear = None,
    parsedDefaultCommodity = None,
    files = List.empty,
    transactions = List.empty,
    lastReadTime = LocalDateTime.MIN,
    parseParentAccounts = List.empty,
    declaredAccountTypes = Map.empty,
    accountTypes = Map.empty,
    inferredCommodities = Map.empty,
    //  globalCommodityStyles = Map.empty
    //  finalcommentlines = ""
  )

  def journalDateSpan(secondary: Boolean, journal: Journal): DateSpan = if (!secondary) {
    journalDateSpanHelper(Some(PrimaryDate), journal)
  } else {
    journalDateSpanHelper(Some(SecondaryDate), journal)
  }

  def journalDateSpanBoth(journal: Journal): DateSpan = journalDateSpanHelper(None, journal)

  def journalDateSpanHelper(mwd: Option[WhichDate], journal: Journal): DateSpan = {
    def getpdate(p: Posting) = mwd match {
      case Some(PrimaryDate) => p.date1.toList
      case Some(SecondaryDate) => (p.date2 orElse p.date1).toList
      case None => List(p.date1, p.date2).flatten
    }

    def gettdate(t: Transaction): List[LocalDate] = mwd match {
      case Some(PrimaryDate) => List(t.date)
      case Some(SecondaryDate) => List(t.date2.getOrElse(t.date))
      case None => List(t.date) ++ t.date2.toList
    }

    val txns = journal.transactions
    val tdates = txns.flatMap(gettdate)
    val pdates = txns.flatMap(_.postings).flatMap(getpdate)
    val dates = pdates ++ tdates
    DateSpan(dates.minOption.map(Exact), dates.maxOption.map(a => Exact(a.plusDays(1))))
  }

  def filterJournalPostings(query: Query, journal: Journal): Journal = {
//    println("reportq", query)
    journal.copy(transactions = journal.transactions
      .map(t => filterTransactionPostingsExtra(journalAccountType(journal, _), query, t)))
  }

  def filterTransactionPostingsExtra(atypes: AccountName => Option[AccountType],
                                     query: Query, transaction: Transaction): Transaction = {
    transaction.copy(
      postings = transaction.postings.filter(p => matchesPostingExtra(atypes, query, p))
    )
  }

  val sampleJournalIsExplicit: Boolean => Journal = explicit => nulljournal.copy(
    transactions = List(
      txnTieKnot {
        Transaction(
          index = 0,
          comment = "",
          sourcepos = nullsourcepos,
          date = LocalDate.of(2023, 1, 1),
          date2 = None,
          status = Unmarked,
          code = "",
          description = "income",
          precedingcomment = "",
          postings = List(
            post("assets:bank:checking", usd(1)),
            post("income:salary", if (explicit) usd(-1) else missingamt)
          )
        )
      },
      txnTieKnot {
        Transaction(
          index = 0,
          comment = "",
          sourcepos = nullsourcepos,
          date = LocalDate.of(2023, 6, 1),
          date2 = None,
          status = Unmarked,
          code = "",
          description = "gift",
          precedingcomment = "",
          postings = List(
            post("assets:bank:checking", usd(1)),
            post("income:gifts", if (explicit) usd(-1) else missingamt)
          )
        )
      },
      txnTieKnot {
        Transaction(
          index = 0,
          comment = "",
          sourcepos = nullsourcepos,
          date = LocalDate.of(2023, 6, 2),
          date2 = None,
          status = Unmarked,
          code = "",
          description = "save",
          precedingcomment = "",
          postings = List(
            post("assets:bank:saving", usd(1)),
            post("assets:bank:checking", if (explicit) usd(-1) else missingamt)
          )
        )
      },
      txnTieKnot {
        Transaction(
          index = 0,
          comment = "",
          sourcepos = nullsourcepos,
          date = LocalDate.of(2023, 6, 3),
          date2 = None,
          status = Unmarked,
          code = "",
          description = "eat & shop",
          precedingcomment = "",
          postings = List(
            post("expenses:food", usd(1)),
            post("expenses:supplies", usd(1)),
            post("assets:cash", if (explicit) usd(-2) else missingamt)
          )
        )
      },
    )
  )

  val sampleJournal: Journal = sampleJournalIsExplicit(true)

}