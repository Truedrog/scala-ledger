package sledger.data

import cats._
import sledger.data.AccountNames._
import sledger.data.Amounts._
import sledger.data.Postings.{Posting, accountNamesFromPostings, postingApplyCommodityStyles}
import sledger.data.Transactions.{Transaction, transactionMapPostings}
import sledger.utils.RoseTree._

import java.time.{LocalDateTime, Year}

object Journals {

  case class Journal(
                      parseDefaultYear: Option[Year],
                      parseParentAccounts: List[AccountName],
                      parsedDefaultCommodity: Option[(CommoditySymbol, AmountStyle)],
                      files: List[(String, String)],
                      transactions: List[Transaction],
                      declaredAccountTypes: Map[AccountType, List[AccountName]],
                      inferredCommodities: Map[CommoditySymbol, AmountStyle], //    -- ^ commodities and formats inferred from journal amounts
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
        .map{case (t, i) => t.copy(index = i+1)}) 
    }
  }

  private def journalApplyCommodityStyles(journal: Journal): Either[String, Journal] = {

    def fixjournal(journal: Journal): Journal = {
      val styles = journalCommodityStyles(journal)
      journalMapPostings(p => postingApplyCommodityStyles(styles, p), journal)
    }
    journalInferCommodityStyles(journal).map(fixjournal)
  }
  
    def journalCommodityStyles(journal: Journal): Map[CommoditySymbol, AmountStyle] = {     
//    val globals = journal.globalCommodityStyles
    val inferred = journal.inferredCommodities
//    globals ++ inferred
    inferred
  }
  
  private def journalPostings(journal: Journal): List[Posting] = journal.transactions.flatMap(_.postings)

  def journalAccountNamesUsed(journal: Journal): List[AccountName] =
    accountNamesFromPostings(journalPostings(journal))

  private def journalAccountNames(journal: Journal): List[AccountName] =
    Foldable[List].foldMap(List(expandAccountNames(journalAccountNamesUsed(journal))))(a => a.distinct)

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
    (for {(a, Some((acctType, _))) <- flatten(t1)} yield (a, acctType)).toMap
  }

  private def journalDeclaredAccountTypes(journal: Journal): Map[AccountName, AccountType] = {
    (for {
      (accountType, accountNames) <- journal.declaredAccountTypes.toList
      accountName <- accountNames
    } yield accountName -> accountType).toMap
  }

  private def journalStyleInfluencingAmounts(journal: Journal): List[Amount] = {
    val l = journalPostings(journal).flatMap(p => amountsRaw(p.amount)).map(Some(_)).flatMap(_.toList)
    println("journalStyleInfluencingAmounts", l)
    l
  }

  private def journalInferCommodityStyles(journal: Journal): Either[String, Journal] = {
    commodityStylesFromAmounts(journalStyleInfluencingAmounts(journal)) match {
      case Left(value) => Left(value)
      case Right(cs) => Right(journal.copy(inferredCommodities = {
        println("journalInferCommodityStyles") 
        cs
      } ))
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
//    globalCommodityStyles = Map.empty
    //    finalcommentlines = ""
  )
}