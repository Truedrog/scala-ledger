package sledger.reports

import cats.syntax.all._
import sledger.Queries.Query.{And, Date, Date2}
import sledger.Queries._
import sledger.data.AccountNames._
import sledger.data.Accounts.FastTree.accountTree
import sledger.data.Accounts._
import sledger.data.Amounts.{MixedAmount, averageMixedAmounts, maSum, mixedAmountLooksZero}
import sledger.data.Dates._
import sledger.data.Journals.{Journal, filterJournalPostings, journalPostings}
import sledger.data.Period.{dateSpanAsPeriod, periodAsDateSpan}
import sledger.data.Postings.{Posting, postingDateOrDate2}
import sledger.reports.ReportOptions._
import sledger.{NormallyNegative, NormallyPositive}

import scala.collection.immutable.HashMap

object BalanceReports {

  type BalanceReport = (List[BalanceReportItem], MixedAmount)
  type BalanceReportItem = (AccountName, AccountName, Int, MixedAmount)

  type MultiBalanceReport = PeriodicReport[DisplayName, MixedAmount]
  type MultiBalanceReportRow = PeriodicReportRow[DisplayName, MixedAmount]
  type ClippedAccountName = AccountName

  def balanceReport(spec: Spec, journal: Journal): BalanceReport = {
    val report = multiBalanceReport(spec, journal)
    val rows = for {
      row <- report.rows
    } yield ((periodicReportRowFullName(row), periodicReportRowDisplayName(row), periodicReportRowDepth(row), row.total))
    val total = report.totals.total
    (rows, total)
  }

  def multiBalanceReport(spec1: Spec, journal: Journal): MultiBalanceReport = {

    val (reportspan, colspans) = reportSpan(journal, spec1)
    val spec = makeReportQuery(spec1, reportspan)
//    println("reportopts", spec)

    val colps = getPostingsByColumn(spec, journal, colspans)
    println("colps", colps)

    val startbals = startingBalances(spec, journal, startingPostings(spec, journal, reportspan))
    println("startbals", startbals)

    val report = generateMultiBalanceReport(spec, journal, Set.empty, colps, startbals)
//    println("multiBalanceReportWith", report)
    report
  }
  def generateMultiBalanceReport(spec: Spec,
                                 journal: Journal,
                                 unelidableaccts: Set[AccountName],
                                 colps: List[(DateSpan, List[Posting])],
                                 startbals: HashMap[AccountName, Account]): MultiBalanceReport = {
    val matrix = calculateReportMatrix(spec, journal, startbals, colps)
    val displayNames = displayedAccounts(spec, unelidableaccts, matrix)
    println("displayNames", displayNames)
    val rows = buildReportRow(spec.options, displayNames, matrix)
//    println("rows", rows)

    val totalsrow = calculateTotalRows(spec.options, rows)
//    println("totalsrow", totalsrow)

    val sortedRows = sortRows(spec.options, journal, rows)
//    println("sortedRows", sortedRows)

    reportPercent(spec.options, PeriodicReport(colps.map(_._1), sortedRows, totalsrow))
  }

  def sortRows(options: Options, journal: Journal, rows: List[MultiBalanceReportRow]): List[MultiBalanceReportRow] = {
    def sortTreeMBRByAmount(rows: List[MultiBalanceReportRow]): List[MultiBalanceReportRow] = {
      val tree = accountTree("root", rows.map(periodicReportRowFullName))
      val rowMap = HashMap.from(rows.map(row => periodicReportRowFullName(row) -> row))
      val setibalance: Account => Account = a => {
        a.copy(ibalance = rowMap.get(a.name).fold(maSum(a.subs.map(_.ibalance)))(_.total))
      }
      val accounttreewithbals = mapAccounts(setibalance)(tree)
      val sortedaccounttree = sortAccountTreeByAmount(options.normalbalance.getOrElse(NormallyPositive), accounttreewithbals)
      val sortedanames = flattenAccounts(sortedaccounttree).drop(1).map(_.name)
      sortedanames.flatMap(a => rowMap.get(a))
    }

    def sortFlatMBRByAmount(rows: List[MultiBalanceReportRow]): List[MultiBalanceReportRow] = {
      options.normalbalance.getOrElse(NormallyPositive) match {
        case NormallyPositive => rows.sortWith((a, b) => (a.total, periodicReportRowFullName(a)) > (b.total, periodicReportRowFullName(a)))
        case NormallyNegative => rows.sortWith((a, b) => (a.total, periodicReportRowFullName(a)) < (b.total, periodicReportRowFullName(a)))
      }
    }

    def sortMBRByAccountDeclaration(rows: List[MultiBalanceReportRow]): List[MultiBalanceReportRow] = {
      val isTree = options.accountListMode match {
        case ReportOptions.ALFlat => false
        case ReportOptions.ALTree => true
      }
      val sortedanames = sortAccountNamesByDeclaration(journal, isTree, rows.map(a => periodicReportRowFullName(a)))
      sortRowsLike(sortedanames, rows)
    }

    (options.sortAmount, options.accountListMode) match {
      case (true, ALTree) => sortTreeMBRByAmount(rows)
      case (true, ALFlat) => sortFlatMBRByAmount(rows)
      case _ => sortMBRByAccountDeclaration(rows)
    }
  }

  def reportPercent(options: Options, report: MultiBalanceReport): MultiBalanceReport = {
    report //todo percentages
  }

  def sortRowsLike[A](sortedas: List[AccountName],
                      rows: List[PeriodicReportRow[DisplayName, A]]): List[PeriodicReportRow[DisplayName, A]] = {
    val rowMap = HashMap.from(rows.map(row => periodicReportRowFullName(row) -> row))
    sortedas.flatMap(a => rowMap.get(a))
  }

  def calculateTotalRows(options: Options, rows: List[MultiBalanceReportRow]): PeriodicReportRow[Unit, MixedAmount] = {
    val flat = options.accountListMode match {
      case ReportOptions.ALFlat => true
      case ReportOptions.ALTree => false
    }

    def isTopRow[A](row: PeriodicReportRow[DisplayName, A]): Boolean = {
      val parents = expandAccountName(periodicReportRowFullName(row)).init
      val rowMap = HashMap.from(rows.map(row => periodicReportRowFullName(row) -> row))
      flat || parents.exists(a => rowMap.contains(a))
    }

    val colamts = rows.filter(isTopRow).map(_.amounts).transpose
    val coltotals = colamts.map(maSum)
//    println("coltotals", coltotals)

    val grandTotal = options.balanceAccum match {
      case ReportOptions.PerPeriod => maSum(coltotals)
    }

    val grandaverage = averageMixedAmounts(coltotals)

    PeriodicReportRow((), coltotals, grandTotal, grandaverage)
  }

  def buildReportRow(opts: Options,
                     displayNames: HashMap[AccountName, DisplayName],
                     matrix: HashMap[AccountName, Map[DateSpan, Account]]): List[MultiBalanceReportRow] = {
    val balance: Account => MixedAmount = a => opts.accountListMode match {
      case ReportOptions.ALFlat => a.ebalance
      case ReportOptions.ALTree => a.ibalance
    }

    def mkRow(name: AccountName, accounts: List[Account]): Option[PeriodicReportRow[DisplayName, MixedAmount]] = {
      val rowbals = accounts.map(balance)
      val rowtot = opts.balanceAccum match {
        case ReportOptions.PerPeriod => maSum(rowbals)
      }
      val rowavg = averageMixedAmounts(rowbals)
      for {
        displayname <- displayNames.get(name)
      } yield PeriodicReportRow(displayname, rowbals, rowtot, rowavg)
    }

    matrix.map { case (k, v) => mkRow(k, v.values.toList) }.toList.flatten
  }

  def getPostingsByColumn(spec: Spec, journal: Journal, colspans: List[DateSpan]): List[(DateSpan, List[Posting])] = {
    val getDate = postingDateOrDate2(whichDate(spec.options), _)
    val ps = getPostings(spec, journal)
    println("getPostingsByColumn", ps)
    groupByDateSpan(true, getDate, colspans, ps)
  }

  def getPostings(spec: Spec, journal: Journal): List[Posting] = {
    val query = filterQuery(!queryIsDepth(_), spec.query)
    println("depthless", query)
    val filtered = filterJournalPostings(query, journal)
    journalPostings(filtered)
  }

  def startingBalances(spec: Spec, journal: Journal, ps: List[Posting]): HashMap[AccountName, Account] = {
    calculateReportMatrix(spec, journal, HashMap.empty, List((emptyDateSpan, ps))).map {
      case (a, m) => a -> m.getOrElse(emptyDateSpan, nullact)
    }
  }

  def startingPostings(spec: Spec, journal: Journal, reportspan: DateSpan): List[Posting] = {

    val datelessq = filterQuery(!queryIsDateOrDate2(_), spec.query)
    println("datelessq", datelessq)
    val precedingspan = DateSpan(None, spanStart(reportspan).map(Exact))
    val precedingspanq = (if (spec.options.date2) Date2 else Date) {
      precedingspan match {
        case DateSpan(None, None) => emptyDateSpan
        case a => a
      }
    }
    val startbalq = And(List(datelessq, precedingspanq))
    println("startbalq", startbalq)

    val precedingperiod = dateSpanAsPeriod(spanIntersect(precedingspan, periodAsDateSpan(spec.options.period))) 
    //todo valuation?
    val spec1 = spec.copy(query = startbalq, options = spec.options.copy(period = precedingperiod))
    getPostings(spec1, journal)
  }

  def makeReportQuery(spec: Spec, reportSpan: DateSpan): Spec = {
    if (reportSpan == nulldatespan) {
      spec
    } else {
      val dateless = filterQuery(!queryIsDateOrDate2(_), _)
      val dateqcons = if (spec.options.date2) Date2 else Date
      val reportspandatesq = dateqcons(reportSpan)
      val query = simplifyQuery(And(List(dateless(spec.query), reportspandatesq)))
      spec.copy(query = query)
    }
  }

  def acctChanges(spec: Spec, journal: Journal, ps: List[Posting]): HashMap[ClippedAccountName, Account] = {
    def filterbydepth(accts: List[Account]): List[Account] = {
      val depthq = filterQuery(queryIsDepth, spec.query)
      println("depthq", depthq)
      spec.options.accountListMode match {
        case ALTree => accts.filter(a => matchesAccount(depthq, a.name))
        case ALFlat => clipAccountsAndAggregate(queryDepth(depthq), accts).filter(a => 0 < a.numposting)
      }
    }

    val accts = filterbydepth(accountsFromPostings(ps).drop(1))
    val xs = for {
      a <- accts
    } yield (a.name, a)
    HashMap.from(xs)
  }

  def subaccountTallies(accountNames: List[AccountName]): HashMap[AccountName, Int] = {
    def incrementParent(a: AccountName, map: HashMap[AccountName, Int]): HashMap[AccountName, Int] = {
      val parent = parentAccountName(a)
      map.updated(parent, map.getOrElse(parent, 0) + 1)
    }

    val expandedAccountNames = expandAccountNames(accountNames)
    val talliesMap = expandedAccountNames.foldRight(HashMap.empty[AccountName, Int])(incrementParent)

    talliesMap
  }

  def displayedAccounts(spec: Spec, unelidableaccts: Set[AccountName],
                        valuedaccts: HashMap[AccountName, Map[DateSpan, Account]]): HashMap[AccountName, DisplayName] = {
    val qdepth = queryDepth(spec.query).getOrElse(Int.MaxValue)

    def isInteresting(name: String, amts: List[Account]): Boolean = {
      val d = accountNameLevel(name)
      val keepWhenEmpty: List[Account] => Boolean = amts => spec.options.accountListMode match {
        case ReportOptions.ALFlat => Function.const(true)(amts)
        case ReportOptions.ALTree => amts.forall(_.subs.nonEmpty)
      }
      val balance: Account => MixedAmount = a => spec.options.accountListMode match {
        case ReportOptions.ALTree if (d == qdepth) => a.ibalance
        case _ => a.ebalance
      }

      def isZeroRow[A](f: A => MixedAmount, xs: List[A]): Boolean = {
        xs.forall(a => mixedAmountLooksZero(f(a)))
      }

      d <= qdepth && unelidableaccts(name) || keepWhenEmpty(amts) || isZeroRow(balance, amts)
    }

    val numbSubs = subaccountTallies(valuedaccts.filter { case (k, v) =>
      isInteresting(k, v.values.toList)
    }.keys.toList)

    def isInterestingParents: HashMap[AccountName, Int] = {
      def hasEnoughSubs(name: AccountName, nsubs: Int): Boolean = {
        val minsubs = if (spec.options.noElide) 1 else 2
        nsubs >= minsubs && accountNameLevel(name) > spec.options.drop
      }

      spec.options.accountListMode match {
        case ReportOptions.ALTree => numbSubs.filter { case (k, v) => hasEnoughSubs(k, v) }
        case ReportOptions.ALFlat => HashMap.empty
      }
    }

    val displayedAccts = if (qdepth == 0) valuedaccts else valuedaccts.filter { case (k, v) =>
      isInteresting(k, v.values.toList) || isInterestingParents.contains(k)
    }

    def displayedName(name: String): DisplayName = {
      val notDisplayed = !displayedAccts.contains(_:AccountName)
      val level = 0.max(accountNameLevel(name) - spec.options.drop)
      val parents = parentAccountNames(name).take(level - 1)
      val boringParents = if (spec.options.noElide) 0 else {
        parents.count(notDisplayed)
      }
      val droppedName = accountNameDrop(spec.options.drop, name)
      val leaf = accountNameFromComponents(droppedName +: parents.takeWhile(notDisplayed).map(accountLeafName).reverse)
      spec.options.accountListMode match {
        case ReportOptions.ALTree => DisplayName(name, leaf, 0.max(level - boringParents))
        case ReportOptions.ALFlat => DisplayName(name, droppedName, 1)
      }
    }

    if (qdepth == 0) HashMap("..." -> DisplayName("...", "...", 1))
    else displayedAccts.map { case (k, _) => k -> displayedName(k) }
  }

  def transposeMap[A](rows: List[(DateSpan, HashMap[AccountName, A])]): HashMap[AccountName, Map[DateSpan, A]] = {
    rows.foldLeft(HashMap.empty[AccountName, Map[DateSpan, A]]) { (acc, row) =>
      val (span, acctmap) = row
      acctmap.foldLeft(acc) { (subAcc, kv) =>
        val (acct, a) = kv
        val newMap = subAcc.getOrElse(acct, Map.empty[DateSpan, A]) + (span -> a)
        subAcc + (acct -> newMap)
      }
    }
  }

  def calculateReportMatrix(spec: Spec,
                            journal: Journal,
                            startbals: HashMap[ClippedAccountName, Account],
                            colps: List[(DateSpan, List[Posting])]): HashMap[ClippedAccountName, Map[DateSpan, Account]] = {
    val colspans = colps.map(_._1)
    val zeros = Map.from(for {
      spn <- colspans
    } yield (spn, nullact))
    println("zeros", zeros)
    println("colps", colps)
    val acctApplyBoth: (MixedAmount => MixedAmount) => Account => Account = f => a =>
      a.copy(ibalance = f(a.ibalance), ebalance = f(a.ebalance))
    val avalue = acctApplyBoth.compose(Function.const(identity[MixedAmount]))
    val colacctchanges: List[(DateSpan, HashMap[ClippedAccountName, Account])] =
      colps.map { c => c.bimap(x => identity(x), xs => acctChanges(spec, journal, xs)) }
    println("colacctchanges", colacctchanges)
    val acctchanges: HashMap[AccountName, Map[DateSpan, Account]] = transposeMap(colacctchanges)
    println("acctchanges", acctchanges)
    val map1 = HashMap.from(acctchanges.view.mapValues(a => a ++ zeros).toList)
    val map2 = HashMap.from(startbals.view.mapValues(_ => zeros).toList)
    val allchanges = map1 ++ map2

    def rowbals(name: ClippedAccountName, unvaluedChanges: Map[DateSpan, Account]): Map[DateSpan, Account] = {
      val rowbals = spec.options.balanceAccum match {
        case ReportOptions.PerPeriod => unvaluedChanges.map { case (d, a) => d -> avalue.apply(d).apply(a) }
      }
      println("rowbals", rowbals)
      rowbals
    }

    allchanges.map { case (aname, changes) => aname -> rowbals(aname, changes) }
  }
}
