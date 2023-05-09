package sledger.reports

import sledger.Queries.Query.{And, Date, Date2}
import sledger.Queries.{filterQuery, queryIsDateOrDate2, simplifyQuery}
import sledger.data.AccountNames.AccountName
import sledger.data.Amounts.MixedAmount
import sledger.data.Dates.{DateSpan, groupByDateSpan, nulldatespan}
import sledger.data.Journals.{Journal, filterJournalPostings, filterTransactionPostingsExtra, journalPostings}
import sledger.data.Postings.{Posting, postingDateOrDate2}
import sledger.reports.ReportOptions._

object BalanceReports {

  type BalanceReport = (List[BalanceReportItem], MixedAmount)
  type BalanceReportItem = (AccountName, AccountName, Int, MixedAmount)


  type MultiBalanceReport = PeriodicReport[DisplayName,MixedAmount]
  type MultiBalanceReportRow = PeriodicReportRow[DisplayName, MixedAmount]
  
  def balanceReport(spec: Spec, journal: Journal): BalanceReport = {
    val report = multiBalanceReport(spec, journal)
    val rows = for {
      row <- report.rows
    } yield ((periodicReportRowFullName(row), periodicReportRowDisplayName(row), periodicReportRowDepth(row), row.total))
    val total = report.totals.total
    (rows, total)
  }
  
  def multiBalanceReport(spec: Spec, journal: Journal): MultiBalanceReport = {
    
    val (reportspan, colspans) = reportSpan(journal, spec)
    val spec1 =  makeReportQuery(spec, reportspan)
    println("reportopts", spec1)
    val colps = getPostingsByColumn(spec1, journal, colspans)
    println("colps", colps)
    
    ???
  }
  
  def getPostingsByColumn(spec: Spec, journal: Journal, colspans: List[DateSpan]): List[(DateSpan, List[Posting])] = {
    val getDate = postingDateOrDate2(whichDate(spec.options), _)
    val ps = getPostings(spec, journal)
    println("getPostingsByColumn", ps)
    groupByDateSpan(true, getDate, colspans, ps)
  }
  
  def getPostings(spec: Spec, journal: Journal): List[Posting] = {
    val filtered = filterJournalPostings(spec.query, journal)
    journalPostings(filtered)
  }
  def makeReportQuery(spec: Spec, reportSpan: DateSpan): Spec = {
    if(reportSpan == nulldatespan) {
      spec
    } else {
      val dateless = filterQuery(!queryIsDateOrDate2(_), _)
      val dateqcons = if(spec.options.date2) Date2 else Date
      val reportspandatesq = dateqcons(reportSpan)
      val query = simplifyQuery(And(List(dateless(spec.query), reportspandatesq)))  
      spec.copy(query = query)
    }
  }
}
