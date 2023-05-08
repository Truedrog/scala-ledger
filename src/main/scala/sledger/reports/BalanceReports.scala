package sledger.reports

import sledger.data.AccountNames.AccountName
import sledger.data.Amounts.MixedAmount
import sledger.data.Journals.Journal
import sledger.reports.ReportOptions._

object BalanceReports {

  type BalanceReport = (List[BalanceReportItem], MixedAmount)
  type BalanceReportItem = (AccountName, AccountName, Int, MixedAmount)


  type MultiBalanceReport = PeriodicReport[DisplayName,MixedAmount]
  type MultiBalanceReportRow = PeriodicReportRow[DisplayName, MixedAmount]
  
  def balanceReport(rspec: Spec, journal: Journal): BalanceReport = {
    val report = multiBalanceReport(rspec, journal)
    val rows = for {
      row <- report.rows
    } yield ((periodicReportRowFullName(row), periodicReportRowDisplayName(row), periodicReportRowDepth(row), row.total))
    val total = report.totals.total
    (rows, total)
  }
  
  def multiBalanceReport(rspec: Spec, journal: Journal): MultiBalanceReport = ???
}
