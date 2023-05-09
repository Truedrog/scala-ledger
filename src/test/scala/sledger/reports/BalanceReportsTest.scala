package sledger.reports

import munit.FunSuite
import sledger.data.Journals.sampleJournal
import sledger.reports.BalanceReports.balanceReport
import sledger.reports.ReportOptions.{defaultSpec, reportSpan}

class BalanceReportsTest extends FunSuite {
  
  test("no args, sample journal") {
    val spec = defaultSpec
    val j = sampleJournal
    balanceReport(spec, j)
    println("=====")
    
  }
}
