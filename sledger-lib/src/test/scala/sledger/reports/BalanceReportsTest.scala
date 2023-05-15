package sledger.reports

import munit.FunSuite
import sledger.data.Accounts.{accountsFromPostings, showAccounts}
import sledger.data.Amounts.{missingamt, usd}
import sledger.data.Journals.sampleJournal
import sledger.data.Postings.post
import sledger.reports.BalanceReports.balanceReport
import sledger.reports.ReportOptions.defaultSpec

class BalanceReportsTest extends FunSuite {
  
  test("no args, sample journal") {
    val spec = defaultSpec
    val j = sampleJournal
    println("samlple journal", j)
    balanceReport(spec, j)
    println("=====")
    // todo proper test
  }
}
