package sledger.reports

import munit.FunSuite
import sledger.Queries.Query.And
import sledger.data.Amounts._
import sledger.data.Journals.{Journal, nulljournal, sampleJournal}
import sledger.reports.BalanceReports.{BalanceReport, balanceReport}
import sledger.reports.ReportOptions._

class BalanceReportsTest extends FunSuite {
  def g(specAndJournal: (Spec, Journal), r: BalanceReport): Unit = {
    specAndJournal match {
      case (spec, journal) =>
        val opts = spec.copy(query = And(List(queryFromFlags(spec.options), spec.query)))
        val (eitems, etotal) = r
        val (aitems, atotal) = balanceReport(opts, journal)

        def show[A, B, C](acct: A, acct1: B, indent: C, amt: MixedAmount): (A, B, C, String) =
          (acct, acct1, indent, showMixedAmountDebug(amt))

        assertEquals(aitems.map { case (a, b, c, d) => show(a, b, c, d) }, eitems.map { case (a, b, c, d) => show(a, b, c, d) })
        assertEquals(showMixedAmountDebug(atotal), showMixedAmountDebug(etotal))
    }
  }

  test("no args, null journal") {
    g((defaultSpec, nulljournal), (List.empty, nullMixedAmount))
  }

  test("no args, sample journal") {
    g((defaultSpec, sampleJournal), (
      List(
        ("assets:bank:checking","assets:bank:checking", 0, mixedAmount(usd(1))),
        ("assets:bank:saving","assets:bank:saving", 0, mixedAmount(usd(1))),
        ("assets:cash","assets:cash", 0, mixedAmount(usd(-2))),
        ("expenses:food","expenses:food", 0, mixedAmount(usd(1))),
        ("expenses:supplies","expenses:supplies", 0, mixedAmount(usd(1))),
        ("income:gifts","income:gifts", 0, mixedAmount(usd(-1))),
        ("income:salary","income:salary", 0, mixedAmount(usd(-1))),
      ),
      nullMixedAmount))
  }

  test("with --tree") {
    g((defaultSpec.copy(options = defaultsOptions.copy(accountListMode = ALTree)), sampleJournal), (
      List(
        ("assets", "assets", 0, mixedAmount(usd(0))),
        ("assets:bank", "bank", 1, mixedAmount(usd(2))),
        ("assets:bank:checking", "checking", 2, mixedAmount(usd(1))),
        ("assets:bank:saving", "saving", 2, mixedAmount(usd(1))),
        ("assets:cash", "cash", 1, mixedAmount(usd(-2))),
        ("expenses", "expenses", 0, mixedAmount(usd(2))),
        ("expenses:food", "food", 1, mixedAmount(usd(1))),
        ("expenses:supplies", "supplies", 1, mixedAmount(usd(1))),
        ("income", "income", 0, mixedAmount(usd(-2))),
        ("income:gifts", "gifts", 1, mixedAmount(usd(-1))),
        ("income:salary", "salary", 1, mixedAmount(usd(-1))),
      ),
      nullMixedAmount))
  }

  test("with --depth") {
    g((defaultSpec.copy(options = defaultsOptions.copy(depth = Some(1))), sampleJournal), (
      List(
        ("expenses", "expenses", 0, mixedAmount(usd(2))),
        ("income", "income", 0, mixedAmount(usd(-2))),
      ),
      mixedAmount(usd(0))))
  }
}
