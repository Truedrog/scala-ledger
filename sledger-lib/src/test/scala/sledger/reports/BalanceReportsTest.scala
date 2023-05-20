package sledger.reports

import cats.effect._
import munit.CatsEffectSuite
import sledger.Queries.Query.And
import sledger.data.Amounts.{MixedAmount, mixedAmount, nullMixedAmount, showMixedAmountDebug, usd}
import sledger.data.Journals.{Journal, nulljournal, sampleJournal}
import sledger.reports.BalanceReports.{BalanceReport, balanceReport}
import sledger.reports.ReportOptions.{Spec, defaultSpec}

class BalanceReportsTest extends CatsEffectSuite {
  def g(specAndJournal: (Spec, Journal), r: BalanceReport): IO[Unit] = {
    specAndJournal match {
      case (spec, journal) =>
        val opts = spec.copy(query = And(List(spec.query)))
        val (eitems, etotal) = r
        val (aitems, atotal) = balanceReport(opts, journal)

        def show[A, B, C](acct: A, acct1: B, indent: C, amt: MixedAmount): (A, B, C, String) =
          (acct, acct1, indent, showMixedAmountDebug(amt))

        assertIO_(IO.pure(aitems.map { case (a, b, c, d) => show(a, b, c, d) } == eitems.map { case (a, b, c, d) => show(a, b, c, d) }))
        assertIO_(IO.pure(showMixedAmountDebug(atotal) == showMixedAmountDebug(etotal)))
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
}
