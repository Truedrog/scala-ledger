package sledger
import cats._
import sledger.data.AccountNames.{AccountName, accountLeafName, accountNameLevel}
import sledger.data.Dates.{DateSpan, Exact, spanEnd, spanStart}

package object reports {

  case class PeriodicReport[A, B](
                                   dates: List[DateSpan],
                                   rows: List[PeriodicReportRow[A, B]],
                                   totals: PeriodicReportRow[Unit, B]
                                 )

  case class PeriodicReportRow[+A, +B](
                                      name: A,
                                      amounts: List[B],
                                      total: B,
                                      average: B
                                    )

  implicit def periodicReportRowFunctor[A]: Functor[PeriodicReportRow[A, *]] = new Functor[PeriodicReportRow[A, *]] {
    def map[B, C](fa: PeriodicReportRow[A, B])(f: B => C): PeriodicReportRow[A, C] =
      fa.copy(amounts = fa.amounts.map(f), total = f(fa.total), average = f(fa.average))
  }

  implicit def periodicReportRowBifunctor: Bifunctor[PeriodicReportRow] =
    new Bifunctor[PeriodicReportRow] {
      def bimap[A, B, C, D](fab: PeriodicReportRow[A, B])(f: A => C, g: B => D): PeriodicReportRow[C, D] =
        fab.copy(
          name = f(fab.name),
          amounts = fab.amounts.map(g),
          total = g(fab.total),
          average = g(fab.average)
        )
    }

  implicit def periodicReportFunctor[B]: Functor[PeriodicReport[*, B]] = new Functor[PeriodicReport[*, B]] {
    def map[A, C](fa: PeriodicReport[A, B])(f: A => C): PeriodicReport[C, B] =
      fa.copy(
        rows = fa.rows.map(prRow => prRow.copy(name = f(prRow.name))),
        totals = fa.totals.copy(name = ())
      )
  }

  implicit def periodicReportRowSemigroup[A, B: Semigroup]: Semigroup[PeriodicReportRow[A, B]] =
    (row1, row2) => prrAdd(row1, row2)
  
  def prrAdd[A, B: Semigroup](row1: PeriodicReportRow[A, B], row2: PeriodicReportRow[A, B]): PeriodicReportRow[A, B] = {
    val PeriodicReportRow(n1, amts1, t1, a1) = row1
    val PeriodicReportRow(_, amts2, t2, a2) = row2
    val amts = zipWithPadded[B]((b1, b2) => Semigroup[B].combine(b1, b2))(amts1, amts2)
    PeriodicReportRow(n1, amts, Semigroup[B].combine(t1, t2), Semigroup[B].combine(a1, a2))
  }
  
  def zipWithPadded[A](f: (A, A) => A)(as: List[A], bs: List[A]): List[A] = (as, bs) match {
    case (a :: asTail, b :: bsTail) => f(a, b) :: zipWithPadded(f)(asTail, bsTail)
    case (as, Nil) => as
    case (Nil, bs) => bs
  }

  def periodicReportSpan[A, B](report: PeriodicReport[A, B]): DateSpan = {
    report.dates match {
      case Nil   => DateSpan(None, None)
      case spans => DateSpan(
        spans.headOption.flatMap(ds => spanStart(ds).map(Exact)), 
        spans.lastOption.flatMap(ds => spanEnd(ds).map(Exact))
      )
    }
  }
  
  def reportMapName[A, B, C](f: A=>B, pr: PeriodicReport[A, C]): PeriodicReport[B, C] = {
    pr.copy(
      rows = pr.rows.map(r => reportRowMapName(f, r))
    )
  }
  
  def reportRowMapName[A, B, C](f: A=>B, row: PeriodicReportRow[A, C]): PeriodicReportRow[B, C] = {
    row.copy(name = f(row.name))
  }
  
  case class DisplayName(
                          displayFull: AccountName,
                          displayName: AccountName,
                          displayDepth: Int
                        )
  
  def flatDisplayName(a: AccountName): DisplayName = DisplayName(a, a, 1)
  def treeDisplayName(a: AccountName): DisplayName = DisplayName(a, accountLeafName(a), accountNameLevel(a))

  def periodicReportRowFullName[A](prr: PeriodicReportRow[DisplayName, A]): AccountName = prr.name.displayFull
  def periodicReportRowDisplayName[A](prr: PeriodicReportRow[DisplayName, A]): AccountName = prr.name.displayName
  def periodicReportRowDepth[A](prr: PeriodicReportRow[DisplayName, A]): Int = prr.name.displayDepth
  
}
