package sledger.reports

import sledger.Queries.Query._
import sledger.Queries.{Query, simplifyQuery}
import sledger.data.Dates._
import sledger.data.Journals.{Journal, journalDateSpan, journalDateSpanBoth}
import sledger.data.Period.periodAsDateSpan
import sledger.data.StringFormat.defaultBalanceLineFormat
import sledger.data.{Period, PeriodAll, StringFormat}
import sledger.{NormalSign, Status}

import java.time.LocalDate

object ReportOptions {
  sealed trait BalanceCalculation

  case object CalcChange extends BalanceCalculation

  case object CalcBudget extends BalanceCalculation

  case object CalcValueChange extends BalanceCalculation

  case object CalcGain extends BalanceCalculation

  def defaultBalanceCalculation: BalanceCalculation = CalcChange

  sealed trait BalanceAccumulation

  case object PerPeriod extends BalanceAccumulation

  //  case object Cumulative extends BalanceAccumulation

  //  case object Historical extends BalanceAccumulation

  def defaultBalanceAccumm: BalanceAccumulation = PerPeriod

  sealed trait AccountListMode

  case object ALFlat extends AccountListMode

  case object ALTree extends AccountListMode

  def defaultAccountListMode: AccountListMode = ALFlat

  sealed trait Layout

  case class LayoutWide(width: Option[Int]) extends Layout

  case object LayoutTall extends Layout

  case object LayoutBare extends Layout

  case object LayoutTidy extends Layout

  case class Options(
                      period: Period,
                      interval: Interval,
                      statuses: List[Status],
                      depth: Option[Int],
                      format: StringFormat,
                      noElide: Boolean,
                      pretty: Boolean,
                      balanceCalc: BalanceCalculation,
                      balanceAccum: BalanceAccumulation,
                      accountListMode: AccountListMode,
                      sortAmount: Boolean,
                      drop: Int,
                      noTotal: Boolean,
                      date2: Boolean,
                      empty: Boolean,
                      color: Boolean,
                      normalbalance: Option[NormalSign],
                      layout: Layout
                    )

  def defaultsOptions: Options = Options(
    period = PeriodAll,
    interval = NoInterval,
    statuses = List.empty,
    depth = None,
    format = defaultBalanceLineFormat,
    noElide = false,
    empty = false,
    pretty = false,
    balanceCalc = defaultBalanceCalculation,
    balanceAccum = defaultBalanceAccumm,
    accountListMode = defaultAccountListMode,
    sortAmount = false,
    drop = 0,
    noTotal = false,
    date2 = false,
    color = true, // todo parse from args
    normalbalance = None,
    layout = LayoutWide(None)
  )

  case class Spec(options: Options, day: LocalDate, query: Query)

  val defaultSpec: Spec = Spec(defaultsOptions, nulldate, Any)

  def reportSpan(journal: Journal, spec: Spec): (DateSpan, List[DateSpan]) = {
    reporSpanHelper(secondary = false, journal, spec)
  }

  def whichDate(options: Options): WhichDate = if (options.date2) SecondaryDate else PrimaryDate

  def reporSpanHelper(secondary: Boolean, journal: Journal, spec: Spec): (DateSpan, List[DateSpan]) = {
    //todo this is requested span specified by various options args if any
    val requested = nulldatespan
    //    println("requested", requested)
    val journalspan = if (secondary) journalDateSpanBoth(journal) else journalDateSpan(spec.options.date2, journal)
    //    println("journalspan", journalspan)
    val requested1 = spanDefaultsFrom(requested, spanUnion(journalspan, nulldatespan))
    //    println("requested1", requested1)

    val intervalspans = {
      val adjust = spanStart(requested).isEmpty
      splitSpan(adjust, spec.options.interval, requested1)
    }
    //    println("intervalspans", intervalspans)

    val reportSpan = DateSpan(
      intervalspans.headOption.flatMap(a => spanStart(a).map(Exact)),
      intervalspans.lastOption.flatMap(a => spanEnd(a).map(Exact))
    )
    //    println("reportSpan", reportSpan)

    (reportSpan, intervalspans)
  }

  def queryFromFlags(options: Options): Query = {
    def consOption[A, B](f: A => B, option: Option[A], xs: List[B]): List[B] = {
      option match {
        case Some(a) => f(a) :: xs
        case None => xs
      }
    }

    simplifyQuery(
      And(
        consOption(
          Depth,
          options.depth,
          List((if (options.date2) Date2 else Date)(periodAsDateSpan(options.period)))
        )
      )
    )
  }

  def reportOptsToSpec(options: Options): Spec = {
    Spec(
      options,
      LocalDate.now(),
      simplifyQuery(And(List(queryFromFlags(options))))
    )
  }
}
