package sledger.reports


import sledger.Queries.Query
import sledger.Queries.Query.Any

import java.time.LocalDate
import sledger.Status
import sledger.data.Dates._
import sledger.data.Journals.{Journal, journalDateSpan, journalDateSpanBoth}
import sledger.data.StringFormat.defaultBalanceLineFormat
import sledger.data.{Period, PeriodAll, StringFormat}

object ReportOptions {
  sealed trait BalanceCalculation
  case object CalcChange extends BalanceCalculation
  case object CalcBudget extends BalanceCalculation
  case object CalcValueChange extends BalanceCalculation
  case object CalcGain extends BalanceCalculation

  def defaultBalanceCalculation: BalanceCalculation = CalcChange

  sealed trait BalanceAccumulation

  case object PerPeriod extends BalanceAccumulation

  case object Cumulative extends BalanceAccumulation

  case object Historical extends BalanceAccumulation

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
                      pretty: Boolean,
                      balanceCalc: BalanceCalculation,
                      balanceAccum: BalanceAccumulation,
                      accountListMode: AccountListMode,
                      drop: Int,
                      date2: Boolean,
                      layout: Layout
                    )

  def defaultsOptions: Options = Options(
    period = PeriodAll,
    interval = NoInterval,
    statuses = List.empty,
    depth = None,
    format = defaultBalanceLineFormat,
    pretty = false,
    balanceCalc = defaultBalanceCalculation,
    balanceAccum = defaultBalanceAccumm,
    accountListMode = defaultAccountListMode,
    drop = 0,
    date2 = false,
    layout = LayoutWide(None)
  )
  case class Spec(options: Options, day: LocalDate, query: Query)

  def defaultSpec: Spec = Spec(defaultsOptions,nulldate, Any)
  
  def reportSpan(journal: Journal, spec: Spec): (DateSpan, List[DateSpan]) ={
    reporSpanHelper(secondary = false, journal, spec)
  }
  
  def whichDate(options: Options): WhichDate = if(options.date2) SecondaryDate else PrimaryDate
  
  def reporSpanHelper(secondary: Boolean, journal: Journal, spec: Spec): (DateSpan, List[DateSpan]) = {
    //todo this is requested span specified by various options args if any
    val requested = nulldatespan
    println("requested", requested)
    val journalspan = if (secondary) journalDateSpanBoth(journal) else journalDateSpan(spec.options.date2, journal)
    println("journalspan", journalspan)
    val requested1 = spanDefaultsFrom(requested, spanUnion(journalspan, nulldatespan))
    println("requested1", requested1)
    
    val intervalspans = {
      val adjust = spanStart(requested).isEmpty
      splitSpan(adjust, spec.options.interval,requested1)
    }
    println("intervalspans", intervalspans)

    val reportSpan = DateSpan(
      intervalspans.headOption.flatMap(a => spanStart(a).map(Exact)),
      intervalspans.lastOption.flatMap(a => spanEnd(a).map(Exact))
    )
    println("reportSpan", reportSpan)
    
    (reportSpan, intervalspans)
  }
}
