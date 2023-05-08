package sledger.data

import java.time.{LocalDate, YearMonth}

sealed trait Period
case class DayPeriod(day: LocalDate) extends Period
case class WeekPeriod(day: LocalDate) extends Period
case class MonthPeriod(year: Int, month: Int) extends Period
case class QuarterPeriod(year: Int, quarter: Int) extends Period
case class YearPeriod(year: Int) extends Period
case class PeriodBetween(start: LocalDate, end: LocalDate) extends Period
case class PeriodFrom(start: LocalDate) extends Period
case class PeriodTo(end: LocalDate) extends Period
case object PeriodAll extends Period

object Period {
  def monthPeriod(year: Int, month: Int): MonthPeriod = MonthPeriod(year, month)

  def quarterPeriod(year: Int, quarter: Int): QuarterPeriod = {
    val month = 3 * quarter - 2
    QuarterPeriod(year, month)
  }

  def yearPeriod(year: Int): YearPeriod = YearPeriod(year)

  def fromLocalDate(date: LocalDate): Period = date.getDayOfMonth match {
    case 1 => MonthPeriod(date.getYear, date.getMonthValue)
    case d if d <= 7 && date.getDayOfWeek.getValue == 1 => WeekPeriod(date)
    case _ => DayPeriod(date)
  }

  def toLocalDate(period: Period): Option[LocalDate] = period match {
    case DayPeriod(day) => Some(day)
    case WeekPeriod(day) => Some(day)
    case MonthPeriod(year, month) => Some(YearMonth.of(year, month).atDay(1))
    case QuarterPeriod(year, quarter) => Some(YearMonth.of(year, 3 * quarter - 2).atDay(1))
    case YearPeriod(year) => Some(LocalDate.of(year, 1, 1))
    case PeriodBetween(start, _) => Some(start)
    case PeriodFrom(start) => Some(start)
    case PeriodTo(end) => Some(end)
    case PeriodAll => None
  }
}