package sledger.data

import sledger.data.Dates.{DateSpan, Exact, Flex, fromEFDay}

import java.time.format.DateTimeFormatter
import java.time.temporal.{IsoFields, WeekFields}
import java.time.{LocalDate, YearMonth}
import java.util.Locale

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
  
  def quarterAsMonth(q: Int): Int = (q - 1) * 3 + 1

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

  def dateSpanAsPeriod(dateSpan: DateSpan) = dateSpan match {
    case DateSpan(Some(b), Some(e)) => simplifyPeriod(PeriodBetween(fromEFDay(b), fromEFDay(e)))
    case DateSpan(Some(b), None) => PeriodFrom(fromEFDay(b))
    case DateSpan(None, Some(e)) => PeriodTo(fromEFDay(e))
    case DateSpan(None, None) => PeriodAll
  }

  def simplifyPeriod(period: Period) = period match {

    case PeriodBetween(start, end) => {
      val a = (start.getYear, start.getMonthValue, start.getDayOfMonth)
      val b = (start.getYear, start.getMonthValue, start.getDayOfMonth)
      val (by, bw, bd) = toWeekDate(start)
      val (ey, ew, ed) = toWeekDate(end.minusDays(1))
      (a, b) match {
        case ((by, 1, 1), (ey, 1, 1)) if (by + 1 == ey) => YearPeriod(by)
        case ((by, 1, 1), (ey, 4, 1)) if (by == ey) => QuarterPeriod(by, 1)
        case ((by, 4, 1), (ey, 7, 1)) if (by == ey) => QuarterPeriod(by, 2)
        case ((by, 7, 1), (ey, 10, 1)) if (by == ey) => QuarterPeriod(by, 3)
        case ((by, 10, 1), (ey, 1, 1)) if (by == ey) => QuarterPeriod(by, 4)
        case ((by, bm, 1), (ey, em, 1)) if (by == ey && bm + 1 == em) => MonthPeriod(by, bm)
        case ((by, 12, 1), (ey, 1, 1)) if (by + 1 == ey) => MonthPeriod(by, 12)
        case _ if (by == ey && bw == ew && bd == 1 && ed == 7) => WeekPeriod(end)
        case ((by, bm, bd), (ey, em, ed)) if ((by == ey && bm == em && bd + 1 == ed)
          || (by + 1 == ey && bm == 12 && em == 1 && bd == 31 && ed == 1)
          || (by == ey && bm + 1 == em && isLastDayOfMonth(LocalDate.of(by, bm, bd)) && ed == 1)) => DayPeriod(start)
        case _ => PeriodBetween(start, end)
      }
    }
    case p => p
  }

  def toWeekDate(date: LocalDate): (Int, Int, Int) = {
    val weekFields = WeekFields.of(Locale.getDefault())
    val weekOfYear = date.get(weekFields.weekOfWeekBasedYear())
    val dayOfWeek = date.getDayOfWeek.getValue
    val year = date.get(weekFields.weekBasedYear())
    (year, weekOfYear, dayOfWeek)
  }

  def isLastDayOfMonth(date: LocalDate): Boolean = {
    date.getDayOfMonth == date.lengthOfMonth
  }

  def periodAsDateSpan(period: Period): DateSpan = period match {
    case DayPeriod(d) =>
      val end = d.plusDays(1)
      DateSpan(Some(Exact(d)), Some(Exact(end)))

    case WeekPeriod(b) =>
      val end = b.plusDays(7)
      DateSpan(Some(Flex(b)), Some(Flex(end)))

    case MonthPeriod(y, m) =>
      val (y2, m2) = if (m == 12) (y + 1, 1) else (y, m + 1)
      val end = LocalDate.of(y2, m2, 1)
      DateSpan(Some(Flex(LocalDate.of(y, m, 1))), Some(Flex(end)))

    case QuarterPeriod(y, q) =>
      val (y2, q2) = if (q == 4) (y + 1, 1) else (y, q + 1)
      val end = LocalDate.of(y2, quarterAsMonth(q2), 1)
      DateSpan(Some(Flex(LocalDate.of(y, quarterAsMonth(q), 1))), Some(Flex(end)))

    case YearPeriod(y) =>
      val end = LocalDate.of(y + 1, 1, 1)
      DateSpan(Some(Flex(LocalDate.of(y, 1, 1))), Some(Flex(end)))

    case PeriodBetween(b, e) =>
      DateSpan(Some(Exact(b)), Some(Exact(e)))

    case PeriodFrom(b) =>
      DateSpan(Some(Exact(b)), None)

    case PeriodTo(e) =>
      DateSpan(None, Some(Exact(e)))

    case PeriodAll =>
      DateSpan(None, None)
  }

  def showPeriod(period: Period): String = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM", Locale.getDefault)

    period match {
      case DayPeriod(b) =>
        b.format(DateTimeFormatter.ISO_DATE)

      case WeekPeriod(b) =>
        val weekYear = b.getYear
        val week = b.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR)
        s"$weekYear-W$week"

      case MonthPeriod(y, m) =>
        f"$y%04d-$m%02d"

      case QuarterPeriod(y, q) =>
        s"$y-Q$q"

      case YearPeriod(y) =>
        f"$y%04d"

      case PeriodBetween(b, e) =>
        val start = b.format(DateTimeFormatter.ISO_DATE)
        val end = e.minusDays(1).format(DateTimeFormatter.ISO_DATE)
        s"$start..$end"

      case PeriodFrom(b) =>
        s"${b.format(DateTimeFormatter.ISO_DATE)}.."

      case PeriodTo(e) =>
        s"..${e.minusDays(1).format(DateTimeFormatter.ISO_DATE)}"

      case PeriodAll =>
        ".."
    }
  }
}