package sledger.data

import cats._
import cats.syntax.all._
import parsley.Parsley
import parsley.character.satisfy

import java.time.temporal.TemporalAdjusters
import java.time.{DayOfWeek, LocalDate, Month, MonthDay}
import scala.annotation.tailrec

object Dates {
  val isDateSepChar: Char => Boolean = {
    case c if c == '-' || c == '/' || c == '.' => true
  }
  val datesepchar: Parsley[Char] = satisfy(isDateSepChar)

  def nulldate: LocalDate = LocalDate.of(0, 1, 1)

  sealed trait WhichDate
  case object PrimaryDate extends WhichDate
  case object SecondaryDate extends WhichDate
  
  sealed trait EFDay

  case class Exact(day: LocalDate) extends EFDay {
    override def equals(obj: Any): Boolean = obj match {
      case Exact(d) => d.equals(day)
      case _ => false
    }
  }

  case class Flex(day: LocalDate) extends EFDay {
    override def equals(obj: Any): Boolean = obj match {
      case Flex(d) => d.equals(day)
      case _ => false
    }
  }

  def fromEFDay(efday: EFDay): LocalDate = efday match {
    case Exact(d) => d
    case Flex(d) => d
  }

  def modifyEFDay(f: LocalDate => LocalDate, efday: EFDay): EFDay = efday match {
    case Exact(d) => Exact(f(d))
    case Flex(d) => Flex(f(d))
  }

  case class DateSpan(start: Option[EFDay] = None, end: Option[EFDay] = None)
  val nulldatespan = DateSpan(None, None)
  def spanStart(span: DateSpan): Option[LocalDate] = span.start.map(fromEFDay)

  def spanEnd(span: DateSpan): Option[LocalDate] = span.end.map(fromEFDay)


  val addDays: Int => LocalDate => LocalDate = n => date => date.plusDays(n)
  val addMonths: Int => LocalDate => LocalDate = n => date => date.plusMonths(n)
  val addYears: Int => LocalDate => LocalDate = n => date => date.plusYears(n)
  
  def splitSpan(adjust: Boolean, interval: Interval, dateSpan: DateSpan): List[DateSpan] = {
    (adjust, interval, dateSpan) match {
      case (_, _, DateSpan(None, None)) => List(DateSpan(None, None))
      case (_, _, ds) if isEmptySpan(ds) => List.empty
      case (_, _, ds@DateSpan(Some(s), Some(e))) if fromEFDay(s).equals(fromEFDay(e)) => List(ds)
      case (_, NoInterval, ds) => List(ds)
      case (_, Days(n), ds) => splitspan(identity, addDays, n, ds)
      case (adjust, Weeks(n), ds) => splitspan(if (adjust) startofweek else identity, addDays, 7 * n, ds)
      case (adjust, Months(n), ds) => splitspan(if (adjust) startofmonth else identity, addMonths, n, ds)
      case (adjust, Quarters(n), ds) => splitspan(if (adjust) startofquarter else identity, addMonths, 3 * n, ds)
      case (adjust, Years(n), ds) => splitspan(if (adjust) startofquarter else identity, addYears, n, ds)
      case (_, DayOfMonth(n),ds) => splitspan(nthdayofmonthcontaining(n, _: LocalDate), addMonths, 1, ds)
      case (_, DayOfYear(m, n),ds) => splitspan(nthdayofyearcontaining(m, n, _: LocalDate), addMonths, 1, ds)

      case (_, WeekdayOfMonth(n, wd), ds) =>
        val advancemonths: Int => LocalDate => LocalDate = n => date => {
          if (n == 0) identity(date) else {
            val f = advancetonthweekday(n, wd, _: LocalDate)
            f.compose(startofmonth).compose(addMonths(n)).apply(date)
          }
        }
        splitspan(nthweekdayofmonthcontaining(n, wd, _: LocalDate), advancemonths, 1, ds)
      case (_, DaysOfWeek(Nil),ds) => List(ds)
      case (_, DaysOfWeek(days@(n::_)), ds) =>
        val (s, e) = dateSpanSplitLimits(nthdayofweekcontaining(n, _:LocalDate), addDays(1), ds)
        val starts = days.map(d => nthdayofweekcontaining(n, s.plusDays(d - n)))
        val bdrys = (0 to 6).flatMap(n => starts.map(d => d.plusDays(n))).toList
        spansFromBoundaries(e, bdrys)
    }
  }

  def nthdayofweekcontaining(n: Int, d: LocalDate): LocalDate = {
    val nthOfSameWeek = d.`with`(TemporalAdjusters.nextOrSame(DayOfWeek.of(n)))
    if (nthOfSameWeek.isEqual(d)) nthOfSameWeek
    else d.`with`(TemporalAdjusters.previous(DayOfWeek.of(n)))
  }
  def nthweekdayofmonthcontaining(n: Int, wd: Int, date: LocalDate): LocalDate = {
    val nthWeekdaySameMonth = advancetonthweekday(n, wd, startofmonth(date))
    val nthWeekdayPrevMonth = advancetonthweekday(n, wd, prevmonth(date))
    if (nthWeekdaySameMonth.isBefore(date) || nthWeekdaySameMonth == date) nthWeekdaySameMonth
    else nthWeekdayPrevMonth
  }
  def advancetonthweekday(n: Int, wd: Int, date: LocalDate): LocalDate = {
    val dayOfWeek = DayOfWeek.of(wd)
    val diff = dayOfWeek.getValue - date.getDayOfWeek.getValue
    val adjDiff = if (diff < 0) diff + 7 else diff
    val firstOccurrence = date.plusDays(adjDiff)
    if (n == 1) firstOccurrence
    else advancetonthweekday(n - 1, wd, firstOccurrence.plusWeeks(1))
  }

  def startofweek(date: LocalDate): LocalDate = {
    val dow = date.getDayOfWeek.getValue
    date.minusDays(dow - DayOfWeek.MONDAY.getValue)
  }
  
  def startofmonth(date: LocalDate): LocalDate = date.`with`(TemporalAdjusters.firstDayOfMonth())
  
  def startofyear(date: LocalDate): LocalDate = LocalDate.of(date.getYear, 1, 1)
  
  def startofquarter(date: LocalDate): LocalDate = {
    val month = date.getMonthValue
    val quarterStartMonth = ((month - 1) / 3) * 3 + 1 // January = 1, April = 4, etc.
    LocalDate.of(date.getYear, quarterStartMonth, 1)
  }
  def nthdayofmonth(mdy: MonthDay, date: LocalDate): LocalDate =
    date.`with`(mdy).`withDayOfMonth`(Math.min(date.lengthOfMonth(), mdy.getDayOfMonth()))

  def prevmonth(date: LocalDate): LocalDate = date.minusMonths(1)

  def nthdayofmonthcontaining(mdy: Int, date: LocalDate): LocalDate = {
    val s = startofmonth(date)
    val nthOfSameMonth = nthdayofmonth(MonthDay.of(mdy, s.getDayOfMonth), s)
    val nthOfPrevMonth = nthdayofmonth(MonthDay.of(mdy, s.getDayOfMonth), prevmonth(s))
    if (nthOfSameMonth.isBefore(date) || nthOfSameMonth.isEqual(date)) {
      nthOfSameMonth
    } else {
      nthOfPrevMonth
    }
  }

  def nthdayofyearcontaining(m: Int, mdy: Int, date: LocalDate): LocalDate = {
    val s = startofyear(date)
    val month = Month.of(m)
    val mmddOfSameYear = s.plusMonths(month.getValue - 1).withDayOfMonth(mdy)
    val mmddOfPrevYear = startofyear(date.minusYears(1)).plusMonths(month.getValue - 1).withDayOfMonth(mdy)
    if (mmddOfSameYear.isBefore(date) || mmddOfSameYear.isEqual(date)) {
      mmddOfSameYear
    } else {
      mmddOfPrevYear
    }
  }
  
  def spansFromBoundaries(e: LocalDate, bdrys: List[LocalDate]): List[DateSpan] =
    (bdrys.takeWhile(a=> a.isBefore(e)), bdrys.drop(1)).mapN((a, b) => DateSpan(Some(Exact(a)), Some(Exact(b))))
    
  def splitspan(start: LocalDate => LocalDate, addInterval: Int => LocalDate => LocalDate, mult: Int, ds: DateSpan) = {
    val (s, e) = dateSpanSplitLimits(start, addInterval.apply(mult), ds)
    val bdrys = (0 to Int.MaxValue by mult).map(i => start(addInterval(i)(start(s)))).toList
    spansFromBoundaries(e, bdrys)
  }

  def dateSpanSplitLimits(start: LocalDate => LocalDate, next: LocalDate => LocalDate, ds: DateSpan): (LocalDate, LocalDate) = ds match {
    case DateSpan(Some(s), Some(e)) => (start(fromEFDay(s)), fromEFDay(e))
    case DateSpan(Some(s), None) => (start(fromEFDay(s)), next(start(fromEFDay(s))))
    case DateSpan(None, Some(e)) => (start(fromEFDay(e)), next(start(fromEFDay(e))))
    case DateSpan(None, None) => throw new RuntimeException("dateSpanSplitLimits: should not be nulldatespan")
  }
      
  def spansUnion(spans: List[DateSpan]): DateSpan = spans match {
    case Nil => nulldatespan
    case List(d) => d
    case d :: ds => spanUnion(d, spansUnion(ds))
  }

  def spanUnion(d1: DateSpan, d2: DateSpan): DateSpan = {
    val b = earliest(d1.start, d2.start)
    val e = latest(d1.end, d2.end)
    DateSpan(b, e)
  }

  def latest(d1: Option[EFDay], d2: Option[EFDay]): Option[EFDay] = (d1, d2) match {
    case (Some(Exact(day1)), Some(Exact(day2))) => Some(if (day1.compareTo(day2) > 0) Exact(day1) else Exact(day2))
    case (Some(Flex(day1)), Some(Flex(day2))) => Some(if (day1.compareTo(day2) > 0) Flex(day1) else Flex(day2))
    case (Some(Exact(day1)), Some(Flex(day2))) => Some(if (day1.compareTo(day2) > 0) Exact(day1) else Flex(day2))
    case (Some(Flex(day1)), Some(Exact(day2))) => Some(if (day1.compareTo(day2) > 0) Flex(day1) else Exact(day2))
    case _ => d1.orElse(d2)
  }


  def earliest(d1: Option[EFDay], d2: Option[EFDay]): Option[EFDay] = (d1, d2) match {
    case (Some(x), Some(y)) => (x, y) match {
      case (Exact(d1), Exact(d2)) => Some(if (d1.isBefore(d2)) x else y)
      case (Flex(d1), Flex(d2)) => Some(if (d1.isAfter(d2)) x else y)
      case (Exact(d1), Flex(d2)) => Some(if (d1.isBefore(d2) || d1.equals(d2)) x else y)
      case (Flex(d1), Exact(d2)) => Some(if (d1.isAfter(d2) || d1.equals(d2)) x else y)
    }
    case _ => d1.orElse(d2)
  }

  def groupByDateSpan[A](showempty: Boolean,
                         date: A => LocalDate,
                         colspans: List[DateSpan],
                         ps: List[A]): List[(DateSpan, List[A])] = {
    def beforeStart(day: LocalDate): Boolean = {
      val start = colspans.headOption.flatMap(spanStart)
      start match {
        case Some(d) => d.isBefore(day)
        case None => true
      }
    }

    def groupByCols(spans: List[DateSpan], rows: List[(LocalDate, A)]): List[(DateSpan, List[A])] = spans match {
      case Nil => Nil
      case c :: cs =>
        if (rows.isEmpty) {
          if (showempty) (c, Nil) :: groupByCols(cs, Nil)
          else Nil
        } else {
          val (matches, later) = rows.span(p => spanEnd(c).forall(e => e.compareTo(p._1) > 0))
          (c, matches.map(_._2)) :: groupByCols(cs, later)
        }
    }

    val xs = ps
      .map(a => (date(a), a))
      .sortBy(_._1)
      .dropWhile(p => beforeStart(p._1))
    groupByCols(colspans, xs)
  }
  
  def spanContainsDate(ds: DateSpan, date: LocalDate): Boolean = {
    (ds, date) match {
      case (DateSpan(None, None), _) => true
      case (DateSpan(None, Some(e)), d) => d.isBefore(fromEFDay(e))
      case (DateSpan(Some(b), None), d) => !d.isBefore(fromEFDay(b))
      case (DateSpan(Some(b), Some(e)), d) => !d.isBefore(fromEFDay(b)) && d.isBefore(fromEFDay(e))
    }
  }
  
  def spansIntersect(dss: List[DateSpan]): DateSpan = {
    dss match {
      case Nil => nulldatespan
      case List(d) => d
      case d::ds => spanIntersect(d, spansIntersect(ds))
    }
  }

  def spanIntersect(ds1: DateSpan, ds2: DateSpan): DateSpan = {
    val b = latest(ds1.start, ds2.start)
    val e = earliest(ds1.end, ds2.end)
    DateSpan(b, e)
  }
  
  def spanDefaultsFrom(span1: DateSpan, span2: DateSpan): DateSpan = {
    val a = span1.start.orElse(span2.start)
    val b = span1.end.orElse(span2.end)
    DateSpan(a, b)
  }

  def isEmptySpan(ds: DateSpan): Boolean = ds match {
    case DateSpan(Some(s), Some(e)) => fromEFDay(e).isBefore(fromEFDay(s))
    case _ => false
  }
  
  sealed trait Interval
  case object NoInterval extends Interval
  case class Days(n: Int) extends Interval
  case class Weeks(n: Int) extends Interval
  case class Months(n: Int) extends Interval
  case class Quarters(n: Int) extends Interval
  case class Years(n: Int) extends Interval
  case class DayOfMonth(dayOfMonth: Int) extends Interval
  case class WeekdayOfMonth(weekday: Int, occurrence: Int) extends Interval
  case class DaysOfWeek(daysOfWeek: List[Int]) extends Interval
  case class DayOfYear(month: Int, day: Int) extends Interval
}
