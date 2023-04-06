package sledger.read

import cats._
import cats.data._
import cats.syntax.all._
import cats.derived._
import parsley.Parsley
import parsley.Parsley.{attempt, join, lookAhead, notFollowedBy}
import parsley.character.{char, digit, endOfLine, isSpace, newline, satisfy, string}
import parsley.combinator.{eof, many, manyUntil, option, optional, optionalAs}
import  parsley.position.offset
import parsley.implicits.zipped._
import parsley.errors.combinator._
import parsley.debug._
import sledger.Types.isDecimalMark

import java.time.LocalDate
import scala.util.Try
import scala.Function.const
import sledger.data.Dates.datesepchar
import utils.Parse.{isLineCommentStart, isNewline, skipNonNewlineSpaces, spacenonewline, takeWhileP, takeWhileP1}

//import scala.math.Numeric.BigDecimalAsIfIntegral.negate

object Common {

  def readDecimal(s: String): Int = {
    s.foldLeft(0) { (a, c) =>
      a * 10 + c.asDigit
    }
  }
  
  val natural: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)  
  
  val emptyorcommentlinep: Parsley[Unit] = {
    val skiplinecommentp: Parsley[Unit] = {
      satisfy(isLineCommentStart) *> takeWhileP(_ != '\n') *> optional(endOfLine)
    }
    skipNonNewlineSpaces *> skiplinecommentp <|> endOfLine.void
  }

  val multilinecommentp: Parsley[Unit] = {
    val trailingSpaces = skipNonNewlineSpaces <* newline
    val startComment = attempt(string("comment")) *> trailingSpaces
    val endComment = eof <|> attempt(string("end comment")) *> trailingSpaces
    val anyLine = takeWhileP(_ != '\n') *> newline.void
    startComment *> manyUntil(anyLine.debug("any line"), endComment).void
  }
  
  val yearorintp: Parsley[Int] = takeWhileP1(_.isDigit).map(readDecimal)
  
  val datep: Parsley[LocalDate] = {
    (yearorintp, datesepchar, natural, datesepchar, natural)
      .zipped
      .collectMsg("This date is malformed because the separators are different.\n"
        ++ "Please use consistent separators.") {
        case (year, sep1, month, sep2, day) if sep1 == sep2 => Try(LocalDate.of(year, month, day)).toOption
      }.collectMsg("this date is invalid, please correct it") {
      case Some(d) => d
    }
  }
  
  val isSameLineCommentStart: Char => Boolean = {
    case ';' => true
    case _ => false
  } 
  
  val isDigitSeparatorChar: Char => Boolean = (c: Char) => isDecimalMark(c) || c == ' '
  
  val noncommenttextp: Parsley[String] = takeWhileP { c => !isSameLineCommentStart(c) || isNewline(c)}
  val descriptionp: Parsley[String] = noncommenttextp.label("description")
  val signp: Parsley[BigDecimal => BigDecimal] = {
    val sign: BigDecimal => BigDecimal = (n: BigDecimal) => 0 - n
    val idD: BigDecimal => BigDecimal = identity[BigDecimal]
    ((char('-') #> sign <|> char('+') #> idD) <* skipNonNewlineSpaces) <|> Parsley.pure(idD)
  }
  val singlespacep: Parsley[Unit] = spacenonewline *> notFollowedBy(spacenonewline)
  val singlespacedtextsatisfying1p: (Char => Boolean) => Parsley[String] = (f: Char => Boolean) => {
    val partp = takeWhileP1(c => f(c) && !isSpace(c))
    (partp, many(attempt(singlespacep*>partp)))
      .zipped {(firstPart, otherParts) => (firstPart :: otherParts).mkString(" ")}
  }
  val singlespacedtext1p: Parsley[String] = singlespacedtextsatisfying1p(const(true))
  val accountnamep: Parsley[String] = singlespacedtext1p
  
  sealed trait RawNumber
  case class NoSeparators(digitGrp: DigitGroup, 
                                  leadingOrTrailingSeparator: Option[(Char, DigitGroup)]) extends RawNumber
  case class WithSeparators(sep: Char, 
                                    digitGroups: List[DigitGroup], 
                                    leadingOrTrailingSeparator: Option[(Char, DigitGroup)]) extends RawNumber
 
  implicit val ShowRawNumber: Show[RawNumber] = Show.show({
    case NoSeparators(digitGrp, leadingOrTrailingSeparator) =>  s"NoSeparators ${digitGrp.show} ${leadingOrTrailingSeparator.show}"
    case WithSeparators(sep, digitGroups, leadingOrTrailingSeparator) => s"WithSeparators '$sep' ${digitGroups.show} ${leadingOrTrailingSeparator.show}"
  })
  
  implicit val ShowNoSeparator: Show[NoSeparators] = semiauto.show
  
  case class DigitGroup(digitGroupLength: Int, digitGroupNumber: Int)
  implicit val ShowDigitGroup: Show[DigitGroup] = (d: DigitGroup) => {
    val numStr = d.digitGroupNumber.show
    val padding = List.fill(d.digitGroupLength - numStr.length)('0')
    "\"" ++ padding.mkString ++ numStr ++ "\""
  }
  
  implicit val DigitGroupSemigroup: Semigroup[DigitGroup] = (x: DigitGroup, y: DigitGroup) => {
      DigitGroup(x.digitGroupLength + y.digitGroupLength, x.digitGroupNumber*10^y.digitGroupLength+y.digitGroupNumber)
  }
  
  implicit val DigitGroupMonoid: Monoid[DigitGroup] = new Monoid[DigitGroup] {
    override def empty: DigitGroup = DigitGroup(0,0)

    override def combine(x: DigitGroup, y: DigitGroup): DigitGroup = x |+| y
  }
  
  case class AmbiguousNumber(group1: DigitGroup, sep: Char, group2: DigitGroup)

  implicit val ShowAmbiguousNumber: Show[AmbiguousNumber] = (n: AmbiguousNumber) => s"AmbiguousNumber ${n.group1.show}${n.sep}${n.group2.show}"

  val digitgroupp: Parsley[DigitGroup] = {
    takeWhileP1(_.isDigit)
      .map { s =>
        s.foldLeft((0, 0)) { case ((l, a), c) =>
          (l + 1, a * 10 + c.asDigit)
        }
      }
      .map(c => DigitGroup(c._1, c._2))
  }
  val rawnumberp: Parsley[Either[AmbiguousNumber, RawNumber]] = {
    val trailingDecimalPt = (grp1: DigitGroup) => {
      satisfy(isDecimalMark).map(decPt => NoSeparators(grp1, Some((decPt, Monoid[DigitGroup].empty))))
    }
    val withDecimalPt = (digitSep: Char, digitGroups: List[DigitGroup]) => {
      (satisfy((c => isDecimalMark(c) && c != digitSep)), optionalAs(digitgroupp, Monoid[DigitGroup].empty)).zipped {(decPt, decDigitGrp)=> {
        WithSeparators(digitSep, digitGroups, Some((decPt,decDigitGrp)))
      }}
    }
    
    val withoutDecimalPt: (DigitGroup, Char, DigitGroup, List[DigitGroup]) => Either[AmbiguousNumber, WithSeparators] = (grp1: DigitGroup, sep: Char, grp2: DigitGroup, grps: List[DigitGroup]) => {
      if(grps.isEmpty && isDecimalMark(sep)) {
        Left(AmbiguousNumber(grp1, sep, grp2))
      }else {
        Right(WithSeparators(sep, (grp1::grp2::grps), None))
      }
    }
    
    val withSeparators = (grp1: DigitGroup) => {
      join(attempt((satisfy(isDigitSeparatorChar), digitgroupp).zipped {(sep, grp2) => {
       join(for {
          grps <- many(attempt(char(sep) *> digitgroupp))
          digitGroups = grp1 :: grp2 :: grps
        } yield {
          withDecimalPt(sep, digitGroups).map(Right(_)) <|> Parsley.pure(withoutDecimalPt(grp1, sep, grp2, grps))
        })
      }}))
    }
    
    val leadingDecimalPt: Parsley[RawNumber] = {
      (satisfy(isDecimalMark), digitgroupp)
        .zipped {(decPt, decGrp) => NoSeparators(Monoid[DigitGroup].empty, Some((decPt, decGrp))) }
    }
    
    val leadingDigits = {
      join(for {
        grp1 <- digitgroupp
      } yield {
        withSeparators(grp1) <|> trailingDecimalPt(grp1).map(Right(_)) <|> Parsley.pure(Right(NoSeparators(grp1, None)))
      })
    }
    (leadingDecimalPt.map(Right(_)) <|> leadingDigits).map(rawNumber => {
      option(lookAhead(satisfy(isDecimalMark)))
        .collectMsg("invalid number (invalid use of separator)") { _.isDefined}
      option(lookAhead(attempt(char(' ') *> offset <* digit)))
        .collectMsg("invalid number (excessive trailing digits)") {_.isDefined}
      rawNumber
    })
  }
  
//  val amountp = {
//    val spaces = skipNonNewlineSpaces
//    
//    val rightornosymbolamountp= (sign: BigDecimal => BigDecimal) => {
//      signp.map {f => 
//        
//      }
//    }
//    
//    ???
//  } 
}
