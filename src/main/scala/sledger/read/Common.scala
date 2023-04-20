package sledger.read

import cats._
import cats.syntax.all._
import cats.derived._

import parsley.Parsley
import parsley.Parsley.{attempt, join, lookAhead, notFollowedBy}
import parsley.character.{char, digit, endOfLine, isSpace, newline, satisfy, string}
import parsley.combinator.{attemptChoice, between, choice, eof, many, manyUntil, option, optional, optionalAs}
import parsley.position.{offset, pos}
import parsley.implicits.zipped._
import parsley.errors.combinator._
import parsley.debug._

import java.time.LocalDate
import scala.util.Try
import scala.Function.const
import sledger.Types.{BalanceAssertion, Cleared, Pending, Status, Unmarked, isDecimalMark}
import sledger.data.Amounts._
import sledger.data.Dates._
import utils.Parse.{eolof, isLineCommentStart, isNewline, skipNonNewlineSpaces, skipNonNewlineSpaces1, skipNonNewlineSpacesb, spacenonewline, takeWhileP, takeWhileP1}

object Common {

  private def readDecimal(s: String): Int = {
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
  
  val followingcommentp_ = (contentp: Parsley[String]) => {
    val headerp = char(';') *> skipNonNewlineSpaces
    (skipNonNewlineSpaces,
      attempt(headerp) *> contentp.map(a => a :: Nil) <|> Parsley.pure(List[String]()),
      eolof,
      many(attempt(skipNonNewlineSpaces1 *> headerp) *> contentp <* eolof))
      .zipped { (_, sameLine, _, nextLines) =>
        val sameLine_ = if(sameLine.isEmpty && nextLines.nonEmpty) {
          List()
        } else sameLine
        (sameLine_ ++ nextLines).foldLeft(List.empty[String]) { (acc, t) =>
          t :: """\n""" :: acc
        }.mkString.strip()
      }
  }
  val followingcommentp = followingcommentp_(takeWhileP(a => a != '\n'))
  val transactioncommentp = followingcommentp
  val noncommenttextp: Parsley[String] = takeWhileP { c => !isSameLineCommentStart(c) || isNewline(c) }
  val descriptionp: Parsley[String] = noncommenttextp.label("description")
  val statusp: Parsley[Status] = {
    attemptChoice(skipNonNewlineSpaces *> char('*') #> Cleared,
      skipNonNewlineSpaces *> char('!') #> Pending,
      Parsley.pure(()) #> Unmarked
    ).label("cleared status")
  }
  
  val codep: Parsley[String] = {
    optionalAs(attempt({
      (skipNonNewlineSpaces1,
        char('('),
        takeWhileP(c => c != ')' && c != '\n'),
        char(')')).zipped { (_, _, code, _) => code }
    }), "").label("transaction code")
  }
  
  val signp: Parsley[BigDecimal => BigDecimal] = {
    val sign: BigDecimal => BigDecimal = (n: BigDecimal) => 0 - n
    val idD: BigDecimal => BigDecimal = identity[BigDecimal]
    ((char('-') #> sign <|> char('+') #> idD) <* skipNonNewlineSpaces) <|> Parsley.pure(idD)
  }
  val singlespacep: Parsley[Unit] = spacenonewline *> notFollowedBy(spacenonewline)
  val singlespacedtextsatisfying1p: (Char => Boolean) => Parsley[String] = (f: Char => Boolean) => {
    val partp = takeWhileP1(c => f(c) && !isSpace(c))
    (partp, many(attempt(singlespacep *> partp)))
      .zipped { (firstPart, otherParts) => (firstPart :: otherParts).mkString(" ") }
  }
  val singlespacedtext1p: Parsley[String] = singlespacedtextsatisfying1p(const(true))
  val accountnamep: Parsley[String] = singlespacedtext1p

  sealed trait RawNumber

  case class NoSeparators(digitGrp: DigitGroup,
                          mDecimals: Option[(Char, DigitGroup)]) extends RawNumber

  case class WithSeparators(sep: Char,
                            digitGroups: List[DigitGroup],
                            mDecimals: Option[(Char, DigitGroup)]) extends RawNumber

  implicit val ShowRawNumber: Show[RawNumber] = Show.show({
    case NoSeparators(digitGrp, leadingOrTrailingSeparator) => s"NoSeparators ${digitGrp.show} ${leadingOrTrailingSeparator.show}"
    case WithSeparators(sep, digitGroups, leadingOrTrailingSeparator) => s"WithSeparators '$sep' ${digitGroups.show} ${leadingOrTrailingSeparator.show}"
  })

  implicit val ShowNoSeparator: Show[NoSeparators] = semiauto.show

  case class DigitGroup(digitGroupLength: Byte, digitGroupNumber: Int)

  implicit val eqDigitGroup: Eq[DigitGroup] = semiauto.eq
  implicit val ShowDigitGroup: Show[DigitGroup] = (d: DigitGroup) => {
    val numStr = d.digitGroupNumber.show
    val padding = List.fill(d.digitGroupLength - numStr.length)('0')
    "\"" ++ padding.mkString ++ numStr ++ "\""
  }

  implicit val DigitGroupSemigroup: Semigroup[DigitGroup] = (x: DigitGroup, y: DigitGroup) => {
    DigitGroup((x.digitGroupLength + y.digitGroupLength).toByte, x.digitGroupNumber * 10 ^ y.digitGroupLength + y.digitGroupNumber)
  }

  implicit val DigitGroupMonoid: Monoid[DigitGroup] = new Monoid[DigitGroup] {
    override def empty: DigitGroup = DigitGroup(0, 0)

    override def combine(x: DigitGroup, y: DigitGroup): DigitGroup = x |+| y
  }

  case class AmbiguousNumber(group1: DigitGroup, sep: Char, group2: DigitGroup)

  implicit val ShowAmbiguousNumber: Show[AmbiguousNumber] = (n: AmbiguousNumber) => s"AmbiguousNumber ${n.group1.show}${n.sep}${n.group2.show}"

  val digitgroupp: Parsley[DigitGroup] = {
    takeWhileP1(_.isDigit)
      .map { s =>
        DigitGroup.tupled.apply(s.foldLeft((0.toByte, 0)) { case ((l, a), c) =>
          ((l + 1).toByte, a * 10 + c.asDigit)
        })
      }
  }

  val rawnumberp: Parsley[Either[AmbiguousNumber, RawNumber]] = {
    val trailingDecimalPt = (grp1: DigitGroup) => {
      satisfy(isDecimalMark).map(decPt => NoSeparators(grp1, Some((decPt, Monoid[DigitGroup].empty))))
    }
    val withDecimalPt = (digitSep: Char, digitGroups: List[DigitGroup]) => {
      (satisfy(c => isDecimalMark(c) && c != digitSep), optionalAs(digitgroupp, Monoid[DigitGroup].empty)).zipped { (decPt, decDigitGrp) => {
        WithSeparators(digitSep, digitGroups, Some((decPt, decDigitGrp)))
      }
      }
    }

    val withoutDecimalPt: (DigitGroup, Char, DigitGroup, List[DigitGroup]) => Either[AmbiguousNumber, WithSeparators] = (grp1: DigitGroup, sep: Char, grp2: DigitGroup, grps: List[DigitGroup]) => {
      if (grps.isEmpty && isDecimalMark(sep)) {
        Left(AmbiguousNumber(grp1, sep, grp2))
      } else {
        Right(WithSeparators(sep, grp1 :: grp2 :: grps, None))
      }
    }

    val withSeparators = (grp1: DigitGroup) => {
      join(attempt((satisfy(isDigitSeparatorChar), digitgroupp).zipped { (sep, grp2) => {
        join(for {
          grps <- many(attempt(char(sep) *> digitgroupp))
          digitGroups = grp1 :: grp2 :: grps
        } yield {
          withDecimalPt(sep, digitGroups).map(Right(_)) <|> Parsley.pure(withoutDecimalPt(grp1, sep, grp2, grps))
        })
      }
      }))
    }

    val leadingDecimalPt: Parsley[RawNumber] = {
      (satisfy(isDecimalMark), digitgroupp)
        .zipped { (decPt, decGrp) => NoSeparators(Monoid[DigitGroup].empty, Some((decPt, decGrp))) }
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
        .collectMsg("invalid number (invalid use of separator)") {
          _.isDefined
        }
      option(lookAhead(attempt(char(' ') *> offset <* digit)))
        .collectMsg("invalid number (excessive trailing digits)") {
          _.isDefined
        }
      rawNumber
    })
  }

  val quotedcommoditysymbolp: Parsley[String] = between(char('"'), char('"'), takeWhileP { c =>
    c != ';' && c != '\n' && c != '\"'
  })

  val simplecommoditysymbolp: Parsley[String] = takeWhileP1(c => !isNonsimpleCommodityChar(c))
  val commoditysymbolp: Parsley[String] =
    quotedcommoditysymbolp <|> simplecommoditysymbolp
  val disambiguateNumber: AmbiguousNumber => WithSeparators =
    (ambgNumber: AmbiguousNumber) => WithSeparators(ambgNumber.sep, List(ambgNumber.group1, ambgNumber.group2), None)
  val fromRawNumber: RawNumber => Either[String, (BigDecimal, Byte, Option[Char], Option[DigitGroupStyle])] = (raw: RawNumber) => {
    def toQuantity(preDecimalGrp: DigitGroup, posDecimalGrp: DigitGroup): Either[String, (BigDecimal, Byte)] = {
      val digitGrpNum = (preDecimalGrp |+| posDecimalGrp).digitGroupNumber
      val precision = posDecimalGrp.digitGroupLength.toInt - 0 // fixme get exponent
      if (precision < 0) {
        Right(BigDecimal(0, digitGrpNum*10^(-precision)), 0)
      } else if (precision < 255) {
        Right(BigDecimal(precision.toByte, digitGrpNum), 0)
      } else Left("invalid number: numbers with more than 255 decimal places are currently not supported")
    }

    def mDecPt(raw: RawNumber): Option[Char] = raw match {
      case NoSeparators(_, mDecimals) => mDecimals.map(md => md._1)
      case WithSeparators(_, _, mDecimals) => mDecimals.map(md => md._1)
    }

    def decimalGroup(raw: RawNumber): DigitGroup = raw match {
      case NoSeparators(_, mDecimals) => mDecimals._2F.getOrElse(Monoid[DigitGroup].empty)
      case WithSeparators(_, _, mDecimals) => mDecimals._2F.getOrElse(Monoid[DigitGroup].empty)
    }

    def digitGroup(raw: RawNumber): DigitGroup = raw match {
      case NoSeparators(digitGrp, _)         => digitGrp
      case WithSeparators(_, digitGroups, _) => digitGroups.combineAll
    }

    def groupSizes(digitGroups: List[DigitGroup]): List[Byte] = digitGroups.map(_.digitGroupLength) match {
      case a :: b :: cs if a < b => b :: cs
      case gs => gs
    }

    def digitGroupStyle(raw: RawNumber): Option[DigitGroupStyle] =
      raw match {
        case NoSeparators(_, _) => None
        case WithSeparators(sep, digitGroups, _) => Some(DigitGroupStyle(sep, groupSizes(digitGroups)))
      }

    for {
      (quantity, precision) <- toQuantity(digitGroup(raw), decimalGroup(raw))
    } yield {
      (quantity, precision, mDecPt(raw), digitGroupStyle(raw))
    }
  }

  val interpretNumber:
    ((Int, Int), Either[AmbiguousNumber, RawNumber])
      => Parsley[(BigDecimal, Precision, Option[Char], Option[DigitGroupStyle])] =
    (numRegion: (Int, Int), ambiguousRawNum: Either[AmbiguousNumber, RawNumber]) => {
      val rawNum = ambiguousRawNum.fold(disambiguateNumber, identity)
      fromRawNumber(rawNum) match {
        case Left(errormsg) => fail(errormsg)
        case Right((q, p, d, g)) => Parsley.pure(q, Precision(p), d, g)
      }
    }
  
  val simpleamountp: Parsley[Amount] = {
    val rightornosymbolamountp = (sign: BigDecimal => BigDecimal) => {
      (offset, rawnumberp, offset, option(attempt((skipNonNewlineSpacesb, commoditysymbolp).zipped)))
        .zipped.flatMap { case (offBeforeNum, ambiguousRawNum, offAfterNum, mSpaceAndCommodity) =>
        val numRegion = (offBeforeNum, offAfterNum)
        Parsley.join(mSpaceAndCommodity match {
          case Some((commodityspaced, c)) =>
            interpretNumber(numRegion, ambiguousRawNum).map { case (q, prec, mdec, mgrps) =>
              val s = amountstyle.copy(side = R,
                commodityspaced = commodityspaced,
                precision = prec,
                decimalpoint = mdec,
                digitgroups = mgrps
              )
              Parsley.pure(nullamount.copy(commodity = c, quantity = sign(q), style = s))
            }
          case None =>
            interpretNumber(numRegion, ambiguousRawNum).map { case (q, prec, mdec, mgrps) =>
              val (c, s) = ("", amountstyle.copy(precision = prec, decimalpoint = mdec, digitgroups = mgrps))
              Parsley.pure(nullamount.copy(commodity = c, quantity = sign(q), style = s))
            }
        })
      }
    }
    signp.flatMap(sign => rightornosymbolamountp(sign))
  }
  
  val amountnobasisp: Parsley[Amount] = {
    simpleamountp <* skipNonNewlineSpaces //todo add cost?
  }
  val amountp: Parsley[Amount] = {
    val spaces = skipNonNewlineSpaces
    simpleamountp <* spaces
  }

//  val balanceassertionp: Parsley[BalanceAssertion] = { //todo re-add later
//    (pos,
//      char('='),
//      option(attempt(char('='))).map(_.isDefined),
//      option(attempt(char('*'))).map(_.isDefined),
//      skipNonNewlineSpaces,
//      amountnobasisp)
//      .zipped { (sourcepos, _, isTotal, isInclusive, _, amount) =>
//        BalanceAssertion(amount, isTotal, isInclusive, sourcepos)
//      }
//  }
}
