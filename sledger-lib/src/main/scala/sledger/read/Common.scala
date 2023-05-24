package sledger.read

import cats._
import cats.derived._
import cats.syntax.all._
import parsley.Parsley
import parsley.Parsley.{attempt, join, lookAhead, notFollowedBy, pure}
import parsley.cats.instances._
import parsley.character._
import parsley.combinator._
import parsley.errors.combinator._
import parsley.implicits.zipped._
import parsley.position.offset
import sledger._
import sledger.data.AccountNames.AccountName
import sledger.data.Amounts._
import sledger.data.Dates._
import sledger.utils.Parse._

import java.time.LocalDate
import scala.Function.const
import scala.util.Try

object Common {

  private def readDecimal(s: String): Int = {
    s.foldLeft(0) { (a, c) =>
      a * 10 + c.asDigit
    }
  }

  val natural: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)

  val emptyorcommentlinep: Parsley[Unit] = {
    val skiplinecommentp: Parsley[Unit] = {
      satisfy(isLineCommentStart) *> takeWhileP(_ != '\n') *> option(endOfLine)
    }.void
    skipNonNewlineSpaces *> skiplinecommentp <|> endOfLine.void
  }

  val multilineCommentp: Parsley[Unit] = {
    val trailingSpaces = skipNonNewlineSpaces <* newline
    val startComment = attempt(string("comment")) *> trailingSpaces
    val endComment = eof <|> attempt(string("end comment")) *> trailingSpaces
    val anyLine = takeWhileP(_ != '\n') *> newline.void
    startComment *> manyUntil(anyLine, endComment).void
  }

  val yearp: Parsley[Int] = takeWhileP1(_.isDigit).map(readDecimal)

  val datep: Parsley[LocalDate] = {
    (yearp, dateSepChar, natural, dateSepChar, natural)
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

  val followingCommentp_ : Parsley[String] => Parsley[CommoditySymbol] = (contentp: Parsley[String]) => {
    val headerp = char(';') *> skipNonNewlineSpaces
    (skipNonNewlineSpaces,
      attempt(headerp) *> contentp.map(a => a :: Nil) <|> pure(List[String]()),
      eolof,
      many(attempt(skipNonNewlineSpaces1 *> headerp) *> contentp <* eolof))
      .zipped { (_, sameLine, _, nextLines) =>
        val sameLine_ = if (sameLine.isEmpty && nextLines.nonEmpty) {
          List()
        } else sameLine
        (sameLine_ ++ nextLines).map(_.strip()).mkString("", "\n", "\n")
      }
  }
  val followingCommentp: Parsley[String] = followingCommentp_(takeWhileP(a => a != '\n'))
  val transactionCommentp: Parsley[String] = followingCommentp
  val nonCommentTextp: Parsley[String] = takeWhileP(c => !isSameLineCommentStart(c) && !isNewline(c))
    .map(_.strip)

  val descriptionp: Parsley[String] = nonCommentTextp
  val statusp: Parsley[Status] = {
    attemptChoice(
      skipNonNewlineSpaces *> char('*') #> Cleared,
      skipNonNewlineSpaces *> char('!') #> Pending,
      pure(()) #> Unmarked
    ).label("status")
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
    ((char('-') #> sign <|> char('+') #> idD) <* skipNonNewlineSpaces) <|> pure(idD)
  }
  
  val singleSpacep: Parsley[Unit] = spacenonewline *> notFollowedBy(spacenonewline)
  
  val singleSpacedTextSatisfying1p: (Char => Boolean) => Parsley[String] = (f: Char => Boolean) => {
    val partp = takeWhileP1(c => f(c) && !isWhitespace(c))

    for {
      firstPart <- partp
      otherParts <- many(attempt(singleSpacep *> partp))
    } yield (firstPart +: otherParts).mkString(" ")
  }
  
  val singleSpacedText1p: Parsley[String] = singleSpacedTextSatisfying1p(const(true))
  
  val accountNamep: Parsley[AccountName] = singleSpacedText1p

  sealed trait RawNumber

  case class NoSeparators(digitGrp: DigitGroup,
                          mDecimals: Option[(Char, DigitGroup)]) extends RawNumber

  case class WithSeparators(sep: Char,
                            digitGroups: List[DigitGroup],
                            mDecimals: Option[(Char, DigitGroup)]) extends RawNumber

  implicit val showRawNumber: Show[RawNumber] = Show.show({
    case NoSeparators(digitGrp, leadingOrTrailingSeparator) => s"NoSeparators ${digitGrp.show} ${leadingOrTrailingSeparator.show}"
    case WithSeparators(sep, digitGroups, leadingOrTrailingSeparator) => s"WithSeparators '$sep' ${digitGroups.show} ${leadingOrTrailingSeparator.show}"
  })

  implicit val showNoSeparator: Show[NoSeparators] = semiauto.show

  case class DigitGroup(digitGroupLength: Int, digitGroupNumber: BigInt)

  implicit val eqDigitGroup: Eq[DigitGroup] = semiauto.eq
  implicit val showDigitGroup: Show[DigitGroup] = (d: DigitGroup) => {
    val numStr = d.digitGroupNumber.show
    val padding = List.fill(d.digitGroupLength - numStr.length)('0')
    "\"" ++ padding.mkString ++ numStr ++ "\""
  }

  implicit val DigitGroupMonoid: Monoid[DigitGroup] = new Monoid[DigitGroup] {
    override def empty: DigitGroup = DigitGroup(0, 0)

    override def combine(x: DigitGroup, y: DigitGroup): DigitGroup =
      (x, y) match {
        case (DigitGroup(l1, n1), DigitGroup(l2, n2)) => DigitGroup(l1 + l2, n1 * BigInt(10).pow(l2) + n2)
      }
  }
 

  case class AmbiguousNumber(group1: DigitGroup, sep: Char, group2: DigitGroup)

  implicit val ShowAmbiguousNumber: Show[AmbiguousNumber] = (n: AmbiguousNumber) => s"AmbiguousNumber ${n.group1.show}${n.sep}${n.group2.show}"

  val digitgroupp: Parsley[DigitGroup] = {
    takeWhileP1(_.isDigit)
      .map { s =>
        DigitGroup.tupled.apply(s.foldLeft((0, BigInt(0))) { case ((l, a), c) =>
          (l + 1, a * 10 + BigInt(c.asDigit))
        })
      }
  }

  val rawnumberp: Parsley[Either[AmbiguousNumber, RawNumber]] = {
    val trailingDecimalPt = (grp1: DigitGroup) => {
      satisfy(isDecimalMark).map(decPt => NoSeparators(grp1, Some((decPt, Monoid[DigitGroup].empty))))
    }

    val withDecimalPt = (digitSep: Char, digitGroups: List[DigitGroup]) => {

      for {
        decPt <- satisfy(c => isDecimalMark(c) && c != digitSep)
        decDigitGrp <- digitgroupp <|> pure[DigitGroup](Monoid[DigitGroup].empty)
      } yield {
        WithSeparators(digitSep, digitGroups, Some((decPt, decDigitGrp)))
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
      join(attempt((satisfy(isDigitSeparatorChar), digitgroupp).zipped { (sep, grp2) =>
        join(for {
          grps <- many(attempt(char(sep) *> digitgroupp))
          digitGroups = grp1 :: grp2 :: grps
        } yield {
          withDecimalPt(sep, digitGroups).map(Right(_)) <|> pure(withoutDecimalPt(grp1, sep, grp2, grps))
        })
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

    for {
      rawNumber <- leadingDecimalPt.map(Right(_)) <|> leadingDigits
      mExtraDecimalSep <- option(lookAhead(satisfy(isDecimalMark)))
      _ <- Applicative[Parsley].whenA(mExtraDecimalSep.isDefined) {
        fail("invalid number (invalid use of separator)")
      }
      mExtraFragment <- option(lookAhead(attempt(char(' ') *> offset <* digit)))
      _ <- mExtraFragment match {
        case Some(_) => fail("invalid number (excessive trailing digits)") // todo? parseError at
        case None => pure(())
      }
    } yield rawNumber
  }

  val quotedCommoditySymbolp: Parsley[String] = between(char('"'), char('"'), takeWhileP { c =>
    c != ';' && c != '\n' && c != '\"'
  })

  val simpleCommoditySymbolp: Parsley[String] = takeWhileP1(c => !isNonsimpleCommodityChar(c))
  val commoditySymbolp: Parsley[String] =
    quotedCommoditySymbolp <|> simpleCommoditySymbolp

  val disambiguateNumber: AmbiguousNumber => RawNumber = {
    case AmbiguousNumber(group1, sep, group2) =>
      if (isDecimalMark(sep)) {
        NoSeparators(group1, Some(sep, group2))
      } else
        WithSeparators(sep, List(group1, group2), None)
  }

  val fromRawNumber: RawNumber => Either[String, (BigDecimal, Int, Option[Char], Option[DigitGroupStyle])] = (raw: RawNumber) => {
    def toQuantity(preDecimalGrp: DigitGroup, posDecimalGrp: DigitGroup): Either[String, (BigDecimal, Int)] = {
      val digitGrpNum = (preDecimalGrp |+| posDecimalGrp).digitGroupNumber
      val precision = posDecimalGrp.digitGroupLength - 0
      if (precision < 0) {
        Right(BigDecimal(0, digitGrpNum.toInt * 10 ^ (-precision)), 0)
      } else if (precision < 255) {
        Right(BigDecimal(digitGrpNum, precision), precision)
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
      case NoSeparators(digitGrp, _) => digitGrp
      case WithSeparators(_, digitGroups, _) => digitGroups.combineAll
    }

    def groupSizes(digitGroups: List[DigitGroup]): List[Int] = digitGroups.map(_.digitGroupLength) match {
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
    (_: (Int, Int), ambiguousRawNum: Either[AmbiguousNumber, RawNumber]) => {
      val rawNum = ambiguousRawNum.fold(disambiguateNumber, { a => identity(a) })
      fromRawNumber(rawNum) match {
        case Left(errormsg) => fail(errormsg)
        case Right((q, p, d, g)) => pure(q, Precision(p), d, g)
      }
    }

  val simpleAmountp: Parsley[Amount] = {

    val leftSymbolAmountp = (sign: BigDecimal => BigDecimal) => {
      (commoditySymbolp, skipNonNewlineSpacesb, signp, offset, rawnumberp, offset).zipped.flatMap {
        case (c, spaced, sign2, offsetBefore, ambiguousRawNum, offsetAfter) =>
          val numRegion = (offsetBefore, offsetAfter)
          join(interpretNumber(numRegion, ambiguousRawNum).map {
            case (q, prec, mdec, mgrps) =>
              val s = amountstyle.copy(side = L,
                commodityspaced = spaced,
                precision = prec,
                decimalpoint = mdec,
                digitgroups = mgrps
              )
              pure(nullamount.copy(commodity = c, quantity = sign(sign2(q)), style = s))
          })
      }
    }

    val rightOrNoSymbolAmountp = (sign: BigDecimal => BigDecimal) => {
      (offset, rawnumberp, offset, option(attempt((skipNonNewlineSpacesb, commoditySymbolp).zipped)))
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
              pure(nullamount.copy(commodity = c, quantity = sign(q), style = s))
            }
          case None =>
            interpretNumber(numRegion, ambiguousRawNum).map { case (q, prec, mdec, mgrps) =>
              val (c, s) = ("", amountstyle.copy(precision = prec, decimalpoint = mdec, digitgroups = mgrps))
              pure(nullamount.copy(commodity = c, quantity = sign(q), style = s))
            }
        })
      }
    }
    signp.flatMap(sign => leftSymbolAmountp(sign) <|> rightOrNoSymbolAmountp(sign))
  }
  
  val amountp: Parsley[Amount] = {
    val spaces = skipNonNewlineSpaces
    simpleAmountp <* spaces
  }
}
