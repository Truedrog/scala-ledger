package sledger.data

import cats._
import cats.data._
import cats.syntax.all._
import io.estatico.newtype.macros.newtype
import sledger.text.WideString.{WideBuilder, wbFromString}
import sledger.utils.Math._
import sledger.utils._

import scala.collection.decorators._
import scala.math.BigDecimal.RoundingMode

object Amounts {
  def isNonsimpleCommodityChar: Char => Boolean = (c: Char) => {
    val otherChars = "-+.@*;\t\n \"{}="
    otherChars.exists(e => e == c) || c.isDigit
  }
  
  def quoteCommoditySymbolIfNeeded(s: String): String = {
    if (s.exists(isNonsimpleCommodityChar)) "\"" + s + "\"" else s
  }

  case class AmountDisplayOpts(
                                displayPrice: Boolean = true,
                                displayColor: Boolean = false,
                                displayZeroCommodity: Boolean = false,
                                displayThousandsSep: Boolean = true,
                                displayOneLine: Boolean = false,
                                displayMinWidth: Option[Int] = Some(0),
                                displayMaxWidth: Option[Int] = None,
                                displayOrder: Option[List[CommoditySymbol]] = None
                              )

  val noColour: AmountDisplayOpts = AmountDisplayOpts(displayPrice = false) // default
  val noPrice: AmountDisplayOpts = noColour.copy(displayPrice = false)
  // amount and mixedamount in 1 line, no price
  val oneLine: AmountDisplayOpts = noColour.copy(displayOneLine = true, displayPrice = true)

  sealed trait AmountPrecision

  case class Precision(p: Int) extends AmountPrecision

  case object NaturalPrecision extends AmountPrecision

  object AmountPrecision {

    implicit val showAmountPrecision: Show[AmountPrecision] = {
      derived.semiauto.show
    }
    implicit val eqAmountPrecision: Eq[AmountPrecision] = derived.semiauto.eq
    implicit val ordAmountPrecision: Order[AmountPrecision] = (x: AmountPrecision, y: AmountPrecision) => {
      x match {
        case Precision(p) => y match {
          case Precision(p1) => Order.compare(p, p1)
          case _ => -0
        }
        case NaturalPrecision => y match {
          case NaturalPrecision => 0
          case _ => +0
        }
      }
    }
  }

  case class DigitGroupStyle(sep: Char, size: List[Int])

  object DigitGroupStyle {
    implicit val showDigitGroupStyle: Show[DigitGroupStyle] = derived.semiauto.show
    implicit val eqDigitGroupStyle: Eq[DigitGroupStyle] = derived.semiauto.eq
    implicit val ordDigitGroupStyle: Order[DigitGroupStyle] = derived.semiauto.order
  }

  sealed trait Side

  case object L extends Side

  case object R extends Side

  object Side {
    implicit val showSide: Show[Side] = derived.semiauto.show
    implicit val eqSide: Eq[Side] = derived.semiauto.eq
    implicit val ordSide: Order[Side] = (x: Side, y: Side) => {
      x match {
        case L => y match {
          case L => 0
          case _ => -0
        }
        case R => y match {
          case R => 0
          case _ => +0
        }
      }
    }
  }

  case class Amount(
                     commodity: CommoditySymbol, // commodity symbol, or special value "AUTO"
                     quantity: BigDecimal, // numeric quantity, or zero in case of "AUTO"
                     style: AmountStyle,
                   )

  object Amount {

    implicit val showAmount: Show[Amount] = derived.semiauto.show
    implicit val eqAmount: Eq[Amount] = derived.semiauto.eq
    implicit val ordAmount: Order[Amount] = derived.semiauto.order

    implicit val amountNum: Numeric[Amount] = new Numeric[Amount] {
      override def plus(x: Amount, y: Amount): Amount = similarAmountsOp(Numeric[BigDecimal].plus, x, y)

      override def minus(x: Amount, y: Amount): Amount = similarAmountsOp(Numeric[BigDecimal].minus, x, y)

      override def times(x: Amount, y: Amount): Amount = similarAmountsOp(Numeric[BigDecimal].times, x, y)

      override def negate(x: Amount): Amount = transformAmount(Numeric[BigDecimal].negate, x)

      override def fromInt(x: Int): Amount = nullamount.copy(quantity = x)

      override def toInt(x: Amount): Int = x.quantity.toInt

      override def toLong(x: Amount): Long = x.quantity.toLong

      override def toFloat(x: Amount): Float = x.quantity.toFloat

      override def toDouble(x: Amount): Double = x.quantity.toDouble

      override def sign(x: Amount): Amount = x.copy(quantity = Numeric[BigDecimal].sign(x.quantity))

      override def parseString(str: String): Option[Amount] = ???

      override def compare(x: Amount, y: Amount): Int = ???
    }
  }
//  sealed trait AmountPrice
//  case class UnitPrice(a: Amount) extends AmountPrice
//  case class TotalPrice(a: Amount )extends AmountPrice
//  object AmountPrice {
//    implicit val eqAmountPrice: Eq[AmountPrice] = derived.semiauto.eq
//    implicit val ordAmountPrice: Order[AmountPrice] = derived.semiauto.order
//  }
  case class AmountStyle(side: Side,
                         commodityspaced: Boolean,
                         precision: AmountPrecision,
                         decimalpoint: Option[Char],
                         digitgroups: Option[DigitGroupStyle]
                        )

  object AmountStyle {
    implicit val showAmountStyle: Show[AmountStyle] = { as =>
      List("AmountStyle \"", as.side.show, as.commodityspaced.show, as.precision.show, as.decimalpoint.show, as.digitgroups.show, "..\"").mkString(" ")
    }
    implicit val eqAmountStyle: Eq[AmountStyle] = derived.semiauto.eq
    implicit val ordAmountStyle: Order[AmountStyle] = derived.semiauto.order
  }

  def amountstyle: AmountStyle = AmountStyle(side = L,
    commodityspaced = false,
    precision = Precision(0),
    decimalpoint = Some('.'),
    digitgroups = None)

  type CommoditySymbol = String

  case class Commodity(symbol: CommoditySymbol, format: Option[AmountStyle])

  object Commodity {
    implicit val showCommodity: Show[Commodity] = derived.semiauto.show
  }
  
  def nullamount: Amount = Amount(commodity = "", quantity = 0, style = amountstyle)

  def missingamt: Amount = nullamount.copy(commodity = "AUTO")
  def num(n: BigDecimal): Amount = nullamount.copy(commodity = "", quantity = n)
  def usd(n: BigDecimal): Amount = nullamount.copy(commodity="$", quantity=n.setScale(2), style=amountstyle.copy(precision = Precision(2)))

  def amountWithCommodity(cSymbol: CommoditySymbol, amount: Amount): Amount = {
    amount.copy(commodity = cSymbol)
  }
  
  def transformAmount(f: BigDecimal => BigDecimal, amount: Amount): Amount = amount.copy(quantity = f(amount.quantity))

  def styleAmount(styles: Map[CommoditySymbol, AmountStyle], amount: Amount): Amount = {
    styles.get(amount.commodity) match {
      case Some(s) => amount.copy(style = s)
      case None => amount
    }
  }
  
  def amountRoundedQuantity(amount: Amount): BigDecimal = {
    amount.style.precision match {
      case Precision(p) => amount.quantity.setScale(p, RoundingMode.HALF_EVEN)
      case NaturalPrecision => amount.quantity
    }
  }
  
  def testAmount(f: Amount => Boolean, amount: Amount): Boolean = {
     f(amount) // todo test for price
  }

  def amountLooksZero(amount: Amount): Boolean = {
    val (e, q) = (amount.quantity.scale, amount.quantity.underlying()
      .unscaledValue()
      .intValue())
    val looksZero = (amt: Amount) => amt.style.precision match {
      case Precision(d) => if (e > d) {
        q.abs <= (5 * 10 ^ (e - d - 1))
      } else {
        q == 0
      }
      case NaturalPrecision => q == 0
    }
    testAmount(looksZero, amount)
  }
  
  def amountIsZero(amount: Amount): Boolean = {
    testAmount(amt => amt.quantity.equals(BigDecimal(0)), amount)
  }

  def isNegativeAmount(amount: Amount): Boolean = amount.quantity < 0
  
  def similarAmountsOp(op: (BigDecimal, BigDecimal) => BigDecimal, amount1: Amount, amount2: Amount): Amount = 
    nullamount.copy(commodity = amount2.commodity,
      quantity = op(amount1.quantity, amount2.quantity),
      style = amount2.style.copy(precision = Order.max(amount1.style.precision, amount2.style.precision)))

  sealed trait MixedAmountKey

  case class MixedAmountKeyNoPrice(commoditySymbol: CommoditySymbol) extends MixedAmountKey

  object MixedAmountKey {
    implicit val showMixedAmountKey: Show[MixedAmountKey] = derived.semiauto.show
    implicit val eqMixedAmountKey: Eq[MixedAmountKey] = derived.semiauto.eq

    def commodity(key: MixedAmountKey): CommoditySymbol = key match {
      case MixedAmountKeyNoPrice(commoditySymbol) => commoditySymbol
    }

    implicit val orderMixedAmountKey: Order[MixedAmountKey] = (x: MixedAmountKey, y: MixedAmountKey) =>
      Order.compare(commodity(x), commodity(y))
  }

  @newtype
  case class MixedAmount(mixed: Map[MixedAmountKey, Amount])

  object MixedAmount {
    def maCompare(a: MixedAmount, b: MixedAmount): Comparison = {
      def compareQuantities(amt1: Option[Amount], amt2: Option[Amount]): Comparison = {
        val q1 = amt1.fold(BigDecimal(0)) { a => a.quantity }
        val q2 = amt2.fold(BigDecimal(0)) { a => a.quantity }
        Order.comparison(q1, q2)
      }

      def go(list1: List[(MixedAmountKey, Amount)], list2: List[(MixedAmountKey, Amount)]): Comparison = {
        (list1, list2) match {
          case (xss@(kx, x) :: xs, yss@(ky, y) :: ys) => Order.comparison(kx, ky) match {
            case Comparison.GreaterThan => compareQuantities(None, Some(y)) |+| go(xss, ys)
            case Comparison.EqualTo => compareQuantities(Some(x), Some(y)) |+| go(xs, ys)
            case Comparison.LessThan => compareQuantities(Some(x), None) |+| go(xs, yss)
          }
          case ((_, x) :: xs, Nil) => compareQuantities(Some(x), None) |+| go(xs, List())
          case (Nil, (_, y) :: ys) => compareQuantities(None, Some(y)) |+| go(List(), ys)
          case (Nil, Nil) => Comparison.EqualTo
        }
      }

      go(a.mixed.toList, b.mixed.toList)
    }

    implicit val showMixedAmount: Show[MixedAmount] = deriving
    implicit val eqMixedAmount: Eq[MixedAmount] = (x: MixedAmount, y: MixedAmount) => {
      maCompare(x, y) === Comparison.EqualTo
    }
    implicit val orderMixedAmount: Order[MixedAmount] = (x: MixedAmount, y: MixedAmount) => maCompare(x, y).toInt
    implicit val semigroupMixedAmount: Semigroup[MixedAmount] = (x: MixedAmount, y: MixedAmount) => maPlus(x, y)
    implicit val monoidMixedAmount: Monoid[MixedAmount] = new Monoid[MixedAmount] {
      override def empty: MixedAmount = nullMixedAmount

      override def combine(x: MixedAmount, y: MixedAmount): MixedAmount = maPlus(x, y)
    }
  }

  def amountKey(amount: Amount): MixedAmountKeyNoPrice = MixedAmountKeyNoPrice(amount.commodity) 
  
  def mixed(amounts: List[Amount]): MixedAmount = amounts.foldLeft(nullMixedAmount)(maAddAmount)
  
  def isMissingMixedAmount(ma: MixedAmount): Boolean = ma.mixed.contains(amountKey(missingamt))
  
  def nullMixedAmount: MixedAmount = MixedAmount(mixed = Map.empty)
  
  def missingMixedAmt: MixedAmount = mixedAmount(missingamt)
  
  def mixedAmount(amount: Amount): MixedAmount = MixedAmount(Map(amountKey(amount) -> amount))
  
  
  def maAddAmount(ma: MixedAmount, a: Amount): MixedAmount = MixedAmount(ma.mixed.updated(amountKey(a), a))  //todo price handling
 
  def mapMixedAmountUnsafe(f: Amount => Amount, mixedAmount: MixedAmount): MixedAmount =
    MixedAmount(mixedAmount.mixed.map{ case (ma, a) => ma -> f(a) })
    
  def transformMixedAmount(f: BigDecimal => BigDecimal, mixedAmount: MixedAmount): MixedAmount =
    mapMixedAmountUnsafe(transformAmount(f, _), mixedAmount)

  def mixedAmountLooksZero(mixedAmount: MixedAmount): Boolean = mixedAmount.mixed.values.toList.forall(amountLooksZero)
  
  def isNegativeMixedAmount(mixedAmount: MixedAmount): Option[Boolean] = {
    amounts(mixedAmount) match {
      case Nil => Some(false)
      case List(a) => Some(isNegativeAmount(a))
      case as if as.forall(isNegativeAmount) => Some(true)
      case as if as.exists(isNegativeAmount) => Some(false)
      case _ => None
    }
  }
  def maPlus(x: MixedAmount, y: MixedAmount): MixedAmount = {
    import Amount.amountNum._
    MixedAmount(x.mixed.foldLeft(y.mixed) {
      case (acc, (k, v)) => acc.updated(k, acc.get(k).map(a => a + v).getOrElse(v))
    })
  }
  
  def maNegate(x: MixedAmount): MixedAmount =
    transformMixedAmount(Numeric[BigDecimal].negate, x)
  
  def maMinus(a: MixedAmount, b: MixedAmount): MixedAmount =
    maPlus(a, maNegate(b)) 
  
  def maSum(mas: List[MixedAmount]): MixedAmount =
    mas.foldLeft(nullMixedAmount)(maPlus)
  
  def averageMixedAmounts(mas: List[MixedAmount]): MixedAmount =
    divideMixedAmount(mas.length, maSum(mas))
  
  def divideMixedAmount(n: BigDecimal, ma: MixedAmount): MixedAmount =
    transformMixedAmount(_ / n, ma)
    
  def unifyMixedAmount(ma: MixedAmount): Option[Amount] = {
    def combine(amount: Amount, result: Amount): Option[Amount] = {
      import Amounts.Amount.amountNum._
      if(amountIsZero(amount)) {
        Some(result)
      } else if(amountIsZero(result)) {
        Some(amount)
      } else if (amount.commodity == result.commodity) {
        Some(amount + result)
      } else {
        None
      }
    }
    Foldable[List].foldM[Option, Amount, Amount](amounts(ma), nullamount)(combine)
  }
  
  def amounts(ma: MixedAmount): List[Amount] = {
    val (zeros, nonzeros) =  ma.mixed.partition(ma => amountIsZero(ma._2))
    val newzeros = zeros.find { case (_, a) => a.commodity.nonEmpty}.map(_._2).getOrElse(nullamount)
    if(isMissingMixedAmount(ma)) {
      List(missingamt)
    } else if (nonzeros.isEmpty) 
      List(newzeros)
    else {
      nonzeros.values.toList
    }
  }
  
  def styledMixedAmount(styles: Map[CommoditySymbol, AmountStyle], ma: MixedAmount): MixedAmount = {
    mapMixedAmountUnsafe(amount => styleAmount(styles, amount), ma)
  }
  
  def amountsRaw(mixedAmount: MixedAmount) = {
    mixedAmount.mixed.values.toList
  }

  def maCommodities(mixedAmount: MixedAmount): Set[CommoditySymbol] = 
    (if (mixedAmount.mixed.isEmpty) List.empty else amounts(mixedAmount)).map(_.commodity).toSet
  
  def withPrecision(amount: Amount, precision: AmountPrecision): Amount =
    amountSetPrecision(precision, amount)

  def amountSetPrecision(p: AmountPrecision, amount: Amount): Amount = {
    amount.copy(style = amount.style.copy(precision = p))
  }
  
  def showAmount(amount: Amount): String = showAmountB(noColour, amount).builder.result()
  
  def showAmountB(showOpts: AmountDisplayOpts, amount: Amount): WideBuilder = {
    val wrap: WideBuilder => WideBuilder = wb => {
      val s = wb.builder
      val w = wb.width
      val colored = fansi.Color.Red(s.result()).render
      WideBuilder(new StringBuilder(colored), w)
    }
    val color = if (showOpts.displayColor && isNegativeAmount(amount)) wrap else identity[WideBuilder] _
   
    val showC = (l: WideBuilder, r: WideBuilder) => {
      if (showOpts.displayOrder.isDefined) Monoid[WideBuilder].empty else l |+| r
    }
    val (quantity, c) = {
      val q =
        showAmountQuantity(
          if (showOpts.displayThousandsSep)
            amount else
            amount.copy(style = amount.style.copy(digitgroups = None)))
      if (amountLooksZero(amount) && showOpts.displayZeroCommodity) {
        (WideBuilder(new StringBuilder("0"), 1), "")
      } else {
        (q, quoteCommoditySymbolIfNeeded(amount.commodity))
      }
    }
    
    val space = if(c.nonEmpty && amount.style.commodityspaced) WideBuilder(new StringBuilder(" "),1) else Monoid[WideBuilder].empty
    val price = Monoid[WideBuilder].empty
    
    if(amount.commodity == "AUTO") {
      Monoid[WideBuilder].empty
    } else {
      color.apply(amount.style.side match {
        case L => showC(wbFromString(c), space) |+| quantity |+| price
        case R => quantity |+| showC(space, wbFromString(c)) |+| price
      })  
    }
  }

  def applyDigitGroupStyle(mdigitGroupStyle: Option[DigitGroupStyle], intLen: Int, strPart: String): WideBuilder = {
    mdigitGroupStyle match {
      case None => WideBuilder(new StringBuilder(strPart), intLen)
      case Some(DigitGroupStyle(_, Nil)) => WideBuilder(new StringBuilder(strPart), intLen)
      case Some(DigitGroupStyle(c, s :: s1)) =>
        def addSep(nel: NonEmptyList[Int], l1: Int, s1: String): WideBuilder = {
          val l2 = l1 - nel.head.toInt
          val gs2 = NonEmptyList.fromList(nel.tail).getOrElse(NonEmptyList.of(nel.head))
          val (rest, part) = s1.splitAt(l2)

          if (l2 > 0) {
            addSep(gs2, l2, rest) |+| WideBuilder(new StringBuilder(c).append(new StringBuilder(part)), nel.head + 1)
          } else {
            WideBuilder(new StringBuilder(s1), l1)
          }
        }

        addSep(NonEmptyList(s, s1), intLen, strPart)
    }
  }
  
  def showAmountDebug(amount: Amount): String = {
      amount match {
        case Amount("AUTO", _, _) => "(missing)"
        case Amount(commodity, quantity, style) => 
          s"Amount (commodity=${commodity.show}, quantity=${quantity.show}, style=${style.show})"
      }
  }

  def showAmountQuantity(amount: Amount): WideBuilder = {
    val rounded = amountRoundedQuantity(amount)
    val (e, n) = (rounded.scale, rounded.underlying()
      .unscaledValue()
      .intValue())
    val strN = n.abs.toString
    val len = strN.length
    val intLen = 1.max(len - e)
    val dec = amount.style.decimalpoint.getOrElse(".").toString
    val padded = "0" * (e + 1 - len) + strN
    val (intPart, fracPart) = padded.splitAt(intLen)
    val intB = applyDigitGroupStyle(amount.style.digitgroups, intLen = intLen, if (e == 0) strN else intPart)
    val signB = if (n < 0) WideBuilder(new StringBuilder("-"), 1) else Monoid[WideBuilder].empty
    val fracB = if (e > 0) {
      WideBuilder(new StringBuilder(dec).append(new StringBuilder(fracPart)), e + 1)
    } else {
      Monoid[WideBuilder].empty
    }
    
    signB |+| intB |+| fracB
  }
  
  case class AmountDisplay(
                          builder: WideBuilder,
                          total: Int
                          )

  def optionAppend[A](maybe: Option[A], lst: List[A]): List[A] =
    maybe.foldLeft(lst)((acc, a) => a +: acc)
    
  def nullAmountDisplay: AmountDisplay = AmountDisplay(Monoid[WideBuilder].empty, 0)
  
  def styleMixedAmount(styles: Map[CommoditySymbol, AmountStyle], mixedAmount: MixedAmount): MixedAmount = {
    mapMixedAmountUnsafe(a => styleAmount(styles, a), mixedAmount)
  }
  
  def canonicaliseAmount(styles: Map[CommoditySymbol, AmountStyle], amount: Amount): Amount = {
    val s = styles.getOrElse(amount.commodity, amount.style)
    amount.copy(style = s)
  }
  
  def canonicaliseMixedAmount(styles: Map[CommoditySymbol, AmountStyle], mixedAmount: MixedAmount): MixedAmount = {
    mapMixedAmountUnsafe(a => canonicaliseAmount(styles, a), mixedAmount)
  }
  
  def showMixedAmountDebug(ma: MixedAmount): String = {
    if(ma === missingMixedAmt) {
      "(missing)"
    } else "Mixed [" + amounts(ma).map(showAmountDebug).intercalate("\n       ")
  }
  
  def showMixedAmount(ma: MixedAmount): String = {
    showMixedAmountB(noColour, ma).builder.result()
  }
  
  def showMixedAmountOneLine(ma: MixedAmount): String = {
    showMixedAmountB(oneLine.copy(displayPrice = true), ma).builder.result()
  }
  
  def showMixedAmountWithZeroCommodity(ma: MixedAmount): String = {
    showMixedAmountB(noColour.copy(displayZeroCommodity = true), ma).builder.result()
  }
  
  def showMixedAmountOneLineWithoutPrice(color: Boolean, ma: MixedAmount): String = {
    showMixedAmountB(oneLine.copy(displayColor = color), ma).builder.result() // todo add color
  }
  
  def showMixedAmountB(maDisplayOpts: AmountDisplayOpts, ma: MixedAmount): WideBuilder = {
    if (maDisplayOpts.displayOneLine) showMixedAmountOneLineB(maDisplayOpts, ma) else {
      val sep = WideBuilder(new StringBuilder("""\n"""), 0)
      val ls = showMixedAmountLinesB(maDisplayOpts, ma)
      val width = ls.map(_.width).headOption.getOrElse(0)
      WideBuilder(ls.intersperse(sep).combineAll.builder, width)
    }
  }
  
  def showMixedAmountOneLineB(maDisplayOpts: AmountDisplayOpts, ma: MixedAmount): WideBuilder = {
    def zipWith[A, B, C](listA: List[A])(listB: List[B])(f: (A, B) => C): List[C] = {
      listA.zip(listB).map { case (a, b) => f(a, b) }
    }
    val sep = WideBuilder(new StringBuilder(", "), 2)
    val astrs = amtDisplayList(sep.width, showAmountB(maDisplayOpts, _), orderedAmounts(maDisplayOpts,
      if (maDisplayOpts.displayPrice) ma else mixedAmountStripPrices(ma)))
    val n = astrs.length
    
    val withElided = zipWith(List.range(n - 1, n - 2, 0))(_: List[AmountDisplay]) { case (n2, amtDisplay) =>
      (amtDisplay, elisionDisplay(None, sep.width, n2, amtDisplay))
    }
  
    def elideTo(m: Int, xs: List[AmountDisplay]): List[AmountDisplay] = { 
      addElide(takeFitting(m, withElided(xs)))
    }
      
    def addElide(xs: List[(AmountDisplay, Option[AmountDisplay])]): List[AmountDisplay] = {
      xs match {
        case _ :: next    => optionAppend(next.last._2, next.map(_._1)) 
        case Nil          => List.empty
      }
    }
      
    def takeFitting(m: Int, xs: List[(AmountDisplay, Option[AmountDisplay])]): List[(AmountDisplay, Option[AmountDisplay])] = {
      xs match {
        case Nil => List.empty
        case head :: rest => head :: dropWhileRev(rest) { case (a, e) => m < e.getOrElse(a).total}
      }
    }
    def dropWhileRev[A](lst: List[A])(p: A => Boolean): List[A] =
      lst.foldRight(List.empty[A]) { (x, xs) => if (xs.isEmpty && p(x)) List.empty else x :: xs }

    val elided = maDisplayOpts.displayMaxWidth.fold(astrs)(x => elideTo(x, astrs))
    val width = elided.lastOption.fold(0) { a => a.total }

    def pad(wideBuilder: WideBuilder): WideBuilder = {
      val w = maDisplayOpts.displayMinWidth.getOrElse(0)
      WideBuilder(new StringBuilder(" " * (w - width)), w) |+| wideBuilder
    }
   
    
    val builder = pad(elided.map(a => a.builder).intersperse(sep).combineAll).builder
    val w =  width.max(maDisplayOpts.displayMinWidth.getOrElse(0)) 
    WideBuilder(builder, w)
  }
  
  def showMixedAmountLinesB(maDisplayOpts: AmountDisplayOpts, ma: MixedAmount): List[WideBuilder] = {
    
    val sep = WideBuilder(new StringBuilder("""\n"""), 0)

    def elideTo(x: Int, xs: List[AmountDisplay]): List[AmountDisplay] = {
      val (short, long) = xs.partition(ad => ad.builder.width >= x)
      val elisionStr = elisionDisplay(Some(x), sep.width, long.length, lastDef(nullAmountDisplay, short))
      optionAppend(elisionStr, short)
    }
    
    val astrs = amtDisplayList(sep.width, showAmountB(maDisplayOpts, _), orderedAmounts(maDisplayOpts,
      if(maDisplayOpts.displayPrice) ma else mixedAmountStripPrices(ma)))
    val elided = maDisplayOpts.displayMaxWidth.fold(astrs)(x => elideTo(x, astrs))
    val width = elided.map(_.builder.width).max
    
    def pad(amountDisplay: AmountDisplay): AmountDisplay = {
      maDisplayOpts.displayMinWidth match {
        case Some(mw) =>
          val w = width.max(mw) - amountDisplay.builder.width
          amountDisplay.copy(builder = WideBuilder(new StringBuilder(" " * w), w) |+| amountDisplay.builder)
        case None => amountDisplay
      }
    }
    elided.map(a => pad(a).builder)
  }

  def orderedAmounts(maDisplayOpts: AmountDisplayOpts, ma: MixedAmount): List[Amount] = {
    def pad(c: CommoditySymbol)(amounts: List[Amount]): Amount = {
      amounts.find(a => c == a.commodity).getOrElse(amountWithCommodity(c, nullamount))
    }
    maDisplayOpts.displayOrder.fold(identity[List[Amount]](_)) {_.traverse(pad)}(amounts(ma))
  }
  
  def amtDisplayList(sep: Int, showAmt: Amount => WideBuilder, amounts: List[Amount]): List[AmountDisplay] = {
    def display(tot: Int, amt: Amount): (Int, AmountDisplay) = {
      val str = showAmt(amt)
      val tot1 = tot + str.width + sep
      (tot, AmountDisplay(str, tot1))
    }

    amounts.mapAccumulate(-sep)(display)._2
  }
  
  def mixedAmountSetPrecision(p: AmountPrecision, ma: MixedAmount): MixedAmount = {
    mapMixedAmountUnsafe(a => amountSetPrecision(p, a), ma)
  }
  
  def mixedAmountStripPrices(ma: MixedAmount): MixedAmount = {
    val (noPrices, withPrices) = ma.mixed.partition { case (_, amt) => true}//todo actual strip prices
    withPrices.foldLeft(MixedAmount(noPrices)) { case (m, (_, a)) => maAddAmount(m, a) }
  }

  def elisionDisplay(mmax: Option[Int], sep: Int, n: Int, lastAmt: AmountDisplay): Option[AmountDisplay] = {
    val fullString = n.toString + " more.."
    val fullLength = sep + 7 + numDigits(n)
    val str = mmax match {
      case Some(m) if fullLength > m => fullString.take(m - 2) + ".."
      case _ => fullString
    }
    val len = mmax match {
      case None => fullLength
      case Some(m) => 2.max(m.min(fullLength))
    }
    if (n > 0) 
      Some(AmountDisplay(WideBuilder(new StringBuilder(str), len), lastAmt.total + len)) 
    else
      None
  }
}
