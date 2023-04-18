package sledger.data

import cats._
import cats.data._
import cats.syntax.all._
import cats.derived
import io.estatico.newtype.macros.newtype
import sledger.text.WideString.{WideBuilder, wideBuilderFromString}

import scala.math.BigDecimal.RoundingMode

object amount {
  def isNonsimpleCommodityChar: Char => Boolean = (c: Char) => {
    val otherChars = "-+.@*;\t\n \"{}="
    otherChars.exists(e => e == c) || c.isDigit
  }
  
  def quoteCommoditySymbolIfNeeded(s: String): String = {
    if (s.exists(isNonsimpleCommodityChar)) "\"" + s + "\"" else s
  }

  case class AmountDisplayOpts(
                                displayPrice: Boolean = true,
//                                displayColour: Boolean = false, //todo color?
                                displayZeroCommodity: Boolean = false,
                                displayThousandsSep: Boolean = true,
                                displayOneLine: Boolean = false,
                                displayMinWidth: Option[Int] = Some(0),
                                displayMaxWidth: Option[Int] = None,
                                displayOrder: Option[List[CommoditySymbol]] = None
                              )

  val noColour: AmountDisplayOpts = AmountDisplayOpts(displayPrice = false) // default
  val noPrice: AmountDisplayOpts = noColour.copy(displayPrice = false)
  val oneLine: AmountDisplayOpts = noColour.copy(displayOneLine = true, displayPrice = true)

  sealed trait AmountPrecision

  case class Precision(p: Byte) extends AmountPrecision

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

  case class DigitGroupStyle(sep: Char, size: List[Byte])

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

  sealed trait AmountPrice
  case class UnitPrice(a: Amount) extends AmountPrice
  case class TotalPrice(a: Amount )extends AmountPrice
  
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

  case class Amount(
                     commodity: CommoditySymbol, // commodity symbol, or special value "AUTO"
                     quantity: BigDecimal, // numeric quantity, or zero in case of "AUTO"
                     style: AmountStyle,
//                     price: Option[AmountPrice] //-- the (fixed, transaction-specific) price for this amount, if any
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

  def nullamount: Amount = Amount(commodity = "", quantity = 0, style = amountstyle
//    , price = None
  )

  def missingamt: Amount = nullamount.copy(commodity = "AUTO")

  def transformAmount(f: BigDecimal => BigDecimal, amount: Amount): Amount = amount.copy(quantity = f(amount.quantity))
  
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
      override def empty: MixedAmount = nullmixedamout

      override def combine(x: MixedAmount, y: MixedAmount): MixedAmount = maPlus(x, y)
    }
  }

  def amountKey(amount: Amount): MixedAmountKeyNoPrice = MixedAmountKeyNoPrice(amount.commodity) 
  def mixed(amounts: List[Amount]): MixedAmount = amounts.foldLeft(nullmixedamout)(maAddAmount)
  def nullmixedamout: MixedAmount = MixedAmount(mixed = Map.empty)
  def missingmixedamt: MixedAmount = mixedAmount(missingamt)
  def mixedAmount(amount: Amount): MixedAmount = MixedAmount(Map(amountKey(amount) -> amount))
  def maAddAmount(ma: MixedAmount, a: Amount): MixedAmount = MixedAmount(ma.mixed.updated(amountKey(a), a)) 
 
  def mapMixedAmountUnsafe(f: Amount => Amount, mixedAmount: MixedAmount): MixedAmount =
    MixedAmount(mixedAmount.mixed.map{ case (ma, a) => ma->f(a) })
    
  def transformMixedAmount(f: BigDecimal => BigDecimal, mixedAmount: MixedAmount): MixedAmount =
    mapMixedAmountUnsafe(transformAmount(f, _), mixedAmount)

  def maPlus(x: MixedAmount, y: MixedAmount): MixedAmount = {
    import Amount.amountNum._
    MixedAmount(x.mixed.foldLeft(y.mixed) {
      case (acc, (k, v)) => acc.updated(k, acc.get(k).map(a => a + v).getOrElse(v))
    })
  }
  
  def maNegate(x: MixedAmount): MixedAmount =
    transformMixedAmount(Numeric[BigDecimal].negate, x)
  
  def maMinus(x: MixedAmount, y: MixedAmount): MixedAmount =
    maNegate(maPlus(x, y))
  
  def maSum(mas: List[MixedAmount]): MixedAmount =
    mas.foldLeft(nullmixedamout)(maPlus)
  
  def amountRoundedQuantity(amount:Amount): BigDecimal = {
    amount.style.precision match {
      case Precision(p) => amount.quantity.setScale(p, RoundingMode.HALF_EVEN)
      case NaturalPrecision => amount.quantity
    }
  }

  def withPrecision(amount: Amount, precision: AmountPrecision): Amount =
    amount.copy(style = amount.style.copy(precision = precision))
  
  def amountLooksZero(amount: Amount): Boolean =
    BigDecimal(0) == amount.quantity
  
  def showAmountB(showOpts: AmountDisplayOpts, amount: Amount): WideBuilder = {
//    val color = if (showOpts.displayColour && isNegativeAmount(amount)) else 
   
    val showC = (l: WideBuilder, r: WideBuilder) => {
      if (showOpts.displayOrder.isDefined) Monoid[WideBuilder].empty else l |+| r
    }
    val (quantity, c) = {
      
      val q =
        showamountquantity(if (showOpts.displayThousandsSep) amount else amount.copy(style = amount.style.copy(digitgroups = None)))
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
      amount.style.side match {
        case L => showC(wideBuilderFromString(c), space) |+| quantity |+| price
        case R => quantity |+| showC(space, wideBuilderFromString(c)) |+| price
      }
    }
  }

  def applyDigitGroupStyle(mdigitGroupStyle: Option[DigitGroupStyle], intLen: Int, strPart: String): WideBuilder = {
    mdigitGroupStyle match {
      case None => WideBuilder(new StringBuilder(strPart), intLen)
      case Some(DigitGroupStyle(_, Nil)) => WideBuilder(new StringBuilder(strPart), intLen)
      case Some(DigitGroupStyle(c, s :: s1)) =>
        def addSep(nel: NonEmptyList[Byte], l1: Int, s1: String): WideBuilder = {
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

  def showamountquantity(amount: Amount): WideBuilder = {
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
}
