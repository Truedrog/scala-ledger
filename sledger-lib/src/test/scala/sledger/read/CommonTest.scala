package sledger.read
import munit.FunSuite
import sledger.data.Amounts._
import sledger.read.Common.amountp
class CommonTest extends FunSuite {

  test("numberp") {
    val p = amountp
    var expected = nullamount.copy(commodity = "", quantity = 1.5, amountstyle.copy(precision = Precision(1), decimalpoint = Some(',')))
    assertEquals(p.parse("1,5").get, expected)
    
    expected = nullamount.copy(commodity = "", quantity = 10047, amountstyle.copy(precision = Precision(0), decimalpoint = None))
    assertEquals(p.parse("10047").get, expected)
   
    expected = nullamount.copy(commodity = "USD", quantity = 47.18, amountstyle.copy(side = R, precision = Precision(2), decimalpoint = Some('.')))
    assertEquals(p.parse("47.18USD").get, expected)
   
    expected = nullamount.copy(commodity = "", quantity = 0, amountstyle.copy(precision = Precision(0), decimalpoint = None))
    assertEquals(p.parse("0").get, expected)
    
    expected = nullamount.copy(commodity = "$", quantity = 0, amountstyle.copy(side = R,  commodityspaced = true, precision = Precision(0), decimalpoint = None))
    assertEquals(p.parse("-0 $").get, expected)

    expected = nullamount.copy(commodity = "", quantity = 0.0001, amountstyle.copy( precision = Precision(4), decimalpoint = Some('.')))
    assertEquals(p.parse("0.0001").get, expected)
    
    expected = nullamount.copy(commodity = "", quantity = 1.0001, amountstyle.copy( precision = Precision(4), decimalpoint = Some('.')))
    assertEquals(p.parse("1.0001").get, expected)
    
    expected = nullamount.copy(commodity = "", quantity = 12.0001, amountstyle.copy(
      precision = Precision(4),
      decimalpoint = Some('.'),
      digitgroups = Some(DigitGroupStyle(',', List(1, 1))))
    )
    assertEquals(p.parse("1,2.0001").get, expected)
    
  }
}
