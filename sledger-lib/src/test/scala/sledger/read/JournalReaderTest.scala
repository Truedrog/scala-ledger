package sledger.read

import munit.CatsEffectSuite
import sledger.data.Amounts.{mixedAmount, num, usd}
import sledger.data.Postings.posting
import sledger.read.JournalReader.postingp
import sledger.read.JournalReader.transactionp

class JournalReaderTest extends CatsEffectSuite {

  test("basic") {
    val p = postingp

    var t = "  expenses:food:dining  $10.00   ; a: a a \n   ; b: b b \n"
    var expected = posting.copy(
      account = "expenses:food:dining",
      amount = mixedAmount(usd(10)),
      comment = "a: a a\nb: b b\n"
    )
    assertEquals(p.parse(t).get, expected)
    
    t = " a  1. ; date:2012/11/28, date2=2012/11/29,b:b\n"
    expected = posting.copy(
      account = "a",
      amount = mixedAmount(num(1)),
      comment = "date:2012/11/28, date2=2012/11/29,b:b\n"
    )
    assertEquals(p.parse(t).get, expected)
  }
  
  test("parses a well-formed transaction") {
    var t =
      List("2007/01/28 coopportunity"
        , "    expenses:food:groceries                   47.18$"
        , "    assets:checking                          -47.18$"
        , ""
      ).mkString("\n")
    assert(transactionp.parse(t).toEither.isRight)
  }
  
  test("does not parse a following comment as part of the description") {
    var t = "2009/1/1 a ;comment\n b 1\n"
    assertEquals(transactionp.parse(t).map(_.description).get, "a")
  }
  
  test("parses a following whitespace line") {
   var t = List("2012/1/1"
      , "  ;"
      , "  a  1"
      , "  b"
      , " "
    ).mkString("\n")
    assert(transactionp.parse(t).toEither.isRight)
  }
    

  test("parses an empty transaction comment following whitespace line") {
    var t = List("2009/1/1 x  ; transaction comment"
      , " a  1  ; posting 1 comment"
      , " ; posting 1 comment 2"
      , " b"
      , " ; posting 2 comment",
    ).mkString("\n")
    assert(transactionp.parse(t).toEither.isRight)
  }
 
  test("comments everywhere, two postings parsed") {
    var t = List("2009/1/1 x  ; transaction comment"
      , " expenses:food  1    ; posting 1 comment"
      , " ; posting 1 comment 2"
      , " expenses:other  2   ; posting 2 comment"
      , " ; posting 2 comment 2"

    ).mkString("\n")
    assertEquals(transactionp.parse(t).map(_.postings.length).get, 2)
  }
}
