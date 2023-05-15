package sledger.read

import munit.CatsEffectSuite
import sledger.read.JournalReader.postingp
import sledger.read.JournalReader.transactionp

class JournalReaderTest extends CatsEffectSuite {

  test("basic") {
    val p = postingp

    var t = "  expenses:food:dining  10.00$   ; a: a a \n   ; b: b b \n"
    println(p.parse(t))
    t = " a  1. ; date:2012/11/28, date2=2012/11/29,b:b\n"
    println(p.parse(t))

  }
  test("complex") {
    var t = "" 
//    var t =
//      List("2007/01/28 coopportunity"
//        , "    expenses:food:groceries                   47.18$"
//        , "    assets:checking                          -47.18$"
//        , ""
//      ).mkString("\n")
//
//    println(transactionp.parse(t).map(_.postings))
//    t = "2009/1/1 a ;comment\n b 1\n"
//    println(transactionp.parse(t).map(_.description))
//
//    t = List("2012/1/1"
//      , "  ;"
//      , "  a  1"
//      , "  b"
//      , " "
//    ).mkString("\n")
//    println(transactionp.parse(t))
//
//    t = List("2009/1/1 x  ; transaction comment"
//      , " a  1  ; posting 1 comment"
//      , " ; posting 1 comment 2"
//      , " b"
//      , " ; posting 2 comment",
//    ).mkString("\n")
//    println(transactionp.parse(t))

    t = List("2009/1/1 x  ; transaction comment"
      , " expenses:food  1    ; posting 1 comment"
      , " ; posting 1 comment 2"
      , " expenses:other  2   ; posting 2 comment"
      , " ; posting 2 comment 2"

    ).mkString("\n")
    println(transactionp.parse(t).map(_.postings.map(_.account)))
  }
}
