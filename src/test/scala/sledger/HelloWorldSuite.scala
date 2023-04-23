package sledger
import scala.collection.immutable.IntMap
import scala.collection.immutable.IntMap.empty
import cats._
import cats.data._
import cats.syntax.all._
import munit.CatsEffectSuite
import org.apache.commons.text.StringEscapeUtils.escapeJava
import sledger.text.tabular.Ascii._
import sledger.text.WideString._

import scala.jdk.CollectionConverters._
import io.github.akiomik.seaw.implicits._
import parsley.Parsley
import parsley.Parsley.{attempt, lookAhead}
import parsley.character.{endOfLine, item, newline}
import parsley.combinator.{choice, eof, many, optional}
import parsley.debug._
import parsley.errors.combinator.ErrorMethods
import parsley.registers.Reg
import sledger.data.Amounts.{mixedAmount, usd}
import sledger.data.Postings.{nullposting, showPosting}
import parsley.implicits.zipped._
import sledger.data.Journals.{Journal, nulljournal}
import sledger.read.Common
import sledger.read.Common.{DigitGroup, amountp, datep, descriptionp, digitgroupp, emptyorcommentlinep, followingcommentp, multilinecommentp, noncommenttextp, rawnumberp, transactioncommentp, yearorintp}
import sledger.read.JournalReader.transactionp
import utils.Parse.spacenonewline

import java.time.{LocalDate, Year}
class HelloWorldSuite extends CatsEffectSuite {
  test("description") {
    var text = ""
    var parser = descriptionp.debug("description") <* eof.debug("eof")
    var j = parser.parse(text)
    println(j)

    text = "abc"
    parser = descriptionp.debug("description") <* eof.debug("eof")
    j = parser.parse(text)
    println(j)


    text = "abc dsa sa aaasd                "
    parser = descriptionp.debug("description") <* eof.debug("eof")
    j = parser.parse(text)
    println(j)
  }
  
  test("transactioncommentp") {
    var text = ""

    var parser = transactioncommentp.debug("transactioncommentp") <* eof.debug("eof")
    var j = parser.parse(text)
    println(j)

    text = ";"

    parser = transactioncommentp.debug("description dest") <* eof.debug("eof")
    j = parser.parse(text)
    println(j)
    
    text = ";  \n"

    parser = transactioncommentp.debug("description dest") <* eof.debug("eof")
    j = parser.parse(text)
    println(j) 
    
    text = ";\n ;\n"

    parser = transactioncommentp.debug("description dest") <* eof.debug("eof")
    j = parser.parse(text)
    println(j)
    
    text = "\n ;\n"

    parser = transactioncommentp.debug("description dest") <* eof.debug("eof")
    j = parser.parse(text)
    println(j)
  }
  
  test("basic transaction") {

    case class Item(description: String)

    case class Root(items: List[Item])
    val text = List("2008/1/1   ",
    ).mkString("","\n","\n")
    println(text+"a")
    val r = Reg.make[Root]
    val t = {
      for {
        d <- datep.debug("date")
        desc <- descriptionp.debug("description")
      } yield Item(d.toString +"  " +desc)

    }
    val addItemP = {
      choice(descriptionp.debug("transaction"),
        emptyorcommentlinep.debug("emptyorcommentlinep").void,
        multilinecommentp.debug("multilinecommentp").void
      ).debug("choice elements of item")
    }
    val parser = r.put(Root(List.empty)) *> many(addItemP).debug("many items") <* eof.debug("eof")
    val j = parser.parse(text)
    println(j)
  }

  test("amountp") {
    var p = amountp
    println(p.parse("1,5"))
    println(p.parse("10047"))
    println(p.parse("47.18USD"))
    println(p.parse("47.18 USD"))
    println(p.parse("0"))
    println(p.parse("-0"))
    println(p.parse("-0 $"))
    println(p.parse("0.0001"))
    println(p.parse("1.0001"))
    println(p.parse("1,2.0001"))
  }
}
