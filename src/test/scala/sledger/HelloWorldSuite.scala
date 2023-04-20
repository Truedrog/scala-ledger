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
import sledger.data.Amounts.{mixedAmount, usd}
import sledger.data.Postings.{nullposting, showPosting}
class HelloWorldSuite extends CatsEffectSuite {
  
  test("basic") {
    val testposting = nullposting.copy(account = "assets", amount = mixedAmount(usd(2)))
    println(showPosting(testposting)+"asdsad as")
    println(WideBuilder(new StringBuilder("""\n"""), 0).builder.result())
  }
}
