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
class HelloWorldSuite extends CatsEffectSuite {
  
  test("basic") {
    println(new StringBuilder("Hello") |+| new StringBuilder(" ") |+| new StringBuilder("world!"))
  }
}
