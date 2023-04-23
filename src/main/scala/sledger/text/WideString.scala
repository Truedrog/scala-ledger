package sledger.text

import cats._
import cats.syntax.all._
import io.github.akiomik.seaw.implicits._
import sledger.Types._
object WideString {
  
  case class WideBuilder(builder: StringBuilder, width: Int)

  implicit val wsBuilderMonoid: Monoid[WideBuilder] = new Monoid[WideBuilder] {
    override def empty: WideBuilder = WideBuilder(Monoid[StringBuilder].empty, 0)

    override def combine(x: WideBuilder, y: WideBuilder): WideBuilder = {
      WideBuilder(x.builder |+| y.builder, x.width + y.width)
    }
  }
  
  def wideBuilderToString(wb: WideBuilder): String = wb.builder.result()

  def wbFromString(t: String): WideBuilder = WideBuilder(new StringBuilder(t), t.width)
}
