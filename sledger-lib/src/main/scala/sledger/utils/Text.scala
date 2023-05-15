package sledger.utils

import io.github.akiomik.seaw.implicits._
import sledger.data.AccountNames.AccountName

import scala.annotation.tailrec

object Text {
  def unlinesB(builders: List[StringBuilder]): StringBuilder =
    builders.foldLeft(new StringBuilder) { (acc, builder) =>
      acc.append(builder).append('\n')
    }

  def formatText(leftJustified: Boolean, minWidth: Option[Int], maxWidth: Option[Int], acctname: AccountName): String = {
    def justify(line: String, width: Int): String = {
      if (line.length >= width) line
      else if (leftJustified) line.padTo(width, ' ')
      else line.reverse.padTo(width, ' ').reverse
    }

    def clip(line: String): String = maxWidth.fold(line)(n => line.take(n))

    val pad: String => String = minWidth.fold(identity[String](_))(n => justify(_, n))

    val lines = if (acctname.isEmpty) List("") else acctname.linesIterator.toList
    val formattedLines = lines.map(line => pad(clip(line)))

    formattedLines.mkString("\n")
  }
  
  def fitText(mminwidth:Option[Int], mmaxwidth: Option[Int], ellipsify: Boolean, rightside: Boolean)(text:String): String = {
    
    val clip: String => String = s => {
      val ellipsis = if (ellipsify) ".." else ""
      mmaxwidth match {
        case Some(w) if(s.width > w) => {
          if (rightside) 
            textTakeWidth(w - ellipsis.length, s) + ellipsis 
          else 
            ellipsis + textTakeWidth(w - ellipsis.length, s.reverse).reverse
        }
        case Some(_) => s
        case None    => s
      }
    }
    val pad: String => String = s => {
      val sw = s.width
      mminwidth match {
        case Some(w) if(sw < w) => {
          if (rightside) {
            s + (" " * (w - sw))
          } else {
            " " * (w - sw) + s 
          }
        }
        case Some(_) => s
        case None => s
      }
    }
    clip.compose(pad)(text)
  }
  
  def textTakeWidth(n: Int, string: String): String = {
    
    @tailrec
    def textTakeWidthHelper(w: Int, t: String, sb: StringBuilder): String = {
      if (t.isEmpty || w <= 0) {
        sb.toString
      } else {
        val c = t.head
        val cw = c.toString.width
        if (cw <= w) {
          textTakeWidthHelper(w - cw, t.tail, sb.append(c))
        } else {
          sb.toString
        }
      }
    }
    
    textTakeWidthHelper(n, string, new StringBuilder())
  }
}
