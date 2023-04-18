package sledger.text.tabular

import scala.collection.decorators._
import cats.{Group => _, _}
import cats.data._
import cats.syntax.all._

import sledger.text.WideString._
import sledger.text.tabular.Tabular._

object Ascii {
  
  case class TableOpts(prettyTable: Boolean = false, tableBorders: Boolean = true, borderSpaces: Boolean = true)
  object TableOpts {
    implicit val TableOpts: Show[TableOpts] = Show.fromToString
  }

  sealed trait Align
  case object TopRight extends Align
  case object BottomRight extends Align
  case object BottomLeft extends Align
  case object TopLeft extends Align
  object Align {
    implicit val TableOpts: Show[Align] = Show.fromToString
  }
  
  case class Cell(align: Align, ls: List[WideBuilder])
  
  def emptyCell: Cell = Cell(TopRight, List.empty)
  
  def textCell(a: Align, x: String): Cell = {
    val texts = if (x.isEmpty) List("") else {
      x.split("\\\\n").toList
    }
    Cell(a,
      texts.map(wideBuilderFromString)
    )
  }
  
  def textCells(a: Align, txts: List[String]): Cell = Cell(a, txts.map(wideBuilderFromString))
  
  def cellWidth(cell: Cell): Int = cell.ls.map(x => x.width).maxOption.fold(0)(a => a)
  
  def verticalBar(pretty: Boolean): String = if (pretty) "│" else "|"
  
  def leftBar(pretty: Boolean, spaces: Boolean): StringBuilder = {
    (pretty, spaces) match {
      case (_, true)  =>  new StringBuilder(verticalBar(pretty)).append(" ")
      case (_, false) =>  new StringBuilder(verticalBar(pretty))
    }
  }
  
  def rightBar(pretty: Boolean, spaces: Boolean): StringBuilder = {
    (pretty, spaces) match {
      case (_, true)  =>  new StringBuilder(verticalBar(pretty)).insert(0, " ")
      case (_, false) =>  new StringBuilder(verticalBar(pretty))
    }
  }
  
  def midBar(pretty: Boolean, spaces: Boolean): StringBuilder = {
    (pretty, spaces) match {
      case (_, true)  => new StringBuilder(verticalBar(pretty)).insert(0, " ").append(" ")
      case (_, false) => new StringBuilder(verticalBar(pretty))
    }
  }
  
  def doubleMidBar(pretty: Boolean, spaces: Boolean): StringBuilder = {
    (pretty, spaces) match {
      case (_, true) => if(pretty) new StringBuilder(" ║ ") else new StringBuilder(" || ")
      case (_, false) => if(pretty) new StringBuilder("║") else new StringBuilder("||")
    }
  }
  
  sealed trait VPos
  case object VT extends VPos
  case object VM extends VPos
  case object VB extends VPos
  
  sealed trait HPos
  case object HL extends HPos
  case object HM extends HPos
  case object HR extends HPos

  def renderTable[R, C, A](
                      tableOps: TableOpts,
                      fr: R => Cell, // Rendering function for row headers
                      fc: C => Cell, // Rendering function for column headers
                      f: A => Cell, //  Function determining the string and width of a cell
                      table: Table[R, C, A]
                    ): String = {
    renderTableB(tableOps, fr, fc, f, table).result()
  }

  def renderTableB[R, C, A](
                       tableOps: TableOpts,
                       fr: R => Cell,
                       fc: C => Cell,
                       f: A => Cell,
                       table: Table[R, C, A]
                     ):StringBuilder = {
    val fRListA: ((R, List[A])) => (Cell, List[Cell]) = { case (r, la) => fr(r) -> la.map(f) }
    val fList: List[C] => List[Cell] = la => la.map(fc)
    renderTableByRowsB[R,C,A](tableOps, fList, fRListA, table)
  }
  
  def renderTableByRowsB[R,C,A](
                             tableOps: TableOpts,
                             fc: List[C] => List[Cell],              // Rendering function for column headers
                             f: ((R, List[A])) => (Cell, List[Cell]),// Rendering function for row and row header
                             table: Table[R, C, A]   
                           ): StringBuilder = {
    val rows = headerContents(table.rh).zip(table.cells).map(f).unzip
    val rowHeaders = zipHeader(emptyCell, rows._1, table.rh).map(a => a._1)
    val colHeaders = zipHeader(emptyCell, fc(headerContents(table.ch)), table.ch).map(a => a._1)
    val cellContents = rows._2

    // ch2 and cell2 include the row and column labels
    val ch2 = Group(DoubleLine, List(Header(emptyCell), colHeaders))
    val cells2 = headerContents(ch2) :: headerContents(rowHeaders).lazyZip(cellContents).map((a, b) => a :: b)

    // maximum width for each column
    val sizes = cells2.transpose.map(a => a.map(cellWidth).maxOption.getOrElse(0))
    def renderR(cs: List[Cell],h: Cell): StringBuilder = {
      renderColumns(tableOps, sizes, Group(DoubleLine, List(Header(h), zipHeader(emptyCell, cs, colHeaders)._1F)))
    }
    
    def renderRs[AA <: StringBuilder](h: TableHeader[AA]): List[StringBuilder] = {
      h match {
        case Header(a) => List[StringBuilder](a)
        case Group(properties, headers) =>
          val sep = renderHLine(vpos = VM, borders = tableOps.borderSpaces, pretty = tableOps.prettyTable,
            w = sizes,
            header = ch2,
            prop = properties)
          headers.map(renderRs[AA]).intercalate(sep)
      }
    }
    
    // borders and bars
    def addBorders(xs: List[StringBuilder]): List[StringBuilder] =
      if (tableOps.borderSpaces) {
        bar(VT, SingleLine) :: xs |+| List(bar(VB, SingleLine))
      } else xs

    def bar(VPos: VPos, properties: Properties): StringBuilder =
      Monoid.combineAll(renderHLine(vpos = VPos,
        borders = tableOps.borderSpaces,
        pretty = tableOps.prettyTable,
        w = sizes,
        header = ch2,
        prop = properties)) 
        
    val unlinesB = (builders: List[StringBuilder]) =>
      Foldable[List].foldMap(builders) {a => a |+| new StringBuilder("""\n""")}

    unlinesB
      .compose(addBorders)
      .apply(List(renderColumns(tableOps, sizes, ch2), bar(VM, DoubleLine)) |+| renderRs(zipHeader(List.empty, cellContents, rowHeaders)
        .map { case (a, b) => renderR(a, b) }))
  }
  
  def renderRow(tableOpts: TableOpts, header:TableHeader[Cell]): String = renderRowSB(tableOpts, header).result()
  
  def renderRowSB(tableOpts: TableOpts, header: TableHeader[Cell]): StringBuilder = {
    val is = headerContents(header).map(cellWidth)
    renderColumns(tableOpts, is, header)
  }
  
  def renderColumns(tableOps: TableOpts, maxWidth: List[Int], header: TableHeader[Cell]): StringBuilder = {
    def padcell(widthAndCell: (Int, Cell)):List[StringBuilder] = widthAndCell match {
      case (w, Cell(TopLeft, ls))     => ls.map(x => Monoid[StringBuilder].combine(x.builder, new StringBuilder(" ".repeat(w - x.width))))
      case (w, Cell(BottomLeft, ls))  => ls.map(x => Monoid[StringBuilder].combine(x.builder, new StringBuilder(" ".repeat(w - x.width))))
      case (w, Cell(TopRight, ls))    => ls.map(x => Monoid[StringBuilder].combine(new StringBuilder(" ".repeat(w - x.width)),x.builder))
      case (w, Cell(BottomRight, ls)) => ls.map(x => Monoid[StringBuilder].combine(new StringBuilder(" ".repeat(w - x.width)),x.builder))
    }
    
    def padRow(cell: Cell): Cell = cell match {
      case Cell(TopLeft,     ls) => Cell(TopLeft, ls ++ List.fill(nLines - ls.length)(Monoid[WideBuilder].empty))
      case Cell(TopRight,    ls) => Cell(TopRight, ls ++ List.fill(nLines - ls.length)(Monoid[WideBuilder].empty))
      case Cell(BottomLeft,  ls) => Cell(BottomLeft, List.fill(nLines - ls.length)(Monoid[WideBuilder].empty) ++ ls)
      case Cell(BottomRight, ls) => Cell(BottomRight, List.fill(nLines - ls.length)(Monoid[WideBuilder].empty) ++ ls)
    }
    
    def nLines: Int = headerContents(header).map(c => c.ls.length).maxOption.fold(0)(a => a)

    def hsep(props: Properties): List[StringBuilder] = props match {
      case NoLine => List.fill(nLines)(if (tableOps.borderSpaces) new StringBuilder("  ") else new StringBuilder(""))
      case SingleLine => List.fill(nLines)(midBar(tableOps.prettyTable, tableOps.borderSpaces))
      case DoubleLine => List.fill(nLines)(doubleMidBar(tableOps.prettyTable, tableOps.borderSpaces))
    }

    def addBorders(xs: StringBuilder): StringBuilder = {
      if (tableOps.borderSpaces) {
        leftBar(tableOps.prettyTable, tableOps.borderSpaces) |+| xs |+| rightBar(tableOps.prettyTable, tableOps.borderSpaces)
      } else if (tableOps.borderSpaces) {
        new StringBuilder(" ") |+| xs |+| new StringBuilder(" ")
      } else {
        xs
      }
    }

    Monoid[StringBuilder]
      .combineAll(
        flattenHeader(zipHeader(0, maxWidth, header.map(padRow)))
          .map(_.fold(hsep, padcell))
          .transpose
          .map(x => addBorders(Monoid[StringBuilder].combineAll(x)))
          .intersperse(new StringBuilder("""\n"""))
      )
  }
  
  def renderHLine[A](vpos: VPos,
                     borders: Boolean, //show outer borders
                     pretty: Boolean,
                     prop: Properties,
                     w: List[Int], //width specs
                     header: TableHeader[A]): List[StringBuilder] = {
    def addBorders(xs: StringBuilder): StringBuilder = {
      if(borders) edge(HL).append(xs).append(edge(HR)) else xs
    }

    def edge(hpos: HPos): StringBuilder = boxchar(vpos, hpos, SingleLine, prop)(pretty)
    def coreLine = {
      val xs = flattenHeader(zipHeader(0, w, header))
      Foldable[List].foldMap(xs) {
        case Left(p) => vsep(p)
        case Right((i, _)) => new StringBuilder(Monoid.combineN(sep.result(), i))
      }
    }
    
    def sep: StringBuilder = boxchar(vpos, HM, NoLine, prop)(pretty)
    
    def vsep(v: Properties) = v match {
      case NoLine => sep.append(sep)
      case _      => sep.append(cross(v, prop)).append(sep)
    }
    def cross(v: Properties, h: Properties) = boxchar(vpos,HM, v,h)(pretty)
    
    List(addBorders(sep).append(coreLine).append(sep))
  }
  
  def boxchar[A](vpos: VPos,
                 hpos: HPos,
                 vert: Properties,
                 horiz: Properties
               )(pretty: Boolean): StringBuilder = {
    val u = vpos match {
      case VT => NoLine
      case _  => vert
    }
    val d = vpos match {
      case VB => NoLine
      case _ => vert
    }
    val l = hpos match {
      case HL => NoLine
      case _  => horiz
    }
    val r = hpos match {
      case HR => NoLine
      case _  => horiz
    }
    lineart(u, d, l, r)(pretty)
  }

  def pick(str1: String, str2: String)(b: Boolean): StringBuilder = {
    (str1, str2, b) match {
      case (x, _, true) => new StringBuilder(x)
      case (x, _, false) => new StringBuilder(x)
    }
  }
  
  def lineart(p1: Properties, p2: Properties, p3: Properties, p4: Properties)(pretty: Boolean): StringBuilder = {
    (p1, p2, p3, p4) match {
      //   up           down        left        right
      case (SingleLine, SingleLine, SingleLine, SingleLine) => pick("┼","+") (pretty)
      case (SingleLine, SingleLine, SingleLine, NoLine)     => pick("┤","+") (pretty)
      case (SingleLine, SingleLine, NoLine,     SingleLine) => pick("├","+") (pretty)
      case (SingleLine, NoLine,     SingleLine, SingleLine) => pick("┴","+") (pretty)
      case (NoLine,     SingleLine, SingleLine, SingleLine) => pick("┬","+") (pretty)
      case (SingleLine, NoLine,     NoLine,     SingleLine) => pick("└","+") (pretty)
      case (SingleLine, NoLine,     SingleLine, NoLine)     => pick("┘","+") (pretty)
      case (NoLine,     SingleLine, SingleLine, NoLine)     => pick("┐","+") (pretty)
      case (NoLine,     SingleLine, NoLine,     SingleLine) => pick("┌","+") (pretty)
      case (SingleLine, SingleLine, NoLine,     NoLine)     => pick("│", "|") (pretty)
      case (NoLine,     NoLine,     SingleLine, SingleLine) => pick("─", "-") (pretty)

      case (DoubleLine, DoubleLine, DoubleLine, DoubleLine) => pick("╬", "++") (pretty)
      case (DoubleLine, DoubleLine, DoubleLine, NoLine)     => pick("╣", "++") (pretty)
      case (DoubleLine, DoubleLine, NoLine,     DoubleLine) => pick("╠", "++") (pretty)
      case (DoubleLine, NoLine,     DoubleLine, DoubleLine) => pick("╩", "++") (pretty)
      case (NoLine,     NoLine,     NoLine,     DoubleLine) => pick("╦", "++") (pretty)
      case (DoubleLine, NoLine,     NoLine,     DoubleLine) => pick("╚", "++") (pretty)
      case (DoubleLine, NoLine,     DoubleLine, NoLine)     => pick("╝", "++") (pretty)
      case (NoLine,     DoubleLine, NoLine,     DoubleLine) => pick("╔", "++") (pretty)
      case (DoubleLine, DoubleLine, NoLine,     NoLine)     => pick("║", "||") (pretty)
      case (NoLine,     NoLine,     DoubleLine, DoubleLine) => pick("═", "=") (pretty)

      case (DoubleLine, NoLine,     NoLine,     SingleLine) => pick("╙", "++") (pretty)
      case (DoubleLine, NoLine,     SingleLine, NoLine)     => pick("╜", "++") (pretty)
      case (NoLine,     DoubleLine, SingleLine, NoLine)     => pick("╖", "++") (pretty)
      case (NoLine,     DoubleLine, NoLine,     SingleLine) => pick("╓", "++") (pretty)

      case (SingleLine, SingleLine, DoubleLine, NoLine)     => pick("╡", "+") (pretty)
      case (SingleLine, SingleLine, NoLine,     DoubleLine) => pick("╞", "+") (pretty)
      case (SingleLine, NoLine,     DoubleLine, DoubleLine) => pick("╧", "+") (pretty)
      case (NoLine,     SingleLine, DoubleLine, DoubleLine) => pick("╤", "+") (pretty)

      case (SingleLine, SingleLine, DoubleLine, DoubleLine) => pick("╪", "+") (pretty)
      case (DoubleLine, DoubleLine, SingleLine, SingleLine) => pick("╫", "++") (pretty)
      case (_         , _         , _         , _         ) => pick("", "") (pretty)
    }
  }

  /*
   Add the second table below the first, discarding its column headings.
   */
  def concatTable[R, C, A](properties: Properties, table: Table[R, C, A], table1: Table[R, C, A]): Table[R, C, A] = {
    Table(Group(properties, List(table.rh, table1.rh)), table.ch, table.cells ++ table1.cells)
  }
}
