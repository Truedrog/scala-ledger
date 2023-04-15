package sledger.text.tabular
import cats._
import cats.data._
import cats.syntax.all._

object Ascii {
  implicit val stringBuilderMonoid: Monoid[StringBuilder] = new Monoid[StringBuilder] {
    override def empty: StringBuilder = new StringBuilder("")

    override def combine(x: StringBuilder, y: StringBuilder): StringBuilder = {
      new StringBuilder().append(x).append(y)
    }
  }
  case class TableOpts(prettyTable: Boolean = false, tableBorders: Boolean = true, borderSpaces: Boolean = true)
  object TableOpts {
    implicit val TableOpts: Show[TableOpts] = Show.fromToString
  }
  sealed trait Properties
  case object NoLine extends Properties
  case object SingleLine extends Properties
  case object DoubleLine extends Properties
  
  sealed trait TableHeader[+A]
  final case class Header[A](a: A) extends TableHeader[A]
  final case class Group[A](properties: Properties, headers: List[TableHeader[A]]) extends TableHeader[A]
  object TableHeader {
    implicit val functorTH: Functor[TableHeader] = new Functor[TableHeader] {
      override def map[A, B](fa: TableHeader[A])(f: A => B): TableHeader[B] = fa match {
        case Header(a) => Header(f(a))
        case Group(properties, headers) => Group(properties, headers.map(h => map(h)(f)))
      }
    }  
  }

  def headerContents[A](h: TableHeader[A]):List[A] = {
    h match {
      case Header(a) => List(a)
      case Group(_, headers) => headers.flatMap(headerContents)
    }
  }
  
  def zipHeader[A](e: Int, ss: List[Int], h: TableHeader[A]): TableHeader[(Int, A)] = {
    def runner(header: TableHeader[A]): State[List[Int], TableHeader[(Int, A)]] = {
      State { initial => {
        header match {
          case Header(a) => initial match {
            case s :: ss => (ss, Header(s, a))
            case Nil => (initial, Header(e, a))
          }
          case Group(properties, headers) =>
            headers.traverse(runner).map(a => Group(properties, a)).run(initial).value
        }
      }
      }
    }

    runner(h).runA(ss).value
  }
  
  def flattenHeader[A](h: TableHeader[A]): List[Either[Properties, A]] = {
    h match {
      case Header(a) => List(Right(a))
      case Group(properties, headers) =>
        val hs: List[List[Either[Properties, A]]] = headers.map(flattenHeader[A])
        val pxs = List(Left(properties))
        Foldable[List].intercalate(hs, pxs)
    }
  }
  
  final case class Table[A](rh: TableHeader[A], ch: TableHeader[A], a: List[List[A]])
  def empty: Table[Nothing] = Table(Group(NoLine, List.empty), Group(NoLine, List.empty), List.empty)

  sealed trait Align
  case object TopRight extends Align
  case object BottomRight extends Align
  case object BottomLeft extends Align
  case object TopLeft extends Align
  object Align {
    implicit val TableOpts: Show[Align] = Show.fromToString
  }
  
  case class Cell(align: Align, ls: List[StringBuilder])
  
  def emptyCell: Cell = Cell(TopRight, List.empty)
  
  def textCell(a: Align, x: String): Cell = Cell(a, 
    if (x.isEmpty) List(new StringBuilder("")) else {
      x.split("\\\\n").map(new StringBuilder(_)).toList
    }
  )
  
  def textCells(a: Align, txts: List[String]): Cell = Cell(a, txts.map(new StringBuilder(_)))
  
  def cellWidth(cell: Cell) = cell.ls.map(x => x.length).maxOption.fold(0)(a => a)
  
  def verticalBar(pretty: Boolean) = if (pretty) "│" else "|"
  
  def leftBar(pretty: Boolean, spaces: Boolean) = {
    (pretty, spaces) match {
      case (_, true)  =>  new StringBuilder(verticalBar(pretty)).append(" ")
      case (_, false) =>  new StringBuilder(verticalBar(pretty))
    }
  }
  
  def rightBar(pretty: Boolean, spaces: Boolean) = {
    (pretty, spaces) match {
      case (_, true)  =>  new StringBuilder(verticalBar(pretty)).insert(0, " ")
      case (_, false) =>  new StringBuilder(verticalBar(pretty))
    }
  }
  
  def midBar(pretty: Boolean, spaces: Boolean) = {
    (pretty, spaces) match {
      case (_, true)  => new StringBuilder(verticalBar(pretty)).insert(0, " ").append(" ")
      case (_, false) => new StringBuilder(verticalBar(pretty))
    }
  }
  
  def doubleMidBar(pretty: Boolean, spaces: Boolean) = {
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
  
  def renderColums[A](tableOps: TableOpts, maxWidth: List[Int], header: TableHeader[Cell]): StringBuilder = {
    def padRow(cell: Cell) = cell match {
      case Cell(TopLeft,     ls) => Cell(TopLeft, ???)
      case Cell(TopRight,    ls) => ???
      case Cell(BottomLeft,  ls) => ???
      case Cell(BottomRight, ls) => ???
    }
    def nLines = headerContents(header).map(c => c.ls.length).maxOption.fold(0)(a => a)
    ???
  }
  
  def renderHLine[A](vpos: VPos, 
                     borders: Boolean, //show outer borders
                     pretty: Boolean,
                     prop: Properties,
                     w: List[Int], //width specs
                     hdr: TableHeader[A]): List[StringBuilder] = {
    
    def addBorders(xs: StringBuilder): StringBuilder = {
      if(borders) edge(HL).append(xs).append(edge(HR)) else xs
    }

    def edge(hpos: HPos): StringBuilder = boxchar(vpos, hpos, SingleLine, prop)(pretty)
    def coreLine = {
      val xs = flattenHeader(zipHeader(0, w, hdr))
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

  def pick(str1: String, str2: String)(b: Boolean) = {
    (str1, str2, b) match {
      case (x, _, true) => new StringBuilder(x)
      case (x, _, false) => new StringBuilder(x)
    }
  }
  
  def lineart(p1: Properties, p2: Properties, p3: Properties, p4: Properties)(pretty: Boolean) = {
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
  def concatTable[A](properties: Properties, table: Table[A], table1: Table[A]): Table[A] = {
    Table(Group(properties, List(table.ch, table1.ch)), table.rh, table.a ++ table1.a)
  }
}
