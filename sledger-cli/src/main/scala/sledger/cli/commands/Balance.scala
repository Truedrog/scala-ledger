package sledger.cli.commands

import cats._
import cats.effect._
import cats.effect.std.Console
import cats.kernel.Monoid
import sledger.cli.CliOptions.CliOpts
import sledger.data.AccountNames.AccountName
import sledger.data.Amounts._
import sledger.data.Journals.Journal
import sledger.data._
import sledger.reports.BalanceReports.{BalanceReport, BalanceReportItem, balanceReport}
import sledger.reports.ReportOptions.{CalcChange, LayoutBare, Options => ReportOptions}
import sledger.text.WideString.{WideBuilder, wbFromString}
import sledger.text.tabular.Ascii.{Align, _}
import sledger.text.tabular.Tabular.{Group, Header, NoLine}
import sledger.utils.Text.{formatText, unlinesB}
import sledger.utils.headDef

object Balance {

  def balance(cliOps: CliOpts, j: Journal): IO[Unit] = {
    val spec = cliOps.reportSpec
    spec.options.balanceCalc match {
      case CalcChange =>
        val report = balanceReport(spec, j)
        val render = balanceReportAsText[IO](spec.options, report).result()
        Console[IO].println(render)
      case ro => Console[IO].errorln(s"Not implemented yet for $ro report")
    }
  }

  def balanceReportAsText[F[_] : Sync](options: ReportOptions, report: BalanceReport): StringBuilder = {
    val isCustom = options.format match {
      case OneLine(FormatField(_, _, _, TotalField) :: _) => false
      case TopAligned(FormatField(_, _, _, TotalField) :: _) => false
      case BottomAligned(FormatField(_, _, _, TotalField) :: _) => false
      case _ => true
    }
    options.layout match {
      case LayoutBare if isCustom => throw new Exception("Custom format not supported with commodity columns")
      case LayoutBare => reportAsText(options, report)
      case _ =>
        val (items, total) = report
        val (ls, sizes) = items.map(r => balanceReportItemAsText(options, r)).unzip
        val (totalLines, _) = renderBalanceReportItem(options, ("", 0, total))
        val overlinewidth = if (isCustom) sizes.transpose.map(_.maxOption.getOrElse(0)).sum else 20
        val overline = new StringBuilder(List.fill(overlinewidth)("-").mkString)
        unlinesB(ls).append(unlinesB(if (options.noTotal) List.empty else List(overline, totalLines)))
    }
  }

  def balanceReportItemAsText(options: ReportOptions, reportItem: BalanceReportItem): (StringBuilder, List[Int]) = {
    val (_, accountName, dep, amt) = reportItem
    renderBalanceReportItem(options, (accountName, dep, amt))
  }

  def renderBalanceReportItem(options: ReportOptions, bitem: (AccountName, Int, MixedAmount)): (StringBuilder, List[Int]) = {

    val render: Boolean => Boolean => List[StringFormatComponent] =>List[Cell] = topAligned => oneline => {
      Functor[List].map(_){renderComponent(topAligned,oneline, options, bitem, _)}
    }
    val renderRow: List[Cell] => (StringBuilder, List[Int]) = is => {
      (
        renderRowSB(TableOpts(tableBorders = false, borderSpaces = false), Group(NoLine, is.map(Header(_)))), 
        is.map(cellWidth)
      )
    }
    options.format match {
      case OneLine(components) => renderRow(render(true)(true)(components))
      case TopAligned(components) => renderRow(render(true)(false)(components))
      case BottomAligned(components) => renderRow(render(false)(false)(components))
    }
  }


  def renderComponent(topAligned: Boolean,
                      oneLine: Boolean,
                      options: ReportOptions,
                      bitem: (AccountName, Int, MixedAmount),
                      formatField: StringFormatComponent): Cell = {
    val (acctname, dep, total) = bitem
    formatField match {
      case FormatLiteral(text) => textCell(TopLeft, text)
      case FormatField(leftJustify, minWidth, maxWidth, field) =>
        val align: Align = (topAligned, leftJustify) match {
          case (true, true) => TopLeft
          case (true, false) => TopRight
          case (false, true) => BottomLeft
          case _ => BottomRight
        }
        val dopts = noPrice.copy(
          displayColor = options.color,
          displayOneLine = oneLine,
          displayMinWidth = minWidth,
          displayMaxWidth = maxWidth
        )

        field match {
          case DepthSpacerField =>
            val d = maxWidth match {
              case Some(value) => Math.min(dep * minWidth.getOrElse(1), value)
              case None => dep * minWidth.getOrElse(1)
            }
            Cell(align, List(WideBuilder(new StringBuilder(" " * d), d)))
          case AccountField => textCell(align, formatText(leftJustify, minWidth, maxWidth, acctname))
          case TotalField => Cell(align, List(showMixedAmountB(dopts, total)))
          case _ => Cell(align, List.empty)
        }
    }
  }
  
  def reportAsText(options: ReportOptions, report: BalanceReport): StringBuilder = {

    def render[A](a: A, acctname: String, dep: Int, amt: MixedAmount): List[Cell] = {
      val cs = if (mixedAmountLooksZero(amt)) List("") else maCommodities(amt).toList
      val dopts = oneLine.copy(displayColor = options.color, displayOrder = Some(cs))
      val dispname = " " * ((dep - 1) * 2) + acctname
      val damts = showMixedAmountLinesB(dopts, amt)
      List(
        Cell(TopRight, damts),
        Cell(TopLeft, cs.map(wbFromString)),
        Cell(TopLeft, List.fill(damts.length - 1)(Monoid[WideBuilder].empty) ++ List(wbFromString(dispname)))
      )
    }

    val (items, total) = report
    val ls = items.map { case (a1, a, n, ma) => render(a1, a, n, ma) }
    val totalline = render("", "", 0, total)

    val sizes: List[Int] = (List(if (options.noTotal) List.empty else totalline) ++ ls)
      .transpose
      .map(a => a.map(cellWidth))
      .foldLeft(List.empty[Int]) {
        case (acc, widths) => (acc ++ widths).maxOption.toList
      }
    val n = headDef(0, sizes)
    val s = List.fill(n)("-").mkString 
    val overline = Cell(TopLeft, List(wbFromString(s)))
    val tableOpts = TableOpts(tableBorders = false)

    unlinesB((ls :+ List(List(overline), if (options.noTotal) List.empty else totalline).flatten)
      .map(cells => renderColumns(tableOpts, sizes, Group(NoLine, cells.map(Header(_))))
      )
    )
  }

}
