package sledger.cli

import cats.effect._
import cats.syntax.all._
import com.monovore.decline.Opts
import sledger.Read.defaultJournalPath
import sledger.data.InputOptions.{InputOpts, defInputOpts}
import sledger.reports.ReportOptions._

import java.nio.file.{Path, Paths}


object CliOptions {
  def flatTreeFlag(showAmountHelp: Boolean): Opts[AccountListMode] = {
    val flat = Opts.flag("flat", "show accounts as a flat list (default)", "l").orTrue
    val tree = Opts.flag(
      "tree", s"show accounts as a tree ${if (showAmountHelp) ". Amounts include subaccount amounts." else ""}",
      "t").orFalse
    (flat, tree).mapN((f, t) => {
      if (t) ALTree else if (f) ALFlat else {
        ALFlat
      }
    })
  }

  case class CliOpts(
                      file: Option[Path],
                      inputOpts: InputOpts, //todo convert args to inputOpts
                      reportSpec: Spec,
                      width: Option[String],
                      availableWidth: Int
                    )

  val defcliopts: CliOpts = CliOpts(file = None, inputOpts = defInputOpts, reportSpec = defaultSpec, width = None, availableWidth = 80)


  val mapOptsToReportSpec: Opts[Spec] = {
    val emptyFlag = Opts.flag("empty", "Show items with zero amount, normally hidden", "E").orFalse
    val color = Opts.flag(long="color", help="Should color-supporting commands use ANSI color codes in text output." ).orTrue
    val pretty = Opts.flag("pretty", s"Show prettier output, e.g. using unicode box -drawing characters.").orFalse
    val depth = Opts.option[Int]("depth", s"hide accounts/postings deeper than this").orNone
    
    (emptyFlag, color, pretty, depth).mapN((ef, color, pretty, depth) => {
      reportOptsToSpec(defaultsOptions.copy(empty = ef, color = color, pretty = pretty, depth = depth))
    })
  }
  
  val generalOpts: Opts[CliOpts] = {
    val fileOpt = Opts.option[String](
      "file",
      short = "f",
      help = "Use a different input file. (default: $LEDGER_FILE or $HOME/.sledger.journal)").orNone
    fileOpt.map { (file) =>
      defcliopts.copy(
        file = file.map(f => Paths.get(f))
      )
    }
  }

  def journalFilePathFromOpts[F[_] : Sync](cliOpts: CliOpts): F[Path] = {

    val getHomeDirectoryIO: F[Path] = Sync[F].delay(Paths.get(System.getProperty("user.home")))
    for {
      f <- defaultJournalPath
      d <- getHomeDirectoryIO
    } yield cliOpts.file match {
      case Some(p) =>
        val absolutePath = p.toAbsolutePath
        if (absolutePath.startsWith("~")) {
          val remainingPath = absolutePath.subpath(1, absolutePath.getNameCount)
          Paths.get(d.toString).resolve(remainingPath)
        } else {
          absolutePath
        }
      case None => Paths.get(f)
    }
  }
}