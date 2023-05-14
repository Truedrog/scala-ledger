package sledger.cli

import cats.effect._
import cats.syntax.all._
import com.monovore.decline.Opts
import sledger.Read.defaultJournalPath
import sledger.data.InputOptions.{InputOpts, defInputOpts}
import sledger.reports.ReportOptions.{Spec, defaultSpec}

import java.nio.file.{Path, Paths}


object CliOptions {
  case class CliOpts(
                     file: Option[Path] = None,
                     inputOpts: InputOpts = defInputOpts, //todo convert args to inputOpts
                     reportSpec: Spec = defaultSpec, //todo convert args to reportSpec
                     width: Option[String] = None,
                     availableWidth: Int = 80
                   )

  def rawToCliOpts: Opts[CliOpts] = {
    val fileOpt = Opts.option[String](
      "file", 
      short="f", 
      help = "Use a different input file. (default: $LEDGER_FILE or $HOME/.sledger.journal)").orNone
    (fileOpt).map { file =>
      CliOpts(
        file =  file.map(f => Paths.get(f))
      )
    }
  } 
  
  def journalFilePathFromOpts[F[_]: Sync](cliOpts: CliOpts): F[Path] = {

    val getHomeDirectoryIO: F[Path] = Sync[F].delay(Paths.get(System.getProperty("user.home")))
    for {
      f <- defaultJournalPath
      d <- getHomeDirectoryIO
    } yield cliOpts.file match {
      case Some(p) => {
        val absolutePath = p.toAbsolutePath
        if (absolutePath.startsWith("~")) {
          val remainingPath = absolutePath.subpath(1, absolutePath.getNameCount)
          Paths.get(d.toString).resolve(remainingPath)
        } else {
          absolutePath
        }
      }
      case None => Paths.get(f)
    }
  }
}