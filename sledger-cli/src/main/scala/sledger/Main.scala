package sledger

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, Sync}
import cats.syntax.all._
import com.monovore.decline._
import com.monovore.decline.effect._
import sledger.Read.readJournalFile
import sledger.cli.CliOptions.{CliOpts, journalFilePathFromOpts}
import sledger.cli.Commands
import sledger.cli.commands.Balance.balance
import sledger.data.Journals.Journal

object Main extends CommandIOApp(
  name = "sledger",
  header = "sledger command line",
  version = "1.0.0"
) {

  def withJournalDo[F[_] : Sync, A](cliOpts: CliOpts, f: Journal => F[A]): F[A] =
    for {
      jp <- journalFilePathFromOpts(cliOpts)
      j <- readJournalFile(cliOpts.inputOpts, jp.toString).value
      result <- j.fold(
        value => Sync[F].raiseError[A](new RuntimeException(value)),
        value => f(value)
      )
    } yield result

  def evalCmd(run: Commands.CLIRun): IO[Unit] = {
    run.command match {
      case Commands.Add => ().pure
      case Commands.Balance => withJournalDo[IO, Unit](run.options, balance(run.options, _))
    }
  }

  override def main: Opts[IO[ExitCode]] = {
    Commands.parseCliRun.map { run =>
      evalCmd(run)
        .handleErrorWith(f => Console[IO].errorln(s"sledger: ${f.getMessage}"))
        .as(ExitCode.Success)
    }
  }
}

