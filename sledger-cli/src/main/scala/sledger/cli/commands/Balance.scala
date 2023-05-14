package sledger.cli.commands

import cats.effect._
import cats.effect.std.Console
import cats.syntax.all._
import sledger.cli.CliOptions.CliOpts
import sledger.data.Journals.Journal


object Balance {
 
  def balance[F[_]: Sync: Console](cliOps: CliOpts, journal: Journal): F[Unit] = {
    Console[F].println("DOING BALANCE with options, and a journal:")
//    Console[F].println(cliOps, journal)
  }
}
