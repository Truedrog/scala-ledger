package sledger.cli

import cats.syntax.all._
import com.monovore.decline.Opts
import sledger.cli.CliOptions.{CliOpts, rawToCliOpts}

object Commands {
  final case class CLIRun(
                           command: CLICommand,
                           options: CliOpts,
                         )

  sealed trait CLICommand

  case object Add extends CLICommand

  case object Balance extends CLICommand

  private val parseCliCommand: Opts[CLICommand] = {
    val balance = Opts.subcommand(
      name = "balance",
      help = "Balance command"
    )(Opts.unit).map(_ => Balance)

    val add = Opts.subcommand(
      name = "Add",
      help = "Prompt for transactions and add them to the journal."
    )(Opts.unit).map(_ => Add)

    balance orElse add
  }

  val parseCliRun: Opts[CLIRun] =
    (
      parseCliCommand,
      rawToCliOpts
    ).mapN(CLIRun.apply)
}
