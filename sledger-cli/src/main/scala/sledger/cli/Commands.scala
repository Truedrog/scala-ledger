package sledger.cli

import cats.syntax.all._
import com.monovore.decline.Opts
import sledger.cli.CliOptions.{CliOpts, flatTreeFlag, generalOpts, mapOptsToReportSpec}

object Commands {
  final case class CLIRun(
                           command: CLICommand,
                           options: CliOpts,
                         )

  sealed trait CLICommand

  case object Add extends CLICommand

  case object Balance extends CLICommand

  val parseCliRun: Opts[CLIRun] = {
    val balance = Opts.subcommand(
      name = "balance",
      help = "Balance command"
    ) {
      val treeOrFlat = flatTreeFlag(true)
      val spec = mapOptsToReportSpec
      val opts = generalOpts

      (opts, treeOrFlat, spec)
        .mapN { (opts, tof, rspec) =>
          opts.copy(reportSpec = rspec
            .copy(options = rspec.options.copy(accountListMode = tof)))
        }
    }.map(opts => CLIRun(Balance, opts))

    val add = Opts.subcommand(
      name = "add",
      help = "Prompt for transactions and add them to the journal."
    )(generalOpts).map(opts => CLIRun(Add, opts))

    balance orElse add
  }
}
