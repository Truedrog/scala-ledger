package sledger.cli

import cats.syntax.all._
import com.monovore.decline.Opts
import sledger.cli.CliOptions.{CliOpts, defcliopts, flatTreeFlag, rawToCliOpts, reportSpecFlags}

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
      val spec = reportSpecFlags

      val opts = rawToCliOpts

      (opts,  treeOrFlat, spec)
        .mapN { (opts, tof, rspec) =>
          opts.copy(reportSpec = rspec
            .copy(options = opts.reportSpec.options.copy(accountListMode = tof)))
        }
    }.map(opts => CLIRun(Balance, opts))

    val add = Opts.subcommand(
      name = "add",
      help = "Prompt for transactions and add them to the journal."
    )(rawToCliOpts).map(opts => CLIRun(Add, opts))

    balance orElse add
  }
}
