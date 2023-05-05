package sledger.data

import sledger.data.Balancing.{BalancingOptions, defBalancingOptions}

object InputOptions {

  case class InputOpts(balancingOptions: BalancingOptions,
                      )
  val defInputOpts = InputOpts(balancingOptions = defBalancingOptions)
}
