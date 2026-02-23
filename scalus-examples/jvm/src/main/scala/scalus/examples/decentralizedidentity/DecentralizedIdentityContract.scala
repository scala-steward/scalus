package scalus.examples.decentralizedidentity

import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release

lazy val DecentralizedIdentityContract =
    PlutusV3.compile(DecentralizedIdentityValidator.validate)
