package scalus.examples.linkedlist

import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release

lazy val LinkedListContract = PlutusV3.compile(LinkedListValidator.validate)
