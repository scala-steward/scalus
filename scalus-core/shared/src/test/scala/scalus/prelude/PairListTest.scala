package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.PairList
import scalus.cardano.onchain.plutus.prelude.PairList.*
import scalus.testing.kit.EvalTestKit

class PairListTest extends AnyFunSuite with EvalTestKit {

    test("head") {
        assertEvalEq(PairList.single(BigInt(1), BigInt(2)).head, (BigInt(1), BigInt(2)))

        assertEvalEq(
          PairCons((BigInt(1), BigInt(2)), PairCons((BigInt(3), BigInt(4)), PairNil)).head,
          (BigInt(1), BigInt(2))
        )

        assertEvalFails[NoSuchElementException](PairList.empty[BigInt, BigInt].head)
    }
}
