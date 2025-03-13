package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.given
import scalus.uplc.Term

class JITSpec extends AnyFunSuiteLike {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("UPLC JIT compilation works") {
        val uplc: Term = compile:
            ((i: BigInt) => if i > 0 then throw new Exception("Not implemented") else i + 1)(2)
//            throw new Exception("Not implemented")
        .toUplc()

        println(uplc.showHighlighted)
        println(JIT.jitUplc(uplc)())
    }
}
