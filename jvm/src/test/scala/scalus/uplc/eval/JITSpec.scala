package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.builtin.{Builtins, given}
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.TermDSL.given
import scalus.uplc.{Constant, DefaultFun, Term}
import scalus.*
import scalus.Compiler.compile
import scalus.sir.SIR
import scalus.uplc.Constant.toValue
import scalus.utils.Utils.lowerFirst

import scala.quoted.*

class JITSpec extends AnyFunSuiteLike {
    private val v3vm = PlutusVM.makePlutusV3VM()

    test("UPLC JIT compilation works") {
        println(JIT.uplc.showHighlighted)
        println(JIT.jitUplc(JIT.uplc))
    }
}
