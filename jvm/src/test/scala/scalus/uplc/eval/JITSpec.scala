package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.builtin.{Builtins, given}
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.TermDSL.given
import scalus.uplc.{Constant, DefaultFun, Term}
import scalus.*
import scalus.Compiler.compile
import scalus.uplc.Constant.toValue

import scala.quoted.*

class JITSpec extends AnyFunSuiteLike {
    private val v3vm = PlutusVM.makePlutusV3VM()
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

    val uplc: Term = compile:
        ((i: BigInt) => i + 1)(2)
    .toUplc()

    private def embed(x: Term)(using Quotes): Expr[Any] =
        import quotes.reflect.{Lambda, MethodType, Symbol, ValDef, TypeRepr, asTerm, Ref}
        def asdf(x: Term, env: List[(String, quotes.reflect.Term)]): Expr[Any] =
            x match
                case Term.Var(name) => env.find(_._1 == name.name).get._2.asExprOf[Any]
                case Term.LamAbs(name, term) =>
                    val mtpe =
                        MethodType(List(name))(_ => List(TypeRepr.of[Any]), _ => TypeRepr.of[Any])
                    Lambda(
                      Symbol.spliceOwner,
                      mtpe,
                      { case (methSym, List(arg1: quotes.reflect.Term)) =>
                          asdf(term, (name -> arg1) :: env).asTerm
                      }
                    ).asExprOf[Any]
                case Term.Apply(f, arg) =>
                    val func = asdf(f, env).asInstanceOf[Expr[Any => Any]]
                    val a = asdf(arg, env)
                    '{ ${ func }.apply($a) } // TODO: beta-reduce
                case Term.Force(term)                    => asdf(term, env)
                case Term.Delay(term)                    => '{ () => ${ asdf(term, env) } }
                case Term.Const(Constant.Integer(i))     => Expr(i)
                case Term.Builtin(DefaultFun.AddInteger) => '{ Builtins.addInteger.curried }
                case Term.Builtin(DefaultFun.IfThenElse) => '{ Builtins.ifThenElse.curried }
                case Term.Error                          => '{ throw new Exception("Error") }
                case Term.Constr(tag, args) =>
                    Expr.ofTuple(Expr(tag) -> Expr.ofList(args.map(a => asdf(a, env))))
                case Term.Case(arg, cases) =>
                    val constr = asdf(arg, env).asExprOf[(Long, List[Any])]
                    val caseFuncs = Expr.ofList(cases.map(c => asdf(c, env).asExprOf[Any => Any]))
                    '{
                        val (tag, args) = $constr
                        args.foldLeft($caseFuncs(tag.toInt))((f, a) => f(a).asInstanceOf[Any => Any])
                    }
                    Expr(1)
        asdf(x, Nil)

    val jit: Any = staging.run { (quotes: Quotes) ?=>
        val expr = embed(uplc)
        println(expr.show)
        expr
    }

    test("UPLC JIT compilation works") {
        println(uplc.showHighlighted)
        println(jit)
    }
}
