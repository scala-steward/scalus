package scalus.uplc.eval

import scalus.builtin.{Builtins, ByteString, Data, given}
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.TermDSL.given
import scalus.uplc.{Constant, DefaultFun, Term}
import scalus.*
import scalus.Compiler.compile
import scalus.sir.SIR
import scalus.uplc.Constant.toValue
import scalus.utils.Utils.lowerFirst

import scala.quoted.*

object JIT {
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

    val uplc: Term = compile:
        ((i: BigInt) => i + 1)(2)
    .toUplc()

    def embed(x: Term)(using Quotes): Expr[Any] = {
        import quotes.reflect.{Lambda, MethodType, Symbol, ValDef, TypeRepr, asTerm, Ref, Select, Flags}
        import scalus.builtin.given

        given ByteStringToExpr: ToExpr[ByteString] with {
            def apply(x: ByteString)(using Quotes): Expr[ByteString] =
                '{ ByteString.fromArray(${ Expr(x.bytes) }) }
        }

        given DataToExpr: ToExpr[Data] with {
            def apply(x: Data)(using Quotes): Expr[Data] = x match
                case Data.Constr(tag, args) =>
                    val tagExpr = Expr(tag)
                    val argsExpr = Expr.ofList(args.map(apply))
                    '{ Data.Constr($tagExpr, $argsExpr) }
                case Data.List(value) =>
                    val valueExpr = Expr.ofList(value.map(apply))
                    '{ Data.List($valueExpr) }
                case Data.Map(values) =>
                    val argsListOfExprTuple = values.map { case (k, v) =>
                        Expr.ofTuple(apply(k), apply(v))
                    }
                    val argsExpr = Expr.ofList(argsListOfExprTuple)
                    '{ Data.Map($argsExpr) }
                case Data.I(value) => '{ Data.I(${ Expr(value) }) }
                case Data.B(value) => '{ Data.B(${ Expr(value) }) }
        }

        def asdf(x: Term, env: List[(String, quotes.reflect.Term)], owner: Symbol): Expr[Any] =
            x match
                case Term.Var(name) =>
                    env.find(_._1 == name.name).get._2.asExprOf[Any]
                case Term.LamAbs(name, term) =>
                    val mtpe =
                        MethodType(List(name))(_ => List(TypeRepr.of[Any]), _ => TypeRepr.of[Any])
                    Lambda(
                      owner,
                      mtpe,
                      { case (methSym, List(arg1: quotes.reflect.Term)) =>
                          asdf(term, (name -> arg1) :: env, methSym).asTerm
                      }
                    ).asExprOf[Any]
                case Term.Apply(f, arg) =>
                    val func = asdf(f, env, owner)
                    val a = asdf(arg, env, owner)
                    '{ ${ func }.asInstanceOf[Any => Any].apply($a) } // TODO: beta-reduce
                case Term.Force(term) =>
                    println("Force")
                    '{ ${ asdf(term, env, owner) }.asInstanceOf[() => Any]() }
                case Term.Delay(term) =>
                    println("Delay")
                    '{ () => ${ asdf(term, env, owner) } }
                case Term.Const(const) =>
                    println(s"Const: $const")
                    const match
                        case Constant.Integer(value)              => Expr(value)
                        case Constant.ByteString(value)           => Expr(value)
                        case Constant.String(value)               => Expr(value)
                        case Constant.Unit                        => '{ () }
                        case Constant.Bool(value)                 => Expr(value)
                        case Constant.Data(value)                 => Expr(value)
                        case Constant.List(elemType, value)       => ???
                        case Constant.Pair(a, b)                  => ???
                        case Constant.BLS12_381_G1_Element(value) => ???
                        case Constant.BLS12_381_G2_Element(value) => ???
                        case Constant.BLS12_381_MlResult(value)   => ???
                case Term.Builtin(DefaultFun.AddInteger)    => '{ Builtins.addInteger.curried }
                case Term.Builtin(DefaultFun.EqualsData)    => '{ Builtins.equalsData.curried }
                case Term.Builtin(DefaultFun.EqualsInteger) => '{ Builtins.equalsInteger.curried }
                case Term.Builtin(DefaultFun.EqualsByteString) =>
                    '{ Builtins.equalsByteString.curried }
                case Term.Builtin(DefaultFun.IfThenElse)   => '{ Builtins.ifThenElse.curried }
                case Term.Builtin(DefaultFun.Trace)        => '{ Builtins.trace }
                case Term.Builtin(DefaultFun.FstPair)      => '{ Builtins.fstPair }
                case Term.Builtin(DefaultFun.SndPair)      => '{ Builtins.sndPair }
                case Term.Builtin(DefaultFun.ChooseList)   => '{ Builtins.chooseList.curried }
                case Term.Builtin(DefaultFun.Sha2_256)     => '{ Builtins.sha2_256(using _) }
                case Term.Builtin(DefaultFun.HeadList)     => '{ Builtins.headList }
                case Term.Builtin(DefaultFun.TailList)     => '{ Builtins.tailList }
                case Term.Builtin(DefaultFun.UnConstrData) => '{ Builtins.unConstrData }
                case Term.Builtin(DefaultFun.UnListData)   => '{ Builtins.unListData }
                case Term.Builtin(DefaultFun.UnIData)      => '{ Builtins.unIData }
                case Term.Builtin(DefaultFun.UnBData)      => '{ Builtins.unBData }
                /*case Term.Builtin(bn) =>
                        val methodName = lowerFirst(bn.name())
                        val expr = Select.unique('{ Builtins }.asTerm, methodName).etaExpand(Symbol.spliceOwner).asExprOf[Any]
                        '{
                            $expr match
                                case expr: Function1[_, _]             => expr
                                case expr: Function2[_, _, _]          => expr.curried
                                case expr: Function3[_, _, _, _]       => expr.curried
                                case expr: Function4[_, _, _, _, _]    => expr.curried
                                case expr: Function5[_, _, _, _, _, _] => expr.curried
                                case expr => throw new Exception(s"Unsupported arity: $expr")
                        }*/
                case Term.Error =>
                    println("Error")
                    '{ throw new Exception("Error") }
                case Term.Constr(tag, args) =>
                    Expr.ofTuple(Expr(tag) -> Expr.ofList(args.map(a => asdf(a, env, owner))))
                case Term.Case(arg, cases) =>
                    val constr = asdf(arg, env, owner).asExprOf[(Long, List[Any])]
                    val caseFuncs = Expr.ofList(cases.map(c => asdf(c, env, owner).asExprOf[Any => Any]))
                    '{
                        val (tag, args) = $constr
                        args.foldLeft($caseFuncs(tag.toInt))((f, a) =>
                            f(a).asInstanceOf[Any => Any]
                        )
                    }
        asdf(x, Nil, Symbol.spliceOwner)
    }

    def jitUplc(term: Term): Any = staging.run { (quotes: Quotes) ?=>
        val expr = embed(term)
        println(expr.show)
        expr
    }
}
