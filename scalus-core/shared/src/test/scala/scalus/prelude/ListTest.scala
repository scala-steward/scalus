package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
import scalus.cardano.onchain.plutus.prelude.Option.{None, Some}
import scalus.cardano.onchain.plutus.prelude.{asScalus, identity, Eq, List, Option, Ord, Order, SortedMap}
import scalus.cardano.ledger.ExUnits
import scalus.testing.kit.EvalTestKit

class ListTest extends AnyFunSuite with EvalTestKit {

    test("empty") {
        assertEvalWithBudget(List.empty[BigInt], Nil, ExUnits(200, 16100))

        assert(scala.List.empty[BigInt].asScalus === List.empty[BigInt])
        assert(List.empty[BigInt].asScala == scala.List.empty[BigInt])
    }

    test("single") {
        check { (value: BigInt) =>
            val scalusResult = List.single(value)
            val scalaResult = scala.List(value)

            scalusResult === Cons(
              value,
              Nil
            ) && scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.single(BigInt(1)),
          Cons(BigInt(1), Nil),
          ExUnits(memory = 1264, steps = 263761)
        )
    }

    test("apply") {
        check { (seq: scala.Seq[BigInt]) =>
            val scalusResult = List(seq*)
            val scalaResult = scala.List(seq*)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List(BigInt(1), BigInt(2), BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(2496, 585186)
        )
    }

    test("apply list of pairs") {
        assertEvalWithBudget(
          List((BigInt(1), BigInt(2)), (BigInt(3), BigInt(4))),
          Cons((BigInt(1), BigInt(2)), Cons((BigInt(3), BigInt(4)), Nil)),
          ExUnits(4656, 1166574)
        )
    }

    test("from IterableOnce") {
        check { (seq: scala.Seq[BigInt]) =>
            val scalusResult = List.from(seq)
            val scalaResult = scala.List.from(seq)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("from java.lang.Iterable") {
        check { (seq: scala.Seq[BigInt]) =>
            import scala.jdk.CollectionConverters.*

            val scalusResult = List.from(seq.asJava)
            val scalaResult = scala.List.from(seq)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("range") {
        forAll(bigIntRangeGen) { (start: BigInt, end: BigInt) =>
            val scalusResult = List.range(start, end - 1)
            val scalaResult = scala.List.range(start, end)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(List.range(0, 0), List.single(BigInt(0)), ExUnits(8702, 1804040))
        assertEvalWithBudget(
          List.range(1, 3),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(16638, 3669550)
        )
        assertEvalWithBudget(List.range(0, -1), List.empty[BigInt], ExUnits(4734, 871285))
    }

    test("rangeUntil") {
        forAll(bigIntRangeGen) { (start: BigInt, end: BigInt) =>
            val scalusResult = List.rangeUntil(start, end)
            val scalaResult = scala.List.range(start, end)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(List.rangeUntil(0, 1), List.single(BigInt(0)), ExUnits(8702, 1806946))
        assertEvalWithBudget(
          List.rangeUntil(1, 4),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(16638, 3675362)
        )
        assertEvalWithBudget(List.rangeUntil(0, 0), List.empty[BigInt], ExUnits(4734, 872738))
        assertEvalWithBudget(List.rangeUntil(0, -1), List.empty[BigInt], ExUnits(4734, 872738))
    }

    test("fill") {
        val generator: Gen[(BigInt, BigInt)] =
            for
                value <- Arbitrary.arbitrary[BigInt]
                times <- bigIntGen
            yield (value, times)

        forAll(generator) { (value: BigInt, times: BigInt) =>
            val scalusResult = List.fill(value, times)
            val scalaResult = scala.List.fill(times.toInt)(value)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(
          List.fill(BigInt(1), 1),
          List.single(BigInt(1)),
          ExUnits(7638, 1616348)
        )

        assertEvalWithBudget(
          List.fill(BigInt(1), 3),
          Cons(BigInt(1), Cons(BigInt(1), Cons(BigInt(1), Nil))),
          ExUnits(14510, 3294166)
        )

        assertEvalWithBudget(List.fill(BigInt(1), 0), List.empty[BigInt], ExUnits(4202, 777439))

        assertEvalWithBudget(List.fill(BigInt(1), -1), List.empty[BigInt], ExUnits(4202, 777439))
    }

    test("map2") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = List.map2(list1, list2)(_ + _)
            val scalaResult = list1.asScala.zip(list2.asScala).map { case (a, b) => a + b }

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        check { (list: List[BigInt]) =>
            List.map2(list, List.empty[BigInt])(_ + _) === List.empty[BigInt] &&
            List.map2(List.empty[BigInt], list)(_ + _) === List.empty[BigInt]
        }

        assertEvalWithBudget(
          List.map2(Cons(BigInt(1), Cons(BigInt(2), Nil)), List.empty[BigInt])(_ + _),
          List.empty[BigInt],
          ExUnits(6328, 1386812)
        )

        assertEvalWithBudget(
          List.map2(List.empty[BigInt], Cons(BigInt(3), Cons(BigInt(4), Nil)))(_ + _),
          List.empty[BigInt],
          ExUnits(6196, 1237818)
        )

        assertEvalWithBudget(
          List.map2(Cons(BigInt(1), Cons(BigInt(2), Nil)), Cons(BigInt(3), Cons(BigInt(4), Nil)))(
            _ + _
          ),
          Cons(BigInt(4), Cons(BigInt(6), Nil)),
          ExUnits(22504, 5530484)
        )
    }

    test("ToData <-> FromData") {
        check { (list: List[BigInt]) =>
            val data = list.toData
            val fromDataList = fromData[List[BigInt]](data)

            fromDataList === list
        }

        assertEvalWithBudget(
          fromData[List[BigInt]](List.empty[BigInt].toData),
          List.empty[BigInt],
          ExUnits(3364, 571885)
        )

        assertEvalWithBudget(
          fromData[List[BigInt]](List.single(BigInt(1)).toData),
          List.single(BigInt(1)),
          ExUnits(memory = 4328, steps = 803546)
        )

        assertEvalWithBudget(
          fromData[List[BigInt]](Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))).toData),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(4960, 1028971)
        )
    }

    test("Eq") {
        check { (list: List[BigInt]) => list === list }

        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 === list2
            val scalaResult = list1.asScala == list2.asScala

            scalusResult === scalaResult
        }

        assertEvalWithBudget(List.empty[BigInt], List.empty[BigInt], ExUnits(200, 16100))

        assertEvalWithBudget(
          List.single(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 1264, steps = 263761)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(1896, 489186)
        )

        assertEvalNotEq(
          List.empty[BigInt],
          List.single(BigInt(1))
        )

        assertEvalNotEq(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Cons(BigInt(0), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("Ord") {
        check { (list: List[BigInt]) => (list <=> list).isEqual }

        check { (list1: List[BigInt], list2: List[BigInt]) =>
            import scala.Ordering.Implicits.given

            val scalusResult = list1 <=> list2
            val scalaResult =
                summon[Ordering[scala.Seq[BigInt]]].compare(list1.asScala, list2.asScala) match
                    case 0          => Order.Equal
                    case x if x < 0 => Order.Less
                    case _          => Order.Greater

            scalusResult === scalaResult
        }

        assertEval((List.empty[BigInt] <=> List.empty[BigInt]).isEqual)

        assertEval((List.single(BigInt(1)) <=> List.single(BigInt(1))).isEqual)

        assertEval(
          (
            Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))) <=>
                Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          ).isEqual
        )

        assertEval((List.empty[BigInt] <=> List.single(BigInt(1))).isLess)

        assertEval((List.single(BigInt(1)) <=> List.single(BigInt(2))).isLess)

        assertEval(
          (
            Cons(BigInt(1), Cons(BigInt(2), Nil)) <=>
                Cons(BigInt(1), Cons(BigInt(3), Nil))
          ).isLess
        )

        assertEval((List.single(BigInt(1)) <=> List.empty[BigInt]).isGreater)

        assertEval((List.single(BigInt(2)) <=> List.single(BigInt(1))).isGreater)

        assertEval(
          (
            Cons(BigInt(1), Cons(BigInt(3), Nil)) <=>
                Cons(BigInt(1), Cons(BigInt(2), Nil))
          ).isGreater
        )
    }

    test("quicksort") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.quicksort
            val scalaResult = list.asScala.sorted

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].quicksort,
          List.empty[BigInt],
          ExUnits(7932, 1381094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).quicksort,
          List.single(BigInt(1)),
          ExUnits(memory = 30010, steps = 6_463525)
        )

        assertEvalWithBudget(
          Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil))).quicksort,
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(152079, 35603643)
        )
    }

    test("flatten") {
        given [T: Arbitrary]: Arbitrary[List[T]] = Arbitrary {
            for {
                size <- Gen.choose(0, 10)
                list <- Gen.listOfN(size, Arbitrary.arbitrary[T])
            } yield list.asScalus
        }

        check { (list: List[List[BigInt]]) =>
            val scalusResult = list.flatten
            val scalaResult = list.asScala.flatMap(_.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[List[BigInt]].flatten,
          List.empty[BigInt],
          ExUnits(7596, 1376879)
        )

        assertEvalWithBudget(
          List.single(Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil)))).flatten,
          Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil))),
          ExUnits(memory = 19582, steps = 4_224796)
        )

        assertEvalWithBudget(
          Cons[List[BigInt]](
            Cons(BigInt(1), Cons(BigInt(2), Nil)),
            List.single(List.single(BigInt(3)))
          ).flatten,
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 36892, steps = 8_560258)
        )
    }

    test("isEmpty") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.isEmpty
            val scalaResult = list.asScala.isEmpty

            scalusResult === scalaResult
        }

        assertEval(List.empty[BigInt].isEmpty)
        assertEval(!List.single(BigInt(1)).isEmpty)
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).isEmpty)
    }

    test("nonEmpty") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.nonEmpty
            val scalaResult = list.asScala.nonEmpty

            scalusResult === scalaResult
        }

        assertEval(!List.empty[BigInt].nonEmpty)
        assertEval(List.single(BigInt(1)).nonEmpty)
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).nonEmpty)
    }

    test("isDefinedAt") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = list.isDefinedAt(index)
            val scalaResult = list.asScala.isDefinedAt(index.toInt)

            assert(scalusResult === scalaResult)
        }

        assertEval(!List.empty[BigInt].isDefinedAt(0))
        assertEval(List.single(BigInt(1)).isDefinedAt(0))
        assertEval(!List.single(BigInt(1)).isDefinedAt(1))
        assertEval(!List.single(BigInt(1)).isDefinedAt(-1))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).isDefinedAt(0))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).isDefinedAt(1))
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).isDefinedAt(2))
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).isDefinedAt(-1))
    }

    test("get") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = list.get(index)
            val scalaResult = list.asScala.lift(index.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(List.empty[BigInt].get(0), None, ExUnits(7266, 1412584))
        assertEvalWithBudget(
          List.single(BigInt(1)).get(0),
          Some(BigInt(1)),
          ExUnits(memory = 9696, steps = 2_152139)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).get(1),
          None,
          ExUnits(memory = 11898, steps = 2_664492)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).get(-1),
          None,
          ExUnits(memory = 5398, steps = 1_063251)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).get(0),
          Some(BigInt(1)),
          ExUnits(9896, 2241202)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).get(1),
          Some(BigInt(2)),
          ExUnits(13864, 3309449)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).get(2),
          None,
          ExUnits(16066, 3821802)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).get(-1),
          None,
          ExUnits(5598, 1152314)
        )
    }

    test("at") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = liftThrowableToOption(list.at(index))
            val scalaResult = list.asScala.lift(index.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].at(0))
        assertEvalWithBudget(
          List.single(BigInt(1)).at(0),
          BigInt(1),
          ExUnits(memory = 9664, steps = 2_078370)
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(-1))
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(0),
          BigInt(1),
          ExUnits(memory = 9864, steps = 2_167433)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(1),
          BigInt(2),
          ExUnits(memory = 13832, steps = 3_235680)
        )
        assertEvalFails[NoSuchElementException](Cons(BigInt(1), Cons(BigInt(2), Nil)).at(2))
        assertEvalFails[NoSuchElementException](Cons(BigInt(1), Cons(BigInt(2), Nil)).at(-1))
    }

    test("!!") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = liftThrowableToOption(list.!!(index))
            val scalaResult = list.asScala.lift(index.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].!!(0))
        assertEvalWithBudget(
          List.single(BigInt(1)).!!(0),
          BigInt(1),
          ExUnits(memory = 9964, steps = 2_126370)
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(-1))
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(0),
          BigInt(1),
          ExUnits(memory = 10164, steps = 2_215433)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(1),
          BigInt(2),
          ExUnits(memory = 14132, steps = 3_283680)
        )
        assertEvalFails[NoSuchElementException](Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(2))
        assertEvalFails[NoSuchElementException](Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(-1))
    }

    test("contains") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.contains(value)
            val scalaResult = list.asScala.contains(value)

            scalusResult === scalaResult
        }

        assertEval(!List.empty[BigInt].contains(BigInt(1)))
        assertEval(List.single(BigInt(1)).contains(BigInt(1)))
        assertEval(!List.single(BigInt(1)).contains(BigInt(2)))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).contains(BigInt(2)))
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).contains(BigInt(3)))
    }

    test("groupBy") {

        check { (list: List[BigInt]) =>
            val scalusResult = list.groupBy(_ % 2)
            val scalaResult = list.asScala.groupBy(_ % 2)

            scalusResult.size == scalaResult.size && scalaResult.forall { case (key, scalaList) =>
                scalusResult.get(key) match {
                    case Some(scalusList) => scalusList.quicksort === scalaList.asScalus.quicksort
                    case None             => fail(s"Key $key not found in scalusResult")
                }
            }
        }

        assertEvalWithBudget(
          List.empty[BigInt].groupBy(_ % 2),
          SortedMap.empty[BigInt, List[BigInt]],
          ExUnits(memory = 16360, steps = 3_003203)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).groupBy(_ % 2),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          ExUnits(memory = 66643, steps = 15_631759)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).groupBy(_ % 2),
          SortedMap.unsafeFromList(
            Cons(
              (BigInt(0), List.single(BigInt(2))),
              Cons((BigInt(1), List.single(BigInt(1))), Nil)
            )
          ),
          ExUnits(memory = 128718, steps = 31_463493)
        )

    }

    test("groupMap") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.groupMap(_ % 2)(identity)
            val scalaResult = list.asScala.groupMap(_ % 2)(identity)

            scalusResult.size == scalaResult.size && scalaResult.forall { case (key, scalaList) =>
                scalusResult.get(key) match {
                    case Some(scalusList) => scalusList.quicksort === scalaList.asScalus.quicksort
                    case None             => fail(s"Key $key not found in scalusResult")
                }
            }
        }

        assertEvalWithBudget(
          List.empty[BigInt].groupMap(_ % 2)(identity),
          SortedMap.empty[BigInt, List[BigInt]],
          ExUnits(memory = 15460, steps = 2_859203)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).groupMap(_ % 2)(identity),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          ExUnits(memory = 66807, steps = 15_683802)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).groupMap(_ % 2)(identity),
          SortedMap.unsafeFromList(
            Cons(
              (BigInt(0), List.single(BigInt(2))),
              Cons((BigInt(1), List.single(BigInt(1))), Nil)
            )
          ),
          ExUnits(memory = 129946, steps = 31_711579)
        )
    }

    test("groupMapReduce") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.groupMapReduce(_ % 2)(identity)(_ + _)
            val scalaResult = list.asScala.groupMapReduce(_ % 2)(identity)(_ + _)

            scalusResult.size == scalaResult.size && scalaResult.forall { case (key, scalaValue) =>
                scalusResult.get(key) match {
                    case Some(scalusValue) => scalusValue === scalaValue
                    case None              => fail(s"Key $key not found in scalusResult")
                }
            }
        }

        assertEvalWithBudget(
          List.empty[BigInt].groupMapReduce(_ % 2)(identity)(_ + _),
          SortedMap.empty[BigInt, BigInt],
          ExUnits(10664, 1881340)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).groupMapReduce(_ % 2)(identity)(_ + _),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          ExUnits(memory = 30531, steps = 6_915789)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Cons(BigInt(4), Nil))))
              .groupMapReduce(_ % 2)(identity)(_ + _),
          SortedMap.unsafeFromList(
            Cons((BigInt(0), BigInt(6)), Cons((BigInt(1), BigInt(4)), Nil))
          ),
          ExUnits(165520, 43454724)
        )
    }

    test("zip") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.zip(list2)
            val scalaResult = list1.asScala.zip(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].zip(List.empty[BigInt]),
          List.empty[(BigInt, BigInt)],
          ExUnits(7732, 1349094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).zip(List.empty[BigInt]),
          List.empty[(BigInt, BigInt)],
          ExUnits(memory = 9128, steps = 1_777749)
        )

        assertEvalWithBudget(
          List.empty[BigInt].zip(List.single(BigInt(1))),
          List.empty[(BigInt, BigInt)],
          ExUnits(memory = 8396, steps = 1_532755)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).zip(List.single(BigInt(2))),
          List.single((BigInt(1), BigInt(2))),
          ExUnits(memory = 25988, steps = 6_829718)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).zip(Cons(BigInt(3), Cons(BigInt(4), Nil))),
          Cons((BigInt(1), BigInt(3)), Cons((BigInt(2), BigInt(4)), Nil)),
          ExUnits(53252, 15543215)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).zip(List.single(BigInt(3))),
          List.single((BigInt(1), BigInt(3))),
          ExUnits(memory = 26920, steps = 7_163775)
        )
    }

    test("prepended") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.prepended(value)
            val scalaResult = list.asScala.prepended(value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].prepended(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(1264, 263761)
        )

        assertEvalWithBudget(
          List.single(BigInt(2)).prepended(BigInt(1)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 2228, steps = 495422)
        )

        assertEvalWithBudget(
          Cons(BigInt(2), Cons(BigInt(3), Nil)).prepended(BigInt(1)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(2428, 584485)
        )
    }

    test("+:") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = value +: list
            val scalaResult = value +: list.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          BigInt(1) +: List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(1264, 263761)
        )

        assertEvalWithBudget(
          BigInt(1) +: List.single(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 2228, steps = 495422)
        )

        assertEvalWithBudget(
          BigInt(1) +: Cons(BigInt(2), Cons(BigInt(3), Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(2428, 584485)
        )
    }

    test("prependedAll") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.prependedAll(list2)
            val scalaResult = list1.asScala.prependedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].prependedAll(List.empty[BigInt]),
          List.empty[BigInt],
          ExUnits(6034, 1229192)
        )

        assertEvalWithBudget(
          List.empty[BigInt].prependedAll(List.single(BigInt(1))),
          List.single(BigInt(1)),
          ExUnits(memory = 6998, steps = 1_460853)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).prependedAll(List.empty[BigInt]),
          List.single(BigInt(1)),
          ExUnits(memory = 8030, steps = 1_753847)
        )

        assertEvalWithBudget(
          List.single(BigInt(2)).prependedAll(List.single(BigInt(1))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 11822, steps = 2_787677)
        )

        assertEvalWithBudget(
          Cons(BigInt(2), Cons(BigInt(3), Nil)).prependedAll(List.single(BigInt(1))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 12022, steps = 2_876740)
        )
    }

    test("++:") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++: list2
            val scalaResult = list1.asScala ++: list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt] ++: List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(6034, 1229192)
        )

        assertEvalWithBudget(
          List.empty[BigInt] ++: List.single(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 8030, steps = 1_753847)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) ++: List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 6998, steps = 1_460853)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) ++: List.single(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 11822, steps = 2_787677)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)) ++: List.single(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 14850, steps = 3_678909)
        )
    }

    test("appended") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.appended(value)
            val scalaResult = list.asScala.appended(value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].appended(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 4964, steps = 973456)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).appended(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 8756, steps = 2_007286)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).appended(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 12084, steps = 2_946518)
        )
    }

    test(":+") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list :+ value
            val scalaResult = list.asScala :+ value

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt] :+ BigInt(1),
          List.single(BigInt(1)),
          ExUnits(memory = 5196, steps = 1_020755)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) :+ BigInt(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 9288, steps = 2_102585)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)) :+ BigInt(3),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 12616, steps = 3_041817)
        )
    }

    test("appendedAll") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.appendedAll(list2)
            val scalaResult = list1.asScala.appendedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].appendedAll(List.empty[BigInt]),
          List.empty[BigInt],
          ExUnits(6934, 1373192)
        )

        assertEvalWithBudget(
          List.empty[BigInt].appendedAll(List.single(BigInt(1))),
          List.single(BigInt(1)),
          ExUnits(memory = 8630, steps = 1_849847)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).appendedAll(List.empty[BigInt]),
          List.single(BigInt(1)),
          ExUnits(memory = 7598, steps = 1_556853)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).appendedAll(List.single(BigInt(2))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 12122, steps = 2_835677)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).appendedAll(List.single(BigInt(3))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 15150, steps = 3_726909)
        )
    }

    test(":++") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 :++ list2
            val scalaResult = list1.asScala :++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt] :++ List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(6934, 1373192)
        )

        assertEvalWithBudget(
          List.empty[BigInt] :++ List.single(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 8930, steps = 1_897847)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) :++ List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 7898, steps = 1_604853)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) :++ List.single(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 12722, steps = 2_931677)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)) :++ List.single(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 15750, steps = 3_822909)
        )
    }

    test("concat") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.concat(list2)
            val scalaResult = list1.asScala.concat(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].concat(List.empty[BigInt]),
          List.empty[BigInt],
          ExUnits(6934, 1373192)
        )

        assertEvalWithBudget(
          List.empty[BigInt].concat(List.single(BigInt(1))),
          List.single(BigInt(1)),
          ExUnits(memory = 8930, steps = 1_897847)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).concat(List.empty[BigInt]),
          List.single(BigInt(1)),
          ExUnits(memory = 7898, steps = 1_604853)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).concat(List.single(BigInt(2))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 12722, steps = 2_931677)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).concat(List.single(BigInt(3))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 15750, steps = 3_822909)
        )
    }

    test("++") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++ list2
            val scalaResult = list1.asScala ++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt] ++ List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(6934, 1373192)
        )

        assertEvalWithBudget(
          List.empty[BigInt] ++ List.single(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 8930, steps = 1_897847)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) ++ List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 7898, steps = 1_604853)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) ++ List.single(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 12722, steps = 2_931677)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)) ++ List.single(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 15750, steps = 3_822909)
        )
    }

    test("map") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.map(_ + value)
            val scalaResult = list.asScala.map(_ + value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].map(_ + BigInt(1)),
          List.empty[BigInt],
          ExUnits(7296, 1328879)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).map(_ + BigInt(1)),
          List.single(BigInt(2)),
          ExUnits(memory = 15018, steps = 3_167745)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).map(_ + BigInt(1)),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          ExUnits(22276, 4912013)
        )
    }

    test("flatMap") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.flatMap(x => List.single(x + value))
            val scalaResult = list.asScala.flatMap(x => scala.List(x + value))

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].flatMap(x => List.single(x + BigInt(1))),
          List.empty[BigInt],
          ExUnits(memory = 8196, steps = 1_472879)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).flatMap(x => List.single(x + BigInt(1))),
          List.single(BigInt(2)),
          ExUnits(memory = 20552, steps = 4_332837)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).flatMap(x => List.single(x + BigInt(1))),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          ExUnits(memory = 36304, steps = 8_193360)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).flatMap(x => Nil),
          List.empty[BigInt],
          ExUnits(29712, 6304459)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).flatMap(x =>
              Cons(x + BigInt(10), Cons(x + BigInt(100), Nil))
          ),
          Cons(BigInt(11), Cons(BigInt(101), Cons(BigInt(12), Cons(BigInt(102), Nil)))),
          ExUnits(40664, 9597267)
        )
    }

    test("filter") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filter(_ > value)
            val scalaResult = list.asScala.filter(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].filter(_ > BigInt(1)),
          List.empty[BigInt],
          ExUnits(7796, 1408879)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).filter(_ > BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 15054, steps = 3_116215)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).filter(_ > BigInt(1)),
          Cons(BigInt(2), Nil),
          ExUnits(22280, 4865315)
        )
    }

    test("filterNot") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filterNot(_ > value)
            val scalaResult = list.asScala.filterNot(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].filterNot(_ > BigInt(1)),
          List.empty[BigInt],
          ExUnits(8996, 1600879)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).filterNot(_ > BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 17487, steps = 3_648626)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).filterNot(_ > BigInt(1)),
          Cons(BigInt(1), Nil),
          ExUnits(25082, 5465413)
        )
    }

    test("filterMap") {

        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filterMap(x => if x > value then Some(x + value) else None)
            val scalaResult =
                list.asScala.flatMap(x => if x > value then scala.Some(x + value) else scala.None)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].filterMap(x => if x > BigInt(1) then Some(x + BigInt(1)) else None),
          List.empty[BigInt],
          ExUnits(8396, 1504879)
        )

        assertEvalWithBudget(
          List.single(BigInt(1))
              .filterMap(x => if x > BigInt(1) then Some(x + BigInt(1)) else None),
          List.empty[BigInt],
          ExUnits(memory = 18452, steps = 3_961231)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).filterMap(x =>
              if x > BigInt(1) then Some(x + BigInt(1)) else None
          ),
          Cons(BigInt(3), Nil),
          ExUnits(30006, 7097358)
        )

    }

    test("find") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.find(_ > value)
            val scalaResult = list.asScala.find(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(List.empty[BigInt].find(_ > BigInt(1)), None, ExUnits(5764, 1051245))

        assertEvalWithBudget(
          List.single(BigInt(1)).find(_ > BigInt(1)),
          None,
          ExUnits(memory = 11158, steps = 2_410796)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).find(_ > BigInt(1)),
          Some(BigInt(2)),
          ExUnits(13688, 3232604)
        )
    }

    test("findMap") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.findMap(x => if x > value then Some(x + value) else None)
            val scalaResult = list.asScala.find(_ > value).map(_ + value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].findMap(x => if x > BigInt(1) then Some(x + BigInt(1)) else None),
          None,
          ExUnits(6264, 1131245)
        )

        assertEvalWithBudget(
          List.single(BigInt(1))
              .findMap(x => if x > BigInt(1) then Some(x + BigInt(1)) else None),
          None,
          ExUnits(memory = 15758, steps = 3_599910)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).findMap(x =>
              if x > BigInt(1) then Some(x + BigInt(1)) else None
          ),
          Some(BigInt(3)),
          ExUnits(22358, 5542038)
        )
    }

    test("foldLeft") {
        check { (list: List[BigInt], initial: BigInt, value: BigInt) =>
            val scalusResult = list.foldLeft(initial)(_ + _)
            val scalaResult = list.asScala.foldLeft(initial)(_ + _)

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].foldLeft(BigInt(0))(_ + _),
          BigInt(0),
          ExUnits(4964, 921838)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).foldLeft(BigInt(0))(_ + _),
          BigInt(1),
          ExUnits(memory = 13222, steps = 2_745301)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).foldLeft(BigInt(0))(_ + _),
          BigInt(3),
          ExUnits(20716, 4426166)
        )
    }

    test("foldRight") {
        check { (list: List[BigInt], initial: BigInt, value: BigInt) =>
            val scalusResult = list.foldRight(initial)(_ + _)
            val scalaResult = list.asScala.foldRight(initial)(_ + _)

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].foldRight(BigInt(0))(_ + _),
          BigInt(0),
          ExUnits(4964, 921838)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).foldRight(BigInt(0))(_ + _),
          BigInt(1),
          ExUnits(memory = 11422, steps = 2_457301)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).foldRight(BigInt(0))(_ + _),
          BigInt(3),
          ExUnits(17116, 3850166)
        )
    }

    test("exists") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.exists(_ > value)
            val scalaResult = list.asScala.exists(_ > value)

            scalusResult === scalaResult
        }

        assertEval(!List.empty[BigInt].exists(_ > BigInt(1)))
        assertEval(List.single(BigInt(1)).exists(_ > BigInt(0)))
        assertEval(!List.single(BigInt(1)).exists(_ > BigInt(1)))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).exists(_ > BigInt(1)))
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).exists(_ > BigInt(2)))
    }

    test("forall") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.forall(_ > value)
            val scalaResult = list.asScala.forall(_ > value)

            scalusResult === scalaResult
        }

        assertEval(List.empty[BigInt].forall(_ > BigInt(1)))
        assertEval(List.single(BigInt(1)).forall(_ > BigInt(0)))
        assertEval(!List.single(BigInt(1)).forall(_ > BigInt(1)))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).forall(_ > BigInt(0)))
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).forall(_ > BigInt(2)))
    }

    test("count") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.count(_ > value)
            val scalaResult = BigInt(list.asScala.count(_ > value))

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].count(_ > BigInt(1)),
          BigInt(0),
          ExUnits(6964, 1241838)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).count(_ > BigInt(0)),
          BigInt(1),
          ExUnits(memory = 16424, steps = 3_378640)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).count(_ > BigInt(1)),
          BigInt(0),
          ExUnits(memory = 16022, steps = 3_213432)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).count(_ > BigInt(1)),
          BigInt(1),
          ExUnits(24718, 5207636)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).count(_ > BigInt(2)),
          BigInt(0),
          ExUnits(24316, 5042428)
        )
    }

    test("indexOfOption") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.indexOfOption(value)
            val scalaResult = list.asScala.indexOf(value) match {
                case -1    => None
                case index => Some(BigInt(index))
            }

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].indexOfOption(BigInt(1)),
          None,
          ExUnits(6664, 1195245)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).indexOfOption(BigInt(1)),
          Some(BigInt(0)),
          ExUnits(memory = 10690, steps = 2_231587)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).indexOfOption(BigInt(2)),
          None,
          ExUnits(memory = 12892, steps = 2_811791)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).indexOfOption(BigInt(2)),
          Some(BigInt(1)),
          ExUnits(16454, 3753535)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).indexOfOption(BigInt(3)),
          None,
          ExUnits(18656, 4333739)
        )
    }

    test("indexOf") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.indexOf(value)
            val scalaResult = BigInt(list.asScala.indexOf(value))

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].indexOf(BigInt(1)),
          BigInt(-1),
          ExUnits(11562, 2278854)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).indexOf(BigInt(1)),
          BigInt(0),
          ExUnits(memory = 16052, steps = 3_604338)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).indexOf(BigInt(2)),
          BigInt(-1),
          ExUnits(memory = 17790, steps = 3_895400)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).indexOf(BigInt(2)),
          BigInt(1),
          ExUnits(21816, 5126286)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).indexOf(BigInt(3)),
          BigInt(-1),
          ExUnits(23554, 5417348)
        )
    }

    test("lastOption") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.lastOption
            val scalaResult = list.asScala.lastOption

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(List.empty[BigInt].lastOption, None, ExUnits(5264, 971245))

        assertEvalWithBudget(
          List.single(BigInt(1)).lastOption,
          Some(BigInt(1)),
          ExUnits(memory = 8557, steps = 2_001124)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).lastOption,
          Some(BigInt(2)),
          ExUnits(12886, 3235550)
        )
    }

    test("last") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.last)
            val scalaResult = liftThrowableToOption(list.asScala.last)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].last)

        assertEvalWithBudget(
          List.single(BigInt(1)).last,
          BigInt(1),
          ExUnits(memory = 13119, steps = 3_245875)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).last,
          BigInt(2),
          ExUnits(17448, 4480301)
        )
    }

    test("headOption") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.headOption
            val scalaResult = list.asScala.headOption

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(List.empty[BigInt].headOption, None, ExUnits(3164, 635245))

        assertEvalWithBudget(
          List.single(BigInt(1)).headOption,
          Some(BigInt(1)),
          ExUnits(memory = 4492, steps = 1_070418)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).headOption,
          Some(BigInt(1)),
          ExUnits(4692, 1159481)
        )
    }

    test("head") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.head)
            val scalaResult = liftThrowableToOption(list.asScala.head)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].head)

        assertEvalWithBudget(
          List.single(BigInt(1)).head,
          BigInt(1),
          ExUnits(memory = 9354, steps = 2_363169)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).head,
          BigInt(1),
          ExUnits(1332, 264462)
        )
    }

    test("length") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.length
            val scalaResult = BigInt(list.asScala.length)

            scalusResult === scalaResult
        }

        assertEvalWithBudget(List.empty[BigInt].length, BigInt(0), ExUnits(5864, 1065838))

        assertEvalWithBudget(
          List.single(BigInt(1)).length,
          BigInt(1),
          ExUnits(memory = 13890, steps = 2_836557)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).length,
          BigInt(2),
          ExUnits(21152, 4464678)
        )
    }

    test("size") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.size
            val scalaResult = BigInt(list.asScala.size)

            scalusResult === scalaResult
        }

        assertEvalWithBudget(List.empty[BigInt].size, BigInt(0), ExUnits(5864, 1065838))

        assertEvalWithBudget(
          List.single(BigInt(1)).size,
          BigInt(1),
          ExUnits(memory = 13890, steps = 2_836557)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).size,
          BigInt(2),
          ExUnits(21152, 4464678)
        )
    }

    test("tail") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.tail)
            val scalaResult = liftThrowableToOption(list.asScala.tail.asScalus)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].tail)
        assertEvalWithBudget(
          List.single(BigInt(1)).tail,
          List.empty[BigInt],
          ExUnits(memory = 3928, steps = 894418)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).tail,
          Cons(BigInt(2), Nil),
          ExUnits(1032, 216462)
        )
    }

    test("drop") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.drop(number)
            val scalaResult = list.asScala.drop(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(
          List.empty[BigInt].drop(BigInt(1)),
          List.empty[BigInt],
          ExUnits(5334, 1084980)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).drop(BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 9534, steps = 2_131398)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).drop(BigInt(1)),
          Cons(BigInt(2), Nil),
          ExUnits(9734, 2220461)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).drop(BigInt(2)),
          List.empty[BigInt],
          ExUnits(13702, 3280212)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).drop(BigInt(0)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(5766, 1160710)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).drop(BigInt(-1)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(5766, 1160710)
        )
    }

    test("dropRight") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.dropRight(number)
            val scalaResult = list.asScala.dropRight(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(
          List.empty[BigInt].dropRight(BigInt(1)),
          List.empty[BigInt],
          ExUnits(11322, 2488669)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).dropRight(BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 21802, steps = 5_434404)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropRight(BigInt(1)),
          Cons(BigInt(1), Nil),
          ExUnits(31848, 8329735)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropRight(BigInt(2)),
          List.empty[BigInt],
          ExUnits(31818, 8285541)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropRight(BigInt(0)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(6766, 1320710)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropRight(BigInt(-1)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(6766, 1320710)
        )
    }

    test("dropWhile") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.dropWhile(_ < value)
            val scalaResult = list.asScala.dropWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].dropWhile(_ < BigInt(1)),
          List.empty[BigInt],
          ExUnits(4832, 885094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).dropWhile(_ < BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 7962, steps = 1_677988)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropWhile(_ < BigInt(3)),
          List.empty[BigInt],
          ExUnits(16456, 3717598)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropWhile(_ < BigInt(2)),
          Cons(BigInt(2), Nil),
          ExUnits(13392, 3022941)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropWhile(_ < BigInt(1)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(8162, 1767051)
        )
    }

    test("deleteFirst") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.deleteFirst(value)
            val scalaList = list.asScala
            val scalaResult = scalaList.indexOf(value) match {
                case -1    => scalaList
                case index => scalaList.patch(index, scala.Nil, 1)
            }

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].deleteFirst(BigInt(1)),
          List.empty[BigInt],
          ExUnits(5932, 1061094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).deleteFirst(BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 9526, steps = 2_027438)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).deleteFirst(BigInt(1)),
          Cons(BigInt(2), Nil),
          ExUnits(9726, 2116501)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).deleteFirst(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(17848, 4276196)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).deleteFirst(BigInt(2)),
          List.single(BigInt(1)),
          ExUnits(15252, 3587690)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(1), Nil)).deleteFirst(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(9726, 2116501)
        )
    }

    test("take") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.take(number)
            val scalaResult = list.asScala.take(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(
          List.empty[BigInt].take(BigInt(1)),
          List.empty[BigInt],
          ExUnits(5934, 1180980)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).take(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 10498, steps = 2_430910)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).take(BigInt(1)),
          Cons(BigInt(1), Nil),
          ExUnits(10698, 2519973)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).take(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(16062, 4076230)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).take(BigInt(0)),
          List.empty[BigInt],
          ExUnits(6066, 1208710)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).take(BigInt(-1)),
          List.empty[BigInt],
          ExUnits(6066, 1208710)
        )
    }

    test("takeRight") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.takeRight(number)
            val scalaResult = list.asScala.takeRight(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(
          List.empty[BigInt].takeRight(BigInt(1)),
          List.empty[BigInt],
          ExUnits(10790, 2393370)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).takeRight(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 22166, steps = 5_647097)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeRight(BigInt(1)),
          Cons(BigInt(2), Nil),
          ExUnits(29756, 7664141)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeRight(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(33078, 8806226)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeRight(BigInt(0)),
          List.empty[BigInt],
          ExUnits(6766, 1320710)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeRight(BigInt(-1)),
          List.empty[BigInt],
          ExUnits(6766, 1320710)
        )
    }

    test("takeWhile") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.takeWhile(_ < value)
            val scalaResult = list.asScala.takeWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].takeWhile(_ < BigInt(1)),
          List.empty[BigInt],
          ExUnits(5132, 933094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).takeWhile(_ < BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 7962, steps = 1_677988)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeWhile(_ < BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(17784, 4220622)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeWhile(_ < BigInt(2)),
          List.single(BigInt(1)),
          ExUnits(14056, 3274453)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeWhile(_ < BigInt(1)),
          List.empty[BigInt],
          ExUnits(8162, 1767051)
        )
    }

    test("distinct") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.distinct
            val scalaResult = list.asScala.distinct

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].distinct,
          List.empty[BigInt],
          ExUnits(13292, 2465658)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).distinct,
          List.single(BigInt(1)),
          ExUnits(memory = 33372, steps = 7_135335)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).distinct,
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(59050, 13122091)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(1), Nil)).distinct,
          List.single(BigInt(1)),
          ExUnits(49426, 11056630)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(1), Nil))).distinct,
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(81398, 18502362)
        )
    }

    test("diff") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.diff(list2)
            val scalaResult = list1.asScala.diff(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].diff(List.empty[BigInt]),
          List.empty[BigInt],
          ExUnits(6933, 1297143)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).diff(List.empty[BigInt]),
          List.single(BigInt(1)),
          ExUnits(memory = 8329, steps = 1_725798)
        )

        assertEvalWithBudget(
          List.empty[BigInt].diff(List.single(BigInt(1))),
          List.empty[BigInt],
          ExUnits(memory = 7897, steps = 1_528804)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).diff(List.single(BigInt(1))),
          List.empty[BigInt],
          ExUnits(memory = 20052, steps = 4_478992)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).diff(List.single(BigInt(2))),
          Cons(BigInt(1), Nil),
          ExUnits(memory = 26510, steps = 6_284238)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).diff(List.single(BigInt(3))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 29106, steps = 6_972744)
        )
    }

    test("init") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.init)
            val scalaResult = liftThrowableToOption(list.asScala.init.asScalus)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].init)

        assertEvalWithBudget(
          List.single(BigInt(1)).init,
          List.empty[BigInt],
          ExUnits(memory = 24435, steps = 6_059447)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).init,
          Cons(BigInt(1), Nil),
          ExUnits(34481, 8954778)
        )
    }

    test("reverse") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.reverse
            val scalaResult = list.asScala.reverse

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(List.empty[BigInt].reverse, List.empty[BigInt], ExUnits(6696, 1232879))

        assertEvalWithBudget(
          List.single(BigInt(1)).reverse,
          List.single(BigInt(1)),
          ExUnits(memory = 14152, steps = 2_902494)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).reverse,
          Cons(BigInt(2), Cons(BigInt(1), Nil)),
          ExUnits(21144, 4477511)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))).reverse,
          Cons(BigInt(3), Cons(BigInt(2), Cons(BigInt(1), Nil))),
          ExUnits(28368, 6099827)
        )
    }

    test("foreach") {
        check { (list: List[BigInt], value: BigInt) =>
            var scalaSum = BigInt(0)
            var scalusSum = BigInt(0)

            list.foreach(x => scalusSum += x + value)
            list.asScala.foreach(x => scalaSum += x + value)

            scalusSum === scalaSum
        }

        assertEvalWithBudget(List.empty[BigInt].foreach(_ + BigInt(1)), (), ExUnits(4532, 837094))

        assertEvalWithBudget(
          List.single(BigInt(1)).foreach(_ + BigInt(1)),
          (),
          ExUnits(memory = 10626, steps = 2_288514)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).foreach(_ + BigInt(1)),
          (),
          ExUnits(15956, 3597336)
        )
    }

    test("asScala/asScalus") {
        check { (scalusList: List[BigInt], scalaList: scala.List[BigInt]) =>
            scalusList === scalusList.asScala.asScalus &&
            scalaList == scalaList.asScalus.asScala
        }
    }

    private val bigIntGen: Gen[BigInt] = Gen.choose(BigInt(-1), BigInt(10))

    private val bigIntRangeGen: Gen[(BigInt, BigInt)] =
        for {
            start <- Gen.choose(BigInt(-5), BigInt(5))
            end <- bigIntGen
        } yield (start, end)

    private val bigIntListAndIndexGen: Gen[(List[BigInt], BigInt)] =
        for {
            list <- Arbitrary.arbitrary[List[BigInt]]
            index <- bigIntGen
        } yield (list, index)
}
