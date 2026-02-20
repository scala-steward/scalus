package scalus.crypto.accumulator

import org.openjdk.jmh.annotations.*
import scalus.crypto.accumulator.BilinearAccumulatorProver

import java.util.concurrent.TimeUnit
import scala.annotation.nowarn

/** JMH benchmark for Poly.product / productTree performance.
  *
  * Run with:
  * {{{
  * sbtn "bench/Jmh/run -i 5 -wi 3 -f 1 -t 1 .*PolyBenchmark.*"
  * }}}
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 5)
@Measurement(iterations = 5, time = 5)
@Fork(1)
class PolyBenchmark {

    @Param(Array("1000", "4000", "16000", "32000"))
    @nowarn("msg=unset private variable")
    private var size: Int = 0

    private var elements: Vector[BigInt] = null

    @Setup(Level.Trial)
    def setup(): Unit = {
        val rng = new scala.util.Random(42)
        elements = Vector.fill(size)(BigInt(256, rng).mod(BilinearAccumulatorProver.p))
    }

    @Benchmark
    def polyProduct(): Int = {
        val poly = Poly.product(elements)
        poly.degree
    }
}
