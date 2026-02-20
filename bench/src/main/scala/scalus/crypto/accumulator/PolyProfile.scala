package scalus.crypto.accumulator

import scalus.crypto.accumulator.BilinearAccumulatorProver

/** Standalone profiling harness for Poly.product.
  *
  * Run with async-profiler:
  * {{{
  * sbtn "bench/runMain scalus.crypto.accumulator.PolyProfile"
  * }}}
  *
  * Attach async-profiler externally, or use -agentpath.
  */
object PolyProfile {
    def main(args: Array[String]): Unit = {
        val size = 32000
        val rng = new scala.util.Random(42)
        val elements = Vector.fill(size)(BigInt(256, rng).mod(BilinearAccumulatorProver.p))

        // Warmup
        println("Warming up...")
        Poly.product(elements.take(1000))
        Poly.product(elements.take(1000))

        println(s"Profiling Poly.product($size)...")
        var i = 0
        while i < 5 do
            val t0 = System.nanoTime()
            val poly = Poly.product(elements)
            val ms = (System.nanoTime() - t0) / 1_000_000
            println(s"  iteration $i: ${ms} ms, degree=${poly.degree}")
            i += 1
        println("Done.")
    }
}
