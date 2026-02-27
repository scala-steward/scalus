package scalus.uplc.eval

import scalus.cardano.ledger.ExUnits

object ExBudget {

    /** The zero budget */
    @deprecated("use ExUnits.zero instead", "0.15.1")
    val zero: ExUnits = ExUnits.zero

    @deprecated("use ExUnits.enormous instead", "0.15.1")
    val enormous: ExUnits = ExUnits.enormous

    /** Constructs an 'ExBudget' from CPU and memory components.
      *
      * @param cpu
      *   CPU steps
      * @param memory
      *   Memory units
      * @return
      *   ExUnits instance with swapped parameter order (memory, steps)
      */
    @deprecated("use ExUnits(memory, steps) instead", "0.15.1")
    def fromCpuAndMemory(cpu: Long, memory: Long): ExUnits = ExUnits(memory, cpu)
}
