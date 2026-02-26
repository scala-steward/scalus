package scalus.cardano.offchain.mpf

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.mpf.MerklePatriciaForestry as OnChain
import scalus.cardano.onchain.plutus.mpf.MerklePatriciaForestry.{Proof, ProofStep}
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.{PlutusV3, Program}
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.{PlutusVM, Result}

class MerklePatriciaForestryBenchmarkTest extends AnyFunSuite {

    private given compilerOptions: Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = false,
      optimizeUplc = true,
      debug = false
    )
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // Compile on-chain functions once, outside any loop.
    private val hasProgram = PlutusV3.compile { (root: Data, key: Data, value: Data, proof: Data) =>
        OnChain(root.to[ByteString]).has(
          key.to[ByteString],
          value.to[ByteString],
          proof.to[Proof]
        )
    }.program

    private val insertProgram = PlutusV3.compile {
        (root: Data, key: Data, value: Data, proof: Data) =>
            OnChain(root.to[ByteString]).insert(
              key.to[ByteString],
              value.to[ByteString],
              proof.to[Proof]
            )
    }.program

    private val deleteProgram = PlutusV3.compile {
        (root: Data, key: Data, value: Data, proof: Data) =>
            OnChain(root.to[ByteString]).delete(
              key.to[ByteString],
              value.to[ByteString],
              proof.to[Proof]
            )
    }.program

    /** Applies a 4-argument compiled program to data and returns the budget spent. */
    private def measure(
        program: Program,
        root: ByteString,
        key: ByteString,
        value: ByteString,
        proof: Proof
    ): (Long, Long) = {
        val applied =
            program.term $
                root.toData.asTerm $
                key.toData.asTerm $
                value.toData.asTerm $
                proof.toData.asTerm
        applied.evaluateDebug match
            case Result.Success(_, exunits, _, _) => (exunits.memory, exunits.steps)
            case Result.Failure(_, exunits, _, _) => (exunits.memory, exunits.steps)
    }

    /** Build a trie of n elements. Keys are 4-byte big-endian ints, value is a fixed byte. */
    private def buildTrie(n: Int): (MerklePatriciaForestry, ByteString, ByteString) = {
        val value = ByteString.fromArray(Array[Byte](1))
        val benchKey = ByteString.fromArray(Array[Byte](0, 0, 0, 0))
        var trie = MerklePatriciaForestry.empty
        var i = 0
        while i < n do
            val k = ByteString.fromArray(
              Array((i >> 24).toByte, (i >> 16).toByte, (i >> 8).toByte, i.toByte)
            )
            trie = trie.insert(k, value)
            i += 1
        (trie, benchKey, value)
    }

    private def proofSize(proof: Proof): Int = proof.toData.toCbor.length

    ignore("benchmark MPF operations at various trie sizes") {
        val sizes = (2 to 9).map(math.pow(10, _).toInt)

        // Header
        println()
        println(
          f"${"Size"}%10s  ${"Proof(B)"}%10s  ${"has-mem"}%12s  ${"has-cpu"}%14s  ${"ins/del-mem"}%12s  ${"ins/del-cpu"}%14s"
        )
        println("-" * 90)

        for n <- sizes do
            print(f"$n%10d  building..."); Console.flush()
            val (trie, key, value) = buildTrie(n)
            val onChain = trie.toOnChain

            print(" proving..."); Console.flush()
            val proof = trie.proveExists(key)
            val psize = proofSize(proof)

            print(" measuring..."); Console.flush()
            val (hasMem, hasCpu) = measure(hasProgram, onChain.root, key, value, proof)

            val withoutKey = trie.delete(key)
            val insertProof = withoutKey.proveMissing(key)
            val (insMem, insCpu) =
                measure(insertProgram, withoutKey.toOnChain.root, key, value, insertProof)

            println(
              f"\r$n%10d  $psize%10d  $hasMem%12d  $hasCpu%14d  $insMem%12d  $insCpu%14d"
            )
            Console.flush()
    }
    println()
}
