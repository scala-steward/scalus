package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network

class ProtocolParamsDiffTest extends AnyFunSuite {

    private val baseParams = CardanoInfo.mainnet.protocolParams

    test("diff of identical params returns empty Seq") {
        assert(ProtocolParams.diff(baseParams, baseParams).isEmpty)
    }

    test("diff with one field changed returns exactly that field") {
        val modified = baseParams.copy(txFeeFixed = baseParams.txFeeFixed + 1)
        val diffs = ProtocolParams.diff(baseParams, modified)
        assert(diffs.size == 1)
        assert(diffs.head.field == "txFeeFixed")
        assert(diffs.head.expected == baseParams.txFeeFixed.toString)
        assert(diffs.head.actual == modified.txFeeFixed.toString)
    }

    test("diff with multiple fields changed returns all of them") {
        val modified = baseParams.copy(
          txFeeFixed = baseParams.txFeeFixed + 1,
          maxTxSize = baseParams.maxTxSize + 100,
          minPoolCost = baseParams.minPoolCost + 50
        )
        val diffs = ProtocolParams.diff(baseParams, modified)
        val fieldNames = diffs.map(_.field).toSet
        assert(diffs.size == 3)
        assert(fieldNames == Set("txFeeFixed", "maxTxSize", "minPoolCost"))
    }

    test("CardanoInfo.verify returns Right for matching info") {
        val info = CardanoInfo.mainnet
        assert(CardanoInfo.verify(info, info) == Right(info))
    }

    test("CardanoInfo.verify returns Left with network diff") {
        val expected = CardanoInfo.mainnet
        val actual = expected.copy(network = Network.Testnet)
        val result = CardanoInfo.verify(expected, actual)
        assert(result.isLeft)
        val diffs = result.left.toOption.get
        assert(diffs.exists(d => d.field == "network"))
    }

    test("CardanoInfo.verify returns Left with slotConfig diff") {
        val expected = CardanoInfo.mainnet
        val actual = expected.copy(slotConfig = SlotConfig(0L, 0L, 500L))
        val result = CardanoInfo.verify(expected, actual)
        assert(result.isLeft)
        val diffs = result.left.toOption.get
        assert(diffs.exists(d => d.field == "slotConfig"))
    }

    test("CardanoInfo.verify returns Left with protocol param diffs") {
        val expected = CardanoInfo.mainnet
        val actual = expected.copy(
          protocolParams = expected.protocolParams.copy(txFeePerByte = 99)
        )
        val result = CardanoInfo.verify(expected, actual)
        assert(result.isLeft)
        val diffs = result.left.toOption.get
        assert(diffs.exists(d => d.field == "txFeePerByte"))
    }
}
