package scalus.cardano.wallet

import scalus.uplc.builtin.ByteString
import scalus.cardano.txbuilder.TransactionSigner
import scalus.crypto.ed25519.{ExtendedSigningKey, Signature, SigningKey, VerificationKey}

/** A key pair for Ed25519 signing operations. */
trait KeyPair {
    type Underlying
    def underlying: Underlying

    /** The verification (public) key - 32 bytes. */
    def verificationKey: VerificationKey

    /** Sign a message and return the signature.
      * @param message
      *   the message to sign
      * @return
      *   64-byte Ed25519 signature
      */
    def sign(message: ByteString): Signature

    /** Verify a signature.
      * @param message
      *   the message that was signed
      * @param signature
      *   the signature to verify
      * @return
      *   true if the signature is valid
      */
    def verify(message: ByteString, signature: Signature): Boolean
}

/** A key pair backed by a standard 32-byte signing key. */
trait StandardKeyPair extends KeyPair {

    /** The signing (private) key - 32 bytes. */
    def signingKey: SigningKey
}

/** A key pair backed by an extended 64-byte signing key (SLIP-001/HD wallets). */
trait ExtendedKeyPair extends KeyPair {

    /** The extended signing key - 64 bytes. */
    def extendedSigningKey: ExtendedSigningKey
}

/** Account abstraction for Cardano wallets. */
trait Account {
    def paymentKeyPair: KeyPair
    def changeKeyPair: KeyPair
    def stakeKeyPair: KeyPair
    def drepKeyPair: KeyPair

    def signerForUtxos: TransactionSigner =
        new TransactionSigner(Set(paymentKeyPair))
}
