# Formation receipt migration

The formation receipt originally had SHA-256:

`92a91384b2bb1a9bf9b4669565c7f159dd38da57fe00e1a1a3db98d5bfa0cf17`

It receipted mutable `STATE.md`, both live ledgers, the live session protocol,
and the live W1 contract directly. Legitimate W0 admission and contract
narrowing would therefore make the formation epoch unverifiable.

W0 copied the exact formation-state bytes, hash
`05f57acaddd0db4f74e059ebdeed43ad49a0308953f39a7f3d5505247cc148d6`,
to `epochs/STATE-FORMATION.md`. It likewise copied the exact formation bytes of:

- `EVIDENCE-LEDGER.tsv` to `epochs/EVIDENCE-LEDGER-FORMATION.tsv`;
- `OBLIGATION-LEDGER.tsv` to `epochs/OBLIGATION-LEDGER-FORMATION.tsv`;
- `SESSION-PROTOCOL.md` to `epochs/SESSION-PROTOCOL-FORMATION.md`;
- `waves/W1-PARSE-THAT-TRANSACTION.md` to `epochs/W1-FORMATION.md`.

`FORMATION-RECEIPT.sha256` changes only those paths. Every formation content
hash is unchanged and still verifies. The live mutable files receive current
hashes from `W0-AUTHORITY.tsv` and the wave receipt.
