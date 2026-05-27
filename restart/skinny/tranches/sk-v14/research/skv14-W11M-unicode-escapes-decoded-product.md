# SK-V14 W11M unicode_escapes Decoded Product Reject

## Verdict

`G-SK-V14-W11M-JSON-UNICODE-ESCAPES-DECODED-PRODUCT` closes as `REJECT`.
No source patch lands, no `skinny/RESULTS.md` row moves, and
`restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.

## Candidate

The measured candidate added a generic typed `DirectScalar::DecodedJsonString`
product and a generated `parse_unicode_escapes` root. Track 1 returned a
per-field product carrying raw escaped source for escaped strings, borrowed
decoded source for plain strings, and decoded semantic facts
`(fingerprint, len)`. Track 2, serde_json, and sonic-rs built the same semantic
product from decoded strings. This did not route through generic `parse_only`,
`JsonDigestSink`, `JsonDirectDigest`, or an aggregate document checksum.

The material differential versus W11B and W11K was removal of Track 1 decoded
string allocation for `unicode_escapes` while preserving a typed per-record
product surface. The candidate was materially distinct from REDRESS-54/55,
REDRESS-66/67/68/69, and REDRESS-117/118, but failed the cold SOTA gate.

The reverted source patch is retained at
`/tmp/skv14-W11M-unicode-escapes-rejected.patch` with SHA-256
`a774358440dd49ae6a46762a2ef5cbd848a5e1e8684f34f954dc2eb34b53d090`.

## Verification Before Measurement

- `cargo run --profile ax-iter -p xtask -- regen-real-typed`
- `cargo run --profile ax-iter -p xtask -- check-real-typed`
- `cargo test --profile ax-iter -p bbnf-bench unicode_escapes -- --nocapture`
- `cargo test --profile ax-iter -p codegen emits_typed_direct -- --nocapture`
- `cargo test --profile ax-iter -p bbnf-bench direct_strict_product -- --nocapture`

The focused `unicode_escapes` tests covered valid escaped strings, full fixture
Track 1/Track 2/serde/sonic parity, and malformed escape/control/surrogate
rejection.

## Cold Evidence

Evidence files:

- `restart/skinny/tranches/sk-v14/research/skv14-W11M-unicode-escapes-decoded-product.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11M-unicode-escapes-decoded-product.raw.log`

SHA-256:

- TSV: `547159381ff553ca2742db1cc18f177481f145ff1e9095907cabe64ceaaf7420`
- raw log: `d5139f262dd5c907e24a0cebac657c8a291bafa8b09207fee6e9bdce39bafced`

| row | Track 1 Mbps | sonic Mbps | margin vs sonic+1 |
|---|---:|---:|---:|
| `unicode_escapes/real_typed_struct` | 5824.372 | 7073.230 | -1249.858 |
| `unicode_escapes/direct_to_struct` | 5707.469 | 7620.832 | -1914.363 |

Track 2 remained finite (`3597.844` Mbps real typed, `3742.417` Mbps direct
strict), but neither Track 1 cleared same-run sonic plus 1.0 Mbps.

## Carry-Forward

W11M pre-blocks the decoded-string typed scalar product shape for
`unicode_escapes` unless a future candidate names a new material differential
beyond per-field raw-source plus decoded-fingerprint facts. It does not
pre-block closed string-token products such as W11L, and it does not alter the
generic parse_only contract.
