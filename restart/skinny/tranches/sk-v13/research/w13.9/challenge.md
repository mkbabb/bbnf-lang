# SK-V13 W13.9 Challenge - Canada Typed Product Surface

Date: 2026-05-22.
Disposition: ACCEPT.

## CH1 Correctness

Accept only a full-fixture typed product route. The checksum must include every
coordinate f64 bit pattern and every retained string field in source order.
Count-only coordinate folding, direct digest stand-ins, source-position facts,
or field omission are rejects.

## CH2 Generality / Lock 14

The edit is allowed because it extends the existing generated typed product
surface and real typed harness. It must not add a directive, BIR variant,
BackendShape, public substrate API, or JSON-specific generic-crate leak beyond
the W13.9 owner paths. The Lock 14 parent-diff test is mandatory.

## CH3 Regression / REDRESS

The material differential must cite REDRESS 80 and REDRESS 119/120. The wave
does not reopen parse-only substrate work and does not demote prior W13 typed
admits. A throughput miss is a measured REDRESS reject with the source patch
saved and reverted.

## CH4 Cost

The expected diff is bounded to real typed schema, generated output, typed
harness routing, gate/report/Lock14 plumbing, and W13.9 artifacts. Generated
size growth is acceptable only if `cargo xtask check-real-typed` passes and the
full fixture parity test stays green.

## CH5 Hidden Coupling

The route must use the same Track 1 generated consumer as the real typed bench.
Track 2, serde_json, and sonic-rs are comparators only. No benchmark-private
shortcut may compute a checksum directly from raw bytes.

## CH6 Anti-Paper-Close

The gate is the native Criterion row:
`json/canada/track1_real_typed_struct > json/canada/sonic_rs_real_typed_struct + 1 Mbps`.
W13.9 cannot close on parity alone, schema existence, generated code presence,
or a future SIMD promise.
