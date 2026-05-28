# SK-V16 S-P0 V1 CH5 - Hidden Coupling

Disposition: REVISE-FOLDED -> ACCEPT.

Initial CH5 rejected because sidecar/comparator non-admission tokens and scan
roots were not explicit enough. The folds added:

- `wrong-plane`, `cross-plane`, `sidecar-same-run`,
  `same-plane-source-sidecar`, `sidecar_freshness`, `historical:*`, and
  `stale:*` as diagnostic-only/non-admission proof;
- `skinny/crates/bbnf-bench/src/gate.rs`, `bin/gate.rs`, `report.rs`,
  `css_l4_w8.rs`, and `nonjson_css_l4.rs` to representative scans where
  relevant;
- explicit x86/AVX documentation scan proof commands.

After the second fold, CH5 returned ACCEPT.
