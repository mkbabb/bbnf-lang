# SK-V16 S-P0 A2 - Admit-Mechanism Integrity

Date: 2026-05-28.
HEAD: `fc16919d4`.
Axis: A2 admit-mechanism integrity.
Disposition: ACCEPT.

## Finding Summary

Current live admission mechanisms satisfy A2. No current row promotes CSS W8R,
old generated CSS proof, FNV hashes, or docs-only scaffold evidence into live
admission.

- JSON rows are bound to strict same-plane mechanisms with row-keyed
  measurement, independent comparator/oracle, and per-iteration equality.
- CSS remains `0/24` live admission and must use grammar-derived typed provider
  plus typed same-workload equality before speed claims.
- FNV is bench-only quarantine evidence and cannot act as a production selector,
  arbiter, or correctness proof.
- W7/W8/W9 dependency admits are executable dependency proofs, not row SOTA
  movement.

## High Findings

### A2-H1 - Wrong-plane and sidecar comparators remain non-admission

Wrong-plane/cross-plane comparators and sidecar comparators remain diagnostic
only unless a future SK-V16 typed report gate proves same-workload typed
equality and structured freshness. Existing gate/report code already treats
plane mismatch, stale comparators, historical sidecars, `sidecar-same-run`
without structured manifest, `same-plane-source-sidecar`, and
`sidecar_freshness` as constrained evidence rather than live SK-V16 admission
proof.

Representative scan:

```sh
rg -n 'wrong_plane|plane mismatch|historical:|stale|sidecar-same-run|same-plane-source-sidecar|sidecar_freshness' \
  skinny/crates/bbnf-bench/src/gate.rs \
  skinny/crates/bbnf-bench/src/bin/gate.rs \
  skinny/crates/bbnf-bench/src/report.rs \
  skinny/crates/bbnf-bench/src/css_l4_w8.rs \
  skinny/crates/bbnf-bench/src/nonjson_css_l4.rs
```

## Evidence

Commands:

```sh
(cd skinny && cargo xtask gate-json --check-results)
# G-SK-V15-W2-LOCK-GATES-ONLY PASS
```

Relevant gate surfaces:

- `skinny/xtask/src/skv15_w0.rs` validates JSON row universe, measurement row
  id, workload/value-plane match, grammar-backed generator source, parity, and
  broadcast status.
- `restart/skinny/ROLLING-SOTA-DELTA.md` keeps all CSS rows `OPEN`.
- `restart/skinny/tranches/sk-v16/research/alpha-hardening/V3/CONSOLIDATED.md`
  records that the `--skv16-*` report consumers are S-P3 obligations, not
  implemented proof.

## Prune Candidates

1. Require executable SK-V16 report validators before any SK-V16 row admission.
2. Keep CSS W8R, `CSS_GENERATED_RS`, fact-stream, brace-counter,
   `CssFullParseSummary`, and historical CSS `PASS-ADMIT` text under explicit
   non-admission quarantine.
3. Treat dirty generated CSS and `generated_real_typed` state as broad-check
   blockers only.
4. Preserve FNV quarantine unless a future typed-semantic contract and fresh
   gate prove otherwise.
