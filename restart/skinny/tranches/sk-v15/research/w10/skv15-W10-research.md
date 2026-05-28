# SK-V15 Wave W10 Research: FNV Quarantine

Inputs: W9 admission `a21573cdf`, SPEC Section 13, DISPATCH-PROMPT W10,
`DEP-W10-FNV-QUARANTINE`, PASS-IMPL V1, and the SK-V15 audit-overfit
packet.

Authority findings:

- SPEC Section 13 requires W11L/W11N/W11O FNV closed-enum products to stay
  bench/xtask-only, adversarial semantic fixtures, and a production
  `rg -n "fnv|FNV"` scan.
- `CONSOLIDATED-AUDIT.md` records the W11L weakness: Track 1 hashes a decoded
  string into a closed token table while serde/sonic sidecars deserialize into
  the same finite token domain. That makes hash equality insufficient as a
  strict-product proof.
- The audit-overfit A2 packet routes this as a medium bench-only caveat, not as
  production runtime authority.

Production scan:

```text
rg -n "fnv|FNV" crates/core/src/runtime crates/core/src/backend crates/core/src/generate skinny/crates/runtime/src skinny/crates/codegen/src
```

Findings are not absent:

- `crates/core/src/generate/regex/emit/dfa/accel.rs` hashes DFA structure for
  compile-time DFA-table interning/canonical hashing. It is codegen-internal,
  not a runtime parser selector, admission arbiter, or correctness proof.
- `skinny/crates/codegen/src/runtime_generator.rs` emits `input_fnv64` in the
  old CSS fact-stream/full-parse diagnostic path. That path is already outside
  live CSS admission after W6.
- Seven dirty generated CSS runtime files under `skinny/crates/runtime/src` also
  contain `input_fnv64` and local `fnv64`. They are generated diagnostic CSS
  metadata, not W10-owned source edits; they remain routed to the W6 old-proof
  retirement path.

Bench/xtask scope:

- W11L/W11N/W11O affected rows are:
  `json/y_string_unicode/{direct_to_struct,real_typed_struct}/main`,
  `json/unicode_mixed/{direct_to_struct,real_typed_struct}/main`, and
  `json/gsoc-2018/{direct_to_struct,real_typed_struct}/main`.
- Bench-side FNV and closed-enum evidence may remain only as quarantine
  metadata. It cannot select a runtime, adjudicate production correctness, or
  replace typed semantic equality.

Research conclusion: W10 can admit only with an executable bench quarantine
module, adversarial tests where equal hash metadata fails on typed-semantic
mismatch, a shared-closed-enum sidecar rejection, and an xtask report gate that
records the non-absent production scan with explicit non-production
classification.
