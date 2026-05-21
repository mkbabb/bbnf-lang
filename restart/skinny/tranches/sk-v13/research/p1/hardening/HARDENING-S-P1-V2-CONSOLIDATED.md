# SK-V13 S-P1 V2 Hardening Consolidated

Pass: S-P1 Profile. Cycle: V2.
Date: 2026-05-21.
Scope: consolidated CH1-CH6 challenge verdict for the SK-V13 S-P1 V2 profile cohort.
Output: this file.

## Verdict

`G-S-P1-V2-CONVERGED`: FAIL.

V2 is a real improvement over V1, not a paper rerun. It fixes the direct
panic-path profiles, adds a complete mode-III capture matrix, records CSS
profile artefacts, and routes branch/L1/LLC export as unavailable rather than
zero. It still does not meet the S-P1 convergence bar because five of six
challenge lenses require a V3 fold.

| Lens | Disposition | Load-bearing reason |
|---|---|---|
| CH1 correctness | REVISE | Direct and mode-III coverage are fixed, but some hot-leaf claims remain line-poor or provisional; P1-E needs a canonical evidence ledger with per-row resolved/unresolved status. |
| CH2 generality / Lock 14 | REVISE | Most hot leaves remain JSON generated envelopes or JSON typed paths; CSS is telemetry but does not yet name a CSS parser primitive. |
| CH3 regression / REDRESS | REVISE | V2 fences REDRESS 96/97/98, but direct-row, pre-pin, and SIMD/orphan signals need inline REDRESS 119/120, pre-pin-route, and REDRESS-126 guards. |
| CH4 cost / reproducibility | REVISE | V2 direct is reproducible, but retained V1 parse/typed provenance, temp mode-III harness, CSS command surface, and offline sidecar extractor are still too ephemeral. |
| CH5 hidden coupling | ACCEPT | V2 keeps Track 1/Track 2, direct/typed/CSS, structural-scan, and symbol-sidecar evidence separated and preserves REDRESS 96/97/98 union-substrate history. |
| CH6 anti-paper-close | REVISE | The artefacts exist, but save-only sidecar symbolication, mode-III file:line gaps, CSS timer/fact-sink dominance, and typed 7/17 coverage prevent acceptance. |

Acceptance rate: 1/6 = 16.7%. Consecutive accepted cycles: 0.

## V2 Improvements Banked

- Direct `direct_to_struct` samply is no longer panic-path evidence: 34 direct
  profiles, 34 sidecars, 34 logs, and 0 bad return codes under
  `/tmp/skv13-p1-v2/samply/`.
- Mode III now has 85 profile/log/counter rows for 17 corpora x five probes,
  plus explicit unsupported routing for `alternate_pext_mask_plan` and the
  disabled duplicate dispatch-table probe.
- CSS declaration-values has equality/throughput/profile evidence under
  `/tmp/skv13-p1-v2/css/`, but the current profile is timer/fact-sink
  dominated and cannot identify a parser primitive.
- xctrace export was tested; branch-miss, L1-miss, and LLC-miss fields are
  `unavailable_from_current_export`, not zero or inferred.
- CH5 accepted the plane/substrate separation: V2 does not normalize structural
  SIMD scan results into a union-substrate route.

## Required V3 Fold

1. Publish a canonical evidence ledger for parse, direct, typed, mode-III, and
   CSS rows with: `row`, `plane`, `profile_path`, `sidecar_path`, `log_path`,
   `symbol`, `self_time`, `file_line`, `primitive_class`,
   `primitive_status`, `non_json_confirmed`, and `resolved_status`.
2. Add a primitive-attribution status vocabulary and apply it consistently:
   `json-parse-envelope`, `json-direct-envelope`,
   `resolved-json-unicode-candidate`, `json-scan-primitive-candidate`,
   `json-typed-only`, `css-profiled-nonparser-overhead`, `timer/noise`,
   `missing-product-surface`, `function-only-sidecar`, and
   `unavailable_from_current_export`.
3. Carry REDRESS guardrails inline beside the relevant signals:
   REDRESS 119/120 + USER-PIN material-differential language for direct-row
   progress; pre-pin route guards for dispatch/masking/unescape signals; and
   REDRESS-126 zero-orphan language beside PEXT/SIMD/ASM sidecar gaps.
4. Make reproducibility third-party durable: record exact V1 retained-capture
   provenance, preserve or inline the mode-III harness source/hash, preserve or
   inline the sidecar extractor source/hash, and add the CSS build/run/samply
   command surface.
5. Keep save-only sidecar symbolication provisional unless a row has profile
   path, sidecar path, log path, symbol, percent self-time, and source
   file:line. Mode-III function-only rows remain unresolved for precise
   primitive attribution unless V3 re-symbolicates them.
6. Keep the ten missing typed rows as `missing_product_surface`; do not count
   them as profiled. Keep CSS declaration-values as
   `throughput_equality_profiled_but_hot_leaf_unresolved`.
7. Preserve branch/L1/LLC as `unavailable_from_current_export` unless V3
   produces row-level numeric exports.

## Cycle Disposition

S-P1 V2 returns to profile fold. The V3 fold can be documentation/provenance
heavy if it supplies the canonical ledger and durable command/source records;
it does not need to discard the V2 captures unless V3 chooses to re-symbolicate
mode-III or CSS. No S-P2 dispatch is authorized from V2.
