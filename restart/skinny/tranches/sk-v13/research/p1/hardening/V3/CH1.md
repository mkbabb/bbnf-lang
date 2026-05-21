# SK-V13 S-P1 V3 CH1 Correctness

Disposition: ACCEPT

Scope reviewed: `restart/prompts/skinny/PASS-1-PROFILE.md` section 3 CH1,
the S-P1 V2 CH1/consolidated hardening disposition, V3 `p1a` through `p1f`,
and `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`.

## Verdict

V3 resolves the CH1 correctness blocker. The PASS-1 CH1 contract asks whether
hot-leaf claims carry symbol path, percent self-time, and source file:line;
whether c/B is derived from real counters; whether corpus coverage is accounted
for; and whether `unprofiled` result cells are resolved to named symbols
(`restart/prompts/skinny/PASS-1-PROFILE.md:123`-`127`). V2 failed because those
facts were still line-poor or provisional and because the fold lacked a
canonical resolved/unresolved status ledger
(`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md:20`,
`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md:47`-`73`).

The V3 question is not whether every unsupported or absent surface became a
new measurement. It is whether the cohort now states what is resolved, what is
line/function-only, what is a missing product surface, what is timer/nonparser
overhead, and what is counter-export unavailable. It does.

## Evidence

- The V3 ledger defines the status vocabulary that V2 requested, including
  `function-only-sidecar`, `missing-product-surface`,
  `css-profiled-nonparser-overhead`, `timer/noise`, and
  `unavailable_from_current_export`, and states that all rows remain
  `profile_signal_not_gate_admission`
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:8`-`23`).
- Direct-to-struct no longer has the V1/V2 CH1 ambiguity. The ledger supplies
  profile, sidecar, and log path patterns plus per-corpus Track 1 symbol,
  self-time, file:line, primitive class, status, and non-JSON confirmation
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:25`-`50`);
  P1-B's per-corpus table carries the corresponding direct self-time and
  file:line claims for both tracks
  (`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:66`-`90`).
- Parse-only save-only evidence is no longer over-promoted. P1-A routes parse
  rows through the V3 ledger and says no `dispatch_value` row is grammar-neutral
  primitive evidence
  (`restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:21`-`24`);
  the ledger classifies 15/17 parse rows as JSON parse envelopes, `distinct_values`
  as function-only, and `y_string_unicode` as a resolved JSON unicode candidate
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:52`-`61`).
- Typed coverage is explicit rather than paper-closed. P1-F records 7/17 typed
  profile coverage and ten missing typed rows
  (`restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:48`-`55`,
  `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:102`-`111`);
  the ledger maps the seven existing typed rows to JSON-typed-only symbols and
  marks the ten absent rows `missing-product-surface`
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:63`-`76`).
- Mode III closes the V1 missing-coverage blocker without pretending the saved
  sidecars have full source precision. P1-C records 85/85 captured profiles,
  zero bad return codes, and explicit unsupported routes
  (`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:46`-`58`,
  `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:98`-`111`);
  the V3 ledger then marks all captured mode-III rank-1 rows as citable by
  function/profile path but `function-only-sidecar` for CH1/CH6 file:line
  closure, with source anchors only where separately stated
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:78`-`98`).
- CSS declaration-values is measured but not over-claimed. P1-E and P1-F both
  identify the CSS profile as timer/fact-sink dominated rather than a parser
  primitive
  (`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:79`-`83`,
  `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:79`-`88`);
  the ledger assigns `css-profiled-nonparser-overhead`
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:100`-`104`).
- c/B is still tied to real cycle/instruction rows, while unavailable counters
  are not converted to zero. P1-D records parse/direct/typed/mode-III coverage
  and states branch/L1/LLC are unavailable from the current xctrace export
  (`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:57`-`68`,
  `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:121`-`130`);
  the ledger preserves the same counter status
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:106`-`113`).
- P1-E now names the ledger as the row-level authority for resolved/unresolved
  status, and its V2-defect table distinguishes resolved, partially resolved,
  and preserved-unavailable cases
  (`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:13`-`16`,
  `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:90`-`97`).

## Fold Actions

No CH1-blocking V4 fold action is required.

Carry these correctness boundaries forward:

1. `function-only-sidecar` rows are profile signals, not precise source-line
   primitive attributions.
2. `missing-product-surface` typed rows are not profiled rows.
3. CSS declaration-values remains equality/throughput/profile evidence with an
   unresolved parser hot leaf.
4. Branch/L1/LLC remain `unavailable_from_current_export` unless a later fold
   produces row-level numeric exports.
5. All S-P1 V3 profile classifications remain
   `profile_signal_not_gate_admission`.
