# SK-V13 S-P1 V2 CH1 Correctness

Disposition: REVISE

Scope reviewed: V2 `p1a` through `p1f`, the V1 consolidation, and the
CH1 contracts in `restart/prompts/skinny/PASS-1-PROFILE.md` plus
`restart/prompts/ORCHESTRATOR.md` §3W/§3Z.

## Verdict

V2 folds the V1 hard blockers for direct panic captures and missing mode-III
coverage, but it is not yet CH1-ACCEPT. CH1 requires every hot-leaf claim to
carry symbol path, percent self-time, and source file:line
(`restart/prompts/skinny/PASS-1-PROFILE.md:123`-`127`), and the universal CH1
gate requires every material claim to resolve (`restart/prompts/ORCHESTRATOR.md:81`-`88`).
V2 still has line-poor mode-III leaves, save-only/offline symbolication, partial
typed/CSS attribution, and incomplete branch/L1/LLC counter export.

## Evidence

- V1 rejected CH1 because P1-C had 0/17 mode-III coverage, direct samply was
  panic-path evidence, hot-leaf citations were incomplete, and RESULTS
  placeholders were not resolved (`restart/skinny/tranches/sk-v13/research/p1/hardening/V1/CH1.md:37`,
  `restart/skinny/tranches/sk-v13/research/p1/hardening/V1/CH1.md:56`,
  `restart/skinny/tranches/sk-v13/research/p1/hardening/V1/CH1.md:75`,
  `restart/skinny/tranches/sk-v13/research/p1/hardening/V1/CH1.md:132`).
- The V1 consolidation assigned CH1 REJECT for the same coverage and
  attribution failures (`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:16`-`23`).
- Direct coverage is materially fixed: P1-B reports 34 direct profile files,
  34 sidecars, and zero bad return codes (`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:42`-`52`),
  with per-corpus direct rank-1 symbols and percentages (`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:66`-`84`).
- Mode-III coverage is materially fixed: P1-C reports 85/85 captured profiles
  with zero bad return codes and explicit unsupported routing for two non-valid
  probes (`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:41`-`54`,
  `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:98`-`106`).
- c/B values are derived from real cycle/instruction rows, not estimates: P1-D
  lists parse, direct, typed, and mode-III counter row sources
  (`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:57`-`64`) and
  V2 direct/mode-III c/B tables (`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:71`-`99`).
- P1-F correctly fences profile classifications as
  `profile_signal_not_gate_admission` and records the current row inventory,
  including 10 missing typed product rows and 23 unmeasured CSS expansion rows
  (`restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:30`-`32`,
  `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:97`-`106`).

## Defects

1. Hot-leaf evidence is still not per-claim complete. P1-A retains save-only,
   offline-symbolicated parse profiles and explicitly says the pass is not a
   clean interactive samply pass (`restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:75`,
   `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:131`-`132`).
   P1-A also leaves `distinct_values` without file:line
   (`restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:98`).
2. Mode-III symbol coordinates are not CH1-clean. P1-C's table gives per-corpus
   percentages, but only symbol names for structural leaves
   (`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:64`-`82`),
   then admits that some NEON symbols have no file:line
   (`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:90`-`91`,
   `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:121`-`122`).
   The raw V2 top-20 TSV (`/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv`) confirms
   mode-III rank-1 rows are function-resolved but line-poor in the emitted
   `file`/`line` columns.
3. P1-E does not resolve every synthesis cell to symbol + percent + file:line.
   Its main table omits percentages for parse and typed cells, uses shorthand
   `scan.rs` without line numbers for many mode-III cells, and marks 10 typed
   rows as missing (`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:47`-`65`).
   The CSS row is profiled, but rank-1 is timer overhead rather than a parser
   primitive (`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:67`-`72`).
4. PMU scope is honest but incomplete. P1-D proves cycles/instructions/c/B, yet
   branch-miss, L1-miss, and LLC-miss fields remain unavailable from current
   xctrace export (`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:47`-`50`,
   `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:119`-`120`).
5. `skinny/RESULTS.md` still carries `criterion-slope-profile` hot-leaf cells
   on the checked result surface, including the first parse/direct/typed rows
   (`skinny/RESULTS.md:5`-`7`). V2 provides a side ledger, but CH1 cannot call
   the RESULTS hot-leaf surface mechanically resolved until every row maps to a
   resolved profile symbol or an explicit row-level unresolved disposition.

## Fold Actions

1. Publish a canonical per-row evidence ledger for parse, direct, typed, mode-III,
   and CSS rows: profile path, sidecar path, symbol path, percent self-time,
   source file:line, and resolved/unresolved status.
2. For mode III, either re-symbolicate the V2 profiles so every rank-1 claim has
   file:line, or mark the affected structural/ASM rows unresolved and prevent
   S-P2 from treating them as precise primitive antecedents.
3. Reconcile P1-E against the canonical ledger so no table cell omits percent
   self-time or file:line when it is presented as resolved.
4. Keep branch/L1/LLC as unavailable unless a row-level xctrace export can be
   produced; do not broaden the PMU claim beyond cycles/instructions/c/B/CPI.
5. Add a row-level RESULTS hot-leaf resolution map for all `criterion-slope-profile`
   cells, including the 10 absent typed rows and CSS timer-dominated profile as
   explicit non-resolved/product-gap dispositions.
