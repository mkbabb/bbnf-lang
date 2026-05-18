# SK-V9 S-P1 Hardening V1 CH6: Anti-Paper-Close / Next-Pass Impact

Disposition: REVISE

Confidence: 96%

Scope: `restart/prompts/skinny/PASS-1-PROFILE.md`,
`restart/audit/pass-1-substrate/PASS-1.md`, all six SK-V9 P1 artifacts,
`restart/skinny/tranches/sk-v9/research/g-alpha/G-ALPHA-PRESENTATION.md`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md`, and
`restart/skinny/tranches/sk-v9/HANDOFF.md`. Lens: CH6 anti-paper-close and
next-pass impact.

## Verdict

The P1 packet is honest about missing evidence, but it is not a converged
S-P1 profile. It cannot advance to S-P2 as a normal research floor while fresh
SK-V9-open samply profiles, PMU counters, c/B rows, and resolved hot-leaf
symbols are absent.

This is REVISE rather than REJECT because the artifacts do not forge profile
claims. They mark the gaps explicitly. Advancing anyway would be the paper-close:
P2 would either design from `absent:*` cells or promote historical Criterion /
fused samply proxies into hot-leaf antecedents.

## Defects

1. CRITICAL - S-P1 entry/convergence evidence is missing.
   The pass contract requires a checked SK-V{N}-open W0 baseline before S-P1
   (`restart/prompts/skinny/PASS-1-PROFILE.md:24-31`), then advances only on
   convergence or an explicit final pin (`restart/prompts/skinny/PASS-1-PROFILE.md:177-180`).
   The artifacts instead record no fresh SK-V9-open profile set: P1-A has 0/17
   fresh samply coverage (`restart/skinny/tranches/sk-v9/research/p1/p1a-samply-mode-1.md:9-11`),
   P1-B ran no samply capture (`restart/skinny/tranches/sk-v9/research/p1/p1b-samply-mode-2.md:40-46`),
   P1-C leaves mode-III measurements absent (`restart/skinny/tranches/sk-v9/research/p1/p1c-samply-mode-3.md:34-40`),
   and P1-D says authoritative PMU/c/B coverage is 0/17
   (`restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md:7-11`).

2. CRITICAL - S-P2 has no valid hot-leaf antecedent to consume.
   S-P2 must consume S-P1 as its empirical floor, and any primitive without a
   P1 hot-leaf antecedent is speculative
   (`restart/prompts/skinny/PASS-1-PROFILE.md:218-223`,
   `restart/prompts/skinny/PASS-1-PROFILE.md:276-279`). P1-E explicitly says
   current hot-leaf cells are Criterion bindings, not samply symbols or self-time
   percentages (`restart/skinny/tranches/sk-v9/research/p1/p1e-hot-leaf-attribution.md:72-81`),
   and every main workload remains `GAP:not-classified`
   (`restart/skinny/tranches/sk-v9/research/p1/p1e-hot-leaf-attribution.md:154-244`).
   P2 cannot legally derive primitive candidates from those gaps.

3. CRITICAL - c/B and PMU cannot be inferred from W0 slope metadata.
   CH1/CH6 require real PMU-derived c/B and real artifacts, not estimates
   (`restart/prompts/skinny/PASS-1-PROFILE.md:123-160`). P1-D correctly refuses
   to derive c/B without same-run cycles and input bytes
   (`restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md:71-73`) and
   requires W0 telemetry-lock to add cycles, instructions, branch misses, L1/LLC
   misses, and `cycles_per_byte = cycles / input_bytes`
   (`restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md:233-284`).
   Treating ns/B, Mbps, CPU model, or Criterion sample cost as c/B would be a
   direct paper-close.

4. MAJOR - P1-F proves absence of SK-V9 row movement, not a fresh delta.
   P1-F shows current `skinny/RESULTS.md` and `skinny/REDRESS.md` are unchanged
   from SK-V8 close and all current rows are `0 / 0 / same`
   (`restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md:19-27`,
   `restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md:85-97`).
   It also flags `RUN8`, `HL`, deferred strictness, stale CostFacts, and sidecar
   gaps (`restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md:99-111`).
   That is useful excavation, but not SK-V9-open performance evidence.

5. MAJOR - W0 is mandatory before any behavior wave.
   G-Alpha names SK-V9-open telemetry/gate refresh as gate-only and non-row-moving
   (`restart/skinny/tranches/sk-v9/research/g-alpha/G-ALPHA-PRESENTATION.md:46-51`).
   SYNTHESIS gives it a same-wave `gate-json` consumer and no behavior movement
   or measured row additions by itself
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:111-120`). HANDOFF repeats that it
   cannot move throughput, admit Apache/CITM measured rows, or alter behavior
   (`restart/skinny/tranches/sk-v9/HANDOFF.md:47-51`). No behavior wave may start
   before that W0-style lock exists, is consumed by the gate, and is folded back
   into S-P1.

6. MAJOR - Pre-blocked routes would reopen if P2/P3 treats gaps as evidence.
   Apache/CITM remain source/product parity only, Canada typed remains routed,
   structural parse waits on retained class/event grammar plus `ValueRef`, and
   direct digest rows stay guard-plane until a contract/control-path tranche
   exists (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:212-219`,
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-329`;
   `restart/skinny/tranches/sk-v9/HANDOFF.md:79-105`). P2/P3 must not convert
   P1 absence into a changed-shape proof.

## Focus Answers

Can P1 advance to P2 despite absent fresh profiles? No, not as normal S-P2
Research. A user could explicitly pin a non-converged cycle, but CH6 should
allow that only as a routed exception that blocks primitive design until the
missing W0/profile evidence is produced. It must not be represented as S-P1
convergence.

Is W0 mandatory before behavior waves? Yes. The SK-V9-open telemetry/gate
refresh is a gate-only prerequisite, and the first downstream plan must place it
before row-moving or behavior-changing waves. It may refresh run identity and
required telemetry; it may not move parser/scanner/SIMD/codegen behavior,
throughput cells, or measured row admission by itself.

What must fold into P2/P3? P2 must receive a negative antecedent ledger, not a
profile floor: no candidate primitive without fresh P1 symbol path, self-time
percentage, source file:line, artifact path, run id, and same-run PMU/c/B where
claimed. P3 must make W0 the first gate-bearing wave and must hard-block behavior
waves until W0 plus a revised S-P1 profile cycle converge.

## Required Folds

1. Fold into S-P1 V2 before any convergence claim:
   produce a SK-V9-open W0 manifest consumed by `gate-json`; keep it behavior
   frozen; run P1-A/P1-B/P1-C with interactive symbol-resolving `samply record`
   and `debug=true` for all 17 corpora; collect P1-D PMU counters for every
   admitted workload and masking probe; rerun P1-E against the actual profiles;
   rerun P1-F against the new run id and explicit SK-V8-close delta.

2. Fold into S-P2 entry checks:
   reject any candidate whose antecedent is `absent:*`, Criterion slope-only,
   source-eligible-only, sidecar-historical-only, or stale fused SK-V4 profile
   evidence. Require a table mapping each S-P2 primitive to a resolved P1 hot
   leaf and corpus/workload row. No c/B from ns/B, throughput, CPU frequency, or
   sample cost.

3. Fold into S-P3 wave planning:
   the first dispatchable wave must be W0 telemetry/gate refresh with no behavior
   movement and with `gate-json` as same-wave consumer. Later behavior waves must
   declare exact owner paths, selected rows, strict same-run comparator plane,
   PMU/profile fields, REDRESS fallback, revert protocol, full-table maintain,
   Lock 14 proof where generic surfaces are touched, and challenge acceptance
   before implementation.

4. Fold the pre-block ledger forward unchanged:
   no Apache/CITM measured-row overclaim, no Canada typed shortcut, no structural
   parse implementation before retained grammar plus cursor proof, no scalar-parent
   or renamed parent-digest fold, no sidecar substrate / parser-owned cursor /
   public substrate API / new directive or BIR variant, and no generic JSON policy
   leak (`restart/audit/pass-1-substrate/PASS-1.md:54-57`;
   `restart/skinny/tranches/sk-v9/HANDOFF.md:79-105`).

## Blockers To P2

- No SK-V9-open W0 telemetry-lock manifest.
- No fresh P1-A/P1-B/P1-C samply artifacts on disk.
- No top-20 self-time symbol tables with source file:line for the 17-corpus
  coverage set.
- No same-run PMU counters or derived c/B rows.
- No resolved P1-E hot-leaf classifications for the main workload rows.
- No P1-F delta against a fresh SK-V9-open run id.

Until these close, S-P1 can only report an evidence gap. It cannot become the
empirical floor for S-P2 without paper-closing the pass.
