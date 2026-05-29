# SK-V17 S-P3 CHALLENGE — CH7 OVERFIT-PRUNE (V2)

Lens: CH7 OVERFIT-PRUNE. Cycle: V2. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/research/p3/{p3a..p3f}.md` + `restart/skinny/tranches/sk-v17/SPEC.md`.
Mandate (PASS-3 §3W / ORCHESTRATOR §3W): no contrivance in the plan — lightningcss the
fair bar; success = real >SOTA on a regular corpus, not a broadcast; tailwind handled
honestly; no fixture/FNV re-entry; grammar-derived projections.
Master HEAD verified: `f87ee713a` (`git rev-parse HEAD` =
`f87ee713a7cf82e6d2cc82738dde313940c49121`).

## §0 — Verdict summary

The S-P3 packet is CH7-clean. All six contrivance vectors are re-verified against live
source at HEAD `f87ee713a` and hold. The single V1 REVISE (R-CH7-1 — the p3b 5-wave vs
SPEC/p3c 6-wave wave-numbering divergence) is **fully folded**: `p3b` is now re-authored
to the SPEC's six-wave map, the load-bearing >SOTA gate is uniformly attributed to W3
across all four binding artefacts (p3a/p3b/p3c/SPEC), and no orphan REVISE remains. There
is nothing left to prune.

Counts: ACCEPT 7, REVISE 0, REJECT 0.

## §1 — V1 REVISE fold verification

### R-CH7-1 (V1) — Wave-numbering divergence — FOLDED → ACCEPT

V1 flagged that `p3b` carried a **5-wave** plan attributing "THE >SOTA gate" to W2 while
the SPEC and `p3c` carried a **6-wave** plan attributing it to W3, opening a seam where a
non-crossing corpus could be reported as closed against the wrong wave's exit.

Re-verified at V2: the divergence is closed.

- `p3b-wave-sequencing.md:132` now reads `Wave count = 6 (W0-W5) ≤ 12 (§3Z). Active
  shortlist = 8 (L1..L8)` — matching `SPEC.md:266` and `p3c`.
- `p3b:79-85` now sequences the SPEC six-wave order: **W0** infra, **W1** PRUNE +
  levers 1+2 substrate (tape activation, L2/L7-gated/L3-minimal), **W2** layout-driven
  lazy projection (split out), **W3** NEON structural index, W4 spine (conditional),
  W5 close.
- The load-bearing close is now uniformly W3: `p3b:463` "**W3 exit (THE >SOTA gate,
  measurable)**"; `p3b:567` explicitly records "reconciled six-wave numbering, >SOTA gate
  at W3"; matching `SPEC.md:299,625-635` (W3 exit gate predicate) and `p3c`. The W1
  paper-close guard is preserved (`p3b:451` "a >SOTA gate at W1 would be a paper-close").
- The gate predicate is unchanged in substance (it was a wave-index reconciliation, not a
  content change), as V1's concrete fix specified. ACCEPT.

## §2 — Six contrivance-vector re-audit (all ACCEPT)

### 2.1 — Fair bar (lightningcss full-CSSOM, not a token-scan) — ACCEPT

Re-verified in source: `css_canon_bench.rs:113-115` defines `lightningcss_full_cssom` as
`StyleSheet::parse(input, ParserOptions::default())` returning `sheet.rules.0.len()` — a
materialized CSSOM rule count, not a token scan. `SPEC.md:130` pins
`css_comparator_plane=full-cssom` as the strict anchor and demotes
cssparser/fact-stream/historical/broadcast planes to flaw probes. The denominator is the
W0-re-baselined same-run median; `SPEC.md:205-209` marks ALL per-corpus lightningcss
endpoints UNMEASURED-PENDING and forbids any wave exit-gate keying on the prior
793/833/929/974 numbers. The bar is fair and self-defended. ACCEPT.

### 2.2 — Real >SOTA on a regular corpus, not a broadcast — ACCEPT

The four corpora are sha256-pinned real published CSS from jsDelivr
(`css_l4_corpus.rs:22-54`): bootstrap 5.3.3 (232803 B), tailwindcss 0.2.0 (179631 B),
material-components-web 14.0.0 (495454 B), animate.css 4.1.1 (71750 B) — genuine deployed
stylesheets, not synthetic fixtures. The tranche success criterion (`SPEC.md:221-223`,
W3 exit gate `SPEC.md:625-628`) is a per-corpus MEDIAN crossing
(`delta_vs_lightningcss > 1.0×`) at N≥50 cold on the full-cssom plane. The harness asserts
N≥50 in code (`css_canon_bench.rs:250`, `assert!(n >= 50, ...)`) and reports the median
(`:161-171`). The W8R single-tuple broadcast is the explicit tripwire: `gate-json` rejects
`css_sample_count==1` or one-tuple-across-multiple-corpus-rows (`SPEC.md:195-196,366`).
Success is a real measurement on a real corpus. ACCEPT.

### 2.3 — Tailwind handled honestly — ACCEPT

The plan does not contrive a tailwind win. `SPEC.md:89-93` (close condition 7) and the W3
exit gate (`SPEC.md:637-639`) require tailwind benched cold N≥50: ADMIT only if
`delta_vs_lightningcss > 1.0×`, ELSE the residual gap is REPORTED with hot-leaf
attribution in REDRESS — "NOT paper-closed, NOT hidden behind a corpus average. No
corpus-average claim substitutes for per-corpus medians." `SPEC.md:769,823` make a
corpus-average substitution an explicit W5 paper-close FAIL. The tranche does not block on
tailwind provided ≥1 regular corpus crosses (`SPEC.md:215,222`). Honest residual is a
first-class disposition. ACCEPT.

### 2.4 — No fixture re-entry — ACCEPT

Re-verified: the canonical CSS harness does NOT route through the per-corpus
`real_typed_struct`/`fixture_for_name` fixture path — `grep -c
real_typed\|fixture_for_name\|direct_struct css_canon_bench.rs` = **0**. The SPEC global
block (`SPEC.md:798-800`) bars per-corpus hand-coded `real_typed.rs` fixtures and
hand-tuned per-corpus capacity constants; L7 one-shot reserve is bound to "a conservative
byte-proportional bound — never a per-corpus literal" (`SPEC.md:448`). No fixture re-entry
seam is open. ACCEPT.

### 2.5 — No FNV re-entry — ACCEPT

FNV / `push_ascii_lower_hex` retires WHOLESALE with the fact-stream String, never
re-admitted as a primitive: `SPEC.md:799-800` ("FNV stays bench-only … no production FNV
selector/arbiter/correctness proof, FNV closed-enum production migration"), W3 pre-block
`SPEC.md:651,821` ("FNV/hex as a primitive"), and the barred-set `SPEC.md:848` excludes
the orphan udot/i8mm digit kernels and FNV/hex by the no-CSS-antecedent rule. FNV cannot
re-enter as a hot-path construct. ACCEPT.

### 2.6 — Grammar-derived projections (no relocated W5C overfit) — ACCEPT

Re-verified: `SPEC.md:54-68` (close condition 3) requires ONE `BackendRule`-walking
accessor generator emitting `document/value/view/visitor` per grammar — JSON the existing
witness (its hand-written `value_from_ref` rider re-emitted byte-equal THROUGH the new
generator, `json/value.rs:143`), CSS L4 the first-mover. The hand-coded
`W5C_REQUEST_FACT_PROFILES` array (confirmed present at `codegen/src/lib.rs:336`) is
RETIRED and DERIVED from the `.bbnf`/`BackendRule` shape, "preserved as DATA in the
tape-plan lowering, NOT lost, NOT re-hardcoded" — and the relocated-W5C overfit (the array
moved into projection data/flag form) is named as the CH2 failure mode. The L8 sparse-flag
side-table is constrained to `BackendRule` branch-tag projections, NOT a hand-curated
per-rule catalogue (`SPEC.md:570-572` W2 pre-block "L8 flag as a hand-curated per-rule
catalogue (the relocated-W5C overfit)"). The W2 close gate is greppable
(`W5C_REQUEST_FACT_PROFILES` retired + no per-rule-id match arms JSON does not need,
`SPEC.md:67`, `w5c_profile_array_retired` schema fact `SPEC.md:177`). Projections are
grammar-derived by construction. ACCEPT.

## §3 — Lens-scope notes (CH7-axis residual sweep)

- **W1 +40%-over-fact_stream threshold** (audited in V1) remains a conservative
  lower-bound tripwire, not a cherry-picked target: the fact_stream plane is 214-365 i/B
  vs full_parse 46-58 i/B (4.4× gap, S-P1 §3.4), so removing even half the String tax
  exceeds +40%. CH7-clean. (Measurability per se is CH1's province.)
- **tailwindcss@0.2.0 (179631 B)** is the pinned corpus version — an older tailwind
  release, not a hand-trimmed or synthetic variant; the sha256 pin
  (`css_l4_corpus.rs:35`) is the contrivance guard. The "hardest hold-out" framing is
  honest: the eager path WATCHDOG'd 10583× under AZ-IV (S-P1), and the plan admits-or-
  reports-residual rather than picking a friendlier corpus. CH7-clean.
- **Re-profile gating of L9 to post-W1, not post-W3** (`p3b:295-302`, `SPEC.md:670`): the
  conditional spine wave (W4) keys its admission on a post-tape re-profile, not on a
  fabricated antecedent — P1-E measured ZERO speculative-rollback self-time, so L9 cannot
  be sequenced ahead of evidence. No contrivance: the lever is gated on a real
  re-measurement, consistent with the carry-forward L9 condition. CH7-clean.

## §4 — Dispositions

| # | Disposition | Section | Path:line | Fix |
|---|---|---|---|---|
| 1 | ACCEPT | V1 R-CH7-1 fold: wave-numbering reconciled to six-wave, >SOTA gate at W3 | `p3b:132,463,567`; `SPEC.md:266,299,625-635`; `p3c` | none (folded) |
| 2 | ACCEPT | Fair bar (lightningcss full-CSSOM) | `css_canon_bench.rs:113-115`; `SPEC.md:130,205-209` | none |
| 3 | ACCEPT | Real >SOTA on regular corpus, not broadcast | `css_l4_corpus.rs:22-54`; `css_canon_bench.rs:250,161-171`; `SPEC.md:221-223,195-196,366` | none |
| 4 | ACCEPT | Honest tailwind | `SPEC.md:89-93,215,222,637-639,769,823` | none |
| 5 | ACCEPT | No fixture re-entry | `css_canon_bench.rs` grep=0 for `real_typed`/`fixture`/`direct_struct`; `SPEC.md:448,798-800` | none |
| 6 | ACCEPT | No FNV re-entry | `SPEC.md:651,799-800,821,848` | none |
| 7 | ACCEPT | Grammar-derived projections (no relocated W5C) | `SPEC.md:54-68,177,570-572`; `codegen/src/lib.rs:336`; `json/value.rs:143` | none |

## §5 — Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §2 scope matrix, §3 CH lens registry.
- `restart/skinny/tranches/sk-v17/SPEC.md` §0.1-0.6, wave gates (W0-W5), §9 ledger, §10.
- `restart/skinny/tranches/sk-v17/research/p3/{p3a,p3b,p3c,p3d,p3e,p3f}.md`.
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V1/CH7.md` (the R-CH7-1 origin).
- `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs:113-115,161-171,250` (full-CSSOM
  comparator, median, N≥50 assert; grep-clean of fixture path).
- `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:22-54` (sha256-pinned real corpora).
- `skinny/crates/codegen/src/lib.rs:336` (`W5C_REQUEST_FACT_PROFILES` confirmed present,
  pre-blocked for deletion in W2).
- `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`
  (corpora, ratio band, instr/byte gap).
- Master HEAD `f87ee713a`.
