# SK-V17 S-P3 CHALLENGE — CH7 OVERFIT-PRUNE (V3)

Lens: CH7 OVERFIT-PRUNE. Cycle: V3. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/research/p3/{p3a..p3f}.md` + `restart/skinny/tranches/sk-v17/SPEC.md`.
Mandate (PASS-3 §3W / ORCHESTRATOR §3W): no contrivance in the plan — lightningcss the
fair bar; success = real >SOTA on a regular corpus, not a broadcast; tailwind handled
honestly; no fixture/FNV re-entry; grammar-derived projections.
Master HEAD verified: `f87ee713a` (`git rev-parse HEAD` =
`f87ee713a7cf82e6d2cc82738dde313940c49121`).

## §0 — Verdict summary

The V3 S-P3 packet is CH7-clean. All six contrivance vectors are re-verified against live
source at HEAD `f87ee713a` and against the freshly re-authored V3 artefacts (p3a-p3f
rewritten 17:01-17:07, AFTER the V2 CH7 at 16:59, plus the V3 SPEC). The single V2
disposition that touched the SPEC (the W2 exit-gate maintain budget REVISEd from a bare 0%
floor to the bench-falsifiable -2.0% median band, `SPEC.md:5`) is a CH1/falsifiability
fix orthogonal to the CH7 axis — it neither relaxes a >SOTA claim nor opens a contrivance
seam, and the >SOTA gate at W3 is untouched. CH7 carried ZERO open dispositions out of V2.
There is nothing to prune.

Counts: ACCEPT 7, REVISE 0, REJECT 0.

## §1 — V2 carry verification

CH7 returned all-ACCEPT at V2 (7/0/0, no REVISE). No CH7 disposition was outstanding to
fold. The only V2 disposition that mutated this packet was CH1/CH-falsifiability's W2
maintain-budget band (`SPEC.md:5` frontmatter, §5 W2 exit gate). Re-read at V3: the band
is `-2.0% median vs the W1 typed-tape baseline`, a bench-falsifiable budget — not a
contrivance vector. The W3 >SOTA gate predicate (`SPEC.md:637-648`) is unchanged in
substance from V2. CH7-clean carry. ACCEPT.

## §2 — Six contrivance-vector re-audit on the V3 packet (all ACCEPT)

### 2.1 — Fair bar (lightningcss full-CSSOM, not a token-scan) — ACCEPT

Re-verified in live source: `css_canon_bench.rs:113-115` defines `lightningcss_full_cssom`
as `StyleSheet::parse(input, ParserOptions::default())` returning `sheet.rules.0.len()` —
a materialized CSSOM rule count, registered as the `"lightningcss"` comparator
(`:126`), not a token scan. `SPEC.md:118-131` pins `css_comparator_plane=full-cssom` as
THE strict anchor and demotes cssparser token-scan / fact-stream / historical / broadcast
to flaw-probe / planning-only. The denominator is the W0-re-baselined same-run median;
`SPEC.md:202-209` marks ALL per-corpus lightningcss endpoints UNMEASURED-PENDING and
forbids any wave exit gate keying on the prior 793/833/929/974 numbers. Bar is fair and
self-defended. ACCEPT.

### 2.2 — Real >SOTA on a regular corpus, not a broadcast — ACCEPT

The four corpora are sha256-pinned real published CSS from jsDelivr
(`css_l4_corpus.rs:22-54`): bootstrap@5.3.3 (`sha256:3c8f27e6…`), tailwindcss@0.2.0
(`sha256:e463dd78…`), material-components-web@14.0.0 (`sha256:60f82e18…`), animate.css@4.1.1
— genuine deployed stylesheets, not synthetic fixtures. The success criterion
(`SPEC.md:221-223`, W3 exit `SPEC.md:639-641`) is a per-corpus MEDIAN crossing
(`delta_vs_lightningcss > 1.0×`) at N≥50 cold on the full-cssom plane; the harness asserts
N≥50 in code (`css_canon_bench.rs:250`). The W8R single-tuple broadcast is the explicit
tripwire: `gate-json` rejects `css_sample_count==1` or one tuple across multiple corpus
rows (`SPEC.md:195-196,366,380,801`). Success is a real measurement on a real corpus, not
a broadcast. ACCEPT.

### 2.3 — Tailwind handled honestly — ACCEPT

The plan does not contrive a tailwind win. Close condition 7 (`SPEC.md:89-93`) and the W3
exit gate (`SPEC.md:642-644`) require tailwind benched cold N≥50: ADMIT only if
`delta_vs_lightningcss > 1.0×`, ELSE the residual gap is REPORTED with hot-leaf
attribution in REDRESS — "NOT paper-closed, NOT hidden behind a corpus average. No
corpus-average claim substitutes for per-corpus medians." A corpus-average substitution
is an explicit W5 paper-close FAIL (`SPEC.md:774,828`). The tranche does not block on
tailwind provided ≥1 regular corpus crosses (`SPEC.md:215,221-225`). Honest residual is a
first-class disposition. ACCEPT.

### 2.4 — No fixture re-entry — ACCEPT

Re-verified: the canonical CSS harness does NOT route through the per-corpus
`real_typed`/`fixture_for_name`/`direct_struct` fixture path — `grep -c` over
`css_canon_bench.rs` = **0**. The SPEC global barred set (`SPEC.md:803-805`) bars
per-corpus hand-coded `real_typed.rs` fixtures and hand-tuned per-corpus capacity
constants; L7 one-shot reserve is bound to "a conservative byte-proportional bound — never
a per-corpus literal" (`SPEC.md:448`). No fixture re-entry seam. ACCEPT.

### 2.5 — No FNV re-entry — ACCEPT

FNV / `push_ascii_lower_hex` retires WHOLESALE with the fact-stream String, never
re-admitted as a primitive: `SPEC.md:803-805` ("FNV stays bench-only … no production FNV
selector/arbiter/correctness proof, FNV closed-enum production migration"), the W3
pre-block (`SPEC.md:656,826` "FNV/hex as a primitive"), and the barred set (`SPEC.md:853`,
no-CSS-antecedent rule excludes orphan udot/i8mm digit kernels and FNV/hex). FNV cannot
re-enter as a hot-path construct. ACCEPT.

### 2.6 — Grammar-derived projections (no relocated W5C overfit) — ACCEPT

Re-verified: close condition 3 (`SPEC.md:54-72`) requires ONE `BackendRule`-walking
accessor generator emitting `document/value/view/visitor` per grammar — JSON the existing
witness (its hand-written `value_from_ref` rider re-emitted byte-equal THROUGH the new
generator, `json/value.rs:143`), CSS L4 the first-mover. The hand-coded
`W5C_REQUEST_FACT_PROFILES` array (confirmed live at `codegen/src/lib.rs:336`, iterated
`:567,611`) is RETIRED and DERIVED from the `.bbnf`/`BackendRule` shape — "preserved as
DATA in the tape-plan lowering, NOT lost, NOT re-hardcoded." The relocated-W5C overfit
(the array moved into projection-data / flag form) is named as the CH2 failure mode at
both layers: the L8 sparse-flag side-table is constrained to `BackendRule` branch-tag
projections, NOT a hand-curated per-rule catalogue (`SPEC.md:576-577,840-841` W2
pre-block; `SPEC.md:237` close-condition rider "never hand-curated; relocating per-rule …
is the relocated-W5C overfit"). The W2 close gate is greppable
(`w5c_profile_array_retired` schema fact `SPEC.md:177`; no per-rule-id match arms JSON
does not need `SPEC.md:67-68`). Projections are grammar-derived by construction. ACCEPT.

## §3 — Lens-scope notes (CH7-axis residual sweep)

- **W1 +40%-over-fact_stream threshold** remains a conservative lower-bound tripwire, not
  a cherry-picked target: the fact_stream plane is 214-365 i/B vs full_parse 46-58 i/B
  (4.4× gap, S-P1 §3.4), so removing even half the String tax exceeds +40%. Measurability
  per se is CH1's province; the threshold's non-contrivance is CH7-clean.
- **tailwindcss@0.2.0** is the pinned corpus version — an older tailwind release, not a
  hand-trimmed or synthetic variant; the `sha256:e463dd78…` pin (`css_l4_corpus.rs:36`) is
  the contrivance guard. The "hardest hold-out" framing is honest: the eager path
  WATCHDOG'd 10583× under AZ-IV (S-P1), and the plan admits-or-reports-residual rather
  than picking a friendlier corpus (`SPEC.md:215`). CH7-clean.
- **L9 re-profile gating to post-W1, not post-W3** (`SPEC.md` W4 conditional): the spine
  wave keys admission on a post-tape re-profile, not a fabricated antecedent — P1-E
  measured ZERO speculative-rollback self-time, so L9 cannot be sequenced ahead of
  evidence. The lever is gated on real re-measurement, consistent with the carry-forward
  L9 condition. CH7-clean.
- **W2 maintain-budget band (V2 fold, `-2.0%` vs W1 typed-tape baseline)**: a falsifiable
  regression budget, not a >SOTA claim relaxation. CH7-clean.

## §4 — Dispositions

| # | Disposition | Section | Path:line | Fix |
|---|---|---|---|---|
| 1 | ACCEPT | Fair bar (lightningcss full-CSSOM) | `css_canon_bench.rs:113-115,126`; `SPEC.md:118-131,202-209` | none |
| 2 | ACCEPT | Real >SOTA on regular corpus, not broadcast | `css_l4_corpus.rs:22-54`; `css_canon_bench.rs:250`; `SPEC.md:221-223,195-196,366,801` | none |
| 3 | ACCEPT | Honest tailwind | `SPEC.md:89-93,215,221-225,642-644,774,828` | none |
| 4 | ACCEPT | No fixture re-entry | `css_canon_bench.rs` grep=0 for `real_typed`/`fixture_for_name`/`direct_struct`; `SPEC.md:448,803-805` | none |
| 5 | ACCEPT | No FNV re-entry | `SPEC.md:656,803-805,826,853` | none |
| 6 | ACCEPT | Grammar-derived projections (no relocated W5C) | `SPEC.md:54-72,177,237,576-577,840-841`; `codegen/src/lib.rs:336,567,611`; `json/value.rs:143` | none |
| 7 | ACCEPT | V2 maintain-budget fold orthogonal to CH7; W3 >SOTA gate untouched | `SPEC.md:5,637-648` | none |

## §5 — Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §2 scope matrix, §3 CH lens registry.
- `restart/skinny/tranches/sk-v17/SPEC.md` §0.1-0.6, wave gates (W0-W5), §9 ledger, §10.
- `restart/skinny/tranches/sk-v17/research/p3/{p3a..p3f}.md` (V3 cohort, re-authored
  17:01-17:07).
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V2/CH7.md` (the all-ACCEPT V2
  carry).
- `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs:113-115,126,250` (full-CSSOM
  comparator, N≥50 assert; grep-clean of fixture path).
- `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:22-54` (sha256-pinned real corpora).
- `skinny/crates/codegen/src/lib.rs:336,567,611` (`W5C_REQUEST_FACT_PROFILES` confirmed
  live, pre-blocked for retirement in W1/W2).
- `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`
  (corpora, ratio band, instr/byte gap).
- Master HEAD `f87ee713a`.
