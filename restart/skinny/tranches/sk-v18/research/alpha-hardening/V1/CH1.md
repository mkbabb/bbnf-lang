# CH1 — CORRECTNESS (V1) — Pass Alpha SK-V18 alpha-hardening

Lens: CH1 Correctness per PASS-ALPHA §3 + ORCHESTRATOR §3W. Reviewer: V1.
Date: 2026-05-31. Subject: `restart/skinny/tranches/sk-v18/research/alpha/{alphaA..E}.md`
+ `SYNTHESIS.md` + `HANDOFF.md` (alphaF's output lives in the latter two; the §2 scope
matrix names α-A..F but the contract §6 routes αF directly to `SYNTHESIS.md`+`HANDOFF.md`,
so the missing `alphaF-*.md` is NOT a defect — it is the prescribed structure).

Focus (per dispatch): every claim cites a V3 finding / RESULTS row / SHA at path:line;
the >SOTA framing is honest (lazy-vs-eager disclosed); gates are measurable.

## Method

Independent re-verification at HEAD (working tree, SHA-pinned where the artefacts pin):
- SHAs `f6a38445b` (SK-V17 close), `7dbe44c22` (V3 audit), `6bb4b2a6c` (W3 NEON),
  `1c5bd7a25` (substrate) all resolve to the claimed commit subjects. ✓
- x86 tree: 24 files total (23 `.rs` + 1 `.asm`), 742 LOC, 14 `unimplemented!`. ✓
  (alphaA "23 `.rs` files" and alphaC/D/SYNTHESIS "24 files" are BOTH correct — `.rs`
  vs all-files — no contradiction.)
- `parse_w11_1_number` × 7 in shipped `json/generated.rs`. ✓
- `CSS_GENERATED_RS` at `runtime_generator.rs:701`. ✓
- `enum RuntimeEmitterKind` at `grammar_provider.rs:40`. ✓
- 7 CSS replicas md5-identical at `f6a38445b` (`b654562ccff46ed62dd48e9ace325830`,
  cross-checked at_rules_and_media ≡ visual_functions). ✓
- phantom `<G: EventGrammar = AnyGrammar>` at `tape/mod.rs:175`. ✓
- `json_sink_direct::render(program: &SinkOnlyProgram)` at `:4`. ✓
- `GENERIC_SCAN_ROOTS` at `lock14_baseline.rs:2409`. ✓
- 16-lock count. ✓
- sonic strict skipper (`IgnoredAny::deserialize` + `deserializer.end()`,
  `sonic_skipper.rs:5-6`); `sonic-rs` `default-features=false features=["sort_keys"]`
  (no `utf8_lossy`), `Cargo.toml:23`. ✓
- W5 close ledger sources the 2.210/2.355/3.348/1.996 ratios (N=200 median). ✓

The citation discipline is overwhelmingly strong: nearly every claim across all seven
artefacts carries a path:line, a V3 finding id (D1–D4, C1–C3, A1–A6, F-n), a RESULTS
row, or a SHA, and the independently checkable ones all resolve. Two correctness
defects survive; one strict-comparator-plane observation is a REVISE.

---

## Disposition by section

### αA — Results Extraction — ACCEPT

Every row of §1 (JSON 51/51) carries Track1/sonic/Δ tuples sourced to
`skinny/RESULTS.md` `parse_only` rows; §2 (CSS) correctly states the headline numbers
do NOT live in RESULTS.md and routes them to the W5 close ledger + `css_canon_bench`
(verified: ledger §3 carries 2.210/2.355/3.348/1.996). The §3 overfit surfaces all
carry path:line that re-verify. §6 caveats are exemplary CH1 hygiene: it pins the
7-replica claim to `f6a38445b` (where md5 IS identical) and discloses the working-tree
regen noise, and it flags that a raw working-tree `diff` now DIFFERS — pre-empting a
false-refutation. The lazy-vs-eager caveat (§2.1) is explicit. ACCEPT.

Note (non-blocking, no fix required): αA states the JSON Δ range as "+1.4% …
+164.7%" — this is the CORRECT range (the SYNTHESIS/HANDOFF understate it; see below).
αA is the source of truth that the downstream contract should have matched.

### αB — Competitor Deltas — ACCEPT

The strict-vs-strict (JSON) vs lazy-vs-eager (CSS) fairness-plane table (§0) is the
single most honest framing in the cohort: it states the asymmetry up front, names the
JSON `parse_only ↔ sonic Skipper` pairing as near-symmetric (both recognition-plane,
both strict), and the CSS pairing as the disclosed-MEDIUM asymmetric comparison. §1.1
correctly marks yyjson/asmjson/RapidJSON as schema-columns-only / honest `None` on
aarch64 (verified: `Cargo.toml` wires only simd-json + sonic-rs). §3.3 explicitly
forbids fabricating an un-run engine's number. §1.4 correctly demotes the typed rows as
schema-conditional, NOT the unconditional bar. The per-corpus deltas reconcile with
αA. ACCEPT.

One internal-consistency item the consolidator should be AWARE of (NOT a CH1 reject on
αB, because αB is honestly sourcing a different run): αB §2.2 uses the **live N=80**
CSS ratios (animate 2.145× / bootstrap 2.905× / tailwind **1.911×** / material 1.975×)
and names tailwind as the THINNEST canary (1.911×), whereas αA/SYNTHESIS use the
**N=200** ratios where tailwind is the WIDEST (3.348×). Both runs are real and cited
(N=80 live reproduction vs N=200 close median); αA discloses both. This is not a
fabrication, but the contract picks the N=200 set as the "bar" while αB's falsifiability
gate (§4) names tailwind via the N=80 number — the two sample-size planes must not be
silently mixed in the downstream SPEC. See REVISE on αE/SYNTHESIS below.

### αC — REDRESS Digest — ACCEPT

Every PRUNE wave (P1–P5) carries live-at-HEAD evidence (file census, md5, grep counts)
re-verified here. The pre-block families (§2.1–§2.6) each carry the measured refutation
SHA/factor (AZ-IV 118× `cb14970f`; StructRegistry 983×/10583×; etc.) AND an SK-V18-
specific re-open test keyed to the NEW surfaces (generator, shared trait, phantom `<G>`)
— this is exactly the CH3-adjacent rigor PASS-ALPHA §3 wants, and the citations resolve.
The §0 state-delta (emit_fact_stream gone, `W5C_REQUEST_FACT_PROFILES` now a comment) is
verified (grep=0). The Lock load-bearing list (§3) cites `LOCKS.md` line ranges. The
"gate keyed to crates/core is a CH1 defect" self-warning (§0) is precisely the right
benched-tree discipline. ACCEPT.

### αD — Validated/Invalidated Ledger — ACCEPT

V1–V8 (validated) and I1–I10 (invalidated) each cite SHA/RESULTS/path:line; the §6
verification log re-greps every checkable claim at HEAD and reports the command + result
(x86 742 LOC, replica IDENTICAL, phantom `<G>`, `CSS_GENERATED_RS:701`, metalang ×7) —
all reproduce here. The DM (demoted) rows correctly distinguish conditional typed-struct
wins from the unconditional `parse_only` proof. S1–S13 map 1:1 to the backlog with owner
surface + parity oracle each. The §5 pre-block assertion ("NONE of S1–S13 re-opens") is
defensible. ACCEPT.

### αE — Candidate Shortlist — REVISE

The shortlist is well-structured (5 clusters A,B1–B4; PRUNE-before-GENERALIZE
sequencing; the falsifiability triple of PRESERVED->SOTA / GRAMMAR-DERIVATION-PROOF /
DISTINCT-GRAMMAR-OUTPUT is a genuinely operational, mutate-the-`.bbnf` test). Each
candidate carries owner paths, scalar-ref/checkasm status, same-wave consumer, LOC
budget, risk, pre-blocks. Most gates are measurable (grep counts, md5-distinct,
−3% thresholds, `accepts_current_allowlist` red-on-reintroduction).

**REVISE — the one unmeasurable / mixed-plane gate.** Candidate B2's PRESERVED->SOTA
gate reads "CSS canonical > lightningcss on bootstrap/tailwind/material/animate ... must
stay within the SK-V17 envelope (1.9–3.3× cold)" (`alphaE-candidate-shortlist.md:91`),
while the falsifiability-threshold convention elsewhere in αE is "no regression beyond
−3% on the named row vs the SK-V17 hand-written baseline" (`:16`). The CSS baseline is
ambiguous between TWO measured planes — the N=200 close ledger (tailwind 3.348×) and the
live N=80 reproduction (tailwind 1.911×). A "−3% vs baseline" gate is not machine-
checkable until αE/the SPEC names WHICH per-corpus Mbps number is the baseline (N=200
median is the contract's stated bar; the N=80 numbers must not be the gate's reference).

Concrete fix: in `alphaE-candidate-shortlist.md:91` (B2 gate #1) and `:140` (B4 gate #4)
pin the CSS preservation baseline to the **N=200 `css_canon_bench` close-ledger
medians** (bootstrap 2473.1 / animate 2937.9 / tailwind 2773.4 / material 2618.5 Mbps
Track1; lightningcss 1119.1/1247.7/828.5/1312.0 — W5 ledger §3), and state the −3%
threshold against THOSE Mbps, not against the "1.9–3.3×" range (a range is not a per-row
gate). Drop or footnote the N=80 ratios as a cross-check only.

### SYNTHESIS.md — REVISE

The contract is strong: §0.1 close-condition table is per-gate verifiable-by-grep, the
telemetry binding (Section 2) adds machine-checkable generalization columns
(`verbatim_blob_present==false`, `phantom_generic_resolved∈{instantiated,deleted}`,
`generated_md5_distinct`, `acceleration_at_admission∈{admission,...}` NOT
`cfg-test-only`, etc.) with an executable `gate-json --skv18-generalization-report`
consumer, and §0.4 pre-blocks are exhaustive. The lazy-vs-eager honesty (H1) is carried
in §0.1 (G2/H1 gate), §0.6, and Section 2 (`materialization_framing` column). Most of it
is ACCEPT-grade.

**REVISE — a concrete measurable-claim error (inaccurate-perf-narrative).** The
SYNTHESIS states the JSON >SOTA delta range as "**Track 1 > sonic +1.4%–78%**" in THREE
places: `SYNTHESIS.md:107` (Ground truth bullet), `:174` (§0.2 starting-state table),
`:322` (§1 validated ledger). This is wrong on the upper bound. The actual measured
maximum per αA §1 / αB §1.2 / `skinny/RESULTS.md` is **+164.7%** (unicode_escapes/
parse_only); "78%" is marine_ik, which is NOT the widest row. The seed and the dispatch
SUBJECT both state "+1.4% … +164.7%". Understating the campaign's own >SOTA margin
against the RESULTS rows αA extracted is a CH1 correctness defect (the gate is fed by
RESULTS, and the contract prose contradicts RESULTS).

Concrete fix: replace "+1.4%–78%" with "**+1.4%–164.7%**" at `SYNTHESIS.md:107`, `:174`,
`:322` (and HANDOFF — see below). Optionally annotate "+78% = marine_ik;
+164.7% = unicode_escapes (widest); thinnest +1.4% = apache_builds".

**REVISE (same disposition, second item) — strict-comparator-gate completeness vs §4.2.**
§0.6 lists the JSON strict gate as "Track 1 vs sonic-rs strict ..., simdjson DOM + On
Demand, yyjson default strict, serde_json strict baseline." Per αB §1.1/§3.3 (verified:
`Cargo.toml` wires only simd-json + sonic-rs; no yyjson/asmjson FFI), **yyjson is NOT
runnable on aarch64 and emits honest `None`.** The SYNTHESIS §0.6 reads as if yyjson is a
live per-row comparator, which would invite a fabricated column. PASS-ALPHA §4.2 marks
yyjson "yes if comparator runnable" — and it is NOT runnable here. Fix: in §0.6 mark
yyjson/asmjson/RapidJSON as "schema columns, honest `None` on aarch64 (FFI not wired)",
mirroring αB §3.3, so the gate cannot be read to require an un-run engine's number.
(Section 2 already does this correctly via the `Option` framing — §0.6 prose must match.)

The CSS-ratio mixing flagged on αE applies here too: §0.2/§1 use the N=200 ledger
ratios (correct as the bar); ensure the downstream SPEC's CSS −3% gate references the
N=200 Mbps, not the N=80 reproduction (carry the αE fix forward).

### HANDOFF.md — REVISE

The HANDOFF accurately mirrors the SYNTHESIS (current state, the 16 backlog items each
tagged with its V3 finding id, pre-blocks, the six CHALLENGE addenda, the R10 close
criterion, the next-move). The CHALLENGE-addenda section and the gate-consumer schema
are correct and measurable.

**REVISE — same +1.4%–78% error.** `HANDOFF.md:26-27` ("Track 1 +1.4%–78%") repeats the
understated JSON range. Fix: "+1.4%–**164.7%**" (consistent with αA/αB/RESULTS), same as
the SYNTHESIS fix.

No other HANDOFF defect: the CSS ratios it carries (bootstrap 2.210× / animate 2.355× /
tailwind 3.348× / material 1.996×, `HANDOFF.md:29`) are the N=200 ledger set, correct.

---

## Summary of required fixes (orphan-free; each REVISE has a path:line + concrete fix)

1. **SYNTHESIS.md:107, :174, :322** + **HANDOFF.md:26-27** — JSON delta range
   "+1.4%–78%" → "**+1.4%–164.7%**" (matches αA §1 / αB §1.2 / RESULTS / the dispatch
   SUBJECT). [CORRECTNESS — measurable claim contradicts the RESULTS rows it cites.]
2. **SYNTHESIS.md §0.6** — mark yyjson/asmjson/RapidJSON as honest `None` / not-runnable
   on aarch64 (mirror αB §3.3 + Section 2's `Option` framing), so the strict-comparator
   gate cannot be read to require an un-run engine's number. [CORRECTNESS — §4.2
   "if runnable" not honored in prose.]
3. **alphaE-candidate-shortlist.md:91, :140** + carry into SYNTHESIS §0.2/SPEC — pin the
   CSS PRESERVED->SOTA −3% gate to the **N=200 `css_canon_bench` close-ledger Mbps
   medians** (not the "1.9–3.3×" range, not the N=80 reproduction), so the per-row gate
   is machine-checkable. [MEASURABILITY — gate references an ambiguous baseline / a range
   instead of a per-row number.]

All three REVISEs are concrete, path:line-anchored, and self-contained (no orphan
REVISE). The lazy-vs-eager (H1) honesty is correctly disclosed throughout (αA §2.1, αB
§0/§3.2, αD I9, αC §2.3, SYNTHESIS §0.6 + Section 2 `materialization_framing`) — that
axis is ACCEPT. The pre-block and SHA/path:line discipline is ACCEPT across the board.

## Tally rationale

- ACCEPT: αA, αB, αC, αD (4 sections).
- REVISE: αE, SYNTHESIS, HANDOFF (3 sections).
- REJECT: none (no claim is unsupported or fabricated; the defects are an understated
  range, a prose/runnability mismatch, and an under-specified per-row baseline — all
  repairable in place).

TALLY accept=4 revise=3 reject=0
