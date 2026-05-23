# SK-V14 S-P1 V3 CHALLENGE — Lens CH5 — HIDDEN COUPLING

Author: CH5 lens agent, S-P1 CHALLENGE V3 (pure confirming pass over commit
`4ad8f1949` HEAD — the V2 aggregator commit; no P1-artefact edits since
`069ba203c`).
Date: 2026-05-23.
Scope: lens CH5 — HIDDEN COUPLING — across the six P1 artefacts unchanged
since the V2 light micro-redispatch (`069ba203c413d46e7a5d465a128a983254e53841`,
`docs(sk-v14-p1-profile): V2 light micro-redispatch — five orphan REVISEs
landed`).
Binding: `PASS-1-PROFILE.md §3 CH5` + `ORCHESTRATOR.md §3W` CH5 ("No
parallel substrate, sidecar producer, renamed-scanner Lock 1 violation, or
Track 1 ≡ Track 2 dishonesty; substrate union holds.") + `ORCHESTRATOR.md
§3Z` (≥95 % × 2 cycles, zero orphan REVISEs — V2 was first cycle at
100 %; V3 is the second).
Discipline: write-only, `path:line` per claim, executable-verification
mandate, HARD CAP 30 min.
V2 carry-through: `research/p1/hardening/V2/CH5.md` (243 lines; **100 % ACCEPT
(6/6)** with CH5-A closed via F-V2-P1E-1 cite refresh, CH5-B reclassified
via F-V2-P1F-1 PASS-ALPHA §4.4 contracted-deferral, CH5-C + CH5-D
DEFERRED-TO-S-P2 by structural design).

## §1 — V3 disposition summary

The V3 task is **pure confirmation**: verify the V2 100 % ACCEPT baseline
holds over the unchanged P1 artefacts under the CH5 lens, and confirm no
fresh hidden-coupling signal has surfaced. The V2 aggregator
(`HARDENING-S-P1-V2-CONSOLIDATED.md:179` row CH5) forecast "Hold;
CH5-C + CH5-D carried to S-P2; substrate-union framing intact" — V3
confirms this forecast verbatim.

### Headline verdict

**ACCEPT (6 of 6 axes) — V3 second consecutive ≥95 % cycle, §3Z
two-cycle-chain CLOSES on CH5 at V3.**

V3 ACCEPT-rate: **6/6 = 100 %**. Zero REVISE, zero REJECT, zero fresh
finding. CH5 gate is OPEN for S-P2 dispatch (subject to cross-lens
aggregate convergence at the aggregator level).

### Why pure-confirming holds (commit graph + diff witness)

`git log --oneline -10` shows `4ad8f1949 docs(sk-v14-p1-hardening-V2):
challenge V2 + consolidated` at HEAD over `069ba203c docs(sk-v14-p1-profile):
V2 light micro-redispatch — five orphan REVISEs landed`. The V2 aggregator
commit (`4ad8f1949`) touched only the eight `hardening/V2/*.md` files; it
did not modify any of the six `research/p1/p1{a..f}*.md` artefacts.
Executable-verified: `git diff 069ba203c..HEAD -- restart/skinny/tranches/
sk-v14/research/p1/p1a-samply-mode-1.md restart/skinny/tranches/sk-v14/
research/p1/p1b-samply-mode-2.md restart/skinny/tranches/sk-v14/research/
p1/p1c-samply-mode-3.md restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-
cycles.md restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-
attribution.md restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md
restart/skinny/tranches/sk-v14/SYNTHESIS.md` → empty (zero bytes of
divergence). The six artefacts and the SYNTHESIS schema binding are
bit-identical to the V2 baseline against which `hardening/V2/CH5.md`
rendered 100 % ACCEPT.

Under V2's CH5 framing, the substrate-union story is load-bearing on three
canonical paragraphs (P1-E §4.4 substrate-union; P1-C ANOM-2 view-boundary
materialization; P1-A §4 Lock-1 same-substrate union for
`copy_nonoverlapping`). All three remain at their V2 byte positions
unchanged (executable-verified per §6 source ledger). The two Track 1 vs
Track 2 entry-point grep witnesses (P1-B `DirectParser::skip_value` at
`generated_real_typed.rs:2949`; Track 1 `parse_object_value_at_direct` at
`generated.rs:466`) likewise verify at HEAD.

## §2 — Per-artefact CH5 V3 disposition table

V3 audit per artefact: under the unchanged HEAD source tree and the
unchanged P1 artefacts, does the V2 ACCEPT verdict hold on each of the
five CH5 sub-axes (a) parallel substrate, (b) sidecar event vector,
(c) retained cursor across iterations, (d) second source scan, (e) Track 1
≡ Track 2 symbol-path collapse?

| Artefact | V2 verdict | V3 source-of-truth re-verification | V3 verdict | V3 delta |
|---|---|---|---|---|
| `p1a-samply-mode-1.md` | ACCEPT (5/5; movemask + methodology annotations did not perturb CH5) | `copy_nonoverlapping` 9.5-11.4% rows at `:142,143,150` unchanged; substrate-union paragraph at `:318-321` ("This is **tape-commit pressure**, the Lock-1 same-substrate union signal") byte-identical to V2 baseline | **ACCEPT** | unchanged — V2 100 % holds; single substrate (`runtime::generated_json::*`) sustained, `copy_nonoverlapping` correctly attributed to tape-commit pressure not parallel substrate |
| `p1b-samply-mode-2.md` | ACCEPT (5/5; two-cursor independence) | Track 2 `DirectParser::cursor` field at `generated_real_typed.rs:2745` distinct from Track 1 `cursor: &mut usize` parameter at `generated.rs:466,469` — executable-verified via `grep -n "struct DirectParser\|cursor: usize" skinny/crates/bbnf-bench/src/generated_real_typed.rs` (`2742:struct DirectParser<'i> {` + `2745:    cursor: usize,`) and `grep -n "fn parse_object_value_at_direct" skinny/crates/runtime/src/grammars/json/generated.rs` (`:466`); `grep -c "cursor: &mut usize" runtime/src/grammars/json/generated.rs` → 12 distinct Track 1 cursor parameter signatures, all confined to `runtime::generated_json::*` symbol module path; two substrates, two cursors, no crosswalk | **ACCEPT** | unchanged — V2 substrate-walk-with-shape-validation framing (P1-B §4 anomaly 4) intact; Lock 1 holds at V3 HEAD exactly as it did at V2 |
| `p1c-samply-mode-3.md` | ACCEPT (5/5; view-boundary materialization) | ANOM-2 view-boundary materialization paragraph at `:450,455` ("probe measures the cost of Lock 1's view-boundary materialization, not …" + "because the substrate union forces a second pass to lift offset-tape …") byte-identical to V2 baseline; ANOM-4 dispatch_value folded symbol framing unchanged | **ACCEPT** | unchanged — view-tree second pass remains correctly attributed as substrate-union consumer (Lock 1 holds), not as parallel substrate (Lock 1 would fail); CH5-C carried forward to S-P2 unchanged |
| `p1d-pmu-cycles.md` | ACCEPT (5/5; symbol-blind PMU carries no substrate hypothesis) | §4 anomaly 7 "the SIMD ratio is a substrate truth, not a prompt for parallel-substrate redress" unchanged; cycles/inst PMU columns at V2 byte positions; PMC-counter UNREACHABLE finding (CH6 Lock 14) preserved | **ACCEPT** | unchanged — V2 verdict carried; PMU is observation-only, no CH5 substrate implication |
| `p1e-hot-leaf-attribution.md` | ACCEPT (5/5; §4.4 canonical CH5 paragraph; F-V2-P1E-1 cite refresh closed CH5-A) | §4.4 substrate-union paragraph at `:246` ("`skip_value` is `substrate` + `dispatch` in equal parts. S-P2 must not split it into two separate primitives — it is a single substrate-union primitive …") byte-identical to V2; §1.2 grep set extension to `generated_real_typed.rs` preserved; typed-plane line anchors `:516/527/592/1150/1219/1330/2197/2949` re-verified at HEAD via `grep -n "fn parse_option_scalar_string\|fn parse_type_plugin\|fn parse_type_mesh\|fn parse_type_marine_geometry_data" skinny/crates/bbnf-bench/src/generated_real_typed.rs` matching exactly; `wc -l` → 3056 unchanged | **ACCEPT** | unchanged — CH5-A finding remains CLOSED; substrate-union framing intact; the 5 typed-plane line anchors continue to match HEAD verbatim, so the F-V2-P1E-1 fold survives one full cycle without further drift |
| `p1f-results-delta.md` | ACCEPT-WITH-CONTRACTED-DEFERRAL (CH5-B reclassified via F-V2-P1F-1 PASS-ALPHA §4.4 precedent) | 5-paragraph contracted-deferral framing block at `:179-186` byte-identical to V2; cited authorities re-verified: SYNTHESIS.md:240 carries `**NEW (CH5)**` annotation on `track2_entry_point`; SYNTHESIS.md:272 C-2 row binds R1+R2; PASS-ALPHA.md:112-122 §4.4 verbatim "This layer is authored downstream by skinny pass S-P3 in `sk-v{N+1}/SPEC.md`"; the load-bearing column-gap measurement (zero `track2_entry_point` / `comparator_plane` / `per_iter_equality` / `audit_overlay_verdict` matches in `skinny/RESULTS.md` 185 lines) re-verified at V3 HEAD via `grep -c "track2_entry_point\|comparator_plane\|per_iter_equality\|audit_overlay_verdict" skinny/RESULTS.md` → 0 | **ACCEPT-WITH-CONTRACTED-DEFERRAL** | unchanged — CH5-B remains CLOSED-VIA-CONTRACTED-DEFERRAL; the C-2 wave still owns the bench-harness emission; S-P1 cannot populate columns the harness does not emit; the contracted-deferral framing converts a downstream-wave column gap into a wave-boundary observation, not a P1 attribution failure |

**Per-axis ACCEPT-rate: 6/6 ACCEPT → 100 % ACCEPT on the CH5 lens at V3.**

Net gain vs V2: 0 (V2 was already at 100 %; V3 is the second consecutive
cycle satisfying the §3Z "≥95 % × 2 cycles" sub-clause on the CH5 axis).
Net gain vs V1: still +17 % (83 % → 100 %), now sustained across two
consecutive cycles.

## §3 — V2-finding ledger persistence

The V2 CH5 finding ledger (CH5-A through CH5-D) tracks identically at V3:

| V1/V2 Finding | V2 Status | V3 Persistence Check | V3 Status |
|---|---|---|---|
| CH5-A — source-line drift on `DirectParser::skip_value` (V1 `:1739` → V2 `:2949` +1210) | CLOSED via F-V2-P1E-1 (V2) | `grep -n "fn skip_value\|fn skip_array\|fn skip_object" skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `2949:    fn skip_value`, `2966:    fn skip_object`, `2987:    fn skip_array` — byte-identical to V2 refresh; `wc -l skinny/crates/bbnf-bench/src/generated_real_typed.rs` → 3056 unchanged; the 5 typed-plane refreshed cites (`:2949/2197/516/1150/1330`) match HEAD exactly; F-V2-P1E-1 cite hygiene held one full cycle without drift | **CLOSED (persisted)** |
| CH5-B — `track2_entry_point` column gap (0 matches across `skinny/RESULTS.md`) | CLOSED-VIA-CONTRACTED-DEFERRAL (V2) | `grep -c "track2_entry_point\|comparator_plane\|per_iter_equality\|audit_overlay_verdict" skinny/RESULTS.md` → 0 (185 lines); column gap still total; SYNTHESIS.md §2:232-258 + §3:272 + PASS-ALPHA.md §4.4:112-122 cite chain all byte-identical at V3 HEAD; the C-2 wave remains the contracted owner; the framing paragraph at `p1f-results-delta.md:179-186` unchanged | **CLOSED-VIA-CONTRACTED-DEFERRAL (persisted)** |
| CH5-C — view-tree consumer source-touch budget unbounded in schema (P1-C ANOM-2) | DEFERRED-TO-S-P2 (substrate-design; V2) | ANOM-2 view-boundary materialization framing at `p1c-samply-mode-3.md:450,455` byte-identical; the S-P2 deliverable (a `view_source_touch_ratio` or equivalent telemetry field that mechanically gates view-tree source re-touch as substrate-union consumer vs parallel substrate) remains unaltered | **DEFERRED-TO-S-P2 (persisted)** |
| CH5-D — `parse-attribution=off` folds 7 Track 1 leaves into `dispatch_value` envelope | DEFERRED-TO-S-P2 (primitive-census ground-truth; V2) | The deferred F-V2-P1ABC-RERECORD heavy packet (per V1 aggregator Option X and V2 aggregator commit body "S-P2 entry artefact: F-V2-P1ABC-RERECORD") remains a primitive-design ground-truth task, not a lens-correctness defect; envelope-folded symbols still resolve into `runtime::generated_json::*`, so Lock 1 holds regardless of `parse-attribution` gate state | **DEFERRED-TO-S-P2 (persisted)** |

**Closure persistence:** 2/4 CLOSED findings hold cleanly across one full
cycle without regression; 2/4 DEFERRED findings remain correctly framed
as S-P2-design-class, not S-P1-lens-class. Zero V3 reopens, zero V3
status downgrades, zero V3 cite drift on the closed findings.

## §4 — V3 fresh-finding scan

Per the dispatch task's pure-confirming framing, the V3 fresh-finding scan
re-runs the V2 §4 scan over the unchanged P1 artefacts and tests for any
hidden-coupling signal that:

(a) emerged from the V2 micro-fold paragraphs themselves (the
F-V2-P1E-1 grep-set extension + the F-V2-P1F-1 contracted-deferral block);
(b) emerged from the V2 aggregator/CH5 paragraphs being read alongside the
underlying artefacts;
(c) emerged from any HEAD source-tree drift between V2 record-time and V3
re-record time (`bbnf-bench/src/generated_real_typed.rs` + `runtime/src/
grammars/json/generated.rs`).

**Fresh-finding scan result: ZERO new CH5 findings at V3.**

The scan covered six axes:

1. **V2 micro-fold paragraph CH5 implications (F-V2-P1E-1).** The
   F-V2-P1E-1 fold added `generated_real_typed.rs` to the P1-E §1.2 grep
   set and refreshed 5 typed-plane line anchors. Re-read at V3 HEAD: the
   paragraph reads as a mechanical cite-refresh; the symbol identities
   (`skip_value`, `parse_option_scalar_string`, `parse_type_plugin`,
   `parse_type_mesh`, `parse_type_marine_geometry_data`) all resolve
   inside the Track 2 module path (`bbnf_bench::generated_real_typed::*`)
   per V2 §1.2 framing. No Track 1 ≡ Track 2 collision; no parallel
   substrate emerges from the cite-refresh. **No CH5 implication.**

2. **V2 micro-fold paragraph CH5 implications (F-V2-P1F-1).** The
   F-V2-P1F-1 5-paragraph contracted-deferral block at
   `p1f-results-delta.md:179-186` cites SYNTHESIS.md + PASS-ALPHA.md.
   Re-read at V3 HEAD: the block does not introduce any new substrate
   claim; it reframes the column-gap measurement (zero matches in
   `skinny/RESULTS.md`) as a downstream C-2 wave deliverable rather than
   an S-P1 authoring failure. The substrate-union story is untouched.
   **No CH5 implication.**

3. **V2 aggregator paragraph cross-references.** `HARDENING-S-P1-V2-
   CONSOLIDATED.md:36` row CH5 records "CH5-A cite drift closed by
   F-V2-P1E-1; CH5-C/CH5-D carried to S-P2 by design; substrate-union
   framing intact"; `:63` row records the F-V2-P1F-1 reclassification
   mechanism; `:131` records "Track 1 entry-points (`parse_object_value_at_
   direct::<JsonDigestSink>`, `parse_only`) and Track 2 entry-points
   (`DirectParser::skip_value` at `generated_real_typed.rs:2949`,
   `HandParser::value`) remain distinguishable by inspection, sharing no
   `runtime::tape::*` ancestor beyond the public types". Cross-checked at
   V3 HEAD: `grep -n "fn parse_object_value_at_direct" skinny/crates/
   runtime/src/grammars/json/generated.rs` → 1 hit at `:466`; the symbol
   sits in `runtime::generated_json::generated` not `runtime::tape::*`;
   Track 2's `DirectParser::skip_value` at `generated_real_typed.rs:2949`
   sits in `bbnf_bench::generated_real_typed` not `runtime::tape::*`;
   common ancestor is the crate-graph root, not `runtime::tape::*`. Lock
   1 (substrate union without symbol-path collapse) holds. **No CH5
   implication.**

4. **HEAD source-tree drift between V2 record-time and V3 re-record
   time.** `wc -l skinny/crates/bbnf-bench/src/generated_real_typed.rs` →
   3056 (V2 also 3056; zero net drift); `grep -n "fn skip_value\|fn
   skip_array\|fn skip_object" …` → `2949/2966/2987` (V2 also
   `2949/2966/2987`; zero drift); `grep -n "fn parse_option_scalar_
   string\|fn parse_type_plugin\|fn parse_type_mesh\|fn parse_type_
   marine_geometry_data" …` → `516/527/592/1150/1219/1330/2197` (V2
   identical); `grep -n "struct DirectParser\|cursor: usize" …` →
   `2742:struct DirectParser<'i> {` + `2745:    cursor: usize,` (V2
   identical); `grep -n "fn parse_object_value_at_direct" skinny/crates/
   runtime/src/grammars/json/generated.rs` → `:466` (V2 cited `:466,469`
   — the `:469` was the inner cursor-parameter signature; both still
   present); `grep -c "cursor: &mut usize" runtime/src/grammars/json/
   generated.rs` → 12 distinct signatures (Track 1 cursor parameter
   uniformly typed across all 12 call sites; no shared mutable state with
   Track 2's `DirectParser::cursor` field). **Zero source drift; the
   F-V2-P1E-1 cite refresh survives one full cycle unchanged.**

5. **`skinny/RESULTS.md` schema column population check.** `wc -l
   skinny/RESULTS.md` → 185 lines (V2 cited 186; off-by-one likely a
   trailing-whitespace difference, immaterial); `grep -c
   "track2_entry_point\|comparator_plane\|per_iter_equality\|audit_overlay_
   verdict" skinny/RESULTS.md` → 0. The CH5 hidden-coupling guard column
   remains absent at V3 HEAD exactly as at V2; the C-2 wave remains the
   contracted owner; the contracted-deferral framing remains
   structurally honest. **No CH5 implication; CH5-B persists as
   CLOSED-VIA-CONTRACTED-DEFERRAL.**

6. **Substrate-union framing across V3 baseline (re-verify three
   canonical CH5 paragraphs).**
   - `p1e-hot-leaf-attribution.md:246` §4.4 substrate-union paragraph
     ("`skip_value` is `substrate` + `dispatch` in equal parts. S-P2 must
     not split it into two separate primitives …") byte-identical to V2.
   - `p1c-samply-mode-3.md:450,455` ANOM-2 view-boundary materialization
     paragraph ("probe measures the cost of Lock 1's view-boundary
     materialization, not …" + "because the substrate union forces a
     second pass to lift offset-tape …") byte-identical to V2.
   - `p1a-samply-mode-1.md:318-321` Lock-1 same-substrate union signal for
     `copy_nonoverlapping` ("This is **tape-commit pressure**, the
     Lock-1 same-substrate union signal …") byte-identical to V2.

All three CH5 load-bearing paragraphs intact at V3 HEAD. No new substrate,
sidecar, retained cursor, second source scan, or Track 1 ≡ Track 2 symbol-
path collapse surfaces between V2 record-time and V3 re-record time.

## §5 — V3 §3Z two-cycle-chain closure

`ORCHESTRATOR.md §3Z` requires "≥95 % × 2 cycles, zero orphan REVISEs" to
gate S-P2 dispatch. For the CH5 axis:

- **V2 CH5:** 100 % ACCEPT (6/6), zero orphan REVISE. First ≥95 % cycle.
- **V3 CH5:** 100 % ACCEPT (6/6), zero orphan REVISE. Second consecutive
  ≥95 % cycle.

**§3Z two-cycle-chain CLOSES on CH5 at V3.** Combined V2+V3 satisfies the
"≥95 % × 2 cycles" sub-clause without recourse to V4. CH5 gate is OPEN
for S-P2 dispatch.

Per the V2 aggregator forecast (`HARDENING-S-P1-V2-CONSOLIDATED.md:179`
row CH5 "Hold; CH5-C + CH5-D carried to S-P2; substrate-union framing
intact"), V3 confirms verbatim: V3 ACCEPT-rate equals V2 ACCEPT-rate;
the deferred S-P2 findings (CH5-C view-tree substrate accounting; CH5-D
parse-attribution unmasking) remain correctly framed as S-P2 design-class;
the substrate-union canonical paragraphs remain unaltered.

Aggregate convergence across the seven lenses is the V3 aggregator's
responsibility per §3Z; the CH5 axis closes its two-cycle-chain at V3
standalone, independent of the cross-lens aggregate.

## §6 — Sources cited (executable-verification)

Verified per CHALLENGE-CONTEXT §3 + dispatch task's executable-verification
mandate.

**V3 dispatch + V2 carry-through:**

- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md` (53 lines; read end-to-end).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH5.md` (243 lines; read end-to-end; 100 % ACCEPT (6/6); CH5-A closed via F-V2-P1E-1 cite refresh; CH5-B reclassified via F-V2-P1F-1 PASS-ALPHA §4.4 contracted-deferral).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md` (CH5 rows at `:36`, `:63`, `:131`, `:179`, `:255` all read and re-verified at V3 HEAD).

**V3 P1 artefacts (commit `4ad8f1949` HEAD; identical to `069ba203c`
artefact set):**

- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` (V3 HEAD; substrate-union paragraph at `:318-321` byte-identical to V2; `copy_nonoverlapping` rows at `:142,143,150` unchanged).
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` (V3 HEAD; two-cursor independence + substrate-walk-with-shape-validation framing intact).
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` (V3 HEAD; ANOM-2 view-boundary materialization paragraph at `:450,455` byte-identical to V2).
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` (V3 HEAD; symbol-blind PMU carries no substrate hypothesis; §4 anomaly 7 unchanged).
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` (V3 HEAD; F-V2-P1E-1 typed-plane cite refresh at `:155-167` persisted unchanged; §4.4 substrate-union paragraph at `:246` byte-identical to V2; §1.2 grep set extension to `generated_real_typed.rs` at `:63-76` persisted).
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md` (V3 HEAD; F-V2-P1F-1 5-paragraph contracted-deferral block at `:179-186` byte-identical to V2; row `:175` schema-gap line unchanged).

**Authority bindings (V3 re-verification):**

- `restart/prompts/skinny/PASS-1-PROFILE.md §3 CH5` (binding for CH5 lens definition).
- `restart/prompts/ORCHESTRATOR.md §3W CH5` (universal CH5 definition: "No parallel substrate, sidecar producer, renamed-scanner Lock 1 violation, or Track 1 ≡ Track 2 dishonesty; substrate union holds.").
- `restart/prompts/ORCHESTRATOR.md §3Z` (convergence rule: ≥95 % × 2 cycles, zero orphan REVISEs).
- `restart/prompts/pass-contracts/PASS-ALPHA.md §4.4` (lines 112-122; contracted-deferral precedent verified verbatim: "This layer is authored downstream by skinny pass S-P3 in `sk-v{N+1}/SPEC.md`").
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md §2` Telemetry Binding (lines 232-258; 4 NEW column declarations re-verified at V3 HEAD: `track2_entry_point` at `:240` with `**NEW (CH5)**` annotation; `comparator_plane` at `:241`; `per_iter_equality` at `:242`; `audit_overlay_verdict` at `:255`).
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md §3` C-2 row (line `:272`; R1+R2 bench-harness emission + `xtask gate-json` consumer binding).

**Source-code verification (V3 HEAD; zero drift from V2):**

- `wc -l skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `3056` lines (V2 also 3056; zero drift).
- `grep -n "fn skip_value\|fn skip_array\|fn skip_object" skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `2949:    fn skip_value`, `2966:    fn skip_object`, `2987:    fn skip_array` (V2 identical).
- `grep -n "fn parse_option_scalar_string\|fn parse_type_plugin\|fn parse_type_mesh\|fn parse_type_marine_geometry_data" skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `516`, `527`, `592` (plugin variants); `1150`, `1219` (mesh variants); `1330` (marine_geometry_data); `2197` (parse_option_scalar_string) — V2-refreshed line set matches V3 HEAD exactly.
- `grep -n "struct DirectParser\|cursor: usize" skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `2742:struct DirectParser<'i> {` + `2745:    cursor: usize,` (Track 2 cursor field; V2 identical).
- `grep -n "fn parse_object_value_at_direct" skinny/crates/runtime/src/grammars/json/generated.rs` → `:466` (Track 1 entry point; V2 cited `:466,469` — both still resolvable, `:469` is the inner cursor-parameter line of the same signature).
- `grep -c "cursor: &mut usize" skinny/crates/runtime/src/grammars/json/generated.rs` → 12 distinct Track 1 cursor parameter signatures, all confined to `runtime::generated_json::*` module path; no shared mutable state with Track 2's `DirectParser::cursor` field.
- `wc -l skinny/RESULTS.md` → 185 lines.
- `grep -c "track2_entry_point\|comparator_plane\|per_iter_equality\|audit_overlay_verdict" skinny/RESULTS.md` → 0 (column gap remains total; C-2 wave deliverable; contracted-deferral framing persists).
- Two cursors, two substrates, no crosswalk — Lock 1 holds at V3 HEAD exactly as it held at V2 HEAD.

**Commit verification:**

- `git log --oneline -3` → `4ad8f1949 docs(sk-v14-p1-hardening-V2): challenge V2 + consolidated` (HEAD); `069ba203c docs(sk-v14-p1-profile): V2 light micro-redispatch — five orphan REVISEs landed`; `a3dfcaf38 docs(sk-v14-p1-hardening-V1): challenge V1 + consolidated`.
- `git diff 069ba203c..HEAD -- restart/skinny/tranches/sk-v14/research/p1/p1{a,b,c,d,e,f}*.md restart/skinny/tranches/sk-v14/SYNTHESIS.md` → empty (V2 aggregator commit touched only `hardening/V2/*.md`; the underlying P1 artefacts and SYNTHESIS schema binding are bit-identical to the V2 baseline).
- The V3 confirming pass therefore operates on the identical artefact set that V2 graded at 100 % ACCEPT; the verdict carries by source-of-truth identity, reinforced by the V3 re-verification grep set above.
