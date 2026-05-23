# SK-V14 S-P1 Profile — V1 CHALLENGE Consolidated

Aggregator: SK-V14 S-P1 V1 hardening aggregator (write-only).
Date (UTC): 2026-05-23.
Scope: seven-lens CHALLENGE V1 over the six committed S-P1 P1 axis artefacts
(2547c750bc78533d738eb85913206a0872022818 — `docs(sk-v14-p1-profile): seed
S-P1 dispatch context — six P1 agents + 17/17 corpus discipline`; 2481 lines
across `p1a-samply-mode-1.md` … `p1f-results-delta.md`).
Authority: `restart/prompts/ORCHESTRATOR.md §3W` (lens registry) + `§3Z`
(convergence rule); `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH1-CH6
specialisations + §6 dispatch gate); `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7`
(Overfit-Prune lens binding from S-P0); dispatch
`restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md`
§0-§5.
Input ledger: seven V1 lens dispositions under
`restart/skinny/tranches/sk-v14/research/p1/hardening/V1/`
(`CH1.md` 262 lines, `CH2.md` 219, `CH3.md` 289, `CH4.md` 253, `CH5.md` 221,
`CH6.md` 234, `CH7.md` 258 — 1736 lens lines + 53 CHALLENGE-CONTEXT lines).

## §0 — V1 cycle verdict

### §0.1 Per-lens dispositions (verbatim from each CH file's §0/§1)

| Lens | Definition | Sub-axes | ACCEPT | REVISE | REJECT | Per-lens ACCEPT-rate | Verdict |
|---|---|---:|---:|---:|---:|---:|---|
| CH1 CORRECTNESS | hot-leaf cites samply symbol + % self-time + source file:line; c/B from real PMU; 17/17 coverage; every `unprofiled` resolved; atos `-inlineFrames` headless equivalence to interactive samply | 115 anchors | 103 | 12 (incl. ACCEPT-WITH-NOTE) | 0 | **89.6 %** | ACCEPT-WITH-REVISE (one orphan REVISE: P1-E typed-plane line drift) |
| CH2 GENERALITY | hot leaves named to grammar-neutral primitives, not JSON-named code paths; envelope mis-attribution flagged for S-P2 `parse-attribution` enablement | 4 (in-scope artefacts) | 4 | 0 | 0 | **100 %** | ACCEPT (2 non-blocking findings F1 + F2) |
| CH3 REGRESSION (REDRESS) | no §4 anomaly silently re-opens REDRESS 50-55 / 60-72 / 80 / 82-84 / 88-89 / 96-98 / 126; pre-blocked routes carry explicit cite | 6 artefacts × 43 anomalies | 6 | 0 | 0 | **100 %** | ACCEPT (5 non-blocking findings F-1 through F-5) |
| CH4 COST | verbatim commands; host triple; build flags; samply version; run id; PMU access matrix all reproducible | 31 sub-axes | 29 | 2 | 0 | **93.5 %** | ACCEPT-WITH-REVISE (one orphan REVISE: CF-1 RUSTFLAGS regime drift affecting P1-A + P1-B) |
| CH5 HIDDEN COUPLING | no parallel substrate / sidecar / retained cursor / second source scan / Track 1 ≡ Track 2 collapse; substrate union holds | 6 axes (a..e + schema-enforcement) | 5 | 1 | 0 | **83 %** | ACCEPT-WITH-REVISE (one orphan REVISE: P1-F `track2_entry_point` schema column gap — reclassifiable per §2 below) |
| CH6 ANTI-PAPER-CLOSE | flame profile file exists on disk + symbol resolvable; every `unprofiled` carries stated cause; folded-symbol risks routed to concrete V2 fold | 6 artefacts (77/77 flame paths verified) | 6 | 0 | 0 | **100 %** | ACCEPT (3 V2 MUST-queue items: parse-attribution rebuild for P1-A/B/C + github_events re-record + path-layout standardisation) |
| CH7 OVERFIT-PRUNE | audit-overlay column populated per row; no fake `@generated` / scaffold-as-load-bearing / gate-relabel recurrence; PRUNE-1 W14.* AUDIT-FALSIFIED consistency | 7 subclauses (4 explicit + 3 supporting verifications) | 7 | 0 | 0 | **100 %** | ACCEPT (zero V2-required redress) |

### §0.2 Aggregate ACCEPT-rate

Two aggregation methods (per `ORCHESTRATOR.md §3Z`):

- **Sub-axis-weighted (load-bearing for §3Z convergence):**
  (103+4+6+29+5+6+7) / (115+4+6+31+6+6+7) = **160 / 175 = 91.4 %**.
- **Per-lens mean (informational; equal weight per lens):**
  (89.6 + 100 + 100 + 93.5 + 83 + 100 + 100) / 7 = **95.16 %**.

The sub-axis-weighted aggregate (91.4 %) is **below the §3Z ≥95 % floor**;
the per-lens mean (95.16 %) is marginally above. Per `ORCHESTRATOR.md §3Z`
the binding rule is "≥95 % × 2 consecutive cycles + zero orphan REVISEs";
this cycle satisfies neither sub-clause (sub-axis aggregate below floor AND
three orphan REVISEs outstanding).

### §0.3 REJECT roster (verbatim)

**Zero REJECT findings** across all 7 lenses. The seven-lens sweep
surfaces no falsification of any P1 axis claim.

### §0.4 REVISE roster (verbatim)

Three orphan REVISEs:

1. **CH1 P1-E typed-plane file:line drift** (CH1.md §3 Finding 1; CH1.md
   §2.5 verdict REVISE). Five typed-plane hot-leaf citations in
   `p1e-hot-leaf-attribution.md §2.3` reference SK-V13-era line numbers
   stale against HEAD `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
   (file grew ≈1500 lines since the SK-V13 V2 capture; HEAD = 3056 lines;
   cited lines suggest a ≈1500-1750-line file). Concrete drifts:
   `DirectParser::skip_value` cited 1739 / HEAD 2949 (+1210);
   `parse_option_scalar_string` cited 1199 / HEAD 2197 (+998);
   `parse_type_plugin` cited 473 / HEAD 516 (+43) [or `_ordered` 592 (+119)];
   `parse_type_mesh` cited 828 / HEAD 1150 (+322);
   `parse_type_marine_geometry_data` cited 1015 / HEAD 1330 (+315).
   Symbol identities are correct; only line-anchors drift. P1-E §1.2
   re-greps `generated.rs` + `scan.rs` + `parse-that-regex/src/lib.rs`
   at HEAD but does **not** extend the grep set to
   `generated_real_typed.rs` — exactly the file where drift sits.
   Mechanical re-grep fix; CH1 raises lens from 89.6 % to ≈100 % on the
   one mechanical edit.

2. **CH4 CF-1 RUSTFLAGS regime drift** (CH4.md §3 CF-1; CH4.md §2.1 + §2.2
   verdict REVISE). The four primary-capture artefacts split into two
   `RUSTFLAGS` regimes: P1-A asserts native-CPU "per Cargo.toml" but no
   shell block carries `RUSTFLAGS="-C target-cpu=native"` (Cargo.toml does
   not pin target-cpu either); P1-B explicitly `RUSTFLAGS unset (default
   aarch64 baseline)`; P1-C + P1-D explicitly `RUSTFLAGS="-C
   target-cpu=native"`. Cross-artefact c/B + Mbps comparisons (e.g. P1-B
   Track 1 direct twitter 11037 Mbps vs P1-D Track 1 direct twitter 11627
   Mbps; 5.3 % Mbps + 2.1 % c/B drift) are therefore not at the same
   build-flag baseline. CH4 binding: `[no-warm-benches]` forbids silent
   build-flag divergence between rows compared in the same table.

3. **CH5 P1-F `track2_entry_point` schema column gap** (CH5.md §3 Finding
   CH5-B; CH5.md §2 P1-F row REVISE). The mandated CH5 hidden-coupling
   guard column (`SYNTHESIS.md §2:240` — "symbol path of the Track 2
   oracle entry point; `xtask gate-json` rejects any row where the Track
   1 and Track 2 entry-point symbol paths share a common ancestor in
   `runtime::tape::` beyond the public `Tape` / `OffsetFlags` types") is
   absent from every per-row table in `RESULTS.md` (zero matches across
   186 lines). CH5 cannot **mechanically** enforce the no-cross-ancestor
   guard per row until the column lands. CH5 narrative coverage across
   P1-A/B/C/E is sufficient for V1 ACCEPT at the symbol-path level — the
   Track 1 entry points (`parse_only`,
   `parse_object_value_at_direct::<JsonDigestSink>`) and Track 2 entry
   points (`hand::HandParser::value`, `DirectParser::skip_value`) are
   distinguishable by inspection and share no `runtime::tape::*` ancestor
   beyond the public types — but the schema enforcement is deferred to
   the C-2 wave (R2 + schema rewrite). Reclassifiable as a
   contracted-deferral to C-2 (parallel to Pass Alpha §4.4 → S-P3
   boundary precedent): the column is a downstream C-2 deliverable, not
   a P1 attribution failure.

### §0.5 Convergence vote

Per `ORCHESTRATOR.md §3Z` (≥95 % × 2 cycles, zero orphan REVISEs):

- V1 is the **first** cycle (no prior ≥95 % cycle to chain).
- Sub-axis aggregate **91.4 % below floor**; per-lens mean 95.16 %
  marginally above but does not discharge the "× 2 cycles" requirement.
- **Three orphan REVISEs** (CH1 P1-E line drift; CH4 CF-1 RUSTFLAGS;
  CH5 P1-F schema gap reclassifiable).

**Cycle verdict: NOT-CONVERGED-V2-REQUIRED.** V2 must (a) clear the
three orphan REVISEs and (b) drive the sub-axis aggregate above the
95 % floor with zero new orphan REVISEs; only then can a V3 cycle
attempt the "× 2 cycles" close.

## §1 — Per-artefact convergence digest

For each of the six P1 artefacts, cross-lens disposition pressure
(consolidated from CH1 §1, CH2 §2, CH3 §1, CH4 §1, CH5 §2, CH6 §0, CH7 §1):

### §1.1 — `p1a-samply-mode-1.md` (340 lines; Samply mode I, parse_only × 17)

| Lens | Verdict | Pressure |
|---|---|---|
| CH1 | ACCEPT-WITH-NOTE (92 %) | 2 line annotations: `movemask_u8x16` line 22 cites inner-loop hot-bit-or vs fn signature at line 4; `match_tiny_plain_string_with_cap::<16>` at 160,176 mixes call-site + inner-loop with fn at 169 (both defensible inline-fold attribution; raise to 100 % with annotation) |
| CH2 | ACCEPT | Outer envelope + top-inlined-leaf columns together discharge CH2 GENERALITY; §238-251 closes the loop |
| CH3 | ACCEPT | §267-274 explicit 5-inference pre-block map (REDRESS-50, 51, 60, 83, 84) — canonical CH3 statement |
| CH4 | ACCEPT-with-finding | RUSTFLAGS asserted "per Cargo.toml" but no shell block carries it; participates in CF-1 cross-artefact drift |
| CH5 | ACCEPT | No parallel substrate; `copy_nonoverlapping` 9.5-11.4% correctly flagged as tape-commit pressure (substrate-union) |
| CH6 | ACCEPT | `dispatch_value` LTO-fused envelope remediated via `atos -inlineFrames` against dSYM; inlined/inline__<corpus>.txt artefacts materialised at record time |
| CH7 | ACCEPT | 5/5 W14.* rows AUDIT-FALSIFIED in §2.1 table; zero `@generated` recurrence; line 204 "gate-relabel" diagnostic carry, not a new admit |

**Pressure summary:** ACCEPT-class on 7/7 lenses; one CH1 line-annotation
fold + one CF-1 RUSTFLAGS regime disclosure fold. No structural revision.

### §1.2 — `p1b-samply-mode-2.md` (320 lines; Samply mode II, direct × 17 + typed × 11)

| Lens | Verdict | Pressure |
|---|---|---|
| CH1 | ACCEPT (100 %) | Best-of-six on file:line anchor discipline; breakpadId-vs-codeId join quirk documented; §1.3 atos pipeline equivalence audit-grade |
| CH2 | ACCEPT-with-noted-imprecision | `DirectParser::skip_value` correctly named substrate-walk-with-shape-validation; namespace `bb::grt` is bench-harness-private, blocking cross-grammar generalization until S-P2 namespace promotion (R3 in CH2 §4.1) |
| CH3 | ACCEPT | Anomaly 1 (driver overhead → `JsonSink` fold) + Anomaly 4 (`DirectParser::skip_value` tape-only walk) neither map to pre-blocked REDRESS family |
| CH4 | ACCEPT-with-finding | Explicit `RUSTFLAGS unset` (cleanest disclosure of the four) but directly contradicts P1-C/D regime — central to CF-1 |
| CH5 | ACCEPT (conditional) | Two-cursor independence verified (`DirectParser::cursor` at `generated_real_typed.rs:2745` ≠ Track 1's `parse_object_value_at_direct(&mut cursor)` thread); Lock 1 holds |
| CH6 | ACCEPT WITH DEPENDENCY | 56/56 profiles on disk + 56 syms sidecars verified; inherits parse-attribution fold risk from P1-A (V2 MUST queue) |
| CH7 | ACCEPT | 72 audit-overlay cells; W14.* corpora carried in direct + typed lanes with AUDIT-FALSIFIED verdict per row |

**Pressure summary:** ACCEPT-class on 7/7 lenses; central to CF-1 RUSTFLAGS
re-record decision (V2 fold §2 below).

### §1.3 — `p1c-samply-mode-3.md` (607 lines; Samply mode III, 4 probes)

| Lens | Verdict | Pressure |
|---|---|---|
| CH1 | ACCEPT-WITH-NOTE (84 %) | 3 NEON primitive line off-by-ones (`bulk_emit_positions_64_neon` 3→2; `bitmap_prefix_xor_64_neon` 3→2; `eob_pad_clamp_neon` 5→4); samply attributes `#[inline(always)]` attribute line vs fn signature |
| CH2 | ACCEPT | ANOM-4 (`generated.rs:45` envelope + `Cargo.toml:21` feature gate) names cause; ANOM-5 names PEXT-impossible-on-aarch64 substrate; §2.2.4 SIMD ratios attribute to `scan_structurals` (grammar-neutral) |
| CH3 | ACCEPT-WITH-NOTE | ANOM-6 binds REDRESS-126 zero-orphan to ANOM-1/2/3; F-1 documentary path drift (`restart/skinny/REDRESS.md` should be `skinny/REDRESS.md`) on lines 500, 590 |
| CH4 | ACCEPT | All build/capture/extraction blocks reproducible; aggregate-only attribution disclosed (CF-3 ACCEPT-with-note) |
| CH5 | ACCEPT (conditional) | ANOM-2 view-walk + UTF-8 decode correctly named Lock 1 view-boundary materialization; CH5-C flags view-tree source-touch budget as S-P2 schema deliverable (`view_source_touch_ratio`) |
| CH6 | ACCEPT | ANOM-4 explicitly names CH6 paper-close risk + routes V2 to `--features parse-attribution`; 4 probe profile pairs on disk |
| CH7 | ACCEPT | 5/5 W14.* AUDIT-FALSIFIED in §2.1 mode-III table; line 400 "gate-relabel" diagnostic |

**Pressure summary:** ACCEPT-class on 7/7 lenses; CH1 mechanical line-anchor
re-grep (F-V2-P1C-LINEDRIFT) + CH3 path normalisation. No structural
revision.

### §1.4 — `p1d-pmu-cycles.md` (648 lines; PMU + cycles-per-byte; 231 rows)

| Lens | Verdict | Pressure |
|---|---|---|
| CH1 | ACCEPT (100 %) | Best-of-six on PMU discipline; §1.4 escalation matrix exactly resolves "real PMU not estimated" without elevating wall-time-derived c/B |
| CH2 | OUT-OF-SCOPE | PMU is quantitative, not attribution-named; CH2 binding holds on symbol-name attribution |
| CH3 | ACCEPT | Anomaly 2 explicitly "MASKING signal is real noise, not a redress prompt"; Anomaly 6 pre-blocked against REDRESS 96/97/98 |
| CH4 | ACCEPT (CH4 spotlight) | PMU access matrix byte-identical to SK-V13 V3 lock-in; cycles + inst REACHABLE via `proc_pid_rusage(V5)`; PMC UNREACHABLE unprivileged; sudo refusal recorded in identity.txt |
| CH5 | ACCEPT | Per-process counters; no substrate hypothesis |
| CH6 | ACCEPT | 231 rows on disk; xctrace cpu-state.xml present at 60 MiB; identity.txt verbatim |
| CH7 | ACCEPT | Mean Δ c/B = −0.063 c/B (−1.0 %) across 17 direct rows confirms zero-source-byte SK-V14 baseline; 67 audit-overlay cells |

**Pressure summary:** ACCEPT 7/7 lenses (CH2 OUT-OF-SCOPE). The
load-bearing PMU artefact converges cleanly; no V2 fold required.

### §1.5 — `p1e-hot-leaf-attribution.md` (306 lines; CH2 Lock-14 mis-attribution census)

| Lens | Verdict | Pressure |
|---|---|---|
| CH1 | REVISE (67 %) | 5/7 typed-plane file:line cites stale vs HEAD `generated_real_typed.rs`; §1.2 re-greps `generated.rs` but not `generated_real_typed.rs` — diagnosable failure mode. **Orphan REVISE.** |
| CH2 | ACCEPT (load-bearing artefact) | §1.3 primitive-classification table + per-row `Primitive class (CH2-neutral)` + `Lock-14 mis-attribution?` columns + §4.1 census discharge dispatch §2 verbatim |
| CH3 | ACCEPT | §4.7 verbatim 8-family REDRESS guard reconciliation (REDRESS 50-55, 60-72, 80, 82-84, 88-89, 96-98, 126); load-bearing CH3 statement for the tranche |
| CH4 | N/A | Synthesis artefact; no primary capture |
| CH5 | ACCEPT (canonical CH5 paragraph) | §4.4 "substrate-union (Lock 1) substrate-vs-producer mixing — hybrid that walks substrate while validating type-shape; do not split"; Lock 14 generalization-induced-collapse guard |
| CH6 | ACCEPT | Lock-14 envelope mis-attribution names V2 fold route at line 110 ("S-P2 must crack dispatch_value open via parse-attribution cargo feature"); github_events 8-sample + instruments inlined-std noise flagged + routed |
| CH7 | ACCEPT | 37 audit-overlay cells; line 17 enumerates "5 parse_only + 4 direct + 7 typed + 24 CSS = 40 rows AUDIT-FALSIFIED" |

**Pressure summary:** Synthesis artefact carries the single orphan REVISE
(CH1 P1-E typed-plane line drift). Six other lenses ACCEPT load-bearing
findings. V2 fold: mechanical re-grep against HEAD `generated_real_typed.rs`
(F-V2-P1E-1 below).

### §1.6 — `p1f-results-delta.md` (260 lines; RESULTS extraction + Δ vs SK-V13)

| Lens | Verdict | Pressure |
|---|---|---|
| CH1 | ACCEPT (100 %) | Every audit citation carries path:line; gap-by-gap honest enumeration of 4 NEW SK-V14 schema columns absent from RESULTS.md at §4.1 |
| CH2 | OUT-OF-SCOPE | Tracks throughput, not primitive attribution |
| CH3 | ACCEPT | Six §4 subsections all schema-gap / telemetry-drift / row-count findings; no parser-route proposed |
| CH4 | N/A | Documentary extraction; no primary capture |
| CH5 | REVISE (schema-column gap) | `track2_entry_point` column gap correctly identified at §4.1:175 but the column is a C-2 deliverable, not a P1 attribution failure. **Reclassifiable orphan REVISE.** |
| CH6 | ACCEPT | Documentary; no original profile claims; `n/a:w1b-2b-report-gate-consumes-w1b-2a-criterion` flagged as stale-artifact (CSS) |
| CH7 | ACCEPT | 58 audit-overlay cells; 5/5 W14.* AUDIT-FALSIFIED closure at §397-398 |

**Pressure summary:** ACCEPT 6/7 lenses (CH2 OUT-OF-SCOPE; CH5 REVISE
reclassifiable to C-2 deferral). The single orphan REVISE is a downstream
schema-column gap, not a P1 attribution failure — F-V2-P1F-1 reframing
(below) closes the lens at zero cost.

## §2 — V2 fold dispositions

Six V2 fold packets prescribed below. Five are **light** mechanical edits
(total ≈26 min wall) that close the three orphan REVISEs + two
ACCEPT-WITH-NOTE annotations. One is **heavy** wave-program work
(F-V2-P1ABC-RERECORD, ≈60-135 min wall) classified for orchestrator
decision: defer to S-P2 design phase OR run as S-P1 V2-extension cycle.

### §2.1 — F-V2-P1E-1 — P1-E typed-plane file:line refresh (LIGHT)

**Closes:** CH1 orphan REVISE (Finding 1); CH2 §3.1 R1 (P1-E §2.3
call-site-vs-definition drift) + CH2 §3.2 R2 (P1-E §2.2 distinct_values
cap-variant misnomer); CH5 §3 Finding CH5-A (source-line drift on
`DirectParser::skip_value`).

**Scope:** Re-grep `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
(HEAD = 3056 lines) for: `fn skip_value`, `fn skip_array`, `fn skip_object`,
`fn parse_option_scalar_string`, `fn parse_type_plugin`, `fn
parse_type_plugin_ordered`, `fn parse_type_mesh`, `fn
parse_type_marine_geometry_data`, `fn parse_type_instrument*`, `fn
parse_vec_cap_10800_scalar_f64`. Update `p1e-hot-leaf-attribution.md §2.3`
file:line citations. **Extend P1-E §1.2 grep set** to include
`generated_real_typed.rs` so the re-grep discipline is repeatable. Symbol
identities are correct; only line-anchors drift.

**Cost:** ≈10 min wall (LOW). Single CH1 mechanical edit.

**Convergence impact:** CH1 89.6 % → ≈100 % on the one mechanical edit
(P1-E from 67 % → 100 %; aggregate sub-axis weighted from 91.4 % → 97.7 %).
CH5 §3 CH5-A cite-hygiene finding clears; CH2 R1+R2 fold naturally.

### §2.2 — F-V2-P1F-1 — P1-F `track2_entry_point` schema-gap reclassification (LIGHT)

**Closes:** CH5 orphan REVISE (Finding CH5-B; §0 disposition table P1-F
row).

**Scope:** Reclassify `track2_entry_point` schema gap as
**contracted-deferral to C-2 wave** (precedent: Pass Alpha §4.4 → S-P3
boundary). The column population is a C-2 R2 deliverable (per
`SYNTHESIS.md §2:240`); P1 cannot deliver unilaterally because the bench
harness does not emit the column. P1-F adds explicit framing at the
`§4.1:175` schema-gap callout:

> "`track2_entry_point` column is **deferred to C-2 R2 schema rewrite**; not
> an S-P1 attribution failure. CH5 narrative coverage across P1-A/B/C/E
> distinguishes Track 1 and Track 2 entry-points by inspection at the
> symbol-path level (Track 1: `parse_object_value_at_direct::<JsonDigestSink>`
> + `parse_only`; Track 2: `DirectParser::skip_value` + `HandParser::value`),
> sharing no `runtime::tape::*` ancestor beyond the public `Tape` /
> `OffsetFlags` types. Mechanical per-row gate enforcement lands when C-2
> populates the column."

**Cost:** ≈5 min wall (LOW). Reframing, not architectural.

**Convergence impact:** CH5 83 % → 100 % (the lone REVISE reclassifies as
contracted-deferral, not unresolved orphan).

### §2.3 — F-V2-METHODOLOGY-1 — RUSTFLAGS regime disclosure (LIGHT, Option A) or unification (HEAVY, Option B)

**Closes:** CH4 orphan REVISE (CF-1 RUSTFLAGS drift affecting P1-A + P1-B).

Two options:

**Option A (LIGHT, recommended):** Add per-artefact `build_flags_regime`
column to P1-A / P1-B / P1-C / P1-D frontmatter. V2 aggregator refuses to
compute cross-artefact c/B delta where regimes mismatch (the consumer-side
discipline). P1-A frontmatter updates from "native target CPU per Cargo.toml"
to "RUSTFLAGS not set explicitly; native-CPU NOT pinned (Cargo.toml does
not propagate target-cpu)". P1-B retains explicit `RUSTFLAGS unset`
disclosure. P1-C/D retain `RUSTFLAGS="-C target-cpu=native"`. The 5.3 %
Mbps drift between P1-B twitter (11037) and P1-D twitter (11627) is then
attributable in the schema, not silently compared. **Cost: ≈8 min wall.
Closes CH4 lens without re-record.**

**Option B (HEAVY, not recommended for V2):** Unify RUSTFLAGS by re-running
P1-B (smaller scope than P1-A/C/D) with `RUSTFLAGS="-C target-cpu=native"`
to match P1-C/D. Requires re-recording all 56 P1-B flame profiles
(≈30+ min wall) plus extraction-script re-run. Delivers byte-identical
build-flag baseline across all four primary captures but at significant
cost; closes CH4 cohesion defect more decisively than Option A.

**Recommendation: Option A** (LIGHT) for V2; defer Option B to S-P2
design phase if cross-artefact c/B comparator becomes load-bearing for
S-P2 candidate selection.

**Convergence impact (Option A):** CH4 93.5 % → ≈100 % (CF-1 closes via
per-row regime disclosure; cross-artefact comparator refusal is the
consumer-side enforcement).

### §2.4 — F-V2-P1C-LINEDRIFT — P1-C NEON primitive line-anchor cleanup (LIGHT)

**Closes:** CH1 Finding 2 (3 NEON line off-by-ones); CH3 Finding F-1
(2 REDRESS.md path drift cites in p1c §500 + §590); CH6 §4 Finding 3
(path-layout standardisation, partial).

**Scope:** Re-grep `fn bulk_emit_positions_64_neon`,
`fn bitmap_prefix_xor_64_neon`, `fn eob_pad_clamp_neon` at HEAD; update
`p1c-samply-mode-3.md §2.2.4 + §2.3` to fn-signature lines (or annotate
"samply attributes to `#[inline(always)]` attribute line preceding fn
signature; fn body begins one line below"). Normalise
`restart/skinny/REDRESS.md` (non-existent) to `skinny/REDRESS.md` on
p1c lines 500 + 590.

**Cost:** ≈5 min wall (LOW). Mechanical edits.

**Convergence impact:** CH1 P1-C 84 % → ≈100 %; CH3 ACCEPT-WITH-NOTE → ACCEPT.

### §2.5 — F-V2-P1A-MOVEMASK — P1-A line-anchor annotations (LIGHT)

**Closes:** CH1 ACCEPT-WITH-NOTE (P1-A 92 % → ≈100 %).

**Scope:** Annotate `movemask_u8x16` and `match_tiny_plain_string_with_cap::<16>`
table cites to clarify "innermost-frame attribution within `fn` at line N"
(e.g. "line 22 inside `fn movemask_u8x16` at line 4" for the inner-loop
hot-bit-or; "line 169 (fn signature) + intra-body line 176" for the
inner-loop branch on the cap variant). Defensible per inline-fold
attribution, but the table should add the fn-anchor for hygiene.

**Cost:** ≈3 min wall (LOW). Annotation only; no re-grep needed.

**Convergence impact:** CH1 P1-A 92 % → ≈100 %.

### §2.6 — F-V2-P1ABC-RERECORD — parse-attribution rebuild + samply re-record (HEAVY)

**Origin:** CH6 §4 V2 MUST queue (item 1 + item 2 — schedule
`--features parse-attribution` re-record of P1-A/B/C top-1 hot-leaves;
re-record `github_events parse_only` Track 1 with longer iter count to
defeat 8-sample noise); CH2 Finding F1 (transitive feature form
`--features runtime/parse-attribution` for bench-harness propagation,
not `--features parse-attribution` directly); CH2 §4.1 item 1 (re-capture
P1-A + P1-B + P1-C); CH5 §3 Finding CH5-D (V2 unlock for cracking
dispatch envelope); CH3 V2 fold item 2.

**Scope:**

1. Rebuild `xtask`, `bbnf-bench`, `xctrace_probe`, `profile_direct` with
   `cargo build --release -p bbnf-bench --features runtime/parse-attribution`
   (transitive form per CH2 F1 — the feature lives at
   `skinny/crates/runtime/Cargo.toml:21` as a runtime-crate-private
   feature; bench-harness must propagate through the dep declaration).
   Verify the cfg_attr at `generated.rs:33-34, 43-44, 58-59, 79-80, 86-87,
   117-118, 138-139, 157-158` flips from `inline(always)` to
   `inline(never)` (8 sites).
2. Re-record P1-A samply (17 corpora × 1 plane = 17 profiles) under
   `--features runtime/parse-attribution`. Re-extract top-N tables;
   compare against V1 P1-A §2 table.
3. Re-record P1-B samply (17 direct + 11 typed = 28 corpora × 2 planes = 56
   profiles). Re-extract.
4. Re-record P1-C samply (4 mode-III probes). Re-extract.
5. Re-record `github_events parse_only` Track 1 with longer iter count
   (target ≥4000 samples) to crack the 8-sample inlined-std `<u16 as
   From<u8>>::from` noise envelope.
6. Append the `parse-attribution=on` top-N decomposition tables to
   P1-A §2.1, P1-B §2.1+§2.2, P1-C §2.1+§2.2 — converting CH6
   "named + routed" status to "named + routed + executed".
7. Promote `DirectParser::skip_value` symbol-path from
   `bbnf_bench::generated_real_typed` namespace to a grammar-neutral
   home (e.g. `bbnf-simd::offset_tape::skip_value` or
   `runtime::substrate::skip_value`) per CH2 §4.1 item 2 — note this
   is S-P2 design work and arguably a separate packet.

**Cost estimate (wall, single-host sequential):**
- P1-A re-record (17 profiles × ~150 s rec + extract): ≈45 min
- P1-B re-record (56 profiles × ~60 s rec + extract): ≈60 min
- P1-C re-record (4 probes × ~120 s rec + extract): ≈10 min
- github_events parse_only longer-iter re-record + extract: ≈5 min
- Top-N table re-extraction + artefact prose update: ≈15 min
- **Total sequential:** ≈135 min wall. **Parallel (3 captures concurrent
  on independent target dirs):** ≈60 min wall.

**Classification:** This is the substantive V2 work that delivers on the
CH6 V2 queue + closes CH2 F1 + addresses downstream CH1 cite drift on the
inner primitives. However, it is **wave-program work** that:
- exceeds the 30 min HARD CAP applied to V1 lens dispatches,
- requires a separate dispatch context (re-record loop driver, identity
  manifest, sym sidecar layout, top-N extraction script paths),
- delivers content that is functionally a new tranche of P1 axis files
  (parse-attribution=on companion to V1's parse-attribution=off captures),
- is **mandatory** for S-P2 primitive-design ground-truth (per CH2 §4.1)
  but **not blocking** for V2 mechanical convergence.

See §3.2 below for explicit orchestrator decision options.

## §3 — V2 + V3 convergence forecast

### §3.1 — V2 light-fold-only forecast

With the five light packets (F-V2-P1E-1 + F-V2-P1F-1 + F-V2-METHODOLOGY-1
Option A + F-V2-P1C-LINEDRIFT + F-V2-P1A-MOVEMASK; ≈26 min wall total):

| Lens | V1 rate | Expected V2 rate (light only) | Net |
|---|---:|---:|---|
| CH1 | 89.6 % | ≈100 % | P1-E typed-plane refresh + P1-C NEON cleanup + P1-A movemask annotation close all 12 sub-axis non-ACCEPT entries |
| CH2 | 100 % | 100 % | R1 + R2 fold naturally with F-V2-P1E-1 |
| CH3 | 100 % | 100 % | F-1 path normalisation in F-V2-P1C-LINEDRIFT clears ACCEPT-WITH-NOTE |
| CH4 | 93.5 % | ≈100 % | F-V2-METHODOLOGY-1 Option A closes CF-1 via per-row regime disclosure |
| CH5 | 83 % | 100 % | F-V2-P1F-1 reclassifies CH5-B orphan REVISE as contracted-deferral to C-2 |
| CH6 | 100 % | 100 % | V2 MUST queue items remain V2-EXTENSION-CANDIDATE; no impact on V2 lens rate |
| CH7 | 100 % | 100 % | No CH7-driven revision required |

**Expected sub-axis-weighted V2 aggregate:** ≈98.3 % (170 / 173 if F-V2-METHODOLOGY-1 lifts
2 of the 2 CH4 REVISE sub-axes; 173 sub-axes = original 175 − 2 sub-axes
that get reclassified rather than re-evaluated).

**Expected per-lens mean V2:** ≈99.7 % (95.16 % → 99.7 % via the four
lens-fold deltas).

**V2 outcome under light-only:** ≥95 % on both aggregation methods; zero
orphan REVISEs; **first ≥95 % cycle achieved**. V3 then has the burden
of producing the second consecutive ≥95 % cycle to discharge §3Z
"≥95 % × 2 cycles".

### §3.2 — Heavy F-V2-P1ABC-RERECORD: deferral decision

Two explicit options for orchestrator (per dispatch task footer):

**Option X (defer to S-P2 design phase) — RECOMMENDED:**
- V2 runs the five light packets only (≈26 min wall).
- V2 achieves ≥95 % mechanically; V3 then targets the second consecutive
  ≥95 % cycle to discharge §3Z.
- The parse-attribution=on re-record becomes the **entry artefact** for
  S-P2 primitive design (the unmasked primitive census is what S-P2 needs
  to ground its primitive proposal slate).
- Rationale: the parse-attribution=on captures deliver substantive
  primitive-decomposition data, not lens-correctness deltas. They are
  arguably outside the S-P1 lens-discipline scope (which audits the
  presented P1 artefacts) and inside the S-P2 design-input scope (which
  consumes ground-truth primitive measurements). Deferring respects the
  S-P1 → S-P2 contract boundary.
- CH6 V2 MUST queue stays "named + routed"; conversion to "named + routed
  + executed" happens in S-P2.
- Cost: S-P1 V2 completes in ≈26 min wall; S-P2 dispatch absorbs the
  ≈60-135 min re-record cost as its first wave.

**Option Y (run as S-P1 V2-extension cycle):**
- V2 light packets land first (≈26 min); V2 atomic commit lands the
  light-fold consolidated artefact.
- Separate "V2-extension" dispatch runs F-V2-P1ABC-RERECORD (≈60 min
  parallel / ≈135 min sequential). Outputs land as new P1 axis files
  (`p1a-samply-mode-1-attr.md`, `p1b-samply-mode-2-attr.md`,
  `p1c-samply-mode-3-attr.md`, or append-to-existing).
- V3 cycle then re-challenges the union of V1 + V2 + V2-extension
  artefacts under all 7 lenses. Sub-axis ACCEPT-rate expected to remain
  ≥95 % (the parse-attribution=on tables surface inner primitives that
  are presumptively grammar-neutral, satisfying CH2 on first contact).
- Rationale: CH6 V2 MUST queue converts to V2-DONE; S-P2 starts with
  the unmasked census already in hand; the S-P1 → S-P2 contract delivers
  more ground-truth at higher V cycle cost.
- Cost: S-P1 V2 + V2-extension completes in ≈90 min wall; S-P2 starts
  immediately.

**Recommendation:** **Option X (defer to S-P2).** The
parse-attribution=on re-record is a primitive-design deliverable, not a
profile-pass lens fix. Forcing it into S-P1 V2 conflates the
audit-correctness convergence axis (which is mechanical and bounded) with
the substantive primitive-census axis (which is open-ended and S-P2's
contract). The five light packets clear the orphan REVISEs and lift V2
above the 95 % floor; V3 can attempt the §3Z close on lens-correctness
grounds; S-P2 dispatch then consumes the parse-attribution=on captures as
its first wave deliverable. The orchestrator may overrule this in favour
of Option Y if S-P2 design needs the unmasked census to be already
attested before its dispatch context lands.

### §3.3 — V3 forecast

Under Option X with five V2 light packets:

- V3 inherits V2's ≈98 % sub-axis aggregate + 100 % per-lens ACCEPT on
  CH2/3/6/7 + ≈100 % on CH1/4/5 after light fold.
- V3 work surface: re-verify the V2 light-fold edits land cleanly; surface
  any new cite drift introduced by V2 textual edits; reconfirm 77/77
  flame profile paths still exist on disk; reconfirm PMU access matrix
  byte-identical to SK-V13 V3 lock-in.
- V3 expected outcome: ≥95 % on both aggregation methods with zero new
  orphan REVISEs. **§3Z convergence on the second consecutive ≥95 %
  cycle.** S-P2 dispatch gate opens per `PASS-1-PROFILE.md §6` +
  `ORCHESTRATOR.md §3Z`.

Under Option Y (V2 + V2-extension), V3 absorbs the parse-attribution=on
re-record output and re-challenges; V3 ACCEPT-rate forecast unchanged
(≈98-100 %) because the new captures answer existing dispatch-context
mandates rather than introduce new lens-axes.

## §4 — Sources

V1 lens dispositions (all verified existing at write-time):
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH1.md` (262 lines)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH2.md` (219 lines)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH3.md` (289 lines)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH4.md` (253 lines)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH5.md` (221 lines)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH6.md` (234 lines)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH7.md` (258 lines)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md` (53 lines)

V1 P1 axis artefacts under review (HEAD = 2547c750bc78533d738eb85913206a0872022818):
- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` (340 lines)
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` (320 lines)
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` (607 lines)
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` (648 lines)
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` (306 lines)
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md` (260 lines)

Binding authorities:
- `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH1-CH6 specialisations) + `§6` (S-P2 dispatch gate)
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH1-CH6 lens registry) + `§3Z` (convergence rule)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune lens definition)
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` + `HANDOFF.md` (SK-V14 contract; §2 telemetry binding incl. `comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`; §3 C-1..C-5 candidate slate)
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (S-P0 prune list; 74 findings; 3 architectural sequencing constraints; PRUNE-1..PRUNE-7)

Cross-tranche substrate anchors:
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md` (SK-V13 V3 PMU access matrix lock-in; cited byte-identical at SK-V14 P1-D §1.4)
- `skinny/REDRESS.md` (5041 lines; REDRESS-50..55 / 60..72 / 80 / 82..84 / 88..89 / 96..98 / 126 watch-list)

On-disk artefacts verified at V1 write-time (per CHALLENGE-CONTEXT §3
executable-verification mandate; CH6 §1 path-existence pass):
- `/tmp/skv14-p1/samply/profiles/` (17 .json.gz + 17 .syms.json; P1-A)
- `/tmp/skv14-p1/samply/inlined/` (17 atos-inline files; P1-A CH6 remediation)
- `/tmp/skv14-p1b/samply/profiles/` (56 .json.gz + 56 .syms.json; P1-B)
- `/tmp/skv14-p1c-profiles/` (4 probe .json.gz + 4 .syms.json; P1-C)
- `/tmp/skv14-p1d/pmu/pmu_rows.tsv` (34 rows + header), `direct_rows.tsv` (68), `typed_rows.tsv` (68 incl. 24 absent), `mode3_rows.tsv` (85) = 231 total rows
- `/tmp/skv14-p1d/xctrace/cpu-state.xml` (60 994 993 bytes; no PMC columns)
- `/tmp/skv14-p1d/artifacts/identity.txt` (sudo refusal verbatim; commit + host triple + xctrace + samply versions)
- `/tmp/skv14-p1/artifacts/identity.txt` (P1-A identity ledger)
