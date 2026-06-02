# Omega-B Skinny Lessons — Pass Omega V10 (SK-V18 GENERALIZATION cycle)

Date: 2026-06-01.
Worker: Pass Omega V10 Omega-B skinny-lessons.
Scope: SK-V1..SK-V18 master docs + `skinny/REDRESS.md` + `skinny/RESULTS.md`; the
SK-V18 S-P0 audit-overfit synthesis, S-P1 profile, S-P2 candidate lattice, the
12-wave S-P3 SPEC; the converged T-P1 1D + 1E inventories and the T-P3 3D
skinny-fold (the upstream consumption boundary).
Disposition: ACCEPT as proposal-only digest. No live V1 governance surface is
edited here; this digest is what the `ARCHITECTURE.md` implementation-status CRUD
consumes POST-G-Omega.
Consumes: `restart/audit/totality/p3/3D-skinny-fold.md` (the 12 proposed deltas
3D-D01..3D-D12); `restart/audit/totality/p1/1D-skinny-lessons.md` (the SK-V18
spec-claim↔impl table + the J/C/G findings + U-1..U-5 open questions).
HARD CAP: 20 min.

## Executive Lesson — The Inflection Point

SK-V18 is the INFLECTION cycle, and the durable lesson the V1 implementation-status
must absorb is its inversion of SK-V13. SK-V13 was a FAKE admit (a fabricated CSS
number). SK-V18 is the opposite: the >SOTA is now MEASUREMENT-VALID, but the
implementation that produced it is hand-written, FORKED, and REPLICATED under a
"grammar-driven" banner — **the one generator does not yet exist**
(`restart/audit/totality/p1/1D-skinny-lessons.md:29`-`38`,
`restart/skinny/tranches/sk-v18/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:36`-`39`).

Two facts must be carried together, never collapsed:

1. **The >SOTA is real (honest, measured).** JSON 51/51 strict cold beats sonic-rs
   strict, same-plane, per-iter equality, bench-row-backed
   (`skinny/RESULTS.md:5`-`55`; e.g. twitter parse_only Track 1 8349.290 > sonic
   4913.095; canada parse_only 16709.901 > 12970.929). CSS canonical cold beats
   lightningcss 1.66–3.38× but is DIRECTIONAL, NOT re-locked (loadavg 4.35; the H1
   `css_canon_bench` quiet re-capture is the re-lock gate)
   (`restart/audit/totality/p1/1D-skinny-lessons.md:67`,`:187`).

2. **The emission story is DISPROVED.** The generator forks: two couriers
   (`skinny/crates/codegen/src/runtime_generator.rs:16` matches
   `RuntimeEmitterKind`, `:91` is the `normalize(CSS_GENERATED_RS)` const courier —
   verified live at HEAD); seven byte-identical css_l4 replicas (md5
   `b654562ccff46ed62dd48e9ace325830`, re-verified across all 7 modules this pass);
   JSON renders 7× `push_str` fixed literals; `RuntimeEmitterKind` forks on grammar
   FAMILY; the phantom `<G>` is test-only; x86 lingers crate-wide (24 files live);
   the Lock-14 gate is green-by-exclusion
   (`restart/audit/totality/p1/1D-skinny-lessons.md:61`-`72`).

The SK-V18 finding the V1 spec MUST reflect — and the single largest paper-close
surface — is the named-primitive escape: **a naive grammar-walk regresses to
lightningcss's own 94.1% `find_component_delim` scan architecture**, so >SOTA
survives ONLY via named, `.bbnf`-invoked, grammar-derived, §6-(a)-(d)-gated
primitives, never a paper-close
(`restart/audit/totality/p1/1D-skinny-lessons.md:46`-`49`,`:207`).

The implementation-status must absorb this as STATUS, not closure: the round-trip
(`delete + regen ⇒ byte-equivalent`) passes, but OVER hand-written content. The
generalization is the SK-V18 wave program's burden, future-tense, gated at
G-Omega.

## Evidence Classification (the implementation-status truth table)

| Evidence class | Status for V1 | Why it matters |
|---|---|---|
| JSON parse_only / direct_to_struct / real_typed_struct | Honest >SOTA guard. 51/51 strict cold > sonic-rs strict, same-plane, per-iter equality, bench-row-backed (`skinny/RESULTS.md:5`-`55`; `1D:115`-`118` D-9). | V1 may cite JSON as the load-bearing same-plane >SOTA proof; must NOT generalize to CSS, Sheets, or fleet closure. The burden is preserving it THROUGH the generator (G1 91.5% leaf). |
| CSS canonical cold vs lightningcss | MEASUREMENT-VALID but DIRECTIONAL, NOT re-locked. 1.66–3.38× ran under loadavg 4.35; the `css_canon_bench` corpus-in-timer cold harness is honest, but the absolute ratio is not a re-locked bench row (`1D:67`,`:187` J-3). | V1 carries CSS as DIRECTIONAL pending the H1 quiet re-lock; do NOT carry an un-caveated "MEASUREMENT-VALID closure" word on the CSS half. This is NOT a fake admit (the residual is hand-written content, not a fabricated number — the SK-V13 inversion). |
| The one generator | DISPROVED — does not exist. Two forked couriers + 7 byte-identical css_l4 replicas; `RuntimeEmitterKind` forks on grammar family (`1D:61`-`63` D-1/D-2; G-6). Verified live at HEAD. | V1 implementation-status must state "forked-courier emission until G1+G2+G3 prove a grammar-DERIVED body"; the round-trip passes over hand-written content. |
| 5-shape `BackendShape` + decision spine | LOAD-BEARING (impl_exceeds_spec at admitted scope). SK-V15 W7/W8/W9 admitted all five lowerers as operation-plan renderers + e-graph rewrite count + falsifiable CSP (`1D:64`,`:119`-`125` D-10/G-3). | V1 cites the 5-shape canon + spine as load-bearing AT GREATER fidelity than the spec asserts — this is what G3 dispatches ON. CAVEAT: selection DEPTH under the Sheets precedence tower is the open L10 stressor until G3 exists (`1E-locks-evidence.md:90`). Preserve EXACTLY five variants; no sixth shape. |
| Unified `Tape`/`ValueRef`/`PayloadArena` substrate | CLEAN foundation (impl_exceeds_spec). One tape owns source/offsets/sparse-flags/payload-arena; `ValueRef` is a cursor into it; no second substrate, no retained sidecar (`1D:73`,`:126`-`128` D-11/G-1). | V1 may cite the single-tape/`ValueRef`-cursor substrate as Lock-1-authoritative at greater fidelity than the spec asserts — it is what S-P0 explicitly KEEPs. |
| Lock-14 gate | Green-by-EXCLUSION. The leak surfaces (`runtime_generator.rs`, JSON sink/typed/template) sit in the WEAK `SKV15_W2_EXTRA_COVERAGE_ROOTS` + a live `diagnostic-x86` exclusion, not strict `GENERIC_SCAN_ROOTS` (`1D:69`,`:108`-`111` D-7/G-7). | V1 lock prose must require strict-root inclusion + `FORBIDDEN_GENERIC_TOKENS ⊇ {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` (the `*` glob is alias-immune) + drop `diagnostic-x86`; the gate lands BEFORE G2/G3 (P4). |
| FNV / closed-enum products | SPLIT (G-5). (a) bench-side FNV quarantine is genuinely bench-only and KEPT; (b) production `emit_full_parse input_fnv64` is LIVE non-equality / non-substrate / non-document-identity telemetry, honest, NOT bench-quarantined (`1D:201` G-5; production path `…/css_l4_declaration_values/generated.rs:393`). | V1 must word FNV as "never an equality arbiter / never substrate / never document-identity", NOT "bench-only" — so the honest telemetry use is not mis-stated and cannot leak into a correctness proof. |
| x86 / SIMD primitives | aarch64-only PROVED admission policy; x86 (24 files `src/x86_64/` + `ext/x86/` vendored ASM) lingers crate-wide and is the P1 PRUNE (≈−4500 LOC). NEON `find_component_delim` is scalar / dead at admission, the G6 retarget target (`1D:68`,`:71` D-4/D-6; G-2). | V1 carries aarch64-only as PROVED policy; x86/AVX-512/SVE2 are diagnostic-only and cannot close an M5 Max row. A NEON admission claim needs the caller census ∧ `simd_admission_profile_sampled` conjuncts. |
| Phantom `<G:EventGrammar>` | Decorative — zero non-test production consumers; witnesses defined, never animated. DELETE (preserve the REAL `K=Kind` axis) (`1D:66`,`:204` D-5/G-8). | V1 carries the `<G>` axis as a G4 DELETE; the `K` axis is preserved. |
| Metalang leak `parse_w11_1_number` ×7 | Bench-wave id contaminates shipped JSON runtime symbols (`…/grammars/json/generated.rs:801,841,881`). Rename-at-source is the P5 PRUNE (loc_delta ≈0) (`1D:72`,`:112`-`113` D-8/C-4). | V1 Lock-6 generated-cleanliness requires `regen --check` exit 0 with no `w[0-9]+`/corpus/`sk_v` tag in shipped runtime. |

## Longitudinal Trajectory Table (SK-V1..SK-V18)

The per-iteration load-bearing win + load-bearing rejection + the trajectory
correction the V1 implementation-status absorbs. The trajectory is one arc: a
honest substrate floor that holds grammar-neutral, a CSS measurement that swung
from fake (SK-V13) to measurement-valid (SK-V18), and an emission story that is
only now revealed as forked — the inflection.

| Cycle | Load-bearing win | Load-bearing rejection | Trajectory correction for V1 |
|---|---|---|---|
| SK-V5..V9 substrate floor | Single `Tape`/`ValueRef`/`PayloadArena` substrate established; scalar→checkasm→same-wave-consumer SIMD discipline; aarch64-only admission. | Parser-local event/structural cursors (`JsonEventCursor` item 51, `JsonStructuralCursor` item 53) REJECTED as second substrates (`skinny/REDRESS.md:742`-`768`,`:784`-`813`). | V1 substrate clauses are the durable floor; the rejected-cursor items pre-block the SK-V18 G4/G6 second-scanner routes. |
| SK-V10..V12 CSS first contact | First CSS PASS-ADMIT rows; the warm/cached-bench dishonesty surfaced and condemned (no-warm-benches). | Warm micro-fixture CSS throughput is non-admitting; only cold per-parse counts. | V1 admission language requires cold, corpus-in-timer benches; warm/cached benches reject. |
| SK-V13 fake admit | (none — the cycle is a cautionary anchor) | CSS L4 admit was a FABRICATED number (broadcast / wrong-plane comparator). | V1 must keep the SK-V13 lesson visible: a number without a same-plane cold bench is a fake admit. SK-V18 is its inversion. |
| SK-V14 row-ledger close | JSON closes 51/51 strict same-plane guard (the durable baseline carried into SK-V18). | CSS 24/24 broadcast close was audit-falsified (one tuple projected into 24 rows). | V1 splits JSON "validated guard" from CSS "diagnostic/directional". |
| SK-V15 prune-then-rebuild contract | The receiver set A-G correctly bracketed the repair; Decision Engine moved from scaffold to LOAD-BEARING via W7/W8/W9 operation-plan renderers + e-graph rewrite count (`skinny/REDRESS.md:6326`-`6414`). | The W11L/N/O FNV closed-enum products are bench-only quarantine; production FNV arbiter blocked. | V1 carries the Decision spine as load-bearing and FNV as the split telemetry/quarantine surface (G-5). |
| SK-V16 / SK-V17 CSS typed | The unified Tape/Layout/Projection substrate LANDED; CSS typed > lightningcss approach matured; the canonical `css_canon_bench` cold harness emerged. | The CSS substrate landed BUT UNWIRED — zero CSS parse-path callers at SK-V17 (the latent-substrate residual). REDRESS coverage for SK-V16/V17 is NOT yet on the committed `skinny/REDRESS.md` ledger (ends at SK-V15 W11, `:6446`) — U-5. | V1 must record SK-V16/V17 as the CSS-typed lead-in to the inflection; the SK-V16/V17 REDRESS reconcile is a Pass-Omega-V10 / pre-W-PRUNE blocker, not an SK-V19 obligation (3D CH3-V1-R2). |
| SK-V18 INFLECTION | The substrate floor + JSON >SOTA + CSS measurement-valid are real; the §6 named-primitive discipline is literature-validated by T-P2; net ≈−10800 LOC PRUNE-before-rebuild plan certified (12 waves). | The one generator does NOT exist (forked couriers + 7 replicas + grammar-family fork + green-by-exclusion gate + x86 + phantom `<G>` + metalang leak); a naive grammar-walk regresses to lightningcss. | V1 implementation-status absorbs the inflection thesis (3D-D11): >SOTA rides hand-written content; survives ONLY via §6-(a)-(d)-gated named primitives; the generalization is the wave-program burden, gated at G-Omega. |

## SK-V18 Focal Findings (the implementation-status load-bearing set)

### The inflection-point finding (3D-D11)

The central SK-V18 lesson: one generator does NOT yet exist — disk shows two
forked couriers (`runtime_generator.rs:16,91`), JSON 7× `push_str`, 7 byte-identical
css_l4 `generated.rs` (md5 `b654562c…`), `RuntimeEmitterKind` forks on grammar
family. The round-trip (`delete + regen ⇒ byte-equivalent`) PASSES, but over
hand-written content — so a courier-swap or replica-relabel would ALSO pass and
must be REJECTED. >SOTA is real but rides hand-written bodies; a naive
grammar-walk that inflates the flat 94.1% scan into a combinator descent regresses
to lightningcss's own architecture (`restart/audit/totality/p1/1D-skinny-lessons.md:82`-`86`,
`:202`,`:205`,`:207`; T-P2 candidate R-B/R-C tree-walk REJECTED).
**V1-fold:** the implementation-status carries this as the durable inflection
finding; the named-primitive escape is the single largest paper-close surface.

### The one-generator architecture (the SK-V18 goal, not yet landed)

ONE grammar-driven generator in `skinny/crates/codegen/` consuming three `.bbnf`
roots (JSON, CSS, Sheets) and emitting three NON-IDENTICAL grammar-DERIVED parsers;
the `RuntimeEmitterKind` fork is DELETED; `render(program)` reads its output-shape
ONLY from `program.policy_summary.backend_shape` (the grammar-neutral 5-shape
`BackendShape{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`), never from
a grammar tag (SPEC close conditions 1-3,
`restart/skinny/tranches/sk-v18/SPEC.md:63`-`85`). The relocated-seam risk
(un-forking the visible enum while leaving a per-grammar branch in a neutral data
table) is caught ONLY by the R16 structural row-collapse co-gate — the +1-line
`RuntimeTarget: PartialEq` full-row derive recursing into both `frontend_requirements`
(#11) and `output_labels` (#12) — never by arm-grep (3D-D12; `1D:94`-`98`,`:208`
G-12). The 3-co-gate is a CONJUNCTION, not an md5 check: {md5-distinct ∧
`branch_count==0` ∧ `type_count==0` ∧ `rows_collapsed==true`}.
**V1-fold:** implementation-status states the one-generator architecture as the
SK-V18 GOAL (forked-courier today, un-forked at G3), and the R16 co-gate as the
only catcher of the relocated seam.

### The §6 named-primitive (a)-(d) discipline (T-P2 literature-validated)

The 94.1% CSS hot leaf (`find_component_delim` + `consume_balanced_at`) has a
delimiter alphabet (`{}:;`) and structural-byte dispatch (`' " / ( [ {`) EMERGENT
from rule shapes; it survives the AST-walk ONLY as a named primitive
(`css_balanced_component_scan`) admissible under the four-conjunct machine-checked
gate: **(a)** grammar-INVOKED-by-name ∧ **(b)** output VARIES under invoking-rule
mutation ∧ **(c)** `verbatim_blob_present == false` ∧ **(d)** PROFILE-PROVEN-NARROW-LEAF
(`restart/audit/totality/p1/1D-skinny-lessons.md:132`-`139`,`:207` G-11;
3D-D11). NEUTRALITY obligation: a non-CSS invoker (JSON `{}/[]` or Sheets
`paren_expr`) must exercise the SAME shell, else the primitive is FORCE-DEMOTED to a
`css_`-scoped name (the FORCED-honest CSS-scoped rename, not a fabricated
cross-grammar caller — T-P2 `2C:215`; SPEC C4 fold). The JSON string/number leaf
micro-opts (`b'-'|b'0'..=b'9'` fast-path, `match_tiny_plain_string_direct`) are the
secondary §6 set, each with the (b) byte-set-mutation falsifier.
**V1-fold:** implementation-status carries the (a)-(d) gate as the machine-checked
escape that keeps the largest paper-close surface honest; a tree-walk that
regresses the flat scan REJECTS before close.

### The G6=WIRE profile (find_component_delim 94.1%)

S-P1 profiled `find_component_delim` at 79.5% of the CSS path / 94.1% of the
scalar scan, and `parse_object_value_at_direct` + `parse_array_element_at_direct`
at 91.5% of the JSON path. The G6=WIRE decision RULE is the grammar-NEUTRAL lesson:
a hot leaf with a measured profile share ABOVE the WIRE threshold warrants a NEON
retarget over an honest-retire (`restart/audit/totality/p1/1D-skinny-lessons.md:206`
G-10). CRITICAL distinction: the supporting 94.1%/79.5% figures are CSS-EMPIRICAL,
not fleet-neutral — `find_component_delim` has ZERO non-CSS caller on disk
(re-verified this pass: `rg find_component_delim …/grammars | grep -v css` is
empty). And it is RETARGET, NOT wire-as-is — the dead `find_css_significant`
kernel was written FLAT (and is `#[cfg(test)]`-only) but the hot leaf RECURSES, so
G6 RETARGETS NEON onto the live recursive shell; T-P2 explicitly REFUTES "wire
as-is" (`1D:71` D-6; T-P2 `2E:80`). Admission is proven by BOTH conjuncts: (i) the
non-`#[cfg(test)]` `generated.rs` caller census ∧ (ii) `simd_admission_profile_sampled`
(non-zero self-time in the `css_canon_bench` sample) — a census-only proof is
`dead`, not `admission` (3D-D08; SPEC condition 10).
**V1-fold:** implementation-status keeps the G6=WIRE decision-rule as the neutral
lesson and the 94.1% ratio as a CSS profile measurement (not a fleet constant), and
records the RETARGET-not-wire-as-is correction.

### The prune findings (PRUNE-before-rebuild, ≈−10800 LOC)

SK-V18 is PRUNE-before-REBUILD: PRUNE lands FIRST to reduce the surface for
GENERALIZE and make the Lock-14 gate trustworthy BEFORE the emitter rebuild. The
five prunes (net ≈−10800 LOC; deletes far more than the campaign adds):

| Prune | Target | LOC delta | Falsifier |
|---|---|---:|---|
| P1 | DELETE the whole x86 surface crate-wide (`src/x86_64/` 24 files + `ext/x86/` vendored ASM + nasm `build.rs`) | ≈−4500 | `find …/src/x86_64 …/ext/x86 -type f == 0` ∧ crate-wide aarch64-neutral grep ∧ `cargo build`+`cargo test --no-run` clean |
| P2 | DELETE the warm micro-fixture CSS bench (`nonjson_css_l4.rs:3091 measure_mbps`) | ≈−700 | the broadcast is already pruned; `css_canon_bench` (cold, N≥50, no broadcast) is the survivor |
| P3 | Collapse the 7 byte-identical css_l4 replicas to ONE CSS config + R16 row-collapse | ≈−5500 (= −5460 replica bodies + ~−40 collapsed rows + 1 `PartialEq` derive, per `SPEC.md:435`) | `runtime_target_rows_collapsed == true` ∧ the 3-co-gate conjunction |
| P4 | Fix the Lock-14 green-by-exclusion gate (promote leak roots into strict `GENERIC_SCAN_ROOTS`; lands BEFORE G2/G3) | ≈+15 | the re-inject-a-`SHEETS_GENERATED_RS`-token falsifier turns the gate RED |
| P5 | Purge the metalang leak (`parse_w11_1_number` → `parse_number`) | ≈0 (rename) | no `w[0-9]+`/corpus/`sk_v` tag in shipped runtime; `regen --check` exit 0 |

The component sum (P1+P2+P3+P4+P5) totals the SPEC-authoritative per-wave −10685
under the ≈ tilde to the −10800 headline (`restart/audit/totality/p3/3D-skinny-fold.md:157`;
`1D:209` G-13). A `P` wave carries ZERO generalization risk and deletes no
>SOTA-bearing code.
**V1-fold:** implementation-status records the PRUNE-before-rebuild order as a
standing constraint and the ≈−10800 net as the campaign signature (it deletes far
more than it adds).

## The Rejected-Route Pre-Block (the regression fence)

Three highest-regression SK-V18 moves (G2 `css_balanced_component_scan`, G4 lazy
`Cursor`/`CssNode`, G6 NEON retarget) each ADJOIN a route already REJECTED in
`skinny/REDRESS.md`. The admissible/rejected distinction the V1 spec must encode:

| REDRESS item | line | rejected shape | bounds wave | admissible vs rejected |
|---|---|---|---|---|
| 246 — W11T parse-only structural stream | `skinny/REDRESS.md:6184`-`6219` | a structural-stream parse_only DRIVER (second substrate) | G4 | ADMISSIBLE: `Cursor` is a VIEW over the EXISTING tape. REJECTED: a structural-stream driver = a second substrate. |
| 247 — W11V parse-only string64 mask | `skinny/REDRESS.md:6230`-`6260` | a bespoke per-grammar 64-byte mask | G2 | ADMISSIBLE: a SHARED grammar-neutral primitive over a grammar-DERIVED byte set. REJECTED: a bespoke per-grammar mask re-emitted per grammar. |
| 51 — SK-V5 event-cursor | `skinny/REDRESS.md:742`-`768` | parser-local transient event-cursor | G6 | ADMISSIBLE: EventTape lowering consumes the single substrate's event stream IN-LOOP. REJECTED: a retained/parser-local second cursor. |
| 53 — SK-V5 structural-mask cursor | `skinny/REDRESS.md:784`-`813` | second retained-parser cursor over a per-64-byte mask | G6 | ADMISSIBLE: RETARGET NEON onto the EXISTING in-loop scalar shell. REJECTED: a parser-local second scanner over a retained mask. |

COMPLETENESS CAVEAT (U-5): coverage is scoped to the committed ledger (ends at
SK-V15 W11, `skinny/REDRESS.md:6446`); SK-V16/V17 rejected routes are NOT yet
captured. The SK-V16/V17 reconcile is a Pass-Omega-V10 / pre-W-PRUNE blocker — G2/G4/G6
entry is BLOCKED until it lands on the committed ledger (3D-D08 / 3D CH3-V1-R2).
**V1-fold:** implementation-status carries the four-item pre-block at path:line and
the SK-V16/V17 reconcile as a pre-W-PRUNE blocker.

## Consumed T-P3 3D Skinny-Fold (the monotonic-fold register)

This digest CONSUMES the 3D skinny-fold verbatim in direction: skinny WIN → V1
implementation-status proposal input; skinny REJECTION → locks-strengthening
evidence; totality NEVER dictates live skinny
(`restart/audit/totality/p3/3D-skinny-fold.md:55`-`56`). The 12 proposed deltas
3D-D01..3D-D12 are the receiver map; this ΩB digest is the
implementation-status-facing distillation of the WIN-side (substrate, JSON >SOTA,
CSS directional, 5-shape spine, §6 discipline, G6=WIRE rule, prune order) and the
REJECTION-side (forked generator, green-by-exclusion gate, x86, phantom `<G>`,
metalang leak, the rejected-cursor pre-block). The two NEW deltas 3D-D11
(one-generator inflection thesis) and 3D-D12 (R16 relocated-seam co-gate) are the
SK-V18-specific additions the implementation-status absorbs as STATUS.

## The ARCHITECTURE §implementation-status CRUD Consumes

The implementation-status update the CRUD applies POST-G-Omega (proposal-only; no
live edit here):

| Status line | Required wording | Evidence driver |
|---|---|---|
| Generator | "Forked-courier emission (two couriers + 7 byte-identical css_l4 replicas + `RuntimeEmitterKind` grammar-family fork); the ONE grammar-driven generator does NOT yet exist — it is the SK-V18 G1+G2+G3 goal." | `1D:61`-`63` D-1/D-2/D-3; live HEAD verification |
| JSON >SOTA | "JSON 51/51 strict cold beats sonic-rs strict, same-plane, per-iter equality — the load-bearing >SOTA guard; preserved THROUGH the generator at G1." | `skinny/RESULTS.md:5`-`55`; `1D:115`-`118` D-9 |
| CSS >SOTA | "CSS canonical cold beats lightningcss 1.66–3.38× MEASUREMENT-VALID but DIRECTIONAL, NOT re-locked (loadavg 4.35; H1 `css_canon_bench` quiet re-lock gate). NOT a fake admit." | `1D:67`,`:187` J-3 |
| Substrate | "Single `Tape`/`ValueRef`/`PayloadArena` substrate is CLEAN (Lock-1 authoritative at greater fidelity than the spec asserts); no second substrate, no retained sidecar." | `1D:73`,`:126`-`128` D-11/G-1 |
| Decision Engine | "5-shape `BackendShape` + decision spine LOAD-BEARING (operation-plan renderers + e-graph rewrite count + falsifiable CSP); selection DEPTH under the Sheets tower is the open L10 stressor. Exactly five variants." | `1D:64`,`:119`-`125` D-10/G-3 |
| §6 named primitives | "The named-primitive escape is the single largest paper-close surface; admissible ONLY under the (a)-(d) machine-checked gate; a tree-walk that regresses the 94.1% flat scan REJECTS." | `1D:132`-`139`,`:207` G-11 |
| G6 / SIMD | "G6=WIRE decision-rule (neutral); the 94.1% `find_component_delim` is a CSS profile measurement (CSS-only leaf); RETARGET onto the recursive shell, NOT wire-as-is; admission needs caller-census ∧ profile-sampled conjuncts." | `1D:206`,`:71` G-10/D-6 |
| Host / x86 | "aarch64-only PROVED admission; x86 (24 files + vendored ASM) lingers crate-wide, the P1 PRUNE (≈−4500 LOC)." | `1D:68` D-4/G-2; live HEAD |
| Pattern-H (SK-V19 carry) | "The totality `crates/core/` 67 hand-written files / 6867 LOC at 9× scale is the SK-V19-ENTRY census (line-1 `@generated` provenance + md5-distinctness), NOT a SK-V18 wave." | `1D:213`-`225` U-1 |
| Wave program | "SK-V18 is PRUNE-before-REBUILD: P1-P5 PRUNE → G1-G6 GENERALIZE → PROVE (Sheets negative control) → H1 HONESTY; net ≈−10800 LOC; G-Omega is the next mandatory gate before W-PRUNE dispatch." | `SPEC.md:16`-`17`,`:45`-`51`; `1D:209` G-13 |

## Pass Omega V10 Carry-Forward

| Consumer | Carry-forward obligation |
|---|---|
| Omega-A (coherence) | Verify any V1 prose implying the one generator EXISTS, or carrying an un-caveated "CSS MEASUREMENT-VALID" closure, is changed to forked-courier-until-G3 / CSS-DIRECTIONAL-until-H1; verify the 5-shape canon stays coherent. |
| Omega-C (locks) | This digest's REJECTION-side (forked generator, green-by-exclusion gate, x86, phantom `<G>`, metalang leak, rejected-cursor pre-block) is the locks-strengthening evidence the 3C-locks-v+1-diff already disposes (9A/11M/0R/1D); the §6 (a)-(d) gate + the R16 relocated-seam co-gate are the load-bearing accepted clauses. |
| Omega-D (master-plan) | The trajectory table feeds the SK-V14..V18 landed/refuted reconciliation; the PRUNE-before-rebuild order + the ≈−10800 net are the wave signature. |
| Omega-E (skinny-corpus) | Sync corpus pages to the SK-V18 anchors: the certified SPEC, the one-generator GOAL (forked today), the §6 discipline, the `css_canon_bench` cold harness + `track1_rich` bit-rot fix, the G6=WIRE finding. |
| Omega-F (migration/handoff) | Carry the prune list as the migration delta (x86 crate-wide delete, CSS courier retirement, 7-replica collapse, phantom `<G>` delete, metalang rename); HANDOFF stops at G-Omega before W-PRUNE dispatch. |

This digest intentionally does not edit live surfaces. It records the SK-V18
skinny lessons the ARCHITECTURE implementation-status CRUD folds POST-G-Omega,
after G-Omega authorization.
