# Omega-E Skinny Corpus Alignment — Pass Omega V10 / SK-V18

Date: 2026-06-01.
Cycle: Pass Omega V10 (astral synthesis above the converged T-P1/T-P2/T-P3 totality passes).
Scope: `restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md`.
Disposition: **STAGED — ACCEPT-WITH-CRUD-5-UPDATES.** This artefact produces a STAGED diff only;
it edits NO live skinny surface. The CRUD-5 merge executes POST-G-Omega, after user sign-off.

SK-V18 anchors consumed (every claim grounded at path:line):

- `restart/skinny/tranches/sk-v18/SPEC.md` — the certified S-P3 12-wave manifest
  (`W-PRUNE/P1..P5 → G1..G6 → PROVE → H1`), ONE grammar-driven generator (`generator_grammar_count == 3`,
  SPEC:66), JSON+CSS+Sheets, net LOC ≈ **−10800** (SPEC:22,61), aarch64-only / x86 DELETED (SPEC:42,130),
  the §6 named-primitive (a)-(d) gate (SPEC:358-394), G6=WIRE (SPEC:28), the `track1_rich` CSS >SOTA
  plane beaten 1.66-3.38× vs lightningcss (SPEC:20,181), the 5-shape `BackendShape` canon preserved
  (SPEC:75), Lock 1 one-substrate preserved (SPEC:397).
- `restart/audit/totality/p3/3C-locks-v+1-diff.md` — 16-lock count preserved by addition (LOCKS.md:71
  "sixteen locks"); `git apply --check` exit 0; no renumber, 5 BackendShape variants intact.
- The bench bit-rot fix `784ceb418` — `track1_fact_stream → track1_rich` in `css_canon_bench`; the
  fact-stream `String` was deleted in W1 PRUNE, so the canonical harness did not compile at HEAD; the
  rich projection beats lightningcss on all 4 corpora (1.66-3.38×).

## Verdict

The live skinny corpus is anchored on a TWO-STRATUM stale receiver stack that the certified SK-V18
generalization supersedes:

1. **Pass Omega V9 / SK-V15 stratum** (the `Pass Omega V9 / SK-V15` blocks in all six surfaces) — the
   PRUNE-then-REBUILD `W0-W11` contract, CSS L4 demotion, fact-stream retirement, Decision-Engine /
   FNV / Pattern-H prune narrative. This stratum is now HISTORICAL: SK-V18's certified plan REPLACES
   the `W0-W11` envelope with `W-PRUNE → G1..G6 → PROVE → H1`, and the wholesale "CSS is contrived /
   demoted / unproven" posture is RESOLVED — CSS now BEATS lightningcss honestly (1.66-3.38×) via
   `track1_rich`, the one-generator architecture is CERTIFIED, and the §6 (a)-(d) named-primitive
   discipline is literature-validated by T-P2.

2. **Pass Omega V5 / SK-V17 stratum** (the `Pass Omega V5 / SK-V17` fold blocks) — the tape /
   `ValueRef<G>` fold, with CSS `>SOTA` marked `UNMEASURED-PENDING — an SK-V18 proof obligation`. This
   stratum's PREDICTIONS are now DISCHARGED: SK-V18 measure-proves CSS >SOTA, lands the ONE
   `BackendRule`-walking projection generator across JSON+CSS+Sheets, and wires the shared NEON
   classifier (G5/G6, G6=WIRE). The "SK-V18-pending" hedge must flip to "SK-V18-certified-plan".

The single most load-bearing terminology drift is the **CSS comparator inversion** (BENCH.md):
the corpus says `lightningcss` is a "diagnostic planning signal, not an admission floor" and elevates
`cssparser` as the "near-term same-workload speed comparator." SK-V18 INVERTS both roles — `cssparser`
is the 9-field EXACT CORRECTNESS oracle (gate-before-speed, structurally distinct from `track1_rich`,
SPEC:183), and `lightningcss` IS the binding CSS >SOTA SPEED bar (`track1_rich/lightningcss > 1.0×`,
SPEC:181). This inversion must be corrected in BENCH.md at lines 32-34, 73, 341-343, 1663-1668, 2268.

Zero surface carries the CERTIFIED-generalization SK-V18 anchor: `grep -c
"W-PRUNE\|G6=WIRE\|track1_rich"` returns 0 across all six docs (verified
2026-06-01). All six carry only the STALE future-adopter SK-V18 framing
("SK-V18 adopts the PROVEN skinny … into crates/core"; INDEX=3, WORKSPACE=2,
HARDENING=1, COMPILER=3, BENCH=4, SUBSTRATE=1) — exactly what CRUD-5 overwrites.
The corpus has ZERO `track1_rich` references despite that being the canonical CSS
workload after bit-rot fix `784ceb418`.

## Current Authority

Read order for CRUD-5 (POST-G-Omega):

1. `restart/skinny/tranches/sk-v18/SPEC.md` (the certified 12-wave manifest).
2. `restart/skinny/tranches/sk-v18/SYNTHESIS.md`.
3. `restart/skinny/tranches/sk-v18/HANDOFF.md`.
4. `restart/skinny/tranches/sk-v18/research/p1/SYNTHESIS-PROFILE.md` (hot leaves, G6=WIRE).
5. `restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md` (R-A..R-F, the §6 (a)-(d) gate).
6. `restart/skinny/tranches/sk-v18/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (PRUNE-list P1..P5, CH7).
7. `restart/audit/totality/p1/hardening/HARDENING-T-P1-V*-CONSOLIDATED.md` (converged).
8. `restart/audit/totality/p2/hardening/HARDENING-T-P2-V*-CONSOLIDATED.md` (converged).
9. `restart/audit/totality/p3/3C-locks-v+1-diff.md` (16-lock amendment-by-addition; `git apply` clean).

Governance note to preserve: SK-V18 S-P0..S-P3 are CERTIFIED; T-P1/T-P2/T-P3 totality are CONVERGED.
G-Omega is the next mandatory user gate. W-PRUNE (P1-P5) is the ONLY dispatch-now-eligible cluster on
S-P3 close; every G1..G6/PROVE/H1 wave is blocked until its predecessor closes its exit gate AND its
entry-gate predicate is GREEN AND the orchestrator/user dispatches the wave triumvirate (SPEC:48-49).

## Surface Disposition

| Surface | CRUD-5 op | Reason |
|---|---|---|
| `restart/skinny/INDEX.md` | Update | The `Pass Omega V9 / SK-V15` authority block (lines 5-36) and the `Pass Omega V5 / SK-V17` fold block (lines 38+) name SK-V15 W0-W11 as the active contract and CSS as audit-demoted; both are superseded by the certified SK-V18 `W-PRUNE→G1..G6→PROVE→H1`. |
| `restart/skinny/WORKSPACE.md` | Update | The SK-V15 W0-W11 workspace receiver (lines 29-60) and the SK-V17 fold receiver (lines 62-78) bind the active workspace to the wrong wave envelope and hedge CSS `>SOTA` as pending. |
| `restart/skinny/HARDENING.md` | Update | The SK-V15 receiver (lines 7-23) and the SK-V17 honesty-firewall (lines 25-41) read CSS `>SOTA` as UNMEASURED and refuse on W6-era criteria; the W2-W10 seven-lens trigger (lines 43-53) must re-key to the SK-V18 wave names + the §6 (a)-(d) lens. |
| `restart/skinny/COMPILER.md` | Update | The SK-V15 compiler receiver (lines 41-68) dispatches W3/W5/W7/W8/W9 and treats Decision Engine as scaffold; the SK-V17 receiver (lines 70+) reads the one-generator as a fold prediction. SK-V18 certifies the one un-forked emitter (G3, DELETE `RuntimeEmitterKind`) + the §6 primitives (G1/G2). |
| `restart/skinny/BENCH.md` | Update | The CSS comparator INVERSION (lines 32-34, 73, 341-343, 1663-1668, 2268): `lightningcss` wrongly demoted to diagnostic; `cssparser` wrongly elevated as speed comparator. Plus the SK-V15 schema (lines 39-55) and the SK-V17 `UNMEASURED-PENDING` block (lines 57-73). Must adopt `track1_rich` + the lightningcss >SOTA bar + the bit-rot fix. |
| `restart/skinny/SUBSTRATE.md` | Limited update | Substrate mechanics survive intact (Lock 1 union, 5-shape canon, no sidecar). Only the authority/status blocks (lines 33-37 SK-V15, lines 63-74 SK-V17) need the SK-V18 anchor; the SK-V17 fold predictions flip from pending to certified. NO substrate data-structure change. |

## CRUD-5 Update Plan By Surface

### INDEX (`restart/skinny/INDEX.md`)

Operation: Update the opening authority block (lines 5-36) and the SK-V17 fold state (lines 38-45+).

Defects (path:line):

- Lines 5-13: `Pass Omega V9 / SK-V15 authority update` names `sk-v15/SPEC.md` + `DISPATCH-PROMPT.md`,
  W0-W11, as the active contract. SK-V18 SPEC is the active contract.
- Lines 18-29: the `W0..W11` PRUNE-then-REBUILD sequence; SK-V18 is `W-PRUNE/P1..P5 → G1..G6 → PROVE → H1`.
- Lines 32-35: "CSS L4's 24 SK-V14 rows are audit-demoted ... `lightningcss` remains diagnostic until
  Track 1 emits comparable CSSOM/value output." SK-V18: CSS BEATS lightningcss 1.66-3.38× via
  `track1_rich` (the binding same-run gate is `track1_rich/lightningcss > 1.0×`, SPEC:181).
- Lines 38-45: the SK-V17 fold block reads as the live fold; SK-V18 is the certified adopter of that
  tape/`ValueRef<G>` fold (one generator now emits CSS+Sheets too).

Required replacement posture (STAGED text):

```text
Pass Omega V10 / SK-V18 generalization authority (2026-06-01). SK-V15's
PRUNE-then-REBUILD W0-W11 narrative is HISTORICAL. SK-V18 — the GENERALIZATION
cycle — is the active certified contract: the two hand-written/forked parsers
(JSON + CSS) collapse into ONE grammar-driven generator emitting JSON + CSS +
Sheets from `.bbnf` (`generator_grammar_count == 3`), preserving >SOTA honestly
(CSS beats lightningcss 1.66-3.38× via `track1_rich`; JSON beats sonic-rs strict),
x86 DELETED (aarch64-only), net LOC ≈ −10800. The active implementation contract
is `restart/skinny/tranches/sk-v18/SPEC.md` — the W-PRUNE→G1..G6→PROVE→H1 12-wave
manifest. W-PRUNE (P1-P5) is the only dispatch-now-eligible cluster after
G-Omega; every G1..G6/PROVE/H1 wave is gated on its predecessor's exit gate.
```

```text
JSON parse_only / direct_to_struct / real_typed_struct remain 51/51 strict
same-plane guard rows (maintained at G1, SPEC:476). CSS is no longer demoted:
the certified close gate is the same-run `track1_rich/lightningcss > 1.0×` ∧ no
same-run regression vs the parser's OWN pre-G2 baseline. `cssparser` is the
9-field EXACT CORRECTNESS oracle (gate-before-speed), NOT a speed comparator;
`lightningcss` IS the CSS >SOTA speed bar.
```

Keep the 5-shape `BackendShape` canon and Lock 1 substrate-union text. The 16-lock count is preserved
by addition (no sixth shape, no renumber, no production FNV route, no new public syntax).

### WORKSPACE (`restart/skinny/WORKSPACE.md`)

Operation: Update the SK-V15 workspace receiver (lines 29-60) and the SK-V17 fold receiver (lines 62-78).

Defects (path:line):

- Lines 29-39: binds the active workspace receiver to the locked SK-V15 W0-W11 contract + the
  SK-V14 W5B/W5C/W5D historical graph. Replace with the SK-V18 W-PRUNE→G1..G6→PROVE→H1 envelope.
- Lines 41-47: the SK-V15 telemetry field set (`measurement_row_id`, `css_comparator_workload`, ...).
  SK-V18 telemetry is the SPEC §3 gate schema (`track1_rich_over_lcss_ratio`,
  `track1_rich_over_lcss_ratio_pre_g2`, `generator_grammar_count`, `emitter_fork_present`,
  `named_primitive_falsifier_pass` per (a)-(d), `acceleration_at_admission`, `corpus_in_timer`,
  `simd_admission_profile_sampled`, SPEC:264-281).
- Lines 53-54: "Pattern H remains a 67-file repair surface until W4 proves line-1 generated
  provenance." SK-V18 routes generated-provenance proof through G1's `.bbnf`-mutation falsifier and
  the byte-equiv diff control, not a standalone W4.
- Lines 62-78: the SK-V17 fold receiver describes the fold direction with SK-V18 as the future adopter;
  flip to "SK-V18 IS the certified adopter — the one generator now emits JSON+CSS+Sheets."

Required replacement posture (STAGED text):

```text
Pass Omega V10 / SK-V18 workspace receiver (2026-06-01). The active workspace
receiver is the certified SK-V18 W-PRUNE→G1..G6→PROVE→H1 contract: P1-P5 PRUNE
(x86 src/x86_64 + ext/x86 DELETED, the const courier CSS_GENERATED_RS and the
JSON 7× push_str fixed-literal RETIRED, the 7 byte-identical css_l4 replicas
collapsed, the 7 xtask RuntimeTarget rows collapsed via the R16 PartialEq
recipe); G1 JSON un-courier + leaf primitives; G2 CSS lowering via the
css_balanced_component_scan named primitive; G3 un-fork the emitter (DELETE
RuntimeEmitterKind, dispatch on BackendShape); G4 shared value-API trait;
G5/G6 neutral NEON scan retarget (G6=WIRE); PROVE Sheets via the un-forked
generator; H1 honest >SOTA re-capture. Net LOC ≈ −10800. The SK-V14 W5B/W5C/W5D
and SK-V15 W0-W11 graphs are historical only.
```

```text
`bbnf-bench` / gate/report code consumes the SK-V18 §3 gate schema:
`generator_grammar_count`, `emitter_fork_present`, `runtime_target_rows_collapsed`,
`named_primitive_falsifier_pass` (per (a)-(d)), `track1_rich_over_lcss_ratio`,
`track1_rich_over_lcss_ratio_pre_g2`, `css_typed_summary_equal` (9-field cssparser
oracle), `acceleration_at_admission`, `simd_admission_profile_sampled`, and
`corpus_in_timer`. Producer-only telemetry, source-present unwired primitives,
self-exempting Lock 14 exclusions, and warm-only bench evidence reject close.
```

Keep crate set and LOC budget receivers; this Omega-E proposes no crate addition. Note the campaign
is net-DELETING (−10800 LOC) so the budget posture is contraction, not growth.

### HARDENING (`restart/skinny/HARDENING.md`)

Operation: Update the SK-V15 receiver (lines 7-23), the SK-V17 honesty-firewall (lines 25-41), and
re-key the W2-W10 seven-lens trigger (lines 43-53).

Defects (path:line):

- Lines 7-23: the SK-V15 receiver reads `sk-v15/SPEC.md`, refuses close on W6-era CSS criteria, and
  treats `CSS_GENERATED_RS`/`CssFullParseSummary`/fact-stream as "diagnostic after W1." SK-V18 RETIRES
  those constructs in W-PRUNE; the refusal list must re-key to the SK-V18 falsifiers.
- Lines 25-41: the SK-V17 firewall reads every CSS-`>SOTA` claim as "predicted / SK-V18-pending, not
  proven" and "CSS `>SOTA` bar UNMEASURED-PENDING." SK-V18 measure-proves it; flip to certified-plan.
- Lines 43-53: the W2-W10 seven-lens trigger names SK-V15 wave numbers; re-key to the SK-V18 wave
  names (W-PRUNE/G1..G6/PROVE/H1) and add the §6 (a)-(d) named-primitive lens (CH7's overfit-prune
  arm now subsumes the per-primitive `*_abcd_pass` falsifier).

Required replacement posture (STAGED text):

```text
Pass Omega V10 / SK-V18 hardening receiver (2026-06-01). A hardening run reads
the certified SK-V18 SPEC, SYNTHESIS, HANDOFF; the S-P1 PROFILE; the S-P2
RESEARCH (R-A..R-F + the §6 (a)-(d) gate); the S-P0 AUDIT-OVERFIT (PRUNE-list,
CH7); and the converged T-P1/T-P2/T-P3 packets before treating any skinny
surface as current. Refuse implementation before G-Omega. Refuse SK-V18 close
from: documentation-only evidence; warm benches; x86/AVX-512/SVE anchors (x86 is
DELETED, a PRUNE target); a verbatim const courier (CSS_GENERATED_RS) or a
fixed-literal render(); a named primitive with named_primitive_falsifier_pass
!= true on ANY of (a)-(d); a primitive whose LOC exceeds its profiled hot-leaf
extent (the (d) god-kernel reject); an emitter still forked on a grammar tag
(emitter_fork_present must be false, dispatched on BackendShape); a SIMD
acceleration claim whose admission is dead (acceleration_at_admission must be
admission, requiring BOTH the generated.rs caller census AND
simd_admission_profile_sampled); and any same-run CSS ratio NOT measured
track1_rich/lightningcss in one quiet plane.
```

```text
W-PRUNE and G1..G6/PROVE/H1 are mandatory seven-lens CHALLENGE candidates unless
the plan proves the redress is ledger-only and non-behavioral. Lenses: CH1
correctness, CH2 generality, CH3 regression (incl. delete-before-rebuild cycle
detection), CH4 cost, CH5 hidden coupling (incl. closed-enum sidecar / one-to-N
broadcast detection), CH6 anti-paper-close, CH7 overfit-prune/gate-exclusion —
CH7 now subsumes the per-primitive (a)-(d) named-primitive falsifier
(grammar-INVOKED-by-name, emitted-output-VARIES-under-rule-mutation,
verbatim_blob_present == false, PROFILE-PROVEN-NARROW-LEAF).
```

### COMPILER (`restart/skinny/COMPILER.md`)

Operation: Update the SK-V15 compiler receiver (lines 41-68) and the SK-V17 receiver (lines 70+).

Defects (path:line):

- Lines 41-51: the SK-V15 obligations are "W3, W5, W7, W8, W9" with Decision Engine as scaffold.
  SK-V18's compiler obligations are G1 (JSON un-courier + §6 leaf primitives), G2 (CSS lowering via
  `css_balanced_component_scan`), G3 (DELETE `RuntimeEmitterKind`, dispatch on `BackendShape`), G4
  (shared value-API trait), PROVE (Sheets via the same un-forked generator).
- Lines 53-59: "fact-stream-only CSS `parse()`, `CssFullParseSummary`, `CSS_GENERATED_RS`, and
  brace-counter proof are not live CSS admission after SK-V15 W6." Under SK-V18 these are RETIRED in
  W-PRUNE/G2 (`CSS_GENERATED_RS` DELETED, `verbatim_blob_present == false` campaign-wide, SPEC:66).
- Lines 61-68: "Pattern H remains a 67 root-runtime-file repair surface until W4." SK-V18 proves
  generated provenance via G1's `.bbnf`-mutation falsifier + byte-equiv diff control, not W4.
- Lines 70-95: the SK-V17 receiver describes the shared NEON classifier + the ONE `BackendRule`-walking
  projection generator as FOLD PREDICTIONS. SK-V18 CERTIFIES both: the one generator emits
  JSON+CSS+Sheets (G1/G2/PROVE), and the NEON classifier is WIRED at G5/G6 (G6=WIRE).

Required replacement posture (STAGED text):

```text
Pass Omega V10 / SK-V18 compiler receiver (2026-06-01). The compiler obligations
are the certified SK-V18 generator waves. G1: retire the JSON 7× push_str
fixed-literal; emit json/generated.rs from json.bbnf with the §6 (a)-(d)-gated
string/number leaf primitives. G2: lower CSS via the css_balanced_component_scan
named primitive (the 94.1% hot leaf, FORCED-CSS-scoped per s6/C4) + fact-keyed
projection; DELETE CSS_GENERATED_RS. G3: un-fork the emitter — DELETE
RuntimeEmitterKind{CompiledLowering,RequestFacts}; render(program) reads its
output-shape ONLY from program.policy_summary.backend_shape (the grammar-NEUTRAL
5-shape BackendShape), never from a RuntimeTarget field (emitter_fork_present ==
false; generator_grammar_branch_count == 0). PROVE: Sheets emits through the
SAME un-forked generator (google-sheets.bbnf, pratt-operator shape) — the
negative control that proves the generalization is real. generator_grammar_count
== 3 (json + css + sheets, NOT json + 7-css + sheets).
```

```text
The compiler receiver preserves the exact 5-shape BackendShape canon at Lock 10
({EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}). The ONE
BackendRule-walking projection generator and the shared
select_classifier(alphabet:&[u8;64]) NEON path — SK-V17 fold PREDICTIONS — are
SK-V18-CERTIFIED: the one generator re-emits JSON byte-equal AND CSS+Sheets lazy
from one walk; the NEON classifier is WIRED (not scalar-delegate) at G5/G6 with
acceleration_at_admission == admission. FactStream is not a sixth BackendShape;
EventTape is not a retained sidecar.
```

### BENCH (`restart/skinny/BENCH.md`)

Operation: Update — NOT read/no-op. This surface carries the load-bearing CSS comparator inversion.

Defects (path:line):

- Lines 29-37: the SK-V15 bench receiver — "CSS W8R 24-row evidence is diagnostic or NO-GO ...
  `lightningcss` is a diagnostic planning signal ... not an admission floor." INVERTED by SK-V18.
- Lines 32-34, 73, 341-343, 1663-1668, 2268: every occurrence of the claim "`lightningcss` is
  diagnostic until Track 1 emits comparable CSSOM/value output" + "`cssparser` is the near-term
  same-workload speed comparator." SK-V18: `lightningcss` IS the >SOTA speed bar
  (`track1_rich/lightningcss > 1.0×`, SPEC:181); `cssparser` is the 9-field EXACT correctness oracle,
  gate-BEFORE-speed, structurally distinct from `track1_rich` (SPEC:183) — NOT a speed comparator.
- Lines 39-55: the SK-V15 telemetry field set; replace with the SK-V18 §3 gate schema.
- Lines 57-73: the SK-V17 "CSS `>SOTA` is UNMEASURED-PENDING — an SK-V18 proof obligation, NOT met"
  block. SK-V18 measure-proves it; flip to certified, and adopt `track1_rich` as the canonical CSS
  workload (the bit-rot fix `784ceb418`: `track1_fact_stream → track1_rich`).

Required replacement posture (STAGED text):

```text
Pass Omega V10 / SK-V18 bench receiver (2026-06-01). The canonical CSS workload
is track1_rich (parser::rich_summary summing the 9 materialized fields:
rules/at_rules/qualified_rules/declarations/selectors/dimensions/numbers/colors/
functions) — an eager-rich typed CSSOM projection against lightningcss's full
CSSOM. The bit-rot fix 784ceb418 replaced the deleted track1_fact_stream
workload (the W-PRUNE deleted the fact-stream String) with track1_rich. BOTH the
track1_rich win (1.66-3.38×) AND the old fact-stream loss (0.60-0.76×) are
DIRECTIONAL figures captured under concurrent-session load (loadavg 4.35 at
capture); neither is a settled MEASUREMENT-VALID comparison until the H1
css_canon_bench re-lock under host_loadavg < 1.0 (CH2-V1-R03: the un-caveated
"MEASUREMENT-VALID" closure word is FORBIDDEN before H1). lightningcss IS the
binding CSS >SOTA speed bar: the binding close gate is the H1 css_canon_bench
re-lock — SAME-RUN track1_rich/lightningcss > 1.0× ∧ no same-run regression vs
the parser's OWN pre-G2 baseline (track1_rich_over_lcss_ratio_pre_g2, captured AT
G2 ENTRY in one quiet run) ∧ ≥1 regular corpus crossing >1.0× same-run under
host_loadavg < 1.0. cssparser is the 9-field EXACT CORRECTNESS oracle (gate-BEFORE-speed,
css_typed_summary_equal), structurally distinct from track1_rich — NOT a speed
comparator.
```

```text
SK-V18 rows require the §3 gate schema: generator_grammar_count,
emitter_fork_present, runtime_target_rows_collapsed, named_primitive_falsifier_pass
(per (a)-(d)), css_track1_rich_median_mbps, css_lightningcss_full_cssom_median_mbps,
track1_rich_over_lcss_ratio, track1_rich_over_lcss_ratio_pre_g2,
css_typed_summary_equal, acceleration_at_admission, simd_admission_profile_sampled,
and corpus_in_timer. Cold per-parse N≥50 median only — no warm/cached rows. A NEON
speedup CLAIM (G6) is admissible only when acceleration_at_admission == admission
(BOTH the generated.rs hot-loop caller census AND simd_admission_profile_sampled).
The corpus-in-timer figure is DEFERRED from G6 to H1 (G6=WIRE: G6 proves the wire,
H1 produces g6_speedup_median_mbps under corpus_in_timer).
```

### SUBSTRATE (`restart/skinny/SUBSTRATE.md`)

Operation: Limited update. Substrate mechanics survive SK-V18 totality closure intact; only the
authority/status blocks need the SK-V18 anchor.

Substrate content that SURVIVES verbatim: Lock 1 substrate union (no parallel substrate, no cross-call
retained classifier state); the 5-shape `BackendShape` canon; no retained EventTape sidecar; Lock 16
scalar-oracle + checkasm posture; the SoA `Tape<'input>` + lazy `ValueRef<G>` as the single
post-fold substrate.

Defects (path:line):

- Lines 33-37: the SK-V15 substrate receiver — "The active implementation route is SK-V15 W0-W11."
  Replace with the SK-V18 W-PRUNE→G1..G6→PROVE→H1 route.
- Lines 46-50: "FactStream remains substrate-manifest classification only ... Fact-stream-only CSS
  output is diagnostic after W1 and must retire from live admission by W6 typed CSS proof." Under
  SK-V18 the fact-stream `String` is DELETED in W-PRUNE; CSS admission is `track1_rich` typed CSSOM.
- Lines 51-61: the SK-V15 W2/W8/W9 obligation phrasing; re-key to SK-V18 G2 (CSS lowering),
  G3 (un-fork), G5/G6 (NEON wire) — the 5-shape search domain holds verbatim.
- Lines 63-74: the SK-V17 tape-fold receiver reads as the live fold posture; SK-V18 is the certified
  adopter (the one generator now projects `ValueRef<G>` for JSON+CSS+Sheets).

Required replacement posture (STAGED text):

```text
Pass Omega V10 / SK-V18 substrate receiver (2026-06-01). No substrate amendment
follows from SK-V18 totality closure. Lock 1 substrate union, no parallel
substrate, no cross-call retained classifier state, and the exact 5-shape
BackendShape canon {EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}
remain binding (the 16-lock count is preserved by addition). The SoA Tape<'input>
is the single post-fold substrate; lazy ValueRef<G> is the one materialization
plane, now SK-V18-CERTIFIED to project JSON byte-equal AND CSS+Sheets lazy from
ONE BackendRule-walk. The fact-stream String is DELETED (W-PRUNE); CSS live
admission is the track1_rich typed CSSOM projection. SK-V18 G5/G6 wire the NEON
classifier (acceleration_at_admission == admission); G3 un-forks the emitter on
the BackendShape discriminator. A sixth shape, retained EventTape sidecar, public
UnionTape, alternate document projection, or production FNV route remains blocked.
```

Do not change substrate data structures from Omega-E. Do not add a retained EventTape sidecar, sixth
`BackendShape`, public `UnionTape`, alternate document projection, or production FNV route.

## Cross-Surface CRUD Notes

- Replace every "current authority is SK-V15 SPEC/DISPATCH" with "current authority is SK-V18 SPEC
  (the certified W-PRUNE→G1..G6→PROVE→H1 manifest) after G-Omega."
- Preserve SK-V14/SK-V15 as historical evidence; do not let them dispatch. SK-V16/SK-V17 are the
  immediate antecedents (SK-V17 tape-fold predictions are now CERTIFIED by SK-V18).
- CSS comparator correction (the load-bearing one): `lightningcss` IS the CSS >SOTA SPEED bar
  (`track1_rich/lightningcss > 1.0×`, directionally 1.66-3.38× and the old fact-stream directionally
  0.60-0.76× — BOTH directional under loadavg 4.35, neither MEASUREMENT-VALID until the H1
  `css_canon_bench` re-lock under host_loadavg < 1.0, CH2-V1-R03); `cssparser` is the 9-field EXACT
  CORRECTNESS oracle (gate-before-speed), NOT a speed comparator. Reverse the corpus inversion.
- Adopt `track1_rich` as the canonical CSS workload everywhere `track1_fact_stream` /
  `CssFullParseSummary` / "fact-stream-only `parse()`" appears (the bit-rot fix `784ceb418`).
- Replace "CSS `>SOTA` is UNMEASURED-PENDING / an SK-V18 proof obligation" with "CSS >SOTA is the
  certified SK-V18 close gate: directionally measured at G2 exit, BINDING-closed only at the H1
  `css_canon_bench` re-lock under host_loadavg < 1.0 (the un-caveated MEASUREMENT-VALID closure word
  is forbidden before H1, CH2-V1-R03)."
- Re-key Decision Engine / FNV / Pattern-H language: SK-V18 RETIRES `CSS_GENERATED_RS` and the JSON
  fixed-literal in W-PRUNE/G1/G2; the §6 (a)-(d) named-primitive gate replaces the "scaffold until W7"
  framing for the leaf scanners.
- State x86 as DELETED (a PRUNE target P1), not "diagnostic." Net LOC ≈ −10800 (a deleting campaign).
- Preserve the 16-lock count and 5 BackendShape variants — amendment by ADDITION, no renumber.

## Staged Diff

The per-surface unified-diff is staged at:
`restart/audit/totality/astral/V10/ΩE-skinny-corpus-staged-diff.md`

It is the EXACT replacement-block-per-surface the CRUD-5 will apply POST-G-Omega. It is STAGED ONLY —
no live skinny surface is edited by this Omega-E.

## Non-Goals

This Omega-E artefact produces STAGED CRUD-5 updates only. It does not edit live surfaces, does not
merge any governance surface (`restart/ARCHITECTURE.md`, `MASTER-PLAN.md`, `locks/LOCKS.md`,
`HANDOFF.md`, `MIGRATION.md`), does not modify `skinny/RESULTS.md` or `skinny/REDRESS.md`, does not
re-derive T-P3 (it CONSUMES the 3C-locks-v+1-diff), does not stage or commit any code, and does not
reopen substrate mechanics beyond authority/status alignment. The CRUD merge executes POST-G-Omega.
