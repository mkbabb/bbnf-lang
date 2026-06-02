# Omega-E Skinny Corpus — STAGED CRUD-5 Diff (Pass Omega V10 / SK-V18)

Date: 2026-06-01. STAGED ONLY — applies POST-G-Omega, after user sign-off. No live surface is edited
by this artefact. Each block is the receiver-region replacement the CRUD-5 will execute. Line anchors
are re-anchored to staging HEAD `25297a7fc` (the same HEAD the locks-diff / master-plan-diff stage
against); all six corpus docs were last touched 2026-05-30 and every cited anchor (INDEX :5/:36/:38,
WORKSPACE, HARDENING, COMPILER, BENCH :73/:2268, SUBSTRATE) is verified-resolvable at `25297a7fc`.
The CRUD-5 MUST re-grep the first-line anchor string of each receiver block before applying (the bodies
below carry context; surrounding prose downstream of each receiver remains unless it cites a superseded
wave name) — a line-range block-replace whose first-line anchor string no longer matches at apply-time
is HALTED, not force-applied.

Order of application is identical per surface: REPLACE the SK-V15 receiver block, then REPLACE/FLIP
the SK-V17 fold block, then correct the in-body CSS-comparator / wave-name references.

---

## 1. INDEX.md

### 1a. Replace the `Pass Omega V9 / SK-V15` authority block (lines 5-36)

REMOVE lines 5-36 (the `**Pass Omega V9 / SK-V15 authority update (2026-05-28).**` paragraph through
`...it cannot substitute for SK-V15 close evidence.`). REPLACE with:

```text
**Pass Omega V10 / SK-V18 generalization authority (2026-06-01).** SK-V15's
PRUNE-then-REBUILD W0-W11 narrative is historical. SK-V18 — the GENERALIZATION
cycle, the inflection backtrack — is the active certified contract: the two
hand-written/forked parsers (JSON + CSS) collapse into ONE grammar-driven
generator emitting JSON + CSS + Sheets from `.bbnf` (`generator_grammar_count
== 3`), preserving >SOTA honestly (CSS beats lightningcss 1.66-3.38x via
`track1_rich`; JSON beats sonic-rs strict), x86 DELETED (aarch64-only), net LOC
≈ −10800. The active implementation contract is
`restart/skinny/tranches/sk-v18/SPEC.md` — the certified
W-PRUNE→G1..G6→PROVE→H1 12-wave manifest. W-PRUNE (P1-P5) is the only
dispatch-now-eligible cluster after G-Omega; every G1..G6/PROVE/H1 wave is
gated on its predecessor's exit gate AND its entry-gate predicate AND an
explicit wave-triumvirate dispatch.

Historical SK-V5/SK-V6/SK-V13/SK-V14/SK-V15 cohorts remain evidence for
rejected routes, primitive-admission discipline, and strict same-plane
comparator language; they are not the active dispatch anchor. SK-V16/SK-V17 are
the immediate antecedents — the SK-V17 tape-fold predictions (the ONE
BackendRule-walking projection generator, the shared NEON classifier) are now
SK-V18-CERTIFIED.

JSON parse_only / direct_to_struct / real_typed_struct remain 51/51 strict
same-plane guard rows (maintained at G1). CSS is no longer demoted: the
certified close gate is the SAME-RUN `track1_rich/lightningcss > 1.0x` ∧ no
same-run regression vs the parser's OWN pre-G2 baseline
(`track1_rich_over_lcss_ratio_pre_g2`, captured at G2 entry). `cssparser` is
the 9-field EXACT CORRECTNESS oracle (gate-before-speed), structurally distinct
from `track1_rich` — NOT a speed comparator; `lightningcss` IS the CSS >SOTA
speed bar. The 16-lock count and 5-shape `BackendShape` canon are preserved by
addition (no sixth shape, no renumber, no production FNV route, no new public
syntax).
```

### 1b. Flip the `Pass Omega V5 / SK-V17 fold state` block (lines 38-45+)

In the block beginning `**Pass Omega V5 / SK-V17 fold state (2026-05-30).**`, REPLACE the framing that
casts SK-V18 as the future adopter. Change the lead sentence to:

```text
**Pass Omega V10 / SK-V18 fold-adoption state (2026-06-01).** The SK-V17 T-P3
tape-fold is CERTIFIED-ADOPTED by SK-V18: the flat lazy-offset SoA
`Tape<'input>` + lazy `ValueRef<G>` projection is the single post-fold
substrate the 5 `BackendShape` shapes project from, and the ONE
BackendRule-walking projection generator now emits JSON byte-equal AND CSS +
Sheets lazy from one walk. The durable skinny REJECTIONS (AZ-IV eager value
tree 118x, per-leaf indirection, CSS fact-stream String as admission plane,
x86/AVX/SVE) are locks-strengthening fences — x86 is now DELETED, not merely
fenced.
```

---

## 2. WORKSPACE.md

### 2a. Replace the `Pass Omega V9 / SK-V15 workspace receiver` block (lines 29-56)

REMOVE lines 29-56 (`**Pass Omega V9 / SK-V15 workspace receiver (2026-05-28).**` through
`...cannot remain live admission after W6.`). REPLACE with:

```text
**Pass Omega V10 / SK-V18 workspace receiver (2026-06-01).** The active
workspace receiver is the certified SK-V18 W-PRUNE→G1..G6→PROVE→H1 contract.
W-PRUNE (P1-P5): x86 (`src/x86_64/` + `ext/x86/` + the nasm `build.rs`) DELETED;
the const courier `CSS_GENERATED_RS` and the JSON 7x `push_str` fixed-literal
RETIRED; the 7 byte-identical `css_l4_*/generated.rs` replicas collapsed to ONE
CSS config; the 7 xtask `RuntimeTarget` rows collapsed via the R16
`PartialEq` full-row derive. G1: JSON un-courier + the §6 (a)-(d)-gated
string/number leaf primitives. G2: CSS lowering via the
`css_balanced_component_scan` named primitive. G3: un-fork the emitter (DELETE
`RuntimeEmitterKind`, dispatch on `BackendShape`). G4: the shared value-API
trait. G5/G6: neutral NEON scan retarget (G6=WIRE). PROVE: Sheets via the SAME
un-forked generator. H1: honest >SOTA re-capture. Net LOC ≈ −10800. The SK-V14
W5B/W5C/W5D and SK-V15 W0-W11 graphs are historical only.

`bbnf-bench` / gate/report code consumes the SK-V18 §3 gate schema:
`generator_grammar_count`, `emitter_fork_present`,
`runtime_target_rows_collapsed`, `named_primitive_falsifier_pass` (per (a)-(d)),
`track1_rich_over_lcss_ratio`, `track1_rich_over_lcss_ratio_pre_g2`,
`css_typed_summary_equal` (the 9-field cssparser oracle),
`acceleration_at_admission`, `simd_admission_profile_sampled`, and
`corpus_in_timer`. Producer-only telemetry, source-present unwired primitives,
self-exempting Lock 14 exclusions, hidden one-to-N measurement stamps, and
warm-only bench evidence reject close.

`bbnf-simd` remains grammar-neutral and Lock-16-governed: each primitive carries
a scalar oracle/reference, strict parity/checkasm where relevant, a same-wave
generated consumer, and native Apple M5 Max / aarch64 admission evidence. x86 is
DELETED, not "diagnostic." A SIMD acceleration claim is admissible only when
`acceleration_at_admission == admission` (BOTH the `generated.rs` caller census
AND `simd_admission_profile_sampled`). The campaign is net-DELETING (≈ −10800
LOC); the budget posture is contraction, not growth.
```

### 2b. Flip the `Pass Omega V5 / SK-V17 workspace receiver` block (lines 62-78)

REPLACE the lead sentence `**Pass Omega V5 / SK-V17 workspace receiver (fold state).**` with:

```text
**Pass Omega V10 / SK-V18 workspace receiver (fold-adoption state).** SK-V18 IS
the certified adopter of the SK-V17 tape-fold: the PROVEN skinny `Tape`/
`ValueRef<G>` is adopted into crates/core (the SK-V19 target); the one generator
now projects `ValueRef<G>` for JSON + CSS + Sheets. The fold direction stays
invariant — skinny→totality, never back. The `crates/ir/src/registry/strategy.rs`
grammar-name string-ident leak surface stays a catalogued Lock-14 baseline whose
monotonic decrease is owned by the SK-V18 `ValueRef<G>` projection generator
wave (G1/G2/G4). `bbnf-simd` stays grammar-neutral, aarch64-only; the shared NEON
classifier's only grammar datum is `alphabet:&[u8;64]`.
```

---

## 3. HARDENING.md

### 3a. Replace the `Pass Omega V9 / SK-V15 receiver` block (lines 7-23)

REMOVE lines 7-23 (`**Pass Omega V9 / SK-V15 receiver (2026-05-28).**` through `...belongs to W6.`).
REPLACE with:

```text
**Pass Omega V10 / SK-V18 hardening receiver (2026-06-01).** A hardening run
reads the certified SK-V18 `SPEC.md`, `SYNTHESIS.md`, `HANDOFF.md`; the S-P1
`SYNTHESIS-PROFILE.md`; the S-P2 `SYNTHESIS-RESEARCH.md` (R-A..R-F + the §6
(a)-(d) gate); the S-P0 `SYNTHESIS-AUDIT-OVERFIT.md` (PRUNE-list, CH7); and the
converged T-P1/T-P2/T-P3 consolidated packets before treating any skinny surface
as current. SK-V14 W5B-FRONTENDR and SK-V15 W0-W11 remain historical evidence
only.

Refuse implementation before G-Omega. Refuse SK-V18 close from:
documentation-only evidence; warm benches; x86/AVX-512/SVE anchors (x86 is
DELETED, a PRUNE target); a surviving verbatim const courier (`CSS_GENERATED_RS`)
or a fixed-literal `render()` (`verbatim_blob_present` must be false); a named
primitive with `named_primitive_falsifier_pass != true` on ANY of (a)-(d); a
primitive whose LOC exceeds its profiled hot-leaf extent (the (d) god-kernel
reject); an emitter still forked on a grammar tag (`emitter_fork_present` must be
false, dispatched on `BackendShape`); a SIMD acceleration claim whose
`acceleration_at_admission == dead`; and any CSS ratio NOT measured
`track1_rich/lightningcss` same-run in one quiet plane. `cssparser` is the
9-field EXACT correctness oracle (gate-before-speed), NOT a speed comparator.
```

### 3b. Flip the `Pass Omega V5 / SK-V17 hardening receiver` block (lines 25-41)

REPLACE the lead sentence `**Pass Omega V5 / SK-V17 hardening receiver (fold-scope honesty
firewall).**` and the CSS-`>SOTA`-pending framing with:

```text
**Pass Omega V10 / SK-V18 hardening receiver (fold-scope honesty firewall).** The
SK-V17 firewall's CSS-`>SOTA`-UNMEASURED-PENDING posture is DISCHARGED: SK-V18
measure-proves CSS >SOTA via the certified `track1_rich/lightningcss > 1.0x`
same-run gate (beaten 1.66-3.38x). The honesty firewall now reads every Sheets
claim as the PROVE-wave negative control (proven-by-exercise once
`google-sheets.bbnf` emits a non-hollow pratt-operator `generated.rs`,
md5-DISTINCT from JSON/CSS), and every NEON acceleration claim as admissible only
under `acceleration_at_admission == admission`. Refuse any claim that ships a
dual AoS/SoA substrate as a Lock-1 closure, that admits a 6th `BackendShape`,
that reads `cssparser` as a speed comparator, or that asserts the generalization
without the Sheets negative control.
```

### 3c. Re-key the seven-lens CHALLENGE trigger (lines 43-53)

REPLACE the opening sentence `W2-W10 are mandatory seven-lens CHALLENGE candidates...` with:

```text
W-PRUNE and G1..G6/PROVE/H1 are mandatory seven-lens CHALLENGE candidates unless
the plan proves the redress is ledger-only and non-behavioral. The lenses are CH1
correctness, CH2 generality, CH3 regression (incl. delete-before-rebuild cycle
detection), CH4 cost, CH5 hidden coupling (incl. closed-enum sidecar and
one-to-N broadcast detection), CH6 anti-paper-close, and CH7
overfit-prune/gate-exclusion — CH7 now subsumes the per-primitive (a)-(d)
named-primitive falsifier (grammar-INVOKED-by-name;
emitted-output-VARIES-under-invoking-rule-mutation; `verbatim_blob_present ==
false`; PROFILE-PROVEN-NARROW-LEAF, primitive LOC ≤ profiled hot-leaf extent).
```

---

## 4. COMPILER.md

### 4a. Replace the `Pass Omega V9 / SK-V15 compiler receiver` block (lines 41-68)

REMOVE lines 41-68 (`**Pass Omega V9 / SK-V15 compiler receiver (2026-05-28).**` through
`...not naming patterns for new compiler code.`). REPLACE with:

```text
**Pass Omega V10 / SK-V18 compiler receiver (2026-06-01).** The compiler
obligations are the certified SK-V18 generator waves. G1: retire the JSON 7x
`push_str` fixed-literal; emit `json/generated.rs` from `json.bbnf` with the §6
(a)-(d)-gated `string`/`number` leaf primitives. G2: lower CSS via the
`css_balanced_component_scan` named primitive (the 94.1% hot leaf, FORCED-CSS-
scoped per s6/C4) + fact-keyed projection; DELETE `CSS_GENERATED_RS`. G3: un-fork
the emitter — DELETE `RuntimeEmitterKind{CompiledLowering,RequestFacts}`;
`render(program)` reads its output-shape ONLY from
`program.policy_summary.backend_shape` (the grammar-NEUTRAL 5-shape
`BackendShape`), never a `RuntimeTarget` field (`emitter_fork_present == false`;
`generator_grammar_branch_count == 0`). PROVE: Sheets emits through the SAME
un-forked generator (`google-sheets.bbnf`, pratt-operator shape) — the negative
control proving the generalization is real. `generator_grammar_count == 3` (json
+ css + sheets, NOT json + 7-css + sheets).

The compiler receiver preserves the exact 5-shape `BackendShape` canon at Lock 10
({EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}). `CSS_GENERATED_RS`,
`CssFullParseSummary`, the fact-stream `String`, and brace-counter proof are
RETIRED in W-PRUNE/G2 (`verbatim_blob_present == false` campaign-wide); CSS live
admission is the `track1_rich` typed CSSOM projection. Generated status requires
grammar-DERIVED generation proven by the `.bbnf`-mutation falsifier
(emitted-output VARIES under invoking-rule mutation) + the byte-equiv diff
control — not a header-only change or a relabeled blob. Historical helpers
(`shapes_for_json`, `nominate_json`, static CSS provider/profile rosters) are
deletion targets, not naming patterns.
```

### 4b. Flip the `Pass Omega V5 / SK-V17 compiler receiver` block (lines 70-95)

REPLACE the lead sentence `**Pass Omega V5 / SK-V17 compiler receiver (shared NEON classifier + single
generator).**` with:

```text
**Pass Omega V10 / SK-V18 compiler receiver (shared NEON classifier + single
generator, CERTIFIED).** The SK-V17 fold PREDICTIONS are SK-V18-certified. The
shared `select_classifier(alphabet:&[u8;64])` NEON path takes the alphabet as its
ONLY grammar datum and is WIRED (not scalar-delegate) at G5/G6 with
`acceleration_at_admission == admission` (G6=WIRE: G6 proves the wire, H1
produces the corpus-in-timer speedup). The ONE `BackendRule`-walking projection
generator emits document/value/view/visitor for JSON byte-equal AND CSS + Sheets
lazy from one walk; the grammar enters as a TYPE parameter, never a generator
`match grammar` arm. A generator with a CSS-specific value branch FAILS the
onboarding predicate. aarch64-only; x86 DELETED.
```

---

## 5. BENCH.md

### 5a. Replace the `Pass Omega V9 / SK-V15 bench receiver` block (lines 29-55)

REMOVE lines 29-55 (`**Pass Omega V9 / SK-V15 bench receiver (2026-05-28).**` through `...mutation is
authorized by this document alone.`). REPLACE with:

```text
**Pass Omega V10 / SK-V18 bench receiver (2026-06-01).** The canonical CSS
workload is `track1_rich` (`parser::rich_summary` summing the 9 materialized
fields: rules/at_rules/qualified_rules/declarations/selectors/dimensions/numbers/
colors/functions) — an eager-rich typed CSSOM projection against lightningcss's
full CSSOM. The bit-rot repair replaced the deleted `track1_fact_stream` workload
(W-PRUNE deleted the fact-stream `String`) with `track1_rich`. BOTH the
`track1_rich` win (1.66-3.38x) AND the old fact-stream loss (0.60-0.76x) are
DIRECTIONAL figures captured under concurrent-session load (loadavg 4.35 at
capture); neither is a settled MEASUREMENT-VALID comparison until the H1
`css_canon_bench` re-lock under `host_loadavg < 1.0` (the un-caveated
MEASUREMENT-VALID closure word is FORBIDDEN before H1, CH2-V1-R03). `lightningcss`
IS the binding CSS >SOTA speed bar: the BINDING close gate is the H1
`css_canon_bench` re-lock — SAME-RUN `track1_rich/lightningcss > 1.0x` ∧ no
same-run regression vs the parser's OWN pre-G2 baseline
(`track1_rich_over_lcss_ratio_pre_g2`, captured AT G2 ENTRY in one quiet run) ∧
≥1 regular corpus crossing >1.0x same-run under `host_loadavg < 1.0`.
`cssparser` is the 9-field EXACT CORRECTNESS oracle (gate-before-speed,
`css_typed_summary_equal`), structurally distinct from `track1_rich` — NOT a
speed comparator.

SK-V18 rows require the §3 gate schema: `generator_grammar_count`,
`emitter_fork_present`, `runtime_target_rows_collapsed`,
`named_primitive_falsifier_pass` (per (a)-(d)), `css_track1_rich_median_mbps`,
`css_lightningcss_full_cssom_median_mbps`, `track1_rich_over_lcss_ratio`,
`track1_rich_over_lcss_ratio_pre_g2`, `css_typed_summary_equal`,
`acceleration_at_admission`, `simd_admission_profile_sampled`, and
`corpus_in_timer`, alongside the existing strictness/output-plane columns. Cold
per-parse N≥50 median only — no warm/cached rows. A NEON speedup CLAIM (G6) is
admissible only when `acceleration_at_admission == admission` (BOTH the
`generated.rs` hot-loop caller census AND `simd_admission_profile_sampled`); the
corpus-in-timer speedup is DEFERRED from G6 to H1. Admission evidence is native
Apple M5 Max / aarch64; x86 is DELETED. No production runtime/generic-codegen FNV
arbiter, selector, or hash-correctness proof is admissible.
```

### 5b. Correct the in-body CSS-comparator inversion

At each anchor below, the corpus says `lightningcss` is "a diagnostic planning signal / not an
admission floor until Track 1 emits comparable CSSOM/value output" and elevates `cssparser` as the
near-term same-workload SPEED comparator. The CRUD-5 REVERSES the roles at:

- Line 73 (`...stays diagnostic until Track 1 emits comparable typed CSSOM/value output.`) →
  REPLACE with: "`lightningcss` IS the binding CSS >SOTA speed bar; the BINDING close gate is the H1
  `css_canon_bench` re-lock under `host_loadavg < 1.0` (the same-run `track1_rich/lightningcss > 1.0x`,
  directionally 1.66-3.38x at loadavg 4.35 — not MEASUREMENT-VALID until H1, CH2-V1-R03)."
- Lines 341-343 (`...not an admission floor until Track 1 emits comparable CSSOM/value ... cssparser
  retime.`) → REPLACE with: "`lightningcss` is the CSS >SOTA speed bar; `cssparser` is the 9-field
  EXACT correctness oracle (gate-before-speed), not a speed comparator."
- Lines 1663-1668 (`...across 24 conceptual rows, cssparser beat Track 1 ... lightningcss counts only
  after Track 1 emits comparable CSSOM/value output.`) → REPLACE the active framing with: "the SK-V14
  24-row broadcast is historical; CSS now beats lightningcss 1.66-3.38x via `track1_rich`. `cssparser`
  is the correctness oracle, not a speed comparator."
- Line 2268 (`Does the substrate generalise beyond JSON? | SK-V15 open | CSS prior probe is historical;
  current proof requires W5 typed CSS output plus W6 same-workload cssparser retime`) → REPLACE with:
  "SK-V18 CERTIFIED | CSS generalizes via the one generator (G2); proof is the same-run
  `track1_rich/lightningcss > 1.0x` gate + the 9-field cssparser oracle parity; Sheets (PROVE) is the
  negative control."

### 5c. Flip the `Pass Omega V5 / SK-V17 bench receiver` block (lines 57-73)

REPLACE the lead sentence and the `CSS >SOTA is UNMEASURED-PENDING` framing with:

```text
**Pass Omega V10 / SK-V18 bench receiver (canonical N≥50 + lightningcss fair
bar, CERTIFIED).** The canonical harness is the cold-per-parse N≥50 median (no
warm/cached rows). JSON rides the tape >SOTA at 51/51 strict A/GO Track 1 > sonic
same-plane. CSS >SOTA is the certified SK-V18 close gate — DIRECTIONALLY measured
at G2 exit (`track1_rich/lightningcss > 1.0x`, 1.66-3.38x at loadavg 4.35) and
BINDING-closed only at the H1 `css_canon_bench` re-lock under `host_loadavg < 1.0`
(the un-caveated MEASUREMENT-VALID closure word is forbidden before H1,
CH2-V1-R03). A QUIET re-capture is required before any absolute Mbps claim in H1
(the S-P1 absolute ratios were load-depressed at loadavg 4.35; they are
directional antecedents, not the floor).
```

---

## 6. SUBSTRATE.md

### 6a. Replace the `Pass Omega V9 / SK-V15 substrate receiver` block (lines 33-61)

REMOVE lines 33-61 (`**Pass Omega V9 / SK-V15 substrate receiver (2026-05-28).**` through the numbered
items ending `...remains blocked.`). REPLACE with:

```text
**Pass Omega V10 / SK-V18 substrate receiver (2026-06-01).** No substrate
amendment follows from SK-V18 totality closure. Lock 1 substrate union, no
parallel substrate, no cross-call retained classifier state, and the exact
5-shape `BackendShape` canon {EagerTape, OffsetTape, EventTape, SinkOnly,
CollapsedStage} remain binding (the 16-lock count is preserved by addition). The
active implementation route is SK-V18 W-PRUNE→G1..G6→PROVE→H1, not SK-V15 W0-W11.

1. **Lock 1 substrate-union boundary remains elevated.** Quote/escape/structural
   masks, class-stream, prev-state byte, and prefix-XOR carry may be transient
   producers; cross-call retained classifier state remains inadmissible. They do
   not create a retained sidecar, second tape, public `UnionTape`, alternate
   document projection, or public substrate API.
2. **The fact-stream `String` is DELETED (W-PRUNE).** FactStream is not a sixth
   `BackendShape`; EventTape is not a retained sidecar stream. `CSS_GENERATED_RS`,
   `CssFullParseSummary`, and brace-counter proof are RETIRED; CSS live admission
   is the `track1_rich` typed CSSOM projection over the tape.
3. **SK-V18 G2 lowers CSS via the `css_balanced_component_scan` named primitive;
   G3 un-forks the emitter on the `BackendShape` discriminator; G5/G6 wire the
   NEON classifier** (`acceleration_at_admission == admission`; the only grammar
   datum is `alphabet:&[u8;64]`). Every SIMD primitive carries a scalar
   oracle/reference, strict parity/checkasm where relevant, a same-wave generated
   consumer, and native Apple M5 Max / aarch64 admission evidence. x86 is DELETED.
4. **The 5-shape search domain at Lock 10 holds verbatim.** A sixth shape,
   retained EventTape sidecar, public `UnionTape`, or production FNV route remains
   blocked.
```

### 6b. Flip the `Pass Omega V5 / SK-V17 substrate receiver` block (lines 63-74)

REPLACE the lead sentence `**Pass Omega V5 / SK-V17 substrate receiver (tape-as-unified-substrate +
ValueRef<G>).**` with:

```text
**Pass Omega V10 / SK-V18 substrate receiver (tape-as-unified-substrate +
ValueRef<G>, CERTIFIED).** The SoA `Tape<'input>` is the single post-fold
substrate; lazy `ValueRef<G>` is the one materialization plane, now
SK-V18-CERTIFIED to project JSON byte-equal AND CSS + Sheets lazy from ONE
`BackendRule`-walk. These are the proven-and-benched skinny WINs (JSON 51/51
strict A/GO Track 1 > sonic same-plane; CSS track1_rich > lightningcss
1.66-3.38x). No substrate amendment is created here. The receiver binds the LOCKS
SK-V17 T-P3 Crystallisation Addendum (Lock 1 tape-substrate-union clause + Lock 14
ValueRef/classifier-generalisation clause), preserved by addition in the 3C
locks-v+1 diff.
```

---

## Apply-Order Note For CRUD-5 (POST-G-Omega)

1. Re-grep each receiver's first-line anchor at the live HEAD before applying (line numbers drift if
   any other CRUD agent edits a surface first).
2. Apply per-surface in the order above (INDEX → WORKSPACE → HARDENING → COMPILER → BENCH → SUBSTRATE).
3. After all six, run `grep -c "SK-V15 W0-W11\|W5B\|track1_fact_stream\|lightningcss.*diagnostic\|
   cssparser.*comparator"` across the corpus — expect 0 active-dispatch hits (historical mentions
   inside explicitly-labelled "historical" sentences are acceptable).
4. Confirm `grep -c "SK-V18\|track1_rich\|W-PRUNE\|G6=WIRE\|generator_grammar_count"` is non-zero in
   all six surfaces.
5. Do NOT touch LOCKS.md / MASTER-PLAN.md / ARCHITECTURE.md / HANDOFF.md / MIGRATION.md from this
   diff — those are owned by the ΩC / ΩD CRUD streams.
```
