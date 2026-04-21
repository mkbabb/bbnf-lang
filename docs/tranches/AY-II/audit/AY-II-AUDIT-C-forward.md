# AY-II-AUDIT-C — Forward Path (pre-close triumvirate, pass II)

Read-only audit of AY-II's pre-close state at master HEAD `b5bbda6c`
(post-W0-fix + pause commit). Frames the architecturally idiomatic
forward path through the coherence gap AY-II/PROGRESS §Pre-close
pause flagged. No patches, no shims. Peer-referenced throughput
targets only (sonic-rs / simd-json / lightningcss / cssparser).

## 1. Scope + methodology

Read surfaces: AY-II.md, AY-II/PROGRESS.md, waves W0-W5,
AY-I/FINAL.md, AUDIT-A/B/D (AY-I-era), BA.md + BB.md + BC.md,
`ValueBuilder` + shape emitters, `TapeBuilder` +
`Columns::rollback_to` + cursor + scan policy, `generated.rs`
projection totality + `__path_walk`, `value_materialize.rs`. Every
verdict cites file:line or SHA.

W0's structural move landed; the **write-time value-stamp
coupling** that makes the fused pipeline fused was deferred
(`PROGRESS.md:98-114`). AUDIT-D §2 measures `bbnf_visitor / sonic
= 1.12×` twitter, 5-fx geomean `0.99×` — emitted parse runs at
near-sonic byte cost when tape-reconstruction is bypassed. The
forward path must preserve that discipline on the default
`to_value()` path.

## 2. Q1 — Fused-pipeline completion: the structural decision

### 2.1 Cost model

| Path | Thesis | Codegen impact | Samply | CSS L4 |
|---|---|---|---|---|
| **A** Thread `&mut ValueBuilder<R>` through every shape fn | Explicit dual-builder lockstep | +1 param on ~22 `emit_parse_*` sigs; `quote!` churn across all shape emitters | Two owners (tape + value) | Compounds land slab children; projection re-dispatches on `variant_idx` |
| **B** Merge to `FusedBuilder<R>` | Type-level collapse; shape emitters unchanged | Tape-crate migration only; `TapeBuilder` struct absorbs ValueBuilder fields | One fused symbol per push shape | Same as A at projection; merge is representation-only |
| **C** Retire `ValueBuilder`; project from tape post-parse | `to_value()` walks tape + dispatches per-rule materializer | Delete `value_builder.rs`; `project_value_output` emits cursor-descend calling the 69 emitted materializers | One owner (parse fn) + one walk owner at to_value | Walk reintroduces the 3.47× gap AUDIT-D §2 names |

### 2.2 Evaluation

| Criterion | A | B | C |
|---|---|---|---|
| Parity with AUDIT-D §2 visitor-lane (geomean 0.99× sonic) | YES — the fused path IS the visitor-lane discipline landed into `to_value()` | YES — same observable shape, single owner | NO — reintroduces the walk cost |
| Code volume | ~800 LOC churn (every shape emit_parse_* + every call site) | ~300 LOC inside tape crate (FusedBuilder trait impl) + 0 at emitter sites | ~150 LOC delete + emitted `project_value_output` body rewrite |
| Samply attribution clarity | Two named owners (tape + value) | One named owner (fused) | One named owner (parse fn) + one walk owner at `to_value()` call |
| Extension surface for CSS L4 rich AST | Explicit; each typed rule gets its own emission site | Explicit at projection; fused-push is uniform | Cleanest at projection (materializer fn per admission) but walks the tape to drive it |
| AY-II.md invariant §1 ("no tape-walking materializer path") | Holds by construction | Holds by construction | **VIOLATED** — `project_value_output` walks the tape |

### 2.3 Recommendation: path B (FusedBuilder)

Two substrates written in lockstep are one substrate with two
columns — the `TapeBuilder`/`ValueBuilder` split at the emitter
surface is premature. Collapsing at the type level:

1. Obeys §1 (single-pass, no tape walk).
2. Preserves §2 (tape remains canonical structural substrate;
   value is a parallel column set inside the same builder).
3. Eliminates A's per-shape threading churn.
4. One samply owner per push shape, not two.
5. `begin_compound`'s tuple `(kind, span_lo, variant_idx,
   meta_idx, frame_depth, extra_flags)` IS the value-frame
   payload; passing it to a separate builder is redundancy.

Path A is architecturally equivalent at ~500-LOC threading tax
for no observable benefit. Path C contradicts §1.

### 2.4 File bounds + LOC + codegen impact for path B

| File | Action | ±LOC | Rationale |
|---|---|---|---|
| `crates/tape/src/builder.rs` | Extend `TapeBuilder<R>` (becomes `FusedBuilder<R>`; rename), add a `ValueColumn` field bearing ValueBuilder fields; `begin_compound` / `end_compound` / `push_leaf_with` stamp value column in lockstep with the structural columns; `rollback_to` truncates both | +280 | Type-level absorption of `runtime/value_builder.rs` |
| `crates/tape/src/columns.rs` | No change to row-major contract; a new `ValueColumn` struct sits alongside `Columns` | +0 | Value frames are separate from tape rows; FusedBuilder owns both |
| `crates/core/src/runtime/value_builder.rs` | Delete; fold the `ValueBuilderOutput` type into `Tape`'s finish path | -700 | One output type, not two |
| `crates/core/src/runtime/parsed.rs` | `new_fused` collapses into `new`; `Parsed<'p, R>` carries the fused Tape only; `to_value()` reads value-column directly from `self.tape` | -50 | Single output handle |
| `crates/core/src/backend/rust/emitter/shapes/*.rs` | Every `builder.begin_compound(...)` / `end_compound(...)` / `push_leaf_with(...)` signature is **unchanged** — FusedBuilder inherits the same surface; retry-IIFE sites call one `builder.rollback_to(open_off)` which atomically truncates both columns | ±0 | This is the elegance — no per-site churn |
| `crates/core/src/backend/rust/emitter/grammar.rs` | `let mut builder = FusedBuilder::<Self>::with_capacity(...)` replaces the dual `TapeBuilder + ValueBuilder` allocation; `builder.finish()` returns both columns in one handle | -15 | Single allocation site |
| `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs` | `emit_project_value_fn` reads the value-column directly off `Parsed::tape()`; renames stay local | ±0 | Read path unchanged |

**Net: ~-485 LOC delete + ~280 LOC move; every emit_parse_*
unmodified.** Post-expand `ay-json.rs` shows one unified
push-sequence, one samply owner per shape — AUDIT-D §2's
`1.12×` visitor lane folded into `to_value()` by type-level
collapse.

## 3. Q2 — Structural-scan consumer: emission-time integration point

### 3.1 Landed state

- `STRUCTURAL_SCAN_POLICY` emitted per grammar
  (`dispatcher.rs:1867`, `#[allow(dead_code)]`; no consumer).
- `lookup_scan_policy` defined (`dispatcher.rs:1899`) with zero
  call sites outside definition.
- Cursor primitives `object_key_seek` / `bounded_lookahead` /
  `scan_structural_bounded` at `cursor.rs:438/473/496` — runtime
  surface ready.
- `generated.rs:24735` `__path_walk` still iterates generic
  children two-at-a-time.

### 3.2 Emission-time integration

Policy is codegen data; consumer is per-rule codegen. Touchpoint
is `__path_walk` emission in `view/value.rs`:

1. Emitter calls `lookup_scan_policy(ir, rule_id)` while walking
   each rule, obtaining `ScanActivationFlags`.
2. When flags admit `OBJECT_KEY_SEEK`, emit
   `cursor.object_key_seek(key_span)` for that rule's path step;
   for `BOUNDED_LOOKAHEAD`, emit `cursor.bounded_lookahead`; for
   `SCAN_STRUCTURAL_BOUNDED`, emit the bounded scan.
3. No runtime lookup — the emitted body carries only the
   primitive the policy admits; non-admitted rules fall through
   to the generic walk.

### 3.3 File bound + LOC

- `view/value.rs` — `__path_walk` emitter dispatches on
  `ScanActivationFlags` at emit-time (~+80 LOC).
- `shapes/dispatcher.rs` — `lookup_scan_policy` already `pub`.

### 3.4 Close gate

Samply on JSON twitter `Parsed::get`: `object_key_seek` named
hot-path owner; generic sibling-walk ≤ 1% (the W6 SOFT-PASS
gate).

## 4. Q3 — Projection totality consumer totality: end-to-end trace

### 4.1 Current state (BbnfBootstrap admission `bool_lit` as trace carrier)

| Step | Location | Status |
|---|---|---|
| Admission entry | `generated.rs:24393` `PROJECTION_DIRECT_TO_STRUCT[0] = ("bool_lit", "BbnfBootstrapBoolLitProjection")` | present |
| Marker fn | `generated.rs:24483` `__grammar_projection_bool_lit` | present |
| Materializer fn | `generated.rs:25683` `materialize_projection_bool_lit_BbnfBootstrap<'p>(...)` | **emitted, zero call sites** |
| Consumer variant | `generated.rs:24459` `BbnfBootstrapValue::bool_lit` | present as enum variant |
| `project_value_output` path | `generated.rs:24711` calls `project_value_BbnfBootstrap` which tail-calls `project_frame_BbnfBootstrap` | **reads ValueBuilderOutput frame directly**; does not invoke `materialize_projection_bool_lit_BbnfBootstrap` |

The wire-contract `projection_totality.rs` asserts
`len(ADMISSION) == len(MATERIALIZERS) == len(CONSUMERS)` — a
structural count match. It **does not** assert the materializer is
actually called from the consumer path. Grep of generated.rs:
`materialize_projection_\w+_BbnfBootstrap\s*\(` returns **0 call
sites.**

### 4.2 Architectural closure

`PROJECTION_MATERIALIZERS` names 69 fns; the consumer path
(`project_value_output` → `project_frame_<Grammar>` → per-variant
arm) dispatches on `frame.variant_idx` and reconstructs the
Value variant inline, bypassing the materializer. Two closures:

**Wrong**: delete `materialize_projection_*` fns (DEAD-surface
retirement). Loses CSS L4's typed-accessor read path.

**Right**: `project_frame_<Grammar>` arm per admission calls
`materialize_projection_<rule>_<Grammar>(output, input, frame)`
and wraps the projection struct in the `<Grammar>Value` variant.
Projection struct is authoritative; Value variant wraps it.
Result:

1. Every materializer is a live consumer of the FusedBuilder
   value column.
2. `cargo expand | grep 'materialize_projection_\w+_\w+\s*\('`
   returns exactly admission count per grammar.
3. CSS L4 (W2) lands on the same mechanism — each `-> TypeName`
   produces a materializer the projector dispatches to.

### 4.3 File bound + LOC

- `value_materialize.rs::emit_project_arm` — emit materializer
  call body per admission (~+60 LOC).
- `projection_totality.rs` — grep expanded output for 1:1
  call-site count vs admission count (~+30 LOC).

### 4.4 Close gate

`grep -c 'materialize_projection_\w\+_JsonParser\s*('
target/expand/ay-json.rs == PROJECTION_DIRECT_TO_STRUCT.len()`.
Totality is runtime-truth, not structural count.

## 5. Q4 — Compose-bridge cleanliness

### 5.1 Three options

| Option | Shape | Cost |
|---|---|---|
| (a) Leave history as-is; master is post-regen | Preserves commit ledger; `f372e7ef`'s hand-patched stub survives in history but is replaced by subsequent regen commits (`db979564`, `58271da1`, `f8ac2cd7`, `c9142405`) | Zero operational cost; SPEC §"Generated files are output of fresh regen; never hand-patch" violation persists as a recorded-commit-message concern |
| (b) Rebase W0's 10-commit sequence to eliminate the transient hand-patch | History-rewriting; requires redispatching W0.b / W0.c / W0.d agents against the corrected compose order, or an orchestrator-driven rebase that replays the same diffs in a different order | Orchestrator-hour cost; introduces a SHA-drift across the pass-II ledger that predecessor audit references will break |
| (c) Restructure the cherry-pick compose order so no transient stub is needed (dispatch sequencing is part of the forward plan) | Forward-facing only; applies to the NEXT wave's dispatch (W0' if W0 re-opens, otherwise the path-B implementation wave). Compose order: FusedBuilder → emitter signature migration → per-shape push sites → regen — dispatch the agents such that master compiles at every cherry-pick boundary without a stub | Zero cost on existing history; the dispatch ceremony itself ensures future waves do not reproduce the pattern |

### 5.2 Pick + justify: **(a) + (c)**

(a) alone leaves the SPEC violation; (c) alone does not address
the committed violation. **Combined**: the stub is a
diagnostic-recorded one-shot; the next wave's compose ceremony
forecloses the pattern via SPEC §cherry-pick-compose discipline
(agents commit contracts that compile at integration, not after).
(b) is rejected — history rewriting breaks predecessor audit
SHA citations for no operational benefit.

## 6. Q5 — W1-W5 wave validity post-W0

### 6.1 Per-wave reassessment

| Wave | Core thesis | Status after path B | Action |
|---|---|---|---|
| W1 (JSON) | `bbnf_value_* / sonic ≤ 1.15` via fused pipeline + competitor bench + per-fixture samply | **VALID.** Path B folds visitor-lane discipline into `to_value()` by construction; AUDIT-D §2 `bbnf_visitor_twitter = 1.12×` is the achievable ceiling; competitor bench + samply plans unchanged | KEEP AS-IS; re-dispatch after path B lands |
| W2 (CSS L4) | Typed projection totality + lightningcss parity + canonical byte parity + competitor bench | **VALID + tightened by Q3 closure.** W0.d's typed projection skeleton lands CSS `Color` + family; post-Q3 the materializer-per-admission path becomes the emitted call graph CSS needs for `Length` / `Angle` / `Selector` typed surfaces. W2's grammar annotation extension plan + at-rules.bbnf module are unaffected | KEEP AS-IS; Q3 closure feeds W2's `css_l4_canonical_parity` tightening |
| W3 (Sheets) | Sheets projection totality + parity suite + fat-LTO panic retirement + samply | **VALID.** W0.a's `rollback_to` retires the `columns.rs:409` panic class (AUDIT-D §5) and Sheets parse_nested's retry-IIFE-under-open-frame hazard collapses under path B's unified rollback (one FusedBuilder `rollback_to` call atomically truncates both columns; no frame-orphan class) | KEEP AS-IS |
| W4 (BBNF) | Bootstrap double-regen byte-identity + BBNF projection totality + `@pretty` repair + bbnf_self samply | **VALID.** Path B is structurally unobservable to bbnf-bootstrap's proc-macro cycle; the `@pretty` drift cause (AY-I.W6) retired at W0-fix; W4 verifies the retirement composed correctly on BBNF | KEEP AS-IS |
| W5 (close matrix) | 5-bench fat-LTO + competitor benches + FINAL + BA/BB/BC handoff | **VALID but misplaced.** After path B + Q2 + Q3 close, W5 is the tranche-close ceremony, not a work wave. Its "2 serial agents" sequencing is correct; the 5-bench fat-LTO run depends on W0-W4 closed under Q1's chosen path | KEEP AS-IS (close ceremony unchanged) |

### 6.2 Scope-reveal: W0 re-opens as W0'

W0's spec predates path B. §W0.b threads `value_builder` through
every shape emitter (path A); path B collapses threading into the
tape crate — §W0.b emitter bounds shrink (shapes unchanged) and
§W0.c's `value_builder.rs` bounds disappear (file deletes).

Per SPEC §Mid-tranche plan pivots: **plan pivot within AY-II,
not a new letter.** Thesis unchanged; W0 decomposition
re-sequences. **W0 re-opens as W0' under path B** rather than
close-with-recorded-miss. Under path A the same re-open applies
at higher LOC cost.

## 7. Q6 — Predecessor + successor alignment

### 7.1 AY-I hard gates routed to AY-II

| Gate | AY-I | AY-II | Post-path-B |
|---|---|---|---|
| Canonical packed substrate + direct JSON write | partial | W0 | CLOSES (fused builder collapses tape + value writes) |
| `view() / to_value() / get()` unified | tape-walk survives | W0 | CLOSES (no walk) |
| Direct-to-struct + Pratt lowering | 71 admissions | W0 | CLOSES (Q3 wires materializer/admission) |
| Every surface has consumer | navigate_tape DEAD | W0 retires | CLOSES (Q2 + Q3) |
| twitter ≤ 1.15×; canada/citm/geomean ≤ 1.20 | 3.995× MISS | W1 | W1 gate; path B enables |
| CSS / Sheets functional | PANIC | W0 | CLOSES (fused rollback_to) |
| Structural scan first-class | no consumer | W0 retires | CLOSES (Q2) |
| B0 closes | PASS | — | unchanged |

### 7.2 BA / BB / BC openers

BA's thesis (`BA.md:14-17`) — visitor-lane default `to_value()`,
unified emission, single-pass finaliser, first-class
`rollback_to` — is exactly what path B delivers. BA opens
cleanly. BB is runtime-agnostic (generated.rs regen-discipline
is orthogonal). BC opens transitively on BA close. No successor
orphaned. Path A: same at higher LOC. Path C breaks BA's
visitor-lane invariant.

## 8. Proposed forward-path wave schedule

The AY-II wave schedule re-sequences under path B. W0 **does not
close on its current landed state**; it re-opens as W0' with the
path B decomposition. W1-W5 keep their theses; W0's internal
contract shifts.

| Wave | Name | Agents | Thesis | File bounds |
|---|---|---|---|---|
| **W0'** | Fused builder collapse + consumer closures | 3 parallel (W0'.a fused builder + W0'.b consumer closures + W0'.c projection-materializer wiring) | FusedBuilder<R> absorbs ValueBuilder; every shape emitter is unmodified at signature; `__path_walk` emission consumes `STRUCTURAL_SCAN_POLICY`; every `materialize_projection_*` gains a call site via `project_frame_<Grammar>` dispatch | `crates/tape/src/builder.rs`, `crates/tape/src/columns.rs` (new ValueColumn), `crates/core/src/runtime/{parsed.rs,value_builder.rs}` (delete value_builder.rs), `crates/core/src/backend/rust/emitter/grammar.rs` (parse-entry alloc), `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs` (project_arm routes through materializer), `crates/core/src/backend/rust/view/value.rs` (__path_walk consumer), `crates/core/tests/projection_totality.rs` (tightened grep), `crates/core/src/grammar/generated.rs` (orchestrator regen at close) |
| **W1** | JSON — semantic parity + peer-referenced perf | 4 parallel (unchanged from current W1.md) | Peer-referenced JSON close; `bbnf_value_twitter ≤ 1.15× sonic`; competitor bench vs sonic-rs + simd-json | unchanged |
| **W2** | CSS L4 — lightningcss total typed parity | 5 parallel (unchanged) | Every lightningcss AST surface → grammar-derived `CssL4Parser<X>Projection` + materializer; lightningcss_parity + css_l4_canonical_parity + typed_accessor_surface green; bbnf CSS ≤ cssparser, ≤ 2× lightningcss | unchanged |
| **W3** | Sheets — grammar-derived typed formula families | 3 parallel (unchanged) | Sheets projection totality; parse_nested no-panic; samply fused-pipeline attribution | unchanged |
| **W4** | BBNF — self-hosting identity + grammar-meta typed surface | 5 parallel (unchanged) | Bootstrap cycle-1 == cycle-2; BBNF projection totality; `@pretty` repair; bbnf_self samply | unchanged |
| **W5** | Cross-grammar close matrix + FINAL + BA/BB/BC handoff | 2 serial (unchanged) | Full fat-LTO 5-bench + competitor benches (sonic-rs, simd-json, lightningcss, cssparser); AY-II FINAL | unchanged |

### 8.1 W0' hard gates (peer-referenced)

1. `nm` on bench binary: zero `push_compound`, `note_push`,
   `navigate_tape`, `ValueBuilder`; `FusedBuilder::*` present.
2. `grep -c 'materialize_projection_\w+_JsonParser\s*(' target/expand/ay-json.rs` equals
   `PROJECTION_DIRECT_TO_STRUCT.len()` (Q3 runtime truth).
3. `grep 'cursor.object_key_seek\|bounded_lookahead\|scan_structural_bounded' target/expand/ay-json.rs` > 0
   where policy admits (Q2).
4. 5-bench fat-LTO clean (CSS + Sheets no panic).
5. Bootstrap cycle-1 == cycle-2.
6. Samply JSON twitter: fused symbol hot; zero
   `parse_with_visitor` on `to_value()` graph.
7. `bbnf_value_twitter / sonic_value_twitter ≤ 1.20` spot (W1
   tightens to 1.15).

### 8.2 No scope reveal to a new letter

Path B is SPEC §Mid-tranche plan pivot. Thesis unchanged, W0
mechanism refines. Path A: same result at higher LOC. Only path
C triggers a new-letter scope-reveal (contradicts §1).

## 9. Summary table

| Q | Verdict | One-line justification |
|---|---|---|
| Q1 | **Path B (FusedBuilder collapse)** | Type-level absorption over per-site threading; one samply owner; zero emitter churn; preserves AY-II §1 invariant |
| Q2 | Emission-time splice at `__path_walk` in `view/value.rs` | Policy is codegen data; consumer is `lookup_scan_policy` at emit time; cursor primitives already ready |
| Q3 | Route `project_frame_<Grammar>` arms through `materialize_projection_*` per admission | Makes the 69 emitted materializers live consumers; tightens totality from structural count to runtime call-count truth |
| Q4 | (a) + (c) | Leave transient-stub history; forward waves compose without a stub (orchestrator dispatch ceremony) |
| Q5 | W1-W5 thesis unchanged; W0 re-opens as W0' | Path B is a plan pivot, not a letter split — thesis consistent, mechanism refines |
| Q6 | Every AY-I hard gate routes to AY-II under path B | BA / BB / BC openers align with path B without edit; BA's opening thesis is exactly what path B delivers |

### 9.1 Peer-referenced close targets (reminder)

- JSON: `bbnf_value_twitter / sonic_value_twitter ≤ 1.15`; geomean
  `≤ 1.20`; within 2× simd-json.
- CSS L4: `bbnf_css / cssparser ≤ 1.00`; `bbnf_css / lightningcss
  ≤ 2.00`; full lightningcss AST admission parity.
- Sheets: self-parity + cyc/byte non-regression vs AY-I.W6.
- BBNF: cycle-1 == cycle-2 + `bbnf_self ≥ 98 MB/s` non-regression.

End of AY-II-AUDIT-C.
