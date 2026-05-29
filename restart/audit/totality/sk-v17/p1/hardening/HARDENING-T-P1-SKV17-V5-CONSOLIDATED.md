# SK-V17 T-P1 Excavation — V5 Hardening Consolidated

```yaml
pass: T-P1-SKV17-excavation
cycle: V5
role: aggregator
generated_at: 2026-05-29T21:10:00Z
master_head: 445925167154de73540e3ea3283d0170371de790
contract: restart/prompts/totality/PASS-1-EXCAVATION.md (§2 scope matrix, §3 CHALLENGE, §3Z) + ORCHESTRATOR §3W/§3Z
hardening_root: restart/audit/totality/sk-v17/p1/hardening/V5/
inventories_consolidated:
  - restart/audit/totality/sk-v17/p1/1a-substrate-evidence.md
  - restart/audit/totality/sk-v17/p1/1b-codegen-evidence.md
  - restart/audit/totality/sk-v17/p1/1c-runtime-evidence.md
  - restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md
  - restart/audit/totality/sk-v17/p1/1e-locks-evidence.md
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
cycle_accept_rate_history:
  V1: 0.0
  V2: 61.9
  V3: 85.3
  V4: 93.3
  V5: 98.7
converged: false   # ≥95%×2-consecutive NOT met (V4 93.3% < 95%); V5 is the V≤5 ceiling
disposition_counts_v5:
  accept: 75
  revise: 1
  reject: 0
  total: 76
residual_revise: [CH1-V5-001]
next_move: ready-for-T-P2 (G1 clean-final pin; governance exception recorded)
```

## §3Z Verdict

**Cycle accept-rate: 75 / 76 = 98.7%.** One REVISE (CH1-V5-001), zero REJECT, zero
open critical defect, zero orphan unresolved REVISE.

**Convergence: NOT met by the strict §3Z two-consecutive rule; pass advances by
clean-final G1 pin.** §3Z requires ≥95% ACCEPT across **two consecutive** cycles.
V5 clears the bar at 98.7%; V4 did not (93.3% < 95%). The two-cycle window
V4→V5 therefore fails the consecutive test, and `converged=false` is recorded
honestly. V5 is simultaneously the **V≤5 hard ceiling** (§4) — no V6 confirming
cycle is legal. Per `PASS-1-EXCAVATION.md` §6 G1 is an optional convergence pin;
under the active orchestrator pin (non-G-Omega gates auto-pass), T-P1 closes as a
**clean-final, G1-pinned** advance. This is the identical governance posture the
SK-V14 totality T-P1 closed under (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`),
and the exception is surfaced here for T-P2/T-P3 + Pass Omega rather than hidden
as a normal §3Z cohort lock.

**The single residual REVISE is non-load-bearing and isolated to one frontmatter
shorthand string** (CH1-V5-001, §"Residual REVISE" below). Every load-bearing
substrate / value-API / BackendShape / NEON fact re-resolved verbatim at master
`445925167` across CH1/CH3/CH5/CH6/CH7. The 5.4-point V4→V5 jump came from
folding the two V4 math.rs REVISEs (1C/1D) and the four V5-localized author
REVISEs (1A Gaps-row Lock-14 relabel; 1B symbol-anchor; 1E LAC-04 path-(b) +
CH5-S8 REDRESS-53 anchor) — all live-re-verified, no new ground-truth slip.

### Per-lens dispositions (V5)

| Lens | ACCEPT/total | Rate | Output | Headline |
|---|---|---|---|---|
| CH1 CORRECTNESS | 7/8 | 87.5% | `V5/CH1.md` | Both V4 math.rs REVISEs folded + live-verified; ONE new frontmatter-glob defect (CH1-V5-001) — body cites the file correctly. |
| CH2 GENERALITY | 14/14 | 100% | `V5/CH2.md` | First zero-REVISE GENERALITY cycle; the V4 "@generated not hand-written" sweep landed across all 7 inventories; predicted third instance did not materialise. |
| CH3 REGRESSION | 8/8 | 100% | `V5/CH3.md` | No pre-blocked route re-opened; D6 inversion guard holds; two V5 deltas (1B symbol-anchor, NEON-general claim) clean. Second consecutive 100%. |
| CH4 COST | 23/23 | 100% | `V5/CH4.md` | V4 LAC-04 path-(b) `LayoutFacts` mispricing folded (`grep -rn LayoutFacts crates/`=0); all 6 LACs carry wave-hint + path:line. Second consecutive ≥95%. |
| CH5 HIDDEN COUPLING | 8/8 | 100% | `V5/CH5.md` | V4 CH5-S8 REDRESS-53 anchor re-pointed to :577/:825/:839; Lock-1 union firewall clean; one unwired tape, no parallel substrate. |
| CH6 ANTI-PAPER-CLOSE | 8/8 | 100% | `V5/CH6.md` | Tape honestly UNWIRED everywhere; zero deferral-pattern hits; every UNKNOWN carries verify_action. Second consecutive ≥95%. |
| CH7 OVERFIT-PRUNE | 8/8 | 100% | `V5/CH7.md` | The two folded V4 REVISEs are over-credit *removals* in CH7's direction; BSHAPE17-009 NEON-already-general resolved without contrivance. Third consecutive ≥95%. |

The CHALLENGE wave is **not paper-close**: cycle V1 ran 0.0%, the wave traversed
61.9 → 85.3 → 93.3 → 98.7 with substantive REVISE in every cycle through V4, and
CH1 alone still carries a live REVISE at V5. The ≥30%-REVISE V1 expectation was
exceeded early; the monotone climb is the fold-discipline working.

---

## The LOCKED Excavation — implemented-vs-spec divergence inventory

The excavation maps the **V1 greater spec** (`restart/ARCHITECTURE.md`,
`restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md`) and the **SK-V18 fold target**
(`crates/core`) against the **SKINNY-proven model** (`skinny/crates/`, SK-V17 SPEC).
Every row resolves at file:line / SHA `445925167`; this inventory is **LOCKED** at
V5 (75/76 ACCEPT, the lone REVISE non-load-bearing). Census across the six
inventories: spec-claims-implemented **10**, spec-claims-unimplemented **25**,
impl-exceeds-spec **11**, unknown **10**; LOCKS-amendment candidates **6** (1E).

### A — Tape substrate (SoA-proven vs AoS-in-core, UNWIRED)

| Surface | SKINNY-proven (master 445925167) | crates/core fold target (V1 spec / impl) | Verdict |
|---|---|---|---|
| Tape shape | SoA `Tape<'input>` six members — `skinny/crates/runtime/src/tape/mod.rs:94-100` (`source: &'input [u8]`, `offsets: Vec<u32>:96`, `flag_cursors: Vec<u32>:97`, `flag_values: Vec<u8>:98`, `payloads: PayloadArena`, `id`); sparse position-keyed side-vectors, `from_offsets` asserts `flag_cursors.len()==flag_values.len()` `:104-117` | AoS `TapeRec` `#[repr(C,align(4))]` `crates/core/src/runtime/tape/record.rs:102`, struct `:103`, const-asserted 16-byte + align-4 `:120-121`; `begin_compound(&StructLayout)` reads only `(layout.rule_id & 0x1F)` `tape/mod.rs:185-186` | **cross-tree divergence** — both admitted offset-tape shapes under Lock 1 `LOCKS.md:75`; AoS↔SoA is the SK-V18 fold-convergence question, NOT a same-tree second substrate |
| Wiring | live; JSON/CSS recognizers beat lightningcss 2-3× (SK-V17 RESULTS) | `TapeStructBuilder` `tape/mod.rs:58` is UNWIRED — `grep -rn TapeStructBuilder crates/core/src \| grep -v runtime/tape/` = **0** (sole non-tape hit = `number.rs:17` doc-comment); live substrate is eager `JsonStructBuilder::new()` (`json/parse_with.rs:11,34`) | **spec-claims-implemented** (§0.1.11 "exists-but-unwired" verified true) |
| Lock-1 closure | — | exactly-ONE-encoding survives post-fold (`LOCKS.md:75` "parallel substrates are dead"); AoS/SoA coexistence admissible ONLY as transient fold-state; dual end-state = Lock-1 violation (`1a:93,105,148`; `1e` LAC-1E-SKV17-01) | **catalogued invariant** for T-P2 to discharge |

### B — Eager OpenFrame builder (the load-bearing divergence)

| Surface | SKINNY-proven | crates/core fold target | Verdict |
|---|---|---|---|
| CSS builder | flat-tape commit-by-construction; materialization gap is the only delta vs lightningcss | eager `OpenFrame` god-module — `css_l4/builder.rs:16` (`OpenFrame` `:16`), **817 LOC**, six `pending_*` Vecs `:74-79` + one `pending_value: Option<CssTypedValue<'p>>` `:71` (= 7 pending fields, none `Vec<Vec>`) | **AZ-IV eager pre-block fold-DELETION target** (SPEC `:791`); replace with lazy `ValueRef<G>`, never carry forward |
| JSON builder | — | eager `OpenFrame` — `json/builder.rs:9/16`, **231 LOC** | fold-deletion target |
| Value tree | lazy `ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>` `tape/mod.rs:175`; `value_from_ref<'doc,'input:'doc>` `grammars/json/value.rs:143` over `ValueRef<'doc,'input>:144` | eager per-grammar `CssTypedValue<'p>` `css_l4/value.rs:414`; NO `value_from_ref`/`ValueRef` in core (`grep` = **0** in both `json/value.rs` + `css_l4/value.rs`) | **spec-claims-unimplemented** — the lazy projection is the durable fold |

### C — Value-API (per-grammar eager vs lazy-ValueRef<G> generalization)

| Surface | SKINNY-proven | crates/core fold target | Verdict |
|---|---|---|---|
| Generation provenance | — | per-grammar value APIs are `@generated by xtask regen-{json,css}` (`json/value.rs:1` = `// @generated by xtask regen-json; do not edit by hand.`, `css_l4/value.rs:1` = `regen-css`) — **EAGER, NOT hand-written; Lock-14 ALLOWED** (hand-written would be the Lock-14 VIOLATION) | **the divergence is the EAGER materialization shape, not the generation provenance** |
| Generalization | one grammar-parametric `ValueRef<G>` projection | per-grammar eager typed enums + `JsonChildrenIter` (`json/view.rs:39`) / `CssChildrenIter` (`css_l4/view.rs:44`); generic `runtime/{tape,view.rs}` carry no grammar names | **spec-claims-unimplemented** — fold lifts per-grammar emit to lazy `ValueRef<G>` |

### D — BackendShape 5-shape canon (Lock 10) + decision engine

| Surface | SKINNY-proven | crates/core fold target | Verdict |
|---|---|---|---|
| Shape enum | `pub enum BackendShape {` `skinny/crates/ir/src/lib.rs:340`; `derive_backend_shape(` `skinny/crates/passes/src/lib.rs:392,401` | `grep -rn 'enum BackendShape' crates/` = **0**; `grep -rn derive_backend_shape crates/` = **0** | **spec-claims-unimplemented in fold target** — selector lives only in skinny |
| Emit strategy | — | single-variant `EmitStrategy::StructDirect` `strategy.rs:104`, `is_struct_direct` `:224`; `PRODUCTION_MANIFEST_TABLE` `:134` with exactly 9 `ManifestStrategyEntry` rows (`:136,142,148,154,160,166,172,178,184`) | the SinkOnly/struct-builder lineage to absorb the 5-shape canon |
| Lowerers | — | four 17-LOC scaffold lowerers `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape}.rs` + `collapsed_stage.rs` (`lower_rule` at `:15-17`), vs 270-LOC `sink_only.rs` | catalogue (4-scaffold + sink_only) |
| CollapsedStage | x86/AVX-512-pinned | 5-shape canon `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` `LOCKS.md:107-108`; CollapsedStage NOT-ADMITTED x86-only, aarch64 candidate = **UNKNOWN-2D-05** `ARCHITECTURE.md:1206` (prose `:1088`/`:1109`) | **unknown** (spec-named open, not a fresh gap); NEON sits under the four LLVM shapes' scan-leaf FFI; no 6th shape |
| Decision engine | — | `crates/egraph` + `crates/csp-solver` already present (root `Cargo.toml`); fold WIRES `backend_egraph` (311 LOC) + `decision_csp` (273 LOC), does not build them | scope-pressure / fold-wiring |

### E — NEON shared classifier (JSON-only narrative vs shared-classifier reality)

| Surface | SKINNY-proven | crates/core fold target | Verdict |
|---|---|---|---|
| Classifier select | `select_classifier(alphabet: &'static [u8;64]) -> SelectedClassifier` `skinny/crates/bbnf-simd/src/dispatch.rs:42` | `scan_structural(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex` `crates/simd-scan/src/lib.rs:80`; `KernelShape::select(alphabet)` `alphabet.rs:118`; richer `StructuralAlphabet` {`singletons:23`, `digraph_mask:[u64;4]:28`, `digraph_pairs:32`, `quote_classes:37`} `alphabet.rs:19-37` | **impl-exceeds-spec** — the classifier is ALREADY grammar-general (alphabet-as-data), NOT JSON-only |
| Scan wiring | JSON-wired; CSS is SK-V17 W3 first-mover | WIRED across **8 of 9** generated grammars (`grep -c scan_structural` = 1 each: json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf; call-sites json.rs:732, css_l4.rs:15982 …); `math.rs` = **0** (its `ScanState` `:287-289` holds only `nospace_bits`/`nospace_start`; the two `OnceCell<StructuralIndex>` mentions `:281,285` are doc-comments, NOT a field) | **impl-exceeds-spec**; spec narrative must absorb the alphabet-parametrised shared form (0-LOC fold) |
| Arch discipline | aarch64-NEON only (`neon.rs:47` baseline); avx2/avx512/wasm cfg-gated non-aarch64 | x86 bar at SPEC `:806`/`:258`; aarch64-only; the multi-arch `crates/simd-scan` is scope-pressure deferred to T-P2 | aarch64-only honoured; no x86 admission |

### F — StructRegistry / FieldSource per-leaf hot-path fence (the regression firewall)

| Surface | SKINNY-proven | crates/core fold target | Verdict |
|---|---|---|---|
| Registry | — | `StructRegistry` `struct.rs:313` (`layouts: BTreeMap<RuleId, StructLayout>:314`), `layout(rule_id)->Option<&StructLayout>:331`, `layout_by_name:337`, `insert:326` | — |
| Layout / FieldSource | — | `StructLayout` `struct.rs:202` (Lock-2-retired-name `LOCKS.md:160`, yet live; `grep -rn StructLayout crates/` = **960**); `FieldSource` enum `:84` | **L02 drift** (960-site rename surface; mis-priced ~8× by V2) |
| Hot-path fence | — | `begin_compound(&StructLayout)` takes a **pre-resolved** layout by reference reading only `layout.rule_id & 0x1F` `tape/mod.rs:185-186` — NO per-leaf registry lookup | **do-not-redrive fence** — a naive per-leaf `StructRegistry::layout(rule)` re-opens the 28-65×/983×/10583× regression (SPEC `:793-795`); the `FieldSource` walk MUST stay compile-time projection-emission |

---

## Load-bearing divergences T-P2 research must address (fold SKINNY tape model → V1 spec)

T-P2 grounds the fold of the SKINNY-proven unified-tape/lazy-view/projection +
aarch64-NEON model into the greater V1 spec. The excavation isolates **six**
load-bearing divergences (each a catalogued fold task, none a proposal — T-P2/T-P3
research and disposition):

1. **Eager OpenFrame → lazy ValueRef<G> (the materialization gap).** The 817-LOC
   CSS + 231-LOC JSON eager `OpenFrame` builders (`css_l4/builder.rs:16`,
   `json/builder.rs:9`) are the AZ-IV-pre-blocked fold-DELETION target; the proven
   lazy `value_from_ref`/`ValueRef<G>` (`skinny/.../tape/mod.rs:175`,
   `grammars/json/value.rs:143`) is the durable replacement. This IS the
   recognizer-vs-materialization gap SK-V17 proved. **Risk: high; ~300-700 LOC
   generator-side + per-grammar regen across 8 grammars; 22+ files touched.**

2. **AoS TapeRec ↔ SoA Tape convergence under Lock-1 exactly-one-encoding.** Core's
   16-byte AoS `TapeRec` (`record.rs:103`) and skinny's SoA `Tape` (`tape/mod.rs:94`)
   are both Lock-1-admitted offset tapes; T-P2 must name the single post-fold
   encoding (`LOCKS.md:75`), since a dual end-state is a Lock-1 violation, not a
   tree-local choice. **Risk: medium; 200-600 LOC; SK-V18.**

3. **Value-API generalization (per-grammar eager enums → grammar-parametric
   ValueRef<G>).** The divergence is the EAGER materialization shape, not the
   `@generated` provenance (Lock-14 honoured). T-P2 must establish the one
   `ValueRef<G>` projection generator that re-emits all 8 per-grammar value
   surfaces. **Risk: high; generator-LOC vs regen-LOC distinguished.**

4. **StructLayout / FieldSource hot-path fence (the regression firewall).** The fold
   MUST keep the `FieldSource`/`StructLayout` walk compile-time projection-emission;
   `begin_compound` already takes a pre-resolved `&StructLayout` (`tape/mod.rs:185-186`).
   A per-leaf `StructRegistry::layout(rule)` re-opens the worst measured regression
   (28-65×/983×/10583×, SPEC `:793-795`). **Risk: 0-LOC fence; CRITICAL/regression
   if violated.** Plus the Lock-2 `StructLayout`→canonical-name 960-site rename
   surface (`grep`=960).

5. **NEON shared-classifier spec narrative (impl-exceeds-spec).** The
   `select_classifier(alphabet)` / `scan_structural(input, &StructuralAlphabet)`
   form is already grammar-general across 8 grammars (`lib.rs:80`, `alphabet.rs:118`).
   T-P2 must fold the spec's JSON-scanner framing to the alphabet-parametrised shared
   form — a **0-LOC narrative fold**, not a build. Scope-reconcile the multi-arch
   `crates/simd-scan` against the aarch64-only proven set. **Risk: low/0-LOC fold +
   100-400 LOC scope reconcile.**

6. **BackendShape 5-shape canon absorption + aarch64 CollapsedStage UNKNOWN-2D-05.**
   The `BackendShape` enum + `derive_backend_shape` selector live only in skinny
   (`ir/lib.rs:340`, `passes/lib.rs:392`); the fold wires them into core atop the
   single `EmitStrategy::StructDirect`. NEON absorbs under the four LLVM shapes with
   NO 6th shape; the aarch64-CollapsedStage question is the spec-named UNKNOWN-2D-05
   (`ARCHITECTURE.md:1206`), for T-P2 research, not a fresh gap. **Risk: medium;
   60-200 LOC selector + 600-1400 LOC joint decision-engine envelope.**

### LOCKS-amendment candidates (1E — candidates only; disposition is T-P3 3C)

Six LACs carry supporting path:line + loc/risk/wave-hint: LAC-01 one-substrate
closure; LAC-02 no-per-leaf-lookup fence; LAC-03 OnceCell all-8 classification;
LAC-04 StructLayout reprice (path-(b) re-priced — `LayoutFacts` is skinny/
prior-totality-only, `grep -rn LayoutFacts crates/`=0); LAC-05 UNKNOWN-2D-05;
LAC-06 simd-scan multi-arch scope. The 16-lock count is fixed; no lock re-numbered.

---

## Residual REVISE

**One residual REVISE — CH1-V5-001 — non-load-bearing, isolated to a frontmatter
shorthand string.** 1B's `live_truth_method` brace-glob reads
`wc -l skinny/crates/codegen/src/lower/{eager,offset,event,collapsed}_tape.rs=17 each`;
the brace-glob expands `collapsed` to the **non-existent** `collapsed_tape.rs` — the
actual lowerer file is `collapsed_stage.rs`. The **17-LOC fact is true** and the
**body cites it correctly** (BSHAPE17-004 + Cross-Tree row + Do-Not-Redrive ledger
all cite `collapsed_stage.rs:15-17`). The defect is a frontmatter-glob imprecision
only; no body claim, verdict, or census is affected (4-scaffold + 270-LOC sink_only
stands). **Fix (for T-P2/T-P3 carry):** enumerate the real filenames —
`{eager_tape,offset_tape,event_tape,collapsed_stage}.rs = 17 each, sink_only=270`.

This is **not an orphan REVISE blocking advance**: it is recorded, the fix is
concrete, and it touches no load-bearing substrate / value-API / BackendShape /
NEON claim. No REJECT exists across any lens.

---

## Next move — ready-for-T-P2

**T-P1 SK-V17 advances to T-P2 as a clean-final, G1-pinned pass close.** The
excavation is LOCKED at 98.7% with the divergence inventory grounded at
file:line/SHA `445925167`, the six load-bearing fold divergences named, and the
single residual REVISE (CH1-V5-001) recorded as non-load-bearing with a concrete
fix.

**Governance exception (carry forward):** `converged=false` — the strict §3Z
≥95%×2-consecutive criterion is NOT met (V4 = 93.3% < 95%), and V5 is the V≤5 hard
ceiling, so no V6 confirming cycle is legal. T-P1 advances by the optional G1
convergence pin (§6) under the active orchestrator pin, mirroring the SK-V14
totality T-P1 close. T-P2/T-P3 and Pass Omega must preserve this note rather than
treat the advance as a normal two-clean-cycle §3Z lock.

**T-P2 entry binding:** fold the SKINNY-proven flat-tape / lazy-`ValueRef<G>` /
`StructLayout`-projection materialization + shared NEON `select_classifier(alphabet)`
classifier into the V1 spec, addressing the six load-bearing divergences above —
the monotonic direction is skinny-proven → `crates/core` (the SK-V18 fold target),
never the reverse. crates/core is the fold TARGET; skinny/crates is the proven
engine. aarch64 only. Lock 1 substrate-union + Lock 14 grammar-neutral + Lock 10
5-shape canon are load-bearing throughout; the StructRegistry/FieldSource per-leaf
fence and the AZ-IV/StructRegistry/fact-stream/x86/D6 pre-blocks (SPEC §9 `:789-857`)
are inviolate. The orchestrator updates `restart/HANDOFF.md` to **ready-for-T-P2**
and dispatches per `totality/PASS-2-RESEARCH.md`.
```
