---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation (SK-V18 totality)
cycle: V8
disposition: REVISE (4 residual precision anchor nits; no reject)
verification_head: dirty working tree at master (post 097c4dd90)
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

# CH1 — CORRECTNESS (SK-V18 T-P1 V8)

## Verdict

REVISE. Four residual precision anchor nits; zero REJECT.

My lens is citation correctness: every spec-claim<->impl row must resolve — the
spec path:line carries the claimed text, the impl path:line carries the claimed
symbol, the verdict matches the evidence; 1D RESULTS/REDRESS citations resolve to
real ledger entries; no recalled LOC/symbol; and (the surface this cycle drives)
every INTRA-INVENTORY cross-reference anchor lands on the row/item it names. I
read all eight inventories and independently re-grounded the most load-bearing
external spec<->impl rows on disk. Every EXTERNAL citation I tested matches disk
verbatim. The four findings below are INTERNAL navigation anchors (an inventory
citing another inventory's row/item by path:line) that drifted off their target
by a row or a section — single-locus path:line nits that would route a T-P2
reader to the WRONG row. They are the residual the V6/V7 clean passes did not
independently probe (those passes verified the external spec<->impl rows and the
REDRESS/RESULTS ledger, but did not test the U-4 / 1A-SUB-012 / EventTape-sibling
self-citation anchors).

Per the corrected REJECT convention: a reject is admissible ONLY when an
inventory STATES SOMETHING FALSE ON DISK with a live falsifying path:line proving
the inventory wrong about CODE/SPEC. None of the four below is that: no inventory
mis-states a code symbol, LOC, md5, lock text, or ledger row. They mis-aim an
intra-doc anchor. The disk-truth content each row asserts is correct; only the
"see <inventory>:<line>" pointer is off. That is a REVISE, never a reject. The
honest tally is reject=0.

## Load-Bearing External Rows Re-Verified On Disk (V8 — all ACCEPT)

### 1A (substrate)
- `tape/mod.rs:94` = `pub struct Tape<'input> {`; `:170` = `pub fn id(&self) -> TapeId {`;
  `:175` = `pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind, G: EventGrammar = AnyGrammar> {`;
  `:178/:179` = `_kind`/`_grammar` PhantomData fields; `:191` = the impl block;
  `:227` = `pub trait DocumentView<'a> {`. All verbatim.
- `ir/src/lib.rs:340-346` = the five-shape `BackendShape` (EagerTape/OffsetTape/
  EventTape/SinkOnly/CollapsedStage); `:211-237` = `ExprKind` = exactly 8 variants
  (Seq, Alt, Repeat, Optional, Literal, Regex, Ref, Annotation) — 1A-SUB-009's "8
  variants, no Predicate/Lookbehind/Call/LayoutDirective/ErrorDirective" exact;
  `:355-391` = `BackendExpr` = exactly 13 variants (Entry…Return) with `SimdScan`
  SEPARATE as `Recognizer::SimdScan` (1A-SUB-010 / 1A-DIV-002 exact). `cost.rs:57`
  = `pub enum SubstrateTarget`. All verbatim.
- 1A-SUB-025 / 1A-LOCK1-AMEND-001 contradiction REAL on disk: `LOCKS.md:620`
  carries verbatim "The `G:EventGrammar` type parameter is the generality
  vehicle"; `sk-v18/SPEC.md:1202-1207` carries the G4a phantom-`<G>` DELETE with
  the K-axis PRESERVED ("the K-axis … is the REAL Kind axis and is PRESERVED");
  `:1254-1257` carries G4.2-conjunct-4 "No second substrate (Lock 1)". The lock
  NAMES `<G>` as the generality vehicle while SPEC DELETEs it — catalogued
  correctly.

### 1B (codegen)
- `passes/src/lib.rs:329 pub mod recognizers`, `:392 pub fn derive_backend_shape`,
  `:473 fn choose_backend_shape`; `lower/mod.rs:18-26` 5-arm `match cost.chosen`
  over the five shapes; `grammar_provider.rs:40-42 enum RuntimeEmitterKind{
  CompiledLowering,RequestFacts}`; `runtime_generator.rs:701 const CSS_GENERATED_RS:
  &str = r#"` … `:1611 "#;`; `backend_egraph.rs:9 const REWRITE_SET =
  "sk-v15-w7-direct-sink-normalization-v1"`. All verbatim.
- Four scaffold lowerers `eager_tape/offset_tape/event_tape/collapsed_stage.rs`
  each EXACTLY 17 LOC; `collapsed_tape.rs` = No such file (CH1-V2-F2 holds);
  `lower/mod.rs:1 pub mod collapsed_stage;`; `tape_plan.rs:58 pub(super) fn
  render_rule`; `sink_only.rs:122 pub fn lower_program`. `json_sink_direct.rs`
  render fns: `:124 render_value_dispatch(out: &mut String)`, `:251
  render_container_rules`, `:326 render_string_rule`, `:497 render_utility_rules`
  (all `&mut String`-only) vs `:457 render_number_emitter(out, name, prefix)`
  (reads program data) — D3 exact.
- `json_typed_direct.rs:668 struct DirectParser<'i> {`, `:671 cursor: usize,`,
  `:361` rollback `let checkpoint = parser.cursor;` — the 1A-DIV-006 / 1A-UNK-001
  re-anchoring exact. ARCH:1409 cites `lib.rs:401-408` while the enum is at
  `:340-346` (D4 stale-line claim true); ARCH:1135 names
  `passes::recognizers::derive_backend_shape` (exact).

### 1C (runtime)
- 7× `css_l4_*/generated.rs` md5 = `b654562ccff46ed62dd48e9ace325830` (all 7
  identical, git-modified yet byte-stable). 67 `@generated` markers in
  `crates/core/src/runtime`. Pattern-H census 71 (per-grammar 67 + 4 `tape/`).
  `css_l4_declaration_values/generated.rs:257` = "Holds exactly the existing
  `Tape` — no second substrate." `css_types.rs` = 66 LOC, `:1` = "//! Host shims
  for the CSS L4 grammar's `-> parse_hex_color(...)` map." `css_l4/builder.rs` =
  817 LOC. All exact.

### 1D (skinny lessons) — RESULTS/REDRESS resolution (lens-critical)
- `RESULTS.md:5-25` cold Track-1 rows verbatim: twitter parse_only 8349.290 >
  sonic 4913.095; citm_catalog 9079.838 > 8335.772; canada 16709.901 > 12970.929;
  apache_builds +1.4% (the headline lower bound); every cited row carries "per-iter
  equality PASS". Real measured ledger rows.
- `REDRESS.md:742` = item 51 "SK-V5 event-cursor redress: byte-class whitespace
  cursor is REJECTED."; `:769` = item 52 "SK-V5 baseline reassay after the
  event-cursor rejection." (correctly carved out as NON-reject); `:784` = item 53
  "SK-V5 structural-mask parser-local cursor is REJECTED." Item-51/52/53 reject-span
  distinction resolves correctly; item 52 NOT conflated into the reject span.
- Live witnesses: css `emit_full_parse input_fnv64`/`fnv64` at css
  `generated.rs:393,:394,:899`; production caller `css … parser.rs:42
  generated::emit_full_parse(input)`; harness `css_cold_harness.rs:131 fn
  track1_full` (`:130` is the comment; path carries `/bin/`); `parse_w11_1_number`
  ×7 in `json/generated.rs`; `bbnf-simd/src/lib.rs:94 pub fn parity_hash(...) ->
  [u8; 32]`. All verbatim.

### 1E (locks)
- 16 numbered lock headings resolve to EXACTLY the cited lines: 75, 160, 170, 179,
  181, 183, 200, 202, 260, 269, 319, 328, 336, 349, 436, 453.
- L14 self-gate falsification (D-1E-V5-14) TRUE on disk: `LOCKS.md:349` carries
  the verification command asserting `rg … crates/{ir,…}/src/` "returns ZERO";
  live `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/ir/src/
  crates/analysis/src/` = 13 (strategy.rs 9 + grammar_facts.rs 1 + scalar.rs 1 =
  11 ir; ast_utils/mod.rs 2 analysis). The lock's OWN gate is RED. x86 cost row
  (D-1E-V5-04): skinny-prefixed `find` = 28 files / 4401 LOC, exact;
  `lock14_baseline.rs:2409 GENERIC_SCAN_ROOTS`, `:2442
  SKV15_W2_EXTRA_COVERAGE_ROOTS`, `:2463 ("crates/bbnf-simd/src/x86_64",
  "diagnostic-x86")` exact.

### 1F (coherence + auxiliaries)
- The 9 `idents` rows in `strategy.rs` resolve to EXACTLY `:137,:143,:149,:155,
  :161,:167,:173,:179,:185`; the narrow 4-name regex catches only the 4 at
  `:137,:143,:149,:155` (Json/GoogleSheets/CssL4/Bbnf) — the "catches only **4**"
  wording is correct; the other 5 (Csv/Math/Bnf/Ebnf/CssPretty) escape. Consumer
  `for_grammar_with_manifest(…, PRODUCTION_MANIFEST_TABLE)` at `:216`. God-file LOC
  all EXACT: report.rs 11863, `src/bin/gate.rs` 6175 (sibling `src/gate.rs` 545 —
  `/bin/` disambiguation correct), lock14_baseline.rs 5095, runtime_generator.rs
  1611, codegen/lib.rs 1473, nonjson_css_l4.rs 3737. `codegen/lib.rs:4-5 mod
  json_sink_direct; / mod json_typed_direct;`; `simd-scan/src/lib.rs:68 pub use
  index::{StructuralIndex, next_structural_at_or_after};`; OnceCell
  `crates/core/src/grammar/generated/json.rs:701/:719/:732`. All verbatim.
- 1F-past-corpora R16: `regen.rs:5 #[derive(Clone, Copy, Debug)]`, `:6 pub(crate)
  struct RuntimeTarget`, `:9-10` entry_rule/source_roots, `:17-18`
  frontend_requirements/output_labels — exact (the V1-inherited `:17-18`=entry_rule
  false claim correctly repaired in prior cycles). `measure_mbps` at src/
  `nonjson_css_l4.rs:3091` (benches/ sibling 318 LOC, :3091 out of range) — exact.

## Residual Precision Findings (the V8 REVISE surface)

All four are INTRA-INVENTORY navigation anchors that drifted off-target. The
on-disk content each row asserts is correct; only the cross-reference path:line is
wrong. Each would route a T-P2 reader to the wrong row.

| id | disposition | severity | finding | evidence |
|---|---|---|---|---|
| CH1-V8-R01 | REVISE | low (misroutes) | The U-4 self-citation anchor is wrong in FIVE places. Multiple rows label the CSS-directional re-lock UNKNOWN as `U-4 \`1D:199-203,228-232\``, but on disk U-4 ("load-depressed absolute Mbps re-lock") lives at `1D-skinny-lessons.md:239-243`; `1D:199-203` is the G-3 digest row, `1D:228-232` is U-2's verify_action. A T-P2 reader following `1D:199-203` lands on G-3, not U-4. **Correction:** in `1D-skinny-lessons.md:67`, `:187`, `:205`, `1F-coherence-scan.md:87`, and `1F-past-corpora.md:27`, change every `U-4 \`1D:199-203,228-232\`` / `U-4 \`1D:199-203\`` to `U-4 \`1D:239-243\``. | live `rg '^- \*\*U-4' 1D` = `:239`; `sed -n '199p' 1D` = "\| G-3 \| PROVED"; `sed -n '228,232p' 1D` = U-2 verify_action; the mis-anchor appears at 1D:67,187,205 + 1F-coherence:87 + 1F-past:27. |
| CH1-V8-R02 | REVISE | low (misroutes) | The 1B EventCursor-fence cross-cite names the wrong 1A line. `1B-codegen-evidence.md:56` (OffsetTape) and `:57` (EventTape) both read "Cross-cite … `1A-substrate-evidence.md:75` (1A-SUB-012)", but 1A-SUB-012 (the "any future typed-event cursor must not revive EventCursor sidecars…" REDRESS fence) is at `1A-substrate-evidence.md:84`; `1A:75` is the 1A-SUB-003 row (tape+direct-to-struct one substrate family). The parenthetical row-ID "(1A-SUB-012)" is right; the line `:75` is wrong. **Correction:** in `1B-codegen-evidence.md:56` and `:57`, change `1A-substrate-evidence.md:75` to `1A-substrate-evidence.md:84`. | `sed -n '84p' 1A` = "\| 1A-SUB-012 \| `…:2052` typed event cursor…REDRESS fence: any future typed-event cursor must not revive EventCursor sidecars…"; `sed -n '75p' 1A` = "\| 1A-SUB-003 \| …one substrate family." |
| CH1-V8-R03 | REVISE | low (misroutes) | The 1B item-52 EventTape-sibling anchor points at the EagerTape row. `1B-codegen-evidence.md:56` (OffsetTape) reads "`:769-783` is item 52, a non-rejected profiling reassay carved out per CH3-V3-005, matching the EventTape sibling at `:55`)", but `1B:55` is the EagerTape row (zero REDRESS-fence/item-5x content); the EventTape sibling (which carries the item-51/53 fence + `742-813` span) is at `1B:57`. **Correction:** in `1B-codegen-evidence.md:56`, change "the EventTape sibling at `:55`" to "the EventTape sibling at `:57`". | `sed -n '55p' 1B` row-id = `EagerTape` (no `742`/`784`/item-5x); `rg '742-768\|CH3-V2-004' 1B` = `:56`,`:57` only; the EventTape row is `1B:57`. |
| CH1-V8-R04 | REVISE | low (misroutes) | 1D propagates the same `:55`/`:57` EagerTape-vs-EventTape swap into a cross-cite. `1D-skinny-lessons.md:170` reads "Reconciles with `1B:55` CH3-V2-004: 1B widened the 51∪53 PAIR span to cover both rejects", but the CH3-V2-004 PAIR-span-widening text lives in the EventTape row at `1B:57`, not the EagerTape row at `1B:55`. **Correction:** in `1D-skinny-lessons.md:170`, change `1B:55` to `1B:57`. | `rg 'CH3-V2-004\|51∪53 PAIR\|742-813' 1B` resolves to `1B:57` (EventTape); `1B:55` = EagerTape, no PAIR-span text. |

Note on R02/R03/R04 grouping: R03 and R04 share one root cause (the `:55`/`:57`
EagerTape-vs-EventTape swap) but land in two different files at two different
loci, so each carries its own one-line correction. R02 is a distinct
(`:75`/`:84`) drift in the same 1B rows. They are enumerated separately because a
single batch fix must touch each locus by hand.

## REJECT Gate

No GENUINE reject. Per the corrected V6/V7 convention, a REJECT requires an
inventory to STATE SOMETHING FALSE ON DISK about code/spec with a live falsifying
path:line. The four findings above are wrong INTRA-DOC navigation anchors, not
false disk-state claims: in each case the row's substantive assertion (the U-4
caveat, the EventCursor REDRESS fence, the item-52 carve-out, the 51∪53 PAIR
span) is CORRECT on disk; only the "see <inventory>:<line>" pointer drifted. Every
external spec text, impl symbol, LOC, md5, lock heading, and REDRESS/RESULTS
ledger row I tested matches disk verbatim. No suspicion I raised about code/spec
content survived probing — they all resolved for the inventory. reject=0.

## Disposition

CH1 returns REVISE at V8 with four residual single-locus anchor nits
(CH1-V8-R01..R04) and zero reject. These are the precision drift the prior two
clean passes (V6/V7) did not independently probe — they verified the external
spec<->impl rows and the REDRESS/RESULTS ledger but not the U-4 / 1A-SUB-012 /
EventTape-sibling SELF-citation anchors, which is where the residual lived. The
external citation surface my lens owns is otherwise verbatim-clean across all
eight inventories. Once R01-R04 land (six one-line path:line edits across 1B, 1D,
1F-coherence, 1F-past-corpora), CH1 should re-converge to the 2-consecutive-clean
fixed point.

TALLY accept=8 revise=4 reject=0
