# CHALLENGE CH4 — COST (cycle V1)

Lens: CH4 Cost. Per PASS-ALPHA §3 ("what is the LOC budget per intervention? Risk
classification? Wave alignment? Same-wave consumer present per intervention?") +
ORCHESTRATOR §3W lens set. Each candidate must carry: LOC budget + risk class +
wave alignment + same-wave consumer + scalar-ref + checkasm expectation.

Subject under bracket: CSS L4 typed parsing must BEAT lightningcss via the unified
tape/layout/projection model + dav1d-style aarch64 NEON hot leaves; preserve-rich-ast;
no x86; fully generalized for SKINNY, foldable into TOTALITY. aarch64 Apple M5 Max only.

Reviewed: `research/alpha/{alphaA..E}.md`, `SYNTHESIS.md`, `HANDOFF.md`.
Note: there is no `alphaF.md`; the α-F contract draft IS `SYNTHESIS.md` + `HANDOFF.md`
at tranche root (PASS-ALPHA §2 row α-F, §6 tree). CH4 brackets the candidate cost
surface in alphaE (where LOC/risk/wave/consumer/scalar-ref/checkasm live) and the
receiver-cost surfaces in SYNTHESIS §0.1/§0.3/§0.5 + HANDOFF.

---

## Verification performed (every disposition is grounded)

| Check | Result | Evidence |
|---|---|---|
| Benched CSS Track 1 lives in skinny tree | YES | `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:596` `track1_facts -> Result<String,String>`; bench crate `skinny/crates/bbnf-bench/Cargo.toml` |
| skinny has `runtime/src/tape/{assembler,mod,offsets,event_grammar}.rs` | YES | `ls skinny/crates/runtime/src/tape/` |
| skinny has NO `StructLayout` | CONFIRMED | `grep -rl StructLayout skinny/crates/` empty |
| `StructLayout`/`OpenFrame`/`begin_compound`/`match layout.rule_id`/`css_l4/builder.rs:274` live ONLY in core tree | CONFIRMED | `crates/core/src/runtime/css_l4/builder.rs:274` `fn begin_compound(&mut self, layout: &StructLayout)`; `match layout.rule_id` |
| skinny has NO `match rule_id` / `begin_compound` builder | CONFIRMED | `grep -rl "match rule_id\|begin_compound" skinny/crates/` empty |
| `TapeStructBuilder` lives ONLY in core | CONFIRMED | `crates/core/src/runtime/tape/{mod,record}.rs`; absent from skinny |
| checkasm tests live in skinny bbnf-simd | YES | `skinny/crates/bbnf-simd/tests/checkasm_{byte_class_from_table_64,ascii_set_member_find_64,bulk_emit_positions_64,bitmap_next_set_bit,byte_class_from_eq_set_64}.rs` |
| `PrimitiveKernels`/`select_classifier`/dispatch in skinny | YES | `skinny/crates/bbnf-simd/src/dispatch.rs`, `lib.rs` |
| `digit_mac.rs` orphan in skinny aarch64 | YES | `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs` |
| i8mm detection anywhere in skinny | NONE | `grep i8mm skinny/crates/` empty (alphaE claim confirmed) |
| `RuntimeEmitterKind::{CompiledLowering,RequestFacts}` is the skinny emit fork | YES | `skinny/crates/codegen/src/runtime_generator.rs:17,25`; `lib.rs:282,291` |
| 7× CSS generated.rs = 646 lines each (4522 total) | YES | `wc -l skinny/crates/runtime/src/grammars/css_l4_*/generated.rs` |
| `runtime_generator.rs`=1336, `grammar_provider.rs`=308, `nonjson_css_l4.rs`=3644 | YES | `wc -l` |
| `json/scan.rs`=337, `json/value.rs`=172 (the copy-model) | YES | `wc -l` |
| skinny `lower/` has `{offset_tape,event_tape,tape_plan,eager_tape}.rs` | YES | `ls skinny/crates/codegen/src/lower/` |

---

## The load-bearing CH4 finding (drives the SYNTHESIS dispositions)

**alphaE is path-correct; SYNTHESIS §0.3 is path-wrong.** alphaE §0 lines 37–51
explicitly diagnoses that the architecture doc cites the TOTALITY core tree
(`crates/core/...`, `StructLayout`, `OpenFrame`, `CssArena`) which is **NOT the
benched skinny surface**, and re-frames every candidate against skinny paths
(`runtime/src/tape/`, `lower/{offset_tape,event_tape,tape_plan}.rs`,
`bbnf-simd/src/dispatch.rs`, `nonjson_css_l4.rs`). Verified: those skinny paths all
exist and `grep "match rule_id"`/`StructLayout`/`begin_compound` in skinny is empty,
while all of them live in `crates/core/`.

SYNTHESIS §0.1 ("Layout-driven projection" row) and §0.3 (receiver goalset) did NOT
apply alphaE's correction. They instruct the receiver to:
- write the generator "in `crates/core/src/backend/rust/emitter/`" (§0.3 row 1) —
  the un-benched tree;
- "Rewrite `regen_css.rs emit_builder` to select `TapeStructBuilder`" and
  "DELETE the `OpenFrame` template + the `match rule_id` begin_compound" (§0.3 row 2);
- delete "`css_l4/builder.rs:274` ~40-arm match" (§0.1).

`TapeStructBuilder`, `OpenFrame`, `match rule_id begin_compound`, and
`css_l4/builder.rs:274` do **not exist in skinny** — they are core-tree artefacts.
A receiver executing §0.3 verbatim would refactor the un-benched core tree, burn
its LOC budget on a surface no bench row measures, and leave the actual benched
`RequestFacts` String path (`nonjson_css_l4.rs:596`, `runtime_generator.rs:25`)
untouched. This is a direct CH4 wave-alignment + owner-path-cost defect: the
SYNTHESIS owner paths are mis-targeted relative to the benched subject, and the
LOC budget would be spent in the wrong wave/tree. alphaE C0 already states the
correct surfaces; SYNTHESIS must be reconciled to alphaE, not the reverse.

---

## Dispositions (path:line + concrete fix)

### alphaE candidate sections (the primary CH4 surface)

**[ACCEPT] alphaE §C0 (lines 63–109) — de-fact-stream typed Track 1.**
LOC budget ~400–700 with generated.rs flagged as template output under
`generated-size-budget` (line 98): credible against `runtime_generator.rs`=1336 +
`nonjson_css_l4.rs`=3644 + 7×646 regen. Risk MEDIUM-HIGH with the routing-fidelity
blocker cited (line 99–101, `w6tape-conversion-report.md:54`): correct. Scalar-ref
N/A correctly justified (codegen route, not a primitive — CH4 scalar-ref binds
*primitives*, line 80–81). checkasm N/A with 8-field equality as the correctness
gate (line 83–86): correct. Same-wave consumer named and real: the benched
`track1_*` fns (`nonjson_css_l4.rs:596`) consume the typed summary in the same
commit the generator stops emitting fact-stream (line 87–89): verified consumer
exists. Wave alignment (precondition, no perf promise) is honest. ACCEPT.

**[ACCEPT] alphaE §C1 (lines 111–155) — tape wiring + lazy cursor view.**
LOC ~600–1000 credible (`json/scan.rs`=337 + `json/value.rs`=172 is the copy-model,
plus 7-grammar view modules + lower tape ops). Risk HIGH with TWO named blockers
(routing fidelity + borrowed-slice-vs-lazy, `w6tape-conversion-report.md:54,55`) and
the directive decision promoted to an ENTRY GATE (line 145–150): this is exactly the
CH4 wave-alignment discipline — the gating decision is named before dispatch, not
discovered mid-wave. Scalar-ref N/A (data-structure migration) correct. checkasm
N/A with `tape_substrate` 10/10 extended to CSS + 8-field equality. Same-wave
consumer = the C0 typed-summary fn consuming the lazy `ValueRef` view in the same
commit (line 130–134), Lock-1 same-wave rule cited (`LOCKS.md:75`): structurally
sound. ACCEPT.

**[ACCEPT] alphaE §C2 (lines 157–214) — NEON structural pre-scan via PrimitiveKernels.**
This is the model CH4 candidate. LOC ~300–500, "kernels REUSED, zero new SIMD
source" (line 201–202): verified — `dispatch.rs`/`PrimitiveKernels`/`classify_tbl4`
all exist in skinny. Scalar-ref PRESENT (`scalar::classify_chunk` dispatch.rs:21,
`scan_structurals_scalar` json/scan.rs:32, line 174–177): verified surfaces exist.
checkasm PRESENT and precedented — the four cited tests
(`checkasm_byte_class_from_table_64`, `checkasm_ascii_set_member_find_64`,
`checkasm_bulk_emit_positions_64`, `checkasm_bitmap_next_set_bit`) verified present
in `skinny/crates/bbnf-simd/tests/`. The lo6-admissibility fallback (line 184–187)
correctly makes scalar the honest answer on alphabet collision — no CSS special-case.
Same-wave consumer = C1 tape build consuming the `Vec<u32>` index (line 188–191).
Risk MEDIUM, correctly the lowest perf-risk lever. ACCEPT.

**[ACCEPT] alphaE §C3 (lines 215–257) — commit-by-construction spine.**
LOC ~400–700 credible (`tape_plan.rs`/`offset_tape.rs`/`event_tape.rs` all exist in
skinny `lower/`). Risk HIGH correctly flagged as "the candidate most likely to need
a triumvirate" (line 252–254) with the provability-of-non-deposition crux named.
Scalar-ref N/A (codegen control-flow) correct. checkasm N/A with the NEW gate
(no-checkpoint Alts PROVEN non-depositing at generation time, not heuristically,
line 235–240) — this is the right cost-control: the equality count catches an
over-eager removal (CH5 coupling acknowledged). Same-wave consumer = the parser
spine itself (line 241–242). ACCEPT.

**[REVISE] alphaE §C4 (lines 259–304) — tailwind tuning: udot + i8mm.**
LOC ~250–450, scalar-ref PRESENT (`digit_mac.rs:18-22` cfg-fallback), checkasm
REQUIRED+NEW (`checkasm_digit_mac`/`checkasm_i8mm_*`) — all correct and verified.
BUT the CH4 cost defect: C4 bundles TWO independently-budgeted interventions of
DIFFERENT risk under one ~250–450 LOC line. (a) wiring the existing
`digit_mac` udot orphan into the CSS number leaf (scalar+NEON present, checkasm-
adjacent, LOW risk); (b) a NET-NEW i8mm runtime-detected kernel (zero i8mm anywhere
in skinny — verified grep-clean — so this is new asm! + new feature-detection
plumbing + new checkasm + scalar twin, MEDIUM-HIGH risk). C4 itself concedes
"LOW perf-ceiling confidence on tailwind" and "whether the number leaf is even a
tailwind hot spot... must be re-profiled, not assumed" (line 296–298). Bundling a
speculative net-new i8mm kernel (whose hot-leaf relevance is unproven) into the same
LOC/wave/commit as the safe udot-orphan wiring violates the same-wave-consumer cost
discipline: the i8mm kernel has no proven same-wave consumer until the tailwind
profile shows the number leaf is hot. **Fix:** split C4 into C4a (wire `digit_mac`
udot orphan into CSS number leaf, ~100–150 LOC, LOW risk, scalar+checkasm present,
same-wave consumer = CSS number leaf — admits unconditionally as orphan-retirement)
and C4b (i8mm net-new kernel, ~150–300 LOC, MEDIUM-HIGH risk, GATED behind a Wave-5
re-profile proving the number/digit leaf is a top-N tailwind self-time symbol; if the
profile does not show it hot, C4b does NOT land — no orphan kernel). State the LOC
budget and risk class per sub-candidate. Keep both checkasm gates.

**[ACCEPT] alphaE §2 dependency order (lines 308–330) + §3/§4 (lines 332–358).**
Wave alignment C0→C1→C2→C3→C4 with per-edge gates (≥30→≥80→≥300→cross) and the
explicit ENTRY GATE on C1 (borrowed-slice-vs-lazy) is sound CH4 sequencing. The
"C0+C1 coupled, may be one wave" note is correct. The escalation note (§4) correctly
binds the unmeasurable-CSS case to PASS-ALPHA §8. ACCEPT (the C4 split from above
propagates into §2 as the only edit).

### SYNTHESIS receiver-cost sections (CH4 owner-path + wave-alignment surface)

**[REVISE] SYNTHESIS §0.1 "Layout-driven projection" row (line 56) + §0.3 receiver
goalset rows 1–2 (lines 84–85).** Owner paths target the UN-BENCHED core tree:
`crates/core/src/backend/rust/emitter/` (§0.3 row 1), `regen_css.rs emit_builder`
+ "DELETE the `OpenFrame` template + the `match rule_id` begin_compound" (§0.3 row 2),
`css_l4/builder.rs:274` "~40-arm match" (§0.1). Verified: `StructLayout`,
`OpenFrame`, `begin_compound`, `match layout.rule_id`, `TapeStructBuilder`, and
`css_l4/builder.rs:274` exist ONLY in `crates/core/` — `grep` of all three in
`skinny/crates/` is empty. The benched CSS Track 1 is the skinny `RequestFacts`
String path (`nonjson_css_l4.rs:596`, `runtime_generator.rs:25`). A receiver
executing §0.3 verbatim spends its LOC budget refactoring the un-benched tree.
**Fix:** reconcile SYNTHESIS §0.1/§0.3 to alphaE §0 lines 37–51 + alphaE C0/C1
owner paths. Replace the receiver paths with the skinny surfaces:
`skinny/crates/codegen/src/grammar_provider.rs` + `runtime_generator.rs:17-25`
(emit fork), `skinny/crates/codegen/src/lower/{offset_tape,event_tape,tape_plan}.rs`
(tape lowering), `skinny/crates/runtime/src/tape/{mod,assembler}.rs` (substrate),
`skinny/crates/runtime/src/grammars/css_l4_*/` (lazy view modules + retire
`emit_fact_stream`), `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:596-624`
(track1 fns return typed summary). Where a core-tree concept (`StructLayout`,
`begin_compound`) is referenced as the design intent, state it as the skinny
equivalent per alphaE: `BackendRule` + `lower/tape_plan.rs` (`TapeFlavor`,
`render_rule`) — NOT the core path. Carry a one-line note that totality (core tree)
adopts the same shape downstream (the "foldable into TOTALITY" gate), so the core
paths are the *fold target*, not the SK-V17 owner paths.

**[ACCEPT] SYNTHESIS §0.5 per-corpus close conditions (lines 138–161).** Wave
alignment of interventions to corpora is sound: normalize/bootstrap → four-lever
stack (W1/W2/W3/W4), tailwind → W5 (delimiter tuning + udot + i8mm). The tranche-
level criterion ("at least one regular corpus crosses") with tailwind allowed to
land short + honest residual is correct cost-bounding. The Wave-0 re-baseline of the
lightningcss bar (lines 145–147) correctly prevents stale-number gates. The W5
intervention list inherits the C4 split disposition above (digit_mac udot is the
safe lever; i8mm is profile-gated) — apply the C4a/C4b split here too, but the
corpus mapping itself ACCEPTS.

**[ACCEPT] SYNTHESIS §0.3 receiver rows 3–6 (lines 86–89).** CSS typed equality
re-proof, N≥50 telemetry + full-CSSOM comparator, NEON grammar-general hot-leaf
union (scalar-ref + checkasm + same-wave + aarch64-only + non-JSON exercise), and
generated-state clean regen are all cost-correct receiver obligations with the right
gates. The NEON row explicitly carries scalar-ref + checkasm + same-wave-consumer
per leaf (line 88), satisfying CH4. ACCEPT.

---

## Counts

- ACCEPT: 7 (alphaE C0, C1, C2, C3; alphaE §2/§3/§4; SYNTHESIS §0.5; SYNTHESIS §0.3 rows 3–6)
- REVISE: 2 (alphaE C4 bundling; SYNTHESIS §0.1+§0.3 rows 1–2 owner-path mis-target)
- REJECT: 0

Two REVISE dispositions, both with concrete path:line fixes and no orphan
(each names the corrective surface). The SYNTHESIS owner-path REVISE is the
load-bearing one: it must be reconciled to alphaE before G-Alpha, else the receiver
burns its LOC budget on the un-benched core tree. CH4 does not raise CH1/CH5/CH6
matters; this lens is cost only.
