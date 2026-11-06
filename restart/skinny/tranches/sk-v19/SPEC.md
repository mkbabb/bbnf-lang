# SK-V19 SPEC — the totality fold: port the skinny-proven architecture into the crates/core 9-grammar fleet

Pass-Omega-certified wave manifest. Date: 2026-06-07. HEAD: `a97f5f365`. Cycle: SK-V19 (skinny→full).
Inputs: PASS-IMPL fold-readiness (`audit/PASS-IMPL-FOLD-READINESS.md`), Pass Alpha (`PASS-ALPHA.md`), the
S-P1/2/3 wave design (`wf_c0dd19f9-f18`), the receiver map MASTER-PLAN MP.SK19.W0-W6, the re-anchored
T-P3 design (`restart/audit/totality/p3/3D-skinny-fold.md`, re-anchored per PASS-IMPL).

## §0 — Thesis + close conditions
ADOPT the SK-V18-proven architecture (the un-forked generator, the SoA tape, `ValueRef<K>`/`Cursor`, the
5-shape `BackendShape` decision, the value-api `ValueProject` derive, the G6 NEON classifier) into
`crates/core` for all 9 grammars (json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf/math), so that
the generic substrate carries ZERO grammar-specific code (Lock 14) and the JSON/CSS >SOTA throughput is
PRESERVED. The fold is a GREENFIELD INTRODUCE (core has no SoA tape, no `ValueRef`, no `BackendShape`).

**Close conditions (all must hold):**
1. `zero_grammar_code`: the Lock-14 9-roster regex over `crates/ir/src` + `crates/analysis/src` == 0
   (code-scoped); the self-gate FLIPS RED→GREEN with a live re-inject falsifier.
2. `one_substrate`: exactly one encoding (SoA `Tape`); `rg TapeRec crates/` == 0; no eager StructBuilder
   family; no dual AoS/SoA (Lock 1).
3. `one_value_generator`: one grammar-agnostic value-plane generator emits all 9 grammars;
   `generator_grammar_branch_count == 0`; the `json_templates` couriers + the false `@generated` header
   are RETIRED; JSON `value_from_ref` byte-equal.
4. `decision_load_bearing`: the 5-shape `BackendShape` selector (egraph+csp) moves ≥1 grammar's shape by
   an executable cost-row; the all-five gate holds; the (a)-(d) hot-leaf primitives are cost-gated.
5. `sota_preserved`: JSON > sonic-rs strict + CSS > lightningcss re-confirmed on the core bench, same-run,
   no regression vs the pre-fold baseline; framed lazy-rich-HEAD-vs-eager-full-CSSOM (a97f5f365).
6. `one_kernel`: one shared aarch64 structural classifier home (`crates/simd-scan`); x86/wasm production
   wiring deleted; the Lock-16 manifest row landed.
Outcome: A (all hold) / S (honest residual, e.g. a quiet-bar re-capture pending) / N (the fold cannot
preserve >SOTA without a contrivance — surfaced honestly, never paper-closed).

## §0.5 — Resolved decisions (Pass-Omega CRUD over the S-P design open-decisions)
- **D-A (W3/W5 granularity):** a FENCED CO-WAVE PAIR with a SHARED close gate (D11), separable internal
  sub-gates (W3a value-plane, W5a-d engine). Not one monolith; co-landed.
- **D-B (the C fold):** COLLAPSE the value/view/document layer into ONE grammar-agnostic generator for all
  9; KEEP the JSON string/number + CSS `css_balanced_component_scan` hot leaves as W5-cost-gated `(a)-(d)`
  named primitives INVOKED by the one walk. The three skinny emit bodies (`json_sink_direct`,
  `css_scan_direct`, `general_descent`) collapse to ONE walk + cost-selected leaf primitives — not three
  forked emitters. `css_balanced_component_scan` stays CSS-scoped (no non-CSS caller; SK-V18 finding).
- **D-C (W6d StructLayout):** PATH-(a) — full `StructLayout→Layout` rename across the 1030 sites (regen 9
  parsers + ~16 tests), the clean root fix that lands the canonical names (no side-table workaround).
- **D-D (carriers):** ALL 9 (math included — the S-design caught the receiver's "8 carrier" undercount).
- **D-E (x86/wasm):** DELETE the `simd-scan` avx2.rs/avx512.rs/wasm.rs production wiring (aarch64-only
  mandate; the P1-prune precedent). aarch64 NEON is the sole production kernel.
- **D-F (BBNF-self):** the second negative control per §13.7 — the generator emits BBNF-self's value plane
  by-construction; the fleet-neutral wording requires BOTH Sheets (proven) AND BBNF-self witnesses.
- **D-G (W0 entry-gate):** confirm the SK-V16/V17 REDRESS pre-block reconcile (items 51/53/246/247) is on
  the committed ledger before W1/W2 dispatch (CH3-V1-R2).

## §1-§7 — the wave manifest (re-censused at HEAD a97f5f365)

### §1 — W0: substrate_target classification (pre-gate; 0 behaviour LOC; GATES W2)
Goal: classify all 9 carriers' `substrate_target` against `bbnf_ir::registry::strategy.rs`
PRODUCTION_MANIFEST_TABLE (:134-189) → every carrier converges to `existing_tape`. Port: the skinny
substrate-manifest discipline (one tape, the 5 shapes project from it). Exit (MEASURABLE): a classification
report covers all 9 carriers; each declares a `substrate_target`; the `md5(builder.rs)` no-identical-pair
distinctness witness; `git diff --stat` == 0 behaviour lines. Falsifier: any carrier declared
`existing_tape` while still carrying a parallel retained structural-index after W2 → REDRESS-53 re-entry.
Entry: D-G reconcile on the ledger. Seq: first; gates W2; consumed by W1+W5.

### §2 — W1: eager builders retire → flat-tape commit-by-construction
Goal: delete the 11 eager StructBuilder families (OpenFrame json/css_l4/bbnf, Frame sheets, SimpleStruct
the 6-cohort) + their `stack.clone()` checkpoint/rollback; re-point the codegen substrate splice to ONE
generic tape builder. The edit lands in the GENERATORS: `xtask/src/regen_css.rs::emit_builder`,
`xtask/src/regen_simple_runtime.rs` (4343 LOC, the 5 RuntimeStyle arms collapse to one). Port: skinny
`assembler.rs TapeBuilder` (`push_plain_offset`; ZERO checkpoint/rollback — speculation lifts upstream of
the tape). Exit: `rg JsonStructBuilder|CssStructBuilder|SheetsStructBuilder|SimpleStructBuilder crates/`
== 0 (was 47); only the generic tape builder remains; per-grammar regen ×9 byte-clean. LOC: 300-700
generator-side. Seq: after W0; jointly lands with W2 (commit-by-construction consumes the SoA encoding).
Falsifier: delete-before-tape-wired (orphan deletion) → revert. >SOTA: commit-by-construction removes the
O(depth) clone speculation — a throughput WIN, not a risk, on the parse path.

### §3 — W2: AoS TapeRec → SoA Tape (exactly-one-encoding closure; Lock 1)
Goal: delete the latent 16-byte AoS `TapeRec` (`runtime/tape/record.rs:103-118`); move the SoA columns
(offsets:Vec<u32> + flag_cursors/flag_values) into `tape/mod.rs Tape`; re-point `cursor.rs` navigation.
Clear the dual-encoding hazard (the unwired AoS tape becomes a Lock-1 violation the moment SoA wires).
Exit: `rg TapeRec crates/` == 0; the tape exposes ONLY SoA columns; `dual_encoding_present == false`; all
parity tests green on SoA; `tape_substrate.rs` rebased. LOC: 200-600. Seq: co-lands with W1; before W3.
Falsifier: a dual AoS/SoA end-state → REDRESS-53/Lock-1.

### §4 — W3: the C fold — ONE grammar-agnostic value-plane (CO-WAVE with W5; D11)
Goal (D-B): introduce ONE grammar-agnostic lazy `ValueRef<K>`/`Cursor`/`DocumentView` value-plane over the
W2 SoA tape, emitting the value/view/document layer for all 9 grammars from the grammar's `->` projection
(the b7ab7c558 `ValueProject{body,target,ty}` derive + `classify_projection`); RETIRE the `json_templates`
hand-authored value/view/visitor couriers + the false `@generated` header; collapse the three skinny emit
bodies to ONE walk + cost-gated leaf primitives. Port: skinny `general_descent/{emit,view}.rs` + the
value-api thread. Exit (MEASURABLE): one generator emits all 9; `generator_grammar_branch_count == 0` ∧
`generator_grammar_type_count == 0`; `verbatim_blob_present == false`; JSON `value_from_ref` byte-equal vs
the pre-fold eager `JsonValue` over the full json corpus; Sheets typed accessors preserved; BBNF-self
emits by-construction (D-F). LOC: 300-700 generator + ×9 regen + the JSON un-courier. Seq: after W1/W2;
co-lands with W5; fenced by W4. Falsifier: a naive general walk that regresses JSON/CSS >SOTA → the (a)-(d)
primitives are NOT cost-gated → reject (D11).

### §5 — W4: StructRegistry/FieldSource compile-time projection fence (0 LOC fence on W3)
Goal: resolve every struct layout ONCE at codegen, NEVER per-leaf at runtime. Remove the runtime
`REGISTRY: LazyLock<StructRegistry>` breach (`grammar/generated/json.rs:276`); sever the `arena.rs:47`
`StructRegistry::compound_kind_for_layout` per-leaf coupling (folded into W1). Exit: `rg
'REGISTRY.layout|LazyLock<.*StructRegistry>|begin_compound' generated/` == 0; a samply sample of the
JSON/CSS hot path shows ZERO `StructRegistry` self-time. Falsifier: ANY per-leaf runtime
`StructRegistry::layout(rule)` → re-opens 28-65×/983× regression → reject. Seq: fences W3 (co-wave).

### §6 — W5: the decision engine + shared kernel (CO-WAVE with W3; D11; 0→N INTRODUCE)
- **W5a** introduce the 5-shape `BackendShape` {EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}
  enum + DecisionCost lattice into `crates/ir` (~60-120 LOC; core has ZERO today). Exit: `rg 'enum
  BackendShape' crates/ir` == 1, 5 variants byte-match the canon.
- **W5b** port the egraph+csp two-stage selector (`backend_egraph` ~312 + `decision_csp` ~273 + derive
  ~210 LOC) into `crates/ir/src/passes/recognizers/`, wired into `crates/core/src/pipeline/compile/`.
  Exit: a `backend_shape` entry for EVERY rule of ALL 9 grammars; the all-five gate (the union of selected
  shapes == exactly 5, no 6th).
- **W5c** the cost model gating the (a)-(d) named primitives (~150-350 LOC): `abcd_pass == all-green` per
  admitted primitive (grammar-INVOKED ∧ output-VARIES-under-rule-mutation ∧ verbatim_blob==false ∧
  PROFILE-PROVEN-NARROW-LEAF); `primitive_loc <= profiled_leaf_extent` (no god-kernel). The >SOTA engine.
- **W5d** unify the two kernels into ONE aarch64 home (`crates/simd-scan`): add the vceqq eq-set fan as a
  4th `KernelShape` branch for lo6-collision alphabets (CSS `; { }`); DELETE avx2/avx512/wasm production
  wiring (D-E); the Lock-16 manifest row. Exit: ONE classifier home; `rg 'mod avx2|mod avx512|mod wasm'
  simd-scan/src/lib.rs` == 0; the vceqq fan handles a lo6-collision alphabet. LOC: ~120-160 fan + ~800-1100
  cost-cell + 600-1400 joint wiring.
Seq: W5a→W5b→W5c (capstone, co-lands with W3); W5d after W5c + W2. Falsifier: a selector that moves no
shape (a constant) → not load-bearing → reject.

### §7 — W6: the leak collapse (the user bar: zero grammar code in generic crates)
- **W6a** delete `strategy.rs:134-189` PRODUCTION_MANIFEST_TABLE (the 9-row grammar-named const, -56 LOC);
  the resolver reads `[workspace.metadata.bbnf]` data. Exit: `rg 'JsonParser|CssL4Parser|BbnfBootstrap|
  GoogleSheetsParser' crates/ir/src/` == 0 (was 9).
- **W6b** relocate `css_types.rs` (66 LOC) → `runtime/css_l4/host.rs`; the ONE consumer (`builder.rs:727`)
  re-points. Exit: `rg css_types crates/core/src/` == 0; css_l4 parity green.
- **W6c** re-key `shape_dict_bbnf.rs:79-101` (`match rule_name { "big_comment"=>… }`) onto the STRUCTURAL
  shape (`is_big_comment_shape`/`is_mapped_factor_shape`), not rule-name strings. Exit: `rg
  '"big_comment"|"mapped_factor"' crates/ir/src/` == 0; the IR-recognizer 9-roster regex == 0 (code).
- **W6d** (D-C, path-a) full `StructLayout→Layout` rename across 1030 sites (regen 9 parsers + ~16 tests).
  Exit: `rg StructLayout crates/` == 0; canonical `Layout`/`LayoutFacts` present.
- **W6e** flip the Lock-14 self-gate RED→GREEN: widen the leak regex 4-name→9-roster; wire it LIVE
  (xtask/CI) with a re-inject falsifier. Exit: the widened regex == 0; the gate is a live check, not prose.
Seq: W6a/W6b/W6c parallel (independent of substrate), gate W6e; W6d after W5 (reconciles against
LayoutFacts); W6e last (the cluster terminus).

## §8 — Sequencing DAG + telemetry
DAG: `D-G ledger → W0 → W1∥W2 → [W3 ⊗ W5]fenced-by-W4 → W6d` ; `W6a∥W6b∥W6c → W6e` (parallel to substrate).
Telemetry (per wave): `substrate_target[9]`, `eager_builder_count`, `tape_encoding`, `dual_encoding_present`,
`generator_grammar_branch_count`, `verbatim_blob_present`, `json_value_from_ref_byte_equal`,
`backend_shape_canon[5]`, `all_five_gate`, `abcd_pass[]`, `kernel_home`, `lock14_roster_hits`,
`structlayout_sites`, `sota_json_ratio`, `sota_css_ratio`, `host_loadavg`. Every column consumed at close.
>SOTA preservation is the throughline gate; the Lock-14 zero-grammar-code bar is the close.
