# Tranche AW-III — Correctness Closure & Architectural Transposition

AW-III closes the correctness arc AW-I + AW-II opened AND ships the
architectural transposition that makes the DTA into the kind of flattened
tape automaton it claims to be. Six waves; one tranche. **No deferrals,
regardless of newfound scope.**

At AW-II close, workspace sits at **1050 passed / 50 failed / 67 ignored**
and the 14 measured bench entries show **5–40× regression vs post-AU**.
The eight-agent research wave (six perf-* + two correctness audits +
arch-comparison) named the regression's shape: `dispatch_one` is a 14-arm
tagged-union interpreter floor (~24% self-time everywhere), and a stage-1
SIMD structural-bitmap pre-pass has been chronically deferred since AQ.5
(`2f7c1bd4`) and never attempted optimally. The follow-up six-agent wave
(`aw3-r1` through `r6` + `SYNTHESIS-2-PATH-FORWARD.md`) resolved the
architectural transposition: three general emitter passes that mechanically
specialise the walker per grammar from existing IR facts.

This is the only path. AW-III ships it.

## Scope

1. **Correctness closure** — every AW-II residual (Cluster A parse
   failures, Cluster C payload activation, Cluster D integration) lands
   green. No `#[ignore]` added; every existing one audited.
2. **Architectural transposition** — three general emitter passes
   (walker specialisation, stage-1 SIMD structural-bitmap, fused SoA
   write API) plus activation of the five emitter-mined consumers
   (ShapeRef, PHF, ClassifyByte, direct-to-struct, Pratt const-fold).
3. **19-entry bench matrix** — every entry measurable, **strict-better-
   than post-AU on ≥ 15/19** with three sheets-small entries documented
   as small-input tradeoff.

AW-IV picks up granular exceed + parity harnesses. AX inherits the same
substrate.

## Architectural thesis

The DTA walker today runs at ~29 cyc/byte on JSON twitter vs post-AU RD's
~1.78 cyc/byte. The 16× gap is canonical state-machine-interpreter
overhead — implementation, not architectural. simdjson and sonic-rs are
flattened tape automata at ~0.9–1.0 cyc/byte; the same property is
recoverable for DTA via three general emitter passes triggered by IR-
structural properties (no per-grammar hand-written branches, per the §7
invariant):

1. **General walker-specialisation pass** — walks `DtaTable.states` and
   lowers each `DtaState` variant to inlined Rust; the enum match
   disappears in the *output* (the table still carries it, replay still
   consults it). One pass; per-grammar OUTPUT because per-grammar IR.
   Industry-standard state-machine-as-labelled-blocks pattern.
2. **General stage-1 SIMD bitmap pass** — reads `compute_structural_
   alphabet` (already mined; field defaults to `&[]` because no consumer
   exists) and emits a per-grammar `scan_structural_<grammar>` whose
   intrinsics are mechanically chosen from alphabet cardinality + digraph
   count + quote-class count.
3. **Fused SoA write API** — one bounds-check + 7 unchecked stores
   replaces 7 `Vec::push` per `reserve_compound` (~15–20% recovery
   uniformly).

These are not new levers; they are the *general* form of mechanisms the
AW-IV-as-originally-planned tried to ship per-grammar. The transposition
is what makes them honest.

## Invariants

1. **Every `#[ignore]` at AW-II close is audited and dispositioned**.
   CLOSE / DELETE / INVESTIGATE-then-resolve. Under no circumstance does
   AW-III close with an undispositioned ignored count > 0.
2. **No new `#[ignore]` added in this tranche.** Inherited edict
   strengthened into a hard gate.
3. **Producer-side surfaces in scope at all waves**. Walker, lifter,
   emitter, IR passes, all editable.
4. **One path.** No dual-path builds, no feature-flagged fallbacks, no
   "legacy mode" shims. The walker-specialisation pass replaces the
   interpretive `dispatch_one` hot path entirely; `dispatch_one` survives
   only as a cold-path fallback for replay-introspection (the AX
   substrate consumes it; the parse hot path does not).
5. **Bootstrap idempotent at every wave boundary.**
6. **Full generalization — no grammar-specific fixes.** Every emitter
   pass is triggered by grammar-structural IR properties (alphabet
   cardinality, Alt density, keyword count, shape repetition, operator
   chain depth, state count, state visit frequency), NOT by grammar
   identity. The grammar's name appears nowhere in any hand-written
   code path. Per-grammar IMPACT varies because grammars have different
   IR; per-grammar MECHANISM does not vary. A pass that ever branches
   on grammar name is rejected as a §6 violation — same standard the
   AW-III-original plan declares for PHF, ClassifyByte, ShapeRef,
   direct-to-struct.
7. **No deferrals, regardless of newfound scope.** Inherited from
   `docs/instructions/README.md` and strengthened: scope-reveal under
   contact triggers re-plan-with-more-agents (per the operational
   protocol's parallel-orchestration contract), never silent forward-
   routing. The architectural transposition is one path, not a wave-
   budget. If a wave reveals more work, the work belongs to that wave.
   Escalation is permitted only for hard environmental blockers
   (compiler bug, authorisation boundary, irrecoverable state); scope-
   reveal is not an escalation condition.

## Wave schedule

| Wave | Scope | Agents | Workspace at close |
|------|-------|--------|--------------------|
| W1 | Six-point payload wiring + Pratt `Next` peel + scanner closure | 1 serial (producer-deep) | Cluster 1 + Group A ignores close; 47 tests pass-flip; HashMap-per-scan eliminated |
| W2 | Parse completeness — EOF/trailing-ws + EBNF offset-0 + CSV | 1 serial | workspace 0-failed or near-zero; 5 AW-II-blocked bench entries unblocked |
| W3 | Ignored audit + close — CLOSE 14 + DELETE 4 + cascades A/B; rest routed | 2 parallel | every remaining ignore has in-file rationale or routing entry |
| **W4** | **General walker-specialisation pass** + cargo-asm verification | 3 parallel | every grammar's emitted walker shows zero `dispatch_one` symbols on hot path; JSON twitter ≥ 1800 MB/s |
| **W5** | **General stage-1 SIMD bitmap pass** + driver dual-cursor redesign + fused SoA write API + new `bbnf-simd-scan` crate | 3 parallel | bitmap sustains ≥ 2 GB/s on 1 MB JSON; walker consumes via cursor; fused writes replace `reserve_compound` everywhere; AQ-5 failure modes verified absent |
| **W6** | Five emitter-mined consumer activations + 19-entry bench matrix + FINAL | 3 parallel + 1 serial close | strict-better than post-AU on ≥ 15/19 entries; `post-AW-III.json` exists; `FINAL-III.md` exists; green workspace |

**Intra-tranche bench checkpoints** (attribution clarity per architectural lever):

- `docs/benchmarks/post-AW-III-W4.json` — walker specialisation only; samply confirms `dispatch_one` symbol absent from hot path.
- `docs/benchmarks/post-AW-III-W5.json` — + stage-1 SIMD + fused writes + dual-cursor; samply attributes bitmap kernel self-time + cursor contention.
- `docs/benchmarks/post-AW-III-W6.json` — + five consumer activations; samply attributes ShapeRef hit-rate, PHF share, ClassifyByte share, direct-to-struct share, Pratt const-fold share.

Three sidecar files; each closes its wave; W6's FINAL-III aggregates them into multi-wave history. Per-lever contribution preserved; bigger AW-III win preserved; no consumer deferred.

## Phases

### W1 — DTA payload wiring + structural levers

Owner: `crates/bbnf-tape/src/{dta,driver}.rs`,
`crates/ir/src/passes/recognizers/dta.rs`,
`crates/core/src/backend/rust/emitter/dta.rs`,
`crates/ir/src/passes/materialization/**`.

**Six payload-wiring points** (Cluster 1 target — 37 tests):

1. Extend `DtaState::Regex` + `DtaState::Literal` with `payload: PayloadKind` field (IR + wire contract).
2. Lifter reads enclosing `IrNode::Map`'s FnDescriptor → resolves to `PayloadKind` → threads into `DtaState::Regex`/`Literal` construction. Alt branches inherit per-branch payload from their FnDescriptor. Fixes A1's Hole #1 (`dta.rs:525` wholesale strip).
3. Walker consumes `state.payload` and emits correct payload bytes — replaces hardcoded `PayloadKind::F64` at `driver.rs:912` (Hole #3); activates Literal payload writes at `driver.rs:875-891` (Hole #4).
4. Emitter const-folds payload writes into the generated DTA table.
5. Bootstrap regen under the extended schema. Verify idempotent.
6. `frame_to_tape_kind` promotes Seq → KvPair when the enclosing rule's layout is `KvPair` (Hole #5).

**Bug 2b residuals folded into the same wave** (originally AV V0 deferrals; coupled tightly to the Six-point payload wiring; ship together):

- **`pinned_number_drops_f64_payload`** (Sheets `number -> f64`): Map-bodied regex rule needs admission to the layout pass. Extend `scalar_layout_eligible` at `crates/ir/src/passes/payload/layout.rs` to admit Map-bodied rules whose body is a regex match producing a typed scalar payload — F64 / I64 / U64 / Bool / U8.
- **Sheets `boolean` FALSE branch drops `0u8`**: dispatch composer today requires literal-branch Alts. `boolean` uses regex-branch (`/TRUE/i`, `/FALSE/i`). Extend dispatch composer at `crates/core/src/backend/rust/emitter/dispatch.rs` to admit `Map { Regex, BoolLit }` branches.
- **3 CSS percentage InlineScalar reader tests**: `payload_u8` reader call sites in `crates/core/tests/css_l4_parity.rs` flip from `#[ignore]` to active once the payload wiring lands. Cascade closes naturally; ensure they're un-ignored at W1 close.

**Two structural one-fix levers**:

7. **Pratt `IrNode::Next` peel** — extend `strip_transparent_owned` at `crates/ir/src/passes/recognizers/dta.rs:885-890` to peel `IrNode::Next(a, b)` alongside `IrNode::Seq`. Unblocks `match_operator_chain_rule` on CSS `calc()` / `min()` / `max()` / `clamp()`. State count drops; walker dispatch depth drops.
8. **Scanner closure** — add `pattern_dfa: Arc<Dfa>` field to `DtaState::Regex`; populate at lift time from the compile-time pattern constant. Walker `dispatch_one` Regex arm uses the pre-bound `Arc<Dfa>` directly — no global HashMap lookup, no SipHash, no `Arc::clone` on the hot path. Eliminates 6–33% self-time depending on grammar per perf-01..05 attribution.

**Hard gate**: `cargo test --workspace --no-fail-fast` Cluster 1 count drops from 37 → ≤ 5. Scanner closure verifiable via samply: `cached_dfa` / `HashMap::get` drops out of top-20. Pratt peel verifiable via summarise call on CSS L4 DTA — new ShuntingYard state count > 0. Bug 2b: `pinned_number_drops_f64_payload` flips; Sheets `boolean` FALSE flips; 3 percentage tests un-ignored.

### W2 — Parse completeness

Owner: diagnose per-test; fix likely spans `crates/bbnf-tape/src/driver.rs`, `crates/ir/src/passes/recognizers/dta.rs`, `crates/core/src/lower/**`.

**Clusters** (13 failures + 1 CSV escalation):

- **Cluster 2 (shared EOF)**: `json_data`, `json_canada`, `parse_data_json`, `parse_canada_json`, `css_tailwind`, `css_bootstrap` truncation, `css_normalize` truncation (7 tests). Walker EOF / trailing-whitespace handling. **Single fix.**
- **Cluster 3 (EBNF offset-0)**: `ebnf_minimal`, `ebnf_recursive_list`, `ebnf_expr_grammar`, `ebnf_root_has_at_least_one_rule`, `ebnf_prettify::parse_{single,multi}_rule` (6 tests). Every EBNF grammar fails at `Syntax { offset: 0, rule: None }`. AW-II.W5b's Minus + double-Repeat were necessary but insufficient; remaining upstream gap in `@ws` or first-literal dispatch for EBNF.
- **Cluster 5 (CSV Repeat-of-Seq)**: `csv_multi` (1 test) — `csv = record, ( /\r?\n/ >> record ) *` Repeat walker regression at the record-separator boundary.

**Hard gate**: Cluster 2 + 3 + 5 closed; all 5 AW-II-blocked bench entries measurable (`data`, `canada`, `tailwind`).

### W3 — Ignored-test audit + close

Owner: two parallel agents (C2 audit already produced `ignores-audit.md`).

**Pre-staged dispositions from AW-III.C2** (58 unique source ignores):

- **CLOSE — 14 tests**: already verified passing when `#[ignore]` lifted. Mechanical attribute lift.
- **DELETE — 4 tests**: 3 `unreachable!()` stubs + 2 gorgeous visualisation dumps (non-checked-in fixtures). Mechanical test-function deletion with rationale.
- **INVESTIGATE — 40 tests** across 7 root-cause groups:
  - A (10): CSS percentage + JSON variant_idx — **cascades from W1 payload wiring**.
  - B (1): `ebnf_rule` serialize — **cascades from W2 EBNF completeness**.
  - C (6): structural-mode analysis pipeline — out of AW scope (analysis-mode rework tranche).
  - D (5): closure-body lowering — grammar-closures project.
  - E (6): CSP solver GAC alldiff — csc411 solver tranche.
  - F (4): gorgeous prettify multi-rule + pprint-vm hint-semantics drift.
  - G (7): miscellaneous producer-side + test-data.

**Hard gate**: CLOSE batch lifted + DELETE batch removed + `docs/tranches/AW/audit/ignore-routing.md` exists with successor-tranche mappings for every remaining ignored test.

### W4 — General walker-specialisation pass

Owner: 3 parallel agents.
- (a) IR-side: emitter-pass scaffolding + `state_visit_frequency` mining.
- (b) Codegen-side: lowering each `DtaState` variant to inlined Rust labels.
- (c) Driver-side: collapse `dispatch_one` to cold-path replay surface; remove from hot path; verify `cargo asm` clean.

**The pass.** Lives at `crates/core/src/backend/rust/emitter/dta_walker.rs`. Signature:

```rust
pub fn emit_specialised_walker(
    grammar: &str,            // symbol-namespace prefix; no behavioural branch
    table: &DtaTable,         // existing IR fact
    alphabet: &StructuralAlphabet, // existing IR fact (extended in W5)
    profile: &GrammarProfile, // existing IR fact
) -> TokenStream;
```

Mechanically lowers `DtaTable.states` to inlined Rust. Per state:

- `Seq` → child sequence with frame open/close inlined.
- `ByteDispatch` → `match input[pos] { ... }` over the table's `[DtaStateId; 256]`.
- `AltLinear` → savepoint loop with branches inlined.
- `Regex` → inlined `find_at` against the state's `pattern_dfa: Arc<Dfa>` (W1.8 lift-time closure).
- `Literal` → byte cmp.
- `Repeat` → counter-bounded loop.
- `ShuntingYard` → operator-precedence step using emitted `PRECEDENCE_LUT` (populated by W6 Pratt const-fold consumer).
- `Ref` → continue dispatch.

Transitions resolve at emit time. The output is one function with N labelled blocks and direct control flow — the industry-standard state-machine pattern.

**Hot/cold partitioning** is the only emit-time decision the pass makes, driven by IR cardinality (state count vs LLVM inlining budget — pluggable via the egraph cost model):

- `state_count ≤ HOT_BUDGET` → single function, all states inline.
- `state_count > HOT_BUDGET` → mine `state_visit_frequency` (same IR fact PHF frequency-ordering uses); hot states inline in the outer loop, cold states emit as `#[cold] #[inline(never)]` siblings called via branch.

Both strategies are general; the choice is driven by IR cardinality, not grammar name.

**Hard gate**: the emitter pass is `pub fn` with no grammar-name branch (`grep` for grammar identifiers in the pass body returns zero hits in conditional branches; only in symbol naming). For every grammar in the corpus, `cargo asm -p bbnf <bench_bin> dta_run_<grammar>` shows no `dispatch_one` symbol in the hot path. JSON twitter bench ≥ 1800 MB/s (sonic-parity ratio ≥ 0.65). The `dispatch_one` enum still exists in `dta.rs` and is still consulted by the cold-path replay surface (AX substrate); it is absent from the parse hot path.

### W5 — Stage-1 SIMD structural bitmap pass + driver redesign + fused SoA write API

Owner: 3 parallel agents.
- (a) IR-side: enrich `compute_structural_alphabet` (digraph_mask, digraph_pairs, quote_classes); new pass for kernel-shape selection.
- (b) Kernel-side: new crate `bbnf-simd-scan` (NEON kernel + AVX2 sibling + CLMUL/PMULL parity + shift-XOR fallback).
- (c) Driver-side: dual-cursor redesign + savepoint extension + fused write API + `reserve_compound` migration.

**The pass.** Lives at `crates/core/src/generate/dta/stage1.rs`. Signature:

```rust
pub fn emit_structural_scanner(
    grammar: &str,            // symbol prefix
    alphabet: &StructuralAlphabet,
) -> TokenStream;
```

Alphabet IR enriched at `crates/ir/src/passes/sets/structural_alphabet.rs`:

- `singletons: BitSet<u8>` — already mined.
- `digraph_mask: [u64; 4]` — first-byte bitset; **NEW**.
- `digraph_pairs: &[(u8, u8)]` — second-byte targets per first-byte; **NEW**.
- `quote_classes: BitSet<u8>` — string-toggle bytes; **NEW**.

Pass mechanically chooses kernel shape from cardinality:

- `|singletons| ≤ 8` → nibble-LUT collapse (one `vqtbl1q_u8` per 16-byte lane).
- `9 ≤ |singletons| ≤ 16` → wide-LUT (lift the `1 << i` cap from R2 §5.1).
- `|singletons| > 16` → multi-pass cmpeq + OR-reduce.
- `|digraph_pairs| > 0` → `vextq_u8` shifted-compare per pair, OR into mask.
- `|quote_classes| > 0` → CLMUL/PMULL parity (x86) or 6-op shift-XOR (NEON).

Output type at `crates/bbnf-tape/src/stage1.rs`:

```rust
pub struct StructuralIndex { pub positions: Vec<u32>, pub kinds: Vec<u8> }
```

**New crate `bbnf-simd-scan`** holds the architecture-neutral kernel infrastructure (per the §architecture invariants — general-purpose constructs in their own crates). Full structure, no stubs:

```
crates/bbnf-simd-scan/
├── Cargo.toml             — workspace member; deps: bbnf-tape (StructuralIndex);
│                            no other workspace deps
├── src/
│   ├── lib.rs             — public API; re-exports per-arch kernels under
│   │                        cfg-gated modules; `pub fn scan_structural` entry
│   │                        function dispatches by `target_feature`
│   ├── alphabet.rs        — `pub struct StructuralAlphabet { singletons,
│   │                        digraph_mask, digraph_pairs, quote_classes }` (mirrors
│   │                        IR side); kernel-shape selector
│   ├── neon.rs            — `#[cfg(target_arch = "aarch64")]` — NEON path:
│   │                        nibble-LUT collapse via `vqtbl1q_u8`, wide-LUT for
│   │                        9–16 singletons, `vshrn_n_u16` movemask, `vextq_u8`
│   │                        digraph compare, 6-op shift-XOR quote parity.
│   │                        Apple M-class P-core targeted; E-core verified
│   │                        functional
│   ├── avx2.rs            — `#[cfg(all(target_arch = "x86_64", target_feature
│   │                        = "avx2"))]` — `_mm256_loadu_si256` +
│   │                        `_mm256_cmpeq_epi8` + `_mm256_movemask_epi8`;
│   │                        PCLMULQDQ quote parity; `tzcnt`-loop compaction
│   ├── avx512.rs          — `#[cfg(target_feature = "avx512vbmi2")]` —
│   │                        `_mm512_cmpeq_epi8` + `_mm512_mask_compressstoreu_epi8`
│   │                        for index compaction; opt-in via build flag
│   ├── wasm.rs            — `#[cfg(target_arch = "wasm32")]` — `i8x16.swizzle`
│   │                        + `i8x16.bitmask`; matches NEON shape
│   ├── scalar.rs          — portable scalar fallback for arches without SIMD
│   │                        intrinsics; correctness reference for fuzz
│   ├── compaction.rs      — bitmap → `Vec<u32>` index compaction;
│   │                        `tzcnt`-loop default; PEXT specialisation under
│   │                        BMI2 cfg; arch-neutral
│   └── parity.rs          — quote-state computation: CLMUL where available,
│                            6-op shift-XOR ladder fallback; arch-neutral
│                            interface
├── benches/
│   └── stage1_throughput.rs — per-arch throughput on canonical corpus:
│                              twitter, citm, canada (JSON);
│                              bootstrap, tailwind (CSS)
└── tests/
    ├── correctness.rs      — every kernel must produce identical
    │                         StructuralIndex on the canonical corpus; scalar
    │                         path is the reference
    ├── quote_parity.rs     — string-state correctness against escape-rich
    │                         inputs (twitter, JSON pathological); CLMUL vs
    │                         shift-XOR equivalence
    ├── digraph.rs          — CSS `/*` `*/`, BBNF `(*` `*)` `->`, EBNF `(*`
    │                         `*)` digraph detection
    └── fuzz.rs             — proptest: random byte sequences; assert
                              StructuralIndex.positions matches scalar
                              reference byte-for-byte
```

The crate is fully implemented at W5 close — no `unimplemented!()`, no
`todo!()`, no `#[cfg]`-gated empty stubs. Every per-arch path either ships or
is `#[cfg]`-gated out cleanly with a published-rationale comment (e.g.
`avx512.rs` may be opt-in if AVX-512 hardware isn't available for testing in
CI; the path compiles and tests under x86_64 cross-compilation with `RUSTFLAGS=
"-C target-feature=+avx512vbmi2"`).

**Driver redesign.** `crates/bbnf-tape/src/driver.rs`:

- Replace `pos: u32` cursor with `Cursor<'a> { src: &'a [u8], idx: &'a StructuralIndex, pos: u32, slot: u32 }`.
- `ByteDispatch` reads `idx.kinds[cursor.slot]`, advances `cursor.slot`.
- `Regex` scans bounded to `[cursor.pos, idx.positions[cursor.slot])` — no open-ended tail.
- New `DtaState::ConsumeToNextStructural` → O(1) cursor jump.
- `WsTrim` collapses to `cursor.pos = idx.positions[cursor.slot]`.
- `FrameStackSavepoint` gains `slot: u32` (fixes the AQ-5 unsaved-cursor failure mode by extending the existing record — no parallel savepoint structure).

**Fused SoA write API.** `crates/bbnf-tape/src/columns.rs`:

```rust
impl Columns {
    pub fn push_compound_fused(&mut self, kind: TapeKind, span_lo: u32) -> u32 {
        let idx = self.len;
        if idx >= self.cap { self.grow_all(); }
        unsafe { /* 7 unchecked stores */ }
        self.len = idx + 1;
        idx as u32
    }
}
```

Existing `reserve_compound` migrates to call `push_compound_fused`; old call sites delete entirely (no `#[deprecated]` shim — direct migration per the no-legacy-code invariant).

**Hard gate**: the bitmap kernel sustains ≥ 2 GB/s on 1 MB JSON; walker consumes via the dual cursor; AQ-5 failure modes (scalar quote-parity, duplicated Alt arms, unsaved cursor on checkpoint, disabled WS elision) all verified absent (test fixtures per failure mode). Every `self.<column>.push` site in `driver.rs` hot path is replaced by `push_compound_fused` or `push_leaf_fused`; `grep -n 'self\.\w*\.push' crates/bbnf-tape/src/driver.rs` returns zero for hot-path call sites. samply confirms `reserve_compound` < 5% self-time on every bench.

### W6 — Emitter-mined consumer activations + 19-entry bench matrix + FINAL

Owner: 3 parallel agents + 1 serial close.

**Five emitter-mined consumers**, each a general pass triggered by IR-structural properties. Full implementations; no stubs:

#### W6.1 ShapeRef runtime dispatch
Owner: `crates/bbnf-tape/src/driver.rs` (compound-emit branch); `crates/bbnf-tape/src/shape_dict.rs`; `crates/core/src/backend/rust/emitter/grammar.rs` for `active_columns` + `shape_dict` population.

Mining pass at `crates/ir/src/passes/recognizers/shape_dict.rs` already exists and emits 13 CSS L4 entries; W6 activates the **consumer**:

```rust
// In specialised walker compound-emit branch (emitted by W4 walker pass):
if let Some(ref_idx) = SHAPE_DICT.lookup(shape_hash) {
    cols.push_shape_ref(span, ref_idx, packed_payload);
} else {
    cols.push_compound_fused(kind, span_lo);  // W5 fused write
    /* ...children... */
    cols.close_compound(span_hi);
}
```

Strict-injective compile-time collision assertion: `emit_shape_dict_arrays` verifies every emitted hash unique per grammar; fails compilation on collision. Runtime walker consults via single indexed load + equality compare, no `columns_range_eq` confirm. View-layer `ShapeRefSyntheticChild` cursor expansion already landed (AV.5.1); W6.1 verifies parity via new `crates/core/tests/shape_ref_view_parity.rs` (walks every CSS L4 declaration in `bootstrap.css`, emits once with dispatch enabled and once with a per-grammar flag disabling it, asserts byte-identical typed-AST projections).

**Hard gate**: `bootstrap.css` declaration record count drops ≥ 30%; `shape_ref_view_parity` test passes; `GRAMMAR_PROFILE.shape_dict` non-empty for CSS L4 + JSON Value tree + BBNF rule shape.

#### W6.2 PHF keyword tables (universal)
Owner: `crates/core/src/backend/rust/emitter/keyword_dispatch.rs` (new — emitter pass); `crates/ir/src/passes/recognizers/keyword_stats.rs` (new — mining pass).

Emitter mines **every Alt-with-literal-branches** pattern across the grammar (CSS 163-branch namedColor + 72-branch keywords + 92-branch properties; BBNF 8-directive prefixes + keyword Alts; JSON `true`/`false`/`null` + delimiter dispatch + string-escape table; Sheets 150 function names + operator Alts). Threshold gates emission; the *mechanism* is grammar-agnostic per invariant 6.

```rust
pub fn emit_keyword_phf(
    grammar: &str,                // symbol-prefix only
    rule_id: RuleId,
    branches: &[LiteralBranch],   // mined IR fact
) -> Option<TokenStream>;
// Returns None when |branches| < threshold; mechanism is general.
```

Walker's `AltLinear` arm in the W4-emitted specialised walker consults the PHF directly — one PHF lookup instead of N-branch linear scan or byte-dispatch.

**Hard gate**: `grep -rn 'const [A-Z_]*: \[&\[u8\]' crates/core/src/backend/rust/emitter/` returns 0 (every keyword table PHF-routed). At least one PHF table per primary grammar (CSS, BBNF, Sheets, JSON).

#### W6.3 `DtaState::ClassifyByte` LUT
Owner: `crates/core/src/backend/rust/emitter/classify_byte.rs` (new); `crates/bbnf-tape/src/dta.rs` (new `DtaState::ClassifyByte` variant + walker arm); `crates/ir/src/passes/recognizers/disjoint_first.rs` (new mining pass).

General mechanism for ANY Alt with mutually-disjoint FIRST sets. CSS `compoundSelector` (5-way) + BBNF `directive` (`@`-prefix second-byte) + JSON escape (`\` second-byte) + Sheets function first-letter — same mechanism, different workload density per invariant 6. Renamed from "selector classifier" to shed CSS bias.

```rust
DtaState::ClassifyByte {
    table: &'static [DtaStateId; 256],  // emitted at codegen time
    fallback: DtaStateId,
}
```

Walker arm reads `table[input[pos]]` and dispatches in one indexed load. The W4 walker-specialisation pass inlines this directly; the variant is the wire contract for the table emission.

**Hard gate**: samply on bootstrap + tailwind shows `__compoundSelector` self-time < 15% (was 33–43% pre-AU). At least one `ClassifyByte` table emitted per primary grammar where mining identifies disjoint-FIRST Alt.

#### W6.4 Direct-to-struct expansion (universal)
Owner: `crates/core/src/backend/rust/view/named_types.rs` (existing — extend resolver); `crates/core/src/backend/rust/emitter/grammar.rs` (extend `emit_view_impl`).

Extend beyond CSS Color (current sole consumer) to every named-type with fixed layout: JSON `Value` tree, BBNF AST (`RuleEntry` + `RhsNode` + directive variants), Sheets formula AST (`Expr` + `Literal` + `Cell` + `FnCall`). The universal resolver mines `TypeDesc::Named` annotations from the grammar and emits the projection without per-grammar opt-in. Any future grammar with a top-level named type automatically enters the fast path.

```rust
pub fn resolve_named_type(
    type_desc: &TypeDesc,         // mined IR fact
) -> Option<NamedTypeBinding>;
// Returns None when the type isn't fixed-layout; emitter falls through to
// the generic `view().as_value()` accessor.
```

**Hard gate**: every primary grammar's top-level type projects via the resolver; `crates/core/tests/{json_value_parity, css_color_parity, bbnf_ast_parity, sheets_expr_parity}.rs` show field-for-field equivalence with hand-written reference projections.

#### W6.5 Per-grammar Pratt const-fold
Owner: `crates/ir/src/passes/recognizers/operator_chain.rs` (extend); `crates/core/src/backend/rust/emitter/precedence.rs` (new).

W1.7 landed the `IrNode::Next` peel; W6.5 completes the calibration. Per-grammar `PRECEDENCE_LUT: [u8; 256]` packed as `prec(4b) | assoc(1b) | arity(2b) | two_byte(1b)` plus sparse `&'static [DtaPrecedenceEntry]` for second-byte + op_rule + discriminant. Mining pass extracts operator precedence from the grammar IR (CSS 148 operators + BBNF value_expr tower + Sheets arithmetic).

The W4-emitted specialised walker's `ShuntingYard` arm consults the emitted LUT, not hardcoded per-grammar dispatch. Healing `test_let_parses_as_let_call` (Sheets dispatch surface) — the Pratt reducer subsumes the LET/IF/LAMBDA dispatch; un-ignore the test as cascade.

**Hard gate**: CSS `calc(2 * (3 + 4))` produces correct AST shape; BBNF `value_or` tower produces correct associativity; `test_let_parses_as_let_call` un-ignored + passing; per-grammar `PRECEDENCE_LUT` const populated for every grammar with mined operators.

Each consumer is a separate file/module; agents partition by file bound (no overlap).

**19-entry bench matrix.** Cold per-parse, mimalloc, sequential. Every entry from the post-AV reality-check matrix measured.

**FINAL.** `docs/tranches/AW/FINAL-III.md` with hard-gate attribution per phase. `docs/benchmarks/post-AW-III.json` with 19-entry matrix + multi-wave history. `docs/tranches/AW/PROGRESS.md` complete.

**Hard gate**: **strict-better-than post-AU on ≥ 15/19 entries**. The three sheets-small entries (`parse_simple` 505 B, `parse_nested` 1.5 KB, `parse_stress` 1.8 KB) are documented small-input tradeoff (stage-1 amortisation; not a per-grammar specialisation, a cardinality fact). All other entries strictly better. Workspace test 0 failures; ignored count = dispositioned-routed-residual only.

## Cross-tranche debt inherited from AW-II

| Item | Origin | AW-III wave |
|------|--------|-------------|
| Cluster A (13 parse failures) | AW-II.W5c residuals | W2 |
| Cluster C (37 payload activation) | AW-II.W5c residuals; root cause diagnosed | W1 |
| Cluster D (1 integration: test_large_grammar) | AW-II.W5c residuals | W2 or W3 |
| 67 ignored tests | accumulated across AW-series | W3 |
| 5 blocked bench entries (data_s, canada, tailwind) | AW-II.W5 bench matrix | W6 (after W2 closure) |
| `serialize_roundtrip::css_simple` ignore | AW-I.W2.5 carry | W3 (CLOSE batch) |

## Cross-tranche debt deferred to AW-IV (granular exceed)

| Item | Origin | AW-IV wave |
|------|--------|-------------|
| AVX2 u8x32 widening (arch-gated x86_64 tuning) | AN.5 chronic | AW-IV W1 |
| Scanner PaddedView migration + scanner cluster consolidation + NEON 17-digit | CO-E2 / AR.6.x / AT.4.3 chronics | AW-IV W2 |
| Bloom + GADT runtime dedup + grammar-level pattern hoisting | AP.4.2 chronic | AW-IV W3 |
| Document-parallel fork over the stage-1 structural index | substrate landed in AW-III.W5 | AW-IV W4 |
| `Tape::reduce_column<C, R>` visitor + 4-lane SIMD pack | AV.2.5 substrate | AW-IV W5 |
| sonic-rs + lightningcss parity harnesses | competitor parity | AW-IV W5 |
| Cost-model grid sweep (egraph CostWeights calibration) | AM.6 chronic | AW-IV W3 or W4 |

## Operational posture

Inherits `docs/instructions/README.md` + `docs/instructions/TRANCHE_SPEC.md` in full.

Specific notes:

- **No deferrals, regardless of newfound scope.** Reiterated from invariant 7. If a wave reveals more work, the work belongs to that wave — re-plan-with-more-agents per the operational protocol's parallel-orchestration contract; never silent forward-routing.
- **Producer-side surfaces in scope at all waves.**
- **`#[ignore]` discipline**: audit-then-close. Never add a new ignore; never leave an existing one un-dispositioned.
- **Bootstrap regen permitted at any wave boundary.** Idempotency verified at every regen. W1 + W4 + W5 likely candidates; orchestrator signs off on each.
- **Profiling discipline**: every performance claim cites a samply profile per `docs/instructions/PROFILING.md`. No speculative throughput numbers.
- **`cargo asm` discipline**: every codegen-specialisation claim cites a `cargo asm` artefact showing the expected output (or the absence of an unexpected symbol).

## Research artefacts

AW-III opened with an 8-agent pre-plan research wave at 2026-04-17:

- `docs/tranches/AW/research/perf-01-json.md` — samply json_monolithic
- `docs/tranches/AW/research/perf-02-css.md` — samply css_l4
- `docs/tranches/AW/research/perf-03-sheets.md` — samply sheets
- `docs/tranches/AW/research/perf-04-bbnf.md` — samply bbnf_monolithic
- `docs/tranches/AW/research/perf-05-json-value.md` — bbnf-vs-sonic twin pair
- `docs/tranches/AW/research/perf-06-code-audit.md` — lever firing audit
- `docs/tranches/AW/research/residuals-triage.md` — 50 failing tests categorised
- `docs/tranches/AW/research/ignores-audit.md` — 58 ignored tests dispositioned
- `docs/tranches/AW/research/SYNTHESIS.md` — first-pass synthesis
- `docs/tranches/AW/research/arch-comparison.md` — DTA vs simdjson/sonic-rs/RD

A follow-up six-agent wave at 2026-04-17 produced the architectural transposition synthesis:

- `docs/tranches/AW/research/aw3-r1-simdjson-cycle-attribution.md`
- `docs/tranches/AW/research/aw3-r2-stage1-simd-bitmap.md` — full archaeology + canonical design
- `docs/tranches/AW/research/aw3-r3-codegen-walker-proof.md`
- `docs/tranches/AW/research/aw3-r4-cycle-accounting.md`
- `docs/tranches/AW/research/aw3-r5-path-a-keep-dta.md`
- `docs/tranches/AW/research/aw3-r6-path-b-rip-dta.md` — devil's-advocate; rejected for §6
- `docs/tranches/AW/research/SYNTHESIS-2-PATH-FORWARD.md`

These artefacts pre-stage AW-III's wave schedule above.

## Successor chain

AW-III closes green → AW-IV opens (granular exceed + parity harnesses). AX inherits the same substrate (DTA_TABLE const + DtaSnapshot + decision log) verbatim; stage-1 bitmap is deterministic, replay re-derives.

Indefatigable. No deferrals. No stubs. No shims. No new `#[ignore]`. No grammar-specific code paths. Architectural transposition ships in this tranche.
