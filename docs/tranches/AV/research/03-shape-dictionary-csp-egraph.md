# Research 03 — Shape-Dictionary Tape (CSP + e-graph + structural mining)

*Verbatim deliverable from architecture research agent, April 2026.
Backs AV Phases 5–6 (ShapeDictionary for CSS and BBNF) and provides
the compile-time half of the optional runtime dedup in AV Phase 10.*

---

# Angle: Shape-Dictionary Tape — Structural Mining + E-graph + CSP for Globally-Informed, Variable-Length Records

## Motivation

Wave-2 profiling (`docs/tranches/AU/profiling-2.md`) exposes a uniform pattern that the current fixed 16 B `TapeRec` cannot exploit. CSS L4 emits **234 `push_compound`** across three hot rules (`__compoundSelector`, `__declaration`, `__value`), and `TapeBuilder::push_compound` itself is ≤ 0.63 % — the cost lives *inside* rule bodies whose arms repeat the same structural skeleton thousands of times (see `crates/core/src/grammar/generated.rs:14143,15181,15407,15896,17569`). Sheets emits **37 compounds and 0 leaves** — 100 % compound churn through a precedence tower where every level pushes a Repeat-wrapped Rule even when its operator never appears. The tape pays a full 16 B record per Repeat-Rule pair regardless of semantic content.

Meanwhile, the infrastructure to detect this is already in place and sitting dormant relative to the tape:

- `crates/ir/src/egraph/analysis/facts.rs` carries `EClassFacts { is_fixed_shape, elision_safe, closure_free, all_descendants_elidable, width: WidthBound }` — a **per-e-class, hash-consed, monotone lattice** that already tells us which classes have exactly one structural shape.
- `crates/ir/src/passes/recognizers/mod.rs` runs ten miners in a single walk and produces `RecognizerSignature { shape_hash: u64 }` (see `signature.rs:22`) — canonical shape hashes are already computed per NodeId.
- `crates/ir/src/passes/payload/layout.rs` already plans 16 B aggregates from `TypeDesc::Tuple` via natural alignment.
- `crates/ir/src/passes/csp_strategy/mod.rs` runs a real `csp_solver::Csp` with `MinimizeCost` branch-and-bound and a `StrategyDomain` that jointly optimizes `{AltMode, WrapMode, RegexEngine, MaterializationClass}` (see line 258). This is the hook — adding a fifth decision family is one enum variant + one constraint file.
- `crates/bbnf-tape/src/kind.rs:109` already reserves `TapeKind::Reserved = 15` explicitly for "grammar-specific shapes: structural bitmap dispatch, keyword PHF lookup".

The seed itself admits one bit layout cannot serve JSON's 8-compound / 3-typed-leaf grammar and CSS's 234-compound / 7-typed-leaf grammar optimally. We need per-grammar variable-length records, but the architectural invariant is one access API — which is exactly what a trait-indirected `#[repr(C)]` per-grammar struct plus a shape dictionary gives us.

## The novel idea — `ShapeRef` tape records backed by a CSP-planned, e-graph-indexed shape dictionary

Build a compile-time **ShapeDictionary**: a per-grammar table of recurring `(skeleton, payload_layout)` templates harvested from the e-graph's `RecognizerSignature.shape_hash` over classes where `EClassFacts.is_fixed_shape` holds. At parse time, any rule whose subtree matches a dictionary entry emits a single **`ShapeRef` leaf** instead of the full compound + children run. The children are reconstructed lazily at `.view()` time by the skeleton template; values are unpacked from the packed payload.

### Per-grammar `TapeRec` schema (chosen by CSP)

One enum variant joins `StrategyValue`:

```rust
// crates/ir/src/passes/csp_strategy/mod.rs — addition
enum LayoutMode { Uniform16, PackedDimScalar9, PackedRuleCompound12, ShapeRef8 }
```

The CSP cost function picks per-grammar layouts with these dimensions (every number grounded in wave-2 measurements):

| dim | var | domain | cost term |
|-----|-----|--------|-----------|
| record width | `W` | {8, 12, 16, 20} B | cache-line crosses = `(W - gcd(W,64))/64` records × compound-share |
| span width | `(u32, u32)` vs `(u32, u16 len)` | 2 values | `span_overflow_prob × recovery_cost` |
| variant bits | 3/4/6 | 3 values | `log2(max_variant_count)` — JSON 8 rules = 3 b; CSS 234 = 8 b but per-alt max ≤ 16 = 4 b |
| payload addressing | inline-in-child_off vs arena-offset vs shape-dict-idx | 3 values | `P(payload ≤ 32 b) × inline_savings + P(repeat-shape) × dict_savings` |

For JSON the solver picks `W = 12 B` with `(span_lo u32, span_hi u32, kind_var u8, flags u8, inline_payload u16)` — number's f64 is out-of-line in the arena, but bool/u8/null (the 83 % path) inline directly. Record density per 64 B cache line doubles from 4 → 5.3.

For CSS the solver picks `W = 16 B` preserved **but** adds the `ShapeRef` variant for the 234 compound sites: the skeleton `Rule(declaration) → Seq[propertyName, colon, ws, value, semi]` recurs 5000+ times across `bootstrap.css`. A `ShapeRef` record is:

```
bits  0..3   : TapeKind = ShapeRef (new, slot 13)
bits  4..7   : reserved
bits  8..15  : shape_dict_idx (u8, 256 entries/grammar, plenty)
bits 16..47  : arena offset for the packed per-instance payload blob (u32)
bits 48..95  : span (u32 lo, u32 hi)
```

The packed payload blob holds **only the non-constant leaf spans/payloads** of that shape. The compile-time skeleton says where each goes. For `declaration`, that's `(propertyName: Span 8 B, value: TapeOffset 4 B)` = 12 B instead of a Rule record + 5 child records + their trees.

### ShapeDictionary construction — e-graph + structural mining + CSP

1. **Candidate harvest (e-graph).** Walk every e-class `c` in `EGraph<GrammarENode, GrammarAnalysis>`. For classes where `data.is_fixed_shape && data.all_descendants_elidable == false` and `MaterializationClass == MustTape`, emit a candidate `ShapeTemplate { skeleton: ENode, leaf_holes: Vec<TypeDesc>, shape_hash: u64 }`. Hash-consing gives us deduplication for free — two syntactically identical subtrees land in the same e-class. This is the on-the-fly dictionary-coding variant the angle asked for.
2. **Frequency estimate.** For every candidate, approximate input-normalized emission frequency using the grammar's control-flow statically: `freq(c) ≈ Π over ancestors of (1/alt-arity) × repeat-unbounded? 1 : avg-count`. A cheap, deterministic estimate; refineable from saved `.profiles/samply/<bench>/<entry>/profile.json.syms.json` later.
3. **CSP selection.** Add a dictionary-selection CSP per component with variables `x_c ∈ {include, exclude}` and cost `-freq(c) × savings(c) + static_entry_cost`. `savings(c) = (records_elided × 16) - packed_payload_size(c)`. Constraint: `Σ include ≤ 256` (the u8 dict index). Branch-and-bound via the existing `OptimizationMode::MinimizeCost` path — **one new file `constraints/shape_dict.rs`** alongside `engine/parent/tier` (the Tranche AF.3 seam). No new solver infrastructure; it's the first production use of a pluggable shape-selection cost model.
4. **Codegen bake.** Dictionary → `const SHAPE_DICT: [ShapeEntry; N]` in the generated parser. The rule body for an included template emits `push_shape_ref(idx, arena_pack(...))`; the view layer expands it lazily when children are walked.

### Interaction with each existing subsystem — concrete

- **E-graph (`crates/ir/src/egraph/analysis/mod.rs`)** — already computes `shape_hash` via `RecognizerSignature` and `is_fixed_shape` via `EClassFacts.merge`. We add a `shape_template_id` field to `EClassFacts` (monotone: `Some(id) ∧ Some(id') → keep iff equal else None`). No new traversal.
- **Structural mining (`crates/ir/src/passes/recognizers`)** — `mine_recognizers` already produces `RecognizerShape::DelimiterBalanced`, `SeparatorList`, `TokenLedBranches`, `KeywordPrefix`, `PunctWsRegion`. These ARE the dictionary's natural population. A new `ShapeDictMiner` folds into the single-walk substrate (one file, 150 LOC) and emits `(NodeId, ShapeTemplate)` into `MineOutputs`.
- **CSP (`csp_solver`)** — reuses `StrategyDomain` + `CostDomainEval` + `OptimizationMode::MinimizeCost` via the `constraints/` seam. `ImplicationConstraint`: "if a node is `ShapeRef`-eligible, its parent's `MaterializationClass` must ≠ `TransparentElide`" (elision and shape-ref are mutually exclusive alternatives). This constraint is already expressible as an `ImplicationConstraint` — the substrate is the one whose first non-trivial use was the AltMode/RegexEngine binding in AF.3.
- **Tape / TapeBuilder** — ONE unified access API per the invariants. `TapeKind::ShapeRef` joins the existing `is_leaf()` set; cursor child iteration checks for it and, when encountered, iterates the *expanded* children over `(skeleton_template, packed_payload)`. One `push_shape_ref(kind, span_lo, span_hi, dict_idx, packed_offset)` method alongside the existing ten `push_leaf_with_*`. AU.6.7's unified-arena work **is the packed-payload backing store** — this proposal subsumes it, not replaces it.
- **GrammarProfile pool (angle 5).** A small typed struct passed to `TapeBuilder::with_capacity`: `{ compound_per_input_byte: f32, leaf_per_input_byte: f32, avg_packed_payload_bytes: u16, shape_dict_size: u8 }` — one instance per grammar, baked at codegen time. JSON `compound/byte ≈ 0.12`, Sheets `≈ 1.1`, CSS `≈ 0.17`. Replaces the universal `input.len() / 2 + 2` (wave-2 measured 10–22 % `_mi_heap_realloc_zero` tails on Sheets from this miscalibration). Same struct seeds arena capacity and dispatch table sizing — the "pool codegen fingerprint data" angle in one struct.

## Risks and required support

1. **View layer complexity**. Lazy expansion of `ShapeRef` must not fork the cursor API. Risk: the cursor's `children()` method in `crates/bbnf-tape/src/cursor.rs` grows a branch. Mitigation: keep the branch in one method with a static template table lookup; no other accessor changes. The invariant "one access API" is preserved — callers still see `.children()`.
2. **Dispatch cost on the view side**. Each `ShapeRef` hit adds one table lookup. Since `ShapeRef` records *replace* N compound/leaf records (measured 5–7 children per CSS declaration), the lookup amortizes. The cost model explicitly accounts for this in `savings(c)`.
3. **Dictionary staleness**. If the grammar changes, the dictionary needs regeneration. Given the `clean-regen-discipline` memory feedback, this is a pure compile-time regenerable artifact — no hand-patching ever. Identical lifecycle to `generated.rs`.
4. **Skeleton expressivity**. Templates must carry leaf-hole types faithfully; any `->` annotation inside a template must be preserved through packing and unpacking. Because `EClassFacts.closure_free` is already a prerequisite for inclusion, closure-carrying nodes never enter the dictionary. Invariant 2 (`every -> reaches the tape emitter`) holds — the leaf-hole's TypeDesc comes from the same `project_types` output that drives `PayloadLayout`.
5. **Codebase support needed**: (a) one new `TapeKind::ShapeRef = 13`, (b) one new `StrategyValue::ShapeRef(Option<u8>)` variant, (c) one new constraint file `constraints/shape_dict.rs`, (d) one new miner `shape_dict.rs` alongside the ten existing. ShapeTemplate representation can live on `GrammarIR` as `HashMap<ShapeTemplateId, ShapeTemplate>` alongside the existing `payload_layouts`.

## Estimated impact against wave-2 hotspots

- **CSS L4 bootstrap / tailwind**: `__declaration` (17–31 % self) emits one compound + one Seq + 3–5 leaves per declaration. The shape `decl = propName : value ;` dominates. A `ShapeRef` collapse replaces ~5 records (80 B) with one 16 B record + ≤ 24 B packed payload — ~40 % record count drop on declaration-dense stylesheets. Closes the AU.2 bootstrap gate (≥ 600 MB/s) and likely opens 700–800 MB/s territory on tailwind.
- **Sheets precedence tower** (56–86 % self): every level's unconditional Repeat-Rule pair has a shape hash that occurs at *every* input byte. With `ShapeRef`, a tower traversal that never matched an operator emits **one** `ShapeRef(IdleTower)` leaf instead of 6×2 = 12 compounds. This is the lever AU.6.3's "Pratt flattening" gestures at but avoids the special-case handling — the CSP discovers it.
- **JSON `__value`** (83 % canada): the shape `value → number` is fixed once `AU.1.1` lands. Include it in the dictionary; the current `push_leaf_with_f64` becomes `push_shape_ref(SHAPE_VALUE_NUMBER, f64_bytes)`. Minor win here (one record already), but the pattern generalizes to `pair = key : value` in object-heavy datasets (twitter/data_xl) where a 2-leaf shape collapses to one — recovers the 9–17 % `memchr` + whitespace-scan mass that rides on object membership bookkeeping.
- **BBNF `__big_comment`** (9–15 % self, 6 grammars): the shape is literally `Rule(memchr-span)` — the canonical one-hole template. A single dict entry, compiled statically, drops three records to one uniformly. Subsumes AU.6.9 without a one-off code path.

The proposal is one integrated system: e-graph facts *identify* fixed shapes, structural mining *names* them, the CSP *selects* which to admit to the dictionary under a byte budget, the tape materializes them as `ShapeRef` leaves, and the view reconstitutes — one API, one layout enum, no orthogonal code paths, and every `->` annotation carried through via the existing `PayloadLayout` planner.
