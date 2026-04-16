# R02 — ShapeRef runtime dispatch (W2.3)

Seed invariants (AW §Architectural invariants 1–2): one codegen
path; `push_shape_ref` folds into the DTA stage-A compound-emit
branch, not a parallel path; view-layer synthetic children project
byte-identical typed output against the non-collapsed compound.

Bootstrap corpus: `data/css/bootstrap.css` is 280 311 B, 6 014
colons (declaration-shape upper bound). Each declaration today fans
to ~5–7 tape records (`Rule → Seq → {propertyName Span, ':', Span
value, ';'?}`); AV.5 target collapses to one ShapeRef.

Record layout (`TapeRec`, 16 B packed; SoA 20 B across 7 slots,
`tape.rs:112`). ShapeRef uses: `kind_meta` low 4b = 13; `flags`
low 5b = `shape_dict_idx`, bit 5 = `has_payload`; `span_lo/hi` =
covered region; `sib_skip` = next-sibling distance; `child_off` =
`pay_agg` byte offset of packed blob.

## 1. Cost model per compound emit

Hot-path sequence:

```
1. local_hash ← compile-time constant baked into the DTA state
2. scan SHAPE_DICT[0..N].shape_hash for equality
3. hit  → push_shape_ref(span, i, &packed_payload)     [1 record]
   miss → push_compound + per-child pushes              [5–7 records]
```

`local_hash` is emitter-baked per state (from `ShapeTemplate::
shape_hash`); parse-time recomputation is zero.

Cycle accounting:

`MAX_SHAPE_DICT_ENTRIES = 32` (5-bit `shape_dict_idx`, `csp_strategy/
constraints/shape_dict.rs:55`). Steady state ≤ 5 entries per grammar
(CSS-L4 declaration + substructures; JSON null/emptyObject/
emptyArray; BBNF big_comment + mapped_factor_empty).

| Op | Cycles |
|----|--------|
| `SHAPE_DICT[i].shape_hash` load (L1) | 4–5 |
| `cmp` with immediate, predicted branch | 1–2 |
| **Per iteration** | ~5 |
| **Worst scan, 5 entries all miss** | **~25** |
| `push_shape_ref` (6 column writes + 8–40 B memcpy into `pay_agg`) | ~40 |
| **Hit total** | **~50** |
| **Miss added-cost over baseline** | **~25** |

Baseline compound-run: 5–7 records × `push_structural` at ~12 cyc
each = **~60–84 cycles** of column stores, plus the call-site
overhead from the per-rule helper.

### Break-even

Let `p` = hit-rate at a dict-gated compound-emit site:

```
expected = p·50 + (1 − p)·(25 + 72) = 97 − 47·p     baseline = 72
break-even: p > (97 − 72)/47 ≈ 0.53
```

For CSS-L4 `declaration` the 26 declDispatch branches all share the
`propertyName ":" ?w value impSuffix ";"?` skeleton → `p ≈ 1.0` per
declaration class after mining, a full win. For non-admitted sites
(`blockContent`, `atRule`), the emitter elides the scan entirely
at codegen (states with no `ShapeTemplate` candidate don't emit the
dispatch): `p = 0` sites pay 0 cycles, not 25.

## 2. `shape_hash` collision strategy

Hash: 64-bit FxHasher over `TemplatePiece` discriminants +
`StringId` + `TypeDesc` discriminants (`recognizers/shape_dict.
rs:344`). FxHash (xor-rotate-multiply) has no cryptographic
guarantee; at 10 K entries: 10 K²/2·2⁶⁴ ≈ **2.7·10⁻¹¹** — not a
runtime concern.

`solve_shape_dict_selection` already deduplicates admissions by
hash (`.retain(…seen_hashes.insert(hash))` at `constraints/
shape_dict.rs:111`) — within one grammar's admitted 32, hash is
injective by construction. The residual risk is two *distinct*
templates with identical hash arriving from miner output before
dedup picks the higher-scoring — the dedup then silently drops the
loser without detection.

**Recommendation: strict-injective emitter-time assertion.** Three
lines in `emit_shape_dict_arrays`:

```rust
let mut sorted: Vec<_> = selection.iter().map(|i|
    ir.shape_dict_templates[*i].1.shape_hash).collect();
sorted.sort_unstable();
for w in sorted.windows(2) { assert_ne!(w[0], w[1],
    "shape_hash collision in admitted dict — perturb template"); }
```

Emitter panics on collision → grammar author perturbs the template
(adds a discriminator piece). Zero runtime cost.

Rejected — runtime `columns_range_eq` confirm (analogous to
AV.6.1's bloom confirm): adds 20–40 cyc/hit, halves §1. Bloom
needs it because `body_hash` is runtime-populated over unbounded
instance-variable bytes; ShapeRef's universe is compile-time-
bounded at 32, injectivity is enforceable at codegen.

## 3. Dict size & I-cache pressure

Per `ShapeEntry` (`bbnf-tape/src/profile.rs:84`):

```
shape_hash: u64                8 B
rule: RuleId(u32)              4 B
child_kinds: &[u8]            16 B (ptr+len)
leaf_payload_offsets: &[u16]  16 B (ptr+len)
payload_bytes: u16             2 B    + 6 B pad → 48 B + ~25 B backing arrays ≈ 75 B
```

Upper bound per grammar: 32 × 75 B ≈ **2.4 KiB in `.rodata`**. L1d
(M-series 192 KiB, Skylake 32–48 KiB) fits 13–80× over. Dispatch
loop fits one 32 B fetch block; I-cache pressure nil. Table is hot
every parse (cache-warm after first declaration), not once per
stylesheet.

`pay_agg` per-instance cost: 8 B (span pair) + 0–8 B (typed hole
per `TypeDesc`) per leaf hole. For `customPropertyDecl` (2 leaf
holes): 16 B × 6 014 decls = **96 KiB of pay_agg** vs. baseline 6 ×
16 B × 6 014 = **577 KiB TapeRec**. Net tape footprint drops
**~481 KiB on bootstrap** — into L2 from overflowing L1.

## 4. View-layer expansion correctness

`ShapeRefSyntheticChild` (`bbnf-tape/src/cursor.rs:413`) yields one
pseudo-cursor per skeleton position. Obligation: pre-order walk
produces byte-identical typed output to the equivalent compound.

- **frame_depth parity.** ShapeRef sits where the collapsed
  compound's top sat; its `frame_depth[i]` is parent + 1. Synthetic
  children don't carry `frame_depth`, but view-layer never reads
  `frame_depth` off children — it is Stage-C-only. Vacuous parity.
- **sib_skip parity.** ShapeRef's own `sib_skip` is finaliser-
  stamped identically to any leaf (points past ShapeRef to next real
  sibling). Synthetic children have no column slot; `ShapeRefChild
  Iter` iterates by `child_idx < entry.child_kinds.len()` — bounded
  by the skeleton length, matching the normal-compound's
  `sib_skip == 0` termination because both bound by the same
  template-length invariant.
- **Span aggregation.** Parent ShapeRef carries the compound outer
  span. Leaf-hole children read per-instance span from `pay_agg[
  payload_start + offset..]` (two LE u32, `cursor.rs:449`).
  Structural children (literal, whitespace) inherit parent span
  (`cursor.rs:470`) — correct because the original compound's
  structural positions were substring `Span` leaves whose typed
  output is fully determined by the template's `TemplatePiece::
  Literal(StringId)` at that index plus the parent bounds.
- **Payload column plumbing.** Leaf-hole typed payload (e.g. a
  `Ref(colorFunction) → LargeAggregate Color`) sits in `pay_agg` at
  `payload_offset + 8`. The existing `Tape::payload_bytes(offset,
  len)` accessor keyed by absolute offset is identical to the
  normal-compound path — ShapeRef reuses it unchanged, no fork.

What *could* break and how the iterator precludes: (i) skeleton-
order drift — `entry.child_kinds` emitted in source order (dta.rs:
546) matches miner `walk_compound_children`; (ii) mistyped hole —
`leaf_payload_offsets[i] == u16::MAX` discriminates structural vs.
hole (cursor.rs:442), matching miner `TemplatePiece::LeafHole`
(shape_dict.rs:252); (iii) typed-projection drift — hole `TypeDesc`
via `collect_leaf_holes` reads the same `ir.types` table the non-
collapsed path consumes (shape_dict.rs:284).

## 5. Admission / population pipeline

1. **IR pass** (`ir/src/passes/recognizers/shape_dict.rs`). Folds
   into `mine_recognizers`, emits `(NodeId, ShapeTemplate)` pairs.
   Eligibility: local `is_fixed_shape` + lattice-populated
   `closure_free`.
2. **CSP admission** (`constraints/shape_dict.rs::
   solve_shape_dict_selection`). Greedy top-N over scored
   candidates (`freq × savings − static_cost`). Writes
   `ir.shape_dict_selection: Vec<usize>`.
3. **Emitter** (`backend/rust/emitter/dta.rs::emit_shape_dict_
   arrays`). Bakes `pub const SHAPE_DICT: &[ShapeEntry]`.

**W2.3's addition**: splice `GrammarProfile.shape_dict = SHAPE_DICT`
in `emitter/profile.rs:145` (currently `&[]`). `active_columns`
(same matrix row, `PROGRESS.md:91`) splices from the columns the
emitted ShapeRef leaf-hole typed payloads require. Per AW.0.9, the
wave that consumes populates — W2.3 owns both slots.

## 6. Interaction with bloom + GADT dedup (W4.4)

Disjoint compound classes, shared admission-gate. **ShapeRef**
handles fixed-shape compounds with typed leaf holes (CSS
declaration, BBNF big_comment, JSON pair-with-scalar, JSON
null/emptyObject/emptyArray); hash universe ≤ 32 compile-time per
grammar; dedup axis = whole subtree → 1 record. **Bloom+GADT**
handles instance-content-variable compounds where raw bytes recur
(`border: 0`, `color: #fff`); hash universe = unbounded runtime
`body_hash`; dedup axis = 2nd+ occurrence → back-ref.

**Compose, don't overlap.** A ShapeRef-collapsed declaration is
itself a leaf. Bloom+GADT runs over leaves too: two `background-
color: red` instances both emit as ShapeRef with identical packed
payloads; bloom+GADT deduplicates the **packed payload blob** in
`pay_agg` — second emit reuses first's `child_off`. One codegen
path, two orthogonal reductions stacking (AV.6's 280k→180k→150k).

**Shared admission**: both read `EClassFacts.closure_free` and
`all_descendants_elidable` (`recognizers/shape_dict.rs:174`;
AV.6.2). **Different emitter channels**: ShapeRef → `SHAPE_DICT`
(compile-time table); bloom+GADT → `dedup_eligible_rules` slot +
runtime bloom/FxHashMap scaffold (AV.6.1).

## 7. Parity test design (W2.4)

`crates/core/tests/shape_ref_view_parity.rs`:

1. Parse bootstrap.css with `SHAPE_DICT` populated (post-W2.3
   default) → `tape_a`.
2. Parse bootstrap.css with a test-only const override
   `SHAPE_DICT: &[] = &[]` → `tape_b`.
3. Walk `tape_a` and `tape_b`'s declaration iterators in lockstep;
   compare typed projections: `property().as_str()`, `value().
   as_str()`, `is_important()`.
4. Record-count assertion validates AV.5.5's "5–7 → 1" projection:
   `tape_a.record_count() ≤ tape_b.record_count() × 0.70` (≥ 30 %
   drop, matching AW W2 gate 17).

Zero divergences on the 6 014 declarations → W2.4 green. Runtime
< 3 s (bootstrap parse at ≥ 100 MB/s × 280 KB × 2 passes ≈ 6 ms of
parse; remainder walk).

## Citations

- `crates/bbnf-tape/src/kind.rs:117` — `TapeKind::ShapeRef = 13`
- `crates/bbnf-tape/src/builder.rs:542` — `push_shape_ref`
- `crates/bbnf-tape/src/cursor.rs:293` — `shape_ref_children`
- `crates/bbnf-tape/src/cursor.rs:413` — `ShapeRefSyntheticChild`
- `crates/bbnf-tape/src/profile.rs:84` — `ShapeEntry` layout
- `crates/bbnf-tape/src/tape.rs:112` — `size_of::<TapeRec>() == 16`
- `crates/ir/src/passes/recognizers/shape_dict.rs:107` —
  `ShapeDictMiner`
- `crates/ir/src/passes/recognizers/shape_dict.rs:344` —
  `hash_skeleton` (FxHasher 64-bit)
- `crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs:55`
  — `MAX_SHAPE_DICT_ENTRIES = 32`
- `crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs:81`
  — `solve_shape_dict_selection`
- `crates/core/src/backend/rust/emitter/dta.rs:492` —
  `emit_shape_dict_arrays`
- `crates/core/src/backend/rust/emitter/profile.rs:145` —
  `shape_dict: &[]` stub (W2.3 splices)
- `grammar/css/l4/properties.bbnf:209` — `declaration` dispatch
- `docs/tranches/AV/AV.md:877` — AV.5.4 DTA-emits-ShapeRef
- `docs/tranches/AW/AW.md:893` — AW.2.3 runtime dispatch
- `docs/tranches/AW/PROGRESS.md:91` — GrammarProfile matrix, W2.3
- `data/css/bootstrap.css` — 280 311 B, 6 014 colons
