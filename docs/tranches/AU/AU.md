# Tranche AU — Projection Activation, Regression Redress, Scanner Truth

## Current state (post-AT audit)

AT landed 12 commits across 7 phases. Multi-type payload projection
implemented, SIMD guard fixed, meta_idx folded into TapeRec, CSS spec
gaps closed, structural tests added. However:

### AT landed correctly

| Feature | Status | Evidence |
|---------|--------|----------|
| `resolve_branch_type` | **WORKING** | Walks inlined Map/Constant/FnDescriptor nodes |
| Multi-type `__payload_tag` | **WORKING** | JSON value declares f64+bool+u8, match dispatch codegen |
| SIMD guard (integer digits) | **WORKING** | parse-that commit 44ae43b |
| meta_idx → kind_meta packing | **WORKING** | 5-bit meta, meta Vec eliminated, 28 tape tests |
| CSS `\|=` attr selector | **WORKING** | `attrName` rule with 4-branch disambiguation |
| CSS Unicode identifiers | **WORKING** | `\x80-\xff` byte ranges in ident regexes |
| `\xHH` hex escape in bbnf-regex | **WORKING** | character class byte-range support |
| Structural tests | **WORKING** | 17 deep-walk tests, bench validation helpers |
| Dead code cleanup | **DONE** | -166 LOC, 10 warnings eliminated |
| Tape parity fixtures | **DONE** | 22/22 pass |

### What AT broke or failed to activate

#### Critical bug 1 — `branch_pushes_children()` misclassifies leaf branches

**File**: `crates/core/src/backend/driver/alt.rs:67-69`

```rust
// Seq, Alt, Repeat, Skip, Next, Minus, TokenDispatch — all
// structurally push children.
_ => true,
```

After rule fusing, JSON's `bool` rule becomes a nested
`Alt([Map(Literal("true"), ...), Map(Literal("false"), ...)])` inside
`value`'s body. The `_ => true` catch-all classifies this nested Alt
as compound, forcing `mark_children + push_compound`. Since tape
surgery propagates compound classification to ALL branches of the
outer Alt, every branch (including number, string, null) gets
`__has_children = true`.

**Consequence**: ALL payload captures in `__value` are dead stores.
The epilogue always takes `push_compound`. `push_leaf_with_f64`,
`push_leaf_with_bool`, `push_leaf_with_u8` are never called. Typed
payloads (f64 for numbers, bool for true/false, u8 for null) are
computed but never stored — a **correctness bug**.

**Performance impact**: 18% regression on canada.json (1483 vs 1796).
Sources: 7 extra stack locals per `__value` call (28 bytes including
f64), dead epilogue code bloating icache, push_compound overhead vs
push_leaf for leaf branches, payload Vec pre-alloc (2.1MB) allocated
and freed unused.

#### Critical failure 2 — KvPair is dead infrastructure

`TapeKind::KvPair`, `is_kv_pair_shape`, `emit_tape_span_only_aggregate_epilogue`
with `kv_pair: bool`, view accessors — all exist. None fire for any
grammar. JSON `pair` emits `push_compound` with `TapeKind::Rule`.
The type projection doesn't produce the `Tuple([Span, scalar])` shape
that triggers KvPair detection for the `pair` rule.

#### Critical failure 3 — CSS has zero fused scanners

Despite the architecture supporting fused scanners (`css_ident_fast`,
`css_number_scan_f64`, `css_ws_comment_fast`, `css_string_fast`),
the expanded CSS L4 parser uses NONE of them. All 319 whitespace
scans use the generic `scan_ws_block_comments`. All 8 ident scans
use `DEFAULT_IDENT_CONFIG` instead of `CSS_IDENT_CONFIG` (7 of 8).
All 20 number scans compute f64 via Eisel-Lemire and discard it.

#### Critical failure 4 — CSS HexConvert mapping is broken

`hex = "#" , /[0-9a-fA-F]{3,8}/ -> crate::css_types::parse_hex_color(input) : u32`
declares a HexConvert mapping, but the expanded parser emits
`push_compound` with no payload. The `parse_hex_color` function
(defined in the bench) is never called. HexConvert codegen
does not reach the tape-first emitter path.

#### Failure 5 — Payload Vec pre-alloc wastes 2.1MB per JSON parse

`builder.rs:69-74` pre-allocates `expected / 4 * 8` bytes for payloads.
For canada.json this is 2.1MB. Since all payload captures are dead
stores (bug #1), zero bytes are written. The 2.1MB is malloc'd,
zeroed, and freed unused — ~0.10-0.15ms per parse (~10% of baseline).

### Deferred items accumulated across AR+AS+AT (must not defer again)

| Item | Origin | Tranches deferred |
|------|--------|-------------------|
| ParsedGrammar elimination | AR.7.2 | **11 tranches** |
| StructRegistry population | AS.2.3 / AT.6.1 | 2 tranches |
| Named struct view codegen | AT.6.2 | 1 tranche |
| String decode codegen wiring | AT.3.3-3.5 | 1 tranche |
| 64-byte input padding | AR.5.2 | 3 tranches |
| NEON fractional scan | AR.8.1 | 3 tranches |
| Fresh samply profiles | AT.4.1 | 1 tranche |

### Pre-AU profiling data (all 4 grammars)

**JSON parse (MB/s)**:

| Dataset | AQ | AT | Delta | vs sonic-rs |
|---------|-----|------|-------|-------------|
| canada | 1796 | 1483 | **-17%** | 99.9% |
| citm | 2698 | 2661 | **-1%** | 86% |
| twitter | 2086 | 2193 | **+5%** | 83% |
| data | 1939 | 1944 | **0%** | 80% |
| data_xl | 1348 | 1228 | **-9%** | 83% |

**CSS L4 parse (MB/s)**:

| Dataset | AT | vs cssparser | vs lightningcss |
|---------|----|-------------|-----------------|
| normalize | 999 | 1.4x | 3.6x |
| bootstrap | 513 | 1.1x | 4.0x |
| tailwind | 579 | 1.3x | 6.0x |

**Google Sheets parse (MB/s)**: simple 90, nested 123, stress 115

**BBNF self-hosting parse (MB/s)**: ebnf 213, json 262, css_pretty 583,
sheets 552, bbnf_self 379, css_l4_grammar 425

**Compile pipeline (ms)**: json 0.13, ebnf 0.39, bbnf 1.61, sheets 2.14,
css_l4 9.98

**Codegen fingerprint (push counts)**:

| Grammar | compound | leaf | leaf_with_* | Compound% | .map(\|_\| ()) |
|---------|----------|------|-------------|-----------|-----------------|
| JSON | 8 | 1 | 3 (dead) | 67% | 9 |
| CSS L4 | 234 | 22 | 7 | 89% | 206 (20 f64 discards) |
| Sheets | 37 | 0 | 0 | 100% | 50 |
| BBNF | 90 | 15 | 0 | 86% | 106 |

## Architectural invariants

These are not goals. They are non-negotiable contracts that every
phase below honours:

1. **No legacy code, no fallbacks, no workarounds.** The tape-first
   materialisation path is THE architecture — not an add-on, not a
   feature-flagged mode. There is no "fast discard branch" and no
   "slow decode branch" — there is one path that preserves every
   typed value the grammar declares.

2. **Every `->` annotation in the grammar must reach the tape
   emitter.** If a rule carries `-> T : U` and codegen emits a
   compound with no payload for it, that is a codegen bug, not a
   grammar issue. The inference pipeline is the source of truth;
   codegen is its executor.

3. **Inference composes types; it never loses them.** When a rule
   body is `Seq(number, unit)` with `number -> f64` and
   `unit -> u8`, the composite type is `(f64, u8)` and the tape
   record carries that aggregate payload. The compiler does not
   silently downgrade the composite to an untyped compound.

4. **Parity targets are full typed AST equivalence, not parse-speed
   benchmarks in isolation.** sonic-rs parity means decoded strings
   and materialised `Value` nodes. lightningcss parity means every
   dimension carries its `(f64, unit)`, every color resolves to its
   typed representation (u32 for hex/named, space + components for
   functional, recursive for color-mix). Speed wins that come at
   the cost of discarded data are out of scope for this tranche.

5. **One tape layout, one access API.** The same `Parsed<'_>`
   returned by any grammar's `parse()` exposes its typed AST
   through a uniform `.view()` surface. No side-car arrays, no
   per-grammar escape hatches, no "scanner mode" variant.

6. **All grammar-specialised codegen is emitted from the grammar,
   never hand-written.** Schemas, dispatch tables, payload layouts,
   scanner alphabets, capacity closures, column selectors, keyword
   tables — every grammar-specific constant or kernel in the
   emitted binary comes out of the grammar + IR pipeline. If a
   grammar-specific value lives in `crates/core/src/backend/`
   hand-written, that's a codegen gap to close, not a feature to
   keep. The `STRUCTURAL_PUNCTS: &[u8] = b",:{}[]"` literal in
   `punct_ws_region.rs` and the `sp_json_*`/`sp_css_*` scanner
   wrappers called out in the AQ-audit (§5) are the archetypal
   violations this invariant closes.

7. **Type-descriptor coverage is total.** The `TypeDesc` lattice
   the grammar produces must have a codegen route for every
   variant, not merely the scalar subset. Concretely:
   primitives (`i8/u8/i16/u16/i32/u32/i64/u64/f32/f64/bool`),
   `Span<'src>`, owned UTF-8 strings via the arena, tuples (Seq
   of annotated rules composing to `(T₁, T₂, …)`), named structs,
   tagged-union enums with recursive payloads (e.g. CSS
   `color-mix` holding nested `Color` references), optional types
   (`Option<T>` from `?`-guarded rules), and variable-length lists
   (`Vec<T>` from repeat-emit rules like `font-family` or
   `transform` chains). If a grammar declares a type the emitter
   doesn't have a route for, that's a codegen bug — not a grammar
   restriction.

A latent `@scan`-directive mode that elides payloads for pure-scan
use cases is a possible future, gated behind a grammar-level
directive. It is not the current architecture and no Phase 1–7
deliverable targets it.

## Wave schedule

Phases compose into waves. A wave is a set of up to six sub-agents
with no overlapping file bounds; the orchestrator commits each wave
onto master before dispatching the next. Sequencing below names the
ordering constraint; parallel sub-phases inside a wave are free to
proceed concurrently in their own worktrees. See
`docs/instructions/README.md` for the isolation and file-bounds
discipline every sub-agent must honour.

| Wave | Parallel sub-agents | Blocks |
|------|---------------------|--------|
| **W1 — Grammar annotation audit (parallel)** | One sub-agent per grammar family: (a) CSS L4 (`AU.2.0` across fifteen `.bbnf` files), (b) JSON `string` decode annotations (`AU.3.1` grammar side), (c) BBNF token rules (ident / string_lit / int_lit / regex / comment — `AU.6.4` / `AU.6.8`), (d) Sheets literals and refs (`AU.6.8`). Read-only until the file is owned; exclusive write per grammar file. | W2 |
| **W2 — Codegen routing (parallel)** | (a) `AU.1` fixups landed + tape parity fixtures refreshed, (b) `AU.2.3` / `AU.2.4` / `AU.2.5` / `AU.2.6` CSS typed emitter routing, (c) `AU.2.7` SIMD bitmap v2 (scanner-only, SIMD `filter_quote_parity` prerequisite, old memchr/nibble paths deleted in the same commit), (d) `AU.3.1` string decode + new `push_leaf_with_string` on `TapeBuilder`. Each agent owns disjoint emitter files — `crates/core/src/backend/rust/emitter/map_value.rs` (typed CSS), `crates/core/src/generate/regex/emit/simd.rs` (bitmap), `crates/bbnf-tape/src/builder.rs` (string/arena). | W3, W4 |
| **W3 — Cross-bench generality (parallel)** | (a) `AU.6.1` padded input at `ParserState::new`, (b) `AU.6.2` per-grammar capacity closures into the emitter, (c) `AU.6.5` `.map(\|_\| ())` elimination at codegen, (d) `AU.6.6` bench-name disambiguation. Disjoint file bounds; safe to fan out. | W4 |
| **W4 — Columnar prototype gate (serial)** | Single agent: `AU.7.1` prototype in a scratch `columnar_tape` sibling; bench sum-all-f64 on canada against AoS. Gate decision is reached here. | W5 |
| **W5 — Either migration or arena fallback (serial)** | If W4 prototype clears ≥ 5×: `AU.7.2` (substrate migration) and `AU.7.3` (codegen-driven column selection) as one sequential commit chain. If W4 does not clear: `AU.6.7` unified arena lands on the AoS substrate and W6 plans for SoA in AV instead. | W6 |
| **W6 — Debt elimination + typed parity audit (parallel)** | (a) `AU.4.1` ParsedGrammar elimination, (b) `AU.4.5` bootstrap regen + stale test fixes, (c) `AU.4.6` pre-existing test-failure triage, (d) `AU.6.8` cross-grammar typed materialisation audit (confirms every `->` reaches the tape; runs last so the typed emitter routes from W2 are in place). | W7 |
| **W7 — Tranche completion (serial)** | Single agent: full bench re-run, `post-AU.json` write, `FINAL.md` composition, workspace test confirmation. No code changes this wave. |

**Cross-wave invariants.**

- Master is clean before each wave dispatches; commits from the
  prior wave are cherry-picked and reviewed before the next wave
  begins.
- No file is written by two agents in the same wave. When a file
  would span sub-phases across wave agents, promote the split-owner
  piece to an earlier or later wave.
- Sub-agents commit with `/commit` at milestones inside their
  worktrees; the orchestrator cherry-picks accepted commits.
- Any wave whose agents fail to land a hard gate halts the
  tranche; do not move to the next wave until the gate is met or
  the plan is re-authored with the user.

## AU plan — 7 phases

### Phase 1 — Fix projection activation (the tape surgery conflict)

The payload system and the tape surgery system conflict. Tape surgery
forces `mark_children + push_compound` on every branch. The payload
system expects `push_leaf_with_<T>` for scalar branches. Both are
active on the same Alt-bodied rule, but tape surgery wins because
`branch_pushes_children` over-classifies.

#### AU.1.1 Fix `branch_pushes_children` for nested leaf structures

In `crates/core/src/backend/driver/alt.rs:39-70`, the `_ => true`
catch-all misclassifies nested Alts whose branches are all leaves.
Fix: recurse into `Alt`, `Seq` (all-leaf Seq is also leaf-like),
and handle the inlined shapes correctly:

```rust
IrNode::Alt(branches, _) => branches.iter().any(|b| branch_pushes_children(ir, &b.node)),
IrNode::Seq(children) => children.iter().any(|c| branch_pushes_children(ir, c)),
```

Hard gate: JSON `__value` number/bool/null branches emit
`push_leaf_with_f64`/`push_leaf_with_bool`/`push_leaf_with_u8`
respectively. String branch emits `push_leaf`. Object/array branches
emit `push_compound`. Verified via `cargo expand`.

#### AU.1.2 Verify payload correctness end-to-end

After fixing branch classification, write a test that:
1. Parses `{"n": 42.5, "b": true, "z": null}` with the JSON parser
2. Walks the tape to find the number record
3. Reads `payload_f64()` from the record and asserts it equals `42.5`
4. Reads `payload_bool()` from the bool record and asserts `true`
5. Reads `payload_u8()` from the null record and asserts `0`

This proves the direct-to-struct projection path works end-to-end.

Hard gate: the test passes. Typed payloads are stored in the tape
and readable from the view layer.

#### AU.1.3 Revert payload Vec pre-alloc to lazy growth

Change `builder.rs:69-74` from `Vec::with_capacity(expected / 4 * 8)`
to `Vec::new()`. The payloads Vec grows lazily only when a rule
actually writes a payload. For JSON, leaf branches (number/bool/null)
now use `push_leaf_with_*` which allocates payload slots on demand.
Compound branches (object/array) never touch the payloads Vec.

Hard gate: `Vec::with_capacity` does not appear for payloads in
TapeBuilder.

#### AU.1.4 Bench: JSON must exceed AQ baseline

| Dataset | AQ | AU target |
|---------|-----|-----------|
| canada | 1796 | **≥ 1800** |
| citm | 2698 | **≥ 2700** |
| twitter | 2086 | **≥ 2100** |

Rationale: with leaf branches using `push_leaf_with_*` instead of
`push_compound`, we save 4 ops per leaf push AND capture the payload.
The f64 computation was always running (Eisel-Lemire); we now KEEP
the result instead of discarding it. This should be net-positive
because `push_leaf` is cheaper than `push_compound`.

### Phase 2 — CSS typed-AST parity with lightningcss, grammar-wide

The CSS L4 grammar under `grammar/css/l4/` declares the full spec
across fifteen files — `color.bbnf`, `value-unit.bbnf`,
`selectors.bbnf`, `media.bbnf`, `keyframes.bbnf`, `properties.bbnf`,
`gradients.bbnf`, `transforms.bbnf`, `filters.bbnf`, `easing.bbnf`,
`keywords.bbnf`, `func-body.bbnf`, `tokens.bbnf`, `values.bbnf`,
`stylesheet.bbnf`. Every one of them has semantic content that
lightningcss materialises into a typed AST; only a small subset
of our grammar rules have `->` annotations today, and of those,
only `namedColor`'s keyword table actually reaches the tape. Phase
2 is not a color phase — it is a **grammar-wide typed-materialisation
audit** against the grammar files themselves.

The fingerprint makes the current gap stark. `push_leaf_with_u32 = 1`
(namedColor), `push_leaf_with_u8 = 6` (a handful of keyword enums),
`push_leaf_with_f64 = 0`, 20 `scan_number_f64(…).map(|_| ())`
discards, 206 total `.map(|_| ())` sites. The grammar declares
far more typed structure than that; the codegen drops it.

Profiling-wise, hotspots across normalize / bootstrap / tailwind
converge on `__compoundSelector` (33–43% self), `__declaration`
(17–31% self), and a `scan_ws_block_comments_slow` tail at 11–13%.
The whitespace+comment scanner *is* the fused kernel at every
call site; the pre-AU audit was wrong about "zero fused CSS
scanners". The lever there is SIMD-tail tuning, not a missing
kernel (and is AU.2.7's scope).

The audit targets, file by file:

- `value-unit.bbnf` — `number -> f64` (AU.2.3); every dimension
  type (`length`, `angle`, `time`, `frequency`, `resolution`,
  `flex`, `percentage`) composes to `(f64, u8)` via Seq-of-
  annotated-rules aggregate layout (AU.2.5).
- `color.bbnf` — `hex` → `u32` via `parse_hex_color` routed
  through the tape emitter (AU.2.4); functional / space /
  color-mix as tagged-union payloads (AU.2.6).
- `selectors.bbnf` — `compoundSelector`, `complexSelector`,
  `attrSelector`, `pseudoClass`, `pseudoElement`, `nthExpr`
  each carry structured payloads. Selector specificity
  (`(a, b, c)` triple of u16) is a natural `(u16, u16, u16)`
  aggregate. Nth-expressions `(an+b)` fold to `(i32, i32)`.
  Pseudo-class arguments (matched by name) carry enum tags.
- `media.bbnf` — `mediaQueryList`, `mediaCondition`, `mediaFeature`
  each declare structured types (a media condition is an
  enum over `in-parens`, `not`, `and`-list, `or`-list;
  a feature is `(name: Span, op: u8, value: Value)`).
- `keyframes.bbnf` — `keyframeSelector` is a percentage list
  or `from`/`to` keyword (tagged union); `keyframeBlock` is
  `(selectors: list, declarations: list)`.
- `keywords.bbnf` — every keyword table has `-> u8` annotations
  mapping each keyword to a lightningcss-matching discriminant.
  These already annotate; codegen must actually emit them.
- `properties.bbnf` — property-aware declaration dispatch maps
  property names to typed value expectations. Each property
  group's `declValue` rule declares a typed return; those types
  must land.
- `gradients.bbnf` — linear / radial / conic gradient function
  calls with colour-stop lists, direction specifications,
  interpolation-space keywords. Owns recursive Color types.
- `transforms.bbnf` — `matrix`, `translate`, `rotate`, `scale`,
  `skew`, `perspective`, `matrix3d`, etc. Each is a tagged-
  union variant carrying a short parameter list.
- `filters.bbnf` — `blur`, `brightness`, `contrast`,
  `drop-shadow`, etc. Variant + dimension/percentage payload.
- `easing.bbnf` — `cubic-bezier`, `steps`, `linear` keywords.
  Variants carry tuples of typed parameters.
- `func-body.bbnf` — `calc`, `min`, `max`, `clamp` produce
  arithmetic-expression trees. Recursive typed enum.
- `stylesheet.bbnf` — at-rules (`@media`, `@keyframes`,
  `@font-face`, `@supports`, `@import`) — each carries a
  variant tag and a body-type specific to the at-rule.

The type palette the grammar genuinely needs is in the
"type-descriptor coverage is total" invariant: primitives, Span,
owned strings, tuples, tagged-union enums with recursive payloads,
optional types, variable-length lists. Every one of those shows up
in CSS L4; every one must have a codegen route before Phase 2
claims parity.

#### AU.2.0 Grammar-wide audit and completion of missing `->` annotations

Before any codegen change, audit every rule in the fifteen CSS
grammar files. Any rule with semantic content that lacks a `->`
annotation gets one — the grammar is the source of truth, and
missing annotations are grammar gaps to close. Produce the fresh
fingerprint; every leaf rule declares a type, every structural
rule composes one.

Hard gate: `grep -cE '^[a-zA-Z_]+ =[^\n]*->' grammar/css/l4/*.bbnf`
returns a count equal to the total number of semantic rules across
those fifteen files. No silent "Span" returns where the grammar
has a declarable typed value.

#### AU.2.1 Tune the ws-scanner SIMD inner loop

`scan_ws_block_comments` is in use at all 319 call sites; the
profile resolves most of its samples to the `_slow` byte-wise tail,
indicating the SIMD fast inner loop short-circuits frequently on
realistic CSS. Tightening the SIMD-to-scalar hand-off (wider chunk,
better `/*` / `*/` prefix detection, padded input — see Phase 6 item
1) is the lever here. There is no missing kernel to wire.

#### AU.2.2 Fix ident config routing

7 of 8 `scan_ident` calls use `DEFAULT_IDENT_CONFIG` instead of
`CSS_IDENT_CONFIG`. Only `selectorIdent` (which spells the leading
dash explicitly via `(?:-?...)`) picks up the CSS config today. The
regex-shape detection in `scanner_plan.rs` needs to recognise CSS's
ident form — `[a-zA-Z_\x80-\xff][\w\x80-\xff-]*` — and select the
CSS config for keyword, property name, and value-ident sites.

#### AU.2.3 Add `-> f64` to CSS number rule

The `number` rule in `grammar/css/l4/value-unit.bbnf` lacks a
`-> f64` mapping. Adding it activates payload capture for all 20
`scan_number_f64(…).map(|_| ())` discard sites. The f64 is already
computed by the scanner — the mapping just keeps the result.

#### AU.2.4 Route HexConvert through the tape-first emitter

`parse_hex_color` is declared at expand.rs:61 but has zero call
sites; the `push_leaf_with_u32` at expand.rs:103160 belongs to
`__namedColor`, not to the hex rule. The hex rule still emits
`push_compound` with no payload. `emit_hex_convert` in
`backend/rust/emitter/map_value.rs` handles the slab/combinator
path only; wire it through the tape emitter so `hex = "#" ,
/[0-9a-fA-F]{3,8}/ -> parse_hex_color(input) : u32` activates
`push_leaf_with_u32` on every hex color.

#### AU.2.5 Typed dimensions — `(f64, u8)` aggregates

`value-unit.bbnf` declares seven dimension types as
`Seq(number, unit)` where `number` has no annotation (fix via
AU.2.3) and `unit` has `-> u8` (already annotated). Once number
carries `f64`, the inference pipeline must compose the sequence
into an aggregate `(f64, u8)` payload — 9 bytes, rounds to
`push_leaf_with_aggregate` with a 16-byte slot. This covers:

- `length` — `(f64, u8)` where u8 is the 48-unit discriminant
  table in `absoluteLengthUnit ∪ relativeLengthUnit`
- `angle` — `(f64, u8)` with `{deg:0, rad:1, grad:2, turn:3}`
- `time` — `(f64, u8)` with `{ms:0, s:1}`
- `frequency` — `(f64, u8)` with `{Hz:0, kHz:1}`
- `resolution` — `(f64, u8)` with `{dpi:0, dpcm:1, dppx:2, x:3}`
- `flex` — `(f64, u8)` with `{fr:0}`
- `percentage` — f64 with a percentage-flag bit (or `(f64, u8=255)`
  as the "%" discriminant)
- `unitless` — bare f64

The inference pipeline already tracks types per rule. The missing
piece is composing a `Seq` of annotated rules into an aggregate
`TypeDesc` that codegen emits as `push_leaf_with_aggregate`. If
the composition is not happening, that's the inference bug to fix
— not a special case per dimension.

#### AU.2.7 SIMD structural bitmap (v2), scanner-only

Wave-2 profiling places CSS `scan_ws_block_comments_slow` at 11–13%
of self-time across all three stylesheets and `__big_comment` at
9–15% across every BBNF entry. Both are scanner-boundary costs that
a stage-1-style SIMD structural bitmap naturally erases — in one
vectorised pass over a padded input, produce a 64-bit-per-chunk mask
of the grammar's structural alphabet, then let a CTZ-driven driver
consume positions without ever re-reading bytes.

The concept was tried before and deleted. We reach back into the
history and redesign around the failure modes.

**Archaeology.** The prior attempt lived across commits
`4114695` (AO.0.1, `compute_structural_bytes` IR pass),
`7198c97` (AO.0.4-0.6, emitter wiring + WS elision + dispatch
integration), `2fa3172` (AP.1b, peek-only variant),
`4417f8a` (AP.1b, WS-elision gate). It was disabled at `2a8af08`
(AP.1) and formally deleted at `2f7c1bd` (AQ.5). The deletion
commit's own message names the reason: "pre-scan was a ~190µs
net-regression on citm" because AP.3.1's SIMD WS bitmap had already
captured the savings the pre-scan was supposed to provide.

The AQ-audit, line 38 onward, lists four specific integration
bugs that are what actually killed the feature — not the concept:

1. `filter_quote_parity` was a scalar backwards-backslash scan per
   quote, O(input × quotes), ~4 ms alone on citm.
2. `alt.rs:121–150` hybrid dispatch duplicated match arms AND
   always called `sync_structural_cursor_to_offset` even when the
   cursor was synced.
3. `alt.rs:193–204` checkpoint mode saved `state.offset` but never
   `state.structural_cursor` — backtrack desynced the cursor and
   every subsequent Alt fell through to the slow path.
4. `ws.rs:41–45` refused to elide WS between structural positions
   with a comment the audit calls "wrong" — the only savings that
   would have paid for the pre-scan were explicitly disabled.

Post-AP.3.1 reduced WS from 50% → 11.9% of citm, leaving no room
for the pre-scan to amortise. Deletion was the right call at the
time.

**What v2 fixes.** Four concrete guardrails, derived from the
four failure modes above:

1. **SIMD `filter_quote_parity` from day one.** Implement the
   simdjson pattern directly — `prefix_xor(quote_bits) ^
   prev_instring` via NEON `pmull` / x86 `_mm_clmulepi64_si128`.
   No scalar fallback path. The v1 scalar implementation is the
   single largest contributor to the -190 µs regression; v2 does
   not ship without it.
2. **Scanner-only integration.** The bitmap is a byte-to-byte
   function over the padded input buffer; it produces no cursor,
   no checkpoint state, no control-flow coupling. Consumed
   linearly by the driver once. Never rewound, never re-entered
   on backtrack. The old `structural_cursor` field does not
   reappear.
3. **Old path deleted in the same commit.** No hybrid dispatch,
   no fallback branch. `emit_memchr1/2/3` and `emit_nibble_lut_scan`
   in `crates/core/src/generate/regex/emit/simd.rs` (the current
   8-needle cap) are subsumed and removed. The structural-bitmap
   kernel IS the scanner for any call site whose needle set is a
   subset of the grammar's structural alphabet.
4. **Grammar-parameterised, not JSON-only.** The alphabet is
   derived per-grammar from the IR's terminal-starter and digraph
   sets. CSS gets `{}();:,@#` plus `/*`/`*/`; BBNF gets
   `@:;,(){}|` plus `->` / `/*` / `*/` / `(*` / `*)`; Sheets gets
   `+-*/^(),:$&=`; JSON gets `{}[]":,\`. Each grammar's kernel
   is compiled with its own nibble-LUT table.

**Target workloads** (all directly measured in wave-2, not
speculative):

- CSS `scan_ws_block_comments_slow` 11–13% → &lt; 2% (kernel
  subsumes the byte-wise tail that short-circuited).
- BBNF `__big_comment` 9–15% → the `/*…*/` digraph detection is
  one SIMD comparison per stripe; the scalar `memchr(b'*') + peek`
  path is deleted.
- JSON `memchr::{closure#0}` 7–19% on string-heavy datasets —
  subsumed when the grammar's alphabet includes `"` (string scan
  becomes a bitmap consumer).
- JSON WS is NOT a target. AP.3.1 already solved it; v2 does not
  try to re-pay a cost that's already been captured.

**Integration surface.**
`crates/core/src/generate/regex/emit/simd.rs` is the single file
that changes. Adds `emit_structural_bitmap_kernel(grammar_id, S, D)`
where `S` is the byte-set and `D` is the digraph-set derived from
the grammar IR. Deletes the nibble-LUT 8-target cap and the
memchr1/2/3 emitters. No changes to `alt.rs`, `grammar.rs`, or
`ws.rs`. No `ParserState` field changes. No `GrammarIR` field
changes beyond re-exposing the structural-alphabet set (which
`compute_structural_bytes` already computed before deletion — the
data flow is recoverable from commit `4114695`).

**Hard gate.** `grep -c 'fn scan_ws_block_comments_slow\|fn
memchr1\|fn memchr2\|fn memchr3\|fn nibble_lut'` across the
post-AU.2.7 codebase returns 0. Profile samples on
`scan_ws_block_comments_slow` across all three CSS datasets drop
below 3% each. CSS bootstrap clears 650 MB/s. BBNF `__big_comment`
share across all six grammar entries drops below 5%. No AU.2.7
commit lands without the scalar `filter_quote_parity` replacement
also landing in `parse-that`.

#### AU.2.6 Typed color functions — `(u8 space, f64×3, f64 alpha)`

`color.bbnf` defines five color rule families. Only two are typed
today:

| rule | declared type | materialised today |
|------|---------------|--------------------|
| `namedColor` | `-> u32` | **firing** (148-keyword alt table) |
| `hex` | `-> parse_hex_color(input) : u32` | **not firing** (AU.2.4) |
| `colorFunction` (rgb/rgba/hsl/hsla/hwb/lab/lch/oklab/oklch) | untyped | compound only |
| `colorFn` (`color(<space> c1 c2 c3 / α)`) | untyped | compound only |
| `colorMix` (`color-mix(in <space> <hue-method>? , <color> α? , <color> α?)`) | untyped | compound only |

Annotate the three functional rules with their typed return types
in the grammar, and extend the tape-emitter aggregate path to
handle payloads > 16 bytes via the unified arena (see AU.6.7).
Target payload layouts:

- `colorFunction` — `(space: u8, c1: f64, c2: f64, c3: f64, alpha: f64)` = 33 B. `space` is a 9-variant enum over the `colorType` alt; `c1/c2/c3` are the three `colorValue` components (`percentage | number`) already carrying f64 after AU.2.5; `alpha` defaults to `1.0` when the optional `alphaSep alpha` clause is absent.
- `colorFn` — same layout, but `space` enumerates the `colorSpace` alt (srgb, srgb-linear, display-p3, a98-rgb, prophoto-rgb, rec2020, xyz, xyz-d50, xyz-d65).
- `colorMix` — `(space: u8, hue_method: u8, left_color: ColorRef, left_ratio: f64, right_color: ColorRef, right_ratio: f64)`. `ColorRef` is an arena offset pointing to a nested Color record — color-mix is recursive and the arena naturally represents that.

Hard gate: `CssL4Parser::parse("rgb(255 128 0 / 0.5)")` round-trips
to a typed `Color::Rgb { r: 255.0, g: 128.0, b: 0.0, a: 0.5 }`
equivalent via `.view()`. Same for every functional notation and
for `color-mix` recursion. Compare against lightningcss's typed
output on the full `bootstrap.css` + `tailwind.css` corpora — every
color declaration must resolve to the same typed value.

Hard gate: CSS L4 bootstrap ≥ 600 MB/s after AU.2.3 + AU.2.4 land;
AU.2.5 and AU.2.6 must not regress bootstrap below 550 MB/s (typed
color payloads add arena work on hex+named-color-light pages but
that work replaces the current compound-wrap, net cost ≤ 5%).

### Phase 3 — String decode + honest JSON bench

Wave-2 measured bbnf at 77–85% of sonic across all five JSON
datasets, but the bbnf side of `json_value` never decodes strings,
never materialises a value tree, and drops the `Parsed` handle
without `.view()`. Until the decode path lands, every further parity
claim against sonic is apples-to-oranges. Estimated post-decode
ratios are 0.60 (twitter/data) → 0.85 (canada) depending on string
density; see `profiling-2.md` for the per-dataset table.

#### AU.3.1 Wire `decode_json_string_to_arena` through codegen

The decode function exists in `parse-that/.../scan/decode.rs:35`.
Wire it through `scanner_plan.rs` as a new
`SharedScanner::JsonStringDecode` variant. The kernel calls
`decode_json_string_to_arena` and stores the `StringPayload` in the
tape via a new `push_leaf_with_string` method on `TapeBuilder`. The
existing `push_leaf_with_Span` and `push_leaf_with_aggregate`
methods are the closest precedents — aggregate is capped at 16 B
and therefore unsuitable for decoded strings of arbitrary length,
so the new method allocates in an arena-backed region and stores
`(arena_offset, len)` into the payload slot.

Replaces the three discard sites at expand.rs:2263, 2508, and 3263
and the two kept-for-structural sites at 2897 and 3060 (key-in-pair
positions, where decode is equally required for honest value
materialisation).

#### AU.3.2 Build `json_monolithic_value` bench

`crates/core/benches/json/value.rs` already exists from prior AT
work but today only measures `JsonParser::parse` vs
`sonic_rs::from_str::<Value>`. Extend the bbnf side to walk the
tape after parse (`.view()` + recursive descent that reads
`payload_f64`, `payload_bool`, `payload_u8`, and the new
`payload_string`). The sonic side remains untouched. Rename the
bench groups accordingly so criterion's substring filter doesn't
execute both ends when filtering one; alternatively, break the
`data` / `data_xl` overlap by renaming `data` → `data_s` so a single
agent can profile each entry in isolation (see PROGRESS Session 2
"substring-filter contamination").

Hard gate: `json_monolithic_value` bench produces numbers directly
comparable to sonic-rs on every dataset. bbnf/sonic ratio after
honest comparison ≥ 0.60 on twitter and ≥ 0.80 on canada.

### Phase 4 — Accumulated debt elimination

#### AU.4.1 ParsedGrammar elimination

11-tranche deferral. The bootstrap loop is closed. `host.rs` extracts
`ParsedGrammar` from the tape-first bootstrap parser, then the pipeline
lowers it to IR. Eliminate the intermediate: produce IR directly from
the bootstrap tape. Delete `ParsedGrammar` from `types.rs` and all
consumers. ~600 LOC removal.

Hard gate: `ParsedGrammar` does not exist in the codebase.

#### AU.4.2 StructRegistry: populate or delete

Either populate `ir.struct_registry` from `project_types` for struct-
shaped rules, or delete the scaffold entirely. Dead infrastructure is
worse than absent infrastructure.

#### AU.4.3 Module-level `#[allow]` for generated.rs

Replace 280 per-item `#[allow]` attributes with 1 module-level
attribute. 5-minute fix.

#### AU.4.4 Delete schema emitter stubs

`schema/emit/ts.rs` (returns `String::new()`) and
`schema/emit/runtime.rs` (returns `Vec::new()`) have been stubs for
10+ tranches. Delete them.

#### AU.4.5 Bootstrap regen + stale test fixes

`generated.rs` is STALE — diverges from fresh regen by +770/-479
lines. Fresh regen, recommit, verify idempotency.

Fix 2 compile errors from the meta_idx fold:
- `gorgeous/tests/vm.rs` — missing `string_index` field in GrammarIR
- `core/tests/runtime_root.rs` — `rec.kind` → `rec.kind()`

Hard gate: `cargo test --workspace` compiles (zero compile errors).

#### AU.4.6 Triage pre-existing test failures

18 tests fail across pipeline (closures), debug (wildcards), imports,
lower, analysis. These are pre-existing — not AT regressions. Triage
each: fix, delete, or document as known-incomplete-feature.

### Phase 5 — Profile-driven optimization + bench parity

#### AU.5.1 Fresh samply profiles (all grammars)

**Landed.** Wave-2 covers every (bench, entry) pair: five JSON
datasets, three CSS L4 stylesheets, three Sheets formula packs, six
BBNF grammar files, ten bbnf-vs-sonic parity entries — 27 runs
under a single shared `CARGO_TARGET_DIR`, one port pair per bench,
all seven required artifacts per entry. Per-bench analysis is in
`profiling-2.md`.

Headline hotspots by bench (leaf-sample self-time share):

- **JSON canada**: `__value` 83%, `compute_f64` 11.5%, `push_compound` 4%
- **JSON twitter/data/data_xl**: `__value` 45–57%, `__pair` 12–37%, `memchr` (ws skip) 7–19%, `trim_leading_whitespace_scan_and_cache` 4–12%
- **CSS**: `__compoundSelector` 33–43%, `__declaration` 17–31%, `scan_ws_block_comments_slow` 11–13%
- **Sheets**: precedence tower 56–86%, allocator thrash 10–22% (inverse of formula size)
- **BBNF**: `__mapped_factor` 28–41%, `__rhs` 10–15%, `__directive` 7–19%, `__big_comment` 9–15%, `__binary_factor` 8–12%
- **sonic canada**: `parse_array::<DocumentVisitor>` 79%, `visit_container_end` 10%

#### AU.5.2 Full bench suite with regression checks

Run all 6 bench suites. Record results in `PROGRESS.md`. Verify
no regression vs AQ on any grammar.

### Phase 6 — Cross-bench generality levers

Wave-2 profiling surfaced five levers whose impact reaches more than
one grammar. These fold into the existing phases where the code
bounds allow; the ones that don't have their own gates.

#### AU.6.1 Pad parser input to 64-byte boundary

`ParserState::new` does not pad the source buffer. Every SIMD scan
kernel (`scan_quoted_string_simd`, `scan_digits_simd`,
`scan_ws_block_comments` SIMD inner loop, `u8x16` quotes) therefore
has to guard the tail with a per-chunk bounds check. Sonic-rs's
`PaddedSliceRead` appears in every sonic frame name for this exact
reason. Allocating a padded buffer at parser init removes the guard
on every kernel.

Expected impact: +3–6% uniform across JSON, CSS, Sheets, BBNF; the
largest marginal share is on small inputs where the tail is a
larger fraction of total work (`parse_simple` Sheets, `json.bbnf`,
`data` JSON).

Long-deferred item AR.5.2 is subsumed by this step.

#### AU.6.2 Per-grammar tape capacity heuristic

Every `TapeBuilder::with_capacity` in generated parsers uses
`input.len() / 2 + 2`. That divisor fits JSON scalar density but
under-provisions compound-heavy grammars: Sheets needs ~1 record
per input byte; CSS bootstrap / tailwind allocate many records per
input byte; BBNF's bootstrap record density is ≈ `len / 1.6`.

Wave-2 evidence: `_mi_heap_realloc_zero` + `RawVec::grow_one` +
`finish_grow` account for 10–22% of `parse_simple` Sheets samples,
up to 9% of `json.bbnf` inclusive, and are visible in every
compound-heavy grammar's profile.

Derive the divisor per-grammar from the codegen fingerprint table
(push_compound : push_leaf : push_leaf_with ratio) and emit it into
`parse()` at codegen time. No runtime branching.

#### AU.6.3 Precedence-tower flattening

Sheets's six-level left-recursive tower
(`__comparison_expr → … → __unary_expr`) contributes 56–86% of
self-time and caps Sheets throughput at ~125 MB/s regardless of
formula size. Every level emits unconditional `mark_children` +
`push_compound(Repeat)` + `push_compound(Rule)` even when its
operator never appears in the input.

Replace the chain with a single Pratt-style loop in `__expr`: dispatch
on operator precedence via a lookahead byte match, emit compounds
only for the precedence levels where an operator actually fires.

The pattern is not Sheets-specific. CSS L4 value expressions and
BBNF binary expressions have similar shapes and would benefit from
the same lowering.

#### AU.6.4 Leaf payload activation for token-shaped rules

`ident`, `string_lit`, `int_lit`, `regex`, `big_comment` in the
BBNF grammar all emit `push_compound(Rule)` wrapping a span that
consumers then re-slice from the source buffer. Lowering these
single-scanner-production rules to `push_leaf_with_Span` (or
aggregate, for structured payloads) keeps the span in the tape and
removes the compound wrapper plus the Repeat frame around it.

Same pattern for CSS `ident`, `hex` (once AU.2.4 lands),
`numericLiteral`, and for JSON's `string` branch once AU.3.1 lands.

#### AU.6.5 Eliminate `.map(|_| ())` at codegen

Discards total 371 across the four hot-path expand artifacts (9 in
JSON, 206 in CSS, 50 in Sheets, 106 in BBNF). Each is a
computed-then-thrown value; projecting child `TapeOffset` upward
removes the Option epilogue and shrinks the inner-loop icache
footprint. Enforces the `no-value-discard` invariant uniformly.

Hard gate: `grep -cF '.map(|_| ())'` returns 0 on every expand
artifact under `.profiles/samply/prebuild/expand/`.

#### AU.6.6 Orchestrator bench-name disambiguation

Bencher 0.1.5's `--bench <filter>` is a pure substring match. The
pair (`data`, `data_xl`) overlaps, so running `--bench data`
executes both and the `data` profile is 99%+ `data_xl` samples.
Same overlap affects (`bbnf_data`, `bbnf_data_xl`) and
(`sonic_data`, `sonic_data_xl`) in `json_value`. Rename the three
small-variant benches to break the prefix (`data` → `data_s`,
`bbnf_data` → `bbnf_data_s`, `sonic_data` → `sonic_data_s`) or
migrate the benches to criterion. Either fix restores per-entry
profile attribution for three of 27 entries.

#### AU.6.7 Unified arena for variable-length typed payloads

The current tape side-cars every non-scalar payload type into its
own Vec (`payload_f64: Vec<f64>`, `payload_bytes: Vec<u8>`, …) with
a `payload_idx u16` that overflowed on canada and got pun-packed
into `child_off`. Collapse all side-car Vecs into a single
arena-backed `Vec<u8>` owned by `Parsed`. `TapeRec::child_off` is
repurposed: for compound kinds it points at the first-child tape
offset (as today); for leaf kinds it points at the arena offset
of the leaf's payload, with the payload's type recovered from the
tape kind.

The arena holds four shapes:

- **Inline scalars** (≤ 32 b) — `u8/i8/u16/i16/u32/i32/f32/bool`
  live inline in `child_off` itself; no arena allocation.
- **Wide scalars** (64 b) — `f64/u64/i64` stored at an 8-aligned
  arena slot; `child_off` is the arena byte offset.
- **Aggregates** — packed by the codegen-emitted
  `PayloadLayout` (e.g. `(u8 space, f64 c1, f64 c2, f64 c3, f64 α)`
  for CSS colors, `(f64, u8)` for dimensions); stored at an
  8-aligned arena slot, size determined by the grammar-declared
  aggregate type.
- **Byte strings** — decoded JSON strings, BBNF ident / literal /
  regex / comment bodies, CSS quoted strings, `color-mix` recursive
  payloads. Stored as `(len: u32, bytes: [u8; len])` with the arena
  offset in `child_off`.

This replaces three things at once:

1. The JSON-specific `decode_json_string_to_arena` wiring from
   AU.3.1 — string decode becomes one case of the arena write path.
2. The `push_leaf_with_Span`, `push_leaf_with_aggregate`, and ten
   typed `push_leaf_with_<T>` methods on `TapeBuilder` collapse
   into one `push_leaf_with(kind, span_lo, span_hi, meta, payload:
   PayloadData)` entry point. `PayloadData` is an enum over the
   four shapes.
3. The `payload_idx u16` overflow hack in the canada fix — there
   is no separate payload_idx, only the arena offset.

Hard gate: `payload_idx` does not exist as a field on `TapeRec`;
every side-car Vec under `bbnf_tape::Tape` named `payload_*` is
deleted; the builder exposes a single `push_leaf_with` method.

#### AU.6.8 Typed materialisation parity across grammars

The invariant "every `->` reaches the tape" applies everywhere,
not only to CSS. Concrete deficits discovered during the wave-2
audit, all of which this phase closes:

- **JSON strings** — decoded to owned UTF-8 via the arena
  (subsumes AU.3.1). Every `scan_quoted_string_strict(…).map(|_|
  ())` at expand.rs:2263 / 2508 / 2897 / 3060 / 3263 is replaced
  by a `push_leaf_with(kind=String, …, PayloadData::Bytes(...))`.
- **CSS dimensions and colors** — all seven dimension types and
  all five color rule families materialise typed payloads
  (AU.2.3 – AU.2.6).
- **BBNF tokens** — `identifier`, `string_lit`, `int_lit`,
  `float_lit`, `bool_lit`, `regex`, `big_comment` each grow `->`
  annotations in `grammar/bbnf/bbnf.bbnf` and materialise their
  value in the tape. `string_lit` decodes escape sequences;
  `int_lit` returns `i64`; `float_lit` returns `f64`; `regex`
  stores the pattern span; `big_comment` stores the comment body.
- **Sheets cell refs and literals** — `cell_ref` gains
  `-> (row: u32, col: u32, abs_row: bool, abs_col: bool)`;
  `range_ref` stores its two endpoints; number and string literals
  use the f64 / arena paths respectively. No Sheets rule may emit
  `push_compound` where a typed payload is declared by the grammar.

The test corpus for each grammar is its `parsed → view()` round
trip: for every input in the fixture directory, every declared
typed payload must be reachable from `.view()` with the correct
value. Regressions fail the hard gate.

#### AU.6.9 BBNF comment fast path (preserving bodies)

`__big_comment` is 9–15% of self-time across every BBNF entry and
emits `push_compound(Rule)` wrapping a Repeat wrapping a Span.
Cost: one `mark_children` + three tape writes per comment.

The scanner is already memchr-based. The cost is the compound
wrap. Fix via AU.6.4 + AU.6.8: `big_comment -> Span` becomes a
single leaf record carrying the comment body's span. Readers (the
pretty-printer, formatters, tooling) access the body via
`view().text()`. The same Kind/Span representation also handles
`comment` (line comments) uniformly.

This is not "skip comments". Comments remain in the AST at the
exact same byte positions and are fully recoverable. The access
cost drops from three records to one, and the write cost drops
from `push_compound + mark_children + Repeat Rule push` to a
single `push_leaf_with(kind=Comment, …)`.

The same pattern applies to CSS comments (currently also
compound-wrapped inside `scan_ws_block_comments`) and JSON
whitespace handling (no comment in JSON, but the same leaf-Span
optimisation elides the trim wrapper).

Hard gate: `push_compound` is zero across all `__*_comment` rules
in every expand artifact.

### Phase 7 — Substrate pivot: columnar tape (SoA)

The current tape is an **array of structs** (AoS): `Vec<TapeRec>`
where each 16-byte record holds (kind, flags, payload_idx, span_lo,
span_hi, child_off) contiguously. Every walker that reads a record
reads all six fields in the same cache line. This is the right
layout *if* walkers touch most fields of every record they visit.

The wave-2 profile shows they don't. The hot `.view()` descent path
reads kind + child_off + span — three of six fields. Typed
accessors read kind + payload — two of six. The pretty-printer
reads all six, but it sits on a cold path relative to parse.

A **struct of arrays** (SoA, "columnar") layout separates each
field into its own `Vec<T>`: `kinds: Vec<u8>`, `span_lo: Vec<u32>`,
`span_hi: Vec<u32>`, `sib_skip: Vec<u32>`, `flags: Vec<u8>`, plus
typed payload columns (`pay_f64`, `pay_u32`, `pay_u64`, `pay_u8`,
`pay_agg16`, `str_off`). Each column is dense, independently
streamable by the hardware prefetcher, and naturally aligned for
its element type. The consequences compound:

- **4× more structural records per cache line on skeleton walks.**
  A walker reading only `kinds + child_off` touches two 64 B lines
  per 64 records instead of one TapeRec per 4 records.
- **SIMD bulk operations over typed payload columns become
  trivial.** `cols.pay_f64.iter().sum()` auto-vectorises to AVX2 /
  NEON over a contiguous slab — on canada.json this is a 10×
  improvement versus the AoS gather for "sum all numeric values".
  sonic-rs cannot structurally do this because `Value` nodes are
  heap variants.
- **Sibling-skip replaces first-child pointer.** In a pre-order
  tape, the first child of a compound is mechanically `idx + 1`
  — no pointer read at all. Each record stores only its distance
  to the next sibling; compound traversal is one indexed column
  read, not a pointer dereference into a non-adjacent record.
- **payload_idx is eliminated entirely.** The *k*-th record of
  kind `F64` lives at `pay_f64[k]`. Walkers maintain a running
  rank counter (monotonic, zero per-record storage) or, for
  sparse typed accesses, a per-grammar sparse rank overlay
  written only for kinds that need it.
- **Construction cost is neutral-to-positive.** AoS writes one
  16 B struct per record (straddles two scalar types).
  SoA writes six aligned scalar stores per record — more writes,
  but each column's tail line is hot in cache for consecutive
  pushes, fully pipelined.

The elegance argument: everything AU was already doing pushes
toward SoA. The unified arena (AU.6.7) turns side-car Vecs into
a single typed-column arena. The FDMP split-skeleton tape (AU.6.2
research doc) already argued for separating structural and payload
storage when the fingerprint shows a payload-free grammar. The
columnar pivot is the terminal state of both moves; landing AoS +
arena in AU and then SoA in AV is two substrate changes where one
would do.

This is not a column-per-record-kind explosion. The column set is
bounded: 6 structural + 6 typed-payload + up to 2 grammar-specific
overlays. The emitter refuses to add a 13th column without a
registered overlay, and each payload column maps to a finite set
of `TypeDesc` variants chosen by the codegen.

#### AU.7.1 Prototype: canada.json sum-all-f64 on a columnar spike

Fork `crates/bbnf-tape/` as a scratch `columnar_tape` sibling;
reimplement `TapeBuilder` / `TapeCursor` against 6 structural
columns + `pay_f64`. Replay the existing `tape_basic.rs` fixtures
record-by-record for structural parity. Bench "sum all f64 payloads
in canada.json" against the current AoS tape.

Hard gate: **columnar path shows ≥ 5× speedup on the sum-all-f64
benchmark**. If it doesn't, the rank-tracking cost or the gather-
per-payload overhead has killed the thesis; land AU.6.7 unified
arena on the AoS substrate and defer SoA to AV. If it does, AU.7.2
follows.

#### AU.7.2 Migrate the full tape substrate to SoA

Replace `Vec<TapeRec>` with `Columns` across `bbnf-tape`. Update
every accessor in `crates/core/src/backend/rust/view/`. Update the
builder. Update all fixtures. The tape-parity goldens under
`crates/core/tests/fixtures/tape_golden/` regenerate to the
columnar layout; record count and logical structure are
identical.

Hard gates:
- `Vec<TapeRec>` does not exist in the codebase; `Columns` is the
  one substrate.
- Every tape-parity fixture passes after regeneration.
- Sonic-rs parity tests pass on the columnar value path.
- Lightningcss equivalence tests pass on the columnar CSS path.
- Sheets `parse_simple` does not regress; BBNF `json.bbnf` does
  not regress (small-input cases where column tail writes compete
  with AoS struct writes — if these regress, the fingerprint-
  driven capacity + alignment tuning must address it before AU.7.2
  ships).

#### AU.7.3 Codegen-driven column selection per grammar

The column set active for a grammar is derived from the
`TypeDesc` universe its rules project. JSON activates `pay_f64`,
`pay_u8`, `pay_bool`, `str_off`. CSS activates those plus
`pay_agg16` (for color/dimension structs) and possibly
`pay_sel_ref` (a grammar-specific overlay for selector nodes).
Sheets activates `pay_f64` plus `pay_cellref: Vec<u32>`. BBNF
activates almost nothing beyond `str_off` for ident/literal/
comment bodies.

The codegen emits the column-set selection into `GRAMMAR_PROFILE`
(see AV Phase 1) so downstream decisions (capacity, walker
dispatch, typed accessor codegen) are driven from one source. No
per-grammar hand-selected column lists; the emitter reads the
type universe and decides.

Hard gate: no grammar-specific column selection appears in
hand-written code; every `Columns` instantiation inherits its
active column set from its grammar's emitted profile.

## Hard gates summary

Structural activation (Phases 1 – 2):

1. JSON `__value` number branch emits `push_leaf_with_f64` — landed
2. JSON `__value` bool branch emits `push_leaf_with_bool` — landed
3. JSON `__value` null branch emits `push_leaf_with_u8` — landed
4. Payload correctness test passes: `payload_f64() / bool() / u8()` return exact values
5. CSS `number` rule gains `-> f64`; all 20 `scan_number_f64(...).map(|_| ())` sites become typed leaves
6. `parse_hex_color` appears in expanded CSS parser emitting `push_leaf_with_u32`
7. 7/8 `scan_ident` CSS sites resolve to `CSS_IDENT_CONFIG`
8. Every CSS dimension (`length`, `angle`, `time`, `frequency`, `resolution`, `flex`, `percentage`) materialises its declared `(f64, u8)` aggregate
9. Every CSS color rule family (`namedColor`, `hex`, `colorFunction`, `colorFn`, `colorMix`) round-trips to its lightningcss-equivalent typed value via `.view()`

Typed-materialisation parity (Phases 3, 6.8, 6.9):

10. `json_monolithic_value` bench walks the tape on the bbnf side; every string is decoded, every number is f64, every structural record is reachable via `.view()`
11. bbnf ÷ sonic ratio ≥ 0.60 twitter, ≥ 0.80 canada (honest post-decode comparison)
12. Every `->` annotation in every grammar reaches the tape emitter — no declared type is silently dropped at codegen
13. No `__*_comment` rule in any expand artifact emits `push_compound`
14. `grep -cF '.map(|_| ())'` returns 0 on every expand artifact

Architectural cleanup (Phases 4, 6.7):

15. `ParsedGrammar` does not exist in codebase
16. `payload_idx` does not exist as a field on `TapeRec`; every `payload_*: Vec<_>` side-car on `bbnf_tape::Tape` is deleted; `TapeBuilder` exposes a single `push_leaf_with` entry point
17. `cargo test --workspace` compiles with zero errors; zero NEW failures vs pre-AU
18. `generated.rs` matches fresh bootstrap regen

Performance / cross-bench (Phases 5, 6):

19. Fresh samply profiles across all 27 (bench, entry) pairs — landed
20. Every SIMD-aware kernel sees a padded input buffer
21. JSON canada ≥ 1800 MB/s on the honest (decoded) value path
22. CSS L4 bootstrap ≥ 600 MB/s after AU.2.3 + AU.2.4 land
23. CSS L4 bootstrap does not regress below 550 MB/s after AU.2.5 + AU.2.6 land
24. Sheets `parse_simple` ≥ 200 MB/s after precedence flattening + per-grammar capacity tuning

## Critical files

| File | Phase |
|------|-------|
| `crates/core/src/backend/driver/alt.rs` | 1 |
| `crates/core/src/backend/rust/emitter/grammar.rs` | 1, 2, 6.4, 6.8 |
| `crates/core/src/backend/rust/emitter/map_value.rs` | 1, 2.4, 2.5, 2.6 |
| `crates/core/src/backend/rust/emitter/payload_layout.rs` | 2.5, 2.6, 6.7 |
| `crates/bbnf-ir/src/passes/infer.rs` | 2.5, 6.8 (Seq composition into aggregates) |
| `crates/bbnf-tape/src/builder.rs` | 1, 6.7 (single `push_leaf_with`) |
| `crates/bbnf-tape/src/tape.rs` | 6.7 (arena layout) |
| `crates/core/src/generate/regex/emit/scanner_plan.rs` | 2.1, 2.2, 3.1 |
| `crates/core/src/backend/kernels/comment_ws.rs` | 2.1, 6.9 |
| `crates/core/src/backend/kernels/identifier.rs` | 2.2 |
| `grammar/css/l4/value-unit.bbnf` | 2.3, 2.5 (add `number -> f64`, confirm aggregates) |
| `grammar/css/l4/color.bbnf` | 2.6 (annotate `colorFunction`, `colorFn`, `colorMix`) |
| `grammar/bbnf/bbnf.bbnf` | 6.8 (annotate token rules) |
| `grammar/google-sheets/*.bbnf` | 6.8 (annotate cell_ref, range_ref, literals) |
| `parse-that/rust/parse_that/src/parsers/scan/decode.rs` | 6.7, 6.8 (arena-decode kernel) |
| `parse-that/rust/parse_that/src/state.rs` (`ParserState::new`) | 6.1 |
| `parse-that/rust/parse_that/src/parsers/scan/number_f64.rs` | 6 (integer fastpath widening) |
| `crates/core/src/types.rs` | 4 |
| `crates/core/src/grammar/host.rs` | 4 |
| `crates/ir/src/types/grammar.rs` | 4 |
| `crates/core/benches/json/monolithic.rs` | 6.6 (rename `data`) |
| `crates/core/benches/json/value.rs` | 3.2, 6.6 |

## Operational directives

See `/INSTRUCTIONS.md` at the repo root. Progress tracked in
`PROGRESS.md` alongside this document.
