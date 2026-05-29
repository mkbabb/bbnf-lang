# CSS >SOTA Rearchitecture — Unified Tape, Lazy Projection, NEON Hot-Leaf Union

Synthesis of five investigations into one buildable plan. Target: a CSS path that
matches the JSON tape's already->SOTA shape, beats lightningcss (754 Mbps full-parse),
and stays grammar-BBNF-general (no CSS special-casing). aarch64 Apple M5 Max only;
NEON/SVE + aarch64 asm; no x86/AVX. preserve-rich-ast is non-negotiable.

Baselines (cold, warmup_iters=0): CSS typed track1 ~63.9 Mbps (fact-stream plane);
`CssL4Parser::parse` rich-AST ~10 Mbps (crates/core monolith, post-O(1)-checkpoint
master `8153236e8`); lightningcss full-parse ~754 Mbps; cssparser token-scan ~2400 Mbps;
JSON tape path already >SOTA vs sonic-rs.

---

## 1. What the A-series did, and whether to recover it

**Verdict: recover the model; it was never thrown away. Recover its *shape*, not its
AZ-IV overfit.**

The "28-118x regression" attributed to the restart is a timeline error. Investigation 1
establishes that `docs/benchmarks/post-AZ-IV.json` (commit `cb14970f`, 2026-05-02)
measures *intra-A-series* self-regression against the post-AU floor (`3b8b757d`,
2026-04-15). The restart (docs->code) began at `b5eb4651c`/`a5145a0bb` on 2026-05-03,
*after* that file was written. The restart did not cause the regression; it inherited it
and then deleted the cause.

The A-series model at AU was a **flat AoS tape**: a single `Vec<TapeRec>`, `TapeRec` =
16 bytes `#[repr(C)]` (4 records/cache line), parser appends fixed-size records, typed
AST is a **lazy view layer** over the tape (`crates/bbnf-tape/src/lib.rs:1-55`,
`tape.rs:60-110`, `builder.rs:150` push_leaf / `:184` push_compound, AU commit
`3b8b757d`). It was **BBNF-generalized, not per-grammar**: one monolithic
`crates/core/src/grammar/generated.rs` (25,184 lines) emitted JSON, CSS L4, Sheets, and
BBNF-self over a single uniform rule ABI
(`fn __rule(state, tape) -> Option<TapeOffset>`, `tape_prelude.rs:1-130`), with a
grammar-neutral three-class materialization (MustTape / TapeSpanOnly / TransparentElide).
This is exactly the consistent-across-grammars unified model the task wants, and it
shipped.

Measured A-series high-water marks (post-AU.json, aarch64-apple-darwin, cold per-parse,
mimalloc, fat-LTO): JSON tape-only with lazy value views — canada 1231 MB/s, citm
2438 MB/s, twitter 1967 MB/s. CSS L4 tape *recognition* — bootstrap 454 MB/s, normalize
735 MB/s, tailwind 496 MB/s. The CSS numbers were recognition only (no typed CSSOM
build), so they are not a parity-complete bar, but they prove the tape substrate is not
the bottleneck.

**What actually killed it** (from post-AZ-IV.json's own floors block, Investigation 1):
AZ-IV W5 made `json_monolithic` parse *into* a value tree by default (canada
1.83ms -> 215.7ms, 118x, the file states the same input tape-only would match AU); and
AZ-IV W5 reintroduced `StructRegistry` + `Arena<G>`/`Builder<G>` registry indirection in
the hot path (28-65x on bbnf/sheets, 983x on css bootstrap = 606.4ms, 10583x WATCHDOG on
tailwind = 77.6s). The irony: AU.4.2 had *deleted* StructRegistry; AZ-IV W5 bolted it
back. Root cause = **eager per-leaf payload materialization** (an f64 alloc per number) +
**registry indirection**, not a tape flaw.

The restart kept the tape and retired the overfit. Today's skinny tape
(`skinny/crates/runtime/src/tape/mod.rs:94`) is the evolved SoA form: separate `Vec<u32>`
offsets + `flag_cursors`/`flag_values` + `PayloadArena` (verified: fields `source`,
`offsets`, `flag_cursors`, `flag_values`, `payloads`, `id`). `StructBuilder`/`OpenFrame`
appear nowhere in skinny/. Restart amendment 01 (`a5145a0bb`) retired the 9 mandatory
per-grammar declaration crates and the StructRegistry template in favor of
template-emitted `grammars/<name>/` subdirs.

**Architectural lesson:** the tape was right; the AZ-IV value substrate was wrong. Never
make materialization eager-by-default, and never put a registry lookup in the per-leaf
hot path. Recover the A-series *uniform flat-tape + lazy view*, do **not** recover
StructRegistry/Arena<G>/Builder<G>.

---

## 2. Unified tape / layout / projection model across grammars

The four investigations agree on the shape; Investigation 2 supplies the load-bearing
correction: there are **two parallel runtime trees**, and the split is not "JSON=tape /
CSS=OpenFrame" but "skinny-vs-core."

- **skinny tree** (`skinny/crates/runtime/src/grammars/`, the *benched* tree): JSON is a
  flat lazy-offset tape (`tape/mod.rs:94`, append in `assembler.rs:71` `push_plain_offset`
  = one branchless `u32` write + `set_len`, geometric cold grow). Materialization is fully
  lazy: `value_from_ref` (`grammars/json/value.rs:143`) reads *one* byte at the offset and
  wraps a `Copy ValueRef{tape,cursor}` (`mod.rs:175`). Zero per-node heap alloc, zero typed
  value at parse time. This is the simdjson/sonic-rs ondemand model — why JSON is >SOTA.
  CSS in skinny emits a **tab-delimited fact-stream String** (`emit_fact_stream` /
  `emit_full_parse`, verified `css_l4_declaration_values/generated.rs:5,61`) — dozens of
  `push_str`/`to_string` + fnv64 + schema/policy/witness headers. *This serialization is
  the dominant CSS cost in skinny and the only place CSS diverges from the fast JSON path.*

- **core tree** (`crates/core/src/runtime/`, the rich-AST monolith): *both* JSON and CSS
  are eager OpenFrame `StructBuilder`s (`json/builder.rs:8` enum OpenFrame is structurally
  identical to `css_l4/builder.rs:16-43`). `begin_compound` (`css_l4/builder.rs:274`) is a
  hand-coded `match layout.rule_id` with ~40 literal arms — pure overfit that ignores
  `layout.kind`/`layout.fields`. The arena (`css_l4/arena.rs:60`) is `Vec<Vec<T>>` per
  compound class + `Vec<Box<CssColor>>`; every `end_compound` does `pending_X.split_off(base)`
  (a fresh `Vec` heap-alloc) then pushes into the slab, and each color is `Box::new`'d.
  checkpoint (`builder.rs:210`) clones the whole OpenFrame stack; rollback (`:233`) drops it
  and truncates 6 slabs.

**The unification target reconciles the AZ fork.** Investigation 2 establishes this was a
*deliberate* fork: AZ-I/AZ-II (`docs/tranches/AZ-I/RESEARCH.md:32`) dissolved the generic
tape in favor of grammar-derived direct-to-struct StructRegistry+OpenFrame, on the
rationale that sonic-rs/simdjson are struct-direct and the tape was a "needless round-trip."
That reasoning was half-right (typed+rich is good) and half-wrong (it made the build eager
and fragmented). The skinny lazy-offset tape is the surviving evolved A-series tape. So:

- A-series tape = unified + cheap + **untyped**.
- AZ direct-to-struct = typed + rich + **fragmented + allocating**.
- **Unify target = lazy-offset-tape (parse side) + layout-driven typed projection (view side).**
  Typed *and* allocation-free/lazy.

**Partial unification already exists at the layout layer.** `bbnf_ir::registry::struct.rs`
defines a fully grammar-derived `StructLayout { rule_id, rule_name, kind: LayoutKind
(Struct/TaggedEnum/UntaggedEnum/NewtypeWrapper, verified `:58`), rule_type, fields:
Vec<StructField> }` where each `StructField` carries a `FieldSource`
(`TypedLeaf`/`BranchTag`/`SeqPosition`/`RepeatElement`/`RuleReference`, verified `:84`),
built once per rule by `classify_body` (`crates/ir/src/passes/types/registry.rs:140`,
exhaustive over `IrNode`, BBNF-general). The `StructBuilder` trait
(`crates/core/src/runtime/builder.rs:66`) is already the single pluggable consumer surface.
**The key gap: the perf-critical builders do not consume `layout.fields` — they re-hardcode
shape as `match rule_id`.** A generic `SimpleStructBuilder<V,C>`
(`builder_template.rs:240`) exists but still `split_off`-allocates a `Vec<V>` per compound
and *discards* scalar leaf payloads (`push_leaf_with_f64 -> V::unit()`), so it is generalized
in dispatch but neither allocation-free nor payload-truthful.

### The unified model (one design, all grammars)

Split parse-time materialization from view-time projection, exactly as skinny JSON already
does, and drive **both** from the existing `StructLayout`.

**(A) One flat tape per parse, all grammars.** A builder-owned SoA column set (the skinny
`Tape` model: `offsets: Vec<u32>` + `flag_cursors`/`flag_values` + `PayloadArena`)
replaces every per-grammar OpenFrame stack and `Vec<Vec<T>>` arena.
- `begin_compound` pushes an **Open** record (source offset + `rule_id`); `end_compound`
  pushes a **Close**. Children are the contiguous run between Open and Close, recovered by
  cursor arithmetic — **no `split_off`, no per-compound `Vec` alloc**.
- A leaf appends one offset record + *optionally* one `PayloadArena` entry, used only for
  decoded scalars that cannot be re-read from source (f64 bits, u32 hex). Strings/idents/
  spans are re-read lazily from the source offset, never copied.
- checkpoint/rollback collapse to a single `offsets.len()` marker + truncate — O(1), no
  stack clone. This directly kills the measured taxes: ~17% allocator + 6% arena-truncate +
  10.7% frame-management (Investigation 2's samply), and the ~12-13% checkpoint+rollback+
  truncate + 13.5% `finish_grow` (Investigation 5's samply).

**(B) One layout-driven projection at view time.** A generic `ValueRef<G>` (the skinny
`ValueRef` `Copy` cursor) whose typed accessors are *generated from `StructLayout`*:
`LayoutKind` picks struct/tagged-enum/untagged-enum/newtype; `FieldSource` resolves each
field's tape slot — `SeqPosition` = nth child between Open/Close, `BranchTag` = the Close
record's tag byte, `TypedLeaf` = `PayloadArena` read or source re-read, `RuleReference` =
recurse. This is the *same* `StructRegistry` that exists today; the builders simply stop
hardcoding `match rule_id` and start reading `layout.fields`, making the path
grammar-BBNF-general and *identical* across JSON/CSS/sheets/bbnf by construction.

**(C) preserve-rich-ast holds** because the typed CSSOM (`CssColor`, `CssDimension`,
`CssLength`, `CssFunction`, `Selector`, `CssRule`, ... `crates/core/src/runtime/css_l4/value.rs`)
is produced by lazy view projection over the tape, not flattened. Structure is
reconstructed on demand from offsets + layout, exactly as `JsonValue` is today, so
cssparser-parity 8-field structural equality still holds.

**Tape record layout (concrete, SoA, NEON-friendly):**
- `offsets: Vec<u32>` — one per structural token (Open/Close/Leaf), source byte offset.
- `kinds`/`flag_cursors` + `flag_values` — discriminator (Open vs Close vs Leaf) +
  `rule_id` / `BranchTag` byte packed alongside (the existing skinny dual-column
  `flag_cursors`/`flag_values` already models sparse per-cursor flags).
- `PayloadArena` — append-only `Vec<u8>`/`Vec<u64>` for decoded scalars only; index stored
  in the leaf's flag slot. (Already exists in skinny `tape/mod.rs`.)

This is migration, not invention: promote skinny `Tape`/`PayloadArena`/`ValueRef` into the
shared runtime, make the builder emit tape records instead of OpenFrame deposits, generate
view accessors from `StructLayout`, retire the per-grammar OpenFrame builders + `Vec<Vec>`
arenas.

---

## 3. Is the value API consistent, grammar-generalized, and overfit-free?

**Verdict: consistent + generalized in the *core* tree; divergent + overfit in the
*benched skinny* tree. Substantial changes required.**

**Consistent where it counts (core tree).** Investigation 3 confirms `crates/core/src/runtime/`
is the unified projection-generated API: all 9 grammars (json, css_l4, google_sheets,
bbnf, ebnf, bnf, csv, math, css_pretty) share the identical
`{value,view,document,arena,builder}.rs` + `mod.rs` shape (67-file Pattern H set). CSS
exposes `CssDocument/CssView/CssFocus/CssDocumentKind/CssPathQuery/CssDeclWalk` +
`view()/document()/focus()/walk_values()` isomorphic to JSON's `JsonDocument/JsonView/...`.
Both headed `@generated by xtask`. The rich typed CSSOM is preserved
(`css_l4/value.rs:414 CssTypedValue<'p>`). In this tree the unified API is real and
preserve-rich-ast holds.

**Divergent + overfit where it is benched (skinny tree).** Confirmed:
- skinny `RuntimeEmitterKind` has exactly two variants — `CompiledLowering`, `RequestFacts`
  (verified `grammar_provider.rs:40`). There is **no generalized typed-value generator**.
  JSON value/view/visitor are built by `include_str!` from JSON-specific templates
  (`json_templates/value.rs`, with 23 literal `Json` occurrences — grammar name baked in).
  CSS goes through `emit_request_facts` -> a single `CSS_GENERATED_RS` string constant;
  `emit_fact_stream`/`emit_full_parse` return `Result<String,_>`, **not a typed value**
  (verified `generated.rs:5,61`).
- The benched CSS "track1" is a fact-stream String, not a typed value
  (`bbnf-bench/src/nonjson_css_l4.rs` `track1_*_facts -> Result<String,String>`). No
  `CssValue`/`CssDocument`/`CssView`/`CssVisitor` is on the benched path. So the ~63.9 Mbps
  figure measures string serialization, not typed CSSOM construction.
- The CSS typed variants are **hand-authored in the projection TOML**, not derived from
  grammar->projections: `xtask/runtime-projections/css_l4.toml` is 594 lines (verified) —
  hand-enumerated `CssLengthUnit` (27 units), `[[records]]`, `CssTypedValue` sum-enum, and
  a `[builder]` block listing 28 hardcoded `*Decl` rule names + numeric/function/color
  routes by literal rule name. Contrast `json.toml` at 34 lines (verified):
  `runtime_style='typed_json'` + simple rule->kind routes. JSON's projection is
  grammar-shape-driven; CSS's is a hand-curated type catalogue — the asymmetry.
- Bench overfit: `real_typed_struct.rs` is per-corpus hand-coded (TwitterSearch /
  CitmCatalog / GsocProposal / CanadaFeatureCollection / GithubEvent);
  `generated_real_typed.rs` (4941 lines, `schema_hash sk-v14-w9ab-canada`) has 187
  fixture-named parse fns + hand-tuned per-corpus capacity constants. Textbook overfit.
- Orphaned codegen: `xtask/src/regen_css.rs:280` and `regen_simple_runtime.rs:244` target
  `crates/core/src/runtime/...` (top-level workspace), entirely disconnected from the
  skinny benched runtime. The A4 audit confirms the 7 skinny CSS `generated.rs` fail
  delete/regenerate roundtrip, `generated_real_typed.rs` is stale, and `CSS_GENERATED_RS`
  "is not a typed grammar-derived CSS admission surface." All 8 are git-dirty now.

**Spec intent** (`restart/skinny/tranches/sk-v16/SPEC.md`): close-condition #4 — CSS
exposes typed document/value/view/visitor (same shape as JSON); #5/#6 — CSS Track 1 typed
summary must equal then beat cssparser *before* any admit; #3/#9 — fact streams,
full-parse summaries, brace counters, FNV are DIAGNOSTIC ONLY and must NOT admit CSS;
CSS L4 is currently 0/24 admitted. So the skinny fact-stream path is spec-illegal for
admission by design.

**What must change:**
1. Unify on a single projection-driven generator emitting `document/value/view/visitor` for
   *every* grammar. Extend skinny `RuntimeEmitterKind` beyond `{CompiledLowering,
   RequestFacts}` with a typed-value emitter (or point skinny at the core generator).
   Eliminate the JSON-only `json_templates/` special-case *and* the
   CSS-fact-stream-as-output-plane.
2. Bring the rich `CssTypedValue` CSSOM onto the benched skinny path so CSS Track 1 produces
   a *typed* summary comparable to cssparser, not a fact string.
3. Quarantine `emit_fact_stream`/`CssFullParseSummary`/`CSS_GENERATED_RS` to diagnostic-only;
   retire/regenerate the dirty `generated_real_typed.rs` + 7 CSS `generated.rs`.
4. Derive CSS typed variants from grammar->projections rather than the 594-line hand-curated
   `css_l4.toml` builder/repr_enums catalogue (or at minimum gate it under a forbidden-token
   scan). This folds directly into the §2 layout-driven projection: if accessors are
   generated from `StructLayout`, the hand-curated TOML catalogue dissolves.

This is a codegen-unification + overfit-removal task; NEON is orthogonal to it.

---

## 4. dav1d-style aarch64 NEON/asm hot-leaf union (no x86)

**Story today:** `bbnf-simd` is a real intrinsic-first NEON crate (`core::arch::aarch64::*`),
not external `.s` files, with 16 aarch64 modules. Inline `asm!` precedent already exists:
`digit_mac.rs:38` (udot 4-digit parse), `:61` (sdot dot4_i8), `cache_hints.rs:5` (prfm
prefetch), `:20` (stnp non-temporal store-pair). JSON is fully NEON-wired
(`grammars/json/scan.rs` `classify_tbl4` via `vqtbl4q_u8` + `escape_mask_64` carry algorithm
+ `prefix_xor_64`; string leaves via `match_tiny_plain_string`). **CSS uses zero SIMD** — all
seven `css_l4_*/generated.rs` have 0 occurrences of simd/neon/vqtbl/core::arch (verified by
the investigation's grep counts; `movemask.rs` has no `to_bitmask64`/`vpaddq`, confirming the
suboptimal-movemask claim).

**Profile** (samply --save-only, debug=true, cold, material-components-web 495KB / bootstrap):
CSS hot leaves are `find_component_delim` ~56% of resolved self-time,
`consume_balanced_at` ~10%, `emit_*` String building ~34%. The whole CSS cost is the scalar
delimiter/balance scan + String output. In the rich-AST profile (Investigation 5),
`regex_scan` (`css_l4.rs:15693`, scalar last-byte-set token scan) is ~3% and **not**
NEON-vectorized.

**Dispatch is compile-time only.** `dispatch.rs:63` `select_primitive_kernels` gates NEON
vs scalar via `#[cfg(target_arch="aarch64")]`; `digit_mac.rs:10` gates udot on
`#[cfg(target_feature="dotprod")]`. **Zero `is_aarch64_feature_detected!`** in skinny. On
aarch64-apple-darwin rustc enables neon+dotprod+fp16+aes by default; `target-cpu=native`
adds i8mm+sha3. Apple cores have **no SVE** (NEON+AMX only) — SVE paths would be dead code
on M5 Max, so this plan is NEON + optional dotprod/i8mm only.

**Orphaned kernels:** `digit_mac` (udot), `cache_hints` (prefetch/stnp), and the general
`scan_dispatch`/`select_classifier`/`SelectedClassifier` API are built but never called
outside tests — only JSON calls `classify_tbl4` directly, bypassing the general surface.
The grammar-general API exists but is dead.

### The plan (aarch64-only, NEON + optional dotprod/i8mm, no x86/SVE)

1. **Fix movemask first (pure win, no API change).** Replace the three divergent impls
   (`movemask.rs:4` scalar-loop pack; `byte_class_from_eq_set_64.rs:79` per-lane multiply;
   `classify_tbl4` 4x movemask + shift/OR) with one `to_bitmask64` per sonic-simd
   `neon.rs:151-164`: 4x `vandq_u8(bitmask)` + cascaded `vpaddq_u8` (pair/quad/octa) + one
   `vgetq_lane_u64`. Feeds every 64-byte classify in every grammar.

2. **Vectorize a grammar-general leaf set** (all keyed on the grammar's delimiter/alphabet
   sets, never CSS-specific):
   - `byte_class_index_64` — the `find_component_delim` / `find_ascii_set_member64` leaf via
     `vqtbl4q_u8` low-6 class table + classify (half-built in `classify_tbl4.rs` +
     `find_ascii_set_member64`). This is the 56% CSS leaf.
   - structural + string-region resolution — reuse `escape_mask_64` + `prefix_xor_64`
     verbatim (already grammar-agnostic carry algorithms).
   - whitespace-skip — `vcleq`/`vcgtq` range compare -> first-clear bit
     (replaces `skip_ws_comments` byte loop).
   - ident/name-byte scan — `vqtbl4q` class probe (`is_name_byte` becomes a 64-entry table).
   - number scan — wire the orphan `digit_mac` udot path.

3. **Dispatch:** keep compile-time `#[cfg(target_arch="aarch64")]` as the primary gate
   (Apple M baseline guarantees neon+dotprod+fp16). Add ONE runtime
   `is_aarch64_feature_detected!("i8mm")` heuristic *only* where i8mm/native-only kernels
   diverge, threaded through the existing `dispatch.rs PrimitiveKernels` OnceLock
   fn-pointer table — the dav1d/cglue analog. Extend it; do not add an orthogonal subsystem.
   Resurrect the dead `scan_dispatch`/`select_classifier` as the single entry.

4. **Grammar-generality:** codegen emits calls to `bbnf_simd::prim::*` leaves keyed on the
   grammar's delimiter/alphabet sets (the lo6-table already takes a runtime alphabet), so
   JSON/CSS/sheets/bbnf share one scanner vocabulary; fall back to scalar when the alphabet
   collides mod 0x3f (`dispatch.rs:101 lo6_table_admissible`).

5. **preserve-rich-ast:** the NEON leaf produces only the delimiter/structural *index*
   (`Vec<u32>` positions, exactly like JSON `scan_structurals`); the §2 tape build + typed
   projection consume the index unchanged. Speed comes from the scan, never from dropping
   structure. Keep checkasm parity gates (scalar reference = executable spec) for every new
   leaf — the dav1d discipline already mirrored in `checkasm_*.rs`.

---

## 5. Honest feasibility vs lightningcss (754 Mbps) + ordered plan

### Can CSS beat lightningcss?

**Honest answer: yes, but only after an architectural change, not optimization alone.** Two
investigations converge on the ceiling:

- Investigation 5 (rich-AST profile, ~10 Mbps): 68.7% of self-time is in system libraries
  (malloc 37.5%, memcpy/memmove 17.5%, kernel page faults on Vec drop/grow 13.8%); own code
  is 31.3%. The alloc levers (checkpoint clone, scratch regrowth, `Vec<Vec>`+split_off arena)
  are worth ~3-4x -> ~30-40 Mbps. After the alloc floor falls, ~31% own-compute (speculative
  branch machinery + scalar `regex_scan` + utf8 revalidation) becomes the wall. **Rich-AST
  parity is near a ~50-100 Mbps structural floor for the *speculative-descent* design.** The
  last 7-15x to 754 needs eliminating speculative checkpoint/rollback (commit-by-construction
  / no-rollback on the structural spine) + SIMD tokenization.

- Investigation 1 + 2: the speculative-descent architecture is itself the problem. The
  skinny JSON tape is *not* speculative on its spine — it is a SIMD structural pre-scan +
  branchless append, which is why it is >SOTA. lightningcss is a hand-tuned single-pass
  tokenizer with no speculative checkpoint/rollback at all; cssparser (2400 Mbps) is pure
  scalar tokenization.

So the ceiling is set by *whether CSS adopts the JSON model* (SIMD structural index ->
flat-tape append -> lazy typed projection, no speculative rollback on the spine), not by how
hard the existing OpenFrame builder is tuned. The two levers and their estimated worth,
stacked:

| Lever | Mechanism | Estimated effect |
|---|---|---|
| Kill fact-stream serialization | tape append instead of String push_str/fnv | removes the dominant skinny-track1 cost; the fast path becomes the JSON path |
| Alloc removal (tape vs OpenFrame) | O(1) checkpoint marker, no split_off, no Vec<Vec>, no per-leaf payload alloc | removes the measured 68.7% syslib floor; ~3-4x on the rich path |
| NEON structural pre-scan | `byte_class_index_64` + movemask cascade replaces the 56%+10% scalar scan | the structural scan stops being the wall; this is what carries past lightningcss |
| Commit-by-construction spine | no speculative rollback on the structural backbone | removes the residual ~31% speculative branch machinery |

**Expected ceiling, honestly:** with all four, CSS full-parse with rich typed CSSOM should
reach the **300-600 Mbps band** and is *capable* of crossing 754 Mbps on
structurally-regular corpora (normalize/bootstrap), because the model becomes
isomorphic to the already->SOTA JSON tape. tailwind (deeply nested, many short rules) is the
hardest corpus and may land short of 754 on the first pass; it is the one to bench
adversarially. Claiming a guaranteed >754 on *every* corpus before the tape lands would be
dishonest — the JSON tape proves the model can exceed these bars, but CSS has a richer typed
projection and a more irregular alphabet, so the realistic first-cross is on regular corpora,
with tailwind following after the NEON delimiter index is tuned.

### Ordered implementation waves (generalized, no CSS special-casing)

Each wave states its honest expected speed. All waves operate on the *skinny* benched tree
and drive everything from `StructLayout` so the change is grammar-general by construction.

**Wave 1 — Codegen unification (substrate for everything; no perf yet).** Extend skinny
`RuntimeEmitterKind` with a typed-value emitter that consumes `StructLayout`; emit
`document/value/view/visitor` for every grammar from one generator. Retire `json_templates/`
JSON special-case and quarantine `emit_fact_stream`/`CSS_GENERATED_RS` to diagnostic-only.
This unblocks a *typed* CSS Track 1. Expected: still ~10-64 Mbps (no hot-path change yet) —
but now measuring a typed CSSOM, not a string. Resolves the §3 overfit and SPEC #4/#3/#9.

**Wave 2 — Promote the flat tape into the shared runtime.** Move skinny
`Tape`/`PayloadArena`/`ValueRef` into shared runtime; make the layout-driven builder emit
Open/Close/Leaf tape records (no OpenFrame, no `Vec<Vec>` arena, no split_off). checkpoint =
`offsets.len()` marker + truncate. Generate view accessors from `StructLayout` `FieldSource`.
Expected: rich CSS ~10 -> **~30-40 Mbps** (removes the measured 68.7% syslib alloc/copy
floor). JSON unchanged (already this model).

**Wave 3 — NEON movemask + structural pre-scan leaf.** Land `to_bitmask64` (replaces 3
divergent impls), then `byte_class_index_64` (`vqtbl4q` low-6 + classify) as the
grammar-general delimiter/structural index. Route CSS scanners (`find_component_delim`,
`skip_ws_comments`, `is_name_byte`) and JSON through the same resurrected
`scan_dispatch`/`select_classifier` entry. NEON produces only the `Vec<u32>` index; the Wave
2 tape consumes it. Expected: rich CSS ~40 -> **~80-150 Mbps** (the 56%+10% scalar scan
collapses; the structural scan stops being the wall on regular corpora).

**Wave 4 — Commit-by-construction structural spine.** Remove speculative checkpoint/rollback
from the structural backbone (the emitter, `regen_css.rs`, emits NO checkpoint for
pure-lexical keyword-dispatch Alts that deposit nothing structural; the spine commits as it
scans, driven by the NEON structural index). Backtracking, if needed, survives only on true
ambiguous leaves, not the spine. Expected: rich CSS ~150 -> **~300-600 Mbps**; first cross of
754 Mbps on regular corpora (normalize/bootstrap) plausible here.

**Wave 5 — Adversarial corpus tuning + scalar-leaf polish.** Wire the orphan `digit_mac`
udot number scan; add the i8mm runtime-detected kernel via the existing OnceLock fn-pointer
table; size scratch from `input.len()`; drop the `from_utf8` revalidation
(`builder.rs:255`, input is already a validated `&str`). Bench tailwind cold and tune the
delimiter table for its short-rule density. Expected: push tailwind toward/past 754; lock the
>SOTA claim across all corpora or report the honest residual gap on tailwind.

---

## Risks

(captured in StructuredOutput)
