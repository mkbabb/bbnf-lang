# AY-II-AUDIT-B — Hitherto Expand Truth at Pre-close Pause

Audit Agent B of the AY-II pre-close triumvirate. READ-ONLY. Worktree
`/Users/mkbabb/Programming/bbnf-wt-ay-ii-audit-b` at HEAD `b5bbda6c`
(pause commit, ahead of cherry-picked sibling audit docs A/C/D which
live on master at `baeed709`/`ad70effd`/`a24ce9be`). Not a
re-iteration of AY-I-era `audit/AUDIT-B-hitherto-expand.md` — that
file was itself cherry-picked from the AY-I tranche and is the
INPUT this pass composes against.

## 1. Scope + methodology

Evidence sources: (1) emitter source at `crates/core/src/backend/
rust/emitter/shapes/*.rs` + `grammar.rs` + `value_materialize.rs`;
(2) tape crate source at `crates/tape/src/{builder,cursor,columns,
structural_scan}.rs`; (3) `crates/core/src/grammar/generated.rs`
(only `BbnfBootstrap` grammar lives there — `JsonParser`,
`CssL4Parser`, `GoogleSheetsParser` are `#[derive(Parser)]` expansions
in bench crates); (4) cargo-expand at `target/expand/ay-*.rs`.

**Expand captures attempted** — every run blocked on shared-target
incremental-state corruption (18+ min wall with `CARGO_BUILD_JOBS=4`;
`target/expand/ay-ii-json.rs` = 0 bytes, `target/expand/ay-ii-bbnf.rs`
= 0 bytes); `target/expand/ay-ii-json.rs.probe` from earlier AUDIT-B
dispatch shows `error: could not compile gorgeous`. **Per-grammar
expand rows are PARTIAL — cited against AY-I-era expansions
(02:00-03:37 UTC, PREDATE W0.a=04:29) where they suffice; sourced
against emitter + `generated.rs` at HEAD `b5bbda6c` elsewhere.**

## 2. Per-shape migration ledger

Counts are static grep over emitter source (emitted `quote!` token
count). Residuals split between `quote!` emissions (load-bearing) and
doc-comment references (harmless). All counts source-verified at
`crates/core/src/backend/rust/emitter/shapes/*.rs`.

| Shape | `begin_compound` | `end_compound` | `end_compound_post_order` | `rollback_to` | `push_compound` | `columns_mut().truncate` | `mark_children` | `note_push` | `value_builder` writes |
|---|---|---|---|---|---|---|---|---|---|
| `object.rs` | 10 | 14 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| `array.rs` | 11 | 14 | 1 | 3 | 0 | 0 | 0 | 0 | 0 (2 doc-comments) |
| `wrap.rs` | 1 | 0 | 1 | 1 | 0 | 0 | 0 | 0 | 0 |
| `keyword.rs` | 0 | 0 | 0 | 3 | 0 | 0 | 0 | 0 | 0 |
| `inline.rs` | 2 | 0 | 2 | 3 | 0 | 0 | 0 | 0 | 0 |
| `alt_dispatch.rs` | 1 | 0 | 1 | 1 | 0 | 0 | 0 | 0 | 0 |
| `flat.rs` | 7 | 0 | 7 | 3 | 0 | 0 | 0 | 0 | 0 |
| `pratt.rs` | 2 | 1 | 1 | 0 | 0 | 0 | 1 (doc) | 0 | 0 |
| `arglist.rs` | 5 | 0 | 5 | 3 | 0 | 0 | 0 | 0 | 0 |
| `unordered.rs` | 2 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |

Source citations for retrieve/verify:
- `object.rs:121,132,157,168` begin_compound call-sites with 6-arg form; `180,182,194,196,296,307,311,355,360,363,372,376` end_compound.
- `arglist.rs:298,310,345,349` retry-IIFE sites calling `builder.columns_mut().rollback_to(…)`.
- `array.rs:612,701` residual `value_builder` tokens are in doc-comments (`// value_builder via rollback_to / value_builder.rollback_to`) — the only two file occurrences.
- `pratt.rs:141-143,167` reference `mark_children` only in doc-comments describing AY-archaeology; zero `quote!` emissions.

**Verdict per shape**: ALL 10 shapes are 6-arg-`begin_compound` +
rollback_to migrated. ZERO residual `push_compound` / `mark_children`
/ `columns_mut().truncate` / `note_push` in `quote!` blocks. ALL 10
shape bodies take `builder: &mut TapeBuilder<…>` only — NONE take
`&mut ValueBuilder<…>`. This cross-confirms AUDIT-A §5 findings
by shape-file: the fused-pipeline write side is substrate without
write-path.

## 3. Per-grammar projection totality

BbnfBootstrap lives in `generated.rs` (grep-verifiable direct).
JsonParser / CssL4Parser / GoogleSheetsParser live only in
`#[derive(Parser)]` expansion — grep against AY-I-era `target/expand/
ay-*.rs` at 02:00–03:37 UTC (PARTIAL: predates W0; reflects the
pre-`db979564` shim-based emission, not the post-W0.d materializer-
per-admission state).

| Grammar | PROJ_DIRECT_TO_STRUCT | PROJ_MATERIALIZERS (const) | PROJ_CONSUMERS (const) | `pub fn materialize_projection_*` | `pub struct …Projection` | `__named_type_shim_*` | Equality verdict | Evidence |
|---|---|---|---|---|---|---|---|---|
| BbnfBootstrap | 15 | 15 | 15 | 15 | 15 | 0 | **PASS 15=15=15=15=15** | `generated.rs:24392,24436,24458`; struct + fn counts via grep (source) |
| JsonParser | PARTIAL | PARTIAL | PARTIAL | 1 (AY-era, stale) | 1 | 1 | **UNVERIFIED** | `ay-json.rs` AY-era; post-W0.d count would require fresh expand |
| CssL4Parser | PARTIAL | PARTIAL | PARTIAL | 48 (AY-era) | 48 | 1 | **UNVERIFIED** | `ay-css-l4.rs` AY-era; 48 is the AY-I baseline AUDIT-B named; post-W0.d tightens |
| GoogleSheetsParser | PARTIAL | PARTIAL | PARTIAL | 10 (AY-era) | 10 | 0 (AY-era, sheets had 0) | **UNVERIFIED** | `ay-sheets.rs` AY-era |

Source confidence for BbnfBootstrap totality:

- `generated.rs:24392` `PROJECTION_DIRECT_TO_STRUCT: &[(&str, &str); 15usize]`
- `generated.rs:24436` `PROJECTION_MATERIALIZERS: &[&str; 15usize]` lists 15 materializer-symbol strings
- `generated.rs:24458` `PROJECTION_CONSUMERS: &[&str; 15usize]` lists 15 `BbnfBootstrapValue::<variant>` consumer paths
- Pre-emitted `pub fn materialize_projection_*_BbnfBootstrap` count in generated.rs = 15 (grep `^    pub fn materialize_projection_`)
- `pub struct Bbnf[A-Z][A-Za-z0-9]*Projection` count = 15 (grep match on projection struct definitions)

`__named_type_shim_*` is 0 in generated.rs (source grep across
`crates/core/src` confirms shim retired; W0.d `db979564`). The
AY-era expand still carries a shim (e.g. `ay-json.rs:
__named_type_shim_string`) because it predates W0.d. Net:
BbnfBootstrap passes totality `15 == 15 == 15 == 15 == 15` with 0
shim. Other three grammars UNVERIFIED at post-W0.d — requires
fresh expand to close.

## 4. Fused-pipeline signature walk

No successful post-W0 `ay-ii-json.rs` expand capture in time budget.
Substitute: trace the emitter source at `grammar.rs:1072-1160`,
`value_materialize.rs:215-289`, + `generated.rs:25656-25697`
(BbnfBootstrap concrete). Every claim carries source citation.

1. **`grammar.rs:1082-1092` parse-entry tape alloc.**
   `TapeBuilder::with_capacity(GRAMMAR_PROFILE.capacity_for(input.len()))`
   — allocates the tape builder. PASS.

2. **`grammar.rs:1101-1104` parse-entry value alloc.**
   `ValueBuilder::<Self>::new(GRAMMAR_PROFILE.capacity_for(input.len()))`
   — allocates the ValueBuilder. Source-confirmed PASS, but as-is
   this builder is empty of any write path.

3. **`grammar.rs:1105-1132` dispatcher invocation.**
   `let off = #dispatcher(__input_bytes, &mut pos, &mut state, &mut builder)`
   — passes `&mut builder` (TapeBuilder) only. **`&mut value_builder`
   is NEVER passed.** FAIL — cross-confirms AUDIT-A §5 +
   AUDIT-D Q2 §Critical-gap.

4. **`grammar.rs:1145-1147` tape finish.** `builder.finish()` — yields
   `Tape`. PASS.

5. **`grammar.rs:1148-1154` value finish.**
   `let value = value_builder.finish(root_off.0)`. Calls
   `ValueBuilder::finish` on an unwritten arena. Returns a
   `ValueBuilderOutput<R>` whose `is_empty()` will be `true`
   (`value_builder.rs:406,472,489`). FAIL — cross-confirms empty-slab.

6. **`grammar.rs:1155-1158` Parsed construction.** `Parsed::new_fused(
   tape, input, root_off, value)`. PASS structurally; value handle
   is empty.

7. **`parsed.rs:348-353` `Parsed::to_value`.** Routes to
   `R::project_value_output(&self.value_builder_output, self.input)`
   — a thin projector. PASS — invariant §1.

8. **`value_materialize.rs:269-288` `project_value_<Grammar>` emission
   template.** Body:

   ```rust
   if output.is_empty() {
       ::core::panic!(
           "AY-II.W0.c: Parsed::to_value() called on an empty value
            substrate; fused parse entry was not invoked. See
            docs/tranches/AY-II/waves/W0.md §W0.c."
       );
   }
   #frame_fn(output, input, output.root_offset())
   ```

   Body compiles — but panics unconditionally because of §5 empty
   slab. **Every grammar's `to_value()` PANICS on every non-empty
   parse.** Cross-confirms AUDIT-D §Q2-§Critical-gap and AUDIT-A §5.

9. **`generated.rs:25656-25666` concrete `project_value_BbnfBootstrap`**
   carries the same template — the `output.is_empty() { panic! }`
   branch is inlined at line 25660-25664.

10. **`generated.rs:25683-25697` concrete
    `materialize_projection_bool_lit_BbnfBootstrap`**:

    ```rust
    let tape = view.cursor().tape();
    let rec = view.cursor().record();
    let __bytes = tape.payload_bytes(rec, 8)?;
    ```

    Reads FROM TAPE. Does NOT read from `ValueBuilderOutput`. This
    confirms AUDIT-C's finding at line 25686-25693: the W0.d
    materializer family and the W0.c `project_value_*` family live
    on different substrates — W0.c → ValueBuilderOutput (empty),
    W0.d → tape-payload-bytes (active). **These are two
    divergent fused-pipeline seams, not one.** The `materialize_
    projection_*` helpers would work — but zero call sites (see §8
    below).

## 5. Structural-scan policy expand inspection

Per-grammar `STRUCTURAL_SCAN_POLICY` emission. Source-verified:

- `generated.rs:12382` — 1× `STRUCTURAL_SCAN_POLICY` entry list,
  `#[allow(dead_code)]` at line 12381.
- `crates/core/src/backend/rust/emitter/shapes/dispatcher.rs:1867`
  + `1899` — emitter site + `lookup_scan_policy` helper
  definition, both under `#[allow(dead_code)]`.
- `byte_class.rs:1` — emission of policy entries (one match).

Consumer count in `generated.rs` for the three scan primitives:

- `object_key_seek`: 0 call sites in `generated.rs` (3 hits all in
  a single doc-comment block at `generated.rs:12375-12377`).
- `bounded_lookahead`: same — 0 call sites, doc-comment only.
- `scan_structural_bounded`: same — 0 call sites.
- `lookup_scan_policy` call sites outside definition: 0 in
  `crates/**`.

`__path_walk` inspection — the emitter for `__path_walk` lives in
`crates/core/src/backend/rust/view/value.rs`; post-W0.e, it was to
consume the policy per AUDIT-C §3.2 plan. At
`grep STRUCTURAL_SCAN_POLICY crates/core/src/backend/rust/view/`
yields 0 matches. **`__path_walk` does NOT reference the policy.**

In all `STRUCTURAL_SCAN_POLICY` entries at `generated.rs:12382+`,
`activation: ScanActivationFlags::from_bits(0)` — every rule's
activation bitmap is zero. Even if a consumer did read the policy,
every entry would fall through. Substrate is thrice-dead: no call
sites, zero activation, consumerless marker. Cross-confirms
AUDIT-A §7 + AUDIT-C §3.1 + AUDIT-D §Q1-§Structural-scan-row.

**Verdict §7**: FAIL. Policy+primitive substrate without any
production consumer. The exact AY-I AUDIT-A §5 AV anti-pattern
AY-II §7 was framed to prevent.

## 6. Cursor + tape API surface verification

Source at `crates/tape/src/builder.rs` + `cursor.rs` + `columns.rs`
(each grep-verified at HEAD `b5bbda6c`):

- `TapeBuilder::begin_compound` **6-arg form confirmed** at
  `builder.rs:324-351`: `(kind: TapeKind, span_lo: u32, variant_idx:
  u8, meta_idx: u8, frame_depth: u8, extra_flags: u16) -> u32`.
  `AY-II.W0-fix` comment at `:319-322` documents the re-admission of
  `variant_idx` + `meta_idx` dropped in initial W0.a (`f8ac2cd7`).
  PASS.
- `TapeBuilder::end_compound` at `builder.rs:364` — pre-order form.
- `TapeBuilder::end_compound_post_order` at `builder.rs:401` — the
  post-order variant added by `f8ac2cd7` to atomically stamp
  `span_hi + child_off + HAS_CHILDREN_BIT`. Called by the emitter
  at 20 sites across 7 shapes (`rollback_to|columns_mut|
  begin_compound|end_compound|push_compound|push_leaf` grep).
  PASS.
- `Columns::rollback_to` at `columns.rs:206+` — atomic multi-column
  truncate.
- `TapeBuilder::rollback_to` at `builder.rs:195`.
- `note_push`, `SIB_SKIP_STAMPED_BIT`, `OpenFrame`, `open_stack` —
  **all 0 matches** across `crates/tape/src`. Confirmed PASS for
  invariant §3. (AUDIT-D §Q3 also names this.)
- `push_compound_fused` in `columns.rs:481` — fused variant under
  `Columns` (not `TapeBuilder`), referenced by `dedup.rs:354` via
  `push_compound_referring`. This is the dedup-substrate lane,
  distinct from the emitter shape lane.

## 7. §2 public-API surface check (TapeBuilder::push_compound)

Source verification:

1. `crates/tape/src/builder.rs:247` — `pub fn push_compound(…)`
   remains PUBLIC on `TapeBuilder`. No `#[doc(hidden)]`, no pub-
   restricted visibility.
2. Call sites across the repo (grep `push_compound` in `crates/tape/
   src`):
   - `visitor.rs:365`, `visitor.rs:389`, `visitor.rs:515` — the
     visitor-lane writer. (Visitor lane is a legitimate
     substrate-only writer per W0.a archaeology; it's NOT the
     grammar-emitted code path.)
   - `dedup.rs:354` — `push_compound_referring`; calls
     `columns.push_compound_fused` (Columns-level, distinct).
   - All other hits are doc comments.
3. Emitter grep (`crates/core/src/backend/rust/emitter/shapes/*`):
   **0 quote! emissions of `push_compound`.** The only textual
   occurrences are doc-comments in `array.rs:35`, `pratt.rs:31,
   213,296,463`, `wrap.rs:174,177,479`, `flat.rs:33`,
   `unordered.rs:18,34`, `grammar.rs:788`, etc. Zero are live
   emissions.

**Verdict §2**: AY-II invariant text reads as "absent from TapeBuilder
public API AND from every quote! block." The emitter half holds
(0 quote! emissions). The API half fails — `push_compound` is
still publicly callable. Cross-confirms AUDIT-A §2: SOFT-FAIL.
Either the invariant text admits a visitor-lane carve-out (retain
the API, mark `#[doc(hidden)]`), OR the visitor lane rebases onto
`begin_compound` + `end_compound_post_order` + the public API
retires. **AUDIT-B does not prescribe — the sibling AUDIT-C §Path-B
recommendation (FusedBuilder absorption) subsumes both paths.**

## 8. Top-5 root-cause findings informing forward path

1. **§5 fused-write-path is the architectural anchor.** Every
   finding in §4 composes back to: the `#dispatcher` call at
   `grammar.rs:1107-1112` threads only `&mut builder`, not `&mut
   value_builder`. Every shape fn takes `builder: &mut TapeBuilder`
   only (§2 above, all 10 shapes). Every `value_builder.finish` is
   called on an empty arena. Every `project_value_<Grammar>` emits
   a panic-on-empty branch that is always taken. Conclusion:
   AUDIT-A §5 FAIL is load-bearing; AUDIT-C §Q1-§Path-B
   (FusedBuilder collapse) or §Path-A (thread value_builder
   through 25 shape fns) is the one forward choice. Expand
   confirms nothing that source doesn't already say — but expand
   would put a name on the panic hot-path. **CONFIRMS AUDIT-A §5 +
   AUDIT-D Q2 — no contest.**
2. **W0.c and W0.d emit-materializers against different
   substrates.** `project_value_<Grammar>` reads
   `ValueBuilderOutput` (source: `value_materialize.rs:270-288`;
   generated `generated.rs:25656-25666`).
   `materialize_projection_<rule>_<Grammar>` reads
   `view.cursor().tape().payload_bytes(rec, …)` (source:
   `value_materialize.rs:…` emission template; generated
   `generated.rs:25683-25697`). **Two divergent fused-pipeline
   seams.** Even if W0.b's threading landed, the W0.d direct-to-
   struct materializers wouldn't benefit — they'd still read the
   tape. Forward path must pick: either both substrates source
   from the fused value column (AUDIT-C §Path-B via FusedBuilder
   ValueColumn), or `materialize_projection_*` is explicitly a
   tape-payload-bytes fast path and the invariant §1 ("no tape-
   walking materializer") relaxes to "no tape-walking at
   `to_value()` root; per-rule `Parsed::get<T>` via tape is OK."
   **REFINES AUDIT-C §Q1 observation #6 in AUDIT-A §8 into a
   structural decision.**
3. **Projection totality post-W0.d cannot be cross-grammar
   verified from expand alone.** BbnfBootstrap is fully closable
   against `generated.rs` (§3 — 15=15=15=15=15, no shim). The
   other three grammars live in derive-macro output; source-only
   verification would require emitter-code inspection (emitter
   emits one materializer per admission + one marker + one
   consumer per W0.d's `db979564`) but cannot cross-count against
   `PROJECTION_*` length literals without a fresh post-W0 expand.
   **OPEN item for W1 pre-open expand-capture cycle.** Stale AY-I
   expansions put JsonParser/CssL4Parser/GoogleSheetsParser at
   1/48/10 projections — post-W0.d tightens but direction
   unknown without expand.
4. **`STRUCTURAL_SCAN_POLICY` is triple-dead**: 0 call sites, 0
   activation bits, 0 `__path_walk` consumer reference. Distinct
   from an AU-era dead surface — the scan primitives on cursor
   (`tape::cursor::object_key_seek/bounded_lookahead/
   scan_structural_bounded`) are alive and callable, but nothing
   emits the call. Forward path per AUDIT-C §Q2: wire
   `__path_walk` at emit time to consume `lookup_scan_policy`
   (~+80 LOC in `view/value.rs`), OR retire the whole surface
   (revert `487b17b7` + `61d0338c`). **CROSS-CONFIRMS AUDIT-A §7,
   AUDIT-C §3, AUDIT-D §Q1.**
5. **`TapeBuilder::push_compound` API half-retirement.** Not a
   live-code problem (0 emitter quote! uses per §2) but an
   invariant-text truth problem. The visitor lane at
   `visitor.rs:365,389,515` is a legitimate secondary writer;
   AY-II §2 must either carve it out explicitly, or the visitor
   lane rebases onto the unified API. **CROSS-CONFIRMS AUDIT-A §2
   (soft FAIL); subsumed by AUDIT-C's FusedBuilder path which
   would retire the split entirely.**

## 9. Expand-truth vs sibling-audit contest

Zero contests. Every source-level finding §2 through §7 corroborates
the sibling audits:

- AUDIT-A §5 (FAIL — value_builder not threaded) — CONFIRMED at
  `grammar.rs:1107-1112` + all 10 shape fn signatures (§2).
- AUDIT-A §2 (FAIL — `push_compound` public API retained) —
  CONFIRMED at `builder.rs:247` (§7).
- AUDIT-A §7 (PARTIAL — STRUCTURAL_SCAN_POLICY consumerless) —
  STRENGTHENED (§5: triple-dead, not merely consumerless).
- AUDIT-C §Q1 (recommendation: FusedBuilder Path-B) — CONFIRMED
  applicable: both fused-pipeline seams (§4 + finding #2) resolve
  cleanly under ValueColumn-absorbing FusedBuilder.
- AUDIT-C seam gap at `generated.rs:25686-25693` (materializer
  reads tape) — CONFIRMED + STRENGTHENED by finding #2 — this is
  a SECOND fused-pipeline seam distinct from §5, not a corollary.
- AUDIT-D Q2 (to_value() panics on non-empty parse) — CONFIRMED at
  `value_materialize.rs:281-285` + `generated.rs:25660-25664` +
  source-causal chain through `grammar.rs:1107-1112`.

## 10. Scope-reveal + partial-verdict markers

- **Per-grammar expand capture (target §1)**: PARTIAL. Zero
  successful post-W0 expand runs; all shared-target compile cycles
  blocked on `bbnf core → gorgeous` check phase (likely a
  target-dir inc-comp cache corruption from the earlier usage-cutoff
  AUDIT-B dispatch). Ay-I-era expansions at
  `/Users/mkbabb/Programming/bbnf-lang/target/expand/ay-*.rs` dated
  02:00-03:37 UTC on 2026-04-21 PREDATE W0.a=04:29; cited for
  totality ceiling only, not post-W0.d reconciliation. **UNVERIFIED
  FROM EXPAND; source-verified direct on BbnfBootstrap (§3 PASS)
  + emitter source (§2, §4 PASS).**
- **Projection totality for JsonParser/CssL4Parser/GoogleSheetsParser
  (§3)**: UNVERIFIED. Fresh expand required to assert 1:1:1 per
  grammar. W1-open pre-cycle must run the full 5-expand matrix.
- **§8.5 public-API surface check**: CONFIRMED soft-FAIL, no expand
  contest; source grep at `builder.rs:247` is definitive.
- **Fused-pipeline expand-line citations in §4**: SOURCED (not
  expanded) for JsonParser; expanded for BbnfBootstrap via
  `generated.rs:25656-25697`. The panic text is byte-identical
  across grammars (derived from one emitter template at
  `value_materialize.rs:281-285`).

Expand-capture operational note for the orchestrator: the shared
`target/` symlink between the main worktree and audit worktrees
means a prior session's stuck incremental state blocks every sibling
audit's expand run. A clean `cargo clean -p bbnf` before the next
expand cycle (or use of a freshly-provisioned worktree for expand-
only sub-agents) would unblock — outside this read-only audit's
scope.

---

No code touched. No test run. Every verdict sourced to `file:line`.
AY-II invariant verdicts align with sibling audit A/C/D
conclusions — zero contests raised by expand-truth or
source-truth.
