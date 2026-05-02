# AZ-IV.W1-CLOSE — Plan Lane

**Lane**: plan (write-authorized to this artefact only)
**Date**: 2026-05-01
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w1-close-plan`
**Base**: `4c28c2a8` (post-W1-CLOSE research commit)
**Research handoff**: `docs/tranches/AZ-IV/audit/W1-CLOSE-research.md`
**Residual ledger**: `docs/tranches/AZ-IV/audit/W1-zero-halt.md`

## §1 Synthesis

The 13 W1 residuals partition into three independent defect classes
with disjoint file footprints, totalling 7 named source files plus
two regenerated grammars. Class A (Sheets, 7 failures) is a Pratt
mining over-share cascading into ArrayRow serialisation, plus two
typed-Span capture gaps in the Wrap-shape codegen for `string`
(quotes stripped) and `range_end` (regex bytes consumed, no leaf).
Class B (CSS L4, 5 failures) is a prefix-tree-factor blind spot in
`branches.rs::find_map_fn` that drops 148/150 namedColor u32 payloads,
plus a `push_branch_tag` catch-all in `css_l4/builder.rs` that
mis-routes dirPseudo and namedColor tags into `CssGlobalKeyword`.
Class C (TS, 1 failure) is a missing `type Color = …` declaration in
`backend/ts/emitter/grammar.rs`'s union preamble.

The two open plan-lane decisions resolve as follows. **Decision 1
(Pratt LUT scope)**: per-rule LUT scoping at
`operator_chain.rs:227-266`. Pratt is a grammar-general shape; a
reclassification of `array_row`/`array_rows` out of `ShapeTag::Pratt`
is escape-hatching that surfaces the underlying generality flaw on
the next list-shaped Pratt rung. Per-rule scoping is the
architectural fix. **Decision 2 (TS Color type)**: emit
`type Color = unknown` in the Runtime-types preamble. The W1.4 gate
is `tsc --noEmit` typecheck pass; `unknown` is the lightest
declaration that satisfies it; W5 owns the structural / executable
binding when it lands the runtime. Mechanism (a) of research §C.3 is
the chosen path.

The redress lane carves into three disjoint sub-units (W1-CLOSE.A/B/C)
dispatched into three sibling worktrees, with caps 30/30/15 min as
the research lane recommended; this plan confirms those caps.

## §2 Routing

| Sub-unit | Worktree (sibling) | CARGO_TARGET_DIR | HARD CAP | Files (modify) | Tests closed |
|---|---|---|---|---|---|
| W1-CLOSE.A (Sheets) | `/Users/mkbabb/Programming/bbnf-wt-aziv-w1-close-A` | `…/target/close-A` | 30 min | `crates/ir/src/passes/recognizers/operator_chain.rs`; `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs` (or `string/struct_direct.rs` analog where the regex span emission lives); `crates/core/src/runtime/google_sheets/document.rs` (only if A.2 narrowing leaves Tag children); `crates/core/src/grammar/generated/google_sheets.rs` (regen) | 7 |
| W1-CLOSE.B (CSS L4) | `/Users/mkbabb/Programming/bbnf-wt-aziv-w1-close-B` | `…/target/close-B` | 30 min | `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs`; `crates/core/src/runtime/css_l4/builder.rs`; `crates/core/src/grammar/generated/css_l4.rs` (regen) | 5 |
| W1-CLOSE.C (TS) | `/Users/mkbabb/Programming/bbnf-wt-aziv-w1-close-C` | `…/target/close-C` | 15 min | `crates/core/src/backend/rust/emitter/grammar.rs` (TS variant — `crates/core/src/backend/ts/emitter/grammar.rs`) | 1 |

Disjointness: A's only file under the W1.5 emitter modify-carve is
the Wrap-shape struct-direct emitter (or its string analog) — A
modifies the regex-leaf push code region; B modifies `find_map_fn`
and `inline::emit_seq_branch_structural_struct_direct` inside
`alt_dispatch/branches.rs`. A does not touch `branches.rs`; B does
not touch the Wrap-shape file. C is entirely inside `backend/ts/`.
No file appears in more than one packet's modify list.

The three units dispatch in parallel; total worktree-wall is the max
cap = 30 min. The orchestrator should commit the W1-CLOSE-research +
W1-CLOSE-plan docs before the parallel dispatch lands so the redress
lanes start from a clean base.

## §3 Exact Wave-Amendment Text

The W1 file-bounds table at `docs/tranches/AZ-IV/waves/W1.md:45-71`
lacks one surface required by Class A: the IR pass file
`crates/ir/src/passes/recognizers/operator_chain.rs`. Every other
W1-CLOSE surface is already inside W1's bounds (`crates/core/src/runtime/css_l4/**`,
`crates/core/src/backend/rust/emitter/shapes/**`,
`crates/core/src/backend/ts/**`,
`crates/core/src/runtime/google_sheets/**` covered by
the `runtime/{…,google_sheets,…}/arena.rs` line, with
`document.rs` in the same `runtime/google_sheets` subtree per modify
scope, and `grammar/google-sheets/**` for grammar edits if needed).
The amendment adds one row.

Insert the following row into `docs/tranches/AZ-IV/waves/W1.md`'s
File Bounds table, in alphabetical position after the
`crates/ir/src/registry/strategy.rs` row:

```markdown
| `crates/ir/src/passes/recognizers/operator_chain.rs` | modify-carve (W1-CLOSE.A — per-rule Pratt LUT scoping; lines 227-266) |
```

Additionally, add the following row after the
`crates/core/src/runtime/{json,css_l4}/builder.rs` row to make the
Sheets serialiser modify scope explicit (it is implicitly inside
`runtime/google_sheets/**` via the existing arena.rs row, but the
`document.rs` file is not literally enumerated):

```markdown
| `crates/core/src/runtime/google_sheets/document.rs` | modify-carve (W1-CLOSE.A — ArrayRow serialiser, lines 281-288) |
```

The Hard Gate section needs no change — Gate 2 ("zero failures")
already covers the W1-CLOSE residuals; Gates 5/6/8/11/12/13 stay
intact.

The W1.5 unit description at `W1.md:117-120` should append one
clause describing the W1-CLOSE.B modify scope:

```markdown
W1.5 also owns the W1-CLOSE.B redress: extending
`alt_dispatch/branches.rs::find_map_fn` to descend into Alt branches
and pushing per-inner-arm Map { fn_id } payloads (the prefix-tree-
factor blind spot that drops 148/150 namedColor u32 payloads).
```

Orchestrator may instead embed these amendments verbatim in the
redress dispatch packets (§4 below) without touching W1.md, since
W1's wave is already in flight and the amendments are scoped to the
carve granularity already permitted.

## §4 Redress Dispatch Packets

### §4.A W1-CLOSE.A — Sheets

You are the W1-CLOSE.A redress lane (Sheets). HARD CAP: 30 min. At
0.9N (27 min) commit, at N (30 min) halt.

**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w1-close-A`
(refuse if `git worktree list` does not show this entry; the
orchestrator creates it from base `4c28c2a8`).
**CARGO_TARGET_DIR**:
`/Users/mkbabb/Programming/bbnf-wt-aziv-w1-close-A/target/close-A`
(export before any cargo invocation).

**Read first**:
1. `docs/tranches/AZ-IV/audit/W1-CLOSE-research.md` §1, §5 (Class A
   surface enumeration).
2. `docs/tranches/AZ-IV/audit/W1-CLOSE-plan.md` §3 (file-bound
   amendment).
3. `crates/ir/src/passes/recognizers/operator_chain.rs:227-266`
   (the Pratt LUT-projection seam).
4. `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs`
   (the Wrap-shape regex emitter — Class A.4 surface).

Read-size preflight: `wc -l` first.

**Mandate**:

1. **A.2 — per-rule Pratt LUT scoping**. At
   `operator_chain.rs:227-266`, narrow the
   `collect_operator_chains` projection so each chain rung receives
   ONLY its own operator entries, not the union of all rungs in the
   detected chain. Mechanism: split the `PrecedenceTable.entries`
   collection so `entries_for_rung(rung_id)` returns only the
   operators that the rung's body literally introduces. The codegen
   downstream (in `pratt/struct_direct.rs:272-338`) already
   consumes per-rule LUTs; the change is at the projection layer.
   Verify: regen Sheets, then check
   `crates/core/src/grammar/generated/google_sheets.rs`'s
   `PRECEDENCE_LUT_array_row` holds only byte 44 (`,`); the
   `array_rows` LUT holds only byte 59 (`;`).

2. **A.3 — string Span capture**. The codegen at
   `crates/core/src/grammar/generated/google_sheets.rs:2369-2456`
   pushes `body = &input[body_start..end]` (inner bytes only) when
   the grammar declares `-> input : Span`. The fix lives in the
   emitter that produces this codegen — `string/struct_direct.rs` or
   the JSON-style scanner shim's lift into Sheets at
   `crates/core/src/backend/rust/emitter/shapes/`. Restore full
   `[open..end+1]` span capture (i.e., include the surrounding
   quotes). Sheets's `""` escape semantics differ from JSON's `\"`
   but no current test exercises embedded escapes; defer the
   pair-on-`""` scanner change.

3. **A.4 — range_end regex Span capture**. `range_end`'s regex alts
   advance `*p` without emitting `push_leaf_with_str`. Fix the
   Wrap-shape emitter (or the regex-led-alt branch helper) so every
   regex match in a Wrap-shape rule whose declared type is `Span`
   pushes the matched bytes. The substrate at
   `alt_dispatch/branches.rs:108-125` does this for AltDispatch
   shapes; the regression is the Wrap path. Cross-reference
   `wrap/struct_direct.rs` for the Wrap shape's regex emission site.

4. After all three sub-fixes land: regen Sheets
   (`cargo xtask regen --grammar google-sheets`); run
   `cargo nextest run -p bbnf-core --test sheets_self_parity --cargo-profile ax-iter`.
   All 7 named tests must pass. Run the full workspace nextest sweep
   to confirm no Sheets-side regression elsewhere.

5. Commit each sub-fix as its own commit:
   - `fix(ir/operator-chain-per-rule-lut): scope Pratt LUT entries to owning rung (AZ-IV.W1-CLOSE.A)`
   - `fix(emitter/wrap-string-span-capture): include surrounding quotes in -> Span emission (AZ-IV.W1-CLOSE.A)`
   - `fix(emitter/wrap-regex-span-capture): push matched bytes for regex-led alt branches (AZ-IV.W1-CLOSE.A)`
   - `regen(grammar/google-sheets): post-W1-CLOSE.A operator-chain + Wrap-span (AZ-IV.W1-CLOSE.A)`

   Generated-file commit body cites the source-edit commit hashes.

**Hard gate**: 7 sheets_self_parity tests pass; full workspace
nextest reports `≤ 6` fails (Class B + Class C remain until B/C
land). Lint cadence: `cargo fmt --all -- --check` +
`cargo clippy --profile ax-iter -p bbnf-core` +
`cargo clippy --profile ax-iter -p bbnf-ir` + `git diff --check`.

**Empty-return rule**: if A.2's per-rule scoping reveals a downstream
codegen consumer that requires the union LUT (e.g., a serialiser
arm), halt and report — do not fall back to the predicate-tightening
alternate without explicit go-ahead. The plan's chosen mechanism is
per-rule scoping; an unworkability triggers triumvirate-of-triumvirate.

Return format: ≤ 300-word summary; commit hash list; failure-count
trajectory; halt time vs cap.

### §4.B W1-CLOSE.B — CSS L4

You are the W1-CLOSE.B redress lane (CSS L4). HARD CAP: 30 min. At
0.9N (27 min) commit, at N (30 min) halt.

**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w1-close-B`
(refuse if mismatch).
**CARGO_TARGET_DIR**:
`/Users/mkbabb/Programming/bbnf-wt-aziv-w1-close-B/target/close-B`.

**Read first**:
1. `docs/tranches/AZ-IV/audit/W1-CLOSE-research.md` §2, §5 (Class B
   surface enumeration).
2. `docs/tranches/AZ-IV/audit/W1-CLOSE-plan.md` §3 (file-bound
   amendment).
3. `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs:165-282`
   (the prefix-tree-factor blind spot).
4. `crates/core/src/runtime/css_l4/builder.rs:383-530, 889-930`
   (the begin_compound dispatch + push_branch_tag catch-all).

Read-size preflight: `wc -l` first.

**Mandate**:

1. **B.2 — per-inner-arm Map payload emission**. Extend
   `branches.rs::find_map_fn` (lines 228-243) to descend into nested
   `Alt` children when the outer branch is `Seq(prefix, Alt(...))`.
   Mechanism (a) from research §B.4: when the helper
   `inline::emit_seq_branch_structural_struct_direct` (lines 165-196)
   emits inner Alt arms, it must call `branch_payload_push` per
   inner arm and emit the matching `push_leaf_with_u64` (or
   `push_leaf_with_unit` for unit-typed projections) immediately
   after the inner literal match succeeds. Each prefix-factored arm
   carries its own `Map { fn_id }`; the per-arm payload is
   recoverable from the inner arm's IR.

2. **B.3 — DirPseudo + namedColor OpenFrame routing**. In
   `runtime/css_l4/builder.rs`, at the `begin_compound` dispatch
   (lines 383-530), add explicit arms for rule_id 71 (`dirPseudo`)
   and rule_id 2 (`namedColor`). Introduce
   `OpenFrame::DirPseudo { kind_tag: Option<u8> }` and
   (only if B.2's per-arm push doesn't already route the namedColor
   payload through `push_leaf_with_u64`) `OpenFrame::NamedColor`. In
   `end_compound` deposit `Selector::PseudoClass(":dir(<kind>)")`
   into the enclosing SelectorList for `DirPseudo`. ALSO narrow the
   `push_branch_tag` catch-all at lines 917-928: drop the
   `CssGlobalKeyword::from_discriminant` trial-cast (and the
   MathOperator equivalent if it exhibits the same pattern); these
   should fire only from their owning rules' frames, never from a
   Wrap-frame fall-through.

3. After both sub-fixes land: regen CSS L4
   (`cargo xtask regen --grammar css-l4`); run
   `cargo nextest run -p bbnf-core --test css_l4_named_color_parity --test css_l4_parity --cargo-profile ax-iter`.
   The 5 named tests must pass. Full workspace nextest sweep to
   confirm.

4. Commit each sub-fix as its own commit:
   - `fix(emitter/alt-dispatch-per-arm-map-payload): descend into nested Alt for prefix-factored Map projection (AZ-IV.W1-CLOSE.B)`
   - `fix(runtime/css-l4-dir-pseudo-frame): route rule_id 71 to DirPseudo OpenFrame; deposit Selector::PseudoClass (AZ-IV.W1-CLOSE.B)`
   - `fix(runtime/css-l4-push-branch-tag-narrow): drop GlobalKeyword/MathOperator trial-cast in catch-all (AZ-IV.W1-CLOSE.B)`
   - `regen(grammar/css-l4): post-W1-CLOSE.B alt-dispatch payload + DirPseudo (AZ-IV.W1-CLOSE.B)`

**Hard gate**: 5 CSS L4 tests pass; full workspace nextest reports
`≤ 7 - delta_A` fails. Lint cadence: `cargo fmt --all -- --check` +
`cargo clippy --profile ax-iter -p bbnf-core` + `git diff --check`.

**Empty-return rule**: if mechanism (a) reveals that the per-inner-arm
emission cannot recover the Map fn_id without a structural IR change
(i.e., the prefix-tree-factor pass actually drops the per-arm
projection at its rotation step), halt and report — do not retreat
to mechanism (b) (pre-codegen IR un-factoring) without explicit
go-ahead. Plan's chosen mechanism is (a).

Return format: ≤ 300-word summary; commit hash list; failure-count
trajectory; halt time vs cap.

### §4.C W1-CLOSE.C — Backend TS

You are the W1-CLOSE.C redress lane (Backend TS). HARD CAP: 15 min.
At 0.9N (13.5 min) commit, at N (15 min) halt.

**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w1-close-C`.
**CARGO_TARGET_DIR**:
`/Users/mkbabb/Programming/bbnf-wt-aziv-w1-close-C/target/close-C`.

**Read first**:
1. `docs/tranches/AZ-IV/audit/W1-CLOSE-research.md` §3 (Class C
   evidence).
2. `crates/core/src/backend/ts/emitter/grammar.rs:88-142`
   (the `emit_grammar_types` preamble + host-fn declaration block).
3. `crates/core/src/backend/ts/projection.rs:11-46`
   (`type_desc_to_ts` — confirm it returns the raw Named string).

Read-size preflight: `wc -l` first.

**Mandate**:

1. Extend `emit_grammar_types` in
   `crates/core/src/backend/ts/emitter/grammar.rs` (lines 88-112)
   to walk every `TypeDesc::Named(sid)` referenced anywhere in
   `ir.types`, deduplicate by name (sorted), and emit one
   `type <Name> = unknown;` line in the "Runtime types" preamble
   (above the union-body emission at lines 103-112). The collection
   should iterate over the IR's `types` map and recursively descend
   into composite TypeDescs (Vec / Tuple / Map projections) to catch
   Named types nested inside.

2. The W5 wave will replace `unknown` with the executable runtime
   type once the TS binding lands; W1's gate is `tsc --noEmit`
   typecheck pass and `unknown` is the lightest declaration that
   satisfies it. Do NOT emit a structural alias from the host fn's
   declared return shape (the alternate research mechanism); that is
   W5's concern and would entangle this fix with the host-fn return
   typing pipeline.

3. Run
   `cargo nextest run -p bbnf-core --test backend_ts_typecheck --cargo-profile ax-iter`
   and confirm `ts_tempdir_typecheck_representative_grammars` passes.
   Spot-check the emitted `css_l4.ts` shows `type Color = unknown;`
   above the union body.

4. Commit:
   - `fix(backend/ts-named-type-preamble): emit type <Name> = unknown for grammar Named types (AZ-IV.W1-CLOSE.C)`

**Hard gate**: 1 backend_ts test passes; full workspace nextest
reports `0` fails (the W1 hard gate, assuming A and B have already
landed). Lint cadence: `cargo fmt --all -- --check` +
`cargo clippy --profile ax-iter -p bbnf-core` + `git diff --check`.

**Empty-return rule**: if the test's actual failure shape diverges
from the research diagnosis (e.g., another TS type beyond `Color`
also lacks a declaration, or `tsc` reports a different error class),
halt and report with the exact `tsc` output — do not improvise a
broader fix. The plan's scope is `unknown`-aliases-for-every-Named.

Return format: ≤ 200-word summary; commit hash; halt time vs cap.

## §5 Risk Note

Two unknowns the plan lane could not resolve from research alone:

1. **A.4 fix locality**. The research artefact identifies the regex-
   led Wrap-shape branch as the missing `push_leaf_with_str` site,
   citing `wrap/struct_direct.rs` AND `alt_dispatch/branches.rs:108-125`
   as candidate emission sites. The redress lane will discover which
   helper actually owns the Wrap regex emission. If the fix needs to
   touch both files, the cap may pressure; the per-sub-fix commit
   discipline keeps progress visible.

2. **B.2 prefix-tree-factor IR shape**. The research artefact
   describes the post-factor IR as `Seq(Literal("a"), Alt([map(...,
   Literal), …]))` but does not enumerate the exact arm shape after
   factoring. Mechanism (a) assumes each inner arm is `Seq(Literal_suffix,
   Map { fn_id })` or `Map { fn_id }` directly. If factoring produces a
   shape where the Map node lives ABOVE the Seq (i.e., `Map { fn_id,
   inner: Seq(...) }`), the per-arm payload push needs to land inside
   the inner Seq emission rather than after the inner literal match.
   The redress lane will discover this on first read of the prefix-
   tree-factor pass output. If mechanism (a) genuinely cannot recover
   the payload, the empty-return rule routes to triumvirate-of-
   triumvirate; the plan does NOT pre-authorise mechanism (b).

3. **B.3 namedColor branch-tag interaction with B.2**. If B.2's
   per-arm push lands the u32 payload via `push_leaf_with_u64`
   (consumed at the runtime side via the existing leaf-payload arm,
   not via `push_branch_tag`), then namedColor's
   `push_branch_tag(N)` for outer branch N becomes either redundant
   or a tag the runtime should interpret as the prefix-group index
   (not the u32). The redress lane should land B.2 first, then
   exercise the test, then evaluate whether the
   `push_branch_tag` narrowing in B.3 is sufficient or whether the
   namedColor codegen needs to suppress the outer tag emission. The
   commit ordering (B.2 → B.3) makes this discoverable in-flight.

No fourth defect class emerged from the research re-read. The 13
residuals close inside the three named sub-units. No tranche split,
no thesis amendment, no W1-NEXT carry — W1's hard gate closes when
A/B/C land and the workspace nextest reports zero failures.
