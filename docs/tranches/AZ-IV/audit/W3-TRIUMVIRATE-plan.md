# W3 Triumvirate Plan Synthesis

## Acceptance

The W3 research finding is accepted: W3.1 / W3.2 / W3.3 each landed clean
in isolation, but their integrated lazy lane today is `parse(input)?.get(path)`
with a constructed-but-unconsulted `PathCursor`. The cursor is threaded
into `PathExecutor::execute`, but neither the per-grammar `parse_with`
closures nor the generated shape-dispatch parse functions read it; the
static `PATH_PLAN` is searched by no production caller; sibling subtrees
are never byte-skipped. Hard Gate 7 (`bbnf_get_twitter ≤ 5x sonic_get_twitter`),
AZ-IV §Invariants #9 ("path skips unvisited subtrees"), and W3.4's
lazy-error-elision contract are all blocked behind the seam the research
names. The research's recommendation — two parallel redress agents with
disjoint write paths plus an integration synthesis — is adopted verbatim.
This plan synthesis amends the W3 wave spec by appending two new
sub-units (W3.6 emitter carve + W3.7 entry rewrite) and two new
hard-gate items, leaves W3.1–W3.5 untouched, and binds both redress
agents to the cursor-threading seam at the five injection points the
research enumerates.

## Sub-Unit Allocation

### AZ-IV.W3.6 Cursor-Threading Carve (Emitter + Regen)

- **Mechanism**: extend the per-shape emitter family to thread
  `cursor: &mut PathCursor<'p, P>` through every emitted
  `parse_<shape>_<Grammar>_<rule>` function. At the four hot decision
  points named by the research (Array/Object loop body, Wrap Alt-dispatch,
  Flat positional Seq, Dispatcher entry), insert `cursor.decide(rule_id)`
  consults that translate `Decision::ParseFully` / `ParseUntil(idx)` /
  `Skip` into emitted control flow. `Skip` calls a per-shape
  `skip_<shape>_<Grammar>_<rule>` byte-range scanner that advances `*p`
  past the rule's bytes without emitting builder records (objects:
  brace-balanced; arrays: bracket-balanced; strings: reuse
  `__regex_scan_<Grammar>` / `simd_scan::quoted_string_simd_body`;
  scalars: reuse the regex-scan adapter at `json.rs:262` shape).
  `ParseUntil(u16)` translates to a `break` after the indexed child in
  Array/Object/Flat loops, and to a direct delegate to the indexed
  branch in Wrap Alt-dispatch (dropping the `'try_branches: loop`
  wrapper for the cursor-decided case). `ParseFully` keeps existing
  emitted body byte-for-byte. Eager parses pass `&mut PathCursor::eager()`
  (or the equivalent always-`ParseFully` cursor); the eager 1582-test
  corpus is byte-stable. Regen reproduces the carve across all 9
  grammars, asserting `cargo xtask regen --check` green.
- **Files (may modify)**:
  - `crates/core/src/backend/rust/emitter/shapes/dispatcher/cross_shape.rs` (signature carve at 160-167)
  - `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs` (signature carve + decision consult at 411-418)
  - `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs` (signature carve + decision consult at 340-341)
  - `crates/core/src/backend/rust/emitter/shapes/array/**` and `shapes/object/**` (loop-body decision consult)
  - `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs` (cursor pass-through; no decision consult)
  - `crates/core/src/backend/rust/emitter/shapes/mod.rs` (per-shape dispatch table updates)
  - `crates/core/src/backend/rust/emitter/path_plan.rs` (emit `Decision::Skip` rows where the path-plan walker can prove a sibling subtree is non-target; per-shape `skip_*` scanner emission)
  - `crates/core/src/grammar/generated/{json,csv,sheets,bbnf,css_l4,css_pretty,ebnf,bnf,math}.rs` (regen output)
- **Files (do not touch)**:
  - `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf}/parse_with.rs` (W3.7 owns)
  - `crates/core/src/path/{executor,cursor,schema,ir}.rs` (W3.1 owns; types are canonical)
  - `crates/core/src/runtime/path/**` (legacy walker; out of W3 scope)
  - W2 IR types and `path_check` pass output
- **Sub-gate**:
  1. `cargo xtask regen --check` passes 9/9 with cursor-threaded
     signatures present in every per-grammar `parse_<shape>_*` fn.
  2. Eager 1582-test corpus stays green under
     `cargo nextest run --workspace --cargo-profile ax-iter`
     (a `&mut PathCursor::eager()` cursor produces byte-stable
     emit behaviour).
  3. `crates/core/tests/no_grammar_name_branch.rs` static AST scan
     stays green (the cursor consult is keyed on `(rule_id,
     SegmentKind)`; no rule-name match arms).
  4. `path_plan.rs` regen diff under
     `docs/tranches/AZ-IV/audit/W3-path-plan-regen-diff.txt` shows
     `Decision::Skip` rows present per grammar.
  5. samply attribution evidence at
     `docs/benchmarks/profiles/post-AZ-IV/W3/json_value/twitter_bbnf_parse_with_get/profile.json.syms.json`
     is RECORDED (W3.5 actually computes the bench but Agent A
     ensures the seam exists for samply to attribute against).

### AZ-IV.W3.7 parse_with Entry Rewrite

- **Mechanism**: replace the eager-then-walk indirection that today's
  `parse_with` carries (see `runtime/json/parse_with.rs:74-91`) with a
  cursor-threaded direct dispatcher call against the now-cursor-aware
  generated parse functions. The new shape:

      let mut state = ScanState::new();
      let mut builder = JsonStructBuilder::new();
      let mut pos = 0;
      let mut cursor = PathCursor::new(path, |rid, kind, _| {
          __path_plan::lookup(rid, kind)
              .map(|e| e.decision)
              .unwrap_or(Decision::ParseFully)
      });
      parse_JsonParser_value(input.as_bytes(), &mut pos, &mut state,
                              &mut builder, &mut cursor).ok()?;
      builder.finalise(input).get::<T>(legacy_path)

  The `PathExecutor::execute` orchestration is preserved; only the
  parse-fn closure body changes from `JsonParser::parse(src) + doc.get`
  to a direct cursor-threaded dispatcher call. The lazy-error-elision
  contract follows by construction: bytes the cursor causes the
  dispatcher to skip never reach a parse error path. Land the W3.4
  negative-fixture test alongside (lazy returns `Some(leaf)` on
  malformed bytes past path's reach; eager returns `None`).
- **Files (may modify)**:
  - `crates/core/src/runtime/json/parse_with.rs`
  - `crates/core/src/runtime/css_l4/parse_with.rs`
  - `crates/core/src/runtime/google_sheets/parse_with.rs`
  - `crates/core/src/runtime/bbnf/parse_with.rs`
  - `crates/core/tests/parse_with_*.rs` (negative-fixture lazy-elision rows)
- **Files (do not touch)**:
  - `crates/core/src/backend/rust/emitter/**` (W3.6 owns)
  - `crates/core/src/grammar/generated/**` (W3.6's regen output)
  - `crates/core/src/path/{executor,cursor,schema,ir}.rs` (W3.1 owns)
- **Sub-gate**:
  1. Each of the four `parse_with.rs` files calls the cursor-threaded
     dispatcher directly; no `JsonParser::parse(src)` (or per-grammar
     equivalent) eager call survives in any `parse_with` body.
  2. The four in-module smoke tests (`parse_with_resolves_*`) stay
     green; each parity-against-eager assertion holds.
  3. The lazy-error-elision negative-fixture test (one per grammar)
     passes: `parse_with(malformed_after_target, &path)` returns
     `Some(leaf)`; `parse(malformed_after_target)?.get(path)` returns
     `None` or a parse error.
  4. samply attribution on `bbnf_get_twitter` shows `parse_array`
     loop body invokes `cursor.decide` and takes the `ParseUntil(0)`
     break for the 99 untargeted statuses.

## Worktrees

| Agent unit | Sibling worktree absolute path | CARGO_TARGET_DIR |
|---|---|---|
| AZ-IV.W3.6 Cursor-Threading Carve (Emitter + Regen) | `/Users/mkbabb/Programming/bbnf-wt-aziv-w3-redress-emit` | `/Users/mkbabb/Programming/bbnf-wt-aziv-w3-redress-emit/target/redress-emit` |
| AZ-IV.W3.7 parse_with Entry Rewrite | `/Users/mkbabb/Programming/bbnf-wt-aziv-w3-redress-entry` | `/Users/mkbabb/Programming/bbnf-wt-aziv-w3-redress-entry/target/redress-entry` |

Both worktrees branch from master HEAD `c6ba1719` (the post-research
integration tip, which already contains W3.0 + W3.1 + W3.2 + W3.3 +
the just-landed research artefact). Agent A and Agent B author
independently and in parallel; the build is not runnable end-to-end
until BOTH commit and the orchestrator integrates them onto master via
sequential cherry-pick (Agent A first, then Agent B's branch rebased
onto Agent A's tip — Agent B's runtime call sites resolve only against
Agent A's regen output).

## Disjointness Confirmation

Agent A writes only:

- `crates/core/src/backend/rust/emitter/**`
- `crates/core/src/grammar/generated/**` (regen artefact)
- `docs/tranches/AZ-IV/audit/W3-path-plan-regen-diff.txt` (refresh)

Agent B writes only:

- `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf}/parse_with.rs`
- `crates/core/tests/parse_with_*.rs` (negative-fixture rows)
- `docs/tranches/AZ-IV/audit/W3-error-elision-contract.txt` (new)
- `docs/tranches/AZ-IV/audit/W3-parse-with-coverage.md` (refresh)

Write-path intersection is `{}`. Each agent's `git diff --name-only`
output cannot collide with the other's. The build won't compile
end-to-end until both land on master, but neither agent's commit
breaks its own branch's compile (Agent A's regen output produces
generated parse fn signatures that include `cursor`; Agent B's
runtime calls those new signatures only after cherry-pick or rebase).

## Hard Cap Allocation

- **Agent A (emitter + regen)**: 30 min. Substantial: 12 emitter
  modules touched, 9 regen outputs, 4 hot-decision-point insertions,
  per-shape `skip_*` scanners. At 27 min commit current state; at 30
  min halt.
- **Agent B (parse_with entry rewrite)**: 25 min. Thinner: 4
  parse_with.rs files (≤ 25 LOC each), 4 negative-fixture test rows,
  one shared coverage-doc refresh. At 22.5 min commit current state;
  at 25 min halt.

## Auto-Trigger Conditions for Second Triumvirate

- Either agent reveals a sixth seam beyond the five named by the
  research (e.g. a new hot-path decision point in `alt_dispatch::branches.rs`
  not covered by the four-injection-point list).
- Agent A's regen output requires per-grammar branching in the
  emitter (violates Hard Gate 17 — grammar-overfit static scan).
- Agent A discovers that a shape lacks a feasible byte-range
  `skip_*` scanner (e.g. Pratt rules without delimiter pairs need
  full descent; W3.6's plan would have to drop `Decision::Skip` for
  that shape and route through `ParseFully` only — acceptable, not a
  trigger; but a shape where neither Skip nor ParseFully nor
  ParseUntil applies IS a trigger).
- Agent B finds that the cursor-threaded dispatcher signature
  produced by Agent A is incompatible with the `PathExecutor::execute`
  parse-fn callback shape (forces W3.1 surface revision mid-W3).
- Eager 1582-test corpus regresses under Agent A's regen output
  (the `&mut PathCursor::eager()` invariant is broken — a
  cursor-threaded fn behaves differently than the pre-carve fn under
  always-`ParseFully` cursor).
- Three diagnostic-loop iterations in either agent fail to isolate
  a cursor-threading divergence.
- JSONL transcript quiet >15 minutes for either agent.
- Empty/no-evidence return from either agent triggers one verbatim
  redispatch with the same worktree pointer (per
  `AZ-IV.md §Orchestration Rules` 4); a second empty/no-evidence
  return triggers the second triumvirate.

## Exact Wave-Amendment Text

### Append to `docs/tranches/AZ-IV/waves/W3.md` §Agent Units

```markdown
### AZ-IV.W3.6 Cursor-Threading Carve (Emitter + Regen)

- Mechanism: extend the per-shape emitter family to thread `cursor: &mut PathCursor<'p, P>` through every emitted `parse_<shape>_<Grammar>_<rule>` function. At the four hot decision points (Array/Object loop body, Wrap Alt-dispatch, Flat positional Seq, Dispatcher entry), insert `cursor.decide(rule_id)` consults that translate `Decision::ParseFully` / `ParseUntil(idx)` / `Skip` into emitted control flow. `Skip` calls a per-shape `skip_<shape>_<Grammar>_<rule>` byte-range scanner that advances `*p` past the rule's bytes without emitting builder records. `ParseUntil(u16)` translates to a `break` after the indexed child in compound loops, and to a direct delegate to the indexed branch in Wrap Alt-dispatch. `ParseFully` keeps existing emitted body byte-for-byte. Eager parses pass an always-`ParseFully` cursor; the eager 1582-test corpus is byte-stable. Regen reproduces the carve across all 9 grammars.
- Files: `crates/core/src/backend/rust/emitter/shapes/dispatcher/cross_shape.rs` (signature carve at 160-167), `shapes/flat/struct_direct.rs` (411-418), `shapes/wrap/struct_direct.rs` (340-341), `shapes/array/**` + `shapes/object/**` (loop-body decision consult), `shapes/alt_dispatch/branches.rs` (cursor pass-through), `shapes/mod.rs` (dispatch table), `crates/core/src/backend/rust/emitter/path_plan.rs` (emit `Decision::Skip` rows + per-shape `skip_*` scanner emission), `crates/core/src/grammar/generated/{json,csv,sheets,bbnf,css_l4,css_pretty,ebnf,bnf,math}.rs` (regen output).
- Sub-gate: `cargo xtask regen --check` green 9/9; eager 1582-test corpus green; `no_grammar_name_branch.rs` static scan green; `Decision::Skip` rows present in `path_plan` regen diff; cursor consult sites visible in generated parse fns.

### AZ-IV.W3.7 parse_with Entry Rewrite

- Mechanism: replace the eager-then-walk indirection (`JsonParser::parse(src) + doc.get`) in each `parse_with.rs` body with a cursor-threaded direct dispatcher call against the now-cursor-aware generated parse functions. `PathExecutor::execute` orchestration preserved; only the parse-fn closure body changes. Lazy-error-elision contract follows by construction: bytes the cursor causes the dispatcher to skip never reach a parse error path. Land the W3.4 negative-fixture lazy-error-elision row alongside.
- Files: `crates/core/src/runtime/json/parse_with.rs`, `runtime/css_l4/parse_with.rs`, `runtime/google_sheets/parse_with.rs`, `runtime/bbnf/parse_with.rs`, `crates/core/tests/parse_with_*.rs` (negative-fixture rows).
- Sub-gate: each entry calls the cursor-threaded dispatcher directly (no `JsonParser::parse(src)` survives in any `parse_with` body); the four in-module smoke tests stay green; the lazy-error-elision negative-fixture test passes per grammar; samply attribution on `bbnf_get_twitter` shows the array-loop `cursor.decide` consult firing.
```

### Append to `docs/tranches/AZ-IV/waves/W3.md` §File Bounds

```markdown
| `crates/core/src/backend/rust/emitter/shapes/dispatcher/cross_shape.rs` | modify-carve (W3.6: thread cursor through dispatcher signature lines 160-167) |
| `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs` | modify-carve (W3.6: cursor signature + Flat decision consult at 411-418) |
| `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs` | modify-carve (W3.6: cursor signature + Wrap Alt-dispatch decision consult at 340-341) |
| `crates/core/src/backend/rust/emitter/shapes/array/**` | modify-carve (W3.6: array-loop decision consult) |
| `crates/core/src/backend/rust/emitter/shapes/object/**` | modify-carve (W3.6: object-loop decision consult) |
| `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs` | modify-carve (W3.6: cursor pass-through; no decision consult) |
| `crates/core/src/backend/rust/emitter/shapes/mod.rs` | modify-carve (W3.6: per-shape dispatch table updates) |
| `crates/core/src/backend/rust/emitter/path_plan.rs` | modify (W3.6: emit `Decision::Skip` rows + per-shape `skip_*` scanner stubs) |
| `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf}/parse_with.rs` | rewrite (W3.7: cursor-threaded dispatcher call replaces eager-then-walk) |
```

### Append to `docs/tranches/AZ-IV/waves/W3.md` §Worktree Plan

```markdown
| AZ-IV.W3.6 Cursor-Threading Carve (Emitter + Regen) | `/Users/mkbabb/Programming/bbnf-wt-aziv-w3-redress-emit` | `/Users/mkbabb/Programming/bbnf-wt-aziv-w3-redress-emit/target/redress-emit` |
| AZ-IV.W3.7 parse_with Entry Rewrite | `/Users/mkbabb/Programming/bbnf-wt-aziv-w3-redress-entry` | `/Users/mkbabb/Programming/bbnf-wt-aziv-w3-redress-entry/target/redress-entry` |
```

### Append to `docs/tranches/AZ-IV/waves/W3.md` §Hard Gate

```markdown
13. **Cursor decisions are CONSULTED at parse time (not constructed-and-discarded)**: samply attribution on `bbnf_get_twitter` shows at least one `cursor.decide(rule_id)` consult firing inside a generated `parse_<shape>_<Grammar>_<rule>` function, AND at least one sibling subtree is byte-skipped (skip-scanner advances `*p` without builder records). Evidence under `docs/benchmarks/profiles/post-AZ-IV/W3/json_value/twitter_bbnf_parse_with_get/profile.json.syms.json`.
14. **`bbnf_get_twitter ≤ 5x sonic_get_twitter` on same-harness comparison**: the redress carve achieves the W3 hard floor (target ≤ 1.0x is out of W3 scope and routes only with profile evidence per AZ-IV §Hard Gates 16). Same-harness paired rows under `docs/tranches/AZ-IV/audit/W3-sonic-comparison.json`.
15. **No `*Parser::parse(src)` survives in any `parse_with.rs` body**: each of the four entry points calls the cursor-threaded dispatcher directly; the eager fallback is gone from the lazy lane.
```

### PROGRESS.md update

Replace the existing W3 row in `docs/tranches/AZ-IV/PROGRESS.md` §Wave Status with:

```markdown
| W3 - Lazy Bail-Out Parse | in_progress | `audit/W3-*.{txt,md}` | W3.0 path-egraph-seed landed (commits `c727df9e`, `d186efcc`). W3.1 (executor + cursor + schema, commit `bcff3423`) + W3.2 (parse_with entry points, commit `0e8dbc10`) + W3.3 (codegen path-plan emitter, commits `1bd05e8f`+`937361d5`+`c22e1104`) integrated cleanly in isolation but together produce a non-functional lazy lane: cursor constructed-but-unconsulted, PATH_PLAN unsearched, `parse_with` is `parse() + walk`. Triumvirate fired (research `c6ba1719`, plan landing now); two redress agents in parallel — W3.6 (emitter + regen carve, hard cap 30 min) on worktree `bbnf-wt-aziv-w3-redress-emit`, W3.7 (parse_with entry rewrite, hard cap 25 min) on worktree `bbnf-wt-aziv-w3-redress-entry`. Hard gate 13/14/15 added: cursor-consulted-at-parse-time, sonic ≤ 5x same-harness, no eager `*Parser::parse` in any `parse_with` body. W3.4 (negative-fixture tests) folds into W3.7; W3.5 (bench harness + samply) sequences after both redress agents land. |
```

Append a new ledger row under `docs/tranches/AZ-IV/PROGRESS.md` §Running Evidence Ledger:

```markdown
| 2026-05-02 | W3 triumvirate (plan) | `audit/W3-TRIUMVIRATE-plan.md` | research finding accepted: 2 parallel redress agents A (emitter carve + regen, hard cap 30 min) + B (parse_with entry rewrite, hard cap 25 min) on disjoint write paths; Agent A writes `crates/core/src/backend/rust/emitter/**` + `crates/core/src/grammar/generated/**`; Agent B writes `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf}/parse_with.rs` + `crates/core/tests/parse_with_*.rs`. Worktrees `bbnf-wt-aziv-w3-redress-emit` (target `redress-emit`) and `bbnf-wt-aziv-w3-redress-entry` (target `redress-entry`) branched from master `c6ba1719`. New W3 hard gates 13/14/15: cursor-consulted-at-parse-time (samply attribution), `bbnf_get_twitter ≤ 5x sonic_get_twitter` same-harness, no eager `*Parser::parse` in any `parse_with` body. Auto-trigger second triumvirate if a 6th seam emerges, regen requires per-grammar branching, eager 1582-test corpus regresses under Agent A's output, or cursor signature is incompatible with `PathExecutor::execute` parse-fn shape. |
```
