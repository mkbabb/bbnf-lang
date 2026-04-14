# Tranche AU — Progress Log

Operational protocol: see `/INSTRUCTIONS.md` at repo root.

## Session 1 recap (2026-04-14)

### Pre-AU audit methodology

6 parallel worktree-isolated agents performed deep analysis:

1. **Codegen regression audit** — expanded AQ vs AT JSON parsers,
   identified dead payload captures, quantified overhead sources
2. **Projection activation audit** — all 4 grammars push counts,
   payload firing, KvPair status, .map(|_| ()) catalog
3. **Prior tranche gates** — full test suite, bootstrap idempotency
4. **JSON hot path profile** — instruction counts, push method costs,
   `branch_pushes_children` misclassification root cause
5. **CSS+Sheets+BBNF profiles** — scanner activation, fused scanner
   absence, per-grammar bench numbers
6. **Deferred items + arch debt** — 11-tranche ParsedGrammar deferral,
   dead StructRegistry, schema stubs

### Root cause (audit convergent finding)

4 of 6 agents independently identified: `branch_pushes_children()`
in `alt.rs` returned `true` for `Ref` nodes pointing to rules the
driver will inline via `CallStrategy::InlineBody`. This forced
`mark_children + push_compound` on ALL branches of JSON's `value`
Alt. Every payload capture (`push_leaf_with_f64`, `push_leaf_with_bool`,
`push_leaf_with_u8`) was dead code. Typed payloads were computed
but never stored — a correctness bug, not just a performance bug.

### Architectural verdicts from audit

- **kind_meta packing: KEEP** — strictly better than meta Vec
- **__payload_tag match: KEEP** — clean architecture, negligible cost
- **branch_pushes_children: FIX** — single root cause of regression
- **ParsedGrammar: ELIMINATE** — 11 tranches deferred, BLOCKING
- **StructRegistry: IMPLEMENT OR DELETE** — dead scaffold

## What landed (commits on master)

### AU.1 — Projection activation

**Commit `83357e4`**: `branch_pushes_children` now takes `DriverState`.
Checks `CallStrategy::InlineBody` in addition to `is_transparent` and
`MaterializationClass::TransparentElide`. Recurses into nested
`Alt`/`Skip`/`Next`/`Minus`.

**Commit `83357e4`** (same): `payload_idx` u16 overflow fix.
canada.json has 111K f64 payloads, exceeds u16 max (65535). Byte
offset now stored in `child_off` (u32 range). `payload_idx: 1` is
the sentinel for "payload present, offset in child_off". Readers
in `tape.rs` (`payload_scalar`, `payload_bool`, `payload_bytes`)
updated accordingly.

**Commit `6724a46`**: payload pre-alloc heuristic
`Vec::with_capacity(expected / 8 * 8)`.

**Fixtures**: canada + data_xl tape_parity goldens regenerated;
22/22 pass. Record counts changed because leaf branches now push
leaves instead of compounds.

### AU.4 — Debt elimination (partial)

Cherry-picked from agent work:
- **Commit `5e13a28`**: `gorgeous/tests/vm.rs` `string_index` field
  added; `core/tests/runtime_root.rs` `.kind` → `.kind()`
- **Commit `ef090a5`**: Deleted schema stub emitters (`ts.rs`,
  `runtime.rs`) — pure stubs since early tranches

## The correctness story

**Before fix**: All JSON `value` branches set `__has_children = true`.
Payload epilogue (`push_leaf_with_f64/bool/u8`) was dead code. f64
computed by Eisel-Lemire, then discarded.

**After fix**: Leaf branches (number/bool/null/string) use
`push_leaf_with_*`. f64 values actually stored in the tape. This
is primarily a correctness fix; performance impact is secondary.

## Bench results

| Dataset | AQ (no values stored) | AU (with f64 stored) |
|---------|----------------------|----------------------|
| canada | 1796 | 1294 |
| citm | 2698 | 2627 |
| twitter | 2086 | 2142 |
| data | 1939 | 1890 |

### Unverified claim — flagged

I initially wrote "the canada delta is the inherent cost of
materializing 111K f64 values (888KB of payload writes)". **I did
not verify this.** What I actually observed:

1. Before fix: canada at 1465 MB/s (payload captures dead, but
   Eisel-Lemire still ran)
2. After fix: canada at 1294 MB/s (payload captures now store values)
3. Delta: -171 MB/s

What I did NOT do:
- No samply profile comparing before/after
- No measurement of payload write time in isolation
- No comparison against a version that runs Eisel-Lemire but does
  not store
- No check for other codegen side effects (stack layout, epilogue
  match arms, cache behavior)

Other possible causes I did not rule out:
- `match __payload_tag` epilogue branch prediction
- `child_off` writes now non-NONE for leaves (cache interaction)
- LLVM producing different code for modified `__value` body
- 8-byte `resize` per payload may be the bottleneck
- Test/bench environment variance

**Honest statement**: canada dropped from 1465 to 1294 MB/s after
activating leaf payload storage. I do not know why. Profiling is
required to identify the actual cost source. This is deferred to
AU.5.

## Bonus finding

`inline_acyclic` and `fuse_single_use` are effectively no-ops at
the IR level. `scc_id` is always `Some(...)` during the normalizer
loop because lowering sets it via `scc_index.get(name)` which
returns `Some` for every rule. `compute_scc` correctly sets
`scc_id = None` for non-cyclic rules but runs AFTER the normalizer
loop. Both passes guard on `r.meta.scc_id.is_none()`, so they skip
every rule. All effective inlining happens at the driver level via
`CallStrategy::InlineBody`.

This is a latent bug worth fixing in a future tranche but not
blocking AU — the driver-level inlining produces correct output.

## Mistakes made this session

1. **Stated unverified claims as fact** — "inherent cost of
   materializing 111K f64 values" was speculation, not evidence.
2. **Scattered debug tracing** in alt.rs and grammar.rs during
   investigation instead of deploying agents for focused analysis.
3. **Used `git stash`** mid-work which reverted my changes — had
   to re-apply.
4. **Accidentally worked on a worktree branch**
   (`worktree-agent-a27879e1`) for one commit instead of master.
5. **Did not parallelize aggressively enough** — should have
   deployed agents for AU.2, AU.3, AU.5 in parallel while
   investigating AU.1.

## Phase status

### Phase 1 — Fix projection activation

Status: **COMPLETE** (commits `83357e4`, `6724a46`)

Projection works correctly. Leaf branches emit `push_leaf_with_*`.
Typed payloads (f64, bool, u8) are stored in the tape. Throughput
regression vs AQ (which did not store values) is not explained;
profiling deferred to AU.5.

### Phase 2 — CSS scanner activation

Status: **IN PROGRESS**

**AU.2.1 WS scanner**: `scan_ws_block_comments` IS the fused
scanner — 319 call sites are correct. The "zero fused CSS scanners"
claim from the audit was incorrect. The scanner handles both `\s`
and `/* */` in one pass. No separate `css_ws_comment_fast` is
needed.

**AU.2.2 Ident config**: 7 of 8 ident scans use
`DEFAULT_IDENT_CONFIG`. 1 uses `CSS_IDENT_CONFIG`. The CSS `ident`
regex `[a-zA-Z_\x80-\xff][\w\x80-\xff-]*` does NOT allow leading
dash in the first char class. `DEFAULT_IDENT_CONFIG` is correct
for most CSS idents. Only `selectorIdent` (which allows leading
dash via `(?:-?...)`) needs CSS config. Current routing is correct.

**AU.2.3 `-> f64` on CSS number rule**: NOT STARTED. 20
`scan_number_f64` calls in CSS currently discard their f64. Adding
the mapping would activate payload capture.

**AU.2.4 HexConvert codegen**: NOT STARTED. `parse_hex_color` is
never called in generated code — HexConvert mapping does not reach
the tape-first emitter path.

### Phase 3 — String decode + honest JSON bench

Status: **NOT STARTED**

Decode kernel exists in parse-that (committed in AT). Needs:
- Wiring through `scanner_plan.rs` as new SharedScanner variant
- `push_leaf_with_string` method in TapeBuilder
- `json_monolithic_value` bench with sonic-rs comparison

### Phase 4 — Accumulated debt elimination

Status: **PARTIAL**

Done:
- Test compile errors fixed (`vm.rs`, `runtime_root.rs`)
- Schema stub emitters deleted

Remaining:
- Bootstrap regen (generated.rs +770/-479 stale vs fresh regen)
- ParsedGrammar elimination (11 tranches deferred)
- Module-level `#[allow]` for generated.rs (280 per-item → 1)
- StructRegistry: populate or delete (dead scaffold)

### Phase 5 — Profile-driven optimization + bench parity

Status: **NOT STARTED**

Required:
- Fresh samply profiles for all 4 grammars (explain canada delta)
- Full bench suite run (parse + compile for JSON, CSS, Sheets, BBNF)
- post-AU.json with complete results

## Files in current state

- `crates/core/src/backend/driver/alt.rs` — updated with
  `DriverState` parameter, checks `CallStrategy::InlineBody`
- `crates/bbnf-tape/src/builder.rs` — payload stored in `child_off`;
  payload pre-alloc at `expected/8*8`
- `crates/bbnf-tape/src/tape.rs` — readers updated for `child_off`
- `crates/core/tests/fixtures/tape_golden/json/canada.json` and
  `data_xl.json` — regenerated with new record counts
- `crates/core/benches/json/value.rs` — UNTRACKED (from prior AT
  agent, needs Cargo.toml registration)

## Open worktrees

Multiple worktree branches exist from prior audit work. The branch
I accidentally committed to (`worktree-agent-a27879e1`) has one
commit (`b825675`, "perf(tape): pre-allocate payload buffer at 25%
of expected records") that is superseded by commit `6724a46` on
master (which uses 12.5%). The worktree branch can be cleaned up
without loss.

## Next session priorities

Given the partial completion and my noted mistakes:

1. **Verify the canada regression cause via samply profiling** —
   do not continue optimizing until the actual cost source is
   known. Deploying an agent with clear profiling directives.

2. **Parallelize Phases 2, 3, 4 aggressively** — each phase has
   independent file bounds:
   - AU.2: grammar files + `scanner_plan.rs`
   - AU.3: parse-that kernels + new bench + TapeBuilder extension
   - AU.4: `host.rs`, `types.rs`, `generated.rs`, struct_registry

3. **Complete AU.5 bench parity with post-AU.json** — the tranche
   contract requires full bench results for all 4 grammars in both
   compile and parse.
