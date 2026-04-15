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

## Session 2 recap (2026-04-15)

### Orchestrator hardening before profiling

Three friction points between running, benching, and profiling
blocked the wave-1 pattern from scaling past one entry per bench.
All three resolved before any subagent was dispatched:

1. `scripts/prepare-profile-wave.sh` rewritten to enumerate every
   (bench, entry) pair — five benches × their entries = 27 rows in
   `.profiles/samply/prebuild/wave.tsv`, one port pair per bench
   (reused sequentially by a single subagent across its entries).
2. `scripts/profile-bench-headless.sh` swapped its three `rg`
   calls to `grep -E`. The ripgrep binary was only a zsh alias on
   this machine, not a `$PATH` binary, so bash-invoked scripts
   found nothing and the record/load wait loops silently timed
   out even when every artifact was on disk. Smoke test on
   `google_sheets_monolithic/parse_simple` proved the fix: all
   seven required artifacts land cleanly.
3. Substring-filter contamination flagged but not fixed. Bencher
   0.1.5's `--bench <name>` filter is `name.contains(filter)`;
   running `--bench data` executes both `data` and `data_xl`, so
   the `data`/`bbnf_data`/`sonic_data` profile dumps contain
   mixed samples (dominated by the `_xl` run because it is ~1000×
   larger). `bench.txt` lines are still entry-matched and the
   throughput figures for those entries remain valid; the profile
   percentages for those three entries specifically should be
   read as `_xl` attribution. Fix options for a future wave:
   rename the small variants (`data` → `data_s`) or switch to
   criterion. Out of scope for this session.

### Wave-2 matrix and findings

Full per-bench detail lives alongside this document in
`profiling-2.md`. Headline:

| bench / side | entries | MB/s range |
|--------------|--------:|-----------:|
| json_monolithic | 5 | 1219 – 2690 |
| css_l4 | 3 |  578 – 1044 |
| google_sheets_monolithic | 3 |   93 –  124 |
| bbnf_monolithic | 6 |  217 –  631 |
| json_value (bbnf side) | 5 | 1230 – 2646 |
| json_value (sonic side) | 5 | 1508 – 3097 |

bbnf is at 77–85% of sonic across all five JSON datasets, but the
comparison is apples-to-oranges until Phase 3 lands: bbnf's side
never decodes strings, never materialises a value tree, and drops
the `Parsed` handle without a `.view()` call. Post-decode ratios
are estimated at 0.60–0.85 depending on string density (see
`profiling-2.md` for the per-dataset table).

### Verified cross-bench claims

- JSON scalar projection is firing (`push_leaf_with_{f64,bool,u8}`
  at expand.rs:2638/2651/2664) — confirmed by independent `grep`
  over `.profiles/samply/prebuild/expand/json_monolithic/expand.rs`.
- JSON string decode is not firing — `scan_quoted_string_strict`
  at expand.rs:2508 still falls through `.map(|_| ())` to a plain
  `push_leaf`. `decode_json_string_to_arena` exists in
  `parse-that/.../decode.rs:35` but has zero expand call sites.
  `push_leaf_with_string` does not exist on `TapeBuilder`; the
  closest existing methods are `push_leaf_with_Span` (packed
  (u32, u32)) and `push_leaf_with_aggregate` (≤ 16 bytes).
- CSS `parse_hex_color` is declared at expand.rs:61 but has zero
  call sites; the `push_leaf_with_u32` at expand.rs:103160 is
  inside `__namedColor` (function starts at 97235), not the hex
  rule. Named-color keyword-to-u32 projection is the only u32
  leaf path currently active.
- CSS number discards: 20 `scan_number_f64(...).map(|_| ())`
  sites verified; `-> f64` on the `number` rule in
  `grammar/css/l4/value-unit.bbnf` is still the trivial
  activation.
- Sheets is 100% compound (`push_compound = 37`, `push_leaf = 0`,
  `push_leaf_with = 0`) — confirmed against
  `.profiles/samply/prebuild/expand/google_sheets_monolithic/expand.rs`.
  Precedence-tower self-time is 56–86% of leaf samples across
  the three formula packs.
- BBNF generated parser fingerprint (push_compound = 90, push_leaf
  = 15, push_leaf_with = 0) matches pre-AU audit exactly. Hot
  rules resolve to `__mapped_factor` (15181), `__rhs` (15896),
  `__directive` (17569), `__big_comment` (14143), `__binary_factor`
  (15407) in `crates/core/src/grammar/generated.rs`.

### Corrected claims from subagent reports

- Fractional-digit SIMD *is* firing. The parse-that
  `scan_number_mantissa` at `number.rs:93` uses SWAR for the
  integer part by design (the comment at lines 128–131 explains:
  short integer runs dominate, 16-byte SIMD loads cost more than
  scalar SWAR on typical inputs) but calls
  `number_simd::scan_digits_simd(bytes, i)` at line 193 for the
  fractional part. Canada's 11.5% `compute_f64` leaf share is
  the Eisel–Lemire bridge, not missing SIMD.
- Byte-size claims in the BBNF agent report ("__directive 18,744 B",
  "__mapped_factor 8,036 B") do not match line counts (142 and
  130 lines respectively). The percentage-of-leaf-samples claims
  were independently verified and are accurate; the byte-size
  figures appear to conflate a different metric and were dropped
  from the integrated findings.

### What Phase 5 now has to work with

Fresh samply profiles for all 27 entries (five per JSON monolithic
dataset, three per CSS stylesheet, three per Sheets formula pack,
six per BBNF grammar file, ten per json_value parity entry). Every
profile has bench.txt, build.txt, record.txt, load.txt,
profile.json.gz, profile.json.syms.json, and syms-proof.txt. Every
entry's `profile.json.gz` symbols were verified against
`syms-proof.txt` for named-frame coverage.

### Architectural correction — the inference pipeline is already
### complete; codegen is the reason most typed ASTs don't materialise

Reading the CSS L4 grammar against the expand artifacts made it
plain that the grammar declares more typed structure than the
generated parser actually emits. `grammar/css/l4/color.bbnf`
defines the complete CSS Color Level 4 / 5 specification: 148
named colors each annotated `-> u32`, hex with
`-> parse_hex_color(input) : u32`, `colorFunction` covering rgb /
rgba / hsl / hsla / hwb / lab / lch / oklab / oklch, `colorFn` for
the nine-space `color()` notation, and `colorMix` for
`color-mix(in <space> …)`. `grammar/css/l4/value-unit.bbnf`
declares seven typed dimensions (`length`, `angle`, `time`,
`frequency`, `resolution`, `flex`, `percentage`) as
`Seq(number, unit)` with `unit -> u8` annotations matching
lightningcss's discriminants. The grammar is the full spec; the
materialised tape is a small subset of it.

The fingerprint against `css_l4` expand:
`push_leaf_with_u32 = 1` (namedColor's 148-branch keyword alt),
`push_leaf_with_u8 = 6` (a handful of keyword enums in CSS
keyword tables), `push_leaf_with_f64 = 0`, 20
`scan_number_f64(...).map(|_| ())` discards, `parse_hex_color`
declared at expand.rs:61 and called zero times. Every other `->`
annotation in the grammar is read by inference but never reaches
the tape emitter.

This is a codegen gap, not a grammar gap. The creed is clear:
no legacy, no fallbacks, no workarounds. Every `->` annotation
in the grammar must reach the tape. Inference composes types;
it never loses them. The AU.md Phase 2 rewrite makes the invariant
explicit and adds AU.2.5 (typed dimensions as `(f64, u8)`
aggregates) and AU.2.6 (typed color functions with
`(space: u8, c1..c3: f64, alpha: f64)` aggregates, arena-backed
for color-mix recursion). AU.6.7 collapses the existing per-type
payload side-car Vecs into a single arena-backed `Vec<u8>` so
aggregate payloads of any size live in one contiguous buffer.
AU.6.8 extends the same "every `->` reaches the tape" audit to
JSON, BBNF, and Sheets — no grammar may silently drop typed
information it declares.

The practical consequence for this tranche: AU.2 is no longer
"CSS scanner activation and payload retention" — it is "CSS typed-
AST parity with lightningcss". Parity means every dimension
carries its `(f64, u8)`, every color format resolves to its typed
value, every comparison against lightningcss uses the full typed
output. Scanner-only fast modes, if ever built, live behind a
grammar-level `@scan` directive — not as a silent codegen drop-off.

## Session 3 (2026-04-15) — wave orchestration begins

Seven-wave schedule declared in AU.md is authoritative; task tracking
mirrors it.

### W1 — Grammar annotation audit — IN PROGRESS

Four worktree-isolated agents dispatched in parallel, one per grammar
family. Each agent owns its grammar files exclusively (no shared
writes). Bootstrap idempotency is each agent's own hard gate before
commit.

| Agent | Worktree | Files owned | Scope |
|-------|----------|-------------|-------|
| (a) CSS L4 | `../bbnf-wt-au-w1-css` | `grammar/css/l4/*.bbnf` (15) | AU.2.0 full audit |
| (b) JSON string | `../bbnf-wt-au-w1-json` | `grammar/json/json.bbnf` | AU.3.1 grammar side |
| (c) BBNF tokens | `../bbnf-wt-au-w1-bbnf` | `grammar/bbnf/*.bbnf` (3) | AU.6.4 / AU.6.8 tokens |
| (d) Sheets | `../bbnf-wt-au-w1-sheets` | `grammar/google-sheets/*.bbnf` | AU.6.8 literals+refs |

Invariants honoured: master clean before dispatch, no shared writes,
each worktree on detached HEAD, no `/tmp` paths. Commits land inside
each worktree; orchestrator cherry-picks accepted commits onto master
at wave boundary.

### W1 — LANDED (19 cherry-picks + bootstrap regen)

Wave boundary integration on master:

| Family | Commits cherry-picked | Net diff | Bootstrap ok? | Idempotent? |
|--------|-----------------------|----------|---------------|-------------|
| CSS L4 | 13 | +252 / -102 across 12 files | yes | yes |
| JSON | 1 | +2 / -1 in `json.bbnf` | yes | yes |
| Sheets | 3 | +84 / -17 in `google-sheets.bbnf` | yes | yes |
| BBNF | 1 (`4bd5a05` only; agent's `e93f317` regen skipped in favour of a clean master regen) | +9 / -9 across `bbnf.bbnf` + `expressions.bbnf` | yes | yes |

Master-level regen commit `6e33a40` lands the fresh `generated.rs`
(24921 lines, down from 24979 under the CSS-heavy agent regen — the
master regen is the authoritative compiled form). `CSS_L4_RULE_COUNT`
frozen snapshot advanced 185→190 in the same commit to match the
intentional grammar edits. Every other rule-count snapshot unchanged.

**Post-W1 test gate.** `cargo test -p bbnf --test grammar_roundtrip` —
6 / 6 pass (all five grammar families including the new annotations).
`cargo test -p bbnf --test payload_layouts` — 13 / 13 pass.
`cargo check --workspace` — zero errors, zero warnings.
`cargo test --workspace` — two failures remain (`parse_debug_wildcard`,
`pipeline_debug_wildcard_sets_all`), both pre-W1 and both in the
Session 1 "pre-existing 18" list. No new failures.

**W2 input from W1 (codegen gaps to close).** BBNF agent surfaced two
latent bugs and one whitelist gap that W2 must reconcile before the
remaining BBNF Alt-of-constant-literal rules (`bool_lit`, `modifier`,
`binary_operators`, `mul_op`, `add_op`, `cmp_op`) can annotate:

1. `TypeDesc::from_scalar_name` and `is_type_name` do not admit
   `"Span"` — `-> Span` shorthand reaches IR as `TypeDesc::Named("Span")`,
   not `TypeDesc::Span`. Either admit `"Span"` to both whitelists OR
   teach the Rust emitter to treat `Named("Span")` as `TypeDesc::Span`
   for leaf-payload routing.
2. `aggregate_constant_setter` in `emitter/map_value.rs` emits
   `__aggregate_buf[…].copy_from_slice(…); __has_payload = true;`
   inside rules whose prelude is `emit_alt_span_only_prelude_epilogue`
   (which declares `__payload_<T>` but NOT `__aggregate_buf` or
   `__has_payload`). Non-inlined Alt-bodied rules fail cargo expand
   with `E0425: cannot find value __aggregate_buf`. Inlined rules
   (CSS `timeUnit`, `angleUnit`, …) escape because they emit into
   their caller's prelude. Fix: gate `ctx.payload_layout` off for
   Alt-bodied non-inlined rules, or extend the Alt prelude to reserve
   aggregate locals.
3. `->` binds to a single factor; Seq / Alt rule bodies must be
   wrapped in `(...)` to carry a rule-level annotation. Documented for
   future grammar authors; not a bug, but a surface-syntax rough edge.

CSS agent surfaced three codegen gaps that W2's typed emitter routing
must absorb:
- Named aggregate structs (`colorFunction`/`colorFn`) cannot be
  expressed in `type_annotation` — `(u8, f64, f64, f64, f64)` needs
  either inference composition from leaf-annotated sub-rules or a
  host-function convert path (`-> parse_color(input) : Color`).
- Recursive arena-backed types (`ColorRef` in `colorMix`, recursive
  `mathExpr` in calc) require `TypeDesc::ArenaOffset` or equivalent.
- Variable-length lists (`colorStopList`, `linearEasing`) have no
  surface syntax.

Sheets agent surfaced the same aggregate-type gap for `cell_ref` —
`(row: u32, col: u32, abs_row: bool, abs_col: bool)` requires either a
named-tuple annotation syntax extension OR a host-function convert
(`-> decode_cell_ref(input) : CellRef`).

All three agents' TODO pointers anchor to AU.2.5 / AU.2.6 / AU.6.7,
which are W2 and W5 scope respectively. Nothing is deferred past AU.

### W2 — LANDED (13 cherry-picks + parse-that fast-forward + regen)

Wave boundary integration on master. Three parallel worktree-isolated
agents completed. Each owned a disjoint emitter region per the plan's
file bounds; cross-agent communication handled via careful commit-
message hand-offs.

| Agent | Commits | Focus | Hard gates |
|-------|---------|-------|------------|
| B (typed CSS emitter) | 7 | Span whitelist, Alt prelude / aggregate-buf reconciliation (Option A), HexConvert → `push_leaf_with_aggregate` + `TapeKind::KvPair`, Seq-leaf classification, hex colour round-trip tests | AU.2.4 hard gate met (`parse_hex_color` emits twice in expand, `push_leaf_with_aggregate` fires at 20 sites); AU.2.5 partial (4 of 20 dimension sites activate; 16 blocked on Ref-projection); AU.2.6 minimum gate met (hex colour round-trip) |
| C (SIMD bitmap v2) | 3 worktree + 3 parse-that | Grammar-parameterised `structural_alphabet` IR pass, `emit_structural_bitmap_kernel` subsuming `memchr1/2/3` + `nibble_lut`, SIMD `filter_quote_parity` via clmul / pmull, `scan_ws_block_comments_slow` deleted | AU.2.7 architectural hard gate met (0 matches for `scan_ws_block_comments_slow\|memchr1\|memchr2\|memchr3\|nibble_lut`); the 650 MB/s CSS bootstrap perf hard gate is **NOT** met (current 427 MB/s; investigation in W3) |
| D (JSON string decode) | 3 worktree + 0 parse-that | `push_leaf_with_string` with per-parse arena + `payload_idx=2` sentinel, `SharedScanner::JsonStringDecode`, JSON value bench walks the tape, `data` → `data_s` rename (AU.6.6) | AU.3.1 hard gate met (`decode_json_string_to_arena` fires 3× in expand); **AU.3.2 bbnf/sonic ratio hard gate is NOT met** (current 0.27–0.41 vs goal ≥ 0.60 twitter / ≥ 0.80 canada — walker allocates `Vec` per `cursor.children()`, structural fix scoped into W3) |

Parse-that fast-forwarded master to `f624ba6` (89 commits ahead of
origin). Three SIMD commits fold in on top of the AR/AS/AT path-
dependency work.

`generated.rs` regens stably in one pass. Confirmed idempotent (diff
= 0 across three consecutive clean-cache regens). Test suite: 6/6
grammar_roundtrip, 13/13 payload_layouts, 5/5 json_decode, 18/18
css_l4 target tests. Workspace: 2 pre-existing debug-wildcard
failures carry over; no new regressions.

### W2 — follow-up work that rolls into W3

1. **Tailwind CSS parse failure at offset 111798** — Introduced by
   W1 grammar annotations (confirmed by W2.C agent via temporary
   revert). `cargo bench -p bbnf --bench css_l4` panics on tailwind;
   normalize and bootstrap complete. Root cause investigation is
   W3-scoped: one of the 13 CSS grammar annotation commits
   `623161b…6843654` widened or narrowed a rule in a way that rejects
   the `@keyframes` at-rule following `}\n\n` at that offset.
2. **CSS bootstrap perf regression** — 644 MB/s (Session 2) →
   427 MB/s (post-W2). Architectural hard gate for AU.2.7 was
   650 MB/s. Three candidate causes:
   - Label shadowing warnings from `derive(Parser)` (`'next_blk`,
     `'skip_blk` shadow) — indicates the Alt prelude / Seq fix
     landed with a gensym hole; the emitted code still compiles but
     LLVM may not be optimising through duplicate-labelled blocks.
   - `emit_structural_bitmap_kernel` routes CSS bootstrap paths
     through a kernel that is slower than the deleted `nibble_lut`
     path for bootstrap's particular byte distribution.
   - `HexConvert` aggregate push adds arena work that is net-
     negative on bootstrap (very few hex colours, but the emitter
     now reserves the aggregate buf per `__hex` call).
3. **AU.3.2 ratio miss** — walker allocates `Vec` per
   `cursor.children()`; a zero-alloc child iterator (running offset
   cursor + `sib_skip`-style traversal) closes most of the gap.
   This is a W3 scope item; the AoS substrate can carry the fix
   without waiting on the AV columnar pivot.
4. **Ref-scalar-projection always projecting BoxedEnum** — named in
   W2.B agent's deliverable. `IrNode::Ref(_)` loses the target
   rule's scalar type at every Seq-with-Ref site, which is the
   direct cause of the 16 inactive dimension sites. Fix lives at
   `crates/ir/src/passes/types/generate.rs:148-149`. W3 scope.
5. **`valueUnit` unreachability** — the CSS L4 stylesheet entry
   does not reference `valueUnit`; `prune_unreachable` cascades,
   removing `length`, `angle`, etc. from the compiled graph. Either
   wire `valueUnit` into the reachable graph from `stylesheet.bbnf`
   or remove its definition. W3 scope (grammar-side).
6. **Factor-pass type loss in large Alts** — `absoluteLengthUnit`
   projects to `BoxedEnum` because `fuse_token_dispatch`'s prefix-
   factoring produces `Seq(Literal, Alt)` branches with
   `Tuple([Span, U8])` shape, heterogeneous with non-factored U8
   branches. W3/W6 scope.
