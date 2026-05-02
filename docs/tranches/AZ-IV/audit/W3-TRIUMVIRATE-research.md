# W3 Triumvirate Research

## Trigger

W3.1, W3.2, W3.3 landed cleanly but together produce a non-functional
lazy lane. `parse_with(input, &path)` constructs a `PathCursor`,
threads it into `PathExecutor::execute`, then calls `JsonParser::parse(src)`
eager and projects via `doc.get(path)`. The cursor is consulted by no
generated parse function; the static `PATH_PLAN` is searched by no
production caller; the lazy lane is `parse() + walk_path` with extra
indirection. Hard Gate 7 (`bbnf_get_twitter ≤ 5x sonic_get_twitter`),
§Invariants #9 (path skips unvisited subtrees), and W3.4's
lazy-error-elision contract test all blocked. The W3.3 commit body's
claim that "per-rule parse functions don't exist" is **false** — the
shape-dispatch family emits one `parse_<shape>_<grammar>_<rule>`
function per shape-classified rule (verified below).

## Codegen Survey

### Where parse functions live

The Rust backend emits real per-rule parse functions through the
shape-dispatch family. `crates/core/src/grammar/generated/json.rs`
exposes nine such functions for JSON alone:

- `parse_keyword_JsonParser_null` — json.rs:855
- `parse_keyword_JsonParser_bool` — json.rs:885
- `parse_number_JsonParser_number` — json.rs:941
- `parse_string_JsonParser_string` — json.rs:1069
- `parse_object_JsonParser_object` — json.rs:1167
- `parse_array_JsonParser_array` — json.rs:1236
- `parse_flat_JsonParser_pair` — json.rs:1329
- `parse_wrap_JsonParser_value` — json.rs:1418
- `parse_JsonParser_value` (root dispatcher) — json.rs:1709
- `parse_JsonParser_value__value` (value-position dispatcher) — json.rs:1723

Their signatures are uniform:

```rust
pub fn parse_<shape>_<Grammar>_<rule><'p>(
    input: &'p [u8],
    p: &mut usize,
    state: &mut __shape_support_<Grammar>::ScanState,
    builder: &mut <Grammar>StructBuilder<'p>,
) -> Result<(), DtaError>
```

Cross-shape recursion (e.g. `parse_array → parse_wrap_value → parse_object`)
is exactly the recursive-descent backbone the cursor must thread.

### Shape-dispatch family map

`crates/core/src/backend/rust/emitter/shapes/mod.rs:141 emit_shapes_for_grammar`
walks every IR rule, consults `ir.shape_assignments.get(rule.id)`, and
dispatches to one of 13 per-shape emitters (mod.rs:244). The dominant
emitters per AZ-IV W1 redress are:

- `flat::emit_parse_flat` → `shapes/flat/struct_direct.rs:377` —
  Seq(positions) bodies (CSS *Decl, JSON pair). Body is a per-position
  emission inside an IIFE wrapped by `begin_compound`/`end_compound`
  (struct_direct.rs:411).
- `wrap::emit_parse_wrap` → `shapes/wrap/struct_direct.rs:385` —
  Alt-of-Refs dispatch (JSON value, CSS color). Body is byte-dispatch +
  linear-try fallback. The branch-tag stamp (`push_branch_tag(idx)`,
  json.rs:1209-area) is precisely the SegmentKind::VariantName decision
  point.
- `alt_dispatch::emit_parse_alt_dispatch` → `shapes/alt_dispatch/branches.rs` —
  byte-class Alt branches (CSS, BBNF).
- `object` / `array` → loop bodies with cross-shape Ref calls
  (json.rs:1194 / json.rs:1264). The `Repeat` loop is precisely the
  SegmentKind::Index decision point.
- `dispatcher::emit_dispatcher` → `shapes/dispatcher/cross_shape.rs:47` —
  the top-level `parse_<grammar>_<root>` and root-Alt body.

The composition is recursive but **not branch-pruning** today: every
function unconditionally parses its full body and pushes records into
the builder.

### Cursor-threading seam candidates

The five injection points where cursor decisions can prune work, with
file:line evidence:

1. **`shapes/dispatcher/cross_shape.rs:160-167` (top-level dispatcher
   signature).** Add `cursor: &mut PathCursor<'p, P>` parameter; the
   per-grammar `parse_with` constructs the cursor and calls the
   dispatcher with it. Generated counterpart: `json.rs:1709`.

2. **`shapes/array/*.rs` and `shapes/flat/struct_direct.rs:411-418`
   (compound iterating loops).** Generated body at `json.rs:1264-1284`
   is the array element loop. Insert `cursor.decide(rule_id)` before
   each child call: `Decision::Skip` → break out of the loop early
   (and skip-scan to closing delimiter); `Decision::ParseUntil(idx)`
   → break after element index `idx`. The closing-delimiter scan is the
   piece W3 owes — emit a byte-level `scan_balanced_to(b']')` helper
   that advances `*p` without producing records. SegmentKind::Index +
   ParseUntil is the dominant lazy lane (most JSON paths step
   through arrays).

3. **`shapes/wrap/struct_direct.rs:340-341 emit_alt_struct_dispatch`
   (Alt branch dispatch).** Generated body at `json.rs:1429-1700` is the
   value-position byte-dispatch. Insert
   `cursor.decide(rule_id)` before the byte-dispatch match: when the
   path's current SegmentKind is VariantName, the cursor + plan select
   exactly one branch (`Decision::ParseUntil(idx)`) and other branches
   become unreachable — the emitter can drop the `'try_branches: loop`
   wrapper for a direct delegate to the indexed branch. SegmentKind::Field
   on an Alt-of-Refs (e.g. JSON `value`) routes through whichever target
   rule the path's `Field("a")` matches, decided by the next descent's
   path plan.

4. **`shapes/flat/struct_direct.rs:411-418` (Flat-shape positional
   loop).** Generated body at `json.rs:1329` (`parse_flat_JsonParser_pair`)
   is the Seq[key, value] body. Each position contributes one
   `(SegmentKind::Field, position)` plan row (per `path_plan.rs:179`).
   Insert `cursor.decide(rule_id)` before each `#emissions[i]`: when
   the cursor's current segment is `Field(key)` and the plan's
   `ParseUntil(0)` row says "stop after position 0", the loop emits
   only the key-recognition emission and breaks. The byte cursor `*p`
   continues to advance through skipped positions but no `builder.push_*`
   fires.

5. **`runtime/{json,css_l4,sheets,bbnf}/parse_with.rs` (entry point).**
   Today (parse_with.rs:74-90) calls `JsonParser::parse(src)` then
   `doc.get(path)`. Replace with a path-aware entry rule call:

   ```rust
   let mut state = ScanState::new();
   let mut builder = JsonStructBuilder::new();
   let mut pos = 0;
   let mut cursor = PathCursor::new(path, |rid, kind, _| {
       __path_plan::lookup(rid, kind).map(|e| e.decision)
           .unwrap_or(Decision::ParseFully)
   });
   parse_JsonParser_value(input.as_bytes(), &mut pos, &mut state,
                          &mut builder, &mut cursor)?;
   builder.finalise(input).get::<T>(legacy_path)
   ```

   The cursor flows through; the builder captures only the leaf the
   path reaches; `doc.get` projects the leaf trivially.

## Grammar-Generality Verification

Hard Gate 17 (no rule-name match arms in production) holds under the
proposed seam:

- The per-shape emitter modules already key on `ShapeTag` (a projected
  discriminator), `LayoutKind`, and `FieldSource` — not rule names.
- The cursor's decision is keyed on `(rule_id, SegmentKind)` — both
  grammar-general; `rule_id` is computed by the registry, `SegmentKind`
  is the cursor's intrinsic alphabet (`crates/core/src/path/cursor.rs:54-65`).
- The proposed signature change adds one parameter to every
  `parse_<shape>_<Grammar>_<rule>` function plus the dispatcher; no
  per-grammar branching is introduced.
- `path_plan::emit_path_plan` (path_plan.rs:247) already iterates
  `ir.struct_registry.iter()` without rule-name keying; the regen
  diff (`audit/W3-path-plan-regen-diff.txt:103-126`) verifies zero
  grammar-name string hits in the emitter.

The seam preserves grammar generality.

## PATH_PLAN Sufficiency

The W3.3 plan is **sufficient for ParseUntil + ParseFully** with one
caveat:

- `Decision::Skip` is in the alphabet (`cursor.rs:99-100`,
  `path_plan.rs:75-77`) but **never emitted** today
  (`path_plan.rs:rows_for_layout` produces only ParseFully and
  ParseUntil). Skip needs a byte-range scanner: a `Decision::Skip`
  return from `cursor.decide` must advance `*p` past the rule's bytes
  without record emission. Implementation: emit a per-shape
  `skip_<shape>_<Grammar>_<rule>` scanner stub that knows the open/close
  delimiter pair (objects: `{...}` brace-balanced; arrays: `[...]`
  bracket-balanced; strings: `"..."` quote-with-escape; scalars: regex-scan
  via the existing `__regex_scan_<Grammar>` adapter at json.rs:262).
  `simd-scan`'s `quoted_string_simd_body` already exists for strings;
  arrays/objects need a brace-balance scan. ~50 LOC per grammar.

- **Pratt precedence:** CSS L4 has Pratt rules where children are not
  byte-aligned (operator chains). `path_plan::rows_for_layout` does not
  walk Pratt bodies. For Pratt-shape rules the only valid plan row is
  `Wildcard → ParseFully`. This is correct (paths that cross Pratt
  bodies need full descent); the 1049 CSS_L4 plan rows confirm Pratt
  isn't getting per-child cuts. No additional row types needed; this
  restricts what lazy can do for CSS rather than blocking it.

- **`field_index` payload:** the plan row carries `field_index` (path_plan.rs:91-94).
  Today the cursor doesn't read it — `decide(rule_id)` returns just
  `Decision`, which already carries the cut index inline (`ParseUntil(u16)`).
  The `field_index` field is redundant under the current cursor surface;
  it can stay (debug aid) or be deleted as dead. Not a blocker.

The plan is sufficient. The carry is the codegen-side `Skip` handling
(byte-range scanners) which is naturally a W3.4 or post-W3 task.

## Sonic-RS Strategy Comparison

Sonic-RS's `get_from(json_str, path)` does ~291ns by **never building
an AST**: the path drives a byte-level scanner that finds the matching
key by string-comparing JSON bytes against the path segments
(`sonic_rs::serde::reader_helper::get_unchecked`). It scans `"<key>":`
and uses balanced-delimiter walks (`{...}`, `[...]`) to skip non-target
subtrees byte-wise. No tree, no records, no allocation.

Our approach materialises records in the builder along the path (the
builder still records every `begin_compound`/`end_compound` along the
descent). For `path = ["statuses", 0, "text"]` on twitter (2.4MB):
sonic-rs scans ~50 bytes of the outer object key, skips the value
brace-balanced, then descends. Our `parse_with` after carving will
still record `begin_compound(object)` → push key string → call
`parse_wrap` → `parse_array` → `begin_compound(array)` → first element →
descend → leaf — recording every compound along the path even when
sibling subtrees skip.

**≤ 5x sonic is feasible with carving alone.** The recorded path is
shallow (twitter `statuses[0].text` is depth 4 of compound records);
sibling skip eliminates the bulk of work (twitter has 100 statuses, we
parse 1; each status has ~30 fields, we need 1; each tweet body is
~200 bytes, sonic skips it brace-balanced and we'd skip-scan it the
same way). The skipped subtree's `__regex_scan_<Grammar>` adapter
already exists for strings; arrays/objects need a brace-balance helper.

**≤ 1.0x sonic is a different architecture**: a no-builder path that
matches sonic's "just byte-scan" approach. That is an `xtask`-scope
codegen rewrite (emit `scan_to_<path>` standalones per grammar, no
records) — out of W3 scope per AZ-IV §Hard Gates 16's "1.0x routes only
with profile evidence" gate.

## Risk/Cost Matrix

| Component | LOC change | Regen impact | Test impact | Agents |
|---|---:|---|---|---:|
| Add `cursor` param to per-shape emitters (12 emitters × ~20 LOC each) | ~240 | regen 9 grammars; +1 param per `parse_<shape>_*` fn | 1582 tests still call `JsonParser::parse(input)` (the eager entry) — unchanged. New `parse_with` tests pass after seam wires | 1 |
| Add `cursor.decide(rule_id)` consult inside Object/Array/Flat/Wrap bodies | ~150 in 4 emitters | inline `match cursor.decide(rule_id) { ... }` per loop/dispatch | parse_with negative-fixture (W3.4) starts passing | 1 |
| Implement `skip_<shape>_<Grammar>_<rule>` byte-range scanners | ~200 (4 shapes × 50 each) | new fns per grammar; consumes `__regex_scan_*` for scalars | new test `parse_with_skip_scanner_*` per grammar | 1 |
| Rewrite `runtime/*/parse_with.rs` to call dispatcher with cursor | ~80 (4 grammars × 20) | none — the entry point is hand-written | parity-against-eager smoke tests in parse_with.rs continue to pass | 1 (folds into entry-rewrite agent) |
| Bench harness extension `bbnf_parse_with_*` rows | ~50 (already in W3.5 scope) | none | bench compile + run | (W3.5 owns) |

Total ~670 LOC added/modified across emitter + runtime; no test
regressions for the 1582-test corpus (eager entry is untouched). Regen
output diff per grammar is mechanical — every `parse_<shape>_*` fn
gains one parameter; every cross-shape call site gets `&mut cursor`
threaded.

## Recommended Path Forward

**Two redress agents in parallel** with disjoint write paths and a
synthesis commit at integration:

- **Agent A — emitter carve + regen (HARD CAP 30 min).** Owns
  `crates/core/src/backend/rust/emitter/shapes/**` + the regen output
  at `crates/core/src/grammar/generated/**`. Adds the cursor parameter
  to every per-shape `parse_<shape>_<Grammar>_<rule>` emission site
  and the `cursor.decide(rule_id)` consult at the four hot decision
  points (Array loop, Object loop, Wrap Alt-dispatch, Flat positional
  loop). Implements per-shape `skip_*` byte-range scanners. Runs
  `cargo xtask regen --check` and ensures the eager 1582-test corpus
  stays green.

- **Agent B — parse_with entry rewrite (HARD CAP 30 min).** Owns
  `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf}/parse_with.rs`.
  Replaces the `JsonParser::parse + doc.get` body with the
  cursor-threaded dispatcher call. Lands the W3.4 negative-fixture
  test for the lazy-error-elision contract. Depends on Agent A's
  regen output — picks up Agent A's branch via cherry-pick or merge
  after Agent A commits.

Sequencing: Agent A lands first; Agent B opens its worktree from
Agent A's tip; both report under W3 close discipline; bench harness
W3.5 picks up after both land. Triumvirate redress completes at
sub-agent commit hash recorded in the parent W3 progress ledger.
