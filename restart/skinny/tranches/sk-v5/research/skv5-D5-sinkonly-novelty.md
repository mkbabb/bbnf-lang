# SK-V5 D5 — Generated SinkOnly from BIR DirectBuild: Novelty Audit

Date: 2026-05-13
Question: Is "Generated SinkOnly from BIR DirectBuild" genuinely new SK-V5
work, or extant in some form? Per-claim verdicts grounded in tree state.

---

## §1. Per-Claim Verdict

| Claim | Verdict |
|---|---|
| "Generated runtime has no SinkOnly entry-point" (B2) | **VERIFIED-TRUE** |
| "BIR DirectBuild exists but is not lowered to direct field writes" | **VERIFIED-TRUE — EXTANT-PARTIAL** (BIR variant present at `ir/src/lib.rs:368-370`; no lowering anywhere) |
| "Track 1 == Track 2 == sink_only_digest in bench-private code" (B2) | **VERIFIED-TRUE** at `bbnf-bench/src/direct_struct.rs:150-156` |
| "SK-V4 Wave 1 commits to generated SinkOnly" | **VERIFIED but GOAL-LEVEL ONLY** — owner paths declared, no concrete spec; one owner path (`codegen/src/lower/`) does not exist on disk |

Composite verdict for the SK-V5 diagnosis:
**EXTANT-PARTIAL on the IR side; NEW on every implementation side.**
- The BIR enum variant exists (skeleton).
- The codegen lowering, the runtime entry-point, the LayoutFacts predicate
  (`backend_shape`), and the bench re-wiring are **all unimplemented**.
- SK-V4 Wave 1 declares the goal but provides no spec, no module skeleton,
  and no concrete typedef for `parse_direct_digest`/`SinkDigest`.

So the SK-V5 A4/B2 reports are not redundant: they identify a specific
multi-locus implementation gap behind a single declared goal.

---

## §2. BIR `DirectBuild` Variant Audit

**Present, structurally minimal, not consumed by codegen.**

Definition — `skinny/crates/ir/src/lib.rs:368-370`:

```rust
DirectBuild {
    shape: String,
},
```

The variant carries a `String` shape label (e.g. `"JsonObject"`, `"JsonPair"`)
and nothing else: no field roster, no slot map, no type info.

Construction — `skinny/crates/passes/src/lib.rs:416-419` (in
`materialize_rule`):

```rust
BackendExpr::TapeEmit { kind },
BackendExpr::DirectBuild {
    shape: shape.to_string(),
},
BackendExpr::Return,
```

Materialization happens for the 7 named JSON rules at
`passes/src/lib.rs:423-434` (`object → JsonObject`, `array → JsonArray`,
`pair → JsonPair`, `string → JsonString`, …). It is emitted unconditionally
alongside `TapeEmit` — i.e. there is no `backend_shape` switch.

Consumption: a test at `passes/src/lib.rs:489-509` asserts the variant
appears in the BIR. **Nothing else reads it.** `rg "DirectBuild"` returns
exactly 3 sites, all in `ir/` + `passes/`.

In particular: codegen does not pattern-match on `DirectBuild`. See §3.

---

## §3. Codegen Direct-Field-Write Emission Audit

**No path exists.** Codegen is currently a static template emitter.

`skinny/crates/codegen/src/lib.rs:60-76`:

```rust
pub fn emit_json(backend: &BackendIr) -> Result<EmittedSource, CodegenError> {
    let mut files = BTreeMap::new();
    files.insert("generated.rs".to_string(), generated_rs(backend));
    …
}
```

The two functions that nominally consume the BIR (`generated_rs`,
`parser_rs`) **explicitly discard it**:

`codegen/src/lib.rs:110-118`:

```rust
fn parser_rs(backend: &BackendIr) -> String {
    let _ = backend;
    include_str!("json_templates/parser.rs").to_string()
}

fn generated_rs(backend: &BackendIr) -> String {
    let _ = backend;
    include_str!("json_templates/generated.rs").to_string()
}
```

So even the existing tape-style code is not codegen-derived; it is a hand-
maintained template file with the BIR parameter ignored.

Tree-level negatives:

- `skinny/crates/codegen/src/lower/` — **does not exist**
  (`ls: No such file or directory`); SK-V4 Wave 1 owner path is aspirational.
- `rg "SinkOnly|sink_only|DirectBuild|emit_direct|direct_field|field_write"
  skinny/crates/codegen/` returns **nothing**.
- `rg "SinkOnly|sink_only|Sink::|SinkParser" skinny/crates/runtime/` returns
  **nothing**.
- No `parse_sink`, no `parse_direct_digest`, no `SinkDigest` trait, no
  `parse_into<T>` API anywhere in `skinny/crates/runtime/`.

So every emitter surface listed by SK-V4 Wave 1 is empty on disk.

---

## §4. Bench-Private `SinkParser` Actual Code Path

`skinny/crates/bbnf-bench/src/direct_struct.rs` exists, 610 LOC,
hand-written and entirely outside the substrate.

API exposed (lines 138-188):

```rust
pub fn track1_view_walk_digest(input: &str) -> Result<JsonDirectDigest, …> {
    let root = runtime::generated_json::parse(input)…   // substrate
    Ok(root_digest(&root))
}
pub fn track2_view_walk_digest(input: &str) -> Result<JsonDirectDigest, …> {
    let root = crate::track2::json::parse(input)…       // independent
    Ok(root_digest(&root))
}
pub fn track1_digest(input: &str) -> Result<JsonDirectDigest, …> {
    sink_only_digest(input)                              // BENCH-PRIVATE
}
pub fn track2_digest(input: &str) -> Result<JsonDirectDigest, …> {
    sink_only_digest(input)                              // SAME CALL
}
```

`sink_only_digest` definition (lines 190-202):

```rust
fn sink_only_digest(input: &str) -> Result<JsonDirectDigest, …> {
    let mut parser = SinkParser { bytes: input.as_bytes(), cursor: 0 };
    let digest = parser.value()?;
    parser.ws();
    if parser.cursor == parser.bytes.len() { Ok(digest) }
    else { Err(parser.error("trailing characters")) }
}
```

`SinkParser` struct (lines 204-207):

```rust
struct SinkParser<'a> {
    bytes: &'a [u8],
    cursor: usize,
}
```

This struct uses **only `&[u8]` + cursor**. It never constructs a `Tape`,
never invokes generated code, never touches `runtime::tape::`, never
references `BackendIr`. It is a wholly independent recursive-descent JSON
parser folded into the digest in one pass. Its `value`/`object`/`array`/
`string`/`number` methods (lines 209-353) hand-roll all primitives.

**Confirms B2's central claim**: `track1_digest` and `track2_digest` are
literally the same function call (`sink_only_digest(input)`), so the
"two-track" parity test for the gated `_digest` path is a tautology. Only
`track1_view_walk_digest` / `track2_view_walk_digest` (the substrate-using
walks) exercise distinct code paths.

---

## §5. SK-V4 Wave 1 Commitment Level

Source: `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md`
§3 (lines 96-128).

**Goal-declared, not specced.** What is named:

- Owner paths (5): `passes/`, `codegen/src/lower/`, `codegen/src/json_templates/`,
  `runtime/src/grammars/json/`, `bbnf-bench/src/direct_struct.rs`.
- 5 numbered implementation bullets (113-121) — each is a goal sentence:
  - "Add runtime grammar API entrypoints `parse_direct_digest` …"
  - "Lower existing BIR `DirectBuild` to direct field writes when
    `LayoutFacts.backend_shape == SinkOnly`."
  - "Delete or demote any private bench parser from the Track 1 path."
- Exit gate (123-128): correctness + at least one row clearing the slack
  OR profile-named blocker.

**What is missing** — verified by inspecting the named files:

| SK-V4 commitment | Tree state |
|---|---|
| `codegen/src/lower/` | Does not exist. |
| `LayoutFacts.backend_shape` | Field absent. `LayoutFacts` at `passes/src/lib.rs:46-51` has `rule_types`, `node_types`, `layout_policies` (empty), `hot_call_graph` — no shape enum. |
| `BackendShape` enum | Not defined anywhere in `skinny/crates/`. (Referenced only in `restart/ARCHITECTURE.md` prose.) |
| `parse_direct_digest` API | Absent from runtime. |
| `SinkDigest` / `parse_into<T>` trait | Absent. |
| `@sink` / `@direct` grammar directives | `skinny/grammars/json.bbnf` (18 lines) carries no annotations; rule-name lookup table in `passes/src/lib.rs:423-434` is the only "shape" signal. |

So Wave 1 is a goal block, not a design. SK-V5 A4/B2's diagnostic value is
to surface the gap between the named owner paths and the actual tree.

---

## §6. Final Novelty Verdict + Remediation Pattern

### Verdict

The diagnosis "Generated SinkOnly from BIR DirectBuild" is:

- **NEW** as an implementation task in every locus that matters
  (codegen lowering, runtime entry-point, LayoutFacts shape predicate,
  grammar directive, bench re-wire).
- **EXTANT-PARTIAL** only at the BIR-enum-variant declaration level
  (`ir/src/lib.rs:368-370` + emission in `passes/src/lib.rs:416-419`).
- **EXTANT-PLANNED at goal level** by SK-V4 Wave 1, but that plan declares
  intent without a spec; one owner path it names does not exist on disk.

The SK-V5 A4/B2 reports are **not redundant restatements** of SK-V4 — they
add the gap inventory (`codegen/src/lower/` missing, `LayoutFacts.backend_shape`
missing, codegen ignoring `&BackendIr`, etc.) that SK-V4 Wave 1 elided.

### Remediation Pattern

The remediation is **extend codegen + add the missing wiring**, not "copy
bench code into codegen." Specifically:

1. **Stop ignoring `&BackendIr`** — `codegen/src/lib.rs:110-118` must
   dispatch on BIR nodes instead of `let _ = backend; include_str!(…)`.
   This is prerequisite #0; without it `DirectBuild` lowering has nowhere
   to land.

2. **Add `BackendShape` enum + `LayoutFacts.backend_shape` field** in
   `passes/src/lib.rs`. SK-V4 names the predicate; the field does not exist.

3. **Add a `lower/` module hierarchy** under `codegen/src/` (the SK-V4
   owner path `codegen/src/lower/` is empty/absent). Walk `BackendExpr`
   nodes; when a rule's `backend_shape == SinkOnly`, emit field-write
   statements into a typed slot instead of `TapeEmit` writes.

4. **Add a `SinkDigest` / `SinkBuilder` trait** in
   `runtime/src/grammars/json/` (or a shared location) + a generated
   `parse_sink<D: SinkDigest>(input) -> Result<D, Error>` entry-point.
   Bench `JsonDirectDigest` becomes one impl of that trait — its
   per-field arithmetic stays bench-side; the *parse loop* generates from
   codegen.

5. **Re-wire `track1_digest` / `track2_digest`** in
   `bbnf-bench/src/direct_struct.rs:150-156` through the new generated
   `parse_sink::<JsonDirectDigest>`. **Delete the private `SinkParser`**
   at lines 204-353; keep `JsonDirectDigest` and its fold helpers; keep
   `track2_digest` distinct via an independent hand-coded `SinkDigest`
   impl (so the parity assertion remains non-trivial).

6. **Grammar directive (optional, for routing)** — either annotate the 7
   materialized rules in `skinny/grammars/json.bbnf` (currently devoid of
   `@sink`/`@direct`) or extend the existing rule-name table at
   `passes/src/lib.rs:423-434` with a `backend_shape` column. The
   directive route is closer to the SK-V4 intent; the table route is the
   shortest path.

**Copy-from-bench is the wrong shape**: the bench `SinkParser` has no
generality (hard-coded JSON, JSON-digest-only sink, no trait). It is a
proof-of-concept for the shape, not a substrate template. The codegen
emitter must be parameterised on rule shape, sink trait, and BIR walk —
none of which the bench code provides.

---

### Cited Negative Evidence

| Claim | Command | Result |
|---|---|---|
| No SinkOnly emitter in codegen | `rg "SinkOnly\|sink_only\|DirectBuild\|emit_direct\|direct_field\|field_write" skinny/crates/codegen/` | empty |
| No SinkOnly in runtime | `rg "SinkOnly\|sink_only\|Sink::\|SinkParser" skinny/crates/runtime/` | empty |
| No `lower/` directory | `ls skinny/crates/codegen/src/lower/` | "No such file or directory" |
| Codegen discards BIR | `codegen/src/lib.rs:111-117` | `let _ = backend;` twice |
| No `backend_shape` anywhere | `rg "backend_shape\|BackendShape" skinny/crates/` | no hits in `crates/`; matches only in `bbnf-simd/ext/x86/bbnf.asm` comments |
| BBNF grammar has no directives | `cat skinny/grammars/json.bbnf` | 18 lines, plain EBNF; no `@sink`/`@direct` |
| `DirectBuild` carries only `shape: String` | `ir/src/lib.rs:368-370` | confirmed |
| Only 3 references to `DirectBuild` repo-wide | `rg "DirectBuild" skinny/crates/` | 3 hits (ir/, passes/lib.rs:416, passes/lib.rs:497) |
| Git history names the goal | `git log --grep DirectBuild\|SinkOnly` | one commit: `1519cf16 docs(restart/skinny): SK-V4 redress … generated SinkOnly mandate` — docs only, no code commit ever |

### Files of Record

- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/ir/src/lib.rs:368-370`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/passes/src/lib.rs:416-419,423-434,46-51`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/lib.rs:60-76,110-118`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/direct_struct.rs:138-202,204-353`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/grammars/json.bbnf` (18 LOC)
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md:96-128`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/audit/SK-V5-COHORT/skv5-A4-tape-union-audit.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/audit/SK-V5-COHORT/skv5-B2-direct-attribution.md`
