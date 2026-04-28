---
audit: AZ-I.AUDIT.2
lens: substrate-vs-consumer
worktree: bbnf-wt-az-i-audit-substrate
opens-against: master HEAD `0321c53a`
session-window: `aed24de0..HEAD` (AZ-I W0+W1+W2-substrate)
---

# AZ-I Substrate-Consumer Audit

## 1. Substrate inventory

Grouped by wave; file:line per item. API-surface-unit granularity;
accessor methods grouped onto host type to stay under the 100-halt.

### W0 audit pass — `crates/ir/src/passes/audit/`

| # | Item | Site |
|---|---|---|
| 1 | `enum GrammarAuditTag` (+`key()`) | `payload_coverage.rs:68,77` |
| 2 | `enum MarkerStatus` | `payload_coverage.rs:91` |
| 3 | `struct PendingMarker` | `payload_coverage.rs:106` |
| 4 | `struct GrammarCoverage` (+`ratio`,`is_clean`) | `payload_coverage.rs:129,154,164` |
| 5 | `struct AuditCoverageReport` (+`new`,`push`,`get`,`is_clean`) | `payload_coverage.rs:171-209` |
| 6 | `trait StructRegistryProbe` | `payload_coverage.rs:243` |
| 7 | `struct AbsentRegistryProbe` | `payload_coverage.rs:292` |
| 8 | `struct PayloadLayoutsProbe<'ir>` (+`new`) | `payload_coverage.rs:305,310` |
| 9 | `fn audit_payload_coverage<P>` | `payload_coverage.rs:394` |
| 10 | `fn is_typed_arrow_fn` | `payload_coverage.rs:479` |
| 11 | `fn write_coverage_report` | `payload_coverage.rs:573` |
| 12 | `struct MissingMarker` | `audit/mod.rs:21` (re-export) |

Re-export: `crates/ir/src/passes/mod.rs:33-35`.

### W1 StructRegistry — `crates/ir/src/registry/` + `passes/types/`

| # | Item | Site |
|---|---|---|
| 13 | `enum LayoutKind` | `registry/struct.rs:58` |
| 14 | `enum FieldSource` | `registry/struct.rs:84` |
| 15 | `struct StructField` (+8 accessors) | `registry/struct.rs:122,141-191` |
| 16 | `struct StructLayout` (+12 accessors) | `registry/struct.rs:202,228-304` |
| 17 | `struct StructRegistry` (+10 methods) | `registry/struct.rs:313,320-371` |
| 18 | `fn populate_struct_registry` | `passes/types/registry.rs:64` |
| 19 | `GrammarIR.struct_registry` field | `types/grammar.rs:392` |

Re-export: `ir/src/lib.rs:25`, `passes/mod.rs:79`.

### W1.B4 registry observer — `crates/core/src/backend/rust/emitter/shapes/registry_observer.rs`

| # | Item | Site |
|---|---|---|
| 20 | `struct RegistryReadEvent` | `:47` |
| 21 | `fn record` | `:65` |
| 22 | `fn drain` | `:75` |
| 23 | `fn clear` | `:82` |

### W2 struct-direct cohort — `crates/core/src/runtime/` + `backend/rust/emitter/`

| # | Item | Site |
|---|---|---|
| 24 | `trait StructBuilder` | `runtime/builder.rs:66` |
| 25 | `enum JsonValue<'p>` | `runtime/json/value.rs:34` |
| 26 | `enum JsonNumber` (+`as_f64`) | `runtime/json/value.rs:64,80` |
| 27 | `struct JsonPair<'p>` | `runtime/json/value.rs:95` |
| 28 | `struct JsonArray<'p>` | `runtime/json/value.rs:104` |
| 29 | `struct JsonObject<'p>` | `runtime/json/value.rs:111` |
| 30 | `struct JsonDocument<'p>` (+3 methods) | `runtime/json/value.rs:124,137-151` |
| 31 | `struct JsonArrayId` (+`EMPTY` const, `is_empty`) | `runtime/json/arena.rs:34,39,43` |
| 32 | `struct JsonObjectId` (+`EMPTY` const, `is_empty`) | `runtime/json/arena.rs:64,68,72` |
| 33 | `struct JsonArena<'p>` (+8 methods) | `runtime/json/arena.rs:95,107-176` |
| 34 | `struct JsonStructBuilder<'p>` (+3 methods) | `runtime/json/builder.rs:95,121-149` |
| 35 | `enum EmitStrategy` (+3 methods) | `emitter/strategy.rs:57,110,154,161` |
| 36 | `fn resolve_emit_strategy` | `pipeline/compile.rs:181` |
| 37 | `fn emit_parse_object_struct_direct` | `shapes/object.rs:441` |
| 38 | `fn emit_parse_array_struct_direct` | `shapes/array/mod.rs:154` |
| 39 | `fn emit_parse_alt_dispatch_struct_direct` | `shapes/alt_dispatch/mod.rs:170` |
| 40 | `fn emit_parse_number_struct_direct` | `shapes/number.rs:219` |
| 41 | `fn emit_dispatch_arms_struct_direct` | `shapes/alt_dispatch/branches.rs:29` |
| 42 | `fn emit_parse_keyword_struct_direct` | `shapes/keyword/struct_direct.rs:88` |
| 43 | `fn emit_parse_wrap_struct_direct` | `shapes/wrap/struct_direct.rs:172` |
| 44 | `fn emit_parse_flat_struct_direct` | `shapes/flat/struct_direct.rs:134` |

Re-exports: `runtime/mod.rs:27,30-33` (11 JSON types), `runtime/json/mod.rs:44`,
`emitter/shapes/mod.rs:101-104` (observer).

**Total: 44 substrate units** (under 100-halt).

## 2. Consumer audit per substrate item

Counts via grep against `crates/`. Patterns cited inline.

### W0 audit pass (items 1-12) — **all consumed in tests only**

```
grep -rn 'audit_payload_coverage|StructRegistryProbe|AuditCoverageReport|AbsentRegistryProbe|PayloadLayoutsProbe|PendingMarker|MissingMarker|MarkerStatus|GrammarAuditTag|GrammarCoverage|is_typed_arrow_fn|write_coverage_report' crates/ --include='*.rs' | grep -v 'crates/ir/src/passes/audit/'
```

External consumers: `ir/tests/payload_coverage_audit.rs`,
`ir/tests/struct_registry.rs`, `core/tests/project_types_{json,sheets,css_l4}.rs`.
Production-code consumer count = **0**. Re-export in `passes/mod.rs`
serves only test-side `use bbnf_ir::passes::*`.

### W1 StructRegistry (items 13-19) — **production-consumed**

```
grep -rn 'ir.struct_registry|\.struct_registry|StructRegistry\b|populate_struct_registry' crates/ --include='*.rs'
```

`populate_struct_registry` called from `ir/src/passes/types/mod.rs:410`
(inside `project_types`). `project_types` itself called from production
at `pipeline/compile.rs:209,218,238` and `backend/driver/analysis.rs:136`
— so the registry populates on every compile. `ir.struct_registry` is
read in production at `emitter/shapes/mod.rs:167,232`,
`emitter/grammar.rs:1090`, `pipeline/compile.rs:187`,
`shapes/flat/struct_direct.rs:81`, `shapes/wrap/struct_direct.rs:51`.
`lower/mod.rs:243` initialises a default registry on the IR.

`StructLayout`/`StructField` accessor surface (~22 methods) consumed
sparsely: `LayoutKind`/`StructLayout` types imported in production at
`runtime/builder.rs:43`, `runtime/json/builder.rs:47`, the per-shape
`*_struct_direct.rs` emit fns. Most `is_*`/`field_*`/`branches`/
`admits_*` accessors are **test-only consumers** (`ir/tests/struct_registry.rs`,
`core/tests/project_types_*.rs`).

### W1.B4 registry observer (items 20-23) — **mixed: 1 prod, 3 test-only**

```
grep -rn 'registry_observer::|drain_registry_read_log|clear_registry_read_log|RegistryReadEvent' crates/ --include='*.rs'
```

- `record` — fires in production at `emitter/shapes/mod.rs:233`.
- `drain`, `clear`, `RegistryReadEvent` — only consumer is
  `core/tests/emitter_registry_read.rs` (lines 33,120,126,135,172,176).

The observer's docstring (line 31-34) self-documents as removable at
AZ-I close: "the buffer is never read by emitter code; only the test
consumes it."

### W2 struct-direct cohort (items 24-44) — **dead in production**

```
grep -rn 'EmitStrategy::StructDirect' crates/ --include='*.rs'   # 52 src + 2 tests
grep -n 'JsonStructBuilder|JsonDocument|runtime::json' crates/core/src/grammar/generated/json.rs   # 0 hits
```

Every src reference sits inside a `match strategy { StructDirect => … }`
arm **structurally unreachable** in production: the resolver at
`emitter/strategy.rs:118-146` returns `TapeDirect` via catch-all
`_ =>`. The 9 `*_struct_direct` emit fns (37-44) are only called
from gated dispatch arms; the `JsonStructBuilder`/etc. types (24-34)
appear only inside `quote!{}` outputs that never reach `generated.rs`.

Test consumer: `core/tests/json_parity_struct.rs` (15+ direct
`JsonStructBuilder::new()` sites). Snapshot artefacts:
`tests/struct_direct_snapshots/*.snap` (9 files, 535 lines).

## 3. W2-act activation gate impact

The single line at `emitter/strategy.rs:118-146` is the gate:

```rust
match (grammar_ident, registry_populated) {
    _ => EmitStrategy::TapeDirect,
}
```

**Activates on JSON-arm flip:** items 24-44 (`JsonStructBuilder` /
`JsonDocument` / `JsonArena` / 11 JSON value types / `StructBuilder`
trait / 9 per-shape emit fns / `EmitStrategy::StructDirect` /
`is_struct_direct`). Plus W1 accessors that the struct-direct
emitters need (`StructLayout::field`, `branches`, `branch_index`,
`seq_position`, `all_fields_share_type`, `is_tagged_enum`).

**Still unreached after flip:** none — the substrate is internally
complete; the W2.md §Reversal blockers are downstream test migrations
(`json_slab` / `projection_totality` / `typed_accessor_surface`),
not unwired substrate.

**Pure scaffold (no consumer regardless of flip):** W0 audit pass
surface (items 1-12) — never called in production code even after
W2 activation; remains a leaf-test diagnostic. Registry observer
`drain`/`clear` (items 22-23) — never load-bearing.

## 4. Dead substrate

Items added this session with **zero non-test, non-emitted-quote
production callers**:

| File:line | Item | Wave | Action |
|---|---|---|---|
| `passes/audit/payload_coverage.rs:394,573` | `audit_payload_coverage`, `write_coverage_report` | W0 | wire into `pipeline/compile.rs` post-`project_types` OR delete `audit/`. |
| `passes/audit/payload_coverage.rs:292,305` | `AbsentRegistryProbe`, `PayloadLayoutsProbe` | W0 | delete — superseded by `&StructRegistry` impl. |
| `emitter/shapes/registry_observer.rs:75,82` | `drain`, `clear` | W1.B4 | delete — module's docstring expects deletion at AZ-I close. |
| `emitter/shapes/registry_observer.rs:65` | `record` | W1.B4 | rewire-or-delete — fires but the recorded data has no production reader. |
| `registry/struct.rs:178,187,257,266,279` | `branch_index`, `seq_position`, `all_fields_share_type`, `field`, `branches` | W1 | wire on activation — needed by AltDispatch/Keyword struct-direct paths but those paths gated dead. |
| 9× `shapes/*/*struct_direct*.rs` (items 37-44) | per-shape struct-direct emitters | W2.RB-RF | activate via §6 flip; substrate complete. |
| `runtime/json/{value,arena,builder}.rs` (items 24-34) | `JsonValue`, `JsonDocument`, `JsonArena`, `JsonStructBuilder`, … | W2.A | activate via §6 flip. |
| `emitter/strategy.rs:154,161` | `is_struct_direct`, `is_tape_direct` | W2.RA | activates when both arms can fire. |

**Dead-substrate count: 11 line-item buckets.**

## 5. Substrate-without-consumer ratio

```
M  (production-consumer): 9      9 / 44 ≈ 20.5%
M' (test-only):           20    20 / 44 ≈ 45.5%
M'' (no consumer):        15    15 / 44 ≈ 34.0%
N  (total):               44
```

Per SPEC §"Substrate-with-consumer is one unit of work" + §Wave
verification ledger ("Substrate-without-consumer is rejected"), this
ratio is below acceptance for a closed wave. The W2 close ledger
declared the deferral intentional with W2-act follow-on. The W2
cohort (~25 items) is internally complete and a single `for_grammar`
arm-flip activates it; the W0 audit pass remains orphan even after
that flip with no W2-act activation path declared.

## 6. Refined wiring proposal

One follow-on dispatch (parallel-disjoint files) closes every dead-
substrate item:

**A. Activate W2 cohort (items 24-44 + 5 W1 accessors).**
- Edit `emitter/strategy.rs:118-147`: replace catch-all with
  `("JsonParser" | "JsonGrammar", true) => EmitStrategy::StructDirect { builder_path: "::bbnf::runtime::json::JsonStructBuilder", document_path: "::bbnf::runtime::json::JsonDocument" }`.
- Migrate 3 test files (`tests/json_slab.rs`, `tests/projection_totality.rs`,
  `tests/typed_accessor_surface.rs`) from `parsed.view()` to
  `JsonDocument` accessors (items 30, `runtime/json/value.rs:144,151`).
- Verify: `tests/json_parity_struct.rs` already covers parity;
  `cargo bench -p bbnf --bench json_monolithic` ≥ AU floor.

**B. Wire OR delete W0 audit pass (items 1-12).**
- Wire: emit `target/audit/<grammar>.json` from `pipeline/compile.rs`
  after `project_types` returns. Activates items 9, 11 + report
  types 3-5, 12 + the `&StructRegistry` impl (item 6).
- Delete: remove `AbsentRegistryProbe` (7), `PayloadLayoutsProbe`
  (8) — only test fixtures use them.

**C. Delete observer (items 20-23).**
- Drop `emitter/shapes/registry_observer.rs`, the `pub use` block
  in `shapes/mod.rs:101-104`, the `record` call at `shapes/mod.rs:233`,
  and `tests/emitter_registry_read.rs`. Module docstring (line 31-34)
  expects this at AZ-I close.

Verification per item: §A → `cargo test --test json_parity_struct` +
bench ≥ AU floor. §B → new `tests/audit_pipeline.rs` loading
`target/audit/json.json`. §C → workspace nextest zero diff
(observer is non-load-bearing today).

## 7. Hand-off for synthesis

For `docs/tranches/AZ-I/audit/W2-CLOSE-AUDIT.md`:

**Activate in W2-act:** items 24-44 via `for_grammar` arm-flip; plus
W1 `StructLayout` accessors (`field`, `branches`, `branch_index`,
`seq_position`, `all_fields_share_type`) which flow naturally once
emitters fire.

**Wire-or-delete in W2-act:** W0 audit pass (1-12) — wire via §6.B or
delete `crates/ir/src/passes/audit/` outright. Registry observer
(20-23) — delete per §6.C; the `record` write-only sink is non-load-
bearing.

**Already production-consumed:** `StructRegistry` +
`populate_struct_registry` (17-19); `EmitStrategy` (35) — both arms
needed.

**Headline:** **9 / 44** production ratio; the **single-line
activation flip** plus 3 test migrations is the smallest delta to
close ~25 of 35 currently-dead-or-test-only substrate items in one
pass.
