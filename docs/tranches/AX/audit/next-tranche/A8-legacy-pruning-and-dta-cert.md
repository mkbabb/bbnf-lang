# A8 — Legacy Pruning + DTA Certification + Housekeeping

Three-part audit grounded in master HEAD `851aaebc` on worktree
`bbnf-wt-ay-a8`. Read-only. Every claim cites a file:line or a bench
binary hash.

## Part 1 — Bench-path certification

### 1.1 Binary hashes + dates (prebuilt, fresh Apr 20 01:43)

All four benches were rebuilt into the shared `.profile-target/` after
the Apr 20 fresh `prepare-profile-wave.sh` run. Binary paths + mtimes:

| Bench | Binary | Size | mtime |
|-------|--------|------|-------|
| json_monolithic | `.profile-target/release/deps/json_monolithic-683fcdeaeb1021e7` | 672 896 B | Apr 20 01:43 |
| css_l4 | `.profile-target/release/deps/css_l4-0d1a22af4b4b8964` | 1 155 336 B | Apr 20 01:44 |
| google_sheets_monolithic | `.profile-target/release/deps/google_sheets_monolithic-55ac0b2aebfb253e` | 834 664 B | Apr 20 01:44 |
| bbnf_monolithic | `.profile-target/release/deps/bbnf_monolithic-1cce73194c2882ba` | 813 152 B | Apr 20 01:44 |

### 1.2 nm symbol verification (zero DTA/walker runtime symbols)

Case-insensitive `nm` over `dta|walker|Walker|Dta` returns empty on
every bench binary:

```
$ nm <binary> | grep -ciE 'dta|walker'
json_monolithic-683fcdeaeb1021e7: 0
css_l4-0d1a22af4b4b8964:          0
google_sheets_monolithic-55ac0b2aebfb253e: 0
bbnf_monolithic-1cce73194c2882ba: 0
```

Raw at `/tmp/a8-nm-dta.txt` + `/tmp/a8-nm-dta-full.txt`. For positive
control: `push_structural` + `finalise` symbols are present
(e.g. `__RNvMNtCs2ncsu9TbDjZ_4tape7columnsNtB2_7Columns15push_structural`),
confirming the nm invocation finds symbols that exist.

### 1.3 Samply top-30 self-time (zero DTA/walker frames)

Case-insensitive `grep -iE 'dta|walker|Dta'` over the
`profile.json.syms.json` sym string tables returns zero on every
bench:

| Bench | Profile syms.json | DTA/walker frame count |
|-------|-------------------|------------------------|
| json_monolithic/canada | `.profiles/samply/json_monolithic/canada/profile.json.syms.json` | 0 |
| css_l4/tailwind | `.profiles/samply/css_l4/tailwind/profile.json.syms.json` | 0 |
| google_sheets_monolithic/parse_stress | `.profiles/samply/google_sheets_monolithic/parse_stress/profile.json.syms.json` | 0 |
| bbnf_monolithic/css_l4_grammar | `.profiles/samply/bbnf_monolithic/css_l4_grammar/profile.json.syms.json` | 0 |

### 1.4 Expand-artifact layer verification

Emission-layer grep over the prepared `cargo expand` outputs in
`.profiles/samply/prebuild/expand/<bench>/expand.rs` returns only
doc-comment residue, zero runtime construction sites. The `DtaState`
enum variants (Literal, Regex, Seq, ByteDispatch, AltLinear, Repeat,
Ref, ShuntingYard, WsTrim, ConsumeToNextStructural, ClassifyByte,
Minus, Epsilon) are **never constructed** — every match is in
`/// the walker's DtaState::…` doc prose:

| Bench | DtaState:: construction | LiteralPayload | ConsumeToNext | __CLASSIFY_TABLE |
|-------|------------------------:|---------------:|---------------:|-----------------:|
| json_monolithic | 0 | 0 | 0 | 0 |
| css_l4 | 0 (3 doc matches) | 0 | 0 | 0 |
| google_sheets_monolithic | 0 (7 doc matches) | 0 | 0 | 0 |
| bbnf_monolithic | 0 | 0 | 0 | 0 |

What **is** emitted into generated.rs from the `tape::dta` module
surface: `DtaPrecedenceEntry` (52 sites — Pratt operator tables),
`DtaAssociativity::{Left,Right}` (44), `DtaRuleId(<num>)` (152, mostly
inside DtaPrecedenceEntry + DtaError payload), `DtaError::{Syntax,
UnexpectedEnd,InvalidState}` (218 — emitted error surface),
`DtaStateId::NONE` (108 — always the NONE sentinel in DtaError).

### 1.5 DtaStateId::NONE dead-at-runtime trace

Every one of the 108 `DtaStateId` references in
`crates/core/src/grammar/generated.rs` is the literal pattern
`DtaStateId::NONE`. Zero occurrences of `DtaStateId(<literal number>)`
— verified by
`grep -cE 'DtaStateId\(' crates/core/src/grammar/generated.rs == 0`.
Every site consumes NONE as a sentinel inside `DtaError::Syntax {
failing_state: DtaStateId::NONE, failing_rule: DtaRuleId(u32::MAX) }`
(the emitted fallback when the shape emitter cannot attribute the
failure to a state). The always-NONE constant survives in the DtaError
payload purely because `DtaError` carries those fields. The entire
`DtaStateId` type collapses to `NONE` at every runtime use site —
**kernel-dead apart from the error payload**.

### Verdict — Part 1

**PASS** on every bench. Zero DTA/walker runtime symbols in every
prebuilt bench binary; zero DTA/walker samply frames in every captured
profile.json.syms.json; zero `DtaState::*` enum construction in every
expand artefact. Non-doc DTA residue in generated.rs is
limited to (a) Pratt precedence tables (`DtaPrecedenceEntry +
DtaAssociativity + DtaRuleId`) which ARE live-consumed by
`tape::driver::lookup_precedence` at runtime, and (b) `DtaError`
payload carrying always-NONE `DtaStateId` + always-MAX `DtaRuleId`
error provenance fields.

## Part 2 — DTA residue inventory

### 2.1 `crates/tape/src/dta.rs` (550 LOC) — per-type classification

| Type | Lines (approx) | Construction in generated.rs | Classification |
|------|---------------:|-----------------------------|----------------|
| `DtaStateId` | 41-45 | 108× (all `::NONE`) | **Partial-live** — only NONE reachable; full 16-bit range dead |
| `DtaRuleId` | 49-51 | 152× (precedence + `u32::MAX` in error) | **Live** in DtaPrecedenceEntry; also always-MAX in error |
| `DtaFrameKind` | 55-65 | 0 | **Kernel-dead** |
| `DtaCounterOptional` | 68-76 | 0 | **Kernel-dead** |
| `SeqPromote` | 79-92 | 0 | **Kernel-dead** |
| `LiteralPayload` | 106-184 | 0 in expand + 0 in generated.rs | **Kernel-dead (runtime side)** — stage helper unused |
| `DtaAssociativity` | 187-195 | 44× | **Live** (precedence entries) |
| `DtaState` (13 variants) | 202-395 | 0 (7 doc-only) | **Kernel-dead** — full 550-LOC enum unreachable at runtime |
| `DtaPrecedenceEntry` | 398-415 | 52× | **Live** (Pratt tables) |
| `DtaRuleEntry` | 418-424 | 0 | **Kernel-dead** |
| `DtaTable` + `DtaTable::EMPTY` + `rule_entry_for` + `has_shunting_yard` + `has_counter_optional` | 431-494 | 0 | **Kernel-dead** |
| `DtaDiagnostic` + `::EMPTY` + `observe` + `tick` | 509-549 | 0 | **Kernel-dead** |

### 2.2 Public `tape` crate re-exports (from `crates/tape/src/lib.rs:81-84`)

The `pub use dta::{…}` re-export enumerates 12 names. Classification
of each re-export at current HEAD:

- `DtaAssociativity`, `DtaPrecedenceEntry`, `DtaRuleId` — **live**
  (Pratt surface + error payload).
- `DtaStateId` — **partial-live** (NONE sentinel only).
- `DtaError` — **live** (re-exported transitively via
  `driver.rs:44`; emitted into every `parse()` signature).
- `LiteralPayload` — **kernel-dead** at runtime; still consumed by IR
  pass `crates/ir/src/passes/recognizers/dta.rs` as a mining-side
  enum (1611 LOC lifter, lines 127-184 define its own mirror).
- `DtaCounterOptional`, `DtaFrameKind`, `DtaRuleEntry`, `DtaState`,
  `DtaTable`, `DtaDiagnostic`, `SeqPromote` — **kernel-dead** at
  runtime AND at emission. Not constructed anywhere in
  `generated.rs` nor in any expand artefact.

### 2.3 External consumers of `dta::*` types (runtime-reachable, non-test)

| File | Line | Type | Purpose |
|------|------|------|---------|
| `crates/tape/src/driver.rs` | 35 | `DtaPrecedenceEntry, DtaRuleId, DtaStateId, LiteralPayload` | Imports for `lookup_precedence`, `DtaError` payload, `stage_literal_payload_in_arena` (unused at runtime) |
| `crates/tape/src/driver.rs` | 44 | `DtaError` definition | Re-exports to tape crate top level |
| `crates/tape/src/driver.rs` | 264-268 | `DtaPrecedenceEntry` | `lookup_precedence` runtime consumer |
| `crates/core/src/backend/rust/emitter/precedence.rs` | 92, 97, 186, 235, 266 | `DtaPrecedenceEntry` | Emitter writes the Pratt tables |
| `crates/core/src/backend/rust/emitter/grammar.rs` | 365 | `bbnf_ir::passes::lift_dta(ir)` | Lifter invoked ONLY to mine regex patterns + precedence; the table itself is NOT emitted as runtime data (comment line 363: "the table is NOT emitted as runtime data") |
| `crates/core/src/backend/rust/emitter/dfa_codegen.rs` | 81-320 | `DtaState, DtaTable` (IR side) | Reads IR-side DtaState::Regex/WsTrim for pattern extraction — mining-side only |

### 2.4 Stale-test references

5 stale tests (already flagged in `00-session-recap.md §3.1` +
`07-synthesis.md §3.1`):

| Test file | DTA residue | Category |
|-----------|-------------|----------|
| `crates/core/tests/json_parity_shape_emit.rs` | Uses deleted `dta_run_JsonGrammar` walker symbol + `__dta_walker_inline::run` | **Test-for-retired-walker**; retire |
| `crates/core/tests/bbnf_profile_wire_contract.rs` | No DTA; 8 compile errors on carved `GrammarProfile` fields | Unrelated to DTA; retire per invariant 14 |
| `crates/core/tests/grammar_profile_wire_contract.rs` | No DTA; 15 compile errors on carved fields | Unrelated to DTA; retire |
| `crates/core/tests/gate_predicate_wire_contract.rs` | No DTA; 2 compile errors on retired shape predicates | Unrelated to DTA; retire |
| `crates/core/tests/aw_v_w5_2_per_ref_routing.rs` | No DTA; 2 compile errors on retired predicates | Unrelated to DTA; retire |

Additional DTA-touching tests whose scope is narrow variant-exists
assertions:

| Test file | DTA construction | Disposition |
|-----------|------------------|-------------|
| `crates/core/tests/ctns_lifter.rs` | `DtaState::ConsumeToNextStructural` (tape side + IR side) | Tape-side variant never emitted at runtime (§1.4); this test asserts the tape variant EXISTS. **Retire if ConsumeToNextStructural is deleted from tape::DtaState** |
| `crates/core/tests/classify_byte_dispatch.rs` | Constructs `DtaState::ClassifyByte { table, fallback }` to assert the emitter produces `__CLASSIFY_TABLE_N` shape | Emitter verified (§1.4) to produce ZERO `__CLASSIFY_TABLE_N` in expand artefacts. **Retire if ClassifyByte is deleted** |
| `crates/core/tests/pratt_const_fold.rs` | Uses `DtaPrecedenceEntry` | **Keep** — Pratt tables are live |
| `crates/core/tests/css_l4_shape_emit.rs` + `sheets_shape_emit.rs` | Reference `DtaError` | **Keep** — DtaError is live |

### 2.5 Other legacy-area audits

#### `crates/json-prototype/` (2246 LOC across 6 .rs files)

Per `AW-V.W2.1` the prototype is an archival speed-ceiling comparator.
Workspace member per `Cargo.toml` top-level; `cargo build --workspace`
compiles it; not gated as dev-only. Files:

- `lib.rs` (394), `number.rs` (366), `simd.rs` (388), `string.rs`
  (212), `value.rs` (352), `visitor.rs` (534).
- Has its own `[[bench]] name = "json_value"` harness in
  `crates/json-prototype/Cargo.toml`.
- NOT referenced by any grammar's generated.rs; NOT consumed by
  `bbnf`/`bbnf-ir`/`bbnf-regex`/`simd-scan`/`tape`/`gorgeous`.

Disposition: **retain as archival bench crate**, or delete the crate
entirely when `<Grammar>Value` materialisation lands (AY.W3). The
2246 LOC serves only as a comparison baseline; post-Value-API landing
it becomes redundant with `bbnf_value_twitter` + sonic comparators.

#### `.cargo/patches/`

Does not exist. `.cargo/` contains only `config.toml` (43 LOC — active
path-dependency overrides). No orphan patches to prune.

#### `GrammarProfile` field consumers

After W0b.A carved 7 slots, 12 fields remain. Non-test consumer
check (`grep \.<field>\b crates/*/src/`):

| Field | Non-test consumer | Status |
|-------|-------------------|--------|
| `push_compound_count`, `push_leaf_count`, `push_leaf_with_count` | Only `profile.rs:245` (`total_push_sites` internal) | **No external consumer**; `total_push_sites` itself has no external consumer either |
| `compounds_per_input_byte`, `leaves_per_input_byte` | `profile.rs:231` (`capacity_for`) + `tape/src/psi.rs:312` | **Live** (tape capacity hint + PSI estimate) |
| `parallel_break_even_bytes` | `tape/src/psi.rs:420-423` | **Live** (parallel-parse gate) |
| `structural_alphabet`, `structural_digraphs`, `structural_digraph_mask`, `structural_quote_classes` | `crates/simd-scan/src/alphabet.rs:89-92` | **Live** (SIMD scanner config) |
| `list_rules` | Zero runtime consumer; emitted into generated.rs as `__GRAMMAR_PROFILE_LIST_RULES` static but never read | **Dead** (reserved for V6) |
| `shape_dict` | Zero runtime consumer; emitted as `__GRAMMAR_PROFILE_SHAPE_DICT` static but never read | **Dead** (reserved for V5) |

#### Other tape siblings

`crates/tape/src/shape_dict.rs` (79 LOC): `BbnfShapeEntry,
BbnfShapeKind` — consumed only by IR pass
`crates/ir/src/passes/recognizers/shape_dict_bbnf.rs`; never
materialised into generated.rs (`BBNF_SHAPE_DICT` emitted: 0
occurrences). `push_shape_ref` defined in `builder.rs:634` but zero
call sites anywhere. **Kernel-dead substrate**.

`crates/tape/src/stage1.rs` (StructuralIndex 81-LOC type): consumed
by `simd-scan` (scalar.rs:12+, neon.rs:40+) and by
`crates/core/src/runtime/mod.rs:43`. However, zero references to
`StructuralIndex` or `scan_structural` appear in generated.rs or any
expand artefact. **Kernel-dead at current emitter wiring**; live
substrate but no emitted call sites.

#### Stage1/SIMD integration status

`simd-scan` compiles against stage1 substrate, but no grammar's
`parse()` fn routes through `scan_structural` at emit time.
Substrate-without-consumer at the grammar-emission boundary.

## Part 3 — Untracked files + housekeeping

### 3.1 Git status

`git status --porcelain -uall` = **clean** (0 untracked, 0 modified).
`.profile-target/` and `.profiles/` both gitignored
(`.gitignore:29-30`). No orphan untracked trees.

### 3.2 `.bbnf-cache` directories

Exactly one `.bbnf-cache` directory exists:
`/Users/mkbabb/Programming/bbnf-lang/target/.bbnf-cache`. Worktrees
inherit `target/` via symlink per the orchestration contract, so no
scattered caches across the 54 worktrees.

### 3.3 Orphan worktrees

`git worktree list` returns **54 entries**. Breakdown:

- **2 prunable `/private/tmp`** — `bbnf-samply-test`, `bbnf-wt-test`.
  Both pre-flight test worktrees. **Prune via `git worktree prune`.**
- **17 `.claude/worktrees/agent-*`** — transient agent worktrees
  from prior sessions. Each 5-20 GB on disk due to target/ symlinks
  but git metadata only; **retain for replay OR prune with
  `--force`.**
- **6 `bbnf-wt-aw3-r*`** — AW tranche research worktrees (closed
  tranche). **Prunable.**
- **2 `bbnf-wt-aw-reduce` / `bbnf-wt-aw-walker`** — AW closure
  worktrees. **Prunable.**
- **8 `bbnf-wt-ax-w1r-*`** — AX.W1r execution worktrees; AX.W1r
  cascade completed 2026-04-18. **Prunable.**
- **1 `bbnf-wt-ax-w1-c`** — prior AX.W1 closure attempt. **Prunable.**
- **6 `bbnf-wt-az-a[1-6]`** — AZ-aliased audit worktrees (these are
  the Apr 20 audit wave that became AX-continuation). Active for the
  07-synthesis wave. **Retain until AY opens.**
- **4 `bbnf-wt-ay-a[7-10]`** — current AY-planning wave (A7, A8 now,
  A9, A10). **Active.**
- **5 `bbnf-wt-{bbnf,css,json,sheets,sonic}-au`** — AU-wave profile
  worktrees. AU closed long ago. **Prunable.**

Total prunable worktrees: **~38 of 54**.

### 3.4 `.profiles/` staleness

Gitignored. Two generations present:

- **Fresh Apr 20 generation** — covers every bench at HEAD `9074a685`
  (the A1-A6 audit prepare). Total `.profiles/samply/prebuild` =
  15 MB; per-bench profile dirs 472 KB – 1.8 MB each.
- **Stale Apr 15-17 generation** — 2098 json profile files in
  `.profiles/samply/json_value/{sonic_,bbnf_}*/`. These predate W0b
  walker retirement.

Additional `-az-a5` suffixed dirs — `json_monolithic/canada-az-a5`,
`css_l4/tailwind-az-a5` — are the A5 agent's re-captures over stale
baselines (Apr 20 01:01-04). Redundant with the fresh Apr 20 01:43-51
bench-level captures; **prune.**

## Part 4 — Pruning scope summary

### 4.1 Kernel-dead DTA carve from `crates/tape/src/dta.rs` (≈420 LOC)

Delete the following from `crates/tape/src/dta.rs`:

| Symbol | Lines | LOC |
|--------|-------|----:|
| `DtaFrameKind` enum | 54-65 | 12 |
| `DtaCounterOptional` enum | 68-76 | 9 |
| `SeqPromote` enum | 78-92 | 15 |
| `LiteralPayload` enum + impl | 94-184 | 91 |
| `DtaState` enum (13 variants + docs) | 197-395 | 199 |
| `DtaRuleEntry` struct | 417-424 | 8 |
| `DtaTable` struct + `EMPTY` + `rule_entry_for` + `has_shunting_yard` + `has_counter_optional` | 426-494 | 69 |
| `DtaDiagnostic` struct + `EMPTY` + `observe` + `tick` | 496-550 | 55 |
| **Total dead carve** | | **~458 LOC** |

**Retained surface** (92 LOC): `DtaStateId` (may shrink to `NONE`-only
sentinel or fold into `DtaError`), `DtaRuleId`, `DtaAssociativity`,
`DtaPrecedenceEntry`. `DtaError` lives in `driver.rs`, not `dta.rs`.

### 4.2 Tape crate siblings to carve

| File | LOC | Kernel-dead? | Disposition |
|------|----:|:------------:|-------------|
| `crates/tape/src/shape_dict.rs` | 79 | Yes (BBNF_SHAPE_DICT never emitted) | Delete |
| `crates/tape/src/stage1.rs` (StructuralIndex) | ~80 body | Substrate-without-emitter-wiring | Keep pending AY SIMD re-enable, OR delete |
| `crates/tape/src/driver.rs` `stage_literal_payload_in_arena` | ~20 | Unused (LiteralPayload dead) | Delete when LiteralPayload prunes |

### 4.3 `GrammarProfile` carve

From `crates/tape/src/profile.rs` + emitter:

- `push_compound_count`, `push_leaf_count`, `push_leaf_with_count`:
  emitted, never externally read. Total 3 u16 fields + emitter lines +
  `total_push_sites` helper. **Carve ~8 LOC.**
- `list_rules` field + `__GRAMMAR_PROFILE_LIST_RULES` emission. No
  consumer. **Carve ~50 LOC (field def + emitter block in
  `profile.rs:127`).**
- `shape_dict` field + `__GRAMMAR_PROFILE_SHAPE_DICT` emission + the
  whole `ShapeEntry` struct (30 LOC) + emitter block in
  `profile.rs:134`. **Carve ~90 LOC.**

### 4.4 Stale tests (AY.W0 wave)

Five files delete in one commit (per W0b.D style):

- `crates/core/tests/bbnf_profile_wire_contract.rs` (~360 LOC)
- `crates/core/tests/grammar_profile_wire_contract.rs` (~480 LOC)
- `crates/core/tests/json_parity_shape_emit.rs` (~200 LOC)
- `crates/core/tests/gate_predicate_wire_contract.rs` (~200 LOC)
- `crates/core/tests/aw_v_w5_2_per_ref_routing.rs` (~150 LOC)

Plus conditional on `DtaState::ClassifyByte` / `ConsumeToNextStructural`
pruning: `classify_byte_dispatch.rs` (~135 LOC) + `ctns_lifter.rs` (~86
LOC). These test emitter shapes that §1.4 proved never emit at
runtime.

### 4.5 IR-side DTA lifter

`crates/ir/src/passes/recognizers/dta.rs` (1611 LOC) is still called
from `emitter/grammar.rs:365` as a mining hook for regex pattern
extraction + Pratt precedence chains. **Not** prunable at the crate
level; however, the DTA-state mining half of the pass is unused by any
consumer — only the precedence + regex-pattern harvest is live. A
follow-on refactor (AY or AZ) would extract the harvest into a
dedicated miner and delete the state-machine lifting, reclaiming the
majority of the 1611 LOC.

## Part 5 — Live-substrate survivors

What from the DTA era is still functionally reachable at runtime:

1. **Pratt precedence pipeline**: `DtaPrecedenceEntry`,
   `DtaAssociativity`, `DtaRuleId` — emitted into every grammar's
   generated.rs (52 + 44 + 152 sites per grammar), consumed at
   runtime by `tape::driver::lookup_precedence` for shunting-yard
   operators (Sheets `__formula` chain, BBNF expression rules). This
   is the only sustained live-at-runtime DTA substrate.
2. **DtaError surface**: the emitted `parse()` signature returns
   `Result<_, DtaError>` with `{Syntax, UnexpectedEnd, InvalidState}`
   variants. `DtaStateId::NONE` + `DtaRuleId(u32::MAX)` appear as
   always-NONE / always-MAX sentinels inside `DtaError::Syntax`. Type
   is live; its `DtaStateId` payload field is dead (always NONE).
3. **SIMD structural alphabet** (`simd-scan` × `GrammarProfile`):
   simd-scan consumes `structural_alphabet / _digraphs / _digraph_mask
   / _quote_classes` at library entry. **But** no grammar's emitted
   `parse()` routes through `simd_scan::scan_structural`, so this is
   substrate-without-emitter-wiring — the lib compiles, the scanner
   runs in tests, never in the hot path.
4. **PSI parallel-parse gate**: `GrammarProfile::
   parallel_break_even_bytes` + `leaves_per_input_byte` consumed by
   `tape::psi::*`. Live.
5. **Tape capacity hint**: `GrammarProfile::capacity_for(input_len)`
   consumed by emitted `TapeBuilder::with_capacity` calls. Live.

Everything else declared in `crates/tape/src/dta.rs` — the 13-variant
DtaState enum, DtaTable, DtaDiagnostic, DtaRuleEntry,
DtaFrameKind, DtaCounterOptional, SeqPromote, LiteralPayload — is
**kernel-dead at runtime across all four bench grammars**.

## Part 6 — Housekeeping cleanup list

1. **Prune ~38 orphan worktrees** (2 `/private/tmp` prunable + 36
   `.claude/worktrees/agent-*` + closed-tranche siblings). Reclaim
   disk + git metadata.
2. **Delete stale Apr 15-17 `.profiles/` profiles** (~2 100 files
   under `.profiles/samply/json_value/`). Predate W0b walker
   retirement; every consumer is the fresh Apr 20 generation now.
3. **Delete redundant `-az-a5` profile dirs** (2 dirs:
   `json_monolithic/canada-az-a5`, `css_l4/tailwind-az-a5`). The
   fresh Apr 20 01:43 captures supersede.
4. **Keep `.bbnf-cache`** — single copy under `target/`, correctly
   scoped. No scattered caches.
5. **Keep `.cargo/config.toml`** — 43 LOC of active path overrides.
6. **json-prototype crate disposition**: 2246 LOC archival
   comparator. Retain until `<Grammar>Value` eager-parse materialiser
   lands (AY.W3), then delete.

## Hard gates

Every finding above cites at least one of:
(a) a binary hash in `.profile-target/release/deps/` with mtime;
(b) a `.profiles/samply/<bench>/<entry>/profile.json.syms.json`;
(c) an expand.rs path under `.profiles/samply/prebuild/expand/`;
(d) a file:line reference in `crates/`.

Report is read-only; worktree `bbnf-wt-ay-a8` committed this document
but made no source changes.
