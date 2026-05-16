# SK-V7 B4 — Stub Lowering Body Design

Per-shape lowering bodies for the four diagnostic-string stubs in
`skinny/crates/codegen/src/lower/`. Companion to A5 (lock audit). Scope:
design only, no tracked-file edits.

---

## 1. Current state (verified)

### 1.1 Dispatcher and stubs

`skinny/crates/codegen/src/lower/mod.rs:1-9` declares one sub-module per
shape plus `rust` (dispatch) and `schema_direct` (SinkOnly→typed lift).

`skinny/crates/codegen/src/lower/rust.rs:48-61` is the per-shape switch:

```rust
fn lower_rule(rule: &BackendRule, shape: BackendShape) -> RuleLoweringPlan {
    let body = match shape {
        BackendShape::EagerTape    => eager_tape::lower_rule(rule),
        BackendShape::OffsetTape   => offset_tape::lower_rule(rule),
        BackendShape::EventTape    => event_tape::lower_rule(rule),
        BackendShape::SinkOnly     => sink_only::lower_rule(rule),
        BackendShape::CollapsedStage => collapsed_stage::lower_rule(rule),
    };
    RuleLoweringPlan { rule: rule.name.clone(), shape, body }
}
```

`RuleLoweringPlan.body` is a `String` — it is collated for diagnostics
only, never written into the generated parser file. The actual parser
output is `json_templates/generated.rs` (hand-authored template,
`codegen/src/lib.rs:201-203`) concatenated with the SinkOnly direct
render (`codegen/src/lib.rs:117`).

### 1.2 Stub LOC (verified by `wc -l` and direct read)

| File | LOC | Body |
|---|---:|---|
| `lower/sink_only.rs` | 226 | Real lowering (BIR→`SinkOnlyExpr`, dispatch counts, direct shape extraction). Drives `json_sink_direct::render` and `lower::schema_direct`. |
| `lower/eager_tape.rs` | 5 | `format!("rule {} -> eager_tape", rule.name)` |
| `lower/offset_tape.rs` | 5 | `format!("rule {} -> offset_tape", rule.name)` |
| `lower/event_tape.rs` | 5 | `format!("rule {} -> event_tape", rule.name)` |
| `lower/collapsed_stage.rs` | 5 | `format!("rule {} -> collapsed_stage", rule.name)` |
| `lower/rust.rs` | 62 | Dispatcher; returns `LoweredRust { rule_plans, sink_only_program }`. |
| `lower/schema_direct.rs` | 41 | Wrapper hoisting SinkOnly into typed schema. |

### 1.3 The OffsetTape conflation

`codegen/src/lib.rs:159-166` defaults **every rule** to
`BackendShape::OffsetTape`. The generated parser file (template
`json_templates/generated.rs` mirrored at
`runtime/src/grammars/json/generated.rs:1-394`) implements the
OffsetTape body inline:

- `parse_value_at` / `dispatch_value` (lines 37-58) — first-byte dispatch
- `parse_object` (lines 62-79), `parse_array` (lines 121-138) — container loops
- `consume_container_next` (lines 309-338) — V6 admit 2b3bef79 (ContainerNext)
- `match_tiny_plain_string_with_cap::<16>` (lines 161-182) — V6 admit 1e213001 (tiny-string cap)
- `state.emit_plain_offset(offset)` — tape offset emission (the OffsetTape primitive)

The SinkOnly path lives in the **same file** at lines 394-end and is
appended via `json_sink_direct::render` (`codegen/src/lib.rs:117`):

- `parse_direct<S: JsonSink>` (line 408) — direct typed sink
- `parse_value_direct` (line 426), `parse_object_direct` (line 547), etc.

So OffsetTape is "real" only as a hand-authored template; SinkOnly is
"real" via both `lower/sink_only.rs` analysis AND a generator
(`json_sink_direct.rs`, 563 LOC) that consumes the analysis to emit
fresh code. The other three shapes have neither analysis nor generator.

---

## 2. Per-shape body design

Each design table lists (a) the trigger conditions per ARCHITECTURE.md
§7.3, (b) the analysis types the body must collect, (c) the emitter it
must drive, (d) the runtime consumer, (e) the falsifiability gate.

### 2.1 OffsetTape (highest priority — extraction, not invention)

**Trigger**: default retained shape; no recovery requirement; no host
parse-time decode; first-set deterministic. This is what bbnf does
today; the body just makes the implicit explicit.

**Body signature** (`lower/offset_tape.rs`, target ~200 LOC):

```rust
pub struct OffsetTapeProgram {
    pub entry_rule: String,
    pub rules: Vec<OffsetTapeRule>,
    pub dispatch_alphabets: BTreeMap<String, DispatchAlphabet>,
    pub container_next_sites: BTreeSet<ContainerNextSite>,
    pub tiny_string_caps: BTreeMap<String, u32>,
    pub structural_alphabet: Vec<u8>,
}

pub struct OffsetTapeRule {
    pub name: String,
    pub expr: OffsetTapeExpr,
    pub emits_offset: bool,
    pub container_kind: Option<ContainerKind>,
}

pub enum OffsetTapeExpr {
    DispatchAlt { alphabet: DispatchAlphabet, branches: Vec<...> },
    SeqEmit { steps: Vec<OffsetTapeStep> },
    ContainerLoop { open: u8, close: u8, sep: u8, body: Box<...> },
    Plain { ... },
}

pub fn lower_program(backend: &BackendIr) -> OffsetTapeProgram { ... }
```

**Analysis collected**:

| Fact | Source | Used by emitter |
|---|---|---|
| dispatch alphabet per `AltMode::Dispatch` | BIR alt branches' first sets | `dispatch_value` arm table |
| container-next sites | BIR `RepeatLoop` with separator literal | `consume_container_next` lowering (V6 admit 2b3bef79) |
| tiny-string caps | `RegexProgram { span_kind: String, .. }` + grammar annotation | `match_tiny_plain_string_with_cap::<N>` (V6 admit 1e213001) |
| structural alphabet | union of `ByteLiteral` containing single byte across grammar | `STRUCTURAL_ALPHABET_JSON` constant |

**Emitter**: a new `codegen/src/offset_tape_emit.rs` (~150 LOC) that
takes `OffsetTapeProgram` and replaces the prelude of the template.
Equivalent to what `json_sink_direct.rs` does for SinkOnly.

**Runtime consumer**: existing `runtime/src/grammars/{grammar}/parser.rs`
+ `tape.rs` + `view.rs` + `value.rs` — already in place; no changes.

**Falsifiability gate**: byte-identical `runtime/grammars/json/generated.rs`
prelude (lines 1-394) before vs after extraction. Zero RESULTS
regression on JSON benches.

**Build order step 1** (Wave 1, post-comparator-plane repair).

---

### 2.2 EagerTape (Wave 2)

**Trigger** (per ARCHITECTURE.md §7.3):
- `@error(recover)` on any rule in the chain — recovery requires
  source-byte fallback so the parser can resynchronize
- `@host fn` parse-time decode — host computation that must see fully
  decoded value at parse time (not lazy)
- `@layout` scope wider than the rule — layout facts span the parent
- first-set overlap that defeats `AltMode::Dispatch` — non-deterministic
  branching needs recursive descent backtracking

**Body signature** (`lower/eager_tape.rs`, target ~150 LOC):

```rust
pub struct EagerTapeProgram {
    pub entry_rule: String,
    pub rules: Vec<EagerTapeRule>,
    pub host_calls: BTreeSet<HostCallSite>,
    pub recovery_sites: BTreeSet<String>,
}

pub struct EagerTapeRule {
    pub name: String,
    pub return_type: ValueType,   // fully decoded
    pub expr: EagerTapeExpr,
    pub recovers: bool,
}

pub enum EagerTapeExpr {
    AltOrdered { branches: Vec<EagerTapeExpr> },  // longest-match
    Seq(Vec<EagerTapeExpr>),
    Repeat { body: Box<EagerTapeExpr>, min: u32 },
    Optional(Box<EagerTapeExpr>),
    Byte(u8),
    Regex { pattern: String, materialize: MaterializeKind },
    Call(String),
    HostDecode { fn_name: String, args: Vec<EagerTapeExpr> },
}

pub fn lower_program(backend: &BackendIr) -> Option<EagerTapeProgram> { ... }
```

**Distinguishing emissions**:

| Concern | OffsetTape | EagerTape |
|---|---|---|
| Per-rule signature | `fn parse_X(&mut ParserState) -> Result<(), ParseError>` | `fn parse_X(&mut ParserState) -> Result<ValueX, ParseError>` |
| Payload materialization | lazy (offset only) | eager (decoded value) |
| Branch failure | first-byte dispatch error | rewind cursor, try next branch |
| Recovery | none | `@error(recover)` rules emit error node, advance to sync set |
| Host call | rewrites later via `ValueRef` | inline `host::decode_X(state, args...)?` |

**Runtime consumer**:
`runtime/src/grammars/{grammar}/eager_tape.rs` (new entry point) +
existing host bridge. The consumer holds decoded values directly; no
ValueRef lift.

**Falsifiability gate**: at least one grammar rule in the cohort that
declares `@error(recover)` or carries an `@host` parse-time decode
admits EagerTape. Cohort C1 (JSON) **does not** trigger this shape —
JSON has no recovery requirement and no parse-time host decode.
Gate row TBD when first triggering grammar lands.

**Build order step 2**.

---

### 2.3 EventTape (Wave 2)

**Trigger** (per ARCHITECTURE.md §7.3): retained shape with **sidecar
facts** — recovery + layout + host decode survive as **separate
event cells** rather than inline offsets. Distinct from EagerTape
because payloads remain lazy; distinct from OffsetTape because
side-facts are first-class.

**Body signature** (`lower/event_tape.rs`, target ~180 LOC):

```rust
pub struct EventTapeProgram {
    pub entry_rule: String,
    pub rules: Vec<EventTapeRule>,
    pub event_kinds: BTreeSet<EventKind>,
    pub sidecar_facts: BTreeSet<SidecarFact>,
}

pub enum EventKind {
    OpenContainer { kind: ContainerKind },
    CloseContainer { kind: ContainerKind },
    Atom { kind: AtomKind },
    LayoutMark { label: String },
    RecoverPoint { label: String },
    HostNote { fn_name: String },
}

pub struct EventTapeRule {
    pub name: String,
    pub emits: Vec<EventKind>,
    pub expr: EventTapeExpr,
}

pub fn lower_program(backend: &BackendIr) -> Option<EventTapeProgram> { ... }
```

**Tape shape difference**:

| Aspect | OffsetTape | EventTape |
|---|---|---|
| Cell type | `u32` offset (packed flags + position) | `Event { kind: u8, payload: u32, tag: u16 }` (8 bytes) |
| Cell density | one per structural | one per structural **plus** per side-fact |
| Payload lookup | `ValueRef::from_offset(tape[i], input)` | `Event::payload_at(input, &events)` |
| Recovery cell | n/a | dedicated `EventKind::RecoverPoint` cell |
| Layout cell | n/a | dedicated `EventKind::LayoutMark` cell |

**Emitter**: a new `codegen/src/event_tape_emit.rs` (~150 LOC). Per-rule
emission writes events to a typed `EventTape<'i>` instead of `Tape<'i>`.
Container loops emit `OpenContainer` / `CloseContainer` pairs; atoms
emit `Atom { kind }`; layout/recover/host produce dedicated event kinds.

**Runtime consumer**:
`runtime/src/grammars/{grammar}/event_tape.rs` (new) + `event.rs`
(new module defining `Event` and `EventTape<'i>`). Visitor pattern over
events for downstream consumption.

**Falsifiability gate**: at least one cohort grammar (C2 or C5 per
ARCHITECTURE.md §10) names sidecar facts as a requirement. Verify with
cohort declaration before committing the body — otherwise the shape has
no consumer and should remain `BBNF-EVENTTAPE-NOT-VIABLE` diagnostic.

**Build order step 3**.

---

### 2.4 CollapsedStage (Wave 5, gated on NASM author)

**Trigger** (per V9.5 PSI excavation, commit 74406332): a grammar whose
admissibility predicate over `CollapsedStage` passes — DPDA stack depth
bounded, alphabet within ISA classifier capacity, no recovery, no host
call inside the stage. The Rust automaton is **fatal** per V9.5; only
hand-written NASM is admissible.

**Body signature** (`lower/collapsed_stage.rs`, target ~80 LOC Rust;
asm body is per-grammar **out-of-tree** at `ext/x86/{grammar}_collapsed.asm`):

```rust
pub struct CollapsedStageProgram {
    pub grammar: String,
    pub classifier_lut: Vec<u8>,           // 256-entry byte→class
    pub state_transition_lut: Vec<u8>,     // (state, class) → state
    pub initial_state: u8,
    pub accept_states: BTreeSet<u8>,
    pub asm_entry_symbol: String,          // e.g. "bbnf_json_collapsed"
}

pub fn lower_program(
    backend: &BackendIr,
    admissibility: &CollapsedAdmissibility,
) -> Result<CollapsedStageProgram, NotAdmissible> { ... }

pub enum NotAdmissible {
    UnboundedStack,
    AlphabetTooWide,
    RequiresRecovery,
    RequiresHostCall,
    NoAsmAuthor,        // .asm file absent
}
```

**Emission**:
1. `classifier_lut.data` — 256-byte file, raw bytes
2. `state_transition_lut.data` — `STATES*CLASSES`-byte file, raw bytes
3. `{grammar}_collapsed_shim.rs` — Rust caller shim with `extern "C"`
   binding to `asm_entry_symbol`, ~40 LOC

**Where Rust automaton is rejected** (V9.5): emitting a Rust state
machine over the same LUTs would produce a switch/branch-heavy body
LLVM cannot pack into SIMD; the .asm body must use direct GFNI/VBMI2
classification per `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md`.

**Runtime consumer**:
`runtime/src/grammars/{grammar}/collapsed.rs` — wrapper that links the
NASM kernel and exposes a parse entry point matching the OffsetTape
signature (so downstream consumers are shape-agnostic).

**Fallback**: when `NotAdmissible::NoAsmAuthor` for a grammar, codegen
emits a `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` diagnostic and the rule(s)
fall back to OffsetTape automatically. This is the only shape with a
fallback rail because asm authorship is out-of-band.

**Falsifiability gate**: data tables emitted by Rust match a
hand-authored .asm body's expected constants (integration test reads
both, diffs bytes). Until one grammar has a complete .asm body, this
body emits the data tables and the diagnostic but no Rust shim is
linked.

**Build order step 4** (after Waves 1-3 land, gated on first NASM author).

---

## 3. Refactor: `json_sink_direct` and `json_typed_direct` into `lower/`

The two existing emitters live at the top of `codegen/src/`:

| File | LOC | Role |
|---|---:|---|
| `codegen/src/json_sink_direct.rs` | 563 | Consumes `SinkOnlyProgram` from `lower/sink_only.rs`, emits direct-sink Rust |
| `codegen/src/json_typed_direct.rs` | 646 | Consumes typed lift from `lower/schema_direct.rs`, emits typed-direct Rust |

**Proposed move** (Wave 1, ~50 LOC delta):
- `codegen/src/json_sink_direct.rs` → `codegen/src/lower/sink_only_emit.rs`
- `codegen/src/json_typed_direct.rs` → `codegen/src/lower/sink_only_typed_emit.rs`

Both become siblings of `lower/sink_only.rs` (analysis) under the same
namespace. The lib.rs entry point updates two `use` paths. No behavior
change.

Rationale: cross-shape isomorphism — every shape has
`lower/{shape}.rs` (analysis) + `lower/{shape}_emit.rs` (Rust emit) in
one directory. Matches the directory-module-structure feedback (splits
use directory modules, not flat siblings).

---

## 4. LOC budget (totals)

| Component | LOC |
|---|---:|
| `lower/offset_tape.rs` body | 200 |
| `lower/offset_tape_emit.rs` (new) | 150 |
| `lower/eager_tape.rs` body | 150 |
| `lower/eager_tape_emit.rs` (new) | 130 |
| `lower/event_tape.rs` body | 180 |
| `lower/event_tape_emit.rs` (new) | 150 |
| `lower/collapsed_stage.rs` body (Rust shim + data emit) | 80 |
| Refactor `json_sink_direct` → `lower/sink_only_emit.rs` | 50 delta |
| Refactor `json_typed_direct` → `lower/sink_only_typed_emit.rs` | 50 delta |
| `runtime/src/grammars/{grammar}/event_tape.rs` template | 80 |
| `runtime/src/grammars/{grammar}/eager_tape.rs` template | 80 |
| `runtime/src/event.rs` (EventTape data structures) | 100 |
| Tests (per-shape parity vs OffsetTape baseline) | 300 |
| **Total** | **~1700** |

Note: my original 960 estimate undercounted the per-shape emitters
(separate from analysis bodies) and the new runtime consumer
templates. Conservative ~1700 LOC across Waves 1, 2, 5.

---

## 5. Wave alignment

| Wave | Item | Behavior change |
|---|---|---|
| **Wave 0** | Comparator-plane repair (prerequisite) | none directly here |
| **Wave 1** | OffsetTape body extraction + refactor SinkOnly emitters into `lower/` | zero — byte-identical generated.rs prelude |
| **Wave 2** | EagerTape + EventTape bodies | new shape rows accepted by `default_backend_shape` selector logic; admit predicate gates whether either is selected |
| **Wave 3** | EagerTape / EventTape runtime consumers + integration tests | per-shape parser entry points live |
| **Wave 5** | CollapsedStage Rust shim + .data table emission | no Rust path; gates on first NASM author |
| Future | Per-grammar NASM kernels (`ext/x86/{grammar}_collapsed.asm`) | enables CollapsedStage path per grammar |

---

## 6. Build-order ranking

1. **OffsetTape body** — highest priority. Currently de-facto via
   hand-written `json_templates/generated.rs`; extraction makes per-rule
   branching visible without behavior change. Prerequisite to any
   multi-shape selection logic.
2. **Refactor SinkOnly emitters into `lower/`** — Wave 1, paired with
   OffsetTape. Establishes the analysis+emit pattern uniformly.
3. **EagerTape body** — Wave 2. Needed before any grammar declares
   `@error(recover)` or parse-time `@host` decode.
4. **EventTape body** — Wave 2. Only if cohort confirms a sidecar-facts
   consumer; otherwise remains a non-viable diagnostic.
5. **CollapsedStage shim** — Wave 5. Gates on per-grammar NASM author.
   Rust-side is data-emission only; never an automaton.

---

## 7. File:line citation index

| Reference | File:line |
|---|---|
| `BackendShape` enum | `skinny/crates/ir/src/lib.rs:335-341` |
| Lower dispatch | `skinny/crates/codegen/src/lower/rust.rs:48-61` |
| Default OffsetTape selection | `skinny/crates/codegen/src/lib.rs:159-166` |
| `generated_rs()` template injection | `skinny/crates/codegen/src/lib.rs:201-203` |
| SinkOnly render append | `skinny/crates/codegen/src/lib.rs:117` |
| SinkOnly analysis | `skinny/crates/codegen/src/lower/sink_only.rs:1-226` |
| SinkOnly Rust emit | `skinny/crates/codegen/src/json_sink_direct.rs:1-563` |
| Typed-direct emit | `skinny/crates/codegen/src/json_typed_direct.rs:1-646` |
| OffsetTape de-facto body — prelude | `skinny/crates/runtime/src/grammars/json/generated.rs:1-394` |
| `consume_container_next` (V6 admit 2b3bef79) | `skinny/crates/runtime/src/grammars/json/generated.rs:309-338` |
| `match_tiny_plain_string_with_cap` (V6 admit 1e213001) | `skinny/crates/runtime/src/grammars/json/generated.rs:161-182` |
| SinkOnly path | `skinny/crates/runtime/src/grammars/json/generated.rs:394-836` |
| EagerTape stub | `skinny/crates/codegen/src/lower/eager_tape.rs:3-5` |
| OffsetTape stub | `skinny/crates/codegen/src/lower/offset_tape.rs:3-5` |
| EventTape stub | `skinny/crates/codegen/src/lower/event_tape.rs:3-5` |
| CollapsedStage stub | `skinny/crates/codegen/src/lower/collapsed_stage.rs:3-5` |

---

## 8. Open questions for Wave 1 author

1. Does `OffsetTapeProgram` need cross-rule alphabet computation, or is
   per-rule sufficient? JSON's structural alphabet `b"{}[],:\""` is
   grammar-global; multi-rule grammars (CSS, EBNF) may diverge.
2. Should `lower/sink_only.rs` remain monolithic or split into
   `sink_only/{analysis.rs, direct_shape.rs, facts.rs}` ahead of the
   other-shape splits? Current 226 LOC is below the
   `feedback_generated_size_budget` threshold but per-shape parity
   suggests splitting.
3. EventTape's `Event` cell size (8 bytes proposed) vs OffsetTape's
   `u32` cell — does the tape arena allocator need a generic over cell
   type, or two parallel arena lanes? Defer to Wave 2 cohort selection.
4. Are the V6 admits (ContainerNext, tiny-string cap) **OffsetTape-only**
   or do they generalize to EventTape? Tiny-string is a string-payload
   optimization independent of cell type; ContainerNext is structural
   and applies to both. Documented as shared analysis facts.
