# SK-V9 P2-B: Retained class/event grammar + `ValueRef` cursor proof

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-18.
Scope: Design the proof artefact for a retained class/event grammar plus a
`ValueRef<'tape, 'src>` cursor — the SK-V9 HANDOFF §3 candidate that REDRESS 92
named as "the retained class/event grammar plus retained `ValueRef` cursor
proof" prerequisite to any future W3 structural-heavy parse implementation.
Output: this file.
P1 hot-leaf antecedents: dead SIMD structural scanner (`consume_structural`
scalar rediscovery, P1 §1; `match_tiny_plain_string` + `match_string_at_quote`
pair-dominance on string-dense LOSS rows, P1 §2; per-cursor `(TapeId, cursor)`
identity already load-bearing in retained view per
`skinny/crates/runtime/src/grammars/json/view.rs:6-71`).
Lock surface: Lock 1 (substrate union; "structural projection IS the tape";
no parallel substrate); Lock 14 (zero grammar-specific code in generic
crates); Lock 16 (SIMD/ASM admissibility allowlist — only consumed by name).

Boundary, verbatim, from `restart/skinny/tranches/sk-v9/HANDOFF.md:72-87`:
"Retained class/event grammar and `ValueRef` cursor proof — REDRESS 92
rejected SK-V8 W3 before source redress. No structural-heavy parse
implementation reopens until the retained grammar and cursor proof are
accepted. […] 450 LOC, ≤90 min, **Proof-only; no `RESULTS.md` row movement at
Alpha depth**."

This artefact specifies *what* the proof is, *what it cannot do*, and *why it
is not the rejected SK-V6 retained-parse family wearing a new costume*.

---

## §1 — The proof shape

### §1.1 Thesis — the proof is a *compile-time* contract, not a runtime swap

The proof is the *grammar of the retained tape*: a Rust trait that names
class identity, event ordering, and the legal cursor walk for a retained
substrate, plus a `ValueRef<'tape, 'src>` cursor type that this trait
parameterises. The proof is satisfied when:

1. The trait compiles, is grammar-neutral by signature, and admits exactly
   the §2.3 SC-3 fact matrix (`json.fact.0`, `jsonl.fact.0`, `layout.fact.0`)
   through opaque generated ordinals.
2. A generated-but-narrow JSON instance witnesses every method without
   replacing today's production retained parser.
3. At least one non-JSON instance — CSS L4 *or* Sheets, by per-grammar
   data table only — compiles against the same trait without touching
   any generic crate's source. Lock 14 is *exercised*, not merely claimed.
4. `ValueRef<'tape, 'src>` borrows are valid against `cargo check`'s
   borrow checker for every method on the trait — there is no `'static`
   leak, no sidecar lifetime, no parser-owned tape cursor that outlives
   the substrate.

Nothing else is in scope. The proof has no production consumer; no
`scan.rs`/`generated.rs` edit lands on the JSON hot path; no row in
`skinny/RESULTS.md` moves; `cargo bench` is *not* a verification surface
for this candidate.

### §1.2 Owner files (proposed; not yet created)

| Path | Role | Approx LOC |
|---|---|---:|
| `skinny/crates/runtime/src/tape/event_grammar.rs` (NEW) | The `EventGrammar` trait + `ValueRef<'tape, 'src>` re-export shimmed against it; trait definition only — no `impl` for any grammar. | ~110 |
| `skinny/crates/runtime/src/tape/event_grammar_tests.rs` (NEW, `tests/` for the runtime crate per `feedback_no_inline_tests`) | Compile-only proof: a `phantom_borrow_check<G: EventGrammar>` function, the JSON instance witness, and the non-JSON instance witness. `#[test]` bodies are *type assertions* (`fn _assert<T: EventGrammar>() {}`), not runtime-executed parser code. | ~80 |
| `skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs` (NEW) | The narrow JSON `EventGrammar` instance — opaque class ordinals only, no `match grammar` arm, no production hot-path call site. Sits alongside `generated.rs` *as a sibling proof file*, not replacing it. | ~120 |
| `skinny/crates/runtime/src/grammars/css_l4_witness/event_grammar_witness.rs` (NEW) *or* `skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs` (NEW) | The Lock-14 non-JSON instance. The grammar directory is named `*_witness` to telegraph that no production parser, scanner, or runtime body lands. | ~80 |
| `skinny/crates/runtime/src/lib.rs` (TOUCHED) | One `pub mod tape;` already exports the substrate; add `pub use tape::event_grammar::EventGrammar;` and the two witness modules behind `#[cfg(test)]` or a `proof` feature so the witnesses cannot be linked into release. | ~5 |
| `restart/skinny/tranches/sk-v9/research/p2/` (this file) | The design artefact; no source. | n/a (research) |

Total source LOC: ~395, comfortably inside HANDOFF's 450 LOC envelope.
Generated-output LOC: zero (the witnesses are hand-written *proof* files, not
generated parsers; they sit in source paths under `runtime/src/grammars/<g>/`
that today exist only for `json/` — the witnesses are explicitly named
`*_witness` to mark them as proof artefacts and the `proof`/`#[cfg(test)]`
gate keeps them out of any release library surface). The clean-regen
discipline in `feedback_generated_files_clean_regen` is not breached because
no generated file is touched.

### §1.3 Trait sketch — `EventGrammar`

```rust
// skinny/crates/runtime/src/tape/event_grammar.rs

/// Compile-time grammar of a retained tape: class identity + event ordering
/// + cursor walk. Implementors are *data tables* (opaque generated class
/// ordinals + the SC-3 fact admission matrix). The trait carries no
/// grammar-specific method, no `match grammar` arm, no role enum.
///
/// `EventGrammar` is *not* a runtime parser API. It is the *contract* that a
/// retained substrate must satisfy for `ValueRef<'tape, 'src, Self>` cursor
/// walks to be statically well-formed. The trait has no production consumer
/// in SK-V9; it is satisfied by compile-only witnesses (§1.4).
pub trait EventGrammar: 'static {
    /// Number of structural classes in this grammar's alphabet (K, per
    /// SC-3 §2.2). Class ordinal 0 is non-structural; 1..=K are alphabet
    /// members in declaration order.
    const STRUCTURAL_CLASS_COUNT: u8;

    /// Opaque generated fact-id ordinal type. Generic code stores and
    /// binary-searches `FactRecord { cursor, fact_id, payload }`; it never
    /// `match`es on a fact id. Per-grammar witnesses pick the concrete type
    /// (a `#[repr(transparent)] struct JsonFactId(u8)` for JSON, etc.).
    type FactId: Copy + Eq + Ord + 'static;

    /// Compile-time admission predicate over a fact id. A fact id may
    /// appear on the tape iff `admits_fact(id)` is `true`. The witness
    /// proves the §2.3 matrix — `json.fact.0` for JSON, `jsonl.fact.0` for
    /// JSONL, `layout.fact.0` for indentation-sensitive grammars — without
    /// the generic substrate ever naming those facts.
    fn admits_fact(id: Self::FactId) -> bool;

    /// Compile-time class-ordinal validity predicate. `class` is the byte
    /// value generated `compact_mask` would write at this cursor; the
    /// witness proves that the class column's bytes are within `1..=K`.
    /// Generic code never interprets the ordinal; it only validates range.
    fn admits_class(class: u8) -> bool {
        class != 0 && class <= Self::STRUCTURAL_CLASS_COUNT
    }
}

/// A cursor into a retained `Tape<'src>`, parameterised by the grammar that
/// produced the tape. The cursor borrows the *tape*; the tape in turn
/// borrows the *source*; the cursor never borrows the source directly. This
/// is the load-bearing lifetime relationship — see §2.
pub struct ValueRef<'tape, 'src: 'tape, G: EventGrammar = AnyGrammar> {
    tape: &'tape Tape<'src>,
    cursor: u32,
    _grammar: PhantomData<fn() -> G>,
}
```

The trait is *deliberately minimal*. It carries no `step_into`, no
`event_kind`, no `class_at`. Method surface beyond `STRUCTURAL_CLASS_COUNT`,
`FactId`, `admits_fact`, and `admits_class` would invite either a generic
event role (Lock 14 violation; cf. SC-3 §2.3's "generic code … never
`match`es on a fact id") or a runtime callback into per-grammar logic
(consumer scope creep). The proof's job is to *bound* the contract, not
furnish a finished retained parser. Method addition is deferred to whichever
future wave reopens the union substrate; this is recorded under §5.

### §1.4 Witnesses — what `impl` looks like

The JSON witness:

```rust
// skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs
//! Compile-only witness; not a production parser. Excluded from the
//! release library by `#[cfg(any(test, feature = "proof"))]` on the
//! parent `pub mod` declaration in lib.rs.

use crate::tape::event_grammar::EventGrammar;

pub struct JsonEventGrammar;

#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct JsonFactId(u8);

impl JsonFactId {
    pub const STRING_ESCAPE_OR_CONTROL: Self = Self(0); // SC-3 §2.3 json.fact.0
}

impl EventGrammar for JsonEventGrammar {
    const STRUCTURAL_CLASS_COUNT: u8 = 7; // { } [ ] : , "
    type FactId = JsonFactId;
    fn admits_fact(id: Self::FactId) -> bool {
        matches!(id, JsonFactId::STRING_ESCAPE_OR_CONTROL)
    }
}
```

The CSS L4 *or* Sheets witness mirrors the same five-line shape. Picking
Sheets is recommended over CSS L4 because Sheets exercises the
`StructuralClassTable` edge case SC-6 §4.4 names (escape-equals-delimiter:
`""` denotes a literal quote). That edge case is the *strongest* Lock 14
exerciser available without writing a new scanner — the witness only needs
to declare `STRUCTURAL_CLASS_COUNT = 7` (`( ) , : { } "`) and admit a single
opaque fact id, and the proof carries Sheets' awkward escape rule on the
record.

The compile-only proof body is a single function in
`event_grammar_tests.rs`:

```rust
fn _proof_compiles<G: EventGrammar>(_tape: &Tape<'_>, _cursor: u32) {
    // Body is type-level only: ValueRef<'_, '_, G>::new is well-formed
    // for any G implementing the trait, and ValueRef's lifetime ordering
    // (see §2) holds for both witness instances.
}

const _: fn() = _proof_compiles::<JsonEventGrammar>;
const _: fn() = _proof_compiles::<SheetsEventGrammar>;
```

That `const _: fn() = …` pair *is* the proof. The compiler refusing to
emit either line is the proof's failure mode; passing them is the
acceptance verdict.

---

## §2 — `ValueRef` cursor semantics + lifetime

### §2.1 What it borrows; what it does *not* borrow

Today's `ValueRef<'doc, 'input: 'doc, K = AnyKind>` lives at
`skinny/crates/runtime/src/tape/mod.rs:171-217`. The field layout is:

```rust
pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind> {
    tape: &'doc Tape<'input>,
    cursor: u32,
    _kind: PhantomData<fn() -> K>,
    _input: PhantomData<&'input [u8]>,
}
```

Three properties that the proof preserves verbatim:

- **The cursor is a `u32`, not a pointer.** It is an ordinal into the
  retained tape's offset/class columns, exactly as SC-3 §2.2 specifies.
  Resolution to a byte position goes through `tape.offset_at(cursor)`; the
  cursor itself does not name a memory address. This is what permits cursor
  values to survive a tape *re-base* (the columns may move; the ordinals
  stay valid).

- **The borrow is of the *tape*, not the *source*.** `'doc` is the tape's
  lifetime; `'input` is the source bytes' lifetime; the constraint
  `'input: 'doc` says the source outlives the tape. The cursor never holds
  a `&'input [u8]` directly; resolution to source bytes happens *through*
  the tape via `tape.source()`. This is the load-bearing property for Lock
  1: the cursor is *transient over the substrate*, not a sidecar lifetime.

- **`PhantomData<fn() -> K>` is variance-correct contravariance.** The
  kind/grammar marker is consumed but never produced, so `ValueRef<…, Json>`
  is *not* assignable from `ValueRef<…, Sheets>` even though both are
  cursors into a tape of bytes. This is what blocks "smuggle a JSON cursor
  into a non-JSON consumer" cross-grammar leaks; the proof simply renames
  the kind marker from `K = AnyKind` to `G: EventGrammar = AnyGrammar` and
  the same variance discipline applies.

### §2.2 Lock-1 compliance

Lock 1 verbatim (`restart/locks/LOCKS.md:34`): "A SIMD mask stream is a
transient producer, not a retained sidecar; if structural offsets are
retained, the structural projection IS the tape." The `ValueRef` cursor
satisfies this because:

1. **It is not a substrate.** A `ValueRef<'tape, 'src, G>` is 4 + 8 + 0 =
   12 bytes on the stack (a `u32` cursor + `&Tape` reference + zero-sized
   `PhantomData`). It owns nothing. It cannot survive the tape; it cannot be
   `Send`-stored beyond the tape's scope; it cannot be cloned into a
   long-lived sidecar without simultaneously cloning the `&Tape` and
   landing in the same lifetime bracket. This is precisely the
   "no sidecar lifetime" property SC-3 §2.6 codifies.

2. **It is not a *second* artefact.** The cursor is *the cursor of the
   substrate*. Today's tape already has cursor identity (`(TapeId, cursor)`,
   `mod.rs:166-168`) and `JsonDocument` already keys retained-view methods
   on `ValueRef::cursor()` (`view.rs:71`, `:92`, `:189`). The proof does
   not introduce a new sidecar; it parameterises the existing cursor by a
   compile-time grammar marker.

3. **It is compatible with the SC-3 union representation.** When SC-3's
   `class` column lands (some future wave), `ValueRef<'tape, 'src, G>`
   resolves a class via `tape.class_at(cursor)` (analogous to today's
   `offset_at`). The cursor's lifetime, identity, and borrow shape do not
   change. The proof, in effect, says: *whatever the substrate's column
   schema becomes, the cursor abstraction admits it without growing a
   sidecar.* That guarantee is what the SK-V10+ union-substrate work needs
   before it dispatches.

### §2.3 What the cursor cannot do

The proof explicitly *forbids*:

- A `'static` cursor (would imply a sidecar). `'tape: 'tape` and
  `'src: 'tape` are constraints on the impl, not defaults.
- A `Vec<ValueRef<'_, '_, G>>` cursor pool. The borrow-checker rejects
  it because each cursor borrows `&'tape Tape<'src>`; storing many
  cursors does not create a sidecar — they *all* share the tape's
  lifetime.
- A cursor that *outlives* the source. The constraint `'src: 'tape` plus
  the tape's own `Tape<'src>` lifetime parameter make this a hard
  borrow-checker reject.

The proof writes a single negative compile test — a `#[should_compile_fail]`
witness (or a documented "uncomment to verify rejection" comment block,
because `compile_fail` doctests inside `tests/` are awkward) that attempts
to build a `ValueRef<'static, 'static, JsonEventGrammar>` against a
borrowed tape. The compiler's rejection is the third leg of the proof.

---

## §3 — Cross-grammar instance (non-JSON) — Lock 14 exerciser

The proof admits *one* non-JSON `EventGrammar` instance. SC-6 §4 enumerates
three candidates: CSS L4 (K = 10), Sheets (K ≈ 7), and BBNF-self
(K ≈ 13).

### §3.1 Recommended choice: Sheets

Sheets is the proof's strongest Lock 14 exerciser because:

- **Reused punctuation byte** — Sheets uses `:` as range operator, `,` as
  arg separator, *and* `:` as Object-style key/value separator in array
  literals. JSON also uses `:` and `,`. Without a generic role API in the
  generic substrate, the same byte must mean different things in the two
  grammars *purely by which witness owns the class table*. Sheets is the
  cleanest demonstration that opaque class ordinals carry that distinction
  without generic-crate branching.
- **Escape-equals-delimiter** — `""` in Sheets denotes a literal quote.
  The witness proves that this is *not* a `fact_id` admitted to the
  generic substrate; it stays inside the generated Sheets module's
  string-body decoder. The proof's `admits_fact` returns `false` for any
  Sheets-side escape fact id, demonstrating that even an awkward escape
  rule does not leak into the generic substrate.
- **No production parser** — there is no Sheets parser in `runtime/src/`
  today. A Sheets witness *cannot* accidentally land a production
  consumer; the proof shape is enforced by the absence.

### §3.2 What the Sheets witness contains

```rust
// skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs
//! Lock 14 witness for SK-V9 P2-B proof. Compile-only.
//! Excluded from release: `#[cfg(any(test, feature = "proof"))]`.

use crate::tape::event_grammar::EventGrammar;

pub struct SheetsEventGrammar;

#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct SheetsFactId(u8);

impl SheetsFactId {
    // Sheets reserves a single fact id for the doubled-quote escape rule;
    // it is admitted to the witness's `admits_fact` predicate but the
    // payload semantics live entirely inside a (future) generated
    // Sheets module. The generic substrate sees an opaque ordinal.
    pub const DOUBLED_QUOTE_ESCAPE: Self = Self(0);
}

impl EventGrammar for SheetsEventGrammar {
    const STRUCTURAL_CLASS_COUNT: u8 = 7; // ( ) , : { } "
    type FactId = SheetsFactId;
    fn admits_fact(id: Self::FactId) -> bool {
        matches!(id, SheetsFactId::DOUBLED_QUOTE_ESCAPE)
    }
}
```

That is the entire Sheets contribution. No scanner, no parser, no class
table data, no test fixtures. The trait `impl` *is* the Lock 14 witness; the
absence of generic-crate edits is the verification.

### §3.3 Lock 14 audit commands

The proof binds itself to these `rg` audits (SC-6 §4.6 idiom):

```sh
# No generic substrate names a grammar or grammar-side role.
rg -n 'Json|JSON|Sheets|sheets|Css|CSS' skinny/crates/runtime/src/tape/ skinny/crates/bbnf-simd/src/
# No generic public role enum or generic grammar API.
rg -n 'enum (Json|Sheets)Role|pub fn .* grammar:' skinny/crates/runtime/src/tape/
# Generic substrate stores opaque ordinals only.
rg -n 'fn admits_fact|fn admits_class' skinny/crates/runtime/src/tape/
# All hits must point at event_grammar.rs trait method bodies.
```

Each of those `rg` invocations is a verification surface for CH2 GENERALITY
of the S-P2 CHALLENGE wave. The proof passes Lock 14 iff every match is
inside `event_grammar.rs`, a witness file, or the proof test file — never
inside generic substrate source.

---

## §4 — Differential vs REDRESS 60-72

REDRESS 60-72 are the SK-V6 Wave 2/3 retained-parse candidate ledger. They
collectively constitute *the rejected class of designs the proof must not
be*. The differential is precise:

### §4.1 The rejected class — what REDRESS 60-72 share

| # | Route | Why rejected (one-line) |
|---:|---|---|
| 60 | Retained trusted-string boundary collapse (delete `match_tiny_plain_string` before `match_string_at_quote`) | Removed an early-out; lost 11–47% on every measured retained row. **Production parser path edit.** |
| 61 | AArch64 64-byte string-block scanner as same-wave consumer in `parse_string_plain_trusted` | Improved gsoc-2018 by 15.4% but regressed canada -9.8% and instruments -7.5%; failed advisory gate. **Production parser path edit.** |
| 62 | Delayed-wide retained trusted string scan (keep tiny probe, enter 64-byte after) | Similar profile; failed gate. **Production parser path edit.** |
| 63 | Array `ContainerNext` next-byte carry | *Admitted* (the only Wave 2 admission); did not close parse-G. **Production parser path edit.** |
| 64 | Retained Unicode-escape run validator | Failed gate. **Production parser path edit.** |
| 65 | Object next-key carry mirroring `ContainerNext` | Failed gate (+0.60% to -1.21% across rows). **Production parser path edit.** |
| 66 | Direct source-hook field-layout materializer | Failed gate; receiver/closure shape change. **Production parser path edit.** |
| 67 | Parser-owned decoded scratch in generated direct | -44% on `unicode_escapes`. **Production parser path edit.** |
| 68 | Byte-output `unescape_json_string` materialization | -4% on `unicode_escapes`. **Production parser path edit.** |
| 69 | `DirectBuild` semantic string field facts | -15% on `unicode_escapes`. **Production parser path edit.** |
| 70 | First `real_typed_struct` implementation as SOTA-close | 0.53× sonic-rs; replaced by REDRESS 71's schema-source contract. **Production parser path edit.** |
| 71 | Schema-source `DirectBuild` typed parser | *Admitted* — but it is a typed-output codegen path, not a retained-parse route. |
| 72 | (no entry under this number) | — |

The unifying defect of REDRESS 60-69: **every candidate edited the
production retained or direct parser, ran it, measured it, and was
falsified by a same-row throughput regression**. Each was a *runtime swap*,
not a contract proof. The "no `RESULTS.md` row movement at Alpha depth"
boundary in HANDOFF §3 is exactly the protection against that failure mode
recurring.

### §4.2 What this proof does instead

The differential is structural, not cosmetic:

1. **No production consumer.** REDRESS 60-69 each carried a same-wave
   production consumer (the retained or direct hot path itself). The proof
   carries none. The `event_grammar_witness.rs` files are gated behind
   `#[cfg(any(test, feature = "proof"))]`; nothing in `release` mode links
   them; `cargo bench -p bbnf-bench` cannot reach them. The same-wave
   consumer rule (Lock 1's discipline; ARCHITECTURE.md §9.1) does *not*
   apply to compile-only proof artefacts — see §5 for the formal argument.

2. **No measurement surface.** REDRESS 60-69 produced focused
   `profile-lazy` Mbps tables and `bench-json --advisory` matrices. The
   proof produces *no measurement*. Its verification is `cargo check
   -p runtime` and `cargo test -p runtime event_grammar -- --nocapture`
   (the latter only running the type-level `_proof_compiles` lines). The
   "row movement" surface that REDRESS 60-69 each tripped is structurally
   absent.

3. **No edit to `generated.rs`, `scan.rs`, `parser.rs`, `view.rs`, or any
   codegen template.** REDRESS 60-72 each touched one or more of those
   files. The proof's owner-files table (§1.2) lists *only* new files
   under `runtime/src/tape/` and `runtime/src/grammars/*_witness/`, plus a
   five-line `lib.rs` re-export under a `cfg`. The clean-regen discipline
   (`feedback_generated_files_clean_regen`) is preserved by *not editing
   generated files*.

4. **No new BIR variant, directive, `BackendShape`, or substrate-API
   addition.** REDRESS 71 added a `DirectBuild` payload field (admitted);
   REDRESS 92 explicitly forbade reopening that family of changes for W3.
   The proof adds *one trait* in a runtime sub-crate; the trait's contract
   is data tables, not a new IR surface. SC-3 §5.2 Tier B owns the
   substrate-API expansion when the time comes; the proof does not pre-bind
   it.

5. **The cursor is unchanged.** REDRESS 60-65 each modified parser control
   flow that consumed cursors; REDRESS 67-69 each modified how decoded
   bytes flowed through parser-owned state. The proof does not modify
   `ValueRef`'s field layout (the rename from `K = AnyKind` to
   `G: EventGrammar = AnyGrammar` is a *renaming of a `PhantomData` marker
   type*, not a layout change). The 12-byte stack footprint is preserved;
   the borrow shape is preserved; the production retained-view consumers
   at `view.rs:25`, `:71`, `:189` continue to compile against the
   `AnyGrammar` default.

### §4.3 REDRESS 92 — the direct antecedent

REDRESS 92 (W3 Tape Plus Structural-Projection) is the immediate parent
rejection. Its closing sentence: "The routed SK-V9/Pass Omega precursor is
to *define the retained class/event grammar including numbers/literals and
string quote ownership, prove the retained `ValueRef` cursor contract over
that grammar, and only then reopen a measured structural-heavy parse row
wave*." This proof is the response to that exact sentence — the *define*
and *prove*, with the *reopen* explicitly out of scope. The HANDOFF
boundary "no `RESULTS.md` row movement at Alpha depth" enforces the
"only then" clause.

---

## §5 — What this proof unlocks

The proof, once admitted, removes exactly one pre-block from the SK-V9
HANDOFF §5 ledger: "W3 structural implementation without retained class/
event grammar plus retained `ValueRef` cursor proof." That admission is
the *necessary* gate before a future tranche may reopen the SC-3 union
substrate as a measured-row implementation candidate.

It does *not* unlock:

- Tier A SC-3 Tier A migration. That still requires its own S-P3 plan with
  same-wave production consumer (generated JSON Track 1 retained parse +
  retained view), measured row gates, and REDRESS 50 falsification proof
  per SC-3 §5.1. The proof furnishes the contract; the migration furnishes
  the production rewire.
- Apache/CITM measured-row admission. That is REDRESS 91's residual,
  routed through a separate row-table wave (HANDOFF §3 row 1).
- Direct output/control-path contract. That is REDRESS 93's residual,
  routed through a separate direct-contract wave (HANDOFF §3 row 3).
- The Lock 1 SC-6-L1-R1 refinement amendment. That is Pass Omega's owner
  (SC-6 §3) and the proof does not pre-bind it; it can be ratified before,
  after, or independently of the proof's admission.

The unlock geometry, stated as a graph:

```
Proof (this artefact, SK-V9 S-P2 ACCEPT)
  ─► removes pre-block "W3 structural implementation without retained
     class/event grammar plus retained `ValueRef` cursor proof"
  ─► makes SC-3 Tier A migration *eligible to dispatch* in a future
     SK-V10+ wave, subject to its own S-P3 plan, same-wave consumer,
     and challenge gate
  ─► does NOT bind: Pass Omega's SC-6-L1-R1 refinement, REDRESS 91
     row admission, REDRESS 93 direct contract, REDRESS 50 aux-table
     row-falsifier
```

The proof's existence is the *necessary* but *not sufficient* condition
for SC-3 Tier A. That asymmetry is what makes it admissible at proof-only
depth: it cannot be misread as W3 dispatch, because it carries none of W3's
production-consumer commitments.

### §5.1 Same-wave-consumer rule — formal disposition

The same-wave-consumer rule (Lock 1; SC-3 §5.1; HANDOFF §3 "same-wave
production consumer") states: a substrate change must land with its
production consumer in the same wave. The proof is *not* a substrate
change. It is a *trait declaration plus three witness `impl`s*. None of the
witnesses claims to be a production parser; the JSON witness sits beside
`generated.rs` as a sibling proof file, not as the production retained
parser. The Sheets witness has no production-parser counterpart anywhere in
the workspace. The CSS L4 alternative would have the same property.

The same-wave-consumer rule applies to artefacts that *land in the parse
hot path*. The proof's artefacts are gated behind `#[cfg(any(test, feature
= "proof"))]` and excluded from `cargo bench -p bbnf-bench`; they cannot
be observed by any production caller. CH5 HIDDEN COUPLING — "does any
candidate introduce a parallel substrate, a sidecar producer, a renamed
scanner, or a Track 1 ≡ Track 2 dishonesty?" — verdicts ACCEPT for the
proof because it introduces none of those: no substrate, no producer, no
scanner, no Track-2 surface, and the proof carries an explicit `rg` check
that `event_grammar_witness.rs` files are never referenced from any
non-`cfg(test, feature = "proof")` source path.

This is the formal answer to "a proof has no production consumer; verify
that this does NOT violate the same-wave-consumer rule": **the rule
binds substrates, not contracts; the proof is a contract, not a
substrate; therefore the rule is silent**, and the proof's admission is
not blocked on furnishing a same-wave production consumer that, by design,
does not exist for compile-only artefacts.

---

## §6 — LOC + risk envelope

### §6.1 LOC budget

| Item | LOC (source) |
|---|---:|
| `tape/event_grammar.rs` (trait + cursor `PhantomData` rename) | ~110 |
| `tape/event_grammar_tests.rs` (type-level proof functions) | ~80 |
| `grammars/json/event_grammar_witness.rs` | ~120 |
| `grammars/sheets_witness/event_grammar_witness.rs` (or CSS L4) | ~80 |
| `runtime/src/lib.rs` `cfg`-gated re-exports | ~5 |
| **Total source** | **~395** |

Margin: 55 LOC inside the HANDOFF 450 LOC envelope. The envelope is *not*
expanded by Lock 14 audit additions (the `rg` commands are documentation,
not source) or by the proof artefact (this Markdown file, which is
research, not source).

Generated-output LOC: **0**. The witnesses are *not* generated files. The
clean-regen discipline is preserved by structural separation: no codegen
template emits a `*_witness.rs`, and no `cargo xtask check-json`/
`check-real-typed`/`check-conformance` invocation runs against the
witnesses.

### §6.2 Risk envelope

| # | Risk | Mitigation |
|---:|---|---|
| R1 | The proof reads as a "substrate API addition" because `EventGrammar` is a new public trait. | The trait is in `runtime/src/tape/` under a `#[cfg(any(test, feature = "proof"))]` gate; it has no production caller; SC-3 §2.6 ("no clone path, cache path, attach-after-build path, parser-owned cursor, or post-build API") forbids exactly the kind of expansion that would make the trait a true API. The proof's admission must include an `rg` check that the trait has zero non-proof callers. |
| R2 | The witness files in `grammars/*_witness/` look like the start of per-grammar runtime modules — Lock 14's *hand-written per-grammar runtime files are forbidden* clause. | The `_witness` suffix in the directory name is deliberate and load-bearing; the proof binds itself to the rule that `runtime/src/grammars/<G>_witness/` exists *only* for `EventGrammar` proof witnesses and never gains a `scan.rs`, `parser.rs`, `generated.rs`, or `view.rs` sibling. CH2 GENERALITY audit: `find skinny/crates/runtime/src/grammars/sheets_witness -mindepth 1 -maxdepth 1` returns only `event_grammar_witness.rs` and `mod.rs`. |
| R3 | A future agent may reopen the proof as a runtime parser swap path, treating "the trait compiles" as license to dispatch SC-3 Tier A. | §5 explicitly states the proof is necessary but not sufficient for Tier A. The HANDOFF "no row movement at Alpha depth" boundary is a top-level fence; the proof itself adds the binding sentence "this proof's admission is not authority to dispatch a structural-heavy parse implementation" to its acceptance verdict. |
| R4 | `cargo bench` accidentally picks up the witness files because of a Cargo feature drift. | Two layers of defence: (1) the `#[cfg(any(test, feature = "proof"))]` gate on the parent `pub mod` in `lib.rs`; (2) no `bbnf-bench` benchmark imports `event_grammar`. Verification: `rg 'event_grammar|event_grammar_witness' skinny/crates/bbnf-bench/` returns zero. |
| R5 | The proof reads as a renamed REDRESS 71 schema-source contract (the admitted typed-output route), confusing reviewers about whether it is a re-open. | REDRESS 71 admitted a *codegen-time schema-source for typed-output `DirectBuild`*; it is a generated-direct-parser path. The proof concerns the *retained tape's class/event contract*, which is a separate substrate axis (REDRESS 71 sits on the SinkOnly direct lowerer; the proof sits on the OffsetTape retained lowerer). The proof's `EventGrammar::admits_fact` matrix is the SC-3 §2.3 retained-fact matrix, not REDRESS 71's `DirectBuildField` payload. The two are orthogonal. |
| R6 | The proof admits Sheets as a witness but Sheets has no `.bbnf` grammar source in the workspace today. | The witness does not require a Sheets grammar source; it requires only a Rust trait `impl`. The proof explicitly states this and records the absence as *strengthening* the Lock 14 demonstration: the witness compiles even though no Sheets grammar source exists in the workspace, *because* the trait carries no `match grammar` arm. If a reviewer prefers a witness against an extant grammar source, CSS L4 is the substitute; the proof's text supports either choice. |

### §6.3 Time envelope

HANDOFF binds ≤90 min implementation/redress. The proof breakdown:

- Trait declaration + `ValueRef` `PhantomData` rename: ~15 min.
- JSON witness: ~10 min.
- Sheets (or CSS L4) witness: ~10 min.
- Type-level proof tests: ~15 min.
- `cargo check -p runtime` + `cargo test -p runtime event_grammar`: ~5 min.
- `rg` Lock 14 audits + commit message: ~10 min.
- Buffer for borrow-checker errors on the variance discipline: ~25 min.

Total: ~90 min, sized to fit the cap without overrun.

---

## §7 — Sources

- `restart/skinny/tranches/sk-v9/HANDOFF.md:64-90` — candidate boundary,
  "Retained class/event grammar and `ValueRef` cursor proof"; 450 LOC,
  ≤90 min, "Proof-only; no `RESULTS.md` row movement at Alpha depth".
- `restart/skinny/tranches/sk-v9/HANDOFF.md:105-130` — pre-block ledger,
  including "W3 structural implementation without retained class/event
  grammar plus retained `ValueRef` cursor proof".
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
  — S-P1 convergence; the "real PMU table" hand-off naming `consume_structural`
  as the 0.00% self-time dead leaf and the string-scanner pair-dominance.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md`
  — §2.2 data layout, §2.3 fact admission matrix, §2.6 Lock-1 no-parallel-
  substrate argument, §5.1 Tier A owner table, §5.3 admission posture; the
  retained-class column and opaque-fact-id discipline this proof binds itself
  to.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`
  — §1 Lock 1 + Lock 14 verbatim; §3 SC-6-L1-R1 refinement (Pass Omega
  candidate); §4 StructuralAlphabet abstraction (JSON / CSS L4 / Sheets /
  BBNF-self instances).
- `skinny/REDRESS.md:1344-1985` — Items 60-72, the SK-V6 Wave 2/3 retained-
  parse + direct-output candidate ledger; specifically Items 60-65 (retained
  parser-control routes) and 66-69 (direct-string materialization routes).
- `skinny/REDRESS.md:2661-2690` — Item 92, the SK-V8 W3 rejection that
  routed the SK-V9/Pass Omega precursor to "define the retained class/event
  grammar […] and prove the retained `ValueRef` cursor contract over that
  grammar, and only then reopen a measured structural-heavy parse row wave".
- `skinny/crates/runtime/src/tape/mod.rs:90-97` — current `Tape<'input>`
  struct field layout (`offsets`, `flag_cursors`, `flag_values`, `payloads`,
  `id`).
- `skinny/crates/runtime/src/tape/mod.rs:171-217` — current
  `ValueRef<'doc, 'input: 'doc, K = AnyKind>` definition and
  `offset()`/`cursor()`/`tape()` methods; the cursor-borrows-tape lifetime
  discipline.
- `skinny/crates/runtime/src/grammars/json/view.rs:6-71` — `JsonDocument`
  + `DocumentView<'input>` impl; how the production retained view consumes
  `ValueRef::new(&tape, cursor)` without naming class identity or
  fact-id semantics — the consumer pattern the proof preserves.
- `skinny/crates/runtime/src/grammars/json/generated.rs:14-17` — current
  `attach_structural_index` no-op (the discard-the-SIMD-output defect SC-3
  §1.2 names; the proof does *not* repair this, it only proves the
  retained-grammar contract that a future repair could rest on).
- `skinny/crates/runtime/src/grammars/json/scan.rs:6-20` —
  `STRUCTURAL_BYTES = b"{}[],:\""` and `STRUCTURAL_CLASS_TABLE_LO6`; the
  current JSON `StructuralAlphabet` instance the witness mirrors.
- `skinny/crates/codegen/src/lower/event_tape.rs:1-17` — current 17-line
  placeholder `EventTape` lowerer that the proof does *not* fill; SC-3
  §5.2 Tier B owns the fill, post-proof.
- `restart/locks/LOCKS.md:34` — Lock 1 verbatim (substrate union;
  no-parallel-substrate spirit; "if structural offsets are retained, the
  structural projection IS the tape"; 2026-05-04 reframe).
- `restart/locks/LOCKS.md:60` — Lock 14 verbatim (full grammar
  generalisation; zero `match grammar` arms in generic crates; hand-written
  per-grammar runtime files forbidden; verification commands).
- `restart/locks/LOCKS.md:69-94` — Lock 16 verbatim (SIMD/ASM admissibility
  allowlist; the primitives the proof's class-table column would eventually
  be consumed by, but does *not* invoke at proof depth).
- `restart/prompts/skinny/PASS-2-RESEARCH.md` — S-P2 frontmatter schema (§2.1),
  six-lens CHALLENGE (§3, especially CH2 GENERALITY and CH5 HIDDEN
  COUPLING), grammar-neutral abstraction discipline (§8.5).
- `restart/ARCHITECTURE.md` §7.3 lines 1060-1098 — `BackendShape` enum +
  `derive_backend_shape` algorithm; the proof binds itself to *not*
  introducing a sixth `BackendShape` variant.
- Feedback memory: `feedback_generated_files_clean_regen` — generated files
  are always output of fresh regen; the proof's separation of witness
  files from generated files preserves this.
- Feedback memory: `feedback_no_inline_tests` — all tests in `tests/`
  directory; the proof's `event_grammar_tests.rs` is a `tests/` artefact,
  not an inline `#[cfg(test)] mod tests` block.
- Feedback memory: `feedback_typed_materialization_invariant` — every `->`
  in the grammar must reach the tape emitter; the proof's `EventGrammar`
  trait, with `admits_fact` as a compile-time predicate, is the contract
  under which a future retained tape preserves that invariant per
  grammar.
