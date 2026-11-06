# PROTO-PB-3b — THE SHEETS-LANE PROTOTYPE (the pass-1 blocker, settled)

Analysis instrument, never landed. Repo: bbnf-lang (scratch crate). Date: 2026-07-10.
Scratch crate: `/private/tmp/pb3b-scratch` (471 LOC, `src/main.rs`); dependency-free;
`cargo run` exits 0, **all 13 sheets-lane parity checks PASS**, warnings only (all disclosed
below and load-bearing). Built on the PROTO-PB-3 seam: the `tape` mirror and the whole
`value_plane` (NodeClass + two-method `Classify`) are carried verbatim; this instrument adds
the faithful sheets leaf-tape recorder, the native `sheets/view.rs` oracle, and the two
attempts to carry sheets through the neutral surface.

## Verdict

**SHEETS = CARRY.** The neutral `NodeClass`/`Classify` surface does **not** carry the sheets
tape. The question PB-3 gated is now settled by executed byte-parity: (a) the sheets tape has
**zero** Enter/Leave events for the neutral container alphabet to represent — so the
"Enter/Leave events over the sheets tape" the charter asks about **do not exist**; (b) the
pure two-method `Classify` **collapses** the live `rule_kind` navigation (byte-divergent from
the oracle); (c) byte-identical parity is recoverable **only** by adding a `NativeTag` escape
hatch that passes the raw 39-kind ordinal across the seam — which *is* the flattening PB-3's
"no enum crosses" thesis disavows, and reduces to re-exposing the native path. This confirms
CRIT-PB-3 refinement (1) with running evidence in place of a source read, and vindicates the
`tape/cursor.rs:8-19` disclaimer rather than answering it.

## The tape SHAPE — flatter than "a 39-kind sweep" (the decisive fact)

CRIT-PB-3 called sheets "a flat linear sweep over 39 kinds." The generated recognizer is
flatter still. Across `grammars/sheets/generated.rs` there are **39** `let _ = (Nu8, &mark);`
references that are **no-ops** (they name the tag and the `mark` fn but never call it),
exactly **one** live `mark` (`generated.rs:225`, the string closer), and **12** live
`match_leaf` sites. The structural rules record **nothing**: `parse_add_expr:788` is
`let _ = (16u8,&mark)` then descent (generated.rs:795); `parse_func_call:1315` is
`let _ = (29u8,&mark)` then descent + `match_literal(")")` (generated.rs:1322,1336). Operators
go through `match_literal` (`parse_add_op:762`, `parse_compare_op:595`) which records nothing.
Only **leaf** rules seal a `(offset, tag)` pair (`match_leaf` → `push_offset(start,
OffsetFlags(kind))`, generated.rs:161).

Executed proof (PART 1). Recording `=SUM(1,2)` through the verbatim-ported `match_leaf`
yields exactly four nodes:

```
nodes() rule_kind = [Some(Formula), Some(Identifier), Some(Number), Some(Number)]
                       (=)            (SUM)             (1)           (2)
```

`FuncCall` / `FuncArgs` / `Arg` / `Comma` / `Expression` / `Primary` appear **nowhere** on the
tape. The `GoogleSheetsKind` names (`view.rs:8-48`) are 39, but for these fixtures only
Formula/Identifier/Number/Boolean ever record; the rich structure (which function, operator
precedence, nesting) is not on the tape at all — it is recovered only by re-inspecting source
spans. There are no container boundaries, hence **no Enter/Leave events to carry**.

## The neutral vocabulary is structurally dead for sheets (compiler-confirmed)

`NodeClass = Enter(Shape) | Leave(Shape) | Scalar(ScalarClass)` (carried from PB-3). Projecting
sheets into it:

```rust
impl Classify for SheetsClass {
    fn classify(tape: &Tape, cursor: u32) -> NodeClass {
        match GoogleSheetsKind::at_cursor(tape, cursor) {
            Some(GoogleSheetsKind::Number)  => NodeClass::Scalar(ScalarClass::Float),
            Some(GoogleSheetsKind::Boolean) => NodeClass::Scalar(ScalarClass::Bool),
            Some(GoogleSheetsKind::String)  => NodeClass::Scalar(ScalarClass::Str),
            // Identifier, CellRef, SheetPrefix, Formula, operators, ... ALL collapse here.
            // Enter/Leave are UNREACHABLE: the sheets tape records no container boundaries.
            _ => NodeClass::Scalar(ScalarClass::Raw),
        }
    }
    // scalar_span = [offset(cursor) .. offset(cursor+1)) — the leaf span.
}
```

Two confirmations that the container half is dead here. Executed: the PART-1 assertion
`(0..n).any(|c| matches!(classify, Enter|Leave)) == false` passes — no cursor on the sheets
tape ever classifies as a container. And the compiler agrees: `cargo run` reports
`variants Enter and Leave are never constructed`, `variants Map and Seq are never constructed`
— the neutral surface's entire tree-navigation apparatus (`entries`/`items`/`path`/
`next_sibling`, all keyed on `Leave` termination and depth counting) has **no reachable input**
for sheets. The surface compiles; it just describes nothing sheets records.

## The COLLAPSE — pure `Classify` breaks the live `rule_kind` navigation

The live consumer navigates by distinct kind (`sheets_conformance.rs:90-91`:
`document.nodes().find(|node| node.rule_kind() == Some(kind))`). `NodeClass`'s 8 classes cannot
carry 39 kinds, and the collapse is not academic — it changes which node is found. Executed
(PART 3) on `=SUM(1,2)`:

```
native  find(rule_kind == Identifier) -> node at offset 1  ("SUM")
neutral find_by_class(Scalar(Raw))    -> node at offset 0  ("=", a Formula, NOT an Identifier)
assert_ne!(Some(1), Some(0))  // PROVEN byte-divergent: Identifier and Formula both -> Raw
```

`find(rule_kind == CellRef)`, `find(rule_kind == SheetPrefix)`, `find(rule_kind == Formula)`,
`find(rule_kind == Identifier)` are four distinct live navigations that all project to the same
`Scalar(Raw)` — the neutral surface cannot tell them apart, and returns the first Raw for all.
The scalar *reads* (`as_f64`/`as_bool`) survive only because Number→Float and Boolean→Bool are
1:1; the *navigation* the disclaimer is about does not.

## Byte parity is recoverable ONLY by re-importing the native ordinal

The only construction that reproduces the oracle is CRIT-PB-3 refinement (2): extend the seam
with a native-tag escape.

```rust
pub trait NativeTag: Classify { fn tag(tape: &Tape, cursor: u32) -> u16; }
impl NativeTag for SheetsClass {
    fn tag(tape: &Tape, cursor: u32) -> u16 {          // the raw 1-based rule ordinal —
        tape.flags_at(cursor).map(|t| t as u16).unwrap_or(0)   // the WHOLE 39-kind space
    }
}
```

Executed (PART 2), across all five conformance fixtures `=1 =3.5 =TRUE =FALSE =SUM(1,2)`:

```
native  : nodes().find(rule_kind==Number).map(|n| (n.offset(), n.as_f64()))
hatch   : find_by_tag(tag==1).map(|c| (offset(c), scalar_text(c).parse::<f64>()))
assert_eq!(native, hatch)   // byte-identical (offset AND value) for every fixture
```

and `find_by_tag(tag==Identifier)` on `=SUM(1,2)` recovers offset 1 — parity restored. But
`tag` returns `0..=39`: the full `GoogleSheetsKind` ordinal crosses the seam verbatim. To
interpret it a consumer must hold the 39-name `KIND_TABLE` (`view.rs:52-92`) — the native
vocabulary, re-imported. The "neutral" plane is bypassed: `tag` + `KIND_TABLE` **is**
`rule_kind`. No sharing, no simplification, no common navigation (the sweep is still
grammar-specific find-by-ordinal, never `entries`/`items`/`path`). The escape hatch does not
carry sheets onto the neutral plane; it reintroduces the native plane through a hole in it.

## Method honesty

- **Faithful tape, not a transcribed literal** (answers CRIT-PB-3 overstatement-A). `match_leaf`
  / `match_literal` / `skip_ws` / `consume_quoted` / `is_text_byte` are ported **verbatim** from
  `generated.rs:69-190`; offsets are **computed** by scanning the source, then asserted against
  the native oracle's own `offset()`. Each fixture's leaf schedule is read directly out of the
  generated descent (formula→number, func_open→identifier, etc.).
- **Oracle is the real surface.** `GoogleSheetsKind` (39) + `KIND_TABLE` + `rule_kind`/`span`/
  `as_f64`/`as_bool`/`nodes()` reproduce `sheets/view.rs` line-for-line; parity is measured
  against it, not against a hand literal.
- **Parity is the criterion; perf is N/A.** Both surfaces are the identical O(n) flat sweep —
  the escape hatch *is* the native read — so there is no distinct hot loop to time; at 2–4 nodes
  any number would be noise. No timing is claimed (loadavg beside for the runs: 9.35 / 8.42, the
  box is busy; irrelevant to a parity verdict). This mirrors the PB-3 discipline (buildability
  sketch, no benchmark).
- **Warnings are the finding.** The disclosed dead-code warnings (`Enter`/`Leave`/`Map`/`Seq`/
  `Int`/`Null` never constructed) are not sloppiness — they are the compiler certifying that the
  neutral container alphabet has no sheets input.

## Bearing on the codex (TB4.W2)

1. **Mark `sheets/view.rs` (279 LOC) CARRY, not retire.** The −1,059 retirement drops to
   **−780**, JSON-only: `json/sink.rs`(120) + `json/view.rs`(488) + `json/value.rs`(172). This
   makes the close gate consistent: the gate's differential oracle names only the JSON corpus
   (CRIT-PB-3 §3.2), and the retirement now matches the proof surface exactly.
2. **State the delineation honestly in the SPEC.** The neutral surface serves **keyed/indexed
   value trees** (JSON, and CSS declaration values, which are data-tree-shaped). **Flat-tape,
   rich-`rule_kind` grammars (sheets) keep their native `GoogleSheetsNode` sweep at full
   fidelity.** This is the `tape/cursor.rs:8-19` disclaimer, vindicated: "a flat-tape grammar
   keeps its node sweep … the navigation each callsite needs is not on the shared seam."
3. **Do NOT adopt the `NativeTag` escape** as the general seam. It buys sheets parity only by
   putting the 39-ordinal back on the seam (weakening the "no enum crosses" invariant) and would
   require a sheets differential oracle the codex does not currently have. CARRY is strictly
   cleaner: no new seam surface, no new gate, the live `sheets_conformance.rs` consumer keeps
   working unchanged.
4. **The consumer-invariant gate is satisfied by CARRY, violated by retire.**
   `sheets_conformance.rs` reads `rule_kind`; retiring `sheets/view.rs` behind a `NodeClass`
   surface that cannot carry `rule_kind` breaks it (CRIT-PB-3 §3.3). CARRY keeps the live
   consumer green.

## Caveats

1. **Fixture set is the tower-core + typed-leaf subset** (`=1 =3.5 =TRUE =FALSE =SUM(1,2)`),
   sufficient to exhibit both a scalar-surviving kind (Number/Boolean) and a collapsing kind
   pair (Identifier/Formula → Raw). Deferred leaves (`cell_ref`/`range`, AU.6.7) are not
   exercised; they would only add more kinds into the same `Scalar(Raw)` bucket, deepening the
   collapse, never relieving it.
2. **The recorder drives leaf schedules, not the full 39-rule descent.** The recording
   *primitive* is verbatim and offsets are computed and oracle-checked, so the sealed tape is
   faithful; the descent *order* is transcribed from reading the generated code. A full port
   would seal an identical tape (structural rules are no-ops) at no change to the verdict.
