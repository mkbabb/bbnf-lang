# AZ-II.cutover.I - BBNF Compact Source Serializer
**Opens after**: AZ-II.cutover.H partial close
**Agents**: up to 10 parallel
**Hard gate**: `BbnfBootstrap::serialize_compact_doc` lands and `bbnf_rule` roundtrip is unignored; unresolved fleet activation blockers are documented.
**Status**: complete_with_misses

## Scope

1. Author a typed compact-source serializer over `BbnfDocument`.
2. Materialize structural literals such as `=`, `;`, `|`, `->`, and
   directive keywords from typed compound kinds, not span scraping.
3. Unignore `bbnf_rule` serialize roundtrip.
4. Preserve bootstrap reproducibility.
5. Diagnose the non-BBNF fleet activation blockers: typed-leaf source
   recovery and open-compound Err-frame leakage.
6. Write cutover.I partial-close audit and update FINAL.

## File bounds

| File | Access |
|---|---|
| `crates/core/src/runtime/bbnf/serialize.rs` | create |
| `crates/core/src/runtime/bbnf/mod.rs` | modify |
| `crates/core/tests/serialize_roundtrip.rs` | modify |
| `docs/tranches/AZ-II/audit/cutover.I-PARTIAL.md` | create |
| `docs/tranches/AZ-II/FINAL.md` | modify |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |

**Do NOT touch**: non-BBNF resolver activation, all-shape transparent
substrate activation, `Parsed<R>` deletion, `crates/tape/` deletion.
Deployment invariant: serializer and diagnostics use fully-contained
worktrees; blocker probes land only as evidence or named follow-on
scope.

## Phase sub-items

### AZ-II.cutover.I.1 Compact Serializer

Mechanism: implement `serialize_compact_doc(doc: &BbnfDocument<'_>)`
as a typed walker over BBNF compound kinds.

Files touched: `crates/core/src/runtime/bbnf/serialize.rs`,
`crates/core/src/runtime/bbnf/mod.rs`.

Sub-gate: serializer emits required structural literals from grammar
structure.

### AZ-II.cutover.I.2 Roundtrip Test

Mechanism: unignore `bbnf_rule` and route through compact serialization.

Files touched: `crates/core/tests/serialize_roundtrip.rs`.

Sub-gate: `bbnf_rule` passes and remains unignored.

### AZ-II.cutover.I.3 Reproducibility Check

Mechanism: run serialize roundtrip and bootstrap reproducibility tests.

Files touched: no source edits.

Sub-gate: 19/19 serialize roundtrip and reproducibility tests pass.

### AZ-II.cutover.I.4 Fleet Activation Diagnosis

Mechanism: probe non-BBNF activation and document blocker classes.

Files touched: `docs/tranches/AZ-II/audit/cutover.I-PARTIAL.md`.

Sub-gate: typed-leaf recovery and Err-frame leakage have follow-on
owners.

### AZ-II.cutover.I.5 Partial Close Update

Mechanism: update FINAL/PROGRESS with the landed serializer and the
remaining blockers.

Files touched: `docs/tranches/AZ-II/FINAL.md`,
`docs/tranches/AZ-II/PROGRESS.md`.

Sub-gate: no document claims fleet activation is complete.

## Hard gate

1. `serialize_compact_doc` exists and is exported.
2. `bbnf_rule` serialize roundtrip is unignored and passing.
3. Bootstrap reproducibility remains green.
4. Fleet activation blockers are named with destinations.

## Verification artefacts

- Commits `a128529a`, `c7e5999b`.
- `docs/tranches/AZ-II/audit/cutover.I-PARTIAL.md`.
- Focused serialize/reproducibility test logs cited in PROGRESS.

## Dependencies

- **Depends on**: AZ-II.cutover.H
- **Blocks**: AZ-II.cutover.K

## Archaeology

cutover.H showed span-range serialization drops required grammar
literals. cutover.I fixes that at the typed document layer instead of
adding source-text shims.
