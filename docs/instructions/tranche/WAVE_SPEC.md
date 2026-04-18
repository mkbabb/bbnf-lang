# Wave Specification — Orchestrator Sub-Document Format

Normative rules for authoring per-wave specification documents at
`docs/tranches/{LETTER}/waves/W<N>.md`. Every wave carries its own
spec; the tranche's parent `{LETTER}.md` is the index; the per-wave
spec is the orchestrator's dispatch input.

## When per-wave specs are required

A tranche MUST break out per-wave specs when any of:
- ≥ 6 waves.
- ≥ 4 parallel agents in any wave.
- Wave file-bounds exceed 5 distinct paths.

Smaller tranches MAY embed wave detail in the parent. The parent
always retains invariants, operational posture, wave summary table,
and handoff contract; all per-wave detail moves to the specs.

## File location + naming

`docs/tranches/{LETTER}/waves/W<N>.md` where `<N>` is the wave label:
`W0a`, `W0b`, `W0c`, `W1`, `W2`, … , `W15`. Letters after digits
denote serially-dependent sub-waves within a larger wave (`W0a` must
close before `W0b` opens).

## Required sections — exactly these, in this order

Every wave spec contains these sections. Add no others. Omit none
(except §9 Archaeology when the wave is genuinely novel).

### 1. Header

`# {LETTER}.W<N> — <Title>`

Single-line title. The title names the architectural change, not the
scope list. Good: "Gate Repair + Routing", "E-Graph Universal
Rewrites G1-G4". Bad: "Ship thing-1 + thing-2 + thing-3".

### 2. Meta block

Immediately after the header, exactly three lines:

```
**Opens after**: <prior wave label, or "tranche open">
**Agents**: <count> <serial | parallel>
**Hard gate**: <one-line summary>
```

The hard-gate summary is a one-line reduction of §6 Hard gate. If it
cannot reduce to one line, the wave's scope is too broad — split it.

### 3. Scope

Bulleted numbered sub-items, each naming a concrete change. Every
bullet is substrate + its consumer OR a concrete deletion manifest
entry. No speculative items. No "if time allows" items.

### 4. File bounds

A table listing every file the wave touches:

| File | Access |
|---|---|
| `crates/foo/src/bar.rs` | modify |
| `crates/foo/src/baz.rs` | create |
| `crates/foo/src/old.rs` | delete |
| `crates/foo/src/driver.rs` | modify-carve |

Access values:
- **create** — file does not exist today; wave writes it.
- **modify** — existing file; wave adds/changes lines.
- **modify-carve** — existing file loses substantial code; use when
  >30% of file deletes.
- **delete** — entire file removes.

Follow the table with an explicit **"Do NOT touch: <list>"** line
naming files OUT of wave scope that an agent might be tempted to
touch. Prevents scope creep during execution.

### 5. Phase sub-items

`### {LETTER}.W<N>.<x> <Title>` subsections. One per sub-item
dispatchable to an agent. Each contains:
- Mechanism (the concrete change — pseudocode, type signatures, or
  prose as fits).
- Files touched (subset of §4 File bounds).
- Sub-gate (wave-close-blocker for this sub-phase).

Sub-items are the orchestrator's agent-dispatch units. Parallelizable
when their file-bounds don't overlap; serial chain when dependent.

### 6. Hard gate

Numbered list of measurable conditions. Each item closes on a
specific verification artefact:

- `nm` symbol presence / absence (`nm target/release/deps/<bench> | grep <sym>`)
- `cargo asm` specific instruction present (`cargo asm -p <crate> <fn>`)
- Wire-contract test pass (specific test name)
- Samply self-time citation (≥ 1% for activation claims; specific profile path)
- Bench entry delta (specific ratio vs specific prior checkpoint)
- File existence + line-count delta (`wc -l` evidence of deletion/creation)

**Prohibited gate forms:**
- Narrative gates ("ledger documents intent").
- Substrate-exists-as-scaffold gates ("consumer wired in follow-on").
- Grep-only gates for source patterns ("symbol name present in file").

Every hard-gate item MUST trace to a verification artefact path in §7.

### 7. Verification artefacts

Bulleted list of concrete outputs the orchestrator saves at wave
close:

- `nm` outputs committed to `docs/benchmarks/post-{L}-W<N>-nm.txt`.
- `cargo asm` output archived per claimed lever.
- Samply profile paths under `.profiles/samply/{L}-W<N>/<bench>/`.
- Bench checkpoints: `docs/benchmarks/post-{L}-W<N>-{mid,close}.json`
  (mid-wave bench per `docs/instructions/tranche/SPEC.md` §Bench contract).
- Wire-contract test pass logs.
- Commit hashes for each milestone.

Every §6 hard-gate item cites at least one §7 artefact.

### 8. Dependencies

Exactly two lines:

- **Depends on**: <prior waves that must close first>
- **Blocks**: <subsequent waves gated on this one>

### 9. Archaeology (optional — include when applicable)

When the wave revisits a prior-attempted mechanism, cite:
- Prior-attempt commits + tranche letter.
- Named failure mode per predecessor's retro.
- What this wave does differently (guardrail, scope narrowing,
  precondition change).

Waived with the section omitted entirely when the wave is genuinely
novel. Do NOT write "no prior attempts" — just omit §9.

## Prohibitions

Per `docs/instructions/README.md` §Code discipline, made explicit for
wave specs:

- **No stubs.** If a file in §4 File bounds would land with `todo!()`,
  `_ => unimplemented!()`, or placeholder variants, the wave is not
  ready.
- **No forward hooks.** Each wave ships complete within its scope.
  Post-wave "future consumer" hooks violate substrate-with-consumer.
- **No mid-wave scope cut.** Inconclusive levers note in the wave's
  close ledger for post-tranche review. Retirement at tranche close
  only.
- **No external state assumptions.** The wave spec is self-contained;
  the orchestrator dispatches an agent with only (a) this wave spec,
  (b) the parent `{LETTER}.md`, (c) `docs/instructions/README.md`,
  (d) `docs/instructions/tranche/SPEC.md`.

## Parent index structure

The tranche's parent `{LETTER}.md` contains, in order:

1. Opening paragraph + architectural thesis.
2. Invariants (numbered, cross-tranche preserved, tranche-specific
   added).
3. Operational posture (process rules specific to this tranche).
4. Wave summary table — one row per wave, with a link to the wave
   spec at `waves/W<N>.md`.
5. Handoff contract (conditions the next tranche verifies before
   opening).
6. Defensible floor (minimal viable close per historical lever
   efficacy).
7. Post-tranche review candidates (inconclusive items deferred to
   tranche-close decision, NOT mid-wave retirement).
8. Indefatigability closing.

The parent carries NO per-wave detail beyond the summary-table row.
All detail lives in wave specs.

## Canonical example

`docs/tranches/AX/AX.md` (parent index) + `docs/tranches/AX/waves/W0a.md`
through `W15.md` (18 wave specs) are the canonical implementation.
Every section in every spec conforms to this document.
