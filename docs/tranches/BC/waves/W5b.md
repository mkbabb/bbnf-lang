# BC.W5b — `bbnf-regex` Endpoint Reconciliation Execution

**Name**: W5b — bbnf-regex Endpoint Rename
**Opens after**: BC.W5a (parallel; can land before or with W5a's bbnf-regex sub-step)
**Hard gate**: `parse-that/rust/regex/` renamed to `parse-that/rust/bbnf-regex/`; `.cargo/config.toml` patch updated; `rg -n "parse-that/rust/regex" docs/ .cargo/config.toml` returns zero
**Status**: planned

## §1 Deliverable

Per `audit/W5-bbnf-regex-endpoint-decision.md`, execute Option A (rename `parse-that/rust/regex` → `parse-that/rust/bbnf-regex`). The wave is mechanical execution per surgery 31 + BC02-3 + C03-7 + D08-9: in-plan decision, gated, no "user adjudicates" residue.

## §2 Milestones

### §2.1 Pre-flight
Mechanism: per `audit/W5-bbnf-regex-endpoint-decision.md:§3`:
```
test -d /Users/mkbabb/Programming/parse-that/rust/regex
test ! -d /Users/mkbabb/Programming/parse-that/rust/bbnf-regex
```
Files: `docs/tranches/BC/audit/W5b-preflight.txt` (create).
Sub-gate: pre-flight commands return expected status; rename has not yet occurred.

### §2.2 Rename execution
Mechanism: `mv /Users/mkbabb/Programming/parse-that/rust/regex /Users/mkbabb/Programming/parse-that/rust/bbnf-regex`.
Sub-gate: post-rename, `test -d .../bbnf-regex && test ! -d .../regex`.

### §2.3 parse-that workspace update
Mechanism: update `parse-that/Cargo.toml` `[workspace.members]` from `"rust/regex"` to `"rust/bbnf-regex"`.
Sub-gate: `cargo metadata --locked --manifest-path /Users/mkbabb/Programming/parse-that/Cargo.toml` succeeds.

### §2.4 bbnf-lang patch update
Mechanism: update `.cargo/config.toml` `[patch.crates-io]` block to use new path.
Sub-gate: `cargo metadata --locked --manifest-path /Users/mkbabb/Programming/bbnf-lang/Cargo.toml` succeeds.

### §2.5 GESTALT + docs scrub
Mechanism: update every `docs/GESTALT.md` reference + every BC tranche document reference.
Sub-gate: `rg -n "parse-that/rust/regex" docs/ .cargo/config.toml` returns zero (post-rename).

### §2.6 Workspace verification
Mechanism: `cargo check --workspace` green.
Sub-gate: bbnf-lang workspace builds clean post-rename.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W5b-G1 | Rename executed | `test -d /Users/mkbabb/Programming/parse-that/rust/bbnf-regex && test ! -d /Users/mkbabb/Programming/parse-that/rust/regex` |
| W5b-G2 | parse-that workspace clean | `cargo metadata --locked --manifest-path /Users/mkbabb/Programming/parse-that/Cargo.toml` succeeds |
| W5b-G3 | bbnf-lang patch clean | `cargo metadata --locked --manifest-path /Users/mkbabb/Programming/bbnf-lang/Cargo.toml` succeeds |
| W5b-G4 | Stale paths scrubbed | `rg -n "parse-that/rust/regex" docs/ .cargo/config.toml` returns zero |
| W5b-G5 | Workspace builds clean | `cargo check --workspace` green |

## §4 Invariants

§I1. **Lock 7**: path crate triplet + bbnf-regex sibling aligned in naming.

§I2. **Crate-name canonicality**: path matches crate name post-rename.

§I3. **In-plan decision** per Operational Rule 2; no "user adjudicates at hardening time" residue.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Rename breaks parse-that's other crates | Medium | parse-that's other crates reference via workspace path-dep; cargo metadata verifies |
| Sibling baseline drift | Low | W0c sibling baseline records pre-W5b state; W5d reconciliation records the rename diff |

## §6 Cross-references

### Carry-tags consumed
| Tag | From | Description |
|---|---|---|
| BC.W4→BC.W5b | BC.W4 | Workspace stable; rename can land |

### BC-G gates closed
| Gate | Closure |
|---|---|
| BC-G7 | bbnf-regex endpoint reconciliation per Option A (W5b-G1); rationale documented per W5-bbnf-regex-endpoint-decision.md |

### Carry-tags produced
| Tag | To | Description |
|---|---|---|
| BC.W5b→BC.W5a | BC.W5a | Renamed bbnf-regex available for freeze |
| BC.W5b→BC.W5d | BC.W5d | parse-that diff in sibling reconciliation |

## §7 Iter-time check

| Activity | Pre-W5b wall | Post-W5b wall | Notes |
|---|---:|---:|---|
| `cargo check --workspace` | ~30 s | ~30 s | unchanged post-rename |

## §8 Dependencies

- **Depends on**: BC.W4 close (workspace stable).
- **Blocks**: BC.W5a's bbnf-regex sub-step (W5a-G3); BC.W5d (sibling reconciliation).

## §9 Closing posture

Mechanical rename executed. Crate-name canonicality. No adjudication-residue.
