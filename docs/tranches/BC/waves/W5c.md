# BC.W5c — `parse-that` Disposition Ratification

**Name**: W5c — parse-that Disposition Ratification
**Opens after**: BC.W5a + BC.W5b close
**Hard gate**: `parse-that` declared permanent private path-dep per `audit/W5-parse-that-disposition.md`; `parse-that/Cargo.toml` `[package].publish = false`; the workspace member is path-dep only
**Status**: planned

## §1 Deliverable

Per gap I option (i) and `audit/W5-parse-that-disposition.md`, ratify `parse-that` as a **permanent private path-dep**. Never published. The wave's deliverable is the in-plan decision attestation: `parse-that/Cargo.toml` carries `publish = false`; bbnf-lang's `[patch.crates-io]` retains the patch entry indefinitely; downstream consumers of bbnf-lang inherit the patch transitively.

This wave is the formalisation of the gap-I decision; W5b executes the bbnf-regex rename within parse-that's tree, and W5c ratifies that the workspace root remains private.

## §2 Milestones

### §2.1 parse-that workspace `publish = false`
Mechanism: ensure `parse-that/Cargo.toml` (workspace root) and `parse-that/rust/parse-that/Cargo.toml` (combinator crate) both declare `publish = false`.
Sub-gate: `grep -n 'publish' /Users/mkbabb/Programming/parse-that/Cargo.toml /Users/mkbabb/Programming/parse-that/rust/parse-that/Cargo.toml` returns expected lines.

### §2.2 Sister sub-crate publication policy
Mechanism: per `audit/W5-parse-that-disposition.md:§3`, sister sub-crates within parse-that have differing publication postures:
- `parse-that/rust/bbnf-regex/`: publication candidate (frozen at W5a; published at BD.W2 via BC→BD.C2)
- `parse-that/rust/pprint/`: private (`publish = false`); bbnf-grammar-targeted
- `parse-that/rust/parse-that/`: private (combinator crate; never publishes)
Sub-gate: per-sub-crate Cargo.toml has correct `publish` field.

### §2.3 bbnf-lang `[patch.crates-io]` retention
Mechanism: bbnf-lang's `.cargo/config.toml` retains the parse-that patch entry indefinitely. Document this is permanent in the migration cookbook §5.7.
Sub-gate: `grep -n 'parse-that' /Users/mkbabb/Programming/bbnf-lang/.cargo/config.toml` returns the patch entry.

### §2.4 Cookbook ratification
Mechanism: `docs/migration/bc-core-split.md:§5.7` documents that downstream bbnf-lang consumers inherit the patch transitively or vendor parse-that themselves.
Sub-gate: cookbook §5.7 exists with the canonical guidance.

### §2.5 BD carry record
Mechanism: per `audit/W6-bd-carry-contract.md:§3 BC→BD.C2`, parse-that is **excluded** from BD publication candidates. The BD agent draft must record this exclusion.
Sub-gate: W6 carry contract names BD's BD.W2 wave with `egraph + egraph-derive + csp-solver + bbnf-regex` only — NOT parse-that.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W5c-G1 | parse-that workspace private | `parse-that/Cargo.toml` and `parse-that/rust/parse-that/Cargo.toml` both have `publish = false` |
| W5c-G2 | Sister sub-crate publication policy correct | `parse-that/rust/bbnf-regex/Cargo.toml` lacks `publish = false`; others have it |
| W5c-G3 | bbnf-lang patch retained | `[patch.crates-io]` entry intact in `.cargo/config.toml` |
| W5c-G4 | Migration cookbook §5.7 exists | `grep -n '5\.7' docs/migration/bc-core-split.md` |
| W5c-G5 | BD carry contract names exclusion | `audit/W6-bd-carry-contract.md` BC→BD.C2 row excludes parse-that |

## §4 Invariants

§I1. **Lock 11 (path-deps for incubating sister crates)**: parse-that is incubating-eternal; never leaves incubation.

§I2. **In-plan decision** per Operational Rule 2.

§I3. **No publication friction inherited by BD**: parse-that's exclusion is structural.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Downstream consumer expects parse-that on crates.io | Low | Cookbook §5.7 documents the path-dep posture; alternative is fork-and-vendor |
| The bbnf-regex sub-crate's publication leaks parse-that internals | Low | bbnf-regex's `Cargo.toml` declares only its own deps; parse-that-internal types are not in bbnf-regex's public surface |

## §6 Cross-references

### Carry-tags consumed
| Tag | From | Description |
|---|---|---|
| BC.W5a→BC.W5c | BC.W5a | Sister crate publication candidacy ratified |
| BC.W5b→BC.W5c | BC.W5b | bbnf-regex rename completed |

### BC-G gates closed
(none directly — W5c is a publication-policy ratification)

### Carry-tags produced
| Tag | To | Description |
|---|---|---|
| BC→BD.C2 (clarified) | BD.W2 | Publication candidates: egraph, egraph-derive, csp-solver, bbnf-regex; explicitly EXCLUDING parse-that |

## §7 Iter-time check

(no iter-time impact; document-only wave)

## §8 Dependencies

- **Depends on**: BC.W5a + BC.W5b close.
- **Blocks**: BC.W5d (sibling baseline reconciliation references parse-that's W5b + W5c diffs); BC.W6 (carry ledger to BD).

## §9 Closing posture

parse-that's disposition ratified per gap I option (i). Permanent private path-dep. BD does not inherit a publication carry for parse-that itself.
