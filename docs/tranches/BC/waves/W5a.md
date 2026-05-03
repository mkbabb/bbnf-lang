# BC.W5a — Sister Crate API Freeze

**Name**: W5a — Sister Crate API Freeze (egraph + egraph-derive + csp-solver + bbnf-regex)
**Opens after**: BC.W4 close (visitor surface formalisation)
**Hard gate**: `cargo doc -p egraph -p egraph-derive -p csp-solver -p bbnf-regex` clean; `cargo publish --dry-run` clean for each
**Status**: planned

## §1 Deliverable

Per Lock 11 + surgery 31 / BC02-3 / D08-9, the sister crates freeze API surfaces and become crates.io publication candidates. The freeze does NOT mean publication; it means surface stability and `cargo publish --dry-run` success. Per `audit/W5-parse-that-disposition.md` and Lock 11 omission per L11, `parse-that` is **explicitly excluded** from publication candidates (permanent private path-dep per gap I option (i)); `bbnf-regex` (the sister sub-crate) is the publication candidate.

## §2 Milestones

### §2.1 egraph + egraph-derive freeze
Mechanism: `cargo doc -p egraph -p egraph-derive` clean; metadata complete (license, description, keywords, categories, repository).
Sub-gate: `cargo publish --dry-run -p egraph -p egraph-derive` clean.

### §2.2 csp-solver freeze
Mechanism: per `feedback_csp_always_optimize`, csc411 sibling is algorithm-evolution authoritative; bbnf-lang in-tree is bench authoritative. `diff -rq` between bbnf-lang `crates/csp-solver/src` and csc411 sibling lists only the declared csc411-only files plus identical content for the shared file set.
Sub-gate: `cargo publish --dry-run -p csp-solver` clean; diff-equality verified.

### §2.3 bbnf-regex freeze (post-W5b rename)
Mechanism: post-W5b (`audit/W5-bbnf-regex-endpoint-decision.md`), `bbnf-regex` lives at `parse-that/rust/bbnf-regex/`. Freeze API at this canonical path.
Sub-gate: `cargo publish --dry-run -p bbnf-regex` clean.

### §2.4 Public surface inventory
Mechanism: per `audit/W3-crate-dependency-dag.md:§2`, document each sister crate's public surface (traits, types, functions). The inventory is the freeze attestation.
Files: `docs/tranches/BC/audit/W5a-public-surface-inventory.md` (create).
Sub-gate: per-crate public surface enumerated; metadata complete.

### §2.5 parse-that exclusion attestation (per gap I)
Mechanism: per `audit/W5-parse-that-disposition.md`, `parse-that` is permanent private path-dep. The freeze list does NOT include `parse-that`. The W5a evidence file records this explicitly.
Files: `docs/tranches/BC/audit/W5a-parse-that-exclusion.md` (create).
Sub-gate: `parse-that/Cargo.toml` `[package].publish = false`; the workspace member is path-dep only.

### §2.6 Publication readiness audit
Mechanism: per Lock 11, the freeze is the precondition for crates.io publication. Audit each sister crate's metadata completeness.
Files: `docs/tranches/BC/audit/W5a-publication-readiness.md` (create).
Sub-gate: per-sister-crate metadata complete; `cargo publish --dry-run` clean for each.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W5a-G1 | egraph + egraph-derive frozen | `cargo doc -p egraph -p egraph-derive` clean; `cargo publish --dry-run -p egraph -p egraph-derive` clean |
| W5a-G2 | csp-solver frozen with csc411 diff-equality | `cargo doc -p csp-solver` clean; `cargo publish --dry-run -p csp-solver` clean; `diff -rq` against csc411 verified |
| W5a-G3 | bbnf-regex frozen | `cargo doc -p bbnf-regex` clean; `cargo publish --dry-run -p bbnf-regex` clean (depends on W5b rename) |
| W5a-G4 | Public surface inventory | `docs/tranches/BC/audit/W5a-public-surface-inventory.md` per-crate complete |
| W5a-G5 | parse-that excluded | `docs/tranches/BC/audit/W5a-parse-that-exclusion.md` records the gap-I option (i) decision |
| W5a-G6 | Publication readiness | per-sister-crate metadata fields complete |

## §4 Invariants

§I1. **Lock 11**: sister crates path-dep until publication; freeze is precondition.

§I2. **No publication in BC**: actual `cargo publish` is BD scope per BC→BD.C2.

§I3. **csp-solver canonicality**: bbnf-lang in-tree bench authoritative; csc411 algorithm-evolution authoritative.

§I4. **parse-that permanently private** per gap I option (i).

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| `cargo publish --dry-run` fails for egraph (path-dep contamination) | Medium | Resolve path-deps for publication readiness; freeze precondition |
| csc411 diverges by content not just file-set | Medium | Per `feedback_csp_always_optimize`, canonicality enforced via `diff -rq`; W5a-G2 fails until reconciled |

## §6 Cross-references

### Carry-tags consumed
| Tag | From | Description |
|---|---|---|
| BB→BC.C4 | BB.W0 | Sister crates path-deps; BC may promote |
| BC.W4→BC.W5a | BC.W4 | Visitor trait at `bbnf-runtime/src/visitor.rs` is part of public surface |

### BC-G gates closed
| Gate | Closure |
|---|---|
| BC-G8 | Sister crates candidates for crates.io publication; `cargo publish --dry-run` clean for each (W5a-G1, W5a-G2, W5a-G3) |

### Carry-tags produced
| Tag | To | Description |
|---|---|---|
| BC.W5a→BC.W5b | BC.W5b | bbnf-regex freeze depends on W5b rename |
| BC→BD.C2 | BD.W2 | Sister crates frozen; BD may publish |

## §7 Iter-time check

| Activity | Pre-W5a wall | Post-W5a wall | Notes |
|---|---:|---:|---|
| `cargo doc -p egraph -p egraph-derive -p csp-solver -p bbnf-regex` | n/a | ~25 s | new doc iter-loop |
| `cargo publish --dry-run -p egraph` | n/a | ~12 s | new publish-check iter-loop |

## §8 Dependencies

- **Depends on**: BC.W4 close.
- **Blocks**: BC.W5b (bbnf-regex rename precedes its freeze; W5a's bbnf-regex sub-step waits on W5b).

## §9 Closing posture

Sister crates freeze; metadata complete; publication candidacy ratified. parse-that excluded per gap I. BD inherits the publication candidate set.
