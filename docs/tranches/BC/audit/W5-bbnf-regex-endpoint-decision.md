# BC.W5b — `bbnf-regex` Endpoint Reconciliation Decision

Date: 2026-05-03
Status: settled. Closes surgery 31 (`audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:71`), BC02-3 (`audit/HARDENING-PLAN-2026-05-03-02-sequencing-discipline.md:60`), C03-7 (`audit/HARDENING-PLAN-2026-05-03-03-cohesion.md:16`), and D08-9 (`audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:18`).

## §1 Decision

**Option A — Rename `parse-that/rust/regex` → `parse-that/rust/bbnf-regex`.**

Per Operational Rule 2, the decision is **in-plan**, not deferred to "user adjudicates at hardening time". The previous draft's adjudication-deferral residue is excised here.

## §2 Selection criteria + scoring

| Criterion | Option A (rename) | Option B (preserve `regex/`) | Weight | Winner |
|---|---|---|---|---|
| Crate-name canonicality | path matches crate name `bbnf-regex` | path mismatch (`regex/` directory hosts `bbnf-regex` crate) | high | A |
| External consumer impact | `parse-that` workspace's other crates updated once at rename | no rename, no update | medium | B |
| Future crates.io publication harmony | path matches publication name | path-name mismatch creates downstream confusion | high | A |
| One-time cargo-config update | updates `.cargo/config.toml`, `Cargo.toml`, GESTALT.md once | no update needed | low | B |
| Disambiguation from std `regex` crate | clear ("bbnf-regex" is unambiguous) | ambiguous ("regex/" colocates with std `regex` discussions) | medium | A |
| Migration cookbook surface | one rename row in `docs/migration/bc-core-split.md` | no rename row | low | B |

**Score**: Option A wins on 3 high-weight criteria; Option B wins on 3 low-medium-weight criteria. Decision is Option A.

## §3 Execution mechanism

Executed at BC.W5b sub-wave:

```bash
# Rename in parse-that
cd /Users/mkbabb/Programming/parse-that/rust
mv regex bbnf-regex

# Update parse-that workspace
sed -i.bak 's|"rust/regex"|"rust/bbnf-regex"|g' /Users/mkbabb/Programming/parse-that/Cargo.toml

# Update bbnf-lang patch
sed -i.bak 's|parse-that/rust/regex|parse-that/rust/bbnf-regex|g' \
  /Users/mkbabb/Programming/bbnf-lang/.cargo/config.toml

# Update GESTALT references
sed -i.bak 's|parse-that/rust/regex|parse-that/rust/bbnf-regex|g' \
  /Users/mkbabb/Programming/bbnf-lang/docs/GESTALT.md

# Verify
cargo metadata --locked --manifest-path /Users/mkbabb/Programming/parse-that/Cargo.toml > /dev/null
cargo metadata --locked --manifest-path /Users/mkbabb/Programming/bbnf-lang/Cargo.toml > /dev/null

rg -n "parse-that/rust/regex" docs/ .cargo/config.toml -- returns zero post-rename
```

## §4 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W5b-G1 | Rename complete | `test -d /Users/mkbabb/Programming/parse-that/rust/bbnf-regex && test ! -d /Users/mkbabb/Programming/parse-that/rust/regex` |
| W5b-G2 | parse-that workspace clean | `cargo metadata --locked --manifest-path /Users/mkbabb/Programming/parse-that/Cargo.toml` succeeds |
| W5b-G3 | bbnf-lang patch clean | `cargo metadata --locked --manifest-path /Users/mkbabb/Programming/bbnf-lang/Cargo.toml` succeeds |
| W5b-G4 | Stale-path scrub | `rg -n "parse-that/rust/regex" docs/ .cargo/config.toml` returns zero |
| W5b-G5 | Workspace builds clean | `cargo check --workspace` green |

## §5 Sibling baseline reconciliation

`parse-that` is one of the five tracked siblings in the BC.W0c sibling baseline (per `audit/W0-sibling-baseline.txt`). After W5b execution, BC.W5d reconciles: `parse-that` will have a non-trivial diff (the rename); the diff is expected and recorded; the other four siblings (pprint, csc411/csp-solver, bbnf-buddy, ffuzzy) remain at baseline modulo upstream pulls.

## §6 Cross-references

| Reference | Description |
|---|---|
| `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:71` (surgery 31) | "remove 'user adjudicates at hardening time'; choose one `bbnf-regex` endpoint in the plan and gate it" |
| `audit/HARDENING-PLAN-2026-05-03-02-sequencing-discipline.md:60` (BC02-3) | Replace adjudication-deferral with default endpoint decision and hard gate |
| `audit/HARDENING-PLAN-2026-05-03-03-cohesion.md:16` (C03-7) | Make BC.W5 choose one endpoint by default |
| `audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:18` (D08-9) | Choose the endpoint in BC.W5 plan text |
| `audit/MODULES-2026-05-03.md:43` | The crate name `bbnf-regex` is canonical; the rename achieves crate-name canonicality |
| `audit/W5-parse-that-disposition.md` | parse-that workspace remains a private path-dep; bbnf-regex sub-crate is the publication candidate |
