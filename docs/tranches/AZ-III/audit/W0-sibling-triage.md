# AZ-III W0.6 — Sibling Repo Triage Packet

**Date:** 2026-04-30
**Agent:** AZ-III.W0.6 (read-only sibling triage)
**HARD CAP:** 25 min (within budget)
**Method:** read-only catalogue + gate execution against each sibling's own
toolchain. No source touched in any repo. No commits. No git config.

This packet exists because AZ-III.W0 §W0.6 requires a verdict on each sibling
repo's red surface plus an explicit disposition (a/b/c/d). The four siblings
are catalogued exactly as the user named them. Findings cross-checked against
`docs/tranches/AZ-III/audit/W0-state-ledger.txt` and
`docs/tranches/AZ-III/audit/REAUDIT-2026-04-30/01-failure-baseline.md` §4
(sibling status) and `…/03-substrate-deadcode.md` §7 (sibling posture).

---

## 0. Disposition Legend

- (a) **registry pin update inside this workspace** — the fix is a Cargo
  pin/patch in `bbnf-lang/Cargo.toml` or in a member crate's `Cargo.toml`,
  with no sibling-source edit required.
- (b) **sibling tranche needs to open** — the sibling repo carries its own
  red surface that warrants a tranche in the sibling repo (or a coordinated
  multi-repo tranche). Out-of-scope for AZ-III source edits.
- (c) **AZ-III blocker carry** — the sibling's red surface blocks AZ-III
  close until resolved. Must be reflected in AZ-III hard gates.
- (d) **NO ACTION** — already green or out-of-scope.

---

## 1. parse-that — `/Users/mkbabb/Programming/parse-that`

### 1.1 Presence and structure

Present. Multi-language repo containing a Rust workspace under `rust/`,
plus TypeScript implementation under `typescript/` and a grammar fixture
set under `grammar/`. Top-level layout:

```
parse-that/
├── rust/        Cargo workspace (members = ["src", "parse_that", "bootstrap", "regex"])
├── typescript/  npm package (out of W0.6 scope; covered as TS sibling)
├── grammar/     fixture grammars
├── data/        data fixtures
└── docs/        documentation incl. instructions submodule
```

Rust workspace root: `/Users/mkbabb/Programming/parse-that/rust/Cargo.toml`.
Members: `src`, `parse_that`, `bootstrap`, `regex`.

### 1.2 Gate results (this run)

| Gate | Command | Verdict | Log |
|---|---|---|---|
| fmt | `cargo fmt --all -- --check` | **PASS** | `/tmp/sibling-pt-fmt.log` |
| clippy | `cargo clippy --workspace -- -D warnings` | **FAIL** — 28 lint errors in workspace member `bbnf-regex` (the in-tree regex crate at `parse-that/rust/regex/src/`); `bbnf-regex` lib does not compile under `-D warnings` | `/tmp/sibling-pt-clippy.log` |
| test build | `cargo test --workspace --no-run` | **FAIL** — `error[E0432]: unresolved imports pprint::Doc, pprint::Join` at `~/.cargo/registry/src/index.crates.io-…/parse_that-0.3.3/src/parsers/csv.rs:7:14` | `/tmp/sibling-pt-test-build.log` |

### 1.3 Cross-reference with prior verdicts

- **W0-state-ledger.txt** named the published-`parse_that 0.3.3`-vs-new-pprint
  conflict as the test failure surface. **CONFIRMED** verbatim.
- **W0-state-ledger.txt** clippy surface listed regex range-loop, mutable
  range-bound, collapsible-if, duplicate branch, manual range contains,
  redundant closure, unwrap default, counter-loop, div-ceil, and UTF-8 loop
  lints. **CONFIRMED** — current clippy run yields exactly these classes
  (see `/tmp/sibling-pt-clippy.log` lines for `explicit_counter_loop`,
  `manual_div_ceil`, `needless_range_loop`, `collapsible_if`, redundant
  closure, `unwrap_or_else` to default, manual `RangeInclusive::contains`).
- **REAUDIT 01-failure-baseline §4** said `cargo test --workspace` aborts
  in compile because `parse_that 0.3.3` imports `pprint::Doc`/`Join`,
  absent in current sibling pprint. **CONFIRMED** — same crate, same
  registry source path, same E0432.
- **REAUDIT 03-substrate-deadcode §7** said sibling integrity is strong,
  no cross-repo dead substrate, `utils.rs` is 20 LOC and not a god module.
  **CONFIRMED** — no AZ-III hard-gate-3 token leaks (`Parsed<R>`,
  `TapeDirect`, `ValueRoot`, `TapeOffset`, generated tape views) appear in
  parse-that source.

### 1.4 Root cause (NEW finer-grained signal beyond W0 ledger)

`cargo tree --invert -p parse_that:0.3.3` traces the broken transitive pin
to a single crate.io consumer:

```
parse_that v0.3.3   (registry, has stale pprint::{Doc,Join})
└── bbnf_derive v0.2.9   (registry, proc-macro)
    └── regex-bootstrap v0.1.0 (parse-that/rust/bootstrap)   (workspace member)
```

The local workspace member `parse_that v0.4.0` (`parse-that/rust/parse_that`)
is fine and is path-resolved by every other consumer. The single offending
edge is `regex-bootstrap`'s `bbnf_derive = "0.2"` pin — which crates.io
resolves to `bbnf_derive 0.2.9`, whose own `Cargo.toml` requires
`parse_that = "0.3"`, locking in the pre-pprint-rename `0.3.3`.

**Mechanism**: bbnf_derive is a third-party-republished proc-macro that
hasn't been bumped to depend on `parse_that 0.4` since pprint dropped its
`Doc`/`Join` re-exports. As long as `bbnf_derive 0.2.x` exists on crates.io
without a coordinated bump, every parse-that workspace test build pulls
the stale `parse_that 0.3.3` and fails E0432 against the path-patched
`pprint 0.3.6`.

### 1.5 bbnf-lang reverse exposure

bbnf-lang itself **does NOT** consume `bbnf_derive` (no hits in
`Cargo.toml`/`crates/*/Cargo.toml` for `bbnf_derive`/`bbnf-derive` other
than the in-tree `crates/egraph-derive` which is unrelated).
bbnf-lang's `parse_that = "0.4"` and `pprint = "0.3"` registry pins are
themselves consistent and resolve correctly. **The parse-that sibling
breakage does NOT propagate into bbnf-lang's own iter-check / iter-test
surfaces** — confirmed by REAUDIT lane 1 §2 gate (a) `iter-check` PASS
and §2 gate (e) `no-default-features build` PASS.

### 1.6 Disposition: **(b) sibling tranche needs to open** + **(d) NO ACTION on bbnf-lang side**

Two-part verdict:

1. **For bbnf-lang's AZ-III**: **(d) NO ACTION**. The parse-that test
   failure does not surface in any bbnf-lang gate; bbnf-lang's `parse_that`
   and `pprint` registry pins are correct. No registry-pin update inside
   `bbnf-lang/Cargo.toml` would change the result. AZ-III is unblocked
   regardless of parse-that's own test-build state.
2. **For parse-that itself**: **(b) sibling tranche** in the
   `/Users/mkbabb/Programming/parse-that` repo. Two work items:
   - **clippy hardening**: the 28 in-tree `bbnf-regex` lint errors are
     a parse-that-internal source quality issue. Items: `collapsible_if`
     (>15), `needless_range_loop` (>3), `manual_div_ceil`,
     `manual_range_contains`, `explicit_counter_loop`, redundant
     closure, `unwrap_or_else` → `unwrap_or`. Pure refactors; no API.
   - **registry-pin breakage**: parse-that owns `bbnf_derive` (per
     historical naming pattern `bbnf_derive` ≈ derive macros for
     parse-that's bbnf grammar interface). Either bump `bbnf_derive`
     on crates.io to depend on `parse_that = "0.4"`, or drop the
     `regex-bootstrap → bbnf_derive` dep in favour of an in-workspace
     macro consumer. Either way, parse-that's `cargo test --workspace`
     close gate is its own repo's responsibility.

**Routed destination for the sibling tranche**: a future `parse-that`
tranche letter (not in the AZ-III sibling's sequence; parse-that has
its own tranche scheme). AZ-III only records this carry as a
non-blocking sibling state.

### 1.7 Justification against blocker-carry vs sibling-tranche

Why **NOT (c) AZ-III blocker carry**: AZ-III hard gates 1-9 do not
include "parse-that workspace tests pass". AZ-III's parity gates
(gate 4) consume bbnf-lang's *own* json/css/sheets/bbnf parity tests,
and bbnf-lang's `parse_that = "0.4"` registry pin gives correct
behaviour to those tests. The 63 nextest failures in bbnf-lang are
domain failures, not transitive-dep resolution failures.

Why **NOT (a) registry pin update**: there is no version of `parse_that`
on crates.io between 0.3.3 and 0.4.0 that would unblock parse-that's
own `regex-bootstrap` test build, because the broken edge originates
from `bbnf_derive 0.2.9` pinning `parse_that = "0.3"`. The fix has to
land in `bbnf_derive` (a parse-that-side artefact), not in any
bbnf-lang `Cargo.toml`.

---

## 2. pprint — `/Users/mkbabb/Programming/pprint`

### 2.1 Presence and structure

Present. Single-Rust-workspace repo. Top-level:

```
pprint/
├── rust/   Cargo workspace (root crate `pprint` v0.3.6 + `derive` member)
└── docs/   playground + docs
```

Workspace root: `/Users/mkbabb/Programming/pprint/rust/Cargo.toml`. Single
crate `pprint v0.3.6` plus `derive` (path = `./derive`, version 0.2.2).

### 2.2 Gate results (this run)

| Gate | Command | Verdict | Log |
|---|---|---|---|
| fmt | `cargo fmt --all -- --check` | **PASS** | `/tmp/sibling-pp-fmt.log` |
| clippy | `cargo clippy --workspace -- -D warnings` | **PASS** | `/tmp/sibling-pp-clippy.log` |
| test build | `cargo test --workspace --no-run` | **PASS** (5 binaries built, 1 dead-code warning `TestEnum::Skipped(i32)`) | `/tmp/sibling-pp-test-build.log` |

### 2.3 Cross-reference with prior verdicts

- **W0-state-ledger.txt** said pprint clippy was FAIL with `benches/digit_count.rs`
  Sync requirements, approximate constants, unused enum field, useless
  `into_iter()`, redundant `RcDoc::as_string` closures.
  **REFUTED** — current clippy passes cleanly with no errors. Either the
  pprint repo has had a fmt/clippy cleanup commit since the W0 ledger
  snapshot (W0 ledger says "rust formatting committed"; this run shows
  the lints have also been resolved), or the W0 ledger captured a
  different clippy invocation (`--all-targets` vs `--workspace`).
  This run uses the prompt's mandated `cargo clippy --workspace -- -D warnings`.
- **W0-state-ledger.txt** said pprint test PASS with one dead-code warning.
  **CONFIRMED** verbatim — same `TestEnum::Skipped(i32)` warning persists.
- **REAUDIT 01-failure-baseline §4** said pprint is GREEN.
  **CONFIRMED**.
- **REAUDIT 03-substrate-deadcode §7** said pprint is clean and the
  `target/package/pprint-0.3.{0..6}/` directories are build artefacts
  not source duplication. **CONFIRMED** — `find pprint -maxdepth 2 -type d`
  shows no orphan source trees.

### 2.4 Disposition: **(d) NO ACTION**

pprint is GREEN under the prompt's exact gate set (fmt, clippy
`--workspace -- -D warnings`, test build). No registry-pin update is
needed — bbnf-lang's `pprint = "0.3"` resolves to crates.io
`pprint 0.3.6`, identical to the local sibling. No sibling tranche is
needed. No AZ-III blocker carry.

The W0 ledger's pprint clippy entry is **stale**; recommend updating
W4 — Workspace Truth wave to record pprint clippy as currently MET
when AZ-III closes (this is the same posture as REAUDIT lane 1 §5
flagging stale-good rows in FINAL.md).

---

## 3. gorgeous — `/Users/mkbabb/Programming/gorgeous`

### 3.1 Presence and structure

**ABSENT.** `ls /Users/mkbabb/Programming/gorgeous` returns
`No such file or directory`. The path is not even an empty directory at
this moment — it does not exist at all on this filesystem (delta from
REAUDIT 03-substrate-deadcode §7 which described it as "empty").

The actual `gorgeous` crate lives **inside** `bbnf-lang` at
`/Users/mkbabb/Programming/bbnf-lang/crates/gorgeous/` — it is a
workspace member, not a sibling repo. Its `Cargo.toml` declares
`name = "gorgeous"`, `repository = "https://github.com/mkbabb/gorgeous"`,
and depends on `parse_that = "0.4"`, `pprint = "0.3"` (both registry).

### 3.2 Gate results (this run)

Not applicable — no sibling repo to test. The in-tree `crates/gorgeous`
is built via `cargo iter-check-prettify` (not the regular `iter-check`
workspace alias which `--exclude`s gorgeous per REAUDIT 01 §2 footnote).

### 3.3 Cross-reference with prior verdicts

- **W0-state-ledger.txt** does not mention gorgeous at all (correctly,
  because it is not a sibling worktree).
- **REAUDIT 01-failure-baseline §1 / §4 / §2 row j** stated:
  `/Users/mkbabb/Programming/gorgeous` does not exist; in-tree
  `crates/gorgeous` builds via `iter-check-prettify` alias and is
  excluded from `cargo iter-check` workspace alias.
  **CONFIRMED** verbatim.
- **REAUDIT 03-substrate-deadcode §7** said the empty
  `/Users/mkbabb/Programming/gorgeous` directory has no Cargo manifest
  and "can be safely deleted". **PARTIALLY CONFIRMED** — at this run
  the directory does not exist at all (so there is nothing to delete).
  Either it was already removed since the REAUDIT lane 3 snapshot or
  the lane-3 finding was based on `find` returning the path with zero
  contents which `ls` then refused.

### 3.4 Disposition: **(d) NO ACTION**

There is no sibling gorgeous repo to triage. The in-tree
`crates/gorgeous` is owned by AZ-III implicitly via the
`iter-check-prettify` alias and any gorgeous-side findings are routed
to W4 — Workspace Truth (per REAUDIT 03 §8 row 16, which flagged the
`format_ir` legacy alias for W4) and W5 — Terminal Close (per row 29
which flagged the absent sibling directory itself as a `LOW` rmdir
target — moot now).

No AZ-III hard gate references the sibling path; no bbnf-lang
`Cargo.toml` patches `gorgeous` from a sibling path
(verified: `crates/gorgeous` is workspace path-internal, `[patch]`
table not used for it). AZ-III is unblocked.

---

## 4. bbnf-buddy — `/Users/mkbabb/Programming/bbnf-buddy`

### 4.1 Presence and structure

Present. Vue 3 + Vite + TypeScript private package. `package.json`:

```json
{
  "name": "@mkbabb/bbnf-buddy",
  "version": "0.1.0",
  "private": true,
  "type": "module",
  "scripts": {
    "dev": "vite",
    "build": "vite build",
    "typecheck": "vue-tsc --noEmit",
    "preview": "vite preview",
    "test": "vitest run",
    "test:watch": "vitest",
    "test:e2e": "playwright test",
    "bench": "vitest bench",
    "bench:json": "vitest bench --outputJson tests/forms/fixtures/bench-current.json",
    "generate:favicon": "tsx scripts/generate-favicon.ts"
  }
}
```

Source layout: `src/{App.vue, main.ts, animation/, app/, assets/,
components/, composables/, editor/, export/, forms/, geometry/, poses/,
skins/, stores/, styles/}`. Tests under `tests/{animation, composables,
e2e, forms, geometry, history, poses}`.

### 4.2 Gate results (this run)

Per the prompt's instruction "if it's an npm/Vite project, just record
`package.json` private flag and any obvious test command name":

| Field | Value |
|---|---|
| `private` flag | **`true`** (confirmed) |
| Test command | `npm run test` → `vitest run` |
| Watch test | `npm run test:watch` |
| E2E | `npm run test:e2e` → `playwright test` |
| Bench | `npm run bench` → `vitest bench` |
| Build | `npm run build` → `vite build` |
| Typecheck | `npm run typecheck` → `vue-tsc --noEmit` |

No vitest/playwright run executed (out of W0.6 scope; private package).

### 4.3 Cross-reference with prior verdicts

- **W0-state-ledger.txt** does not mention bbnf-buddy. **EXPECTED** —
  the W0 ledger only catalogued Rust-workspace siblings.
- **REAUDIT 01-failure-baseline §4** does not mention bbnf-buddy.
  **EXPECTED** — same reason.
- **REAUDIT 03-substrate-deadcode §7** said bbnf-buddy is private,
  not consumed by bbnf-lang Rust workspace, "irrelevant to AZ-III
  scope; clean". **CONFIRMED** — `grep "bbnf-buddy" bbnf-lang/Cargo.toml
  bbnf-lang/crates/*/Cargo.toml` returns no hits; bbnf-buddy is not a
  build-graph dependency of bbnf-lang.

### 4.4 Disposition: **(d) NO ACTION**

bbnf-buddy is a private Vue/SVG mascot project per project memory
`project_bbnf_buddy.md`. It does not feed any AZ-III hard gate. No
registry-pin update applies (it is npm/Vite, not a Cargo dep). No
sibling tranche need is observable from this triage; if Vue/Vite gates
fail in their own iteration loop, that is a bbnf-buddy-internal concern
out of AZ-III's mandate.

---

## 5. Sibling Posture Summary

| Sibling | Path | Present | fmt | clippy | test build | Disposition | Routed destination |
|---|---|---|---|---|---|---|---|
| parse-that | `/Users/mkbabb/Programming/parse-that` | YES | PASS | **FAIL** (28 lint errors in `bbnf-regex`) | **FAIL** (E0432 in registry `parse_that 0.3.3`, transitively via `bbnf_derive 0.2.9` → `regex-bootstrap`) | **(b) + (d)** sibling tranche for parse-that itself; no bbnf-lang side action | parse-that's own tranche scheme; AZ-III records as non-blocking carry |
| pprint | `/Users/mkbabb/Programming/pprint` | YES | PASS | **PASS** (refutes W0 ledger which said FAIL — stale) | PASS | **(d) NO ACTION** | record as STALE-GOOD in W4 — Workspace Truth wave |
| gorgeous | `/Users/mkbabb/Programming/gorgeous` | **NO** (does not exist; in-tree `crates/gorgeous` is the actual artefact) | n/a | n/a | n/a | **(d) NO ACTION** | already noted in REAUDIT 03 §7 / §8 row 29 |
| bbnf-buddy | `/Users/mkbabb/Programming/bbnf-buddy` | YES | n/a (npm) | n/a | n/a | **(d) NO ACTION** | private Vue/Vite project; out of AZ-III mandate |

---

## 6. Net AZ-III Impact

**No sibling state blocks AZ-III close cleanly.**

Detail:

1. parse-that's red surfaces (28 clippy lint errors in `bbnf-regex`,
   plus the `bbnf_derive 0.2.9 → parse_that 0.3.3` E0432 chain) live
   inside parse-that's own workspace and do not propagate into any
   bbnf-lang gate. bbnf-lang's `parse_that = "0.4"` registry pin
   resolves correctly to `parse_that 0.4.0` on crates.io, which has
   the post-rename pprint imports. None of bbnf-lang's 63 nextest
   failures stem from a transitive `parse_that` resolution; they all
   stem from the AZ-II carry-over chronic surfaces enumerated in
   REAUDIT 01 §6 (CSS skip_space, hex-color/dir-pseudo/named-color
   typed-payload, Sheets `push_leaf_with_unit`, `EmitStrategy::for_grammar`
   missing entries, JSON Number-vs-U64). These are W2/W3 charters,
   not sibling-dep charters.
2. pprint is GREEN by every gate the prompt specified; the W0 ledger's
   "pprint clippy FAIL" entry is stale and should be reconciled at W5
   — Terminal Close.
3. gorgeous-as-sibling does not exist; there is nothing to triage.
   The in-tree `crates/gorgeous` is owned by W4 (`format_ir` alias)
   and its `iter-check-prettify` alias green-state is a separate
   gate not exercised by W0.6.
4. bbnf-buddy is a private downstream consumer with no inverse
   dependency on bbnf-lang's build graph.

**Carry to AZ-III hard gates**: zero. No new hard gate is required.
The Carried-Work-Ledger entry "REAUDIT 2026-04-30 | sibling-repo
(parse-that, pprint, gorgeous, bbnf-buddy) red-state triage" is
satisfied by this document; the actual sibling-source edits are
explicitly routed to (i) parse-that's own tranche scheme (not opened
here) and (ii) the in-tree `crates/gorgeous` work owned by W4.

**Carry to W4 — Workspace Truth (recommendation, not mandate)**:
   - record pprint as STALE-GOOD in workspace-health re-baseline
     (the W0 ledger's pprint clippy FAIL is no longer reproducible);
   - record parse-that's red state as a documented non-blocking
     carry (the bbnf-lang reverse-pin posture is correct, no Cargo
     edit can resolve the bbnf_derive 0.2.9 issue from this side);
   - confirm `crates/gorgeous` `format_ir` legacy alias deletion
     (REAUDIT 03 §8 row 16) is on the W4 deletion roster.

---

## 7. Evidence Logs

All sibling-gate evidence collected by this packet:

- `/tmp/sibling-pt-fmt.log` — parse-that fmt PASS
- `/tmp/sibling-pt-clippy.log` — parse-that clippy FAIL (28 lints)
- `/tmp/sibling-pt-test-build.log` — parse-that test build FAIL
  (E0432 in registry parse_that 0.3.3)
- `/tmp/sibling-pp-fmt.log` — pprint fmt PASS
- `/tmp/sibling-pp-clippy.log` — pprint clippy PASS
- `/tmp/sibling-pp-test-build.log` — pprint test build PASS

These are local-machine artefacts; the per-sibling verdicts above
cite them by category. Re-runs by other agents should produce the
same surface absent intervening commits in the sibling repos.
