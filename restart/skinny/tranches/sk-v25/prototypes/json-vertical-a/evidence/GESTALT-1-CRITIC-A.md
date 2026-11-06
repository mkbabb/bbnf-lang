# BB-JSON Family A — Gestalt 1 critic A

## Critic identity and verdict

- Critic: fresh hostile critic A, `gpt-5.6-sol`, reasoning effort `xhigh`.
- Review posture: implementation, specification, and tranche choice were assumed
  wrong and independently falsified.
- Subject status: `INCOMPLETE_ASSAY`. This is not one of the ten-or-more/twelve
  complete value/tape portfolio prototypes and earns no prototype count,
  selection authority, movement authority, or production-transposition authority.
- Verdict: **RED**.
- Consecutive-clean count: **0**.
- Hostile result: six executable counterexamples failed and one forbidden public
  API/type construction compiled and passed. Any one is enough to reopen the wave.

The narrow author gates and checksum claims reproduce. They establish only that
the checked-in slice is stable and self-consistent; they do not establish BBNF
language validity, semantic authority, W0 completion, Family A authority, JSON
behavior, generality, or tranche fitness.

## Total-tranche ruling

This slice was not the optimal experiment even if every implemented line were
perfect. Family A's distinguishing proposition is that one immutable chain can
carry sole authority through `LoweringPlan`, checked code, typed values, artifacts,
and crossings. This slice stops before all of those boundaries. Its implemented
source/rule/SCC skeleton is not a discriminating Family-A experiment and cannot be
compared with Family D. The same architectural family is already represented at
product scope by VT-1, while the verified alternative is represented by VT-4.
Maintaining a separate JSON-only half-carrier creates sand and duplicate future
work without resolving the binding JSON-plus-exhaustive-CSS-L4 product question.

More seriously, the BB-JSON-P0 contract cannot lawfully reach its own W2 from its
frozen input under the higher sole-source law. Section 11 requires, among other
facts, `Pair = Record { key: String, value: Json }`, named JSON variant labels, and
`Null: Unit`. The locked 537-byte BBNF module contains no `key` field declaration,
no record/variant/schema declaration, and maps `null` to `0u8`. Production grammar
editing is prohibited by section 2. A compiler therefore cannot derive the exact
required record field and semantic schema from that module alone. It must either:

1. consume the prose type table as a sidecar;
2. branch on JSON/rule names and invent fields;
3. add hidden host/type facts; or
4. change the BBNF module/package.

The first three are expressly forbidden; the fourth is outside this wave's frozen
boundary. The implementation's type hard-stop correctly exposes that architectural
counterexample, but the wave should terminate there. Adding a Rust table does not
repair it.

Optimal successor order is therefore: first close the BBNF module language and a
versioned BBNF prelude/host module so the complete semantic/type schema is present
in BBNF bytes; finish and freeze the exhaustive CSS-L4 union; then let an
independent complete VT-1 candidate test immutable stages against the same full
oracle. Do not extend this crate into a pseudo-vertical.

## Correct findings

### G1-A-01 — The frozen wave contract is unsatisfiable without forbidden extra type facts

`BB-JSON-P0.md` sections 2, 3, and 11 jointly freeze the current grammar, prohibit
grammar movement, and demand exact named record/variant facts absent from the
module. `rg` finds none of `key`, `Object`, `Array`, `Pair`, `Json`, `Null`,
`Unit`, `schema`, `field`, `variant`, or `entry` in the source fixture. The later
BB-VALUE-TAPE-P0 law correctly says that a prelude or host signature must itself be
a BBNF module. No such imported BBNF module is part of this assay.

This is a stop-condition architectural counterexample, not ordinary W2 backlog.
It invalidates completion of A or D against this frozen carrier and makes the wave
non-optimal as a route to the complete portfolio.

### G1-A-02 — Public handwritten JSON type facts violate sole BBNF authority

`src/vertical/types.rs:8-16` declares the public Rust enum facts `Json`, `Pair`,
`List`, `ExcludedPunctuation`, `Unit`, `Bool`, `F64`, and `String` by hand.
`src/vertical/types/row.rs` then accepts those facts in a public `ClosedRow`.
`tests/vertical/types/row.rs:6-20` manually asserts the JSON-specific `key` and
`value` table. These are precisely the handwritten semantic/type table and extra
type facts forbidden by the governing law.

Calling them "unreachable scaffolding" does not cure the defect: `vertical::types`
is public, the types are constructible by downstream crates, and the hostile
consumer compiled `Type::Json` and `Type::Pair`. This code must not be donated to a
successor.

### G1-A-03 — The source carrier is not a BBNF syntax transducer

`src/vertical/program.rs:125-164` only splits at semicolons outside a minimal
quote/slash state machine. It never parses rule-body syntax, checks trailing input,
resolves namespaces, validates operators, preserves syntax nodes/spans/directives,
or diagnoses undefined references. `contains_identifier` at lines 167-208 treats
any same-spelled identifier outside strings/regexes as a rule reference.

The hostile copy proved all four failures:

| Counterexample | Required result | Actual result |
|---|---|---|
| `root = "x" ; this is not a statement` | reject trailing non-BBNF bytes | `Program::from_source` succeeds and discards the suffix |
| `root = @@@ ;` | reject malformed rule syntax | succeeds with opaque body `@@@` |
| `root = missing ;` | reject undefined reference | succeeds with zero dependency edges |
| `input = "i" ; root = "x" -> decode(input) ;` | host `input` is not a grammar edge | emits `root -> input` |

The exact locked JSON edge set happens to pass because its spelling fits the
scanner. That is fixture agreement, not language validity.

### G1-A-04 — Required stage privacy and identity separation are absent

The wave requires private stage constructors and distinct identities such as
`SourceDigest`, `SemanticDigest`, `ProgramRevision`, and `GraphDigest`. The crate
instead uses one freely interchangeable `Digest` type for every plane. It exposes
`Program::from_source`, `NormalizedProgram::identity`,
`DependencyGraph::derive`, and `TypeSolution::solve` publicly, plus public fields
on `AssayStages` (`src/vertical.rs:60-78`).

The hostile consumer directly constructed every stage and compiled successfully.
There is no `SemanticDigest` at all. Consequently the Rust type system cannot
prevent a source digest from being supplied where a program/graph/type identity is
intended, and the public route bypasses the only nominal chain function.

### G1-A-05 — `W0: IMPLEMENTED` is false; the G3 and corpus gates are vacuous

`evidence/STATUS.json:10` marks W0 implemented, but W0 requires the exact isolated
placeholder topology, source-isomorphic RED tests, a complete intended-failure
source/corpus surface, and worktree/independence evidence. The crate omits the
specified facts/plan/code/runtime/artifact/WASM modules, nested type modules, web
package, frontend tests, craters for privacy/revision safety, and mechanical
independence gates.

The G3 test at `tests/born_red.rs:8-17` supplies the 24-byte mutation to a lock
that requires exactly 537 bytes and the JSON SHA. It proves only that a different
file fails the JSON source lock. It does not parse the G3 carrier, execute empty,
`a`, and `aa`, test normalization, or prove no publication. All positive and
negative JSON fixtures and all three benchmark fixtures are otherwise unread by
tests or production code. `serde_json` is used for digest/receipt serialization,
not as a corpus cross-check.

The test named `second_module_derives_its_own_rule_names_without_sidecars` reads
`json.bbnf` again. A separate crater does read `tiny.bbnf`, but only checks names
and the last-rule entry heuristic. Neither is a design-frozen unseen holdout.

The row test's loop is also not 1,024 seeded permutations. It repeats one already
canonicalized `BTreeMap` equality 1,024 times. Open rows, lacks, occurs-check,
levels, transactions, recursive closure, alpha stability, and store independence
are absent as the friction ledger admits.

### G1-A-06 — Resource ceilings are unbounded and caller-bypassable

`Budget::default` sets `runtime_operations` and `value_nodes` to `u64::MAX`
(`src/vertical/budget.rs:31-32`) rather than the locked formulas. The public fields
also let a caller raise every hard ceiling. The hostile test set
`source_bytes = 65_537` and successfully acquired 65,537 bytes, exceeding the
65,536-byte compile ceiling.

Only source bytes, rule count, the rule-as-node count, and final edge count are
ever charged. Reference discovery performs its allocation and roughly
rule-by-rule lexical scan before the edge limit check; graph-operation count,
solver work, strings, proof rows, plan/code/runtime/value/artifact work, and all
checked formulas are unimplemented. The public budget API therefore makes a
resource claim it does not enforce.

### G1-A-07 — Normalization and graph publication describe a rule skeleton, not the required program

`NormalizedProgram::identity` remaps only rule indices. The `Program` has no
operator/node arena, source spans, directives, hosts, mappings, cuts, whitespace
nodes, or ordered children, so the claimed bijection cannot cover the semantic
objects section 9 requires it to preserve.

`DependencyGraph` correctly emits the locked 13 rule-reference edges, seven SCCs,
and recursive component `{array, object, pair, value}` for the fixture. That is the
one narrow graph fact this pass did not falsify. It does not implement semantic
containment (`Contains`, `CallsHost`, `Annotates`, `Produces`, `Drops`), type flow,
stage provenance, plan CFG, deterministic condensation order, or a typed witness.
Its digest consequently cannot attest the required graph family.

### G1-A-08 — Generality is not established and JSON overfit is already present

There is no explicit grammar-name branch in the source scanner or SCC algorithm,
which is positive. Nevertheless:

- the public Rust `Type` table names `Json` and `Pair`;
- the row test supplies JSON field facts by hand;
- entry identity is always the last textual rule (`program.rs:85-88`);
- reference identity is token spelling, not parsed BBNF namespaces;
- the only non-JSON source was authored with the assay and checks only four names;
- no modules, imports, language-native decorators, host signatures/effects, or
  unseen grammar are parsed.

The host-input collision above is an external source counterexample to the generic
reference algorithm. The locked JSON corpus cannot select this architecture.

### G1-A-09 — Reproducibility claims are narrow; wave reproducibility remains RED

The following author claims are substantiated on the frozen ledger:

- all rows in `evidence/source-checksums.sha256` verify;
- the governing-wave digest and receipt hashes match;
- pinned rustfmt, clippy, and 13 author tests pass;
- two author-suite executions pass;
- `cargo metadata --locked` succeeds;
- the observed dated Rust/Cargo/LLVM/host versions match the receipt;
- the repository status byte hash is identical before and after each replay;
- no family-local `target` directory is created when an external target is used;
- an out-of-tree copied crate runs locked/offline metadata without lock mutation.

These do not close W0-W8. The in-repository Cargo commands inherit unused absolute
root patch entries and print eleven warnings, so an empty `[workspace]` is not by
itself an independence proof. No mechanical cross-family link/hardlink/path gate
is present. The two test executions reused one target and are not two clean
fresh-output artifact builds. There is no artifact emitter, receipt generator,
canonical command/exit-code receipt set, WASM/npm/browser build, deterministic
fresh-directory output comparison, injection matrix, or Apple profile. Those
omissions are mostly honestly recorded as W2/greater-tranche RED; they simply mean
the clean replay cannot be promoted into a wave or architecture verdict.

## Exact specification/gate matrix

| BB-JSON-P0 surface | Result | Evidence |
|---|---|---|
| Frozen production boundaries | PASS for this review | Only the required critic report was added; implementation and production files were not modified. Status bytes were identical across assay commands before this report. |
| Exact source bytes/hash | PASS | 537 bytes and `ab8d2121...f2604`; checksum verified. |
| Sole semantic/type source | FAIL | Public handwritten `Type`/row facts; frozen source cannot derive required `key` field/schema. |
| Family independence | UNPROVEN/RED | Empty `[workspace]` and no path dependencies, but no D family comparison or mechanical independence receipt; in-tree Cargo inherits root patch configuration. |
| Exact Goldilocks topology/W0 placeholders | FAIL | Most specified production/test/web modules are absent. |
| Distinct semantic identities | FAIL | One `Digest`; no `SemanticDigest`; missing all post-graph identities. |
| Family A lawful private chain | FAIL | Constructors and stage fields are public; chain stops before the distinguishing authority boundary. |
| G3 mutation | FAIL | Wrong-source lock mismatch only; no semantic execution. |
| Identity normalization | PARTIAL/RED | Zero rewrite and one-to-one rule remap pass; semantic nodes do not exist. |
| Locked rule graph/SCC | NARROW PASS | Exact 13 edges/seven components/recursive SCC pass. Required graph projections/order/witness are absent. |
| Type/row solution | FAIL | `Incomplete`; forbidden handwritten table; one repeated closed-row equality test. |
| Facts/plan/code/runtime/value | NOT RUN/RED | Honest hard-stop. |
| Rust/TS/WASM craters and browser protocol | NOT RUN/RED | Rust privacy crater absent; TypeScript/WASM/web absent. |
| Deterministic artifacts/publication | NOT RUN/RED | No emitter or artifacts. |
| Resource ceilings | FAIL | Two unbounded defaults, mutable hard caps, sparse charging. |
| Born-RED JSON corpus | FAIL | Files are inert; no acceptance/value/span/diagnostic observations. |
| Rust format/clippy/tests/metadata | PASS narrowly | Reproduced on the frozen ledger. |
| wasm/npm/typecheck/test/build/e2e | NOT RUN/RED | Surfaces do not exist. |
| Apple/aarch64 measurements | NOT RUN/RED | Honest receipt; no cost claim. |
| Canonical receipts/privacy/injections | FAIL/NOT RUN | Manual evidence text and checksum list only. |
| Two hostile clean passes | FAIL | This pass has correct findings; clean count remains zero. |

## Implemented feature/file/API audit

| Surface | Hostile disposition |
|---|---|
| `Cargo.toml`, `Cargo.lock`, `rust-toolchain.toml` | Standalone dependency resolution and pin reproduce. Root patch residue is unused but shows the in-tree commands are not an isolation gate. |
| `src/lib.rs`, `src/vertical.rs` | Small and readable. Public modules, public stage fields, one unbranded digest, and an always-incomplete `compile` expose more surface than the slice proves. |
| `budget.rs` | Source/rule/node/edge checks work at requested limits. Future fields are unused, two are unbounded, and all caps are caller-increasable. |
| `source.rs` | Exact byte hash/length and UTF-8 byte count work. Unlocked acquisition plus a non-parser downstream accepts invalid modules. |
| `program.rs` | Deterministic on the two fixtures and preserves raw rule body text. It is a handwritten delimiter/reference scanner, accepts invalid BBNF, drops trailing bytes/directives, conflates host and rule identifiers, and guesses entry from order. |
| `normalize.rs` | Deterministic zero-rewrite wrapper. It clones the program and remaps rules only; no independent semantic normalization invariant is proved. |
| `graph.rs` | Iterative SCC implementation and exact locked fixture result pass. It is only one projection and does not charge operations. |
| `types.rs`, `types/row.rs` | Must be deleted from any lawful successor. They are forbidden handwritten semantic facts, public, recursive (`Box<Type>`), and not a bounded flat solver. |
| External tests | Correctly live outside production and mirror the small modules. They test happy fixture values and receipt booleans, not language/resource/semantic adversaries. |
| Corpus/bench fixtures | Checksummed but inert. Their presence is not corpus execution. |
| Evidence files | Honest about incompleteness, W2, profiles, and movement. `W0: IMPLEMENTED`, G3 implication, row permutation wording, and the breadth inferred from 13 tests are not justified. |

## Graph structure and KISS/deletion opportunities

There is no shipping SCC or god module in this small crate. The larger defect is
sand: modules and fields that mimic the final topology without enforcing its
invariants.

Immediate KISS dispositions for a successor are:

1. **DELETE**, do not repair, the public handwritten `Type`, `ClosedRow`, and their
   JSON-specific test. A lawful type carrier must first be derivable from BBNF
   module bytes.
2. **DELETE or keep absent** unused future budget fields until their owning stages
   exist with hard maximums and checked formulas. An `u64::MAX` placeholder is a
   false invariant.
3. **COLLAPSE** the pass-through normalization wrapper unless a real parsed syntax
   arena and remap invariant are present. A cloned rule list plus new digest is
   mechanism without semantic work.
4. **REPLACE**, do not grow, the handwritten `definitions` and
   `contains_identifier` scanners with the one authoritative BBNF module parser.
   Adding special cases for comments, decorators, hosts, or JSON names would deepen
   the second authority.
5. **DELETE after evidence extraction** inert JSON/bench fixture copies from this
   retired assay. The common oracle may retain the bytes; an implementation should
   not claim fixtures it never consumes.
6. Keep source-isomorphic external tests and the small iterative SCC algorithm only
   as evidence ideas. Neither may be imported into a supposedly independent VT
   candidate; that candidate must re-earn them in its private universe.

## Greater-product effect and precise successor disposition

This crate adds a second BBNF source transducer, a second semantic identity scheme,
a JSON-only type authority, and a graph skeleton. None reaches a real package,
runtime, Value/tape, CSS, backend, ARM, or consumer edge. If extended, these would
compete with BBNF's one-home module/type authority and duplicate VT-1 rather than
reduce product risk.

Disposition:

1. Mark this frozen Family A ledger **REFUTED / RED / INCOMPLETE_ASSAY**, clean count
   zero. Do not call it a prototype and do not rank or profile it.
2. The fresh adjudicator must rerun the author gates and the hostile cases below,
   then label every finding. Any correct finding keeps the wave open.
3. **BANK** only the negative evidence: the current 537-byte grammar cannot derive
   the demanded exact schema, invalid/trailing modules are accepted, host/rule
   namespaces collide, stage privacy is absent, and hard ceilings are bypassable.
4. Route those cases into the authoritative BBNF module-language W0 and later
   complete candidate oracles as external counterexamples. Do not route this
   implementation, Rust types, tables, scanner, or helper code.
5. **RETIRE** this implementation as a Family-A candidate. Do not continue W2 and
   do not repair it by adding a sidecar, shim, fallback, adapter, rule-name branch,
   or extra type fact.
6. Family D may proceed only independently under its non-sharing law and must face
   the same frozen-source impossibility; it cannot use A as a helper or default
   winner. A versus D remains unresolved.
7. Before any full candidate work, supply the missing semantics as BBNF modules
   (including versioned prelude/host types) and close the exhaustive CSS-L4 input
   union. Then implement every PT/BB lock, full value/tape API, JSON and CSS,
   generated exact Rust/TS/WASM types, all modular backends and genuine Apple ARM
   lanes, package/consumer proof, profiles, unseen holdout, and two consecutive
   hostile clean passes independently per candidate.
8. **DELETE** the retired crate after adjudicated counterexamples and immutable
   receipts are preserved in their owning evidence bank. No production movement is
   authorized by this review.

## Executable hostile counterexamples

The hostile test was added only to an out-of-tree temporary copy, never to the
subject. Its final SHA-256 was
`cdadf17bbaeb8ac6781b9d5dc8bad8fe24a45460208c1f63066a833e30b0265b`.
The material assertions were:

```rust
let s = SourceUnit::acquire(
    "root = \"x\" ; this is not a statement", Budget::default()
).unwrap();
assert!(Program::from_source(&s, Budget::default()).is_err());

let s = SourceUnit::acquire("root = @@@ ;", Budget::default()).unwrap();
assert!(Program::from_source(&s, Budget::default()).is_err());

let s = SourceUnit::acquire("root = missing ;", Budget::default()).unwrap();
assert!(Program::from_source(&s, Budget::default()).is_err());

let s = SourceUnit::acquire(
    "input = \"i\" ; root = \"x\" -> decode(input) ;", Budget::default()
).unwrap();
assert!(assay(&s, Budget::default()).unwrap().graph.edges().is_empty());

let b = Budget::default();
let s = SourceUnit::acquire("root = \"x\" ;", b).unwrap();
let p = Program::from_source(&s, b).unwrap();
let n = NormalizedProgram::identity(&p, b).unwrap();
let g = DependencyGraph::derive(&n, b).unwrap();
assert!(matches!(TypeSolution::solve(&n, &g, b), Err(VerticalError::Incomplete(_))));
assert_eq!(Type::Json, Type::Json); // forbidden API/type facts compile
assert_eq!(Type::Pair, Type::Pair);

assert_ne!(Budget::default().runtime_operations, u64::MAX);
assert_ne!(Budget::default().value_nodes, u64::MAX);

let b = Budget { source_bytes: 65_537, ..Budget::default() };
assert!(SourceUnit::acquire(&"x".repeat(65_537), b).is_err());
```

Result: exit 101; six failed, one forbidden-construction test passed, zero ignored.
Original repository status bytes were identical before and after.

## Exact execution record

All commands ran from `/Users/mkbabb/Programming/bbnf-lang` unless an inner `cd`
is shown. In the command records, `<family>` expands exactly to
`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v25/prototypes/json-vertical-a`;
`<above>` expands to the full external-target path printed immediately before its
command block.

### Full read and immutable enumeration

```text
sed -n '1,220p' restart/skinny/tranches/sk-v25/LOCK-CONSTITUTION.md
sed -n '221,440p' restart/skinny/tranches/sk-v25/LOCK-CONSTITUTION.md
sed -n '1,200p' restart/skinny/tranches/sk-v25/CHALLENGE-LEDGER.md
sed -n '1,230p' restart/skinny/tranches/sk-v25/TRANCHE-PORTFOLIO.md
sed -n '231,500p' restart/skinny/tranches/sk-v25/TRANCHE-PORTFOLIO.md
sed -n '1,260p' restart/skinny/tranches/sk-v25/waves/BB-JSON-P0.md
sed -n '1,210p' restart/skinny/tranches/sk-v25/waves/BB-VALUE-TAPE-P0.md
sed -n '211,420p' restart/skinny/tranches/sk-v25/waves/BB-VALUE-TAPE-P0.md
sed -n '421,620p' restart/skinny/tranches/sk-v25/waves/BB-VALUE-TAPE-P0.md
find restart/skinny/tranches/sk-v25/prototypes/json-vertical-a -type f ! -path '*/evidence/GESTALT-1-CRITIC-A.md' -print0 | sort -z | xargs -0 awk 'FNR == 1 { print "FILE " FILENAME } { print }'
```

The combined read was followed by complete individual reads of every truncated
source file (`vertical.rs`, `budget.rs`, `graph.rs`, `normalize.rs`, and
`program.rs`). Every remaining assay file, including every fixture, was present in
the combined read.

### Author gates and status preservation

The shared external target was
`/var/folders/ld/20j4p2791k5_90_0ccgx2txm0000gr/T/tmp.t8fFODYJdp/target`.

```text
CARGO_TARGET_DIR=<above> cargo +nightly-2026-04-10 fmt --manifest-path <family>/Cargo.toml -- --check
CARGO_TARGET_DIR=<above> cargo +nightly-2026-04-10 clippy --manifest-path <family>/Cargo.toml --locked --all-targets --all-features -- -D warnings
CARGO_TARGET_DIR=<above> cargo +nightly-2026-04-10 test --manifest-path <family>/Cargo.toml --locked --all-features
CARGO_TARGET_DIR=<above> cargo +nightly-2026-04-10 test --manifest-path <family>/Cargo.toml --locked --all-features
CARGO_TARGET_DIR=<above> cargo +nightly-2026-04-10 metadata --manifest-path <family>/Cargo.toml --locked --format-version 1
```

Results: all exit 0; each test run reports 13 passed, 0 failed, 0 ignored. Before
and after each command:

```text
git status --porcelain=v2 -z
```

had SHA-256
`306f7914cfc050cf6d7e7c9ea1c05a33833f70bc9d9d48c1f3f5ae9c706ad2c5`
and bytewise `cmp` equality. The family-local `target` was absent. Metadata SHA-256
in this in-tree run was
`e4e2709a7b2cb51553628c7118c9a8de8da170da2b567f615d2a16c3d8d50d48`.

### Checksum and standalone replay

```text
cd restart/skinny/tranches/sk-v25/prototypes/json-vertical-a
shasum -a 256 -c evidence/source-checksums.sha256
```

Result: every row `OK`.

```text
cp -R <family> /var/folders/ld/20j4p2791k5_90_0ccgx2txm0000gr/T/tmp.31tz8Sa10A/json-vertical-a
cd /var/folders/ld/20j4p2791k5_90_0ccgx2txm0000gr/T/tmp.31tz8Sa10A/json-vertical-a
CARGO_TARGET_DIR=/var/folders/ld/20j4p2791k5_90_0ccgx2txm0000gr/T/tmp.31tz8Sa10A/target CARGO_NET_OFFLINE=true cargo +nightly-2026-04-10 metadata --locked --format-version 1
```

Result: exit 0; copied `Cargo.lock` stayed
`2d2efd5806186b28df89065e52639ec04b267d6059bd6e0c45d7eb26d28b3618`;
standalone metadata SHA-256 was
`3e1d4d440bef7a3f84ddd8116b2ee73aec0819c575e127ea7e67f99e628d8f05`.

### Hostile replay

```text
cp -R <family> /var/folders/ld/20j4p2791k5_90_0ccgx2txm0000gr/T/tmp.cA9EydY2PX/json-vertical-a
cd /var/folders/ld/20j4p2791k5_90_0ccgx2txm0000gr/T/tmp.cA9EydY2PX/json-vertical-a
CARGO_TARGET_DIR=/var/folders/ld/20j4p2791k5_90_0ccgx2txm0000gr/T/tmp.cA9EydY2PX/target CARGO_NET_OFFLINE=true cargo +nightly-2026-04-10 test --locked --test hostile_gestalt -- --nocapture
```

Result: exit 101; six failed and one passed as detailed above. Repository status
hash remained `306f7914...ad2c5` byte-for-byte.

### Other challenge commands

```text
rg -n "fixtures/(corpus|bench)|serde_json|G3|g3|Type::|pub fn|pub struct|pub enum|u64::MAX" <family>/src <family>/tests <family>/Cargo.toml
rg -n "\\b(key|Object|Array|Pair|Json|Null|Unit|schema|field|variant|entry)\\b" <family>/tests/fixtures/source/json.bbnf
git rev-parse HEAD
git branch --show-current
rustc +nightly-2026-04-10 -Vv
cargo +nightly-2026-04-10 -V
uname -m
sw_vers
```

The second `rg` returned no matches. Reviewed repository identity was
`af15f63e0d2d3d719938c13b906a50acbb92ea3b` on `master`; host/toolchain was
arm64 macOS 26.4.1 build 25E253, rustc nightly `f5eca4fcf`, Cargo
`eb94155a9`, LLVM 22.1.2.

## Immutable reviewed ledger

### Governing inputs

```text
006066f64915f02020102776093c4a519e3466fe6c31a3bbcfcfd8fef4a611e5  LOCK-CONSTITUTION.md
729b12b1cfa2e6fb5d49ad989b20a36a996619d75f62ecfe11e2ab633ac57617  CHALLENGE-LEDGER.md
ffea9974b0ecf8723e7b1b83ff1394167d0419cc973c0f6fc0e867a28bf03b39  TRANCHE-PORTFOLIO.md
36f88bf72043c95a33b9e0f814b08634664fa018ac2a6396d18a319e3c7bfc64  waves/BB-JSON-P0.md
1bb07e0e325495a64d7a6a4a65b75090bdfdfd7a630bb0ace0789f7fff624c05  waves/BB-VALUE-TAPE-P0.md
```

SHA-256 of those five sorted ledger lines, including their displayed relative
paths, is
`625829d2402747012b6166eb7df0c41f13ad801ade68e59491160a224c56f037`.

### Assay subject

The author manifest
`evidence/source-checksums.sha256` has SHA-256
`8eadb2e0d8475e52520095ae74092f0c47d6d7d195c0a0d060cb7e628b6744d1`
and every one of its 41 implementation/test/fixture rows verified. Evidence files
excluded from that author manifest were independently frozen as:

```text
1671bb230aeb3ad8d3a831c02f125c2fc3d3499726b06ddfc2605d2a7d05331c  evidence/FRICTION-AND-ROUTING.md
0cdf73e132ab28f4e18aeece679cdb2a45186205022409ce0b615105af9a1767  evidence/README.md
80466016e3c8462fc01e7a38969a7867fa556a31e43f785a1a099f201d5191d4  evidence/SOURCE-RECEIPT-LEDGER.md
ad2b2d27791de9e5d4e75b2cdb8cbff8929624d6062e4a3e4a2632d38dcf4d64  evidence/STATUS.json
8eadb2e0d8475e52520095ae74092f0c47d6d7d195c0a0d060cb7e628b6744d1  evidence/source-checksums.sha256
```

For an independent total-tree binding, the critic ran:

```text
find restart/skinny/tranches/sk-v25/prototypes/json-vertical-a -type f ! -path '*/evidence/GESTALT-1-CRITIC-A.md' -print0 | sort -z | while IFS= read -r -d '' file; do shasum -a 256 "$file"; done | shasum -a 256
```

The resulting SHA-256 of the complete sorted per-file ledger was
`573b97387124c7428aa53f08b4cba95684fdb96efd0097c8e8a21fe601c9e748`.
This self-excludes only this critic report and binds every file reviewed under the
Family A root.

## Final ruling

**RED.** The checksum-clean crate is an honest hard-stop plus a handful of narrow
source/graph facts, not a JSON vertical and not a portfolio prototype. The wave's
frozen source cannot supply the exact demanded types under sole BBNF authority;
the implementation then violates that authority with public handwritten JSON
types, accepts invalid BBNF, exposes private-stage constructors, leaves W0 corpus
gates vacuous, and fails hard resource ceilings. Preserve the counterexamples,
adjudicate them, retire the implementation, and route the product question to
complete independent BBNF-module-derived VT candidates only after the BBNF module
and exhaustive CSS-L4 inputs are actually total.
