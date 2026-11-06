# BB-JSON Family A — hostile Gestalt 1, critic B

## Declaration and verdict

- Reviewer lane: independent critic B.
- Declared model/effort: `gpt-5.6-sol`, `xhigh`.
- Posture: Family A was assumed wrong. Author receipts were inputs to challenge,
  not accepted evidence.
- Independence: I did not read or coordinate with
  `GESTALT-1-CRITIC-A.md`. That file was excluded from every inventory and
  subject-ledger command.
- Subject status: `INCOMPLETE_ASSAY`, never a product prototype.
- Production movement: not authorized and not performed.

**Verdict: RED. Consecutive-clean count: 0.** The exact source bytes and the
locked JSON rule-dependency SCC reproduce, but correct findings B01-B11 falsify
tranche usefulness, wave fidelity, feature truth, evidence closure, and any
greater-product or selection credit. No implementation was changed by this
review.

## Immutable review ledger

The governing inputs read in full were:

| Input | SHA-256 |
|---|---|
| `LOCK-CONSTITUTION.md` | `006066f64915f02020102776093c4a519e3466fe6c31a3bbcfcfd8fef4a611e5` |
| `CHALLENGE-LEDGER.md` | `f9d3a1fcedc4cd7c504c11e1d991cb7bf294d292c14d065c3549194f6ba77c52` |
| `TRANCHE-PORTFOLIO.md` | `ffea9974b0ecf8723e7b1b83ff1394167d0419cc973c0f6fc0e867a28bf03b39` |
| `waves/BB-JSON-P0.md` | `36f88bf72043c95a33b9e0f814b08634664fa018ac2a6396d18a319e3c7bfc64` |
| `waves/BB-VALUE-TAPE-P0.md` | `0564548b2e7149e9ec8f9651c23f66ba277fa21db5ed8406c52f0be66aa1dfb0` |

I read all 46 files under `prototypes/json-vertical-a` that existed before this
report, excluding both critic-report paths. The canonical stream of
`sha256(file-bytes) path`, sorted by relative path, hashes to
`6334b066902e750830fed54286b7e19ce9f7b669e61ec5f568a0da5122788454`.
The author's 41-entry implementation/test/fixture list hashes to
`8eadb2e0d8475e52520095ae74092f0c47d6d7d195c0a0d060cb7e628b6744d1`
and every listed entry verifies when the command is run from the family root.
The additional evidence files independently hashed as follows:

| File | SHA-256 |
|---|---|
| `evidence/README.md` | `0cdf73e132ab28f4e18aeece679cdb2a45186205022409ce0b615105af9a1767` |
| `evidence/STATUS.json` | `ad2b2d27791de9e5d4e75b2cdb8cbff8929624d6062e4a3e4a2632d38dcf4d64` |
| `evidence/FRICTION-AND-ROUTING.md` | `1671bb230aeb3ad8d3a831c02f125c2fc3d3499726b06ddfc2605d2a7d05331c` |
| `evidence/SOURCE-RECEIPT-LEDGER.md` | `80466016e3c8462fc01e7a38969a7867fa556a31e43f785a1a099f201d5191d4` |
| `evidence/source-checksums.sha256` | `8eadb2e0d8475e52520095ae74092f0c47d6d7d195c0a0d060cb7e628b6744d1` |

The pre-assay and post-assay repository status byte stream was unchanged at
`306f7914cfc050cf6d7e7c9ea1c05a33833f70bc9d9d48c1f3f5ae9c706ad2c5`.
The root `Cargo.lock` has SHA-256
`a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908`
and `git diff --exit-code -- Cargo.lock` exited 0. The family remains an
untracked isolated directory; this report is its only review write.

Observed toolchain:

```text
arm64
macOS 26.4.1 build 25E253
rustc 1.96.0-nightly f5eca4fcf (2026-04-09)
LLVM 22.1.2
cargo 1.96.0-nightly eb94155a9 (2026-04-09)
```

This agrees with the receipt's dated pin. It does not establish WASM, browser,
artifact, PMU, or performance closure.

## Exact command ledger

Commands below were executed from the repository root unless a different
working directory is stated. External target directories prevented family-local
build output.

1. Source and governing-input identity:

   ```sh
   shasum -a 256 restart/skinny/tranches/sk-v25/LOCK-CONSTITUTION.md restart/skinny/tranches/sk-v25/CHALLENGE-LEDGER.md restart/skinny/tranches/sk-v25/TRANCHE-PORTFOLIO.md restart/skinny/tranches/sk-v25/waves/BB-JSON-P0.md restart/skinny/tranches/sk-v25/waves/BB-VALUE-TAPE-P0.md
   shasum -a 256 grammar/json/json.bbnf restart/skinny/tranches/sk-v25/prototypes/json-vertical-a/tests/fixtures/source/json.bbnf restart/skinny/tranches/sk-v25/waves/BB-JSON-P0.md restart/skinny/tranches/sk-v25/prototypes/json-vertical-a/Cargo.lock restart/skinny/tranches/sk-v25/prototypes/json-vertical-a/evidence/source-checksums.sha256
   cmp -l grammar/json/json.bbnf restart/skinny/tranches/sk-v25/prototypes/json-vertical-a/tests/fixtures/source/json.bbnf
   ```

   Exit 0. Both BBNF files are 537 bytes with SHA-256
   `ab8d21218b2a5f49d9231af6f71503a9949782fdc0a50d81a845fc69054f2604`.

2. From `prototypes/json-vertical-a`:

   ```sh
   shasum -a 256 -c evidence/source-checksums.sha256
   shasum -a 256 evidence/README.md evidence/STATUS.json evidence/FRICTION-AND-ROUTING.md evidence/SOURCE-RECEIPT-LEDGER.md evidence/source-checksums.sha256
   ```

   Exit 0; all 41 listed paths were `OK`. Invoking the checksum command from
   the repository root fails because its paths are family-relative; the author
   ledger gives no exact verification command or working directory.

3. From `prototypes/json-vertical-a`:

   ```sh
   rustc +nightly-2026-04-10 -Vv
   cargo +nightly-2026-04-10 -V
   uname -m
   sw_vers
   CARGO_TARGET_DIR=$(mktemp -d) cargo +nightly-2026-04-10 fmt --manifest-path Cargo.toml -- --check
   CARGO_TARGET_DIR=$(mktemp -d) cargo +nightly-2026-04-10 clippy --manifest-path Cargo.toml --locked --all-targets --all-features -- -D warnings
   CARGO_TARGET_DIR=$(mktemp -d) cargo +nightly-2026-04-10 metadata --manifest-path Cargo.toml --locked --format-version 1 --no-deps
   CARGO_TARGET_DIR=$(mktemp -d) cargo +nightly-2026-04-10 test --manifest-path Cargo.toml --locked --all-features -- --test-threads=1
   CARGO_TARGET_DIR=$(mktemp -d) cargo +nightly-2026-04-10 test --manifest-path Cargo.toml --locked --all-features -- --test-threads=1
   ```

   All exited 0. Both serial test runs reported 13 passed, 0 failed, 0
   ignored, 0 source-colocated tests, and 0 doc tests. The in-tree clippy and
   test commands emitted eleven warnings for inherited unused root patch
   entries, so their output was not clean. Metadata showed one standalone
   package and only registry dependencies (`serde`, `serde_json`, `sha2`, and
   `thiserror`).

4. A copy made outside the repository with both critic reports excluded was
   replayed with:

   ```sh
   rsync -a --exclude target --exclude GESTALT-1-CRITIC-A.md --exclude GESTALT-1-CRITIC-B.md ./ /var/folders/ld/20j4p2791k5_90_0ccgx2txm0000gr/T/tmp.wrRwUcBZi4/family/
   CARGO_TARGET_DIR=$(mktemp -d) cargo +nightly-2026-04-10 metadata --manifest-path Cargo.toml --locked --format-version 1 --no-deps
   CARGO_TARGET_DIR=$(mktemp -d) cargo +nightly-2026-04-10 test --manifest-path Cargo.toml --locked --all-features -- --test-threads=1
   ```

   Exit 0, again 13/13. The external copy did not inherit the root patch
   warnings. This proves narrow crate portability, not clean in-repository
   output or artifact determinism.

5. The reported stage hashes were exposed with:

   ```sh
   CARGO_TARGET_DIR=$(mktemp -d) cargo +nightly-2026-04-10 test --manifest-path Cargo.toml --locked --all-features stage_result_digests_are_reported -- --nocapture --test-threads=1
   ```

   It reproduced:

   ```text
   source     ab8d21218b2a5f49d9231af6f71503a9949782fdc0a50d81a845fc69054f2604
   program    908129a7e097b1070ac1b7ac587cd242999ca8672057c13faf22abf41051ca71
   normalized 650f68b18ef9a566ed0857feead3afc4c232fec9c606dec3ca26ffbeab47ccaa
   graph      f0e97cd27cb8d67d1b86e310c823bb62a976d6c4fbdb092f24eca4c24e6cd8ac
   ```

6. Two external `rustc -D warnings` drivers were compiled against the locked
   rlib, not added to the subject. One replayed the live locked BBNF source and
   one supplied hostile strings through the public API. The hostile cases were
   exactly:

   ```rust
   assay("start = missing ;")
   assay("1bad = \"a\" ;")
   assay("start = \"a\" ; THIS IS NOT BBNF")
   assay("start = \"a\" | epsilon ;")
   assay("input = \"x\" ; root = \"y\" -> host(input) ;")
   ProgramB::rule(ProgramA::rules()[0].id())
   assay(&format!("root = {} \"z\" ;", "\"a\" | ".repeat(4_000)),
         Budget { nodes: 1, ..Budget::default() })
   ```

   The driver output was byte-identical twice, SHA-256
   `ccbba1cf5678455f70a637f926cc66456f3d98df012bcb2ef00d63188cb8d342`.
   Its material output is preserved in the feature matrix and findings below.

7. The required locked WASM build was attempted without installing tooling:

   ```sh
   CARGO_TARGET_DIR=$(mktemp -d) cargo +nightly-2026-04-10 build --manifest-path Cargo.toml --locked --target wasm32-unknown-unknown --release
   ```

   Exit 101: `wasm32-unknown-unknown` is not installed. Under section 21 of the
   wave this is `BLOCKED-TOOLING`, never green. The `web` directory is absent,
   so the required npm/typecheck/test/build/e2e commands are `NOT-RUN` rather
   than skipped green.

8. Structure, test reachability, and budget-use scans:

   ```sh
   rg --files src tests web 2>/dev/null | sort
   wc -l src/*.rs src/vertical/*.rs src/vertical/types/*.rs tests/*.rs tests/vertical/*.rs tests/vertical/types/*.rs
   rg -n 'fixtures/(corpus|bench)|serde_json::from_str|compile\(' tests
   rg -n '#\[cfg\(test\)\]|#\[test\]' src
   ```

   There are 691 source lines, 272 Rust test lines, no inline tests, no `web`
   tree, and no test reference to any positive/negative JSON fixture or bench
   fixture. The only corpus mutation reference is `g3.bbnf`.

## Independent feature matrix

| Plane | Independent execution | Result |
|---|---|---|
| Source lock | Compared the live production BBNF file to the family copy, then called `acquire_locked` | Exact bytes/hash reproduce. Narrow PASS only. |
| Rule acquisition | Printed all names and entry from the live file | Ten expected rules and entry `value` reproduce. RED for malformed acceptance and opaque bodies (B03). |
| Identity | Replayed hashes, mixed a `RuleId` between two programs, and mixed normalized/graph stages | Hashes repeat and `TypeSolution::solve` rejects a graph from another program; a foreign `RuleId` silently resolves in the second program. RED (B04). |
| Normalization/G3 | Printed the live remap and directly assayed `start = "a" \| epsilon ;` | Ten rule-index pairs are identity-mapped; G3 is accepted with 0 rewrites/0 deletions and has no executable semantic oracle. RED (B05). |
| JSON graph/SCC | Printed every edge, component, and condensation edge | The 13 locked rule edges, seven components, recursive SCC `{array,object,pair,value}`, and six outgoing condensation edges reproduce. Narrow PASS. Generic role confusion is RED (B06). |
| Type/row hard stop | Called cross-revision solve and two closed-row comparisons | Cross-revision solve returns `Identity`; publication path returns `Incomplete`. The only row operation is equality over a handwritten JSON enum; the 1,024 test iterations are identical. RED (B04/B07). |
| Budgets | Exhausted source and graph-edge limits; supplied 4,001 syntax leaves with `nodes=1` | Source/edge errors are typed. The 24,013-byte syntax is accepted and charged as one node; eight budget fields have no behavioral use and two default to `u64::MAX`. RED (B08). |
| Receipts | Recomputed all hashes, replayed source checksums, toolchain, status, and root lock | Listed byte hashes/toolchain/status reproduce narrowly. W0 status, command exactness, completeness relations, and output claims are RED (B02/B10). |
| Determinism/artifacts | Ran tests twice, hashed hostile output twice, attempted WASM, inventoried web/artifacts | Current in-memory hashes repeat. No artifact publisher, fresh-output comparison, TS/WASM/browser crater, or package exists. WASM is blocked. RED (B10). |

For the locked file, the independently observed graph was:

```text
array  -> comma, value
pair   -> colon, string, value
object -> comma, pair
value  -> null, bool, number, string, array, object

SCCs: {null}, {bool}, {number}, {comma}, {colon}, {string},
      {array, object, pair, value}
```

That observation is useful source census evidence. It is not evidence for the
missing semantic, type, value, code, backend, package, or product planes.

## RED findings

### B01 — the slice does not test the Family A product hypothesis

Family A's distinctive risks are immutable authority through facts, plan, code,
runtime publication, stale-stage resistance, and consumer behavior. This slice
stops at a rule-dependency graph. Its `SourceUnit -> Program -> identity wrapper
-> graph` chain does not test plan authority, hidden code authority, stale
publication, rollback, a value representation, or any consumer. A direct robust
BBNF parser plus dependency census is the simpler control and would produce all
surviving evidence. The slice therefore cannot rank Family A against Family D or
against any value/tape candidate, and it is not the smallest discriminating route
through the total campaign.

The correct disposition is assay evidence only: retain the exact-source and
locked-SCC observations, but give the architecture no advance, donor, selection,
or transposition credit.

### B02 — W0 is not implemented and the normative topology is absent

`STATUS.json` says `"W0": "IMPLEMENTED"`, but the normative Family A topology is
missing 42 required paths: 13 production modules, their 13 source-isomorphic
tests, and the entire 16-file web/package/test surface. Missing production owners
include `facts`, `plan`, `code`, `runtime`, `artifact`, `wasm`, type infer/solve,
runtime value/execute/wire, and artifact emit/publish. The corresponding W0
placeholder APIs, RED tests, independence mechanics, browser protocol, and package
craters do not exist.

The README's `INCOMPLETE_ASSAY` label is correct, but it does not make the W0
receipt true. W1 is less than the wave's W1 and W2 is wholly RED.

### B03 — the handwritten statement scanner is not a BBNF parser

`Program::from_source` stores every rule body as opaque `Arc<str>`. The private
`definitions` scanner splits on semicolons and equals signs, while
`contains_identifier` mines rule edges lexically. It has no module, import,
namespace, decorator, operator, mapping, host-signature, type, span, or syntax-node
model. Direct hostile execution produced:

```text
ACCEPT unresolved-reference: rules=1 edges=0 entry=start
ACCEPT leading-digit-rule: rules=1 edges=0 entry=1bad
ACCEPT ignored-trailing-garbage: rules=1 edges=0 entry=start
```

Thus malformed modules, unresolved references, and ignored trailing input can
become immutable programs. The tiny second carrier tests only four extracted
names and a last-rule convention; it does not exercise heterogeneous module
syntax or semantics. This is a founding-grammar lexical assay, not grammar
generality or a source-total parser.

### B04 — identity ownership is structurally false

The required identities are represented by one interchangeable public `Digest`
type. There is no `SemanticDigest` and no nominal `ProgramRevision`,
`GraphDigest`, or typed downstream identity. `RuleId` serializes only a `u16`,
not its owning revision. The direct counterexample took rule 0 from program A and
looked it up in program B:

```text
foreign RuleId resolves as Ok("other")
```

Whitespace-only spellings of the same two-rule grammar also produced different
program revisions, which is acceptable for an exact source revision but proves
that no separate semantic identity exists. Public stage constructors
(`Program::from_source`, `NormalizedProgram::identity`, and
`DependencyGraph::derive`) contradict the wave's private-constructor contract.
`pub struct TypeSolution;` is itself externally constructible despite the claimed
publication hard stop and carries no program revision or solved content.

One narrow guard works: mixing program A's normalized stage with program B's
graph makes `TypeSolution::solve` return `Identity { stage: "types" }`. It does
not repair the foreign-ID, untyped-digest, public-construction, or forged-solution
surfaces.

### B05 — normalization and the G3 mutation are not semantically assayed

The normalizer maps rule indices to themselves; it does not have syntax nodes or
operators to normalize. The hostile G3 grammar was accepted directly and yielded:

```text
G3 direct: rewrites=0 deleted=0 remap=1 body="\"a\" | epsilon"
```

The `locked_source_rejects_g3_carrier` test merely compares the G3 file against
the unrelated locked JSON byte hash. Any non-JSON bytes would pass that negative
test. Nothing executes the grammar on empty input, `a`, and `aa`; no test can
detect an epsilon-erasing normalizer or consistently altered semantics. The
claimed G3 kill and semantic-preservation proof are therefore isomorphic to a
hash mismatch, not to the required mutation.

`NodeRemap` also maps ten rules, not the syntax-node arena promised by the wave,
and carries neither source nor normalized revision.

### B06 — the JSON SCC is right by accident of lexical roles, not a general graph

The exact JSON rule graph and iterative Kosaraju result reproduce. However, only
untyped rule dependency edges exist. There are no containment kinds, host calls,
annotations, production/drop edges, type flow, stage provenance, plan CFG,
condensation order report, or typed SCC witness.

Lexical role confusion is executable. For:

```bbnf
input = "x" ;
root = "y" -> host(input) ;
```

the graph emitted `root->input`, treating a host argument as a grammar-rule
reference. The same mechanism can create false cycles and SCCs in heterogeneous
modules. It cannot serve as the canonical module or semantic graph.

### B07 — the row assay violates the BBNF-only source law

`types.rs` handwrites the JSON-specific variants `Json`, `Pair`, and
`ExcludedPunctuation`; tests construct those facts directly. This is exactly the
extra handwritten semantic/type input forbidden by the complete value/tape law.
The BBNF file derives none of it.

`ClosedRow::unify` is only equality over a public `BTreeMap<String, Type>`.
Duplicates are erased before the operation can diagnose them. It has no variables,
open tails, lacks, occurs check, recursive closure, level lowering, substitution,
or budget. The supposed 1,024 permutations repeat one already canonicalized pair
1,024 times; there is no seed and no set of distinct permutations. The independent
row execution established only:

```text
row_same=Ok(ClosedRow({"key": String, "value": Json}))
row_missing=Err(Type("incompatible closed row at row labels"))
```

This code and its test are removable assay sand, not type-kernel evidence.

### B08 — resource accounting counts containers, not semantic work

Only source bytes, rule count, rule-as-node count, and deduplicated graph-edge
count are enforced. A one-rule body containing 4,001 literal/alternative leaves,
24,013 bytes total, succeeds with `Budget { nodes: 1, .. }` and reports one remap
node. Parser/reference/SCC operations are uncharged. `solver_operations`,
`plan_decisions`, `code_instructions`, `input_bytes`, `nesting`,
`runtime_operations`, `value_nodes`, and `artifact_bytes` have no behavioral use;
the runtime and value defaults are `u64::MAX`, contrary to the bounded-context
law.

The typed source and zero-edge errors are correct but insufficient. The assay has
no proof that every real resource is charged or that exhaustion is transactional
at the semantic stage boundaries.

### B09 — the corpus, craters, and claimed test structure are mostly decorative

None of the four positive JSON files, four negative JSON files, or three bench
files is referenced by any test. There is no JSON parser or observation oracle,
so acceptance, values, duplicates, spans, diagnostic code/offset, numbers,
Unicode, depth, or resources are not tested. `serde_json` is used in tests only
to parse `STATUS.json`.

`tests/craters.rs` is an ordinary runtime assertion over the tiny source; it does
not invoke `rustc`, TypeScript, WASM, a packed consumer, or a compile-fail boundary.
`second_module_derives_its_own_rule_names_without_sidecars` actually reads the
JSON fixture again, not the second module. The external source-isomorphic layout
is clean for the few modules that exist, and source contains no inline tests, but
missing owners and dead fixtures prevent structural or feature credit.

### B10 — receipts preserve bytes but cannot close output or artifact truth

Positive observations are limited: the listed checksums validate from the family
root; the toolchain identity is accurate; the current stage hashes repeat; both
serial Rust test runs pass; an external copy is standalone; and no tracked root
lock changed.

The receipt is still RED:

- its commands use `<family>` placeholders and omit a checksum verification
  command/working directory, exact exit-code records, raw outputs, and a canonical
  reciprocal receipt schema;
- `STATUS.json` overstates W0;
- exact in-tree commands inherit eleven root-patch warnings, so output is not
  clean despite an empty `[workspace]`;
- no caller-supplied `BBNF_ASSAY_OUT` path is consumed;
- no module/Rust/TypeScript/WASM/provenance artifact set exists to rebuild twice,
  corrupt, race, compare, or publish;
- the locked WASM build is `BLOCKED-TOOLING`, and npm/browser commands are
  `NOT-RUN` because `web` is absent;
- there are no Apple layout, paired performance, PMU, or profile samples.

The current deterministic digests are deterministic serialization of a source
hash, opaque rule strings, identity report, lexical edges, and SCCs. They are not
deterministic product artifacts.

### B11 — greater-product disposition remains RED/routed

The slice adds no production package edge, does not mutate production, and does
not create a shipping second authority. Those are correct preservation facts.
It also supplies no real consumer edge and cannot satisfy BB-1/2/3/7, any PT lock,
the full value/tape API, JSON runtime truth, exhaustive CSS L4, exact BBNF-derived
Rust/TypeScript types, regex/bootstrap, generated backends, WASM/package/browser,
Apple kernels, profiles, or an unseen holdout.

Under `BB-VALUE-TAPE-P0`, this remains a narrow assay organ and cannot be counted
among the required complete candidates. Family D has not been compared here and A
cannot win by default. No production transpose, hybridization, graph movement,
or later-wave closure follows from this report.

## KISS and routed remainder

The smallest honest repair is not to extend this lexical carrier into a shadow
compiler. Preserve the exact source-lock and locked JSON SCC observations as banked
census evidence, then route implementation to a candidate whose BBNF module system
is the sole semantic/type/host authority.

Immediate deletion/collapse candidates if this slice is retained:

- delete the handwritten `Type`/`ClosedRow` authority and its counterfeit
  1,024-loop test, or replace the entire plane with one explicit hard-stop error;
- remove the eight unused budget fields until an implemented stage owns and
  charges them;
- collapse the identity-only `NormalizedProgram` wrapper unless a real parsed
  syntax arena and revision-qualified remap give it an independent invariant;
- delete or stop crediting dead corpus/bench fixtures until executable oracles
  consume them;
- do not add facts, plans, code, or backends on top of opaque body strings and
  lexical reference mining.

Required successor work remains the complete born-RED portfolio: real BBNF
modules/imports/decorators/host signatures, semantic and revision-qualified IDs,
parsed grammar operators, total graph projections, genuine type/row/effect solving,
facts/cost/lowering, execution and the full value API, JSON and exhaustive CSS L4,
Rust/TypeScript/WASM/ARM/package/consumer surfaces, deterministic artifacts,
resources and profiles, unseen holdouts, and two consecutive fresh hostile
adjudications. Until then the terminal state is `INCOMPLETE_ASSAY / RED`, with no
clean-pass credit.
