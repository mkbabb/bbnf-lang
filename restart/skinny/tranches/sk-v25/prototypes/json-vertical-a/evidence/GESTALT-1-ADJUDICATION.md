# BB-JSON Family A — Gestalt 1 adjudication

## Authority, subject, and verdict

- Adjudicator: fresh `gpt-5.6-sol`, reasoning effort `xhigh`.
- Posture: neither critic was treated as authoritative. Every numbered claim was
  checked against the governing texts, the complete Family A tree, source, and
  independent execution.
- Subject: the immutable ledger under `prototypes/json-vertical-a` before this
  report. The complete sorted `sha256(file-bytes) path` stream for its 48 input
  files, excluding this report, hashes to
  `2ba6118c2c480093c926e195e2150dc420c9ba77f9249946ac94a7585839905d`.
- Critic A SHA-256:
  `093d175341525f123da82585a8da7ad25bb7d7af2a84cecc3eb15c0232633d95`.
- Critic B SHA-256:
  `87c228838e4c9aa9629f86a9de08ffe35825c24798cf8ffbe43a6a4af11b8af5`.
- Verdict: **RED / REFUTED AS A FAMILY-A CANDIDATE / INCOMPLETE_ASSAY**.
- Gestalt 1 consecutive-clean count: **0**.
- Prototype, architecture, movement, production-transposition, donor, and
  performance credit: **none**.

All nine A findings and all eleven B findings are **CORRECT**. The findings are
not twenty independent defects; they overlap heavily, as mapped below. The narrow
author suite remains green, but no clean hostile pass can be earned while any of
these correct findings survives. No repair occurred in this adjudication.

`CORRECT` means the material claim is proved by current source/specification and,
where useful, an independent counterexample. `INCORRECT` would mean the claimed
behavior is disproved. `UNPROVEN` would mean the available evidence cannot decide
it. No numbered claim falls into the latter two classes.

## Independent tranche and higher-law ruling

### The binding product experiment is not this slice

The constitution requires a hostile pass to judge total-tranche fit, not merely
whether author tests pass. `TRANCHE-PORTFOLIO.md` expressly supersedes the earlier
linear campaign: BB-JSON Families A and D are narrow architecture assay organs,
while `BB-VALUE-TAPE-P0.md` is the binding product experiment. A complete candidate
must independently close every PT/BB lock, the full value/tape API, JSON,
exhaustive CSS L4, exact BBNF-only types, Rust/TypeScript/WASM/ARM/package/consumer
surfaces, profiles, an unseen holdout, and two clean hostile passes.

This crate stops at one lexical rule-dependency graph. It does not exercise the
Family A proposition that immutable authority can remain sole and revision-safe
through facts, plan, checked code, runtime values, publication, artifacts, or
crossings. Its surviving evidence can be obtained by a source-lock plus a valid
BBNF dependency census. It therefore cannot discriminate Family A from Family D,
cannot rank against VT-1 or VT-4, and is not an optimal continuation of the total
tranche.

### The frozen BB-JSON carrier cannot satisfy the higher BBNF-only source law

The locked 537-byte module is byte-identical to `grammar/json/json.bbnf`, with
SHA-256
`ab8d21218b2a5f49d9231af6f71503a9949782fdc0a50d81a845fc69054f2604`.
It contains:

```text
null = "null" -> 0u8 ;
pair = string, colon >> value ;
value = object | array | string | number | bool | null ;
```

It contains no declaration of the required record field `key`, no record or
variant schema, no `Json` recursive binder, and no BBNF prelude/import that gives
the host literals and mappings their required semantic types. Yet BB-JSON section
11 requires, among other exact facts:

```text
Json = mu J. Variant { Object: Vec<Pair>, Array: Vec<J>, ... Null: Unit }
Pair = Record { key: String, value: Json }
```

The higher constitution and BB-VALUE section 5 require semantic/type/public-shape
facts to come solely from BBNF module bytes; even a prelude or host signature must
be an imported BBNF module. From the frozen bytes, a grammar-general compiler can
derive a sequence involving `string`, dropped punctuation, and `value`; it cannot
derive the absent public field name `key` or turn the explicit `0u8` mapping into
the demanded `Null: Unit` fact without an additional convention or fact. Reaching
W2 therefore requires one of four forbidden moves: use the prose table as a
sidecar, branch on JSON/rule names, consult a hidden host/type registry, or edit/add
BBNF modules outside the frozen boundary.

This is a specification-level stop condition. The implementation's decision to
return `Incomplete` before publication is honest and preferable to inventing
facts, but it does not complete W1/W2 or cure the public handwritten JSON facts
already present. BB-JSON-P0 cannot close on this carrier under the higher law.

## Finding-by-finding adjudication — critic A

| Finding | Ruling | Independent proof |
|---|---|---|
| **G1-A-01 — frozen contract is unsatisfiable without extra type facts** | **CORRECT** | The source/hash reproduced. An `rg` search found none of `key`, `Object`, `Array`, `Pair`, `Json`, `Null`, `Unit`, `schema`, `field`, `variant`, or `entry`. Section 11 nevertheless requires named `key/value` record fields and `Null: Unit`; the higher sole-source law forbids a prose/host/schema sidecar. The conflict is intrinsic to the frozen input, not ordinary W2 backlog. |
| **G1-A-02 — public handwritten JSON type facts violate BBNF authority** | **CORRECT** | `src/vertical/types.rs` publicly handwrites `Json`, `Pair`, and `ExcludedPunctuation`; `row.rs` publicly accepts them; the row test handwrites `key` and `value`. The external probe constructed `Type::Json`, `Type::Pair`, `ClosedRow`, and a forged `TypeSolution` successfully. These are grammar-specific Rust facts, not derived outputs. |
| **G1-A-03 — source carrier is not a BBNF syntax transducer** | **CORRECT** | The external probe showed `Program::from_source` accepts `root = @@@ ;`, `root = missing ;`, `1bad = "a" ;`, and `root = "x" ; this is not a statement`. It stores opaque bodies and lexically mines identifiers. `input = "i" ; root = "x" -> decode(input) ;` emitted the false grammar edge `root -> input`. There is no parsed operator/module/import/namespace/decorator/host/span/directive model or trailing-input validation. |
| **G1-A-04 — stage privacy and identity separation are absent** | **CORRECT** | `Digest` is the one public identity type; no `SemanticDigest` or branded revision/digest types exist. `Program::from_source`, `NormalizedProgram::identity`, `DependencyGraph::derive`, and `TypeSolution::solve` are public, and `AssayStages` fields are public. The probe directly constructed all stages and `TypeSolution`. A `RuleId` from one program resolved as rule 0 in an unrelated program. One working types-stage revision comparison is too late and too narrow to restore ownership. |
| **G1-A-05 — `W0: IMPLEMENTED`, G3, corpus, and row gates are false/vacuous** | **CORRECT** | Forty-two exact topology paths are absent: 13 production owners, 13 matching tests, and all 16 web/package paths. The G3 test only gives a 24-byte file to a 537-byte JSON lock. Direct assay accepts `start = "a" \| epsilon ;` and records zero rewrites/deletions; there is no execution on empty/`a`/`aa`. No positive, negative, or bench JSON fixture is referenced. The purported 1,024 permutations repeat one canonical `BTreeMap` comparison; the admitted open-row/lacks/occurs/level/transaction/closure/alpha/store surfaces do not exist. |
| **G1-A-06 — resource ceilings are unbounded and caller-bypassable** | **CORRECT** | `runtime_operations` and `value_nodes` default to `u64::MAX`. All hard caps are public caller fields; a 65,537-byte source succeeded after raising `source_bytes` to 65,537. A 4,001-leaf, 24,013-byte body succeeded with `nodes = 1` because only rules are counted as nodes. Eight fields have no behavioral consumer, and lexical/reference/SCC operation work is not charged. Source and edge checks that do work do not satisfy total checked charging. |
| **G1-A-07 — normalization/graph publish only a rule skeleton** | **CORRECT** | The normalizer remaps rule indices only; the program has no syntax nodes, operators, ordered children, mappings, cuts, whitespace nodes, spans, hosts, or directives to preserve. The exact locked 13 rule edges, seven SCCs, recursive component, and six outgoing condensation edges reproduced. Semantic containment kinds, host/annotation/produce/drop edges, type flow, provenance, CFG, condensation order report, witness, and graph-operation budget are absent. |
| **G1-A-08 — generality is unproved and overfit is present** | **CORRECT** | There is no grammar-name branch in the lexical scanner/SCC routine, but public `Json`/`Pair` types and the handwritten `key/value` row are direct JSON overfit. Entry is always the last statement; identifiers are unqualified spellings; host/rule roles collide. `tiny.bbnf` proves only four extracted names and the same last-rule convention, not modules/imports/decorators/hosts/effects or an unseen holdout. |
| **G1-A-09 — narrow reproducibility passes, wave reproducibility remains RED** | **CORRECT** | All 41 author checksums, pinned fmt/clippy, two 13-test runs, metadata, stage hashes, and external-target cleanliness reproduced. In-tree Cargo emits eleven inherited unused-root-patch warnings. There is no mechanical A/D independence receipt, caller-supplied `BBNF_ASSAY_OUT`, artifact publisher, reciprocal receipt schema, fault/race/corruption matrix, clean-output artifact twin, web/package build, installed WASM target, or Apple profile. Repeatable skeleton digests are not product artifact closure. |

## Finding-by-finding adjudication — critic B

| Finding | Ruling | Independent proof |
|---|---|---|
| **B01 — slice does not test the Family A product hypothesis** | **CORRECT** | The only public successful chain is source acquisition, opaque rule extraction, an identity wrapper, and one dependency graph. `compile` always returns `Incomplete`. No fact/plan/code/value/publication/consumer risk distinctive to Family A is exercised, while the binding portfolio already assigns the complete staged experiment to VT-1. |
| **B02 — W0 and normative topology are absent** | **CORRECT** | The corrected expected-path loop reports `missing_required_paths=42`: 13 production modules, 13 source-isomorphic tests, and 16 web/package/test files. Therefore placeholders, RED APIs, privacy/revision craters, protocol, and mechanical independence are not implemented despite `STATUS.json` saying W0 is implemented. |
| **B03 — statement scanner is not a BBNF parser** | **CORRECT** | The same four invalid/incomplete/trailing cases in A-03 all succeeded. `definitions` only tracks quote/slash/escape state while splitting at `;` and `=`; `contains_identifier` is lexical. The tiny fixture does not falsify these missing language features. |
| **B04 — identity ownership is structurally false** | **CORRECT** | One `Digest` type represents every existing digest, `RuleId` is only `u16`, constructors are public, and `TypeSolution` is a public zero-sized constructible value. The foreign-ID probe returned the unrelated program's rule. The one types-stage equality check correctly rejected a mixed graph/normalized pair, but does not close the other forgery routes. |
| **B05 — normalization/G3 are not semantically assayed** | **CORRECT** | The direct G3 probe succeeded with `rewrites=0`, `deleted=0`, one rule remap, and the opaque body preserved. There is no executable grammar/code/value layer, so epsilon preservation cannot be observed and no altered publication can be rejected. `NodeRemap` also has no owning revisions. |
| **B06 — exact JSON SCC is a narrow lexical coincidence, not a general graph** | **CORRECT** | The locked edge/SCC/condensation observation reproduced exactly. The host-input example generated a false rule edge, proving that lexical role mining can create false dependencies/cycles. All other required typed graph projections and reports are absent. |
| **B07 — row assay violates BBNF-only source law** | **CORRECT** | The type and row facts are handwritten/public, duplicate labels are already collapsed by `BTreeMap`, and `unify` is equality only. No variables/open tails/lacks/occurs/recursion/levels/substitution/budget exist. The 1,024 loop has neither seeds nor distinct permutations. |
| **B08 — resources count containers rather than semantic work** | **CORRECT** | The 4,001-leaf case with `nodes=1`, two unbounded defaults, eight unused fields, mutable maxima, and uncharged parser/reference/SCC work reproduced. Typed source and final-edge failures are real narrow positives, not total transactional accounting. |
| **B09 — corpus, craters, and test structure are mostly decorative** | **CORRECT** | `rg` found only the G3 fixture reference; all 11 JSON/bench files are inert. `tests/craters.rs` is an ordinary runtime assertion, not a compile/package crater. The test named as a second module rereads `json.bbnf`; only the separate crater reads `tiny.bbnf`. Existing implemented modules do have external tests and production has no inline tests, but missing owners and unconsumed fixtures deny broader credit. |
| **B10 — receipts preserve bytes but cannot close artifact truth** | **CORRECT** | Checksums, toolchain, root-lock preservation, external-target build, and repeatable hashes are accurate. `W0` is overstated; commands/outputs are prose rather than a canonical reciprocal receipt set; in-tree warnings survive; no `BBNF_ASSAY_OUT`, TS/WASM/web, artifacts, publication, determinism/race/corruption, or Apple evidence exists. `rustup ... target list --installed` reports only `aarch64-apple-darwin`, so the required WASM lane is blocked rather than green. |
| **B11 — greater-product disposition is RED/routed** | **CORRECT** | The slice changes no production package and creates no shipping consumer edge. It also closes no PT lock, BB-1/2/3/7, runtime JSON truth, CSS L4, value API, bootstrap, backend, package, ARM, profile, or unseen-holdout gate. It cannot count in VT-1...VT-12, win by default over unstarted D, authorize movement, or donate an unmeasured implementation. |

## Overlap, differences, and evidentiary drift

The substantive overlap is:

| Common defect | Critic A | Critic B |
|---|---|---|
| Non-discriminating total-tranche experiment | total-tranche ruling, A-01/A-08 | B01/B11 |
| False W0/topology/corpus credit | A-05 | B02/B05/B09 |
| Lexical non-BBNF source carrier | A-03/A-08 | B03/B06 |
| Unowned identities/public stages | A-04 | B04 |
| Handwritten JSON type/row facts | A-02/A-05 | B07 |
| Rule-only normalization/partial graph | A-07 | B05/B06 |
| Bypassable/incomplete resources | A-06 | B08 |
| Narrow receipts, absent artifacts/crossings/cost | A-09 | B10/B11 |

There is no material factual disagreement between the critics. Three differences
needed an adjudicator ruling:

1. Critic A states the higher semantic-source impossibility explicitly; critic B
   limits itself to the handwritten facts and routed remainder. A's stronger claim
   is correct for the exact `key`/`Null: Unit` contract under the current higher
   law.
2. A calls the prototype a second source/type authority; B says it creates no
   *shipping* second authority. Both are true at different scopes: it is a shadow
   authority inside the assay but has no production/package consumer edge.
3. A asks to retire/delete after banking counterexamples; B asks to retain only
   source/SCC observations as assay evidence. The combined disposition is
   **RETIRE implementation, BANK observations/counterexamples, DELETE-AFTER the
   owning evidence bank has absorbed them**. No source code is a donor.

The critic input ledgers were captured at different governing-file revisions.
Current adjudication hashes are:

```text
006066f64915f02020102776093c4a519e3466fe6c31a3bbcfcfd8fef4a611e5  LOCK-CONSTITUTION.md
1e0a69f828130a37358bbf1cde34aac0579caec43f44a273fa7d1449d54384f3  CHALLENGE-LEDGER.md
ffea9974b0ecf8723e7b1b83ff1394167d0419cc973c0f6fc0e867a28bf03b39  TRANCHE-PORTFOLIO.md
36f88bf72043c95a33b9e0f814b08634664fa018ac2a6396d18a319e3c7bfc64  waves/BB-JSON-P0.md
0564548b2e7149e9ec8f9651c23f66ba277fa21db5ed8406c52f0be66aa1dfb0  waves/BB-VALUE-TAPE-P0.md
```

The lock, portfolio, and BB-JSON hashes agree across all material reviews; the
challenge ledger and, for critic A, BB-VALUE changed afterward. The current texts
make Family A's RED status and the complete-portfolio supremacy more explicit;
the drift does not reverse any finding.

## Complete implemented-feature and wave-fidelity audit

| Surface | Adjudicated truth |
|---|---|
| Standalone manifest/lock/toolchain | **Narrow PASS.** Empty `[workspace]`, registry dependencies, pinned lock, format/clippy/tests, and out-of-tree resolution work. In-tree Cargo still discovers root patch configuration and emits eleven warnings. |
| `Digest`/canonical serialization | **Narrow PASS / identity FAIL.** SHA-256 and deterministic JSON serialization repeat for the current structs. One interchangeable type and absent semantic identity violate the identity contract. |
| `Budget` | **Partial FAIL.** Source/rule/final-edge checks return typed errors. Hard maxima are mutable, syntax/operations are not charged, eight future fields are inert, and two defaults are unbounded. |
| `SourceUnit` | **Narrow PASS.** Exact UTF-8 bytes, length, and SHA lock work. Public unlocked acquisition is lawful only if a real parser rejects invalid modules; the current downstream does not. |
| `Program` | **FAIL.** Deterministic rule strings for two fixtures are not a parsed semantic program. Invalid/trailing/undefined syntax is accepted, host identifiers become rule edges, directives are discarded, and entry is guessed as last rule. |
| `NormalizedProgram` | **FAIL as the specified normalizer.** It is a deterministic clone plus rule-index remap, but has no syntax-node/operator semantics or revision-qualified bijection to preserve. |
| `DependencyGraph` | **Narrow PASS / total graph FAIL.** Iterative SCC and the locked 13-edge observation are correct. The representation has one untyped rule-reference projection and demonstrable role confusion; all other projections/reports are missing. |
| `Type`/`ClosedRow`/`TypeSolution` | **FAIL.** Publication correctly returns `Incomplete`, but public handcrafted JSON facts and a forgeable empty solution violate sole-source/transactional authority. Closed-row equality is not the required solver. |
| `assay`/`compile` | **Assay-only.** `assay` exposes public intermediate fields. `compile` always returns a typed hard stop. No Family A authority boundary is reached. |
| Rust safety | **Narrow PASS.** `unsafe_code` is forbidden and no unsafe source exists. No document/runtime/borrowed-value implementation exists, so this says nothing about the wave's lifetime/value safety. |
| External test topology | **Partial PASS.** Existing implemented modules have external tests and source has no inline tests. Forty-two normative owner/test/web paths are absent, privacy/revision craters do not exist, and `craters.rs` is not a compile crater. |
| G3/oracle/corpus | **FAIL.** G3 is a source-lock mismatch; JSON corpus and benches are inert; complete-input values/spans/duplicates/Unicode/numbers/diagnostics/resources are not executable. |
| Facts/plan/code/runtime/value | **NOT IMPLEMENTED / RED.** Honest hard stop, no feature credit. |
| Rust/TypeScript/WASM/browser/package | **NOT IMPLEMENTED / RED.** No web tree or TS/WASM artifacts; WASM target is not installed. |
| Artifact publication/determinism/injections | **NOT IMPLEMENTED / RED.** Repeatable in-memory digests do not substitute. |
| Apple/aarch64 layout/performance/PMU | **NOT RUN / RED.** The receipt is honest; no cost or selection claim is possible. |
| Evidence/status | **Mixed.** Checksums, source/stage hashes, toolchain, movement denial, and incompleteness are accurate. `W0: IMPLEMENTED`, row-permutation breadth, and any implication of corpus/G3 closure are false. |

Wave status is therefore: W0 **RED**, W1 **partial and falsified**, W2 **RED hard
stop**, W3-W9 **not reached**. Family D is not present, so the two-family selection
question remains unanswered; A cannot win by default.

## KISS, generality, resources, and product fit

### KISS and structure

There is no god module or shipping SCC in this small crate. Its structural failure
is sand: wrappers, public future fields, copied fixtures, and a shadow source/type
authority without the invariants that would justify them.

- **DELETE, do not donate:** `Type`, `ClosedRow`, the zero-sized public
  `TypeSolution`, and their JSON-specific test.
- **COLLAPSE or keep absent:** the identity-only normalizer until a real parsed
  syntax arena and revision-qualified remap exist.
- **DELETE future placeholders:** unused budget fields until implemented stages own
  hard clamped limits and charge real work.
- **REPLACE, do not extend:** `definitions`/`contains_identifier` with the one
  authoritative, total BBNF module parser. Special-casing comments, hosts,
  decorators, or rule names here would deepen the shadow authority.
- **BANK bytes centrally, then delete local copies:** inert corpus/bench fixtures
  must not earn implementation credit; a common oracle may retain them.

The exact-source lock, external-test habit, deterministic ordering, and iterative
SCC are useful observations/techniques, not code donors. Independence law forbids
copying helpers into VT candidates; each must re-earn them.

### Generality and resources

The tiny fixture changes names/literals/delimiters, but does not test the BBNF
module system, imports, namespaces, decorators, host signatures/effects, recovery,
or an unseen frozen grammar. JSON-specific public type facts already violate the
no-overfit law. Resources are neither hard maxima nor a shared multidimensional
cost policy; only four container counts are partially checked. There is no
measured cost or product-wide resource conclusion.

### Greater-product effect

The crate adds no shipping dependency, artifact, or consumer and does not touch
production. That preservation is good. If continued, however, its lexical BBNF
transducer, identity scheme, JSON type table, and graph would become second
authorities beside BBNF/parse-that and duplicate the VT-1 experiment. It therefore
reduces no remaining product risk beyond the banked negative counterexamples.

## Exact survivor, retirement, and successor disposition

1. **Family A implementation:** `RETIRE / RED / DELETE-AFTER-EVIDENCE`. Freeze it
   only long enough for the owning evidence bank to absorb the observations below;
   do not repair or continue W2.
2. **Surviving observations:** `BANK` only the exact source identity; ten extracted
   rule names; the locked 13 lexical rule edges; seven SCCs; recursive component
   `{array,pair,object,value}`; six outgoing condensation edges; and the fact that
   narrow source/final-edge limits can return typed errors. These are census facts,
   not architecture features.
3. **Surviving counterexamples:** route invalid/trailing modules, unresolved
   references, host/rule collision, foreign `RuleId`, direct G3 non-assay,
   absent-field/`0u8` source conflict, semantic-node undercharging, and hard-cap
   raising into the authoritative BBNF-language and complete-candidate born-RED
   oracles. Preserve assertions/observations, not this implementation.
4. **No donor code:** the scanner, handwritten types/rows, identity wrapper,
   budget shell, receipts, and unused fixtures receive no `HYBRID-DONOR` status.
5. **BB-JSON-P0:** supersede/terminate it as a closable two-family wave on the
   frozen carrier. It authorizes no production movement or hybrid.
6. **Family D under this frozen wave:** `BLOCK / RETIRE-BEFORE-START`. It faces the
   same absent-semantic-source contradiction and may not use A. The independently
   verified-module design family survives only as the later complete VT-4
   experiment.
7. **Architecture families:** immutable staged compilation is not globally
   refuted; it routes to a fresh, fully independent VT-1. Verified modules route
   independently to VT-4. Neither may import this crate or win by default.
8. **Prerequisite successor order:** first make modules/imports/decorators,
   versioned prelude/host signatures, exact semantic/type/public-shape facts, and
   effects expressible in BBNF bytes under their own authority; finish/freeze the
   exhaustive CSS-L4 union and oracle; then implement complete VT candidates across
   every PT/BB/value/backend/package/resource/profile/holdout gate. Only two
   consecutive fresh clean hostile adjudications can advance a survivor.
9. **Clean count:** remains **0**. No finding was repaired, so Gestalt 2 cannot be
   treated as a clean continuation on this ledger.

## Independent execution record

All repository commands ran from `/Users/mkbabb/Programming/bbnf-lang`. The exact
family path was
`restart/skinny/tranches/sk-v25/prototypes/json-vertical-a`; all Cargo build output
went under `/tmp/bbnf-json-a-adjudication.7rxka1`. No family-local target appeared.

### Input and status binding

```sh
git status --porcelain=v2 -z | shasum -a 256
shasum -a 256 restart/skinny/tranches/sk-v25/LOCK-CONSTITUTION.md \
  restart/skinny/tranches/sk-v25/CHALLENGE-LEDGER.md \
  restart/skinny/tranches/sk-v25/TRANCHE-PORTFOLIO.md \
  restart/skinny/tranches/sk-v25/waves/BB-JSON-P0.md \
  restart/skinny/tranches/sk-v25/waves/BB-VALUE-TAPE-P0.md \
  restart/skinny/tranches/sk-v25/prototypes/json-vertical-a/evidence/GESTALT-1-CRITIC-A.md \
  restart/skinny/tranches/sk-v25/prototypes/json-vertical-a/evidence/GESTALT-1-CRITIC-B.md
cmp -s grammar/json/json.bbnf \
  restart/skinny/tranches/sk-v25/prototypes/json-vertical-a/tests/fixtures/source/json.bbnf
```

Results: status stream SHA-256 was
`306f7914cfc050cf6d7e7c9ea1c05a33833f70bc9d9d48c1f3f5ae9c706ad2c5`;
the governing/report hashes are recorded above; `cmp` exited 0. The same status
hash recurred after every material assay and immediately before this report write.

The complete input-tree binding was produced by:

```sh
family='restart/skinny/tranches/sk-v25/prototypes/json-vertical-a'
(
  cd "$family"
  find . -type f ! -path './evidence/GESTALT-1-ADJUDICATION.md' -print0 |
    sort -z |
    while IFS= read -r -d '' input_file; do
      file_hash=$(shasum -a 256 "$input_file" | awk '{print $1}')
      printf '%s  %s\n' "$file_hash" "${input_file#./}"
    done
) | shasum -a 256
```

Result: `2ba6118c2c480093c926e195e2150dc420c9ba77f9249946ac94a7585839905d`.

### Author replay

```sh
family='restart/skinny/tranches/sk-v25/prototypes/json-vertical-a'
target='/tmp/bbnf-json-a-adjudication.7rxka1/author-target'
(cd "$family" && shasum -a 256 -c evidence/source-checksums.sha256)
CARGO_TARGET_DIR="$target" cargo +nightly-2026-04-10 fmt \
  --manifest-path "$family/Cargo.toml" -- --check
CARGO_TARGET_DIR="$target" cargo +nightly-2026-04-10 clippy \
  --manifest-path "$family/Cargo.toml" --locked --all-targets --all-features -- -D warnings
CARGO_TARGET_DIR="$target" cargo +nightly-2026-04-10 test \
  --manifest-path "$family/Cargo.toml" --locked --all-features -- --test-threads=1
CARGO_TARGET_DIR="$target" cargo +nightly-2026-04-10 test \
  --manifest-path "$family/Cargo.toml" --locked --all-features -- --test-threads=1
CARGO_TARGET_DIR="$target" cargo +nightly-2026-04-10 metadata \
  --manifest-path "$family/Cargo.toml" --locked --format-version 1 --no-deps \
  >/tmp/bbnf-json-a-adjudication.7rxka1/metadata.json
shasum -a 256 /tmp/bbnf-json-a-adjudication.7rxka1/metadata.json
```

Results: all 41 checksum rows were `OK`; fmt, clippy, both test runs, and metadata
exited 0. Each test run reported 13 passed, 0 failed, 0 ignored. Cargo emitted the
same eleven inherited unused-patch warnings. Metadata SHA-256 was
`de6db1bedf7962cb19f6b4a5a946be22cab081934556f3d030c6332bb58c83da`.

The focused digest replay was:

```sh
CARGO_TARGET_DIR=/tmp/bbnf-json-a-adjudication.7rxka1/author-target \
  cargo +nightly-2026-04-10 test \
  --manifest-path restart/skinny/tranches/sk-v25/prototypes/json-vertical-a/Cargo.toml \
  --locked --all-features stage_result_digests_are_reported -- \
  --nocapture --test-threads=1
```

It exited 0 and printed:

```text
source     ab8d21218b2a5f49d9231af6f71503a9949782fdc0a50d81a845fc69054f2604
program    908129a7e097b1070ac1b7ac587cd242999ca8672057c13faf22abf41051ca71
normalized 650f68b18ef9a566ed0857feead3afc4c232fec9c606dec3ca26ffbeab47ccaa
graph      f0e97cd27cb8d67d1b86e310c823bb62a976d6c4fbdb092f24eca4c24e6cd8ac
```

### Adjudicator hostile probe

The family was copied outside the repository with `rsync -a --exclude target`.
Only the copy received `tests/adjudication_probe.rs`; the subject did not. The
probe SHA-256 was
`72c0f18e9d695ce347374458f96e09d42af01ab5016a2d531710770234484060`.
It asserted the observed defective behaviors and the two narrow working guards:

- invalid/incomplete/trailing modules succeed;
- a host argument creates a rule edge;
- direct G3 assay is a zero-rewrite rule wrapper;
- a foreign `RuleId` resolves in another program;
- public stage, unbranded digest, forged solution, and handwritten JSON row/type
  construction compile;
- 4,001 syntax leaves pass `nodes=1`, defaults are unbounded, and a raised hard
  source ceiling succeeds;
- the types-stage mixed-revision guard and final graph-edge budget do return typed
  errors;
- the exact locked 13-edge/seven-SCC/six-condensation-edge narrow fact holds.

```sh
CARGO_TARGET_DIR=/tmp/bbnf-json-a-adjudication.7rxka1/hostile-target \
CARGO_NET_OFFLINE=true \
  cargo +nightly-2026-04-10 test \
  --manifest-path /tmp/bbnf-json-a-adjudication.7rxka1/hostile/Cargo.toml \
  --locked --test adjudication_probe -- --test-threads=1
```

Result: exit 0; **8 passed, 0 failed, 0 ignored**. A passing hostile probe here is
proof that each asserted defect is reachable, not a clean subject result.

### Structure, reachability, tooling, and locks

A literal loop over the 42 normative production/test/web paths listed by
BB-JSON section 5 returned `missing_required_paths=42`. Reachability scans were:

```sh
rg -n 'fixtures/(corpus|bench)|corpus/(positive|negative)|bench/(tiny|medium|large)' \
  "$family/src" "$family/tests" || true
rg -n '#\[cfg\(test\)\]|#\[test\]' "$family/src" || true
rg -n '\b(key|Object|Array|Pair|Json|Null|Unit|schema|field|variant|entry)\b' \
  "$family/tests/fixtures/source/json.bbnf" || true
rg -n 'null =|pair =|value =' "$family/tests/fixtures/source/json.bbnf"
rustup +nightly-2026-04-10 target list --installed
test ! -d "$family/web"
find "$family" -maxdepth 3 -type f \
  \( -name '*.wasm' -o -name '*.ts' -o -name package.json -o -name package-lock.json \) -print
git diff --exit-code -- Cargo.lock
shasum -a 256 Cargo.lock "$family/Cargo.lock"
```

Results: the only corpus/bench reference was `tests/born_red.rs` including
`g3.bbnf`; no production inline tests or required semantic-source terms were
found; the three source rules printed as shown above; only
`aarch64-apple-darwin` was installed; `web` was absent; no TS/WASM/package files
were found; root lock diff exited 0. Root/family lock SHA-256 values were
`a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908`
and `2d2efd5806186b28df89065e52639ec04b267d6059bd6e0c45d7eb26d28b3618`.

One preliminary topology shell invocation used zsh's special `path` variable as a
loop name. It did enumerate the same 42 missing paths, then clobbered command
lookup and caused its remaining `rg`/`wc`/`git` subcommands to exit 127. The
corrected loop used `expected_file`; all results reported above are from that
successful rerun. This tooling error changed no repository bytes and supports no
finding.

## Final ruling

**RED, clean count 0.** The source lock and one lexical JSON SCC census are real,
but the frozen module cannot provide the mandated exact schema under the higher
BBNF-only law. The implementation compounds that conflict with handwritten JSON
facts, a non-total BBNF scanner, unowned identities, public/forgeable stages,
vacuous G3/corpus/W0 claims, and bypassable resources. Retire this implementation,
bank only the negative/census evidence, terminate the frozen BB-JSON two-family
wave, and re-run the immutable-stage and verified-module ideas only as complete,
independent VT candidates after BBNF semantic modules and the exhaustive CSS-L4
oracle are total. No production movement is authorized.
