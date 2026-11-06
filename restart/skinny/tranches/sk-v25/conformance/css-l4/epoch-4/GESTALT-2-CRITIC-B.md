# CSS L4 W0 epoch 4 — Gestalt 2 Critic B

Date: 2026-07-19  
Role: second fresh local correctness critic  
Canonical task: `/root/css_w0_epoch4_review_b`  
Model / effort: `gpt-5.6-sol` / `xhigh`  
Delegation: none

## Review boundary

Before repository inspection I declared the exact model, effort, canonical task,
and no-delegation boundary. I read the current epoch-4 implementation, tests,
fixtures, deterministic data, retained-input manifest and policy, README, AUTHOR,
the full Lock Constitution, the full governing `BB-VALUE-TAPE-P0` wave, and the
epoch-3 Gestalt-1 adjudication. I did not inspect any prohibited epoch-4 review or
campaign/session material, and I did not inspect repository history or diffs.

No network operation, fetch, pin change, subject edit, production edit, cache
creation, or candidate creation was performed. Fresh counterexamples and the
two-root proof ran only in disposable `/tmp` roots. The only file written in the
subject is this critic report, which is outside the sealed subject by the declared
root `GESTALT-*.md` review exclusion.

Replayable bytes are treated below as narrow evidence only. They do not establish
CSS semantics, CSS-L4 completeness, candidate completeness, a product, or
benchmark credit.

## Verdict

**GESTALT 2 CRITIC B: RED. Seven findings: five P1, two P2. Consecutive-clean
count: 0.**

The epoch-4 repair is materially smaller and more candid than epoch 3, and its
stored terminal boundary is honest. It does not, however, close all adjudicated
repair mechanisms. The integrated evidence gate, independent denominator,
dialect parsers, future-candidate crater, and claimed twenty-mutation coverage
remain fail-open. WPT-rest membership is overinclusive, and genesis cleanliness
does not reject untracked worktree inputs.

Candidate count is **0** and complete-candidate count is **0**. Candidate credit is
**NONE**. No tranche, wave, product, package, production, clean-pass, control, or
performance movement is authorized.

## Findings

### Finding 1 — P1 — the integrated claim/evidence gate still accepts fabricated success

**Location.** `src/common.py:129-137`; `src/pipeline.py:426-469`;
`tests/mutations.py:25-26,152-155`.

**Reproduction / evidence.** I deep-copied the checked observation document,
changed the four terminal labels to `PASS`, `PASS`, `GREEN`, and `COMPLETE`,
removed the evidence receipts from every `NORMATIVE_CSS_SOURCE` and
`EXTERNAL_PARSING_AUTHORITY` dependency decision, and called
`pipeline.validate_document()`. It accepted the document:

```text
integrated_gate_counterexample ACCEPTED PASS PASS GREEN COMPLETE 937
```

The last number is the count of positive dependency decisions whose receipts were
emptied. `validate_claim_boundary()` rejects only the exact local spelling
`VALID` with blockers; it does not enumerate the allowed state for any plane or
bind the terminal labels to their counts. `validate_positive_state()` is tested
only as an isolated helper and is never called while validating the artifact.
Even that helper checks only receipt-list nonemptiness, while `evidence()` checks
digest length rather than lowercase-hex shape or referent content.

**Required correction.** Make each plane an exact closed enum with a cross-field
invariant: the current bytes must permit only `VALID`, `BLOCKED`, `RED_OPEN`, and
`ABSENT_RED` at the stored boundary. Validate the schema, digest, identity,
referent, ownership, and state compatibility of every positive receipt, including
dependency decisions. Route the A-01 and B-F06 adversarial documents through
`validate_document()` rather than testing detached helpers or only one privileged
spelling.

**Effect.** The stored artifact itself remains honestly RED, but the executable
repair for adjudicated A-01/B-F06 is not a fail-closed authority. A later edit can
launder the same open facts into green labels without evidence. This awards no
candidate or product credit and keeps the clean count at zero.

### Finding 2 — P1 — the source-semantic denominator is neither failure-total nor atomic at its advertised source-native units

**Location.** `src/pipeline.py:268-359,446-459` and
`data/observations.json` at `source_semantics`.

**Reproduction / evidence.** Three independent checks fail the denominator claim.

First, a minimal normative unclosed carrier is silently discarded because all
`element_body()` exceptions are caught and continued:

```text
input:  <pre class="propdef">Name: x\nValue: a
result: 0 census rows, 0 blockers
```

The same broad suppression occurs in the dfn and heading passes. A detector
failure therefore becomes absence instead of a typed blocker.

Second, deleting the last stored census row and decrementing
`occurrence_count` from 4,954 to 4,953 still passed `validate_document()`. The
A-06 test deletes a row without updating the redundant count, so it proves only
that two self-counts agree; it does not compare against an independently rederived
denominator.

Third, the current artifact nests source-native units beneath one carrier identity
and one carrier-wide disposition. Sixty-five carrier rows contain more than one
unit, with as many as ten units under one disposition. Nested current units include
287 productions, 13 Yacc rules, and 152 WebIDL declarations, but they do not
receive independent IDs, dispositions, or reverse links. Structural algorithms
are likewise one unit per carrier rather than source-exact step occurrences. This
does not satisfy the adjudicated requirement that Lex/Yacc rules, complete WebIDL,
productions, and structural steps be atomic source-native denominator entries.

**Required correction.** Turn every extraction failure into a source-exact typed
blocker. Emit each parsed source-native unit as its own stable occurrence with its
own span, normativity, disposition, and reverse links. Bind the artifact to a
freshly extracted denominator digest and validate family totals, disposition
totals, reverse links, and blocker coverage against that denominator. Add deletion
mutations that repair all redundant counters and still must fail.

**Effect.** The 4,954 count is a replayable representation fact, not evidence that
the source-semantic union is total. The current 1,220 blockers remain honest, but
additional detector losses can disappear without blockers. CSS coverage and wave
fidelity remain RED; no candidate effect follows because no candidate exists.

### Finding 3 — P1 — the replacement dialect parsers still accept fresh malformed complete-language inputs

**Location.** `src/languages.py:74-99,313-372,375-447,450-531` and
`tests/mutations.py:37-52`.

**Reproduction / evidence.** The following fresh probes were all accepted:

```text
parse_css_vds("{")
parse_yacc("a: @@@ ;")
parse_lex("%option utterly_bogus\nA a\n%%\na return A;\n")
parse_webidl("interface X @@@ { attribute DOMString x; };")
```

They returned, respectively, a delimiter node, a Yacc AST, a Lex AST, and a
WebIDL AST. The causes are direct: bare `{` is tokenized as a delimiter; Yacc
rejects only a small blacklist and never parses RHS token syntax; Lex accepts any
identifier-shaped option; and WebIDL searches forward from a recognized head to
the next brace or semicolon without validating the intervening declaration
grammar. The existing A-03 test kills six fixed examples but does not exercise
these accepting paths.

**Required correction.** Replace the blacklist/envelope paths with consuming
grammars for the exact supported carrier dialects, or return a typed unsupported
blocker before any `PARSED`/mapped disposition. Add these probes and grammar-class
mutations, including unknown punctuation, unknown options, and garbage between a
WebIDL head and body.

**Effect.** Some current carrier dispositions can be promoted on recognizers that
do not establish their advertised dialect. This does not prove that any CSS
feature is present or absent; it proves only that the repair mechanism is not
total. Local semantics and the wave remain RED, with zero candidate credit.

### Finding 4 — P1 — the future-candidate crater accepts the exact checked observation artifact and invented runtime receipts

**Location.** `src/crater.py:13-21,47-125`; `tests/mutations.py:77-85,166-172`.

**Reproduction / evidence.** In a disposable candidate root I copied the exact
stored `data/observations.json` bytes to `oracle.bin`, referenced it as
`include_bytes!("oracle.bin")`, and supplied an empty self-authored
`resolved-inputs.json`. The crater returned:

```text
current_observation_oracle_crater VALID []
```

The checked observation bytes contain none of the small forbidden-term list, and
the path scanner ignores a quoted filename with no slash. In a second root I
declared a runtime read of a definitely absent path with a syntactically valid
all-zero SHA-256; the crater again returned `VALID []`. Runtime receipts are never
resolved or hashed. More generally, the self-authored trace is not checked against
an independently captured build/package/runtime graph.

The A-07 test detects only an old literal `semantic-matrix` marker and an explicit
`conformance/css-l4/...` path. B-F08 proves only that a missing trace fails. Neither
test kills the current observation bytes, bare relative filenames, fabricated
trace edges, environment/dynamic paths, or nonexistent runtime reads.

**Required correction.** Derive the graph from the actual compiler, build scripts,
package closure, generated outputs, symlink resolution, binaries, and instrumented
runtime reads. Canonicalize every resolved file identity, read and hash every
allowed input, and reject all known observation-artifact digests independent of
name or encoding. Treat the candidate-supplied trace as a claim to verify, not the
graph authority. Add the two reproduced bypasses as external mutations.

**Effect.** `NO_CANDIDATE_PRESENT` is still a true current-state fact, but the
future isolation mechanism is unproved and cannot guard any later candidate. No
candidate currently exists, so this finding awards no candidate casualty or
credit; it blocks future candidate credit until repaired.

### Finding 5 — P1 — twelve of the twenty registered “mutations” are positive/read-only assertions

**Location.** `fixtures/mutations.json`; `tests/mutations.py:25-208`;
`tests/proof.py:65-101`.

**Reproduction / evidence.** The registry has twenty unique IDs and every named
function executes once per regenerated set. That is the retained fact. It is not
twenty hostile mutations. Only eight entries currently inject a malformed input,
alter a document/record, forge a receipt, or omit a required graph:
`A-01`, `A-03`, `A-06`, `A-07`, `A-08`, `B-F06`, `B-F08`, and `B-F09`.

The other twelve—`A-02`, `A-04`, `A-05`, `A-09`, `A-10`, `B-F01`, `B-F02`,
`B-F03`, `B-F04`, `B-F05`, `B-F07`, and `B-F10`—inspect the current artifact or a
positive fixture and return a success string. A-09 merely checks set equality
between registry names and functions. The proof then writes
`mutation_execution_count: 20` as a constant rather than deriving per-ID typed
failures from executed mutation results.

**Required correction.** Give every adjudicated finding an external adversarial
fixture or document mutation, the exact owning gate, and an expected typed failure.
Capture the per-ID result records from the runner and derive the proof count from
those records. Positive regression fixtures may remain, but must not be counted as
hostile mutations.

**Effect.** The two-root proof establishes deterministic execution of one suite,
not executable closure of all twenty adjudicated counterexamples. The final
receipt remains valid for byte replay only; hostile-pass and clean-count credit
remain zero.

### Finding 6 — P2 — `wpt-rest` prefix expansion includes non-test support and metadata blobs while reporting reproduced membership

**Location.** `src/pipeline.py:375-423`, especially `393-397` and `417`;
`tests/mutations.py:138-142`; `data/observations.json` at `wpt`.

**Reproduction / evidence.** The one retained `wpt-rest` directive resolves the
prefix `css/css-variables/` by selecting every catalog blob with that prefix. Its
273 matches include:

```text
css/css-variables/META.yml
css/css-variables/WEB_FEATURES.yml
css/css-variables/resources/variable-reference-refresh-iframe.css
css/css-variables/support/external-variable-declaration.css
... four further support CSS files
```

Thus a path/blob-OID prefix proves repository membership, but it does not classify
WPT tests. The current test checks only that a `WPT_REST` directive exists and
that the global resolved count is nonzero. The artifact correctly retains
`ownership_status: UNMAPPED`, `execution_status: UNRUN`, and a blob-content
blocker, but `membership_status: REPRODUCED_WITH_TYPED_BLOB_BLOCKER` and README
wording overstate what the prefix expansion establishes.

**Required correction.** Retain and apply the pinned WPT manifest or the minimum
blob/metadata content needed to distinguish tests, references, support files, and
metadata before calling the result test membership. Otherwise rename this field
to prefix-blob membership and keep WPT test membership explicitly blocked. Add a
mutation containing support/metadata siblings under a `wpt-rest` prefix.

**Effect.** Exact WPT test membership is still RED. Ownership relations remain
zero, execution remains `UNRUN`, and outcomes remain zero, so no current row or
candidate receives false execution credit. Wave fidelity remains blocked.

### Finding 7 — P2 — genesis “clean checkout” accepts untracked inputs that can enter the captured source-key catalog

**Location.** `src/capture_inputs.py:56-62,97-120`; `inputs/manifest.json` at
`capture.csswg_clean` and `capture.wpt_clean`.

**Reproduction / evidence.** `require_checkout()` invokes status with
`--untracked-files=no`, so untracked files and directories cannot make the
checkout dirty. The CSSWG key catalog is then built by iterating the working-tree
directories and selecting working-tree `Overview.*` files. An untracked directory
or eligible file can therefore enter the captured catalog while the manifest
records `csswg_clean: true`. The same cleanliness helper is used for WPT, although
the WPT catalog itself is correctly derived from the pinned tree.

**Required correction.** Reject tracked and untracked changes, require every
selected/catalog source path to be tracked, and compare its bytes with the pinned
tree blob. Prefer deriving the CSSWG catalog from the pinned tree rather than from
working-tree directory enumeration. Record cleanliness only from that complete
check.

**Effect.** The current retained objects and catalog still have exact replayable
digests; this finding does not infer that they were contaminated. It invalidates
the general genesis cleanliness proof and weakens the asserted relation between
the CSSWG commit label and the captured catalog. Candidate and benchmark credit
remain none.

## Facts that survive review

- The governing hashes match the current Lock Constitution
  (`b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5`),
  wave (`745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562`),
  and epoch-3 adjudication
  (`e29fde0532d273fe1c5d730cff40555858d7a68914f461c73b0758ff75469736`).
- All ten epoch-owned JSON documents inspected here decoded with duplicate-key
  rejection: 3,764,975 bytes and 120,171 recursively visited nodes.
- The input manifest contains 109 logical object records and the store contains
  exactly 109 matching physical SHA-256 objects, totaling 43,013,202 bytes.
- The seven-test offline runner passed. A fresh two-empty-output-root proof exited
  zero, regenerated byte-identical A/B artifacts, and matched both stored checked
  artifacts. This is valid narrow byte-replay evidence.
- `data/observations.json` is 3,582,193 bytes at
  `d320298d5cf2fd33f23b58830c2cf77eaaf4d91493812504d404eba234bd7267`;
  `data/boundary.json` is 408 bytes at
  `4e61a7f1384dca31803cf45cbb91b3a19f84569ee7966c0d5b94e8d3e95604d5`.
- The final non-review manifest and receipt agree on 134 files, 46,776,370 bytes,
  and subject SHA-256
  `12023cbd61f34e47e0aafde47cdc43611a6589f4b7a4ccd6dbb5e1adadaea9d5`.
- The stored boundary is candid: byte replay `VALID`, local semantics `BLOCKED`,
  downstream `RED_OPEN`, and candidate product `ABSENT_RED`.
- The current data retains 2,971 dependency decisions, 4,954 census carrier rows,
  1,220 unique typed local blocker instances, 8,796 atomic open obligations, one
  open plane vector, one RED terminal decision, and 8,864 reverse links.
- The occurrence-qualified dependency multimap retains 38 collision keys and does
  not choose a first collision winner. The current inherited normativity examples,
  railroad maximum-descendant envelope, and typed Unicode/ISO10646 blockers
  reproduce.
- WPT ownership and Cartesian fanout relations remain zero; execution is `UNRUN`;
  actual outcomes are zero. Lightning CSS and sonic-rs remain entirely `UNRUN`.
- Final-receipt sealing and the explicit review/self-reference exclusion reproduce.
  The normalization is substantially smaller than epoch 3 and retains the atomic
  inherited obligation identities without cloning their plane payloads.

These facts establish only deterministic retained bytes and honest open state.
They do not establish source-semantic completeness, CSS support, candidate
completeness, a product, or performance.

## Tranche, wave, mechanism, and greater-product assessment

| Plane | Assessment |
| --- | --- |
| Tranche fit | **RED, direction retained.** A sealed read-only CSS observation assay remains appropriate W0 work and is much smaller than epoch 3. It is not a completed W0 route because the JSON oracle, incumbent profiles/hot leaves, four compiling placeholders, and external control freezes remain explicit RED placeholders, while five repaired mechanisms still fail locally. |
| Wave fidelity | **RED.** The wave requires an exhaustive CSS/WPT union and source-native matrix. Honest open rows and zero outcomes are correct, but the denominator is not atomic/total, dialect acceptance is fail-open, and WPT-rest does not yet establish test membership. |
| Feature truth | **RED with narrow bytes retained.** Object identities, deterministic artifacts, normativity examples, collision blocking, railroad extent, final sealing, and all-open state reproduce. Integrated evidence validation, carrier totality, complete dialect consumption, crater isolation, and hostile-mutation coverage do not. |
| Greater-product effect | **RED / no current shipping edge.** No candidate or product consumes these observations, which avoids a present product casualty. The fail-open validator and crater would become a second semantic authority and an oracle-input edge if reused by a future candidate. |
| Candidate effect | **None exists.** Candidate count 0; complete-candidate count 0. No candidate can be ranked, banked, retired, advanced, benchmarked, or credited from this assay. |

## Final disposition

**Explicit verdict:** RED  
**Finding count:** 7 total — 5 P1, 2 P2  
**Consecutive-clean count:** 0  
**Candidate credit:** NONE — 0 candidates, 0 complete candidates  
**Movement authorization:** NONE

Retain the exact byte receipts, honest RED boundary, collision-total occurrence
ledger facts, known normativity repairs, railroad envelope, normalized inherited
row identities, and final sealing. Reopen and correct the seven mechanisms above,
then begin the fresh hostile-review sequence again at clean pass 1.
