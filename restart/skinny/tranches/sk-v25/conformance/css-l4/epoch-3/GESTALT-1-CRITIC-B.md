# CSS L4 W0 epoch 3 — Gestalt 1 blind hostile Critic B

Date: 2026-07-19  
Role: second fresh, strictly blind hostile critic  
Canonical task: `/root/css_w0_epoch3_critic_b_r2`  
Turn-context model / effort: `gpt-5.6-sol` / `xhigh`  
Session ID: `019f7685-254a-7a22-9917-1da91f977861`  
Subagent thread ID: `019f7985-ab4d-77f3-a796-7b3e94aa7d8f`  
Delegation: none  
Network, fetch, retry, pin change, or in-place regeneration: none  
Files edited: this report only

## Blindness and review boundary

The model-truth gate passed before repository inspection. I did not read any
epoch-3 `GESTALT-*.md`, any RUN/CAMPAIGN/CHALLENGE/WALLS ledger, any prior critic
or adjudicator report for epoch 3, or any reviewer message. No prohibited context
appeared.

I read the three permitted governing documents completely; read all epoch-3
README/AUTHOR, source, tests, fixtures, manifests, receipts, and input JSON;
fully decoded and recursively traversed every generated JSON value; and read,
length-checked, and SHA-256-checked all 105 retained content objects (67,196,482
bytes). I independently inspected the selected CSSWG source bytes and WPT tree
through the declared checkouts. All probes and proof outputs were written only
under `/private/tmp`.

## Terminal verdict

**GESTALT 1: RED. Consecutive-clean count: 0.**

The narrow present-machine byte replay is deterministic, and the author correctly
labels the semantic portfolio RED. It is not a clean hostile pass. Correct
blocker-class defects remain in source-dialect semantics, reference closure,
normativity, algorithm inventory, WPT completeness, external authority closure,
stage/status gating, input sealing, clean-root replay, boundary enforcement, and
the final stored proof receipt. KISS/resource defects remain as well.

No CSS coverage, CSS-L4 completeness, VT.W0 completion, PT/BB lock closure,
candidate count, complete-candidate count, clean-pass count, performance result,
ranking, production movement, source movement, or downstream transposition is
authorized. The epoch remains an incomplete defective input assay; candidate
count and complete-candidate credit remain zero.

## Numbered findings

### F01 — BLOCKER: the claimed strict legacy Lex dialect drops every scanner rule and every macro reference edge

`parse_lex()` recognizes `%option` and any line shaped like `NAME payload`; it
does not split or parse the definitions and rules sections around `%%`. On the
actual retained carriers:

- `CSS2::legacy-grammar::3` has 39 nonblank live lines and 1,296 bytes after
  `%%`; none becomes a semantic definition or row.
- `selectors-3::legacy-grammar::3` has 24 nonblank live lines and 829 bytes after
  `%%`; none becomes a semantic definition or row.
- The module artifact emits only 68 pre-`%%` macro definitions. Their patterns
  contain 62 `{macro}` reference occurrences, but `reference_closure` contains
  zero rows sourced from those definitions because each pattern is stored as an
  opaque `lex-pattern.value` string.

Thus token rules such as `{string} -> STRING`, `{ident} -> IDENT`, numeric/unit
rules, comments, bad strings/URIs, CDO/CDC, and the catch-all rule disappear. The
stage nevertheless reports every retained Lex carrier `PARSED`, no strict-stage
root blocker, and total AST reference closure. This is current-byte semantic
loss, not a hypothetical future mutation.

The same validator also accepts malformed standalone probes such as a Lex rule
body `BROKEN RULE BODY` as a macro and unbalanced Yacc RHS `a: b [ c | d ;` as a
parsed production. The external negative suite does not exercise either loss.

### F02 — BLOCKER: normativity is still structurally wrong and contaminates five matrix rows plus dependency edges

`extract._normativity()` checks selected ancestor classes but never checks the
carrier's own classes and does not treat `class=note` as non-normative. Exact
current casualties are:

- `css-ruby-1::propdef::1` is literally
  `<pre class="propdef partial informative">`; it is marked `NORMATIVE` and
  creates two entity-field rows plus one extension row.
- `css-images-4::railroad::1` and `css-pseudo-4::railroad::1` are inside
  `<div class=note>`; both are marked `NORMATIVE`, parsed, and create one railroad
  row each. The latter source even says the pictured expression is not actually
  BNF.

The membership scanner has the same missing structural state. I found 39 `[[!...]]`
occurrences inside `note`, `example`, `informative`, or `non-normative` ancestors;
they enter the normative-dependency branch because `occurrences()` preserves only
comment/edit state for classification. This repeats the predecessor's central
normativity failure in a new form. `root_blocker_count: 0` is therefore false even
under the epoch's narrower input-stage vocabulary.

### F03 — BLOCKER: the semantic inventory omits pinned normative algorithm families

Extraction recognizes algorithms only when the opening element itself has an
`algorithm` attribute or `algorithm` class. Normative algorithms expressed by a
heading/definition followed by source-native numbered prose are invisible. Exact
retained examples are:

| Source | Exact byte start | Required family | Stored algorithm carrier covering it |
| --- | ---: | --- | ---: |
| `css-grid-1/Overview.bs` | 142,672 | Grid Item Placement Algorithm | 0 |
| `cssom-1/Overview.bs` | 126,413 | CSS property to IDL attribute | 0 |
| `cssom-1/Overview.bs` | 127,238 | IDL attribute to CSS property | 0 |

`css-grid-1` has zero algorithm carriers at all. `cssom-1` has two algorithm
carriers elsewhere, neither covering the two cited algorithms. The 292 generated
`algorithm-step` rows are consequently not a total algorithm-family inventory.
Missing rows cannot be made honest by routing the rows that do exist to VT.W1.

### F04 — BLOCKER: the WPT union ignores the retained source's `wpt-rest` directive

`membership.wpt_routes()` selects only start tags whose name is exactly `wpt`.
The selected `css-variables-1/Overview.bs` contains a live
`<wpt-rest></wpt-rest>` immediately after its explicit test list. It is absent
from `scope.json`; there is no rejected/unsupported directive record and no root
blocker.

At the pinned WPT tree, `css/css-variables` has 273 paths. The source artifact
retains 172 explicit unique routes. A conservative census restricted to test-like
HTML/XHTML files and excluding support/resource/reference paths has 245 candidates,
73 of which are not in those explicit routes. The exact `wpt-rest` expansion must
be interpreted before that numerical difference can be called the final missing
set, but silently ignoring the directive already disproves an exhaustive
source-declared WPT union. The honest zero-outcome/UNRUN statuses do not cure the
missing membership records.

### F05 — BLOCKER: external parsing-authority decisions are global labels and omit a direct Unicode scanner dependency

`reference_decisions` is keyed only by bibliography key, so every occurrence of a
key receives one global `CSS_SOURCE`, `EXTERNAL_PARSING_AUTHORITY`, or
`NORMATIVE_NON_PARSING` label regardless of its source-qualified semantic use.
There is no retained occurrence-level rationale or authority edge.

The concrete casualty is in the assay's own selected CSS2 Lex carrier at byte
638,117: the scanner grammar defines its maximum escape/code-point behavior with
normative `[[!UNICODE]]` and `[[!ISO10646]]` references. The implementation emits
that carrier as parsing semantics while classifying both authorities as
`NORMATIVE_NON_PARSING`; neither authority's bytes is among the eight retained
external parsing authorities. Full normative parsing-dependency closure is not
sealed.

### F06 — BLOCKER: unresolved semantic facts are laundered past the fail-closed stage gate

`require_clean()` checks only `root_blockers`. `modules.capture()` and
`matrix.capture()` unconditionally return empty root-blocker arrays even when the
values they produce are semantically unresolved. Current exact values include:

- 1,564 `UNMAPPED`, 37 ambiguous-import, and 15 ambiguous-local reference
  occurrences;
- one unmapped and two ambiguous extension bases;
- 601 source-declared WPT routes already known `UNMAPPED`; and
- all 8,796 matrix rows still `OBLIGATION` with every BBNF, fixture, and owner
  plane open.

Those explicit RED facts are honest as counts, but treating them as a clean
prerequisite lets incomplete module/closure values flow into a matrix with
`author_gate: PASS` and yields `input_epoch_reproducible: true`. F01-F05 show that
the partition is not merely nomenclature: local extractor/dialect/inventory
failures are absent from both the open rows and root blockers. The zero-root-blocker
status is not an evidence-bearing semantic gate and cannot support a clean pass.

### F07 — BLOCKER: the proof is an empty-output replay, not sealed deterministic clean-root input reconstruction

All 96 selected CSSWG source hashes are absent from `inputs/objects`; the
intersection between the 96 source digests and the 105 retained authority-object
digests is zero. The subject manifest contains no CSSWG checkout and no WPT Git
objects. Replay instead hard-codes:

```text
/private/tmp/bbnf-csswg.X76O73/csswg-drafts
../epoch-2/vendor/wpt
```

The proof creates two empty *output* roots but reuses those already-populated,
unsealed external input roots. `require_checkout()` checks only `rev-parse HEAD`,
not cleanliness or acquisition. There is no checked bootstrap that reconstructs
either input repository from epoch-owned bytes, and the exact CPython executable
is likewise required by host path rather than supplied. A prepared-machine replay
passes; an epoch-only clean root cannot run. This fails the requested toolchain and
input sealing and deterministic clean-root replay plane.

### F08 — MAJOR: the future-candidate boundary contract names a nonexistent allowed artifact and its “crater” is a substring scan

`future-candidate-contract.json` allowlists
`conformance/css-l4/epoch-3/data/observations.json`, but that file does not exist
and is not generated. The executable inspector scans only UTF-8 files with a
short suffix allowlist and searches literal substrings. It does not inspect
resolved build inputs, package graphs, generated artifacts, binaries, shell/build
inputs, indirect strings, or imports. The tests even assert that a candidate may
reference the nonexistent path.

`NO_CANDIDATE_PRESENT` is a true current-state statement; it is not mechanical
candidate-boundary proof. The promised future build/import/package crater remains
absent, so the assay cannot establish that four future candidates will consume
only permitted observation bytes or avoid this oracle as a second authority.

### F09 — MAJOR: the stored “final proof” receipt predates the final sealed subject

`data/verification.json` records subject hygiene for 138 files, 130,606,247 bytes,
and subject SHA-256
`14eb8b32d414ff95bc4f06b41854ec2c6e0989504acfadad2e99575eedb523eb`.
The current checked `data/file-manifest.json` records 141 files, 130,631,391 bytes,
and subject SHA-256
`b0e115d879ca6b9a14c7c59e73653e9891410d58e8d1532e7b1ec835e3848597`.

The delta is exactly three files and 25,144 bytes, equal to the current README
(7,855), AUTHOR (10,623), and verification receipt itself (6,666). Therefore the
stored receipt cannot attest the current author/seal/receipt bytes. A fresh blind
proof over the current manifest did pass and was retained only in `/private/tmp`,
but that independent review fact does not retroactively make the stored author
receipt final.

### F10 — MAJOR: evidence amplification remains disproportionate to the narrow assay

The six deterministic generated artifacts occupy 63,188,477 bytes and peak at a
reported 562,413,568 bytes RSS; the owned subject is 130,631,391 bytes. This is
not just unavoidable authority content:

- 781 normative entity carriers contain 5,425 field occurrences, but per-name
  entity cloning expands them to 7,339 module field occurrences (1,914 clones).
- The 8,796 matrix rows point to only 6,850 exact source identities; 1,946
  row-to-source repetitions remain, with one source repeated up to eight times.

Separate obligations per property name can be semantically necessary; cloning
the full field/value structures through modules and matrix is not evidence of
more coverage. In combination with F01-F06, this amplification makes the assay
harder to direct while preserving omissions. The constitution's parsimony and
resource planes therefore remain RED.

## Retained truths (facts only, not credit)

1. The authority manifest has 106 logical records and 105 exact retained objects;
   all 67,196,482 object bytes match their content-addressed names. The 96 dated
   publication records have canonical dated W3C URL shapes and retained bytes.
2. The CSSWG commit/path/hash census and compact WPT commit/tree/path-OID receipt
   reproduce on the already-present declared checkouts.
3. Unknown matrix statuses fail against closed enums. The current artifacts do
   not fabricate BBNF mapping, fixture proof, responsible owners, WPT outcomes,
   controls, ratios, or candidate existence.
4. Every current matrix row is explicitly `OBLIGATION`; the semantic portfolio,
   external controls, complete CSS claim, and broader VT.W0 remain explicitly RED.
5. On the prepared host, a fresh two-run proof regenerated the six stored artifact
   byte streams exactly and the current 141-file manifest verified.
6. No current candidate, package, production import, performance result, or source
   movement is present in this epoch.

These facts do not offset any numbered finding and award no lock, wave, candidate,
coverage, clean-pass, or product credit.

## Four-plane and total-tranche verdict

| Plane | Verdict | Reason |
| --- | --- | --- |
| Tranche fit | **RED** | An external CSS observation input remains directionally relevant, but this 130.6 MB CSS-only assay is not the smallest complete VT.W0 route and supplies none of the acknowledged JSON, profile/hot-leaf, four-placeholder, or external RED-gate remainder. |
| Wave fidelity | **RED** | Full CSS-L4/no-exception membership is disproved by missing algorithms, Lex rules, WPT-rest routes, and parsing authorities; every matrix plane is still open and the rest of VT.W0 is absent. |
| Feature truth | **RED** | Strict dialect, normativity, source inventory, reference closure, WPT ownership, boundary, and clean-root claims fail under executable counterexamples. |
| Greater-product effect | **RED** | No shipping edge exists, but candidate isolation is not enforced, the allowed observation artifact is absent, the assay can become a second authority, and duplicated evidence/resource cost remains high. |
| Total tranche | **RED** | No candidate exists; none of PT-E/J/C/K/R/V or BB-1/2/3/7 is implemented or proved here; complete-candidate and performance gates remain untouched. |

## Clean-count effect and disposition

F01-F10 are findings on the sealed epoch-3 bytes. Under the constitution, correct
findings reopen the affected CSS W0 design cluster and keep/reset the consecutive
clean count at **0**. This review is not clean, earns no hostile-pass credit, and
cannot be followed by an acceptance count over these bytes. All production,
package, source, campaign, and downstream movement remains prohibited.

## Exact identities

```text
repository HEAD                         af15f63e0d2d3d719938c13b906a50acbb92ea3b
LOCK-CONSTITUTION.md SHA-256            b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5
BB-VALUE-TAPE-P0.md SHA-256             745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562
epoch-2 adjudication SHA-256            781bbd1a49c9d89baab3bd393d175338192b3c714b5a97818e08ac95caf242f5
data/file-manifest.json SHA-256         a35dd2c84c490c7adb72c1916e7f4bd4dc9e5997135edaad7d9ddc9247376495
manifest subject SHA-256                b0e115d879ca6b9a14c7c59e73653e9891410d58e8d1532e7b1ec835e3848597
fresh blind proof receipt SHA-256       a7d7013e426d78ef82d58f724dc747b29cf12e119fd1cab16f621d3cc9cdc171
```

The subject digest excludes `data/file-manifest.json` by self-reference and root
`GESTALT-*.md` review appendices by declared construction, so writing this report
does not change that digest.

## Exact rerunnable evidence and commands

The principal gate replay was:

```sh
probe_root=$(mktemp -d /private/tmp/bbnf-css-l4-epoch3-critic-b.XXXXXX)
env PYTHONDONTWRITEBYTECODE=1 GIT_NO_LAZY_FETCH=1 GIT_TERMINAL_PROMPT=0 \
  python3 -B tests/proof.py \
  --run-root "$probe_root/runs" \
  --output "$probe_root/proof.json"
env PYTHONDONTWRITEBYTECODE=1 python3 -B src/manifest.py
```

Exact result:

```text
proof:    status=PASS; artifact_count=6; input_epoch_reproducible=true; semantic_portfolio=RED
manifest: file_count=141; total_bytes=130631391; subject_sha256=b0e115d879ca6b9a14c7c59e73653e9891410d58e8d1532e7b1ec835e3848597
```

The strict-dialect counterexamples were executed with:

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -B - <<'PY'
from dialects import parse_lex, parse_yacc
print(parse_yacc('a: b [ c | d ;'))
print(parse_lex('%option noyywrap\nNAME [a-z]+\n%%\nBROKEN RULE BODY\n'))
PY
```

Both malformed values returned parsed objects. Counts for F01 were reproduced by
partitioning the two retained carrier texts at `%%`, comparing emitted definition
`source.text_start` values, matching `\{name\}` in emitted Lex patterns, and
filtering `reference_closure` by the 68 scanner definition IDs.

F02 was reproduced by rescanning all selected sources with `scanner.scan()`,
joining carrier records on `(path,start)`, and testing both the opening token's
classes and every ancestor class against `note`, `example`, `informative`, and
`non-normative`. F03 was reproduced by locating the three quoted byte strings in
their pinned source files and testing containment against every stored
`kind=algorithm` carrier span.

The WPT-rest census used:

```sh
rg -n -C 8 '<wpt-rest' \
  /private/tmp/bbnf-csswg.X76O73/csswg-drafts/css-variables-1/Overview.bs
git -C ../epoch-2/vendor/wpt ls-tree -r --name-only HEAD -- css/css-variables
```

Input sealing was checked by set-comparing all `data/scope.json.specs[].sha256`
against all `inputs/authorities.json.authorities[].sha256` and then checking every
`data/file-manifest.json.files[].path` for CSSWG or WPT Git objects. Exact result:

```text
csswg_source_hashes=96
retained_as_objects=0
missing_source_objects=96
manifest_has_wpt_git_objects=0
manifest_has_csswg_checkout=0
```

Finally, every epoch JSON file was decoded with duplicate-key rejection and fully
walked, every input object was read and hashed, and the governing/manifest/proof
identities were independently reproduced with `sha256sum`.
