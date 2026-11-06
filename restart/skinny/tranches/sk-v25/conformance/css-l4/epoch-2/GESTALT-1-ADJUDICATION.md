# CSS L4 W0 epoch 2 — Gestalt 1 independent adjudication

Date: 2026-07-19  
Adjudication session: `019f788f-8c95-7410-ac39-bbcaa1d4f176`  
Task: `/root/css_w0_epoch2_adjudicator`  
Declared and task-context model / reasoning effort: `gpt-5.6-sol` / `xhigh`  
Role: fresh independent adjudicator  
Delegation: none  
Network, fetch, retry, pin change, or in-place regeneration: none  
Files edited: this report only

## Terminal ruling

**GESTALT 1: RED. Consecutive-clean count: 0.**

The CSS Level 4 external-oracle direction belongs in VT.W0, and the exact CSSWG
source receipt, WPT commit/tree/path-OID receipt, deterministic regeneration, and
honest aggregate `BLOCKED` result are bankable narrow facts. The semantic oracle is
not sound. Correct blocker-class defects exist in membership authority, normative
dependency closure, publication replay, carrier normativity, CSS value grammar,
railroad and legacy dialects, profiles, entity/reference closure, semantic spans,
WPT ownership, matrix status closure, candidate-boundary enforcement, and test
adequacy. The 2,975 stored blockers are an exact concatenated list length, not an
exact census of failed W0 invariants.

No CSS coverage, coverage percentage, CSS/W0 completion, lock closure, candidate
count, complete-candidate credit, hostile-pass credit, ranking, production move,
source movement, downstream transposition, or SOTA claim is authorized. This
implementation must be frozen as a `DEFECTIVE_BLOCKED_W0_ASSAY`, repaired in its
owning CSS W0 design cluster, and presented to a new first hostile pass. A second
pass over these bytes cannot earn progress.

All sixteen numbered Critic A findings and all eleven numbered Critic B findings
are **CORRECT in their central ruling**. Three auxiliary numerical descriptions
need correction: the handwritten `PRELUDE` has 29 names, not A's 32 or B's 30; and
the 126 Compositing/Filter Effects false definitions consist of 121 records marked
`PARSED` and five already marked `UNPROVEN`, rather than 126 parsed successes.
Those corrections make no finding non-blocking. No numbered finding is
`INCORRECT` or `UNPROVEN` after those explicit subclaim corrections.

## Review boundary and authorities

I read in full:

- `LOCK-CONSTITUTION.md`, `TRANCHE-PORTFOLIO.md`, and
  `waves/BB-VALUE-TAPE-P0.md`;
- this epoch's `AUTHOR.md`, `README.md`, `.gitignore`, seven source files, six
  test/support files, fixture, publication input, and all four generated artifacts;
- every object, array, and scalar reached by fully decoding and recursively
  traversing `scope.json`, `carriers.json`, `grammar.json`, and `matrix.json`; and
- both blind reports, `GESTALT-1-CRITIC-A.md` and
  `GESTALT-1-CRITIC-B.md`.

I assumed the implementation and both critics were wrong. I did not use critic
agreement as proof. Governing and critic file identities for this pass are:

```text
LOCK-CONSTITUTION.md       3622262dcbf638dc911564bef8ee1babd65cb81a9175c72141da3166634ca85a
TRANCHE-PORTFOLIO.md       729202d59650a8c5a42b568bda11091af8ca631add143d039dfd84238b941e10
BB-VALUE-TAPE-P0.md        710e50f4901b63642b0645051b0ef055d942b8ce377529ae7f793f1e9f35c6c1
GESTALT-1-CRITIC-A.md      e20979f5ae603241739447f1174777cefae9b19011ce557b2aac8e059fb9bec9
GESTALT-1-CRITIC-B.md      5834afd01154cc7bace7875007518e551a2b71e57ba9c30ab6282e81bca355ad
repository HEAD            af15f63e0d2d3d719938c13b906a50acbb92ea3b
```

Host evidence was Apple arm64 Darwin 25.4.0, Python 3.14.5, and Apple Git 2.39.5.

## Gate replay and immutable receipts

From the epoch directory, with lazy Git fetch and terminal prompts disabled:

| Command | Independent result |
|---|---|
| `python3 -B src/regen.py --check --allow-blocked` | exit 0; `BLOCKED`; 2,975 blockers |
| `python3 -B -m unittest discover -s tests -p '*.py'` | exit 0; 26 tests pass |
| `python3 -B src/regen.py --check` | exit 2; `BLOCKED`; 2,975 blockers |

The exact stored partition reproduces:

```text
scope:   624 WPT_ROUTE_UNMAPPED
          11 PUBLICATION_MISSING
           8 DEPENDENCY_UNPROVEN
           1 PUBLICATION_FETCH_BLOCKED
extract:   0
grammar: 185 REFERENCE_UNMAPPED
          26 GRAMMAR_PARSE_FAILED
          18 PROFILE_CONFLICT
matrix: 2102 MATRIX_ROW_INCOMPLETE
total:  2975
```

All four JSON files decode fully and reproduce their current bytes and hashes:

| Artifact | Bytes | Fully traversed container/scalar census | SHA-256 |
|---|---:|---:|---|
| `data/scope.json` | 37,285,537 | 191,331 objects; 372 arrays; 502,736 scalars | `a99597159f540fa76c132a875fac48eb36bff427cc12477e6236e73cd35ed037` |
| `data/carriers.json` | 3,649,956 | 12,594 objects; 2,939 arrays; 64,241 scalars | `22b8bf619d2d47dfdbc87112c7c25a6d5440641c0534a9774622aa79e8009c95` |
| `data/grammar.json` | 4,021,193 | 24,452 objects; 8,193 arrays; 82,253 scalars | `ca6e361de15d55f144606abeeaf05b7272575d9635181ebfd566c77831540109` |
| `data/matrix.json` | 5,203,904 | 21,022 objects; 8,410 arrays; 93,826 scalars | `46fedfdce6b8314a4d66cc3d62f737a5cfec1bbf009cb58e660e71a130bb1873` |

The sealed pre-critic subject has 22 files and 50,279,312 bytes. Hashing sorted
relative paths as `path || NUL || bytes || NUL` reproduces:

```text
complete sealed subject    7135457fd39458826fe5994c7fd310d90b1ef3d83fe0115f6991e438258e29a9
seven source + six tests   0af7719c0c4a9b2479599b8151f5e2060f3841eee83a29ff4d7fce65917125cb
four generated artifacts  c08c3c2114af1595b48cda42f1e02fb86df3c3b022897b0c7966e7c5c6994992
```

Critic A is correct and Critic B's non-numbered ledger-pass claim is incorrect:
after excluding `AUTHOR.md`, `inputs/publications.json`, and the four generated
files, 16 files remain, not 17. The author's advertised
`c6e5ea8b...65ab3f418` does not reproduce under its stated
`path || NUL || file_sha256 || NUL` construction for that set. This receipt defect
is secondary to the semantic blockers but must be corrected.

## Tranche optimality, stage fidelity, and friction

| Question | Adjudication |
|---|---|
| Does an external CSS oracle belong before the four private VT candidates? | **PASS, direction only.** CSS scope and read-only oracle observations are explicitly shareable VT.W0 inputs. |
| Is this a complete VT.W0 wave? | **NO.** JSON closure, incumbent profile/hot-leaf recapture, four private compiling placeholders, and their external RED suites are absent. The author correctly requests no W0 credit. |
| Is this the smallest useful successor experiment? | **RED.** Strict source-dialect and negative-carrier probes would have killed Stage 2 before 50.16 MB of profile, closure, WPT, and matrix expansion was built. |
| Are the abstract stage boundaries useful? | **PASS narrowly.** `scope -> carriers -> grammar -> matrix` is legible, and seven dependency-free Python modules avoid framework ceremony. |
| Are stage contracts sealed? | **NO.** Each later stage accepts policy labels and structurally present keys while failing to validate the preceding stage's semantic completeness. |
| Is friction proportionate? | **NO overall.** The direct code/test topology is good, but hard-coded local inputs, absent clean acquisition, and 32.53 MB of redundant manifest/placeholder expansion make the assay difficult to direct. |

The coherent stage order is wrong in practice: membership and dependency authority
remain unproved; extraction admits equations and drops live syntax; grammar accepts
malformed token trees; the matrix then manufactures proximity routes and terminal
owners. Downstream generation should have stopped at the first semantic mutation.

## Exact scope, publication, and WPT adjudication

### CSSWG source and scope

The CSSWG receipt passes narrowly. All 96 unique paths at commit
`c7573530343759ace8e46438a1fa2c44515b5554` rehash exactly, total 11,676,246
bytes, and the checkout is clean. This proves selected source identity, not a
canonical CSS L4 union.

The initial 84 members are loaded from epoch 1 rather than rederived in this
implementation. The pinned checkout contains 24 directories whose metadata says
`Level: 4`; 21 are selected, while `css-gcpm-4`, `css-page-4`, and `css-text-5`
are excluded without this epoch retaining the W3C Current Work authority or a
reviewable selection decision. The 96-source result therefore remains an
unproved membership/dependency closure.

Raw `[[!reference]]` scanning finds 1,263 occurrences. Twenty-six occur inside
comments. `CSS3UI`, `HTML40`, and `HTML5` occur only in comments yet enter the
dependency table. Eighty-six unresolved keys are made terminal by the default
`CLASSIFIED_EXTERNAL_NON_SOURCE` branch. This includes aliases such as `CSS21`,
`CSS3-SYNTAX`, `CSSSTYLEATTR`, `CSS3-ALIGN`, `CSS3-ANIMATIONS`,
`CSS3-BACKGROUND`, and `CSS3-MEDIAQUERIES`, despite corresponding CSS sources
already being in scope. `SOURCE_OVERRIDES` is unused; the attempted `CSS21 -> CSS2`
directory match is case-sensitive and misses `css2`. The eight explicit parsing
authorities are honest blockers, but they are not exhaustive.

### Publications

The artifact contains 84 rows labeled `PINNED`, but only 82 have visibly dated W3C
TR URLs. The two exceptions are:

```text
css-fonts-3     https://drafts.csswg.org/css-fonts/
css-style-attr  http://dev.w3.org/csswg/css-style-attr/
```

Twelve of 96 specs remain open: eleven ordinary `MISSING` rows and
`css3-mediaqueries`, whose main spec row also says `MISSING` while the separate
failure receipt says `BLOCKED_INPUT`. The input stores URL, claimed length, and
claimed digest, but no publication bytes; `scope.capture()` trusts those fields
without URL-shape, length, or digest validation. Offline replay proves receipt
identity only. `fetch.py` increments `requests_made` after a successful request,
so the failing HTTP attempt is omitted from the claimed 123 requests. Publication
records are bankable only as unverified historical receipts.

### WPT

The WPT receipt passes narrowly and exactly:

```text
commit                         37cd7ff74eb974fd41600bf7bc01d37576abf8b5
tree                           12fa64c9afe86e199ee0e737755b6507f363ceff
blob paths                     160788
path/OID manifest SHA-256      cf3aedd08cbb161aeebd3c9e19c10f855f2be0be23fa5ecbbd2035d2e31db699
stored rows equal ls-tree      yes
local packed commits/trees     1 / 6605
local packed blobs             0
```

There are 29,229 source route records, 24,829 unique spec/route pairs, 28,605
`PINNED` paths, and 624 `UNMAPPED` paths. No WPT blob content is locally retained
and no test was executed. A tree/path/OID receipt is not semantic ownership or a
WPT outcome.

## Carrier extraction and normativity

Narrow passing facts must survive repair: all 1,468 selected carriers' coarse raw
and body spans, line numbers, lengths, and hashes recheck against CSSWG bytes;
outer comments are skipped; quoted `>` opening attributes work; `xmp` raw text is
preserved; and the two live railroad `<!--`/`-->` literals survive. These mechanics
do not establish carrier completeness or normativity.

The implementation treats a plain `<pre>` as grammar whenever an equality-like
line matches, without requiring proved grammar context. Pinned false carriers
include Compositing equations, a Filter Effects SVG example, Filter Effects C code,
and a Fonts 4 adjustment equation. They yield 85 Compositing definitions and 41
Filter Effects definitions. Of those 126 records, 121 are marked `PARSED` and five
are marked `UNPROVEN`; all 126 are false CSS grammar membership.

Normativity is raw substring/window logic. `b"<ins"` matches Bikeshed references
such as `<<inset()>>`, while actual current `<ins>` correction text marks an entire
carrier historical. A previous 800-byte mention of `non-normative` poisons a later
carrier without structural containment. Direct pinned casualties are:

- 13 live property carriers: three CSS Gaps carriers, nine CSS UI carriers, and
  `scrollbar-width`;
- three live production carriers: the CSS Gaps `inset-value`, Shapes `inset()`
  family, and Color 4 color-space production; and
- the separate Compositing `globalCompositeOperation` property carrier, which is
  extracted but silently loses entity membership because it uses typographic
  property-name markup instead of a `Name:` field.

Thus at least 14 live property carriers and three live production carriers are
absent or misrepresented downstream. Stage 2's zero-blocker/PASS claim is false.

`MARKUP_NAMES` is a handwritten allowlist rather than a Bikeshed/HTML tokenizer.
Fifteen entity value occurrences retain `<nobr>` and `</nobr>`, producing 15
`nobr` and 15 `/nobr` references. Coarse carrier spans remain exact, but semantic
source maps do not: AST nodes and tokens have no source spans; `text_start` and
`text_end` are decoded/stripped-text offsets rather than source bytes.

## CSS, railroad, and legacy grammar truth

The CSS value parser recognizes balanced token shape, not strict CSS Value
Definition Syntax. Independent probes return ASTs for all of these invalid forms:

```text
a &&        -> all-order(a, empty)
| a         -> alternative(empty, a)
a || || b   -> any-order(a, empty, b)
[]          -> group(empty)
a {wat}     -> repeat(a, "{wat}")
a ???       -> repeat(a, "?", "?", "?")
a !!        -> repeat(a, "!", "!")
a = prose   -> sequence(a, "=", prose)
a ; prose   -> sequence(a, ";", prose)
```

The stored corpus marks 529 `prod` definitions and 240 `legacy-grammar`
definitions `PARSED`. Across definition, expression, and entity ASTs, atom values
include `=` 68 times, `;` 47 times, and `:` 19 times, plus `HREF=`, `operator=`,
and `0.0f;`. These are executable counterexamples to AST validity.

Railroad parsing accepts any alphabetic operator and arbitrary indentation. Both
`Bogus: payload` and `Choice:\n  Bogus: x` return trees. The pinned output contains
17 operator spellings but no vocabulary, arity, payload, or cardinality validator.
All 46 normative railroad expressions are nevertheless `PARSED_RAILROAD`.

Legacy parsing is colon/semicolon scraping. It has no dialect registry and cannot
distinguish CSS2/Yacc productions from equations, SVG attributes, JavaScript,
pseudocode, or C. The value parser, railroad adapter, and generic legacy adapter
must be retired as semantic authorities; their false positives should become
negative fixtures.

## Profiles, entities, closure, and spans

Definitions are grouped by bare global name and selected by a hardcoded scalar
priority. Exact profile facts are:

```text
profile names                         580
names with repeated definitions       132
repeated names with differing ASTs     64
differing names silently resolved       46
terminal conflicts                      18
non-conflicting cross-spec selections  113
```

This is neither a CSS union nor a module system. Color 4 receives priority 504
from `current-work:testing`; Color 5 receives 455 from pre-CR/snapshot scope.
Consequently Color 4 replaces differing Color 5 definitions for `color`,
`color-base`, `color-function`, `color()`, `hwb()`, `lab()`, `lch()`, `oklab()`,
and `oklch()`, discarding newer scoped additions. False code definitions also
collide under bare names such as `a`, `s`, `position`, and `media`.

All 1,012 entities are called `STRUCTURED`, largely because the permissive value
parser accepts their text. Seventeen entity-level `New values` rows represent only
11 distinct underlying carrier/value occurrences, with no base or extension edge.
Multi-name carriers clone flattened occurrences rather than retaining
source-qualified occurrence identity.

Entity ASTs are entirely omitted from stored reference closure. Three exact views
of the same omission are useful and non-conflicting:

```text
raw entity reference occurrences                  1180
distinct reference names per individual value AST 1125
distinct entity/reference pairs                   1117
entities containing references                     745
distinct referenced names                          278
stored closure rows whose source is an entity         0
```

The critics' 1,180 and 1,125 counts therefore measure different valid granularities.
The handwritten prelude has **29** names, not 32 or 30, and terminates 322 closure
edges with invented `css-syntax-prelude::*` targets. It is an unversioned semantic
table, not a source-derived import.

The 2,102 matrix rows collapse onto 1,396 unique carrier spans: 706 rows merely
repeat an already-used span, and as many as 33 rows share one span. Serialized AST
objects contain zero `start`, `end`, or `span` keys. Exact carrier receipt does not
equal exact production, token, field, IDL declaration, algorithm-step, or entity
span.

## WPT semantic ownership

`scope.WPT_BLOCK` scans raw files without comment state. It emits 162 records from
commented-out WPT blocks: 138 `PINNED`, 23 `UNMAPPED`, and one
`CLASSIFIED_NO_DIRECT_TESTS`. Six matrix rows choose one of these commented routes:
three Color 4 algorithms and three Conditional 3 IDL blocks.

`matrix.nearest_routes()` then selects by absolute byte distance alone. Exact
results are:

```text
rows with at least one nearest route          1587
route associations (ties expanded)           18861
unique spec/routes used                        7946 of 24829
median per-row nearest distance                1642 bytes
90th percentile per-row nearest distance      44654 bytes
rows over 10,000 / 50,000 / 100,000 bytes     312 / 152 / 131
maximum distance                               599051 bytes
```

The CSS2 `border-width-011.xht` route is attached to 130 rows;
`css-backgrounds/inheritance.sub.html` is attached to 83; several Filter Effects
routes fan out to 61 rows. Moving a carrier without changing semantics can change
its selected test. Empty `NO_DIRECT_TESTS` blocks are filtered out before nearest
selection. The matrix's 1,489 `UNRUN` versus 613 `UNMAPPED` split is therefore a
proximity census, not WPT ownership.

The exact tree receipt passes. Route semantics and WPT execution are RED.

## Matrix, statuses, blockers, and semantic granularity

`common.terminal()` defines terminality negatively: any string outside six known
incomplete values is terminal. Mutating every plane of the first row to
`TYPO_COUNTS_AS_TERMINAL`, setting its row state to `MAPPED`, and calling
`matrix.validate()` succeeds. The future gate is fail-open even though current
rows happen to be honestly incomplete.

Current matrix facts are:

```text
BBNF expectations   2102 UNMAPPED
fixtures            2102 UNMAPPED; 0 positive; 0 negative
WPT                 1489 UNRUN; 613 UNMAPPED
recovery            2102 UNPROVEN
diagnostics         2102 UNPROVEN
serialization       2102 UNPROVEN
owners              2102 ASSIGNED across 96 generated strings
terminal rows          0 MAPPED; 2102 UNPROVEN
```

No corresponding owner modules exist. Algorithms are raw blocks without steps,
inputs, outcomes, recovery, or authority. A direct WebIDL declaration-start census
finds 183 declarations in 133 normative IDL blocks, with 32 blocks containing more
than one; the matrix has only 133 IDL rows and no IDL AST. The critics' weaker
"at least 179" claim is therefore established. Parse failures, profile conflicts,
reference edges, and extension relations have no row families.

The matrix has required keys, not a full external observation matrix. Unknown
statuses must fail against closed enums, and evidence planes need semantic units
and real owners before they can become terminal.

## Candidate boundary, tests, evidence size, and greater-product effect

The current directory has no candidate consumer, so no actual candidate-side
violation has landed. It also has no mechanical boundary. The implementation
checks a policy string, a top-level Boolean, and row Booleans; it never inspects a
candidate's build inputs, imports, package graph, generated types, or crater. The
matrix manufactures BBNF paths/production names/owners, and grammar data contains
the prelude and flattened profile authority. Calling those fields
`candidate_input: false` does not prevent consumption. The author's mechanical
PASS is revoked; the honest present status is `DECLARATIVE_ONLY / UNPROVEN_UNTIL_A_CANDIDATE_CRATER`.

The 26 tests pass, and exact present artifacts regenerate twice offline on the
already-present checkouts. Those are narrow passing facts. The tests assert current
producer counts, the hardcoded external-authority set, happy grammar examples,
OID shape, and status presence. They omit every strict grammar mutation,
normativity casualty, entity edge, module/profile conflict, comment dependency/WPT
edge, publication URL check, unknown-status mutation, semantic span, and WPT owner
counterexample above. Two builds occur in one process against hard-coded/default
checkouts; they are not two CLI runs into distinct empty output roots. No checked
bootstrap reconstructs the CSSWG and WPT verification inputs on a clean machine.

Evidence amplification is exact:

```text
source + tests                                      85,471 bytes
four generated artifacts                       50,160,590 bytes
pretty WPT manifest expansion                   27,323,245 bytes
all-placeholder matrix rows + duplicate blockers 5,203,540 bytes
minimum redundant tree/placeholder expansion   32,526,785 bytes
```

The WPT tree OID already commits the path/OID relation, and the local tree objects
replay it. The all-placeholder matrix does not need 5.2 MB to say all rows are
open. This is dirigibility debt, not evidence strength.

No shipping/package or candidate import edge exists, which is a passing greater-
product fact. If advanced, however, the shared false oracle would make four
independent candidates agree for the wrong reason and would become a second
semantic authority. Candidate independence is not yet harmed, but it is not
protected. Preserve the oracle outside every candidate build graph.

## Numbered finding adjudication

The overlap column merges the two reports without erasing the distinct claims.

### Critic A

| ID | Ruling | Overlap | Independent adjudication |
|---|---|---|---|
| F01 | **CORRECT** | B-10 | Unknown statuses count terminal; the supplied `TYPO_COUNTS_AS_TERMINAL` mutation is accepted. |
| F02 | **CORRECT** | B-01, B-03 | Invalid CSS expressions and non-grammar programs are accepted. Precision correction: the 126 Compositing/Filter definitions are 121 `PARSED` plus 5 `UNPROVEN`, not 126 parsed successes. |
| F03 | **CORRECT** | B-01, B-03 | Railroad has no legal operator/arity schema; legacy parsing is colon/semicolon scraping with no dialect semantics. |
| F04 | **CORRECT** | B-02 | Substring/window normativity drops 13 live property and three live production carriers. |
| F05 | **CORRECT** | B-01, B-04, B-05 | `globalCompositeOperation` silently has no entity; `New values` has no extension semantics; permissive parsing makes all entities appear structured. |
| F06 | **CORRECT** | B-02, B-04 | Fixed markup leaks 30 bogus `nobr` reference occurrences; semantic/token spans do not exist. |
| F07 | **CORRECT** | B-07 | Twenty-six dependency occurrences are in comments, three keys are comment-only, CSS aliases are missed, and 86 unknown externals terminate by default. |
| F08 | **CORRECT** | B-05 | Bare-name scalar priority silently resolves 46 differing repeated names and incorrectly lets Color 4 replace scoped Color 5 definitions. |
| F09 | **CORRECT** | B-04 | Entity closure is absent: 1,125 distinct per-value-AST edges and 1,180 raw occurrences are omitted. Precision correction: `PRELUDE` contains 29 names, not 32. |
| F10 | **CORRECT** | B-06 | Comment routes and byte-nearest mapping are not semantic WPT ownership; all cited route and distance pathologies reproduce. |
| F11 | **CORRECT** | B-08 | Only 82 of 84 pins are visibly dated, 12 specs are open, and no publication bytes are available for offline rehashing. |
| F12 | **CORRECT** | B-09 | The candidate boundary is declarative only; no consumer/build/import enforcement exists. |
| F13 | **CORRECT** | B-06, B-10 | External planes are shells; 183 declarations collapse into 133 IDL block rows, algorithms are unstructured, and owners are fictional strings. |
| F14 | **CORRECT** | B-11 | Green tests preserve current counts and omit the hostile semantic mutations. |
| F15 | **CORRECT** | B-11 | At least 32,526,785 generated bytes are redundant; the author's 17-file wording/digest does not reproduce as stated. |
| F16 | **CORRECT** | B-10 | The 2,975 visible blockers alone forbid W0 exit, and F01-F15 prove additional semantic undercount. |

### Critic B

| ID | Ruling | Overlap | Independent adjudication |
|---|---|---|---|
| B-01 | **CORRECT** | F02, F03 | Pinned equations, SVG, C, and font math are accepted as grammar; Stage 2 and all downstream counts are contaminated. |
| B-02 | **CORRECT** | F04 | `<<inset...>>`, real `<ins>`, and nearby prose mutations reproduce false normativity. |
| B-03 | **CORRECT** | F02, F03 | Empty operands, arbitrary multipliers, and unknown railroad operators return ASTs rather than errors. |
| B-04 | **CORRECT** | F06, F09 | 1,180 raw entity reference occurrences are outside closure; 322 prelude edges terminate without source proof. Precision correction: the prelude has 29 names, not 30. |
| B-05 | **CORRECT** | F08 | Bare global identity and invented scalar priority violate module/profile semantics and misresolve Color 4/5. |
| B-06 | **CORRECT** | F10 | Nearest-byte routing is relocation-unstable and unrelated to feature/assertion ownership; exact tree pinning remains a narrow PASS. |
| B-07 | **CORRECT** | F07 | Alias/default-external handling creates false terminal dependency edges; `SOURCE_OVERRIDES` is unused. |
| B-08 | **CORRECT** | F11 | Two pins are not dated, publication bytes are absent, failure states disagree, and the request counter omits the failed attempt. |
| B-09 | **CORRECT** | F12 | Policy strings do not enforce future candidate inputs; no present candidate violation is inferred. |
| B-10 | **CORRECT** | F01, F04, F09, F10, F16 | The 2,975 count is deterministic but semantically incomplete and mixes root versus row-level granularity. |
| B-11 | **CORRECT** | F14, F15 | Present-checkout replay is deterministic, but clean-machine acquisition, empty-root CLI replay, and hostile mutation proof are absent. |

## Narrow passing facts and artifact disposition

| Evidence or mechanism | Disposition |
|---|---|
| Epoch-1 eight-file hash bank | **BANK** as historical identity only. |
| CSSWG commit plus 96 path/length/hash records | **BANK AS SOURCE RECEIPT**, not canonical membership or semantic coverage. |
| WPT commit/tree/path-OID digest | **BANK AS TREE RECEIPT**; store compactly and require local blob content plus outcomes before semantic use. |
| Publication metadata | **BANK AS UNVERIFIED RECEIPTS**; not dated-publication proof. |
| Coarse carrier spans/hashes and comment/quote/raw-text witnesses | **BANK SMALL MECHANICAL CORE**; replace classification and add source maps. |
| Seven-module direct stage split and external source-isomorphic tests | **BANK TOPOLOGY**; strengthen validators rather than add a registry framework. |
| Deterministic present-checkout regeneration and strict blocked exit | **BANK MECHANICAL EVIDENCE**; it proves stability and honest fail-closure only. |
| Current malformed grammar/profile/route artifacts | **BANK AS NEGATIVE COUNTEREXAMPLES; RETIRE AS AUTHORITY**. |
| `PRELUDE`, scalar `priority()`, generic `<pre>` guessing, and nearest WPT routing | **DELETE/REPLACE AS SEMANTIC AUTHORITY**. |
| `grammar.json` and `matrix.json` | **RETIRE AS COVERAGE/GRAMMAR AUTHORITY**; preserve only compact open-obligation and counterexample receipts. |
| Whole epoch | **FREEZE AS `DEFECTIVE_BLOCKED_W0_ASSAY`**. |

## Smallest coherent KISS successor repair set

The successor should keep the four understandable stage homes but repair their
values and stop downstream work at the first failed semantic invariant. It does
not need another adjudication registry or contract layer.

1. **Seal source membership and immutable inputs.** Rederive Snapshot 2026,
   Current Work Level-4 membership, exceptions, Syntax, and normative parsing
   dependencies from retained pinned authority bytes. Emit occurrence-level,
   comment/history-aware dependency edges; canonicalize aliases; use a closed,
   reviewed parsing-relevance enum. Retain actual dated publication bytes in a
   content-addressed input and validate canonical dated URLs, response targets,
   length, and digest. Keep WPT as commit/tree plus a compact reproducible receipt,
   not a 27 MB duplicate manifest.

2. **Make extraction structural and fail closed.** Use one source-state tokenizer
   for comments, real tags, raw text, Bikeshed `<<...>>`, edit segments, ancestors,
   dependency occurrences, and WPT blocks. Preserve current `<ins>` content,
   exclude `<del>` only according to pinned source convention, and separate edit
   state from carrier normativity. Remove nearby-prose substring decisions and the
   fixed markup-name authority. Emit reversible source maps from carrier text,
   fields, definitions, and tokens to exact source bytes. Restrict generic carriers
   to Bikeshed classes or explicitly pinned legacy dialect sites; equations,
   programs, markup examples, and unclassified `<pre>` blocks remain blockers.

3. **Use strict, separate grammar dialects.** Validate CSS Value Definition Syntax
   combinators, nonempty operands, legal multipliers and combinations, delimiters,
   references, and functions. Define Bikeshed railroad operator vocabulary,
   payload constraints, arities, and agreement. Give each necessary legacy source
   dialect a narrow parser and negative oracle; delete the generic
   colon/semicolon adapter. Parse WebIDL into declarations and algorithms into
   explicit semantic steps/outcomes where they own CSS observations.

4. **Restore modules, entities, and total closure.** Keep every definition
   source/spec-qualified. Derive imports, exports, aliases, extensions,
   supersession, and profile selection from pinned source decisions; remove scalar
   priority. Handle property-name source forms such as
   `globalCompositeOperation`, preserve each field occurrence, and model `New
   values` as extension edges. Walk every definition, expression, and entity AST;
   require exactly one source-qualified closure decision per reference. Derive any
   prelude from pinned CSS Syntax/Values sources or leave it explicitly unmapped.

5. **Make the matrix semantic and status-closed.** Replace nearest routing with
   explicit section/anchor/family/assertion ownership or `UNMAPPED`; preserve
   `NO_DIRECT_TESTS`; exclude comments; retain required WPT blob bytes and outcomes.
   Use closed status enums whose unknown values fail. Rows must represent exact
   productions, entity fields/extensions, IDL declarations, algorithms,
   conflicts, recovery/diagnostic/serialization families, independent fixtures,
   and existing responsible modules. Report root blockers separately from derived
   affected rows/planes.

6. **Prove the boundary and replay with less evidence.** Narrow the oracle's
   current claim to observation-only intent. In each future VT candidate, enforce
   an input allowlist and a build/import/package crater that fails if any generated
   grammar, matrix, profile, prelude, expectation, or owner table is consumed as
   schema/type authority. Add every A/B counterexample as an independent mutation
   before regenerating downstream data. Provide a checked content-addressed input
   bootstrap and run the CLI twice into distinct empty temporary output roots.
   Store compact receipts and open-obligation summaries instead of expanded tree
   and placeholder duplication.

After item 2, the pipeline must stop unless carrier and normativity mutations are
clean. After item 3, it must stop unless strict AST and source-map validators are
clean. Only then should profiles, closure, and matrix rows be regenerated. JSON,
profile/hot-leaf recapture, four private placeholders, and their external RED tests
remain separate required VT.W0 work; repairing this CSS assay cannot substitute for
them.

## Exact reset and credit disposition

- Correct findings reopen the CSS W0 oracle at design-loop step 2 and reset the
  consecutive-clean count to **0**.
- The author's aggregate no-advancement/no-candidate discipline remains a PASS;
  the component Stage-2, candidate-boundary, publication, owner, and exact-ledger
  PASS claims are revoked.
- This epoch earns only `SOURCE_RECEIPT_BANK`, `WPT_TREE_RECEIPT_BANK`,
  `COARSE_CARRIER_RECEIPT_BANK`, and `DETERMINISM_MECHANICS_BANK` evidence labels.
  These are evidence-bank labels, not lock or product credit.
- VT.W0 remains RED even after a CSS-only repair until the JSON oracle, incumbent
  profiles/hot leaves, four private compiling placeholders, and external RED gates
  also close.
- No VT candidate exists here; candidate count and complete-candidate credit remain
  **zero**. No performance ranking or BANK/RETIRE candidate disposition is
  applicable.
- Do not request another hostile pass on current bytes. After the corrected CSS
  assay's own strict gate is clean, refreeze all source/corpus/receipt ledgers and
  start Gestalt 1 again with two fresh critics and a fresh adjudicator. Any correct
  finding then resets the count again; only two consecutive clean passes over
  corrected bytes can converge.
- No production, package, source, tranche, or downstream consumer movement is
  authorized.

This adjudication supersedes critic judgments where it corrects counts or receipt
claims. It does not edit, erase, or diminish either critic's preserved evidence.
