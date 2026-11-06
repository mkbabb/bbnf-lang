# CSS L4 W0 epoch 3 — Gestalt 1 Critic A hostile review

Date: 2026-07-19  
Role: first fresh hostile routine-correctness critic  
Task: `/root/css_w0_epoch3_critic_a`  
Delegation: none  
Network, fetch, retry, pin change, or implementation/input/artifact/test/receipt/
governance/campaign-ledger edit: none  
Files written: this report only

## Terminal verdict

**GESTALT 1: RED. Consecutive-clean count: 0.**

The CSS standards-observation direction still belongs in VT.W0, and the current
bytes reproduce deterministically on the author's already-present local inputs.
That is the limit of the passing claim. The semantic gates are not routine-correct:
the matrix validator accepts a completely fabricated green result; retained legacy
HTML sources have live normative dependencies the scanner never sees; ambiguous
source keys silently choose a checkout-order winner; the purported strict CSS,
Yacc, Lex, and WebIDL parsers accept malformed languages; 34 railroad rows have
truncated semantic spans; WPT routes are attached by document/section Cartesian
fanout rather than feature/assertion ownership; the source inventory is circular
and omits live semantics; and the future candidate crater is a substring scan that
is trivially bypassed and even allowlists a nonexistent artifact.

The author is correct that the checked artifact is RED and earns no semantic or
product credit. The current all-open BBNF, fixture, owner, recovery, diagnostic,
serialization, control, and WPT-outcome statuses are honest. Those disclosures do
not make unsound gates clean. No CSS/CSS-coverage, W0, lock, candidate,
complete-candidate, product, clean-hostile-pass, production, source-movement,
control, performance, or campaign credit is authorized.

## Review boundary and replay

I did not read an earlier CSS critic report, either campaign ledger, or reviewer
messages. I read the binding epoch-2 adjudication, current lock constitution and
VT.W0 wave, and the complete epoch-3 README, AUTHOR, source, tests, inputs,
fixtures, compact receipts, manifest, and generated JSON structures. I treated the
implementation and all author claims as suspect.

The advertised local commands reproduced:

```text
python3 -B src/bootstrap.py       exit 0; 106 authorities; 67,818,439 logical bytes
python3 -B tests/offline_runner.py
                                  exit 0; 24 tests
python3 -B src/manifest.py        exit 0; 141 files; 130,631,391 bytes
tests/proof.py in a fresh temporary run root
                                  exit 0; six A/stored and A/B artifact comparisons true;
                                  input_epoch_reproducible=true; semantic_portfolio=RED
```

Both presently configured Git worktrees returned an empty `git status
--porcelain=v1`. The host was Apple arm64, Darwin 25.4.0, macOS 26.4.1; the
executing CPython was 3.14.5 at the required binary digest; `/usr/bin/git` was
Apple Git 2.39.5 at the prose-claimed digest. These are observed host facts, not a
proof that the program enforces all of them.

## Numbered findings

### 1. The “closed” matrix gate accepts evidence-free BBNF, fixtures, WPT PASS, observed behavior, and a fabricated owner for all 8,796 rows

`src/matrix.py:23-30` declares statuses evidenced solely by spelling.
`src/matrix.py:165-186` checks enum membership, equality with compact status
records, nullness for an `UNMAPPED` owner, and a derived label. It does not require
BBNF semantic/type evidence, fixture identities, a WPT outcome, recovery,
diagnostic or serialization evidence, or an existing owner module. In particular,
`src/matrix.py:181-182` accepts any nonempty string for an `ASSIGNED` owner, while
`src/matrix.py:187-191` uses the modules artifact only to compare row IDs.

The following in-memory hostile probe changed every compact BBNF expectation to
`MAPPED`, every WPT owner to `PASS`, every behavior plane to `OBSERVED`, every
fixture to `MAPPED`, and every owner to the nonexistent `fabricated.owner`; it then
rederived the two label fields and called `matrix.validate(document, modules,
csswg)`. Result:

```text
ACCEPTED 8796 EVIDENCED OBSERVATION_COMPLETE fabricated.owner PASS
```

No evidence object was added. A parallel defect is explicit at
`src/matrix.py:26`: WPT `FAIL` is also in `EVIDENCED`, so a failed conformance test
can participate in an `EVIDENCED` / `OBSERVATION_COMPLETE` terminal state. The
unknown-status test at `tests/test_modules_matrix.py:33-49` catches a typo and an
owner attached to `UNMAPPED`; it never tries an invented `ASSIGNED` owner or an
evidence-free legal success spelling.

Retained truth: the stored artifact itself has 8,796 `UNMAPPED` BBNF/fixture/owner
planes, null responsible modules, 502 `UNRUN` plus 8,294 `UNMAPPED` WPT planes,
and 8,796 `UNPROVEN` rows in each behavior plane. No current BBNF type, owner, WPT
outcome, or behavior result is fabricated. The validator is nevertheless
fail-open and cannot be a future authority.

### 2. Normative dependency closure is incomplete, and ambiguous CSS source keys silently take the first checkout entry

The one dependency lexer is only the `[[...]]` regular expression at
`src/scanner.py:19`; `src/scanner.py:184-200` therefore cannot recognize rendered
or legacy bibliography syntax in retained `.html` sources. Those sources are not
hypothetical: `inputs/decisions.json:156`, `:172`, and `:222` select
`css-color-3/Overview.html`, `css-fonts-3/Overview.html`, and
`mediaqueries-3/Overview.html`.

For a concrete missed parsing edge, retained
`mediaqueries-3/Overview.html:544-570` states that rules not defined there come
from the CSS2 grammar and marks CSS21 normative as rendered
`<a ... rel=biblioentry>[CSS21]<!--{{!CSS21}}--></a>`. The stored scope contains
**zero** dependency occurrences for `css3-mediaqueries`; `css-color-3` also has
zero, and `css-fonts-3` has only one informative `[[...]]` occurrence despite its
retained normative references and direct CSS2 grammar links. Thus comment/edit
state is correct only for the one syntax the scanner knows; it is not occurrence
complete across the selected source formats.

There is a second closure error at `src/membership.py:185-196`: multiple directory,
shortname, level, and TR keys are collapsed with `dict.setdefault`, so the first
lexicographically scanned source wins. `src/membership.py:400-414` then treats that
winner as an exact normative source. An independent index census on the pinned
checkout found 38 duplicate keys and six collision keys actually used by live
normative occurrences:

```text
COMPOSITING    -> compositing-1, compositing-2
CSS-SHAPES     -> css-shapes-1, css-shapes-2
CSS-TEXT-4     -> css-text-4, css-text-5
CSS-VALUES     -> css-values-3, css-values-4, css-values-5
MEDIAQUERIES   -> mediaqueries-4, mediaqueries-5
WEB-ANIMATIONS -> web-animations-1, web-animations-2
```

For example, both live `CSS-VALUES` occurrences from `cssom-view-1` become imports
of `css-values-3` solely because that path was first. There is no occurrence-level
retained alias/ambiguity decision for these keys. Consequently the reported 906
normative CSS-source edges, downstream direct-import resolution, and zero local
membership blockers are not a complete or uniquely authorized dependency census.

Retained truth: the 96 selected source paths rehash at 11,676,246 bytes and all 21
Current Work Level-4 rows parsed from retained bytes occur in the selected set.
That proves selected-byte identity, not exhaustive or unambiguous dependency
membership.

### 3. Four dialect gates advertised as strict accept malformed languages

The CSS value parser turns closing `)` and `]` into ordinary delimiter atoms at
`src/dialects.py:224-227`; the top-level parser therefore accepts unbalanced
closers. The hostile probe returned ASTs for `a )`, `a ]`, bare `]`, `a }`, and
bare `}`. The ten fixtures at `fixtures/mutations.json:29-40` do not exercise this
class.

The legacy parsers recognize envelope shape, not their languages:

- `src/dialects.py:359-427` splits Yacc text at a colon and quote-aware semicolon,
  then extracts word-shaped references. It accepted `a: ??? ;`, `a: %bogus b ;`,
  and `a: b | | c ;` as productions.
- `src/dialects.py:430-447` requires a `%option` line and a name plus nonempty
  remainder. It accepted both `%option\nA [unterminated` and `%option bogus\nA ???`
  as scanner definitions without validating an option or regex.
- `src/dialects.py:450-482` is a declaration-start regex plus brace/semicolon
  search. It accepted `garbage\ninterface X {};\nmore garbage` and
  `enum X {"a"}; trailing`, silently ignoring the garbage. It also does not
  consume or validate WebIDL members and can miss a second same-line declaration.

The railroad vocabulary and the specific stored diagrams are substantially better
than epoch 2, but that does not cure the other dialects. `data/dialects.json` having
1,361 `PARSED` units and zero root blockers means only that every retained carrier
passed these recognizers. It is not strict CSS VDS, Yacc/Lex, or WebIDL proof.

### 4. The exact railroad semantic source maps are truncated for 34 of 49 rows

Each railroad node's `text_end` is set to the end of its own instruction line at
`src/dialects.py:326`. The returned tree envelope at `src/dialects.py:353` uses the
last root node's line end, not the maximum descendant end. `src/modules.py:69-70`
copies that envelope, and `src/matrix.py:120-122` uses it as the exact semantic row
span.

The stored `css-images-4::railroad::1` counterexample is executable in the artifact:
`data/dialects.json:168704-168797` has descendant nodes ending at text offset 151,
while the railroad tree itself says `text_end: 36`. The matrix source span for that
row is only 32 bytes although the carrier text is 154 characters. A full stored
census produced:

```text
railroad units                         49
root envelopes shorter than a child   34
maximum omitted suffix                349 characters
```

The byte spans that are stored do rehash, but 34 of them identify only a prefix of
the semantic tree. This directly falsifies the exact-semantic-source-row claim and
also weakens downstream closure evidence for those diagrams.

### 5. WPT “ownership” is a document/section Cartesian product, not feature or assertion ownership

`src/matrix.py:198-200` groups every source-declared route only by `(spec_id,
anchor)`. For every semantic item, `src/matrix.py:215-231` attaches the whole group
to one shared owner and labels the relation `EXACT_SPEC_SECTION_ANCHOR`. `None` is
treated as a real document-level anchor. There is no family, feature, assertion,
test-content, or WPT metadata relation.

The consequence is large and exact:

```text
WPT owner records marked UNRUN          10
semantic rows marked UNRUN             502
row x route associations implied   123,827
css-masking document owner rows        192
css-masking document owner routes      500
css-masking implied associations    96,000
```

The stored object itself calls the null-anchor masking relation exact at
`data/matrix.json:290620-290624`, begins a 500-route list there, and terminates it
with `status: UNRUN` at line 292627. Every one of 192 unrelated masking semantic
rows points at that same owner. Two other broad examples attach 187 CSS Text 3
routes to 31 rows and 146 CSSOM routes to 89 rows. Relocation stability, tested at
`tests/test_modules_matrix.py:57-59`, proves only that changing carrier byte offsets
does not change a string formed from spec and anchor; it does not prove test
semantics.

Retained truth: the WPT commit/tree/path-OID receipt exactly replays and no WPT
outcome is claimed. Until a source section is narrowed to an explicit family or
assertion relation, these 502 rows must remain semantically `UNMAPPED`, not
ownership-mapped-but-`UNRUN`.

### 6. Matrix totality is circular and does not enumerate all live source semantics

`src/matrix.py:187-191` calls a row set total when it equals
`semantic_inventory(modules)` from the same implementation. That function at
`src/matrix.py:81-98` can only emit definitions, cloned property/descriptor fields,
extensions already recognized by the extractor, coarse IDL declarations,
structural algorithm list items, and synthetic duplicate-name conflicts. It does
not compare against an independent source census or retained source-semantic
manifest.

A concrete casualty survives in the pinned corpus. Flexbox places the partial
`display`/`flex | inline-flex` property table in an informative wrapper at
`css-flexbox-1/Overview.bs:545-550`, then normatively defines `flex` immediately at
lines 556-560. Grid does the same for `grid | inline-grid` at
`css-grid-2/Overview.bs:908-924`. Structural classification correctly labels the
tables non-normative (`data/carriers.json:121688-121692` for Flexbox), but the
extractor has no semantic carrier for the following normative `<dfn>` value
definitions. The modules artifact has no `display` entity or extension for either
module. Therefore these live value additions are neither semantic rows nor open
row obligations.

The same design limitation applies generally to normative terms and behaviors
outside the small `_kind()` set at `src/extract.py:146-157`. WebIDL rows are only
coarse declarations, and algorithm rows are normalized list-item text, not a
validated step/input/outcome language. The current 8,796-row total is internally
stable, but it is not the exhaustive CSS observation matrix required by VT.W0.
The aggregate RED label is honest; it does not identify source semantics that were
never inventoried.

### 7. The future-candidate boundary is trivially bypassed and allowlists a file that does not exist

`src/boundary.py:14-18` defines a suffix allowlist and forbidden substrings.
`src/boundary.py:50-65` scans only UTF-8 text files with those suffixes, searches
for contiguous words, and inspects only lines containing the contiguous literal
`conformance/css-l4`. It does not resolve source imports, macros, build scripts,
generated artifacts, package graphs, binary inputs, symlinks, or actual build
reads.

Two temporary candidate probes both returned `PASS` with zero violations:

```text
include_bytes!(concat!("../../conformance/", "css-l4/epoch-3/data/",
                       "ma", "trix.json"))

include_bytes!("oracle.bin")
  where oracle.bin contained {"stage":"semantic-matrix"}
```

The first evades contiguous strings; the second is never scanned. In addition,
`inputs/future-candidate-contract.json:2-8` allowlists
`conformance/css-l4/epoch-3/data/observations.json`, but that file does not exist.
The positive test at `tests/test_boundary.py:29-30` accepts a source string naming
that nonexistent artifact and never resolves it.

Retained truth: there is no current VT candidate, and `data/boundary.json:32-36`
states exactly that. Thus no present candidate-side violation is inferred. The
static inspector and allowlist are not a mechanical future build/import/package
crater and earn no boundary-enforcement credit.

### 8. Replay is deterministic only with unsealed author-local checkouts; Git/host and checkout cleanliness are not enforced

`src/common.py:20-21` hardcodes the CSSWG input under a private temporary path and
the WPT input in epoch 2. `src/regen.py:65-67` uses those defaults. Neither checkout
nor the 96 CSSWG source bytes is part of the epoch-3 content-addressed object set,
and `src/bootstrap.py` does not reconstruct either repository. With explicitly
absent checkout paths, `python3 -B src/bootstrap.py` exited 1 with
`GIT_INPUT_INVALID` and a traceback. This is present-input replay, not offline
clean-root bootstrap.

`src/common.py:136-139` verifies only `rev-parse HEAD`; it never runs a worktree
cleanliness check. That matters for CSSWG because source indexing reads the working
tree. `src/common.py:121-133` invokes whatever `git` appears on inherited `PATH`,
and the proof at `tests/proof.py:54-64` inherits the environment. Only CPython is
programmatically version-and-binary pinned. `data/verification.json:212-216`
therefore contains Python identity but no Git binary, OS, or host receipt, despite
the exact prose claims in AUTHOR.

Retained truth: on this host the current checkouts were clean, the current Git and
Python binaries matched the author prose, the 96 selected CSSWG files rehashed,
and both temporary-output regenerations were byte-identical for the six governed
artifacts. That is a useful local determinism receipt, not a clean-root or full
toolchain identity proof.

### 9. The claimed independent mutation coverage is largely a prose key census

`fixtures/mutations.json:2-27` maps prior finding IDs to English descriptions.
`tests/test_boundary.py:47-50` calls a set of those keys “every ... finding” and
checks only that the names exist. It does not connect each name to an executable
mutation. The executable negative sets contain only ten CSS strings and four
railroad strings (`fixtures/mutations.json:29-46`). The candidate positive test
even blesses the nonexistent observations artifact, the WPT test checks only
offset-insensitive identity, and the matrix tests never try legal success statuses
without evidence.

The accepted counterexamples in findings 1-8 are all absent. Running the same 24
tests twice, as `tests/proof.py:57-59` does, proves repeatability of that test set,
not independent hostile completeness. The stored proof receipt and my replay are
truthful about exit status; README's assertion at lines 140-143 that the suites
cover governing hashes, closed statuses, no-fabricated-owner behavior, and future
candidate craters materially overstates what those mutations establish.

### 10. The evidence layout retains substantial removable duplication

The direct 11-module Python topology is understandable, but the sealed subject is
not the compact successor requested by the binding repair. Exact current sizes are:

```text
source implementation                 137,020 bytes
tests/support                           22,414 bytes
six generated artifacts            63,188,477 bytes
authority object store              67,196,482 bytes
owned manifested subject           130,631,391 bytes
```

The authority bytes are legitimate evidence. Much of the generated amplification
is not. Entities clone complete field/source-map structures: compact serialization
of all clones is 4,327,737 bytes versus 3,160,496 bytes for one copy per carrier,
or 1,167,241 immediately redundant bytes inside `modules.json`. The 8,796 matrix
rows use only two distinct plane vectors and two downstream decisions; 1,946 row
references reuse an already represented exact source. Nevertheless `matrix.json`
is 11,481,625 bytes. The size test at `tests/test_modules_matrix.py:60` checks only
that this artifact is below an arbitrary 12 MB ceiling, not that it beats a
simpler normalized control.

Exact per-row identities must remain reviewable, but shared immutable status
vectors, field structures, and source identities need not be expanded repeatedly.
This is parsimony and dirigibility debt under `LOCK-CONSTITUTION.md:52-56`, and it
amplifies every semantic mistake above.

## Retained truths and exact identities

The following narrow facts survive this review and should be banked only at their
stated scope:

1. The governing documents rehash exactly:

   ```text
   LOCK-CONSTITUTION.md                    b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5
   waves/BB-VALUE-TAPE-P0.md               745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562
   epoch-2/GESTALT-1-ADJUDICATION.md       781bbd1a49c9d89baab3bd393d175338192b3c714b5a97818e08ac95caf242f5
   repository HEAD                         af15f63e0d2d3d719938c13b906a50acbb92ea3b
   ```

2. `inputs/authorities.json` is 41,424 bytes at
   `f21ea6c596de9a3234903202f2f3297275bf146d3444c6a6f63e1a401495e0b4`.
   It has 106 logical records, 105 unique digests, and an object set exactly equal
   to those digests. Every stored object rehashes at its recorded length. All 96
   dated-publication rows use visibly dated W3C URLs equal to their recorded
   response target. This proves retained bytes and metadata, not dependency or
   semantic totality.

3. The current CSSWG checkout is at
   `c7573530343759ace8e46438a1fa2c44515b5554`; the selected 96 paths rehash at
   11,676,246 bytes. The current WPT checkout is at
   `37cd7ff74eb974fd41600bf7bc01d37576abf8b5`, tree
   `12fa64c9afe86e199ee0e737755b6507f363ceff`, with 160,788 blob paths and
   path/OID digest
   `cf3aedd08cbb161aeebd3c9e19c10f855f2be0be23fa5ecbbd2035d2e31db699`.
   No WPT blob content or outcome is claimed.

4. The six governed artifact identities reproduce:

   ```text
   scope.json       12,058,423  6b9cceb0a9016dad077023f9be34563b28782f8aca3afd121283cd348aa466fe
   carriers.json    11,986,795  35b4289e9346dacb2d16073468717d89337148173f27fa4f10003202f1aa12a9
   dialects.json    11,720,876  da330bc3b018357b040c4c2b0e204f1c613ec76670f51088c20887851b59288b
   modules.json     15,939,592  028724633ad66fce242e5ae8038655d1aa15bff46bd14f66083c5ad46c189544
   matrix.json      11,481,625  7c0572371be7a54a9bbef65ba3858a175e55c6595faa19f88963d7dbec82a60a
   boundary.json         1,166  18306f408ea7a0de9d8b1248af32f127316d834236b53f35901f3580b1d26c77
   ```

5. The current file manifest verifies 141 non-review files, 130,631,391 bytes,
   subject digest
   `b0e115d879ca6b9a14c7c59e73653e9891410d58e8d1532e7b1ec835e3848597`;
   the manifest file itself is
   `a35dd2c84c490c7adb72c1916e7f4bd4dc9e5997135edaad7d9ddc9247376495`.
   No symlink, cache, bytecode, or host-metadata entry was present. The future
   critic report namespace is intentionally excluded.

6. Current status honesty is useful: all 8,796 rows are obligations; BBNF,
   fixtures, and owners are all `UNMAPPED`; all responsible modules are null; WPT
   has zero outcomes; recovery, diagnostics, and serialization are all
   `UNPROVEN`; Lightning CSS and sonic-rs correctness/selection/performance remain
   `UNPROVEN`/`UNRUN`/`UNRUN`; and no candidate exists.

## Full-plane verdict

| Review plane | Verdict | Basis |
| --- | --- | --- |
| Tranche fit | **RED implementation; direction retained** | A read-only CSS oracle belongs in VT.W0, but unsound gates and 130 MB of premature expansion are not the smallest killing experiment. JSON, profiles, four private placeholders, and their external RED gates remain absent exactly as disclosed. |
| Authority and membership completeness | **RED** | Snapshot/Current Work parsing is reproducible, but legacy HTML normative edges are invisible and used source-key collisions silently choose winners. |
| Primary-source byte identities | **PASS, receipt only** | Authority objects, selected CSSWG paths, and compact WPT tree receipt rehash; this does not prove the semantic union. |
| Source scanner/edit/comment correctness | **RED** | Structural comment/`ins`/`del` handling improved, but the dependency language is not complete for selected HTML source forms. |
| Carrier and normativity classification | **RED totality; mechanics retained** | Structural examples/informative ancestry and coarse spans work, but only a small carrier class set feeds the inventory and live normative value definitions remain absent. |
| Strict dialect parsers | **RED** | CSS closers, malformed Yacc/Lex, and garbage-surrounded WebIDL are accepted. Railroad shape is improved but does not rescue the stage. |
| Exact source maps | **RED** | Coarse carrier/field hashes recheck; 34 railroad semantic rows truncate descendants. |
| Module/reference/extension closure | **RED** | AST occurrence counts are internally total, but imports rest on incomplete/colliding source edges; 1,564 unmapped and 52 ambiguous references plus three open extension bases remain honestly open. |
| Semantic-row totality and status honesty | **RED** | Present statuses are honest, but totality is circular, live source semantics are missing, and legal success spellings require no evidence. |
| WPT ownership | **RED** | Tree receipt passes; null/section anchors induce 123,827 broad associations and no outcomes exist. |
| No fabricated BBNF types/owners | **PASS current bytes; RED gate** | All current rows are unmapped/null, but the validator accepts an invented assigned owner and evidence-free mappings. |
| Downstream Lightning CSS/sonic gates | **HONESTLY OPEN / RED** | Discovery pins match governance; correctness, selection, performance, samples, and ratios are unrun. |
| Candidate boundary | **RED** | No candidate exists, but the proposed inspector is bypassable and its sole observation artifact is absent. |
| Toolchain/runtime identity | **RED full claim; Python pass** | CPython binary is pinned; Git, host, and clean worktree are prose/ambient facts. |
| File manifest | **PASS current owned subject** | Exact 141-file census and hygiene reproduce; this does not seal external checkouts. |
| Offline/deterministic clean-root replay | **RED clean-root; PASS local determinism** | Six artifacts reproduce twice from empty output roots only when the unsealed author-local CSSWG/WPT repositories already exist. |
| Resource/KISS structure | **RED** | Understandable modules, but removable cloned field/status/source evidence remains and semantic validation did not stop expansion. |
| Exact no-credit scope | **PASS** | README and AUTHOR explicitly deny every CSS/W0/lock/candidate/product/performance/production/clean-pass credit relevant here. |

## Disposition

This implementation may be retained as a deterministic, born-RED local
observation experiment and as an evidence bank for primary byte receipts, source
scanner mechanics, negative examples, and open status censuses. It is not a sealed
CSS semantic oracle or a clean hostile pass. Findings 1-10 reopen the owning CSS W0
design cluster and keep the clean count at zero. Repair must occur in a successor
implementation and be presented to a new first hostile pass; no repair is made in
this review.

No CSS/W0/lock/candidate/product/clean credit and no production, package, source,
downstream, or campaign movement is authorized.
