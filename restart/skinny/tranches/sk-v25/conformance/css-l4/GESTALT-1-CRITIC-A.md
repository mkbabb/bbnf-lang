# CSS L4 W0 input epoch — Gestalt 1 critic A

Date: 2026-07-18  
Declared reviewer: fresh independent critic A  
Declared model / effort: `gpt-5.6-sol` / `xhigh`  
Subject: `restart/skinny/tranches/sk-v25/conformance/css-l4` before this report  
Pinned CSSWG checkout: `/tmp/bbnf-csswg.X76O73/csswg-drafts` at
`c7573530343759ace8e46438a1fa2c44515b5554`

## Verdict

**GESTALT 1: RED. Consecutive-clean count: 0. Movement: denied.**

The 84-member set faithfully replays the *stated* Snapshot/current-work/foundation
selection, and the source manifest faithfully binds 10,454,319 bytes from 84
paths in the clean pinned checkout. Those narrow receipts survive. The epoch does
not freeze the constitutional CSS scope, however, and its derived carrier, row,
and production data are not faithful enough to bank:

- 61 of 84 members have no dated-publication pin, normative dependencies have no
  classified union, and WPT is only a revision string;
- classless current and legacy grammars are omitted while commented-out,
  example, historical, and explicitly non-normative carriers are counted;
- quote-unaware markup removal corrupts at least two extracted productions;
- 690 carrier rows are not a property/descriptor identity matrix, and 512 split
  entries are not 512 source-valid named definitions;
- railroads, IDL normativity, value-definition parsing, reference closure, BBNF
  mapping, recovery/serialization rows, and executable oracle routing remain
  absent.

This remains an incomplete conformance input. It is never an implementation,
prototype, semantic coverage result, or candidate evidence.

## Read and immutable subject ledger

I read completely `LOCK-CONSTITUTION.md`, `waves/BB-VALUE-TAPE-P0.md`,
`CHALLENGE-LEDGER.md`, and every one of the 18 pre-report files under this
directory. The author receipts for all data/source/test files match the bytes on
disk. The complete pre-report hashes were:

```text
32dae3052f331ee34d628ef535709b301259a45df7c7522c4d35dcf49873f00b  .gitignore
6a528495949457660cfc170657a35f642cc08fe285f4793c209ea8635152846c  README.md
b7d6d155705e6654a7f80c72711f11fcc21863b8096a79b714b7f4a5de770fae  data/inventory.json
e0e4cc80af1f8b038b98d22f569271fda5c7b62bfa2de5f73d06b9289421d17a  data/productions.json
67fbbbcd9e93d0d6f2aafa86aa9031cc00c13e0f4736ffb34bdeb9cefbc1e6f2  data/rows.json
99e2223fb686443b6e612d659115940a20d3c053014201df51bf9c2ec52be50b  data/scope.json
ac236359f163d444a1f2bbb20692f930fb677c10b2f0ec6837bfbb0dd91e6300  data/source-manifest.json
293573fe4071646c9c9eab97e8358a5b21ec16f28a0c25a796a311c2f2518d78  data/syntax.json
dfe74f5bd3780d5a4035c9024d2439a14228e64242180d2436647202be7efa3d  src/inventory.py
b21d78799a118085d3cefd00952c4839e17bdd31d535abe33ddbf1a28b65e17a  src/productions.py
b7f63bb5db9e6d98d3de3adb73c0368f6fa861a90171668ce919ea992e0372e8  src/rows.py
2644bdfc472564b21e17149d1ad6f6550ce60ae6aa3f1f9fd91410395363e0e1  src/source_manifest.py
e8244b0d99bf683426363f4b3394d73d5ded30d268f5d057d1b1cafc804dd7e9  src/syntax.py
9e18cffd0f734627beecf49f356113ed1167f6166011962c717b1921a83388ef  tests/inventory.py
91ce04ad5789e4a154eff03409cbb63c48219db0f460c61e20d64bd77de11bd6  tests/productions.py
eb68d55c8634fd9a7e2df9e25b3491532ea4b68530a09385f74da8f9f4161d4c  tests/rows.py
d81218fa55abb68d7011e090474679f40dae6ad2545571a2299bf5aa898f3979  tests/source_manifest.py
a487ee8d3de3451158f3cb2ba206a3766438261702c18d99e9b1d052db9d6fa2  tests/syntax.py
```

The subject directory was already wholly untracked in the dirty parent worktree.
I did not modify any pre-existing subject or any CSSWG file.

## Exact replay

From `/Users/mkbabb/Programming/bbnf-lang`:

```sh
python3 -B -m unittest discover \
  -s restart/skinny/tranches/sk-v25/conformance/css-l4/tests \
  -p '*.py'
```

Result: exit 0, `Ran 16 tests in 0.008s`, `OK`.

```sh
python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/inventory.py \
  --wpt-revision 37cd7ff74eb974fd41600bf7bc01d37576abf8b5 \
  --scope-from restart/skinny/tranches/sk-v25/conformance/css-l4/data/inventory.json \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/scope.json
```

Result: exit 0, `CSS L4 scope verified: 84 union specs`.

```sh
python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/source_manifest.py \
  --scope restart/skinny/tranches/sk-v25/conformance/css-l4/data/scope.json \
  --csswg-root /tmp/bbnf-csswg.X76O73/csswg-drafts \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/source-manifest.json
```

Result: exit 0, `CSSWG source manifest verified: 84 union specs`.

```sh
python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/syntax.py \
  --manifest restart/skinny/tranches/sk-v25/conformance/css-l4/data/source-manifest.json \
  --csswg-root /tmp/bbnf-csswg.X76O73/csswg-drafts \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/syntax.json
```

Result: exit 0, `CSSWG syntax census verified: 1144 carriers`.

```sh
python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/rows.py \
  --census restart/skinny/tranches/sk-v25/conformance/css-l4/data/syntax.json \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/rows.json

python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/productions.py \
  --census restart/skinny/tranches/sk-v25/conformance/css-l4/data/syntax.json \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/productions.json
```

Results: both exit 0: `CSS definition rows verified: 690 rows` and
`CSS production split verified: 512 definitions from 291 carriers`.

The intended full dated replay was also run:

```sh
python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/inventory.py \
  --wpt-revision 37cd7ff74eb974fd41600bf7bc01d37576abf8b5 \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/inventory.json
```

Result: exit 1 after 15.2 seconds. All five fetch attempts ended in
`HTTP Error 429: Too Many Requests` for
`https://www.w3.org/Style/CSS/current-work.en.html`, raised finally as
`RuntimeError: cannot fetch ...: HTTP 429`. This is a blocked network replay,
not publication completeness.

Checkout verification:

```sh
git -C /tmp/bbnf-csswg.X76O73/csswg-drafts rev-parse HEAD
git -C /tmp/bbnf-csswg.X76O73/csswg-drafts status --short
git -C /tmp/bbnf-csswg.X76O73/csswg-drafts show -s \
  --format='%H%n%aI%n%cI%n%s' c7573530343759ace8e46438a1fa2c44515b5554
```

Result: exact requested HEAD; empty status; authored
`2026-07-17T16:42:36-04:00`, committed `2026-07-17T16:42:40-04:00`, subject
`[css-values-5] Editorial reorg of calc-size()`.

An independent `curl` of Current Work returned the exact frozen digest:

```sh
curl -L --fail --silent --show-error \
  https://www.w3.org/Style/CSS/current-work.en.html | sha256sum
```

Result:
`f85a5166c5a20a8d11bf5ef74ac8a4c0d597adc7e7b014f32e318393a3627aa5  -`.
Thus the stored Current Work bytes are reproducible through `curl`; the Python
replay's 429 remains a tool/host-rate gate.

The aggregate audit was reproduced with:

```sh
python3 -B - <<'PY'
import json,re
from collections import Counter
from pathlib import Path
b=Path('restart/skinny/tranches/sk-v25/conformance/css-l4/data')
i=json.loads((b/'inventory.json').read_text())
s=json.loads((b/'scope.json').read_text())
m=json.loads((b/'source-manifest.json').read_text())
y=json.loads((b/'syntax.json').read_text())
r=json.loads((b/'rows.json').read_text())
p=json.loads((b/'productions.json').read_text())
print('scope',len(s['specs']),len({x['id'] for x in s['specs']}),len({x['latest_url'].lower().rstrip('/') for x in s['specs']}))
print('source',len(m['specs']),sum(x['bytes'] for x in m['specs']))
pins={x['latest_url'].lower().rstrip('/') for x in i['foundations']+i['level_4']}
urls={x['latest_url'].lower().rstrip('/') for x in s['specs']}
print('dated',len(pins),'missing',len(urls-pins))
print('syntax',y['counts'],'specs',len({x['spec_id'] for x in y['records']}))
print('rows',r['counts'])
names=[q.strip() for x in r['rows'] if x['fields'].get('name') for q in x['fields']['name'].split(',')]
nc=Counter(names)
print('names',len(names),len(nc),sum(1 for x in r['rows'] if ',' in x['fields'].get('name','')),sum(v-1 for v in nc.values()))
print('productions',p['counts'],'definitions',p['definition_count'])
refs=Counter(); root=Path('/tmp/bbnf-csswg.X76O73/csswg-drafts')
for x in m['specs']:
    for q in re.findall(r'\[\[!([A-Za-z0-9_.:-]+)',(root/x['path']).read_text()):
        refs[q.split('#')[0].upper()]+=1
print('normrefs',sum(refs.values()),len(refs))
PY
```

Result:

```text
scope 84 84 84
source 84 10454319
dated 23 missing 61
syntax {'descdef': 107, 'idl': 114, 'prod': 291, 'propdef': 583, 'railroad': 49} specs 79
rows {'anomalous': 1, 'complete': 679, 'partial-extension': 10}
names 919 599 85 320
productions {'empty-rhs': 1, 'expression-only': 29, 'split': 258, 'unrecognized-equals': 3} definitions 512
normrefs 1140 165
```

## Numbered findings

### F01 — The 84-member replay is exact for its stated selection, not for the constitutional scope (blocker)

`scope.json` has 84 unique IDs and URLs. Its scope labels comprise 21 Current
Work Level-4 rows, two foundations, and Snapshot membership (with overlaps): 24
official, 8 reliable, 10 fairly stable, 22 rough-interoperability, and 19 pre-CR
exception memberships. Independent inspection of `css-2026/Overview.bs` supports
that stated union.

The wave requires the union to include *all normative parsing dependencies*.
Neither `scope.json` nor any companion artifact has a dependency row, edge,
classification, or terminal status. Across the 84 pinned sources, an independent
scan finds 1,140 explicit `[[!…]]` occurrences over 165 distinct reference keys.
They include parsing/crossing authorities outside the 84-member membership set,
such as `ENCODING`, `URL`, `WEBIDL`, `DOM`, `INFRA`, `HTML`, `SVG2`, `XML-NAMES`,
and `FETCH`. Not every one is a parsing dependency; that is precisely the missing
classification. The artifact supplies no way to prove which are in or out.

The phrase “corrected membership union” therefore overstates the evidence. Bank
the exact 84-member *selection receipt*, not an exhaustive CSS-L4 scope verdict.

### F02 — Dated-publication binding is absent for 61 of 84 members (blocker)

`inventory.json` pins 21 Level-4 publications plus two foundations. Comparing its
canonical latest URLs with `scope.json` leaves exactly 61 unpinned members. Those
61 are the Snapshot-only expansion. Editor-source hashes at a CSSWG commit do not
substitute for dated W3C publication bytes, and the only full recapture command is
currently RED on HTTP 429. The declared W0 source epoch is consequently split
between 23 dated publications and 84 editor sources.

### F03 — WPT is named, not bound (blocker)

`inventory.json` and `scope.json` repeat
`37cd7ff74eb974fd41600bf7bc01d37576abf8b5`, but this directory contains no WPT
checkout receipt, file manifest, test-path routing, byte/hash binding, spec/feature
row, positive/negative/recovery classification, execution result, or skipped-test
ledger. A revision string proves neither that the object exists nor that any
required test is included or run. The constitutional WPT union and all observable
conformance bindings remain absent.

### F04 — Carrier discovery drops live classless and legacy grammar (blocker)

`syntax.py` recognizes only five class tokens on six tag names. Exact pinned-source
counterexamples include:

- `css-animations-1/Overview.bs:205-213`: a live classless `<pre>` defines
  `@keyframes`, `<keyframes-name>`, `<keyframe-block>`, and
  `<keyframe-selector>`; none is in `syntax.json`.
- `mediaqueries-4/Overview.bs:895-922` and
  `mediaqueries-5/Overview.bs:883-910`: live classless formal grammar defines the
  media-query family; the census records only two historical `ratio` productions
  for Level 4 and one unrelated production for Level 5.
- `css-namespaces-3/Overview.bs:150-154`: the live `@namespace` grammar and
  `<namespace-prefix>` definition are omitted even though the README lists this
  file merely as having no “recognized carrier.”
- `css-style-attr-1/Overview.src.html:162-172`,
  `mediaqueries-3/Overview.html:550-583`, and
  `selectors-3/Overview.src.html:2366-2500`: formal declaration-list, media-query,
  selector, and lexical-scanner grammars are all classless and entirely absent.
- `cssom-1/Overview.bs:368-376`: two live WebIDL typedef alternatives use
  `class="def lang-webidl"`, not `idl`, and are omitted.

This is not just a five-file unknown bucket. The missing syntax is visible and
normative. Discovery must inventory legacy/unclassed forms and record explicit
normativity and disposition rather than equating a CSS class name with coverage.

### F05 — The census includes dead, example, historical, and non-normative carriers (blocker)

The scanner searches opening tags before excluding HTML comments. Five clear
commented-out openings become records:

```text
css-color-4/Overview.bs:3844       prod
css-color-5/Overview.bs:2840       prod
css-conditional-3/Overview.bs:887  idl
css-display-3/Overview.bs:1246     propdef
css-display-4/Overview.bs:1259     propdef
```

The two dead `display-or-not` rows are classified `complete`; the two dead
function grammars are classified `split`. Separately, six `prod` carriers lie
inside `div.example` (one in Values 3, four in Values 4, one in Values 5).
`css-backgrounds-3:4021` and `mediaqueries-4:2320-2321` are change-history
productions, not the current grammar. `resize-observer-1:311` explicitly says its
`ResizeObservation` IDL is non-normative and not visible to JavaScript, yet it is
counted as IDL.

The `idl` set is doubly unsound: at least the commented and non-normative records
must leave, while the two live CSSOMString typedefs from F04 must enter. The
nominal count can remain unchanged while every relevant identity is wrong.

### F06 — Markup normalization is quote-unaware and corrupts grammar (blocker)

`MARKUP = re.compile(r"(?<!<)</?[A-Za-z][^>]*>|...")` stops an opening tag at a
`>` inside a quoted attribute value. Two exact corruptions are present:

```text
css-color-4:1098 source:
  <dfn dfn-for="<color>">&lt;alpha-value></dfn> = ...
syntax.json text / productions LHS:
  "><alpha-value> = ...

css-shapes-1:1365 source:
  ... <a data-xref-for="<shape-box>">margin-box</a> ...
syntax.json text:
  <<shape-box>> = <<visual-box>> | ">margin-box | ">half-border-box
```

Both carriers are reported `split`; the first has a malformed LHS and the second
has a malformed RHS. The tests cover nested same-name tags and simple markup, but
not quoted delimiters, comments, raw-text behavior, or source-offset-preserving
normalization.

### F07 — `rows.json` is a carrier flattening, not an exact property/descriptor matrix (blocker)

The mechanical dispositions are 679 `complete`, 10 `partial-extension`, and one
`anomalous`, but the identities and boundaries needed by W0 are absent:

- two of the 679 complete rows are the commented-out `display-or-not` records;
- the anomaly is Canvas `globalCompositeOperation`, a non-CSS property-shaped
  carrier, not a CSS property/descriptor row;
- 85 named carriers contain comma-separated names. Naive expansion yields 919
  name occurrences, 599 spellings, and 320 repeated occurrences across modules;
  no per-property identity, defining/overriding module, level precedence, or
  descriptor `For` qualification is resolved;
- the ten `New values` rows remain detached extensions rather than merged,
  source-ordered type rows;
- four repeated `Value:` labels are reduced to one concatenated string plus the
  label name in `duplicates`; occurrence boundaries and values are not retained
  as structured rows.

The raw carrier hash allows later re-extraction, but the present output cannot map
“every property and descriptor” or prove repeated/fallback order.

### F08 — The production splitter's counts are not definition truth (blocker)

The regular expression accepts any word-shaped token before `=` and has no source
structure or normativity model. The advertised 512 definitions therefore include
at least:

- two commented-out definitions from F05;
- three historical change-log definitions (`bg-position` and two versions of
  `ratio`);
- the Values 4 example's
  `some-really-long-pattern-of-stuff =`, counted as a named empty-RHS definition;
- the two corrupted definitions from F06.

The fixture test intentionally proves that a broken example is converted into a
definition with an empty RHS; retaining it RED is honest, but including it in
“512 named definitions” is not. Beyond those false identities, 29 production
carriers are expression-only, three have unrecognized equality text, one is
empty-RHS, and RHS tokenization/value-definition parsing/reference closure do not
exist. The `split` label means only “the regex found a boundary and nonempty text.”

### F09 — Railroad, IDL, legacy, dependency, and semantic closure gates have no owner rows (blocker)

All 49 railroads remain normalized strings: 28 from Syntax, 11 from Transforms 1,
four each from Media Queries 4/5, and one each from Images 4 and Pseudo 4. There is
no named production split, parallel-grammar agreement, or terminal disposition.
IDL has the loss/false-positive defects in F04/F05. Legacy grammars have no
classified format or transposition. The 165-reference dependency census has no
closure. No value-definition tokenizer/parser, range/multiplier/combinator model,
RHS resolver, property-extension merger, recovery/diagnostic expectation,
serialization expectation, BBNF production, responsible module, or terminal
status exists. These are all current W0 blockers, even where the README admits
them.

### F10 — Green replay tests self-consistency, not extraction correctness (major)

The five check commands re-run the same producer and compare it with its stored
JSON. The 16 unit tests are small shape fixtures. They contain no exact 84-ID
independent oracle, exhaustive override/path identity table, comment/normativity
case, classless/legacy carrier, quoted-`>` markup case, all-carrier source-offset
round trip, grouped-name identity, repeated-field occurrence model, historical
section exclusion, railroad parser, IDL dialect recognition, dependency closure,
dated 84-publication manifest, or WPT mapping/execution.

Source/test filenames are isomorphic and the direct five-module topology is
appropriately small. No module split is required to repair these defects. The
test *content*, not the directory shape, is insufficient.

### F11 — Completeness wording exceeds the evidence (major)

The following phrases must not survive unchanged into a repaired epoch:

- “corrected membership union” without “for the stated Snapshot/title-only
  selection”;
- the syntax module docstring's “normative syntax carriers”;
- “all 690 property/descriptor carriers” when two are comments and one is a
  Canvas property shape;
- “258 split cleanly” and “512 named definitions” when comments, history,
  examples, and corrupted markup are included.

The surrounding born-RED caveats correctly deny semantic acceptance, but a RED
label does not make false set identities or counts accurate.

### F12 — W0 is the right tranche, but this epoch cannot advance (blocker)

Freezing the CSS corpus before any VT candidate is the smallest useful campaign
route and is required to prevent candidate-specific scope selection. The direct
extractor topology is KISS-compatible and should be repaired in W0 rather than
replaced by a prototype or registry tower. Advancing this epoch would instead
let all twelve independent candidates inherit a corrupted and incomplete oracle,
which defeats portfolio comparability and the no-overfit law.

## Gate matrix

| Gate | Result | Evidence / required closure |
|---|---|---|
| Subject classification | PASS | README calls it a born-RED input, not an implementation/prototype. |
| Stated 84-member Snapshot/current-work/foundation selection | PASS | 84 unique IDs/URLs; scope replay green; independent Snapshot inspection agrees. |
| Constitutional exhaustive scope | RED | No normative-parsing-dependency union or classification; F01. |
| Legacy path mapping | PASS for the six selected overrides | All 84 paths exist and hash; inspected legacy targets match their source identities. Not a legacy grammar-coverage pass. |
| CSSWG revision and byte binding | PASS | Clean exact HEAD; 84 unique sources; 10,454,319 bytes; manifest replay green. |
| Dated-publication binding | RED | 23/84 pinned; 61 missing; full command blocked by 429; F02. |
| WPT binding and routing | RED | Revision string only; F03. |
| Carrier discovery completeness | RED | Live classless/current/legacy grammar and two CSSOM IDL blocks omitted; F04. |
| Carrier normativity / false-positive exclusion | RED | Comments, examples, history, non-normative IDL counted; F05. |
| Markup normalization / source fidelity | RED | Exact LHS/RHS corruption; F06. |
| Property/descriptor identity and field boundaries | RED | Flattened grouped identities, detached partials, collapsed repeats; F07. |
| Production boundary and definition identity | RED | False/corrupt definitions and 33 unresolved carriers; F08. |
| Railroad / IDL / legacy classification | RED | No closure and wrong IDL set; F09. |
| RHS value-definition parse and reference closure | RED | Not implemented. |
| Normative algorithms, recovery, diagnostics, serialization | RED | No rows or executable oracle. |
| BBNF/incumbent grammar mapping and responsible module | RED | No mapping artifacts. |
| Mechanical replay | PASS, preservation only | 16 tests and five local checks green. |
| Independent correctness tests | RED | Defects survive all author tests; F10. |
| KISS/module/test topology | PASS WITH REPAIR | Five direct source-isomorphic modules are sufficient; strengthen semantics without a registry tower. |
| Total VT campaign fit | RED / correctly routed to W0 | Required input, but corrupted epoch cannot advance; F12. |
| Production movement / prototype ranking | PROHIBITED | No implementation or candidate evidence exists. |

## Survivor and repair disposition

**Disposition: `REPAIR_W0_INPUT`; no advance; no in-place semantic acceptance.**

Bank only:

1. the exact phase-one 23-publication receipt as historical input evidence;
2. the exact 84-member stated-selection receipt, clearly relabeled as non-exhaustive;
3. the clean CSSWG commit and 84-path/10,454,319-byte source binding;
4. the small source-isomorphic module topology and counterexamples from this pass.

Regenerate in a new adjudicated input epoch after repair:

1. form and freeze the normative-parsing-dependency union, all 84-plus required
   dated publications, and a verified WPT checkout/file/routing manifest;
2. replace regex-only carrier discovery/markup deletion with a comment-aware,
   quote-aware, source-offset-preserving extraction route plus explicit legacy,
   classless, normativity, example, change-history, and dialect classifications;
3. preserve field occurrences and source order; expand/qualify property and
   descriptor identities; merge partial extensions only through explicit edges;
4. parse productions and railroads with the CSS value-definition grammar, classify
   at-rule/expression-only carriers, and close RHS references and dependencies;
5. add independent exact-scope, negative extraction, legacy, WPT, BBNF mapping,
   recovery, diagnostic, and serialization gates.

`syntax.json`, `rows.json`, and `productions.json` are counterexample-bearing
evidence only. They are not donor schemas and must be recaptured after the owning
extractors are corrected. Any repair resets both hostile passes. Clean count
remains **0**.

## Final integrity

The SHA-256 of this report is recorded after final write in the handoff message;
embedding it here would change the file being hashed. No other file was changed by
this critic.
