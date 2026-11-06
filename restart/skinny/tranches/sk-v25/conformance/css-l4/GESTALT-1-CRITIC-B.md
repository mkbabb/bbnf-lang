# CSS L4 W0 input epoch — Gestalt-1 hostile critic B

Declared critic model: `gpt-5.6-sol`  
Role: independent hostile critic B  
Frozen subject: `restart/skinny/tranches/sk-v25/conformance/css-l4`  
Pinned CSSWG checkout (read-only): `/tmp/bbnf-csswg.X76O73/csswg-drafts`  
Verdict: **BLOCK / not clean**  
Clean-audit count after this pass: **0 of 2 required hostile clean passes**

## Executive verdict

The subject reproducibly captures its own current outputs, but it does not yet
capture the exhaustive CSS L4 oracle required by `BB-VALUE-TAPE-P0`. The exact
replay is green because the checked JSON was produced by the same extractors; the
replay does not prove that the extractors found the authoritative source set or
preserved its semantics.

This audit independently found all of the following blocker classes:

1. the 84-member union is Snapshot/current-work membership, not a closed set of
   normative parsing dependencies, and 61 members have no dated publication pin;
2. the carrier scanner counts at least five commented-out carriers as live;
3. class-gated extraction misses normative grammar in modern and legacy sources,
   including four sources reported as having zero carriers;
4. raw-text `xmp` normalization deletes literal angle-bracket syntax from every
   `xmp.prod` record and from ten `xmp.idl` records;
5. production extraction leaves 33 declared `prod` carriers without a definition,
   performs no RHS parse or reference closure, and unions conflicting levels without
   profiles;
6. row extraction is a carrier-block census, not a property/descriptor/algorithm
   matrix, and likewise unions conflicting definitions without profiles;
7. WPT is represented only by a repository revision, with no frozen test inventory,
   routing, polarity, recovery, serialization, execution, or proof status;
8. no BBNF, type, owner, consumer, or candidate mapping exists, so the JSON artifacts
   can be oracle evidence only and cannot be candidate semantic/type inputs.

The README correctly calls this input born RED and already acknowledges the missing
dated pins, hostile passes, WPT binding, and normative rows. That acknowledgement is
honest, but it does not cover the extractor false positives, source-family false
negatives, raw-text corruption, or level/profile conflicts proved below. No coverage
or tranche exit can be credited from the present counts.

## Governing evidence and frozen state

The following governing files were read completely before the subject audit:

| File | SHA-256 |
|---|---|
| `restart/skinny/tranches/sk-v25/HANDOFF.md` | `9be82e72d82c09aae2e1dd251c9d516a6843a39868ce9853ef656ea44a9b9786` |
| `restart/skinny/tranches/sk-v25/LOCK-CONSTITUTION.md` | `006066f64915f02020102776093c4a519e3466fe6c31a3bbcfcfd8fef4a611e5` |
| `restart/skinny/tranches/sk-v25/TRANCHE-PORTFOLIO.md` | `44d9f646b57898bcbb90c334ce5ccf030474764bdec9fabb6cf2e765e9442181` |
| `restart/skinny/tranches/sk-v25/waves/BB-VALUE-TAPE-P0.md` | `0564548b2e7149e9ec8f9651c23f66ba277fa21db5ed8406c52f0be66aa1dfb0` |

The binding requirements are unambiguous:

- `BB-VALUE-TAPE-P0.md:42-52` requires every Snapshot constituent/exception,
  every current-work L4 feature, all normative parsing dependencies, and bound WPT,
  with missing syntax, recovery, values, serialization, or typed projection RED.
- `BB-VALUE-TAPE-P0.md:94-116` requires a machine-readable matrix mapping every
  normative production/algorithm family to BBNF, grammar-derived rows, positive and
  negative fixtures, recovery, diagnostics, serialization, owner, and status; any
  `UNMAPPED`, `UNRUN`, or `UNPROVEN` row forbids a coverage percentage.
- `BB-VALUE-TAPE-P0.md:165-176` makes BBNF the sole candidate semantic/type input
  and forbids JSON/CSS rule tables, sidecar schemas, and post-hoc repair as inputs.
- `TRANCHE-PORTFOLIO.md:3-19` supersedes horizontal selection and makes the binding
  experiment twelve complete, non-sharing value-tape verticals. Until those gates
  close, no production transposition, source movement, or hybrid is authorized.

The pinned checkout was independently verified at
`c7573530343759ace8e46438a1fa2c44515b5554`; tracked-state dirt count was `0`.
`source_manifest.py --check` independently re-read and hashed all 84 paths, totaling
`10,454,319` bytes. Thus the source-byte pin is reproducible. The findings below are
about the chosen set and its semantic extraction, not unnoticed checkout drift.

Per instruction, no CSS critic-A file was opened, enumerated, or searched. The final
networked W3C inventory recapture was not invoked: `inventory.py` contains automatic
HTTP-429 retries, while this audit explicitly prohibited W3C network retries. The
pinned-GitHub scope replay and all offline commands were run exactly.

## Numbered findings

### 1. BLOCKER — the 84-member scope is not normative-dependency closed and is split across input epochs

`scope.json` says exactly what its implementation does: it unions CSSWG Snapshot
source sections with the phase-one current-work L4 seed and the two foundations.
Neither `scope_capture()` nor any later stage walks normative dependencies. The
independent source scan found **165 distinct explicit `[[!…]]` normative reference
keys** across the 84 pinned sources. Examples of explicitly cited CSS-family inputs
that are outside `scope.json`, despite having sources in the same pinned checkout,
include:

- `CSS-ANIMATIONS-2` → `css-animations-2/Overview.bs`;
- `CSS-CASCADE-3` → `css-cascade-3/Overview.bs`;
- `CSS-GAPS-1` → `css-gaps-1/Overview.bs`;
- `CSS-OVERFLOW-3` → `css-overflow-3/Overview.bs`;
- `CSS-PAGE-3` → `css-page-3/Overview.bs`;
- `CSS-RUBY-1` → `css-ruby-1/Overview.bs`;
- `FILL-STROKE-3` → `fill-stroke-3/Overview.bs`;
- `WEB-ANIMATIONS-2` → `web-animations-2/Overview.bs`.

This list is evidence of absent closure, not a claim that every normative citation is
a value-grammar dependency. The artifact has no classification that could prove
which references are parsing dependencies and which are not, so they remain
`UNPROVEN` under the wave law. Bikeshed autolink-created dependencies are also not
enumerated by the current `[[!…]]` scan and require authoritative resolution.

The epoch is also incomplete. `inventory.json` pins dated publication bytes for 23
canonical TR members; `scope.json` contains 84 members, leaving **61 without a dated
publication pin**. `source-manifest.json` deliberately replaces those publication
bytes with editor-source paths at the CSSWG commit. That is a useful editor-source
epoch, but it is not the wave's still-RED dated-publication union. Five phase-one
record IDs also differ from their canonical TR shortnames:

| Phase-one ID | Canonical TR ID |
|---|---|
| `cascade-4` | `css-cascade-4` |
| `writing-modes-4` | `css-writing-modes-4` |
| `selectors4` | `selectors-4` |
| `pseudo-4` | `css-pseudo-4` |
| `css4-background` | `css-backgrounds-4` |

`scope_capture()` canonicalizes the URLs, so the current union happens to join these
five. The inconsistent IDs still make direct cross-artifact joins unsafe and should
be eliminated in the next epoch rather than normalized differently by each consumer.

**Required disposition:** remain RED. Freeze one schema-2 epoch containing canonical
IDs, dated publication pins, editor-source pins, resolved normative dependency edges,
and an adjudicated parsing-dependency classification. Fail closed on an unresolved
edge.

### 2. BLOCKER — the carrier scanner treats commented-out source as normative input

`syntax.carriers()` searches opening tags in the raw document before excluding HTML
comments. `normalized()` removes comments only from a carrier's body; it does not
prevent a carrier opening tag inside a comment from being selected. Five declared
records are therefore commented-out material:

| Source | Line | Counted kind | Commented material |
|---|---:|---|---|
| `css-color-4/Overview.bs` | 3844 | `prod` | superseded `color()` grammar |
| `css-color-5/Overview.bs` | 2840 | `prod` | superseded `light-dark()` grammar |
| `css-conditional-3/Overview.bs` | 887 | `idl` | removed `CSSDocumentRule` interface |
| `css-display-3/Overview.bs` | 1246 | `propdef` | removed `display-or-not` property |
| `css-display-4/Overview.bs` | 1259 | `propdef` | removed `display-or-not` property |

These inflate the declared totals by at least two `prod`, two `propdef`, and one
`idl` carrier. The two property records flow into `rows.json`; the two production
records flow into `productions.json`. Merely subtracting these known false positives
would reduce the declared 1,144 carriers to 1,139, the 690 rows to 688, and the 512
split definitions to 510—before adding any of the omitted live material in finding
3. Therefore none of those declared totals is an authoritative inventory count.

`css-syntax-3/Overview.bs:753-761` is the adversarial counterpart: literal `<!--`
and `-->` tokens occur inside two real railroad carriers. A generic HTML comment
strip over the entire document would incorrectly delete the second carrier. The fix
must preserve raw-text/carrier context; it cannot be another global regular
expression.

**Required disposition:** reject the present census. Parse comment and raw-text
state before carrier selection, and add frozen regression cases for all six lines
above.

### 3. BLOCKER — class-gated extraction omits live normative grammar, including four “zero-carrier” sources

The scanner recognizes only six HTML tags carrying one of five exact class tokens:
`propdef`, `descdef`, `prod`, `railroad`, or `idl`. CSSWG sources do not consistently
mark normative grammar that way. The pinned corpus contains live, explicitly
normative plain `<pre>` grammar blocks outside the census, including at least:

| Source | Lines | Omitted normative material |
|---|---:|---|
| `css-animations-1/Overview.bs` | 205-213 | `@keyframes` and three productions |
| `CSS2/Overview.bs` | 16210-16299, 16311+ | core grammar and lexical scanner |
| `css-fonts-3/Overview.html` | 2149-2166 | `font_face_rule` and `descriptor_declaration` |
| `mediaqueries-3/Overview.html` | 550-565, 574-583 | media-query grammar and scanner additions |
| `mediaqueries-4/Overview.bs` | 895-922, 1396-1398 | main media grammar and `mq-boolean` |
| `mediaqueries-5/Overview.bs` | 883-910 | main media grammar |
| `css-namespaces-3/Overview.bs` | 150-154, 277-293 | `@namespace` and qualified-name grammar |
| `css-style-attr-1/Overview.src.html` | 162-172 | style-attribute declaration grammar |
| `selectors-3/Overview.src.html` | 1448+, 2366-2440, 2454+ | `nth`, selector grammar, and scanner |

This is not a speculative tag heuristic: the surrounding normative prose says
“defined as follows,” “formal grammar,” or “the productions are,” and the quoted
blocks contain their productions. Four of the README's five zero-carrier sources—
`css-namespaces`, `css-style-attr`, `css3-mediaqueries`, and `selectors-3`—contain
such live grammar. Only the Snapshot overview is reasonably carrier-free. In
addition, Media Queries 3 contains **13** legacy `<div class=media-feature>`
definition rows at lines 783-1113, none recognized as `descdef`.

Modern sources are affected too: the principal Media Queries 4 and 5 grammar blocks
are plain `<pre>`, so the presence of a few other `prod` carriers in those specs does
not make the per-spec census complete.

**Required disposition:** do not grow an ad-hoc list of class names until the count
turns green. Define the normative source families and extract authoritative spans
with source-aware adapters (Bikeshed definitions plus explicit legacy grammar/table
adapters), retaining exact source coordinates and a reason each span is normative.
Every in-scope spec must terminate as mapped or explicitly adjudicated no-grammar.

### 4. BLOCKER — raw-text normalization deletes production and WebIDL type syntax

The scanner accepts `xmp` as a carrier tag, then applies the same `MARKUP` regular
expression used for normal HTML bodies. In an `xmp` raw-text body, literal
angle-bracket sequences are content, not nested markup. The independent census found:

- 114 `xmp` carriers: 79 `propdef`, 30 `idl`, and 5 `prod`;
- **all 5 of 5 `xmp.prod` carriers are altered**, losing 34 angle-bracket tokens;
- **10 of 30 `xmp.idl` carriers are altered**, losing 22 generic/type tokens.

Concrete corruption in checked `syntax.json` includes:

- CSS Values 4 line 428: `example( <length>, <length> )` becomes `example( , )`;
- CSS Values 4 line 518: `<foo> = … <bar> …` becomes
  `= keyword | | some-really-long-pattern-of-stuff =`;
- CSS Values 5 line 268: parameterized `<boolean-expr[ <test> ]>` is destroyed;
- Font Loading line 512 loses `sequence<FontFace>`, `setlike<FontFace>`,
  `Promise<sequence<FontFace>>`, and related type arguments;
- Web Animations line 6324 loses sequence element types.

This directly explains the three `unrecognized-equals` and one `empty-rhs`
production classifications; the fifth corrupted `xmp.prod` carrier is classed
`expression-only`. The stored raw hashes prove which bytes were selected but cannot
repair the corrupted normalized semantic text.

**Required disposition:** raw-text elements must be decoded as raw text, with only
entity/whitespace normalization that is valid for that source family. Add exact
round-trip fixtures for nested WebIDL generics and parameterized CSS productions.

### 5. BLOCKER — production splitting is not grammar parsing, reference closure, or profile selection

`productions.py` uses a boundary regular expression over flattened text. It neither
parses CSS value-definition syntax nor resolves references. Its own output is born
incomplete:

- 291 declared `prod` carriers;
- 258 `split` carriers producing 512 definition strings;
- 29 `expression-only`, 3 `unrecognized-equals`, and 1 `empty-rhs` carrier;
- therefore **33 declared carriers produce no definition**.

Several expression-only carriers are normative at-rule or functional grammars, such
as `@layer`, `@media`, `@supports`, `@container`, and `@counter-style`; others are
pedagogical examples. The extractor records no source-context distinction and maps
neither category. Conversely, `class=prod` is not proof of a named normative
production: CSS Values 3 line 200 and CSS Values 4 lines 220/428/440/518 include
examples used to explain notation.

An independent conservative normalization of `<<…>>` references found:

- 512 definition instances but only 416 normalized LHS names;
- 91 LHS names defined more than once;
- **41 LHS names with differing RHS text** across the indiscriminate level union;
- 408 unique non-property reference names in production RHS and row values;
- **88 reference names / 748 occurrences without a definition in this dataset**.

Some unresolved names are legitimate external primitives (`length`, `number`,
`ident-token`, and so on); others, such as `media-query-list`, `mq-boolean`, and
legacy CSS2 productions, expose the omitted carriers. The point is not to label all
88 invalid. The artifact has no prelude/dependency/external classification at all,
so it cannot prove which are closed and which are intentional imports.

Conflicting definitions are expected when levels 3, 4, and 5 are unioned, but a
consumer needs a profile and precedence rule. Examples include `color`, `position`,
`ratio`, `repeat-style`, `image`, `supports-feature`, and `calc-value`. No profile in
the subject selects or composes them. Feeding the union to a product would be
ambiguous and grammar-name special casing would violate the constitution.

**Required disposition:** parse RHS syntax through an explicit grammar, resolve every
reference to a source-qualified definition/import/prelude, and emit conflicts as
profile decisions rather than silently retaining all definitions. Every current
non-split carrier must be mapped or terminally adjudicated.

### 6. BLOCKER — `rows.json` is not the required normative-row/algorithm matrix

The row disposition `complete` means only that a carrier yielded `Name` and `Value`
without a prefix. It does not establish source completeness, entity cardinality,
version precedence, an algorithm mapping, fixtures, recovery, serialization, owner,
or execution status.

Independent row accounting found:

- 690 carrier rows, of which 689 have a `name` (the remaining prefixed record is the
  `globalCompositeOperation` anomaly);
- 10 rows are `partial-extension` and 1 is `anomalous`;
- 85 rows contain multiple comma-separated names;
- expanding those names yields **919 name instances**, not 690 entities;
- only 615 unique `(kind, name)` pairs exist;
- 228 `(kind, name)` pairs repeat across rows, and **149 have different `Value` or
  `New values` text**.

Examples include six `display` rows (levels plus Flex/Grid extensions), four
`text-align` rows, and three each for many CSS2/level-3/level-4 properties. The ten
extension rows are not joined to a base definition. There is no consumer profile to
say whether a repeated value replaces, extends, or coexists with another level.
The two commented `display-or-not` rows from finding 2 are nevertheless counted
complete, while all 13 legacy Media Queries 3 feature rows from finding 3 are absent.

Most importantly, there is no artifact for normative algorithms at all. The wave
requires production/**algorithm family** mapping, including tokenization, parsing,
error recovery, and serialization. Property and descriptor tables alone cannot
represent that plane.

**Required disposition:** give every source-qualified entity a stable identity,
explode multi-name rows without losing shared provenance, explicitly compose
extensions, select them by versioned consumer profile, and add the missing algorithm
families and statuses. Rename `complete` to a narrower syntactic term until those
obligations close.

### 7. BLOCKER — the WPT pin is not a WPT inventory or conformance binding

The only WPT data in the JSON chain is the same repository/revision pair in
`inventory.json` and `scope.json`; `source-manifest.json`, `syntax.json`,
`rows.json`, and `productions.json` contain no WPT field:

`37cd7ff74eb974fd41600bf7bc01d37576abf8b5`

There is no WPT checkout path, manifest hash, selected-test list, source-to-test
mapping, positive/negative classification, recovery expectation, serialization
expectation, run result, owner, or status. Yet the pinned CSSWG sources themselves
contain **2,055 `<wpt>` elements across 53 specs**. None flows into
`source-manifest.json`, `syntax.json`, `rows.json`, or `productions.json`.

A Git revision proves only an intended repository epoch. It does not prove that a
test exists at that revision, that all relevant tests were selected, or that a
candidate ran and passed them. It also does not cover independently authored
negative fixtures where WPT is silent.

**Required disposition:** freeze the WPT tree/manifest at the declared revision,
derive and adjudicate a source-qualified test union, bind each matrix row to positive
and negative/recovery/serialization evidence, and retain `UNMAPPED`, `UNRUN`, and
`UNPROVEN` explicitly. No percentage or CSS coverage claim is permitted now.

### 8. BLOCKER — green tests prove deterministic self-replay, not completeness

All exact offline replays passed:

```text
................
Ran 16 tests in 0.006s — OK
CSS L4 scope verified: 84 union specs
CSSWG source manifest verified: 84 union specs
CSSWG syntax census verified: 1144 carriers
CSS definition rows verified: 690 rows
CSS production split verified: 512 definitions from 291 carriers
```

The 16 unit tests are small synthetic tests for the implemented mechanics. They have
no fixture for:

- an opening carrier tag inside an HTML comment;
- literal comment tokens inside a live carrier;
- raw-text angle-bracket productions or nested WebIDL generics;
- plain/legacy normative `<pre>` grammar or legacy media-feature tables;
- the actual pinned sources as an independently asserted inventory;
- RHS parsing and reference closure;
- multi-name expansion, extension composition, or level/profile selection;
- dated-publication closure, WPT routing, algorithms, or the BBNF matrix.

The data `--check` commands recompute with the same implementation and compare to its
previous JSON. They correctly detect byte or implementation drift; they cannot detect
a stable extractor blind spot. A green receipt is therefore reproducibility evidence,
not a semantic coverage gate.

**Required disposition:** preserve these receipts, but add hostile source fixtures
and an independent expected matrix whose rows are not generated by the extractor
under test. A completeness gate must fail when a normative source span is neither
mapped nor explicitly adjudicated.

### 9. BLOCKER — there is no permissible product/tranche promotion from these sidecars

The subject contains no BBNF module, BBNF production mapping, grammar-derived type,
recovery product, serializer contract, responsible module, consumer edge, profile,
candidate placeholder, or run status. It therefore cannot satisfy VT.W0, much less
advance one of the twelve VT candidates.

Under the BBNF-only law, `rows.json` and `productions.json` may be independent oracle
evidence; they may not become semantic/type input tables for a candidate. Treating
this source census as a selectable horizontal “CSS tranche” would repeat the exact
horizontal-stage error superseded by `TRANCHE-PORTFOLIO.md:3-19`. There is also no
authorization here for production source movement or a hybrid.

The KISS route is not another regex layer or a parallel “fixed” manifest. Keep one
auditable W0 oracle pipeline with four explicit stages:

1. canonical, dated, dependency-closed source and WPT epochs;
2. source-aware carrier/algorithm extraction with exact spans and legacy adapters;
3. parsed, source-qualified grammar/reference/profile closure;
4. one matrix routing BBNF, types, fixtures, recovery, serialization, owner, and
   terminal status.

Candidate implementations must independently derive their semantic/type products
from BBNF and compare against that external oracle. This is the smallest design that
honors both the source diversity and the no-sidecar/no-overfit law.

**Required disposition:** keep this subject as RED W0 evidence, correct it in a new
adjudicated input epoch, and do not credit candidate, product, or production movement.

## Exact replay and audit commands

All commands ran from `/Users/mkbabb/Programming/bbnf-lang` unless a `git -C` path is
shown. `/path/to/...` in the README was instantiated only as the pinned checkout
listed at the top of this report.

```sh
git -C /tmp/bbnf-csswg.X76O73/csswg-drafts rev-parse HEAD
git -C /tmp/bbnf-csswg.X76O73/csswg-drafts status --porcelain=v1 --untracked-files=no | wc -l

python3 -B -m unittest discover \
  -s restart/skinny/tranches/sk-v25/conformance/css-l4/tests \
  -p '*.py'

python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/inventory.py \
  --wpt-revision 37cd7ff74eb974fd41600bf7bc01d37576abf8b5 \
  --scope-from restart/skinny/tranches/sk-v25/conformance/css-l4/data/inventory.json \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/scope.json

python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/source_manifest.py \
  --scope restart/skinny/tranches/sk-v25/conformance/css-l4/data/scope.json \
  --csswg-root /tmp/bbnf-csswg.X76O73/csswg-drafts \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/source-manifest.json

python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/syntax.py \
  --manifest restart/skinny/tranches/sk-v25/conformance/css-l4/data/source-manifest.json \
  --csswg-root /tmp/bbnf-csswg.X76O73/csswg-drafts \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/syntax.json

python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/rows.py \
  --census restart/skinny/tranches/sk-v25/conformance/css-l4/data/syntax.json \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/rows.json

python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/productions.py \
  --census restart/skinny/tranches/sk-v25/conformance/css-l4/data/syntax.json \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/productions.json
```

The prohibited network command was intentionally not run:

```sh
python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/inventory.py \
  --wpt-revision 37cd7ff74eb974fd41600bf7bc01d37576abf8b5 \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/inventory.json
```

Independent audits were read-only Python/`jq`/`rg` passes over the explicit manifest
paths. Their reproducible core queries were:

```sh
# All explicit normative citation keys in exactly the 84 manifest sources.
python3 -B - <<'PY'
import json, re
from pathlib import Path
from collections import defaultdict
p = Path('restart/skinny/tranches/sk-v25/conformance/css-l4/data')
r = Path('/tmp/bbnf-csswg.X76O73/csswg-drafts')
m = json.loads((p/'source-manifest.json').read_text())
refs = defaultdict(set)
for s in m['specs']:
    text = (r/s['path']).read_text()
    for x in re.finditer(r'\[\[!([^\]|\s]+)', text):
        refs[x.group(1).split('#', 1)[0].upper()].add(s['id'])
print(len(refs))
PY

# Comment spans containing recognized carrier openings; css-syntax-3:759 is the
# literal CDO/CDC adversarial case, while the other five are false positives.
python3 -B - <<'PY'
import json, re
from pathlib import Path
p = Path('restart/skinny/tranches/sk-v25/conformance/css-l4/data')
r = Path('/tmp/bbnf-csswg.X76O73/csswg-drafts')
m = json.loads((p/'source-manifest.json').read_text())
op = re.compile(r'<(?P<tag>pre|table|span|div|xmp|code)\b(?P<a>[^>]*)>', re.I)
cl = re.compile(r'\bclass\s*=\s*(?:"([^"]*)"|\'([^\']*)\'|([^\s>]+))', re.I)
kinds = {'propdef','descdef','prod','railroad','idl'}
for s in m['specs']:
    text = (r/s['path']).read_text()
    for c in re.finditer(r'<!--.*?-->', text, re.S):
        for x in op.finditer(c.group()):
            q = cl.search(x['a'])
            if q and set(next(v for v in q.groups() if v is not None).lower().split()) & kinds:
                print(s['id'], text.count('\n', 0, c.start()+x.start())+1)
PY

# Declared per-tag carrier totals.
jq -r '[.records[].tag]|group_by(.)[]|[.[0],length]|@tsv' \
  restart/skinny/tranches/sk-v25/conformance/css-l4/data/syntax.json

# All non-split production carriers and all non-complete rows.
jq -r '.carriers[]|select(.disposition!="split")|
  [.spec_id,.line,.disposition,.text]|@tsv' \
  restart/skinny/tranches/sk-v25/conformance/css-l4/data/productions.json
jq -r '.rows[]|select(.disposition!="complete")|
  [.spec_id,.line,.disposition,.prefix,(.fields|tojson)]|@tsv' \
  restart/skinny/tranches/sk-v25/conformance/css-l4/data/rows.json

# Legacy media-feature rows omitted by the carrier vocabulary.
rg -n '<div class=media-feature>' \
  /tmp/bbnf-csswg.X76O73/csswg-drafts/mediaqueries-3/Overview.html

# WPT elements in the exact manifest source set were counted with
# len(re.findall(r'<wpt\b', source, re.I)) for each manifest path.
```

The XMP corruption, row cardinality/conflict, and production reference/conflict
counts were computed by importing the frozen regex/constants from `src/syntax.py`
and independently walking only the manifest paths and checked JSON objects. No
temporary or subject file was written by those audits.

## Subject receipt hashes

These hashes were recomputed during this pass:

| Artifact | Bytes | SHA-256 |
|---|---:|---|
| `README.md` | 6,645 | `6a528495949457660cfc170657a35f642cc08fe285f4793c209ea8635152846c` |
| `src/inventory.py` | 15,966 | `dfe74f5bd3780d5a4035c9024d2439a14228e64242180d2436647202be7efa3d` |
| `src/source_manifest.py` | 3,223 | `2644bdfc472564b21e17149d1ad6f6550ce60ae6aa3f1f9fd91410395363e0e1` |
| `src/syntax.py` | 5,135 | `e8244b0d99bf683426363f4b3394d73d5ded30d268f5d057d1b1cafc804dd7e9` |
| `src/rows.py` | 3,840 | `b7f63bb5db9e6d98d3de3adb73c0368f6fa861a90171668ce919ea992e0372e8` |
| `src/productions.py` | 3,134 | `b21d78799a118085d3cefd00952c4839e17bdd31d535abe33ddbf1a28b65e17a` |
| `tests/inventory.py` | 4,129 | `9e18cffd0f734627beecf49f356113ed1167f6166011962c717b1921a83388ef` |
| `tests/source_manifest.py` | 2,775 | `d81218fa55abb68d7011e090474679f40dae6ad2545571a2299bf5aa898f3979` |
| `tests/syntax.py` | 2,624 | `a487ee8d3de3451158f3cb2ba206a3766438261702c18d99e9b1d052db9d6fa2` |
| `tests/rows.py` | 2,290 | `eb68d55c8634fd9a7e2df9e25b3491532ea4b68530a09385f74da8f9f4161d4c` |
| `tests/productions.py` | 2,473 | `91ce04ad5789e4a154eff03409cbb63c48219db0f460c61e20d64bd77de11bd6` |
| `data/inventory.json` | 9,077 | `b7d6d155705e6654a7f80c72711f11fcc21863b8096a79b714b7f4a5de770fae` |
| `data/scope.json` | 15,443 | `99e2223fb686443b6e612d659115940a20d3c053014201df51bf9c2ec52be50b` |
| `data/source-manifest.json` | 21,457 | `ac236359f163d444a1f2bbb20692f930fb677c10b2f0ec6837bfbb0dd91e6300` |
| `data/syntax.json` | 697,696 | `293573fe4071646c9c9eab97e8358a5b21ec16f28a0c25a796a311c2f2518d78` |
| `data/rows.json` | 461,416 | `67fbbbcd9e93d0d6f2aafa86aa9031cc00c13e0f4736ffb34bdeb9cefbc1e6f2` |
| `data/productions.json` | 208,383 | `e0e4cc80af1f8b038b98d22f569271fda5c7b62bfa2de5f73d06b9289421d17a` |

## Final disposition

**Clean count: 0.** The exact bytes are pinned and the self-replay is deterministic,
but the input epoch is not complete, semantically faithful, reference-closed,
profiled, WPT-bound, or mapped to BBNF/product obligations. Findings 1-9 are blocking
for VT.W0 closure. No production movement, candidate credit, hybridization, or CSS
coverage percentage is authorized by this artifact.

## Digest seal

Digest convention:

- **Body SHA-256** hashes every byte from byte 0 through the byte immediately before
  the `## Digest seal` heading.
- **Normalized full-file SHA-256** hashes the entire final file after replacing the
  64 hexadecimal characters on the normalized-full line below with 64 ASCII zeroes.
  This self-normalization lets the report carry a digest covering its own seal
  without making a false impossible self-hash claim.

Body SHA-256: `b6ecc842e46fb78dd2f0c237ded4ef11654f8532298e73cf999ba8797f43427c`

Normalized full-file SHA-256: `b634b01a3c8dd6953fbd552a7a5e145d0f2a6e624b825d4d404052e0f477ce12`
