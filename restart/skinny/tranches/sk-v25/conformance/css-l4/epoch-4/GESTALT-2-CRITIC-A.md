# CSS L4 W0 epoch 4 — Gestalt 2 Critic A

Date: 2026-07-19  
Role: fresh hostile critic A  
Canonical task: `/root/css_w0_epoch4_g2_review_a_r3`  
Model / effort: `gpt-5.6-sol` / `xhigh`  
Delegation: none  
Files written: this report only

## Review boundary

Before reading repository files I reported the model, effort, canonical task, and
parent task to `/root`; root confirmed the recorded runtime metadata. I read the
lock constitution, governing wave, epoch-3 Gestalt 1 adjudication, and every
epoch-4 file except epoch-4 `GESTALT-*.md` reports. I did not inspect a campaign,
run, challenge, or walls ledger; Git history or diff; a Codex session/history
file; another agent; or any other repository path.

All ten epoch-4 JSON files were decoded with duplicate-key rejection and
recursively traversed (3,764,975 bytes; 120,171 nodes). All 109 physical retained
objects were read and SHA-256 checked (43,013,202 bytes), and their digest set was
compared with all 109 logical manifest references. The compressed source-key and
obligation JSON objects were decoded and duplicate-key checked; the WPT catalog
was decoded and traversed. No prohibited context appeared.

The cache-disabled offline unit suite, two-empty-output-root replay, stored-byte
comparison, file-manifest verification, and final-receipt verification were run
without network access and with all output under a fresh `/tmp` directory. No
subject file, cache, or bytecode was created. Independent counterexamples below
used only retained epoch inputs or synthetic values in temporary directories.

## Terminal verdict

**GESTALT 2 CRITIC A: RED. Six findings. Consecutive-clean count: 0.**

The assay retains useful exact-byte and honest open-state facts, but it is not a
complete repair of the epoch-3 adjudication. In particular, its grammar consumer
deletes semantic syntax before parsing, its Yacc/Lex/WebIDL recognizers still
accept malformed languages, its purported atomic source census maps whole
carriers by overlap rather than parsed occurrences, its candidate boundary is
neither an absence check nor a closed input graph, and its mutation suite does
not exercise several adjudicated counterexamples. The genesis `clean` receipt
also ignores precisely the untracked worktree entries that can enter the retained
source-key catalog.

This remains an input assay, not a VT candidate. Candidate count and complete-
candidate count receive **zero credit**. No CSS, CSS-L4, WPT-outcome, VT.W0,
PT/BB-lock, control, profile, performance, package, product, hostile-pass, clean-
count, production, or campaign movement is authorized.

## Numbered findings

### 1. `[P1]` Angle-bracket grammar and WebIDL syntax is deleted before parsing, yet affected carriers can be marked mapped

**Location:** `src/pipeline.py:206-244`, especially `src/pipeline.py:208`, and
`src/pipeline.py:297-303`; the advertised consumer claim is `README.md:50-52`.
One stored casualty is visible at `data/observations.json:59555-59583`.

`_parse_carrier()` converts every non-railroad body with
`re.sub(r"<[^>]*>", "", text)`. This is not an HTML-only operation. It deletes
CSS value-definition references such as `<length>` and mangles Bikeshed
`<<length>>` into the literal atom `>`. It also deletes WebIDL generic type syntax
such as `sequence<DOMString>`. The downstream CSS parser can then accept the
damaged string, after which `semantic_census()` assigns
`MAPPED_BASELINE_OBLIGATIONS` solely because some inherited source span overlaps
the outer carrier.

The retained CSS2 `top` property carrier at bytes `266293..266698` contains:

```text
Value: <<length>> | <<percentage>> | auto | inherit
```

The actual preprocessing/parser input is:

```text
 > | > | auto | inherit
```

It is nevertheless stored as `MAPPED_BASELINE_OBLIGATIONS` with nine reverse
links and one accepted value field. Reproduce from the epoch directory:

```sh
env PYTHONDONTWRITEBYTECODE=1 python3 -B -c '
import pathlib,sys
r=pathlib.Path(".").resolve(); sys.path.insert(0,str(r/"src"))
import pipeline
i=pipeline.load_inputs()
raw=i["source_bytes"]["css2/Overview.bs"][266293:266698]
body=raw[raw.index(b">")+1:raw.rindex(b"<")]
plain,units=pipeline._parse_carrier("propdef","SOURCE_CLASS",body)
print(plain.split("Value:",1)[1].split("Initial:",1)[0]); print(units)
'
```

Observed output is `> | > | auto | inherit` and one accepted `entity-table`
unit. This is a silent local detector loss, not merely an honestly RED row.

**Required correction:** tokenize source markup and embedded dialect text without
erasing dialect delimiters. Every accepted AST unit must retain exact source-byte
spans and the original nonterminal/type text; a damaged or untranslatable unit
must be a typed local blocker.

**Effect:** byte provenance survives, but the claimed CSS grammar/WebIDL consumer,
source occurrence count, mapped dispositions, reverse links, and blocker census
do not. Clean count is zero. No candidate exists, so there is no current product
casualty; using these observations to seed VT.W1 would corrupt types and coverage.

### 2. `[P1]` The Yacc, Lex, and WebIDL functions are still envelope recognizers, not consuming dialect parsers

**Location:** `src/languages.py:313-372` (Yacc),
`src/languages.py:375-447` (Lex), `src/languages.py:450-531` (WebIDL), and the
narrow negative set at `tests/mutations.py:37-52`.

The Yacc function validates balancing and only rejects `???`, empty alternatives,
and `%` words other than `%prec`; arbitrary RHS tokens remain accepted. The Lex
function validates brackets/macro names but does not parse Lex regular
expressions; `a**` is accepted. The WebIDL function recognizes a declaration
head, braces, and semicolon-delimited member text, but never parses member syntax;
`???;` is accepted as a member.

Reproduce:

```sh
env PYTHONDONTWRITEBYTECODE=1 python3 -B -c '
import pathlib,sys
r=pathlib.Path(".").resolve(); sys.path.insert(0,str(r/"src"))
from languages import parse_yacc,parse_lex,parse_webidl
print(parse_yacc("a: @@@ ;"))
print(parse_lex("%option noyywrap\nA a**\n%%\n{A} return TOKEN;\n"))
print(parse_webidl("interface X { ???; };"))
'
```

All three return successful AST-shaped dictionaries. The existing A-03 mutation
passes because it contains only the six pinned probes the recognizers explicitly
reject. Whole-carrier consumption is not grammar-language consumption.

**Required correction:** implement source-exact parsers for the retained Yacc,
Lex, and WebIDL dialects, or classify every unsupported construct as a typed
blocker. Add the three accepted probes above and representative valid productions,
regexes/actions, extended attributes, types, arguments, and members to an
external negative/differential corpus.

**Effect:** the stored 140 typed dialect blockers are honest as far as they go,
but accepted-carrier semantics and blocker completeness are unproved. This resets
the hostile clean count without changing candidate count zero or authorizing a
product edge.

### 3. `[P1]` The “4,954 source-native occurrences” are carrier envelopes; internal parsed units never receive atomic dispositions

**Location:** `src/pipeline.py:247-257`, `src/pipeline.py:263-309`, and
`src/pipeline.py:446-459`. A concrete stored carrier is
`data/observations.json:89036-89066`. The ineffective A-06 mutation is
`tests/mutations.py:71-74`.

`links()` returns every inherited row whose byte span overlaps an entire carrier.
After parsing, `semantic_census()` emits exactly one ledger occurrence and one
disposition for that carrier; its `units` are merely nested metadata. Yacc rules,
Lex macros/rules, WebIDL declarations/members, and multi-definition CSS
productions therefore do not each receive a source-exact identity, disposition,
or evidence link. The retained data contains 1,216 successfully parsed carrier
rows holding 1,372 parser units; 65 carriers have more than one unit. For example,
the Grid carrier at bytes `64113..65265` stores nine productions inside one
`MAPPED_BASELINE_OBLIGATIONS` occurrence and attaches eighteen carrier-overlap
links with no unit-to-link relation.

The validator is also self-referential. It checks only that the stated
`occurrence_count` equals the ledger length. Coordinately deleting a census row
and decrementing the count is accepted:

```sh
env PYTHONDONTWRITEBYTECODE=1 python3 -B -c '
import copy,json,pathlib,sys
r=pathlib.Path(".").resolve(); sys.path.insert(0,str(r/"src"))
import pipeline
d=json.loads((r/"data/observations.json").read_bytes())
d["source_semantics"]["census"].pop()
d["source_semantics"]["occurrence_count"]-=1
pipeline.validate_document(d)
print("ACCEPTED",d["source_semantics"]["occurrence_count"])
'
```

The command prints `ACCEPTED 4953`. The shipped A-06 mutation deletes a row
without decrementing the redundant counter, so it tests length bookkeeping, not
an independently anchored denominator.

**Required correction:** promote every parsed source-native semantic unit to its
own byte-qualified occurrence; map, exclude, or block it individually; and retain
explicit unit-to-baseline reverse links. Validate exact expected source-native
fixtures and mutation casualties, not a count computed from the same ledger.

**Effect:** the inherited 8,796 row identities remain atomic, but the claimed
4,954-occurrence denominator, 1,245 mapped count, 792 unmapped count, 8,199 reached
rows, 597 unreached rows, and 8,864 reverse links are not complete semantic facts.
This blocks CSS scope freeze and VT.W0 exit; candidate and product credit remain
zero.

### 4. `[P1]` Candidate absence is hard-coded, and the future crater accepts unverified external inputs as `VALID`

**Location:** `src/crater.py:24-35`, `src/crater.py:47-125`, particularly
`src/crater.py:62-65`, `src/crater.py:100-118`; the normal generator calls only
the constant capture at `src/generate.py:40`. The stronger claim appears at
`README.md:56-58`.

`capture()` takes no candidate root and always returns zero candidates. The
normal pipeline never invokes `inspect_candidate()`. Even when called explicitly,
the inspector trusts the candidate-authored `resolved-inputs.json`; it validates
the shape of `runtime_reads` but never reads the named object or verifies its
digest. A path literal outside `conformance/css-l4` and the two allowed prefixes
also receives no violation.

This temporary candidate is accepted:

```sh
env PYTHONDONTWRITEBYTECODE=1 python3 -B -c '
import json,pathlib,sys,tempfile
r=pathlib.Path(".").resolve(); sys.path.insert(0,str(r/"src")); import crater
with tempfile.TemporaryDirectory() as td:
 p=pathlib.Path(td)
 (p/"resolved-inputs.json").write_text(json.dumps({
   "allowed_external_inputs":{},
   "runtime_reads":[{"path":"/tmp/unverified-oracle.bin","sha256":"0"*64}]}))
 (p/"main.rs").write_text(
   "const X: &[u8] = include_bytes!(\"/tmp/unverified-oracle.bin\");")
 print(crater.inspect_candidate(p))
'
```

Observed result is `status: VALID` with zero violations. Thus the mechanism does
not follow resolved source/macro/build/package/generated/binary/symlink/runtime
edges and does not accept only exact digest-qualified inputs.

**Required correction:** for the current assay, derive the absence fact from a
declared, enumerated candidate root set. For future candidates, resolve the actual
build/package/import/macro/generated/runtime graph independently, require every
external node to match an allowlisted digest and retained object, and reject
untraced or nonexistent reads. Until that exists, remove the 126-line future
checker and its claim; a narrow, evidence-bearing absence statement is smaller
and more truthful.

**Effect:** no candidate implementation was observed in the allowed epoch-4
subject, and zero candidate credit remains the correct conservative disposition.
The proof does not establish candidate absence or isolation, however. The
fail-open future mechanism is a greater-product risk and needless current
authority, so clean count is zero and no candidate may advance.

### 5. `[P2]` Several adjudicated “mutations” are status/count assertions, not executable counterexamples

**Location:** the coverage claim is `README.md:63-65`; examples are
`tests/mutations.py:29-34` (A-02), `tests/mutations.py:64-68` (A-05),
`tests/mutations.py:71-74` (A-06), `tests/mutations.py:130-142` (B-F03/B-F04),
and `tests/mutations.py:187-192` (B-F10). The proof reports only a fixed count at
`tests/proof.py:89-90`.

These functions mostly assert that a current status, spelling, count, or known
string exists. They do not inject the adjudicated defect and require the owning
gate to reject it. B-F04, for example, accepts any artifact claiming one
`WPT_REST` directive and any positive route count; it never checks prefix
expansion or route/blob identity. Replacing the stored WPT section in memory with
only `WPT_REST:DECLARED = 1`, `resolved_route_count = 1`, and a fabricated digest
still returns the expected `WPT_REST`. Finding 3 above independently shows that
the A-06 “mutation” misses a coordinated occurrence deletion.

Reproduce the B-F04 weakness:

```sh
env PYTHONDONTWRITEBYTECODE=1 python3 -B -c '
import copy,pathlib,sys
r=pathlib.Path(".").resolve(); sys.path[:0]=[str(r/"tests"),str(r/"src")]
import mutations as m
d=copy.deepcopy(m.observations())
d["wpt"]["directive_counts"]={"WPT_REST:DECLARED":1}
d["wpt"]["resolved_route_count"]=1
d["wpt"]["resolved_route_blob_oid_sha256"]="0"*64
m.observations=lambda:d
print(m.wpt_rest_is_expanded())
'
```

It prints the expected `WPT_REST`.

**Required correction:** each A-01…A-10 and B-F01…B-F10 entry must construct or
load the exact counterexample, invoke the owning implementation/gate, and assert
the expected typed rejection or exact source-preserving result. Record actual
executed mutation IDs from the run rather than emitting the constant `20`.

**Effect:** the two replay suites did execute twenty named functions once per
artifact set, but this is not twenty-finding mutation coverage. The final receipt
retains byte-replay value only. Hostile clean count is zero; no product or
candidate effect is credited.

### 6. `[P2]` The genesis `clean checkout` receipt ignores untracked entries that can change the source-key multimap

**Location:** `src/capture_inputs.py:56-62`, `src/capture_inputs.py:102-121`, and
`src/capture_inputs.py:212-220`; the resulting assertion is
`inputs/manifest.json:2-10` and is repeated at `README.md:37-39`.

`require_checkout()` runs `git status --porcelain=v1 --untracked-files=no`.
Consequently an untracked CSSWG directory is explicitly invisible to the
cleanliness gate. Immediately afterward, source-key capture iterates every
worktree directory with `csswg_root.iterdir()` and reads any `Overview.bs`,
`Overview.html`, or `Overview.src.html` it finds. An untracked
`some-key/Overview.bs` can therefore add a key or collision to the retained
catalog while `csswg_clean: true` is stamped into the manifest. WPT tree capture
uses `ls-tree HEAD` and is not affected by the same directory path, but the
CSSWG collision catalog is.

This follows directly and reproducibly from the two quoted commands: the first
excludes untracked entries and the second consumes them. No repository history or
checkout inspection is necessary.

**Required correction:** reject tracked, untracked, ignored, staged, and submodule
worktree changes, or derive the catalog entirely from the pinned Git tree object
through the pinned Git binary. Retain the tree identity used for every catalog
entry.

**Effect:** the exact current catalog bytes and their object digest remain
replayable, and all 38 stored collisions remain useful observations. What fails is
the stronger clean-genesis provenance claim. This resets clean count but causes no
current candidate/product movement.

## Retained facts

The following facts survived independent reproduction. They are observations,
not CSS completeness or candidate evidence.

1. `data/file-manifest.json` hashes to
   `7e63647d0ec3378e205836a7ecc906150bff02b2d41bbcea8bdec26d5f9f3512`.
   Its 134-file, 46,776,370-byte subject reproduces at
   `12023cbd61f34e47e0aafde47cdc43611a6589f4b7a4ccd6dbb5e1adadaea9d5`.
   `src/manifest.py --verify-receipt` agrees with the stored final receipt under
   the declared manifest/verification self-reference exclusion.
2. The 109 input-manifest references name exactly 109 physical objects with no
   extras, totalling 43,013,202 bytes. Every object length and digest matches.
   The 96 selected CSSWG source objects total 11,676,246 bytes.
3. The WPT catalog decodes to 160,788 unique path/blob-OID entries and matches
   tree `12fa64c9afe86e199ee0e737755b6507f363ceff` and uncompressed SHA-256
   `cf3aedd08cbb161aeebd3c9e19c10f855f2be0be23fa5ecbbd2035d2e31db699`.
   Stored directive/path membership counts reproduce. Exact feature ownership is
   `UNMAPPED`, execution is `UNRUN`, outcomes are zero, and blob-content/test
   meaning remains intentionally blocked.
4. The inherited bundle contains 8,796 unique row IDs and 6,850 unique source IDs,
   with valid source indexes and no duplicate full rows. The one interned open
   plane vector honestly leaves all 8,796 BBNF, fixture, owner, WPT ownership,
   recovery, diagnostic, and serialization obligations open; WPT execution is
   `UNRUN` for every obligation.
5. The raw dependency pass reproduces 2,971 distinct occurrence IDs and one stored
   decision per occurrence. The 38-key multimap retains all six adjudicated live
   collision examples: `COMPOSITING`, `CSS-SHAPES`, `CSS-TEXT-4`, `CSS-VALUES`,
   `MEDIAQUERIES`, and `WEB-ANIMATIONS`; their twelve live normative occurrences
   are blocked rather than assigned a first winner. These occurrence decisions
   remain useful even though broader parsing-dependency closure is locally
   blocked.
6. The claim boundary remains conservatively spelled `VALID` byte replay,
   `BLOCKED` local semantics, `RED_OPEN` downstream, and `ABSENT_RED` candidate
   product. Lightning CSS and sonic-rs are `UNRUN`; JSON oracle, incumbent
   profiling/hot leaves, and four private placeholders remain explicit RED
   placeholders. No speedup, control, package, or production fact is present.
7. The cache-disabled seven-test unit suite passed. The fresh two-root replay
   regenerated both deterministic artifacts byte-identically, matched stored
   artifacts, and returned `PASS_BYTE_REPLAY_ONLY`. This retains replay evidence,
   not the semantic or mutation interpretations rejected above.
8. The representation is materially smaller than epoch 3: the two deterministic
   artifacts total 3,582,601 bytes, while retaining the 43,013,202-byte primary
   input store and the compressed atomic obligation bundle. The normalized data
   direction is appropriately small. The unused/fail-open candidate checker and
   assertion-only mutation layer are the principal removable mechanisms; the
   compactness claim itself is retained as a byte/resource observation only.

## Tranche, wave, feature, product, and candidate effect

| Plane | Critic result |
| --- | --- |
| Tranche fit | **RED; direction retained.** A sealed, read-only specification-input assay is appropriate VT.W0 work, and the normalized artifact is a much smaller route than epoch 3. The current semantic/candidate gates are not yet sound enough to freeze design inputs. |
| Wave fidelity | **RED/open.** Honest placeholders correctly disclose that JSON, incumbent profiles, four private compiling placeholders, and control freezes are absent. CSS source totality is still inaccurate because accepted carrier text and atomic source-unit mapping are unsound. VT.W0 cannot exit and the stored `VT.W1` terminal route at `src/pipeline.py:538` must not be interpreted as movement. |
| Feature truth | **RED with byte receipts retained.** Exact objects, dependency occurrence decisions, WPT tree paths/OIDs, obligation identities, deterministic artifacts, manifest, and receipt reproduce. CSS/WebIDL/Yacc/Lex parsing, exact occurrence census, candidate isolation, mutation coverage, and clean genesis do not. |
| Greater-product effect | **RED / no shipping edge.** No production/package edge or current runtime casualty exists. The fail-open crater would become a second, candidate-authored input authority if reused, while coarse mapped observations would feed wrong grammar/type facts downstream. Both must be corrected or deleted before a candidate boundary exists. |
| Candidate effect | **Zero credit.** No VT-1…VT-4 implementation is present in this assay, and neither candidate count nor complete-candidate count may increase. The hard-coded zero remains the conservative disposition, not an evidence-backed absence proof. |
| Clean/product state | **Clean count 0; movement NONE.** Any one of findings 1–6 prevents a clean hostile pass. Product state remains RED; no CSS, JSON, PT/BB, performance, package, candidate, transpose, or production authorization follows. |

## Required disposition

Bank the exact retained objects, current file/subject identities, WPT path/blob-OID
catalog, inherited atomic open-row bundle, collision-total occurrence decisions,
honest downstream/candidate zero-credit states, deterministic byte replay, and
normalization/resource improvement.

Reopen the local-semantic, source-census, candidate-boundary, mutation, and genesis-
provenance mechanisms. Repair in the order of findings 1 through 6: preserve
embedded dialect bytes; use consuming parsers; emit unit-atomic source occurrences;
replace the crater with an independently resolved input graph; turn every
adjudicated counterexample into a real external mutation; and derive genesis data
from a provably clean pinned tree. Then regenerate and reseal all artifacts and
receipts and restart hostile review. These bytes do not accumulate a clean pass.

**Terminal state: RED. Findings: 6. Clean count: 0. Candidate credit: 0.
Movement authorization: NONE.**
