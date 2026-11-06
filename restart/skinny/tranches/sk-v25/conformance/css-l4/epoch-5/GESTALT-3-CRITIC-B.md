MODEL DECLARATION: gpt-5.6-sol / xhigh

# CSS L4 W0 epoch 5 — Gestalt 3 critic B

Date: 2026-07-19  
Role: fresh independent hostile critic B  
Model / effort: `gpt-5.6-sol` / `xhigh`  
Delegation: none  
Files written: this report only

## Bound inputs and exclusion boundary

I began from the assumption that corrected epoch 5 was wrong. I did not read,
list, search, open, or infer from `GESTALT-3-CRITIC-A.md`, another epoch-5
critic report, root `GESTALT-*.md`, RUN/CAMPAIGN commentary, or a future
adjudication. I did not inspect Git history or another repository. The only
Gestalt input read was the expressly governing epoch-4 adjudication. No network
operation or subject, source, test, data, proof, manifest, receipt, product,
package, production, campaign, or sibling-repository mutation was performed.

The required identities matched before substantive review:

| Input | Required and observed SHA-256 |
| --- | --- |
| `LOCK-CONSTITUTION.md` | `b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5` |
| `waves/BB-VALUE-TAPE-P0.md` | `745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562` |
| epoch-4 `GESTALT-2-ADJUDICATION.md` | `728b5929543bc754b742be151761a8b5097d0274e37587b248ebd107836759fd` |
| epoch-5 `data/file-manifest.json` | `4f6d7157af13b99ac0218931dab9b659e8381ff492030790ca15899250fe5aef` |
| epoch-5 manifest subject | `5acbb3e5b2ef27794cc713cfeddf8e567d7d84ac3761c7aa613a21dbf9aa4d7d` |
| epoch-5 `data/verification.json` | `bb0b0b3cc85c069e1320db362149454f4386aa81fba4fdf389e5635e2201d75f` |
| epoch-5 `AUTHOR.md` | `ed3dcf7b4156ab99ef6d78b5874ea5b45aca127121608a14419183747417ab74` |

The ordinary manifest reproduces 27 files, 15,562,415 bytes, and the required
subject digest. This review file is in the declared root review namespace and is
not an ordinary subject byte.

## Terminal verdict

**GESTALT 3 CRITIC B: RED. Eleven findings: 7 P1, 4 P2. Consecutive-clean
count: 0.**

The epoch preserves useful exact bytes, an honestly RED terminal state, a real
four-root byte replay, and zero product credit. It does not establish a
failure-total source-native denominator, consuming dialect parsers, an independent
denominator oracle, per-occurrence unresolved WPT provenance, pre-execution
runtime closure, the named commit-to-tree relation, campaign candidate absence,
one-validator field authority, or exact final-receipt/resource closure. Byte
stability is not CSS semantics, WPT membership, or candidate credit.

Credited candidate count is **0**; complete-candidate count is **0**; candidate
credit is **NONE**. The assay does not prove campaign-wide physical absence, as
Finding 8 explains, but nothing in the sealed subject can be counted as a
candidate. There is no candidate casualty.

## Fresh replay and scoped counterexamples

The exact sealed launcher reproduced these narrow facts:

- final receipt verification: 27 files, 15,562,415 bytes, subject digest
  `5acbb3e5b2ef27794cc713cfeddf8e567d7d84ac3761c7aa613a21dbf9aa4d7d`;
- cache-disabled suite: 6/6 tests passed in 18.108 seconds;
- fresh four-empty-root proof: `PASS_TWO_COMPLETE_TWO_ROOT_REPLAY_PAIRS`, two
  pairs/four runs, baseline generated-artifact subject
  `2ff4987bca76b8e6491849b15af2fb26bd2fec0b4c7fdcbcfdcba7677e6aa1b0`;
- fresh proof bytes matched stored `proof/replay.json` exactly at
  `3e0567ebba5095dcff79882fd104b7c2adfd652468c108d117aea563a5af49df`;
- all four runs reproduced 2,565 stored rows and 23 registered executions.

Those are byte and runner facts. The following source-scoped, in-memory, or
disposable-root counterexamples passed without changing the sealed subject:

| Probe | Fresh result |
| --- | --- |
| CSS VDS `<<@>>` and `a,,b` | both accepted |
| valid CSS VDS `<length>#{1,2}` | rejected `CSS_POSTFIX_INVALID` |
| Yacc `a: () ;` and `a: foo ** ;` | both accepted |
| Lex rule with unmatched `(` | accepted |
| WebIDL `interface A { nonsense; };` | accepted |
| WebIDL member with mismatched `( ... >` | accepted |
| valid nonempty `enum Color { "red", "green" };` | rejected `WEBIDL_MEMBER_UNCLOSED` |
| forged controls/obligations/remainder/generation in the observation document | `validate_document()` accepted |
| receipt with schema 999, `SEALED_GREEN_CANDIDATE`, and an extra field | `validate_receipt()` accepted |
| canonical `prototypes/value-tape/vt-1/` present in a disposable root | `candidate_absence()` returned `ABSENT_RED`, count 0 |
| three different B-F10 payload mutation names | all produced the same hard-coded rejection |
| exact runtime startup probe | `no_site=0`, `site_loaded=true`, and an external source path already present in `sys.path` |

## Numbered findings

### 1. [P1] The 2,565-unit denominator is not failure-total or source-native atomic

**Evidence.** `src/source.py:289-298` recognizes algorithm steps only as closed
HTML `<li>` elements. Seven sealed `<ol class="algorithm">` carriers exist. Five
use Bikeshed numbered-line source rather than `<li>`: three in
`css-masking-1`, one in `css-transforms-1`, and one in `css-transforms-2`.
They contain 23 directly lexed numbered step lines, all of which receive no unit.
Only the two `filter-effects-1` lists use `<li>`, producing the stored four
`algorithm-step` rows. Thus the family count of 4 omits 23 step occurrences from
the sealed sources.

The excluded plane is also carrier-atomic rather than unit-atomic.
`src/source.py:275-277` emits one envelope row and does not parse an excluded
carrier. The 14 excluded rows include one `css-values-5` carrier holding five CSS
definitions, a `css-syntax-3` carrier holding two definitions, and excluded
railroads holding many instructions. Those productions/instructions do not each
receive the promised excluded disposition. The lack of stored detector failures
does not repair source families the detector never models.

**Reproduction.** Over all 96 retained source objects, scan closed
`<ol class=algorithm>` bodies for `(?m)^\s*\d+\.\s+` and count descendant closed
`li` tags with `scan_markup()`: five carriers report `23/0`, two report `0/4`;
compare with `data/denominator.json.family_counts.algorithm-step == 4`. Decode
the 14 `EXCLUDED_NON_NORMATIVE` fragments and count their contained definition
and railroad lines.

**Owner.** Mechanism 4: `src/source.py::scan_markup()`, `denominator()`, and the
atomic denominator schema.

### 2. [P1] Exact fragments coexist with markup corruption and four structurally unsound parsers

**Evidence.** `MARKUP_NAMES` in `src/parsers.py:20-25` omits Bikeshed markup
elements used inside accepted carriers. Fourteen accepted stored units contain
61 unremoved closing markup occurrences: 57 `</l>` and 4 `</nobr>`. Examples
include accepted color productions containing `<l>''srgb''</l>`. The CSS lexer
then treats `<l>` and `</l>` as CSS operands. Exact fragment digests therefore
prove the bytes of semantically polluted inputs, not CSS VDS semantics.

The consuming claims fail independently. `parse_css_vds()` validates only a
shallow operand/operator state and both accepts malformed references/punctuation
and rejects the valid `#` plus range multiplier form. `parse_yacc()` tokenizes an
RHS but does not validate grouping/postfix structure. `_lex_pattern()` checks
only doubled postfixes, two magic garbage strings, macro names, and square
brackets, so unmatched round groups pass. `parse_webidl()` accepts any
character-class-compatible semicolon member and mismatched delimiter kinds, while
its advertised `enum` head cannot consume an ordinary nonempty enum.

**Reproduction.** Run the parser probes in the replay table. Separately join each
accepted row's exact fragments and search unknown closing tags; the result is 14
affected units and 61 occurrences (`l:57`, `nobr:4`). All results come from the
sealed parser and source bytes.

**Owner.** Mechanism 3: `src/parsers.py::project_markup()`, `_css_tokens()`,
`parse_css_vds()`, `parse_yacc()`, `_lex_pattern()`, `parse_lex()`, and
`parse_webidl()`; `src/source.py::_projected_units()`.

### 3. [P1] `validate_document()` is not the one authority for control, remainder, obligation, or generation fields

**Evidence.** `src/assay.py:405-443` closes the seven claim statuses, resolves
evidence receipts, checks the denominator/WPT/candidate artifacts, and checks
zero credit. It never validates `controls`, `inherited_obligations`, or
`routed_remainder`; it permits arbitrary top-level additions. In
`src/generate.py:58-83`, document validation occurs before mutation results and
before the generation receipt exists, so the validator cannot own their counts,
status, artifact ledger, or credit fields. No later semantic validator closes the
generation receipt.

**Reproduction.** Load the stored document and four owning artifacts, replace
controls with a fabricated `ADVANCE` control, set inherited obligation count to
0/status `COMPLETE`, delete the routed remainder, and add generation status
`COMPLETE`/credit `ALL`. With all existing evidence bytes unchanged,
`validate_document()` returns normally (`FORGED_DOCUMENT_ACCEPTED`).

**Owner.** Mechanism 1: `src/assay.py::validate_document()` and
`src/generate.py` generation-receipt construction. The exact zero-credit check is
retained; the broader authority claim is rejected.

### 4. [P1] Runtime identity is checked after unsealed startup, site, `.pth`, bytecode, and module code can execute

**Evidence.** The required command lacks `-S`. An exact startup probe reports
`sys.flags.no_site == 0`, `site` already loaded, and `sys.path` containing both
`/opt/homebrew/lib/python3.14/site-packages` and the external
`/Users/mkbabb/Programming/fourier-analysis/src`. This occurs before
`launcher.py` executes. `-B` prevents bytecode writes but does not disable reads;
the runtime contains stdlib `__pycache__/*.pyc`, while the contract and
`common.py:190-203` deliberately exclude `site-packages`, `__pycache__`, `.pyc`,
and `.pyo` from the digest.

The launcher itself imports `hashlib`, `json`, `os`, and `pathlib` before the
binary/flag/environment checks, imports `common` before the full stdlib/dynamic
closure check, and closes only the first target word of `argv`; exact argv is not
retained in `runtime.json`. The current manifest verification still passes after
this startup path. The two recorded `-O`/DYLD mutations therefore establish only
that those two late checks reject, not that no startup/site/`.pth`/module code ran.

**Reproduction.** Under the exact allowlisted environment and flags, run
`python3 -I -B -X utf8 -X warn_default_encoding -c` and print `no_site`, whether
`site` is in `sys.modules`, and `sys.path`; observe `0`, `true`, and the external
path. Then run the sealed launcher receipt verification; it succeeds unchanged.

**Owner.** Runtime boundary: `src/launcher.py:6-48`,
`src/common.py::verify_runtime()`, and `inputs/contract.json.python`.

### 5. [P1] The WPT artifact loses every unresolved path occurrence and its provenance

**Evidence.** The block census itself is exact: 1,905 declared direct WPT blocks,
194 no-direct-test blocks, 6 excluded blocks, and one declared `wpt-rest`. The
direct blocks contain 29,051 declared path occurrences: 28,450 resolved and 601
unresolved. Resolved provenance is retained as 28,450 source-directive rows plus
273 `wpt-rest` expansion rows. Unresolved paths instead enter a Python `set` at
`src/assay.py:208,262`; output retains only 526 unique-name count and a digest of
the sorted set at lines 278-279. It retains zero unresolved route-name or source-
fragment rows, losing 75 repeated occurrences and all 601 occurrence-level
provenances.

The 16,147-row resolved union and its conservative counts (2 metadata, 43
reference, 127 support/resource, 15,975 unclassified) are useful prefix/blob
facts. The zero-test boundary is also honest: test count 0, execution `UNRUN`,
outcomes 0. They do not close the required direct-route occurrence union.

**Reproduction.** Sum `declared_path_count` and `resolved_path_count` over direct
declared blocks to get 29,051/28,450/601; compare 601 with
`unmapped_declared_path_count == 526`, then observe there is no unresolved row
class or route ledger. Sum all resolved `source_directives` to get 28,723 =
28,450 + 273.

**Owner.** Mechanism 5: `src/assay.py::wpt_prefix_blobs()` and
`data/wpt-prefix-blobs.json`.

### 6. [P1] The tree reconstructs, but the named commit-to-tree relation is only asserted

**Evidence.** `validate_tree_catalog()` correctly reconstructs 404 tree OIDs and
root tree `75bf19c016ed98126381508073de6893c9f756f5`; all 96 selected blobs are
checked against tree entries. However, the sole new gzip object decodes to only
`schema_version`, `commit`, `root_tree`, `truncated`, and `entries`. It contains
no raw Git commit object. `_tree_entries()` checks that its two strings equal the
same strings in the contract, while `validate_tree_catalog()` accepts only an
expected tree and never computes commit OID
`c7573530343759ace8e46438a1fa2c44515b5554` from a commit object containing the
tree header. `genesis()` copies the commit label into output.

The catalog is therefore a cryptographic proof of a tree object and selected
blob relation, but not a proof that the named Git commit points to that tree.

**Reproduction.** Gzip-decode the retained catalog and list top-level keys; there
are five and no commit-object bytes/header. Trace `assay.py:68-83,86-151`: no
function hashes a Git commit object or relates a computed commit OID to the
computed root tree.

**Owner.** Mechanism 2: `inputs/contract.json.csswg_tree`, the retained tree
catalog, `src/assay.py::_tree_entries()`, `validate_tree_catalog()`, and
`genesis()`.

### 7. [P1] The child denominator rederivation is process-isolated, not independently derived

**Evidence.** `src/denominator_probe.py` imports and calls the exact same
`source.denominator()` as the parent. `src/generate.py:42-49` compares its bytes
with the parent's result. The receipt itself correctly says
`FRESH_ISOLATED_CHILD_SAME_SEALED_EXTRACTOR_NOT_AN_ALGORITHMIC_ORACLE`. This can
detect coordinated ledger editing after extraction, but cannot detect an
extractor blind spot; it reproduced all 23 omitted algorithm step lines and the
non-atomic excluded carriers identically.

**Reproduction.** Trace both calls to `source.denominator()` and run the fresh
four-root proof: all four reproduce the same deficient unit digest
`868e551f37de3a095d884b41a33596c9edd8d2cfc0563674c20954c74cd6a2e0`.
No second detector, source-isomorphic occurrence ledger, or independent oracle is
invoked.

**Owner.** Mechanism 4: `src/generate.py`, `src/denominator_probe.py`, and the
denominator-totality gate.

### 8. [P2] Candidate absence checks invented local names, not the governing candidate topology

**Evidence.** The governing wave's exact candidate topology is
`prototypes/value-tape/vt-n/`. The epoch contract instead enumerates only
`candidate` and `candidates`, resolved beneath the epoch directory. Nothing binds
those names to the campaign topology, yet `candidate_absence()` emits
`enumeration_closed: true` and campaign-shaped candidate counts.

**Reproduction.** In a disposable root, create
`prototypes/value-tape/vt-1/` while leaving `candidate` and `candidates` absent.
`candidate_absence(root)` returns `ABSENT_RED`, candidate count 0, while the
canonical candidate directory exists.

**Owner.** Mechanism 6: `inputs/contract.json.candidate_roots` and
`src/assay.py::candidate_absence()` / `validate_candidate()`.

### 9. [P2] Manifest bytes and self-reference reproduce, but final-receipt schema/status do not close

**Evidence.** The ordinary manifest is exact and the manifest/verification
self-reference split is clear. `src/manifest.py::validate_receipt()` checks only
the subject-definition string and four manifest agreement fields. It neither
requires `schema_version == 1` nor the candid `SEALED_RED_ASSAY` status and does
not reject extra keys. Thus the advertised final exact-schema closure is absent
even though the stored receipt happens to be candid and hash-pinned for this
review.

**Reproduction.** Supply matching subject/manifest fields with schema 999,
status `SEALED_GREEN_CANDIDATE`, and an extra key. `validate_receipt()` returns
normally (`FORGED_RECEIPT_SCHEMA_AND_STATUS_ACCEPTED`).

**Owner.** Mechanism 8 sealing: `src/manifest.py::validate_receipt()` and
`data/verification.json`.

### 10. [P2] Twenty-three executions reproduce, but not all adversarial inputs own their mutations

**Evidence.** The registry identity/count checks, 23 typed results, and result
digest `ab4870b9eebd0db3e542c6b975efc6cb92b5f961663db9c43150cd94d368815a`
reproduce. But the B-F10 fixture payload says `forge_subject_digest` while
`tests/mutations.py:114-119` ignores the payload and constructs a fixed forged
receipt in code. Changing that external mutation string to `do_nothing` or any
unrelated value produces the same rejection. A-03 likewise uses a bespoke test
comparison rather than an integrated projection validator. The suite also has no
adversary for the actual accepted markup, parser shapes, validator fields, startup
site path, or unresolved WPT provenance above.

**Reproduction.** Call `execute()` on receipt-gate rows whose payload mutation is
respectively `forge_subject_digest`, `do_nothing`, and `unrelated_payload`; all
return `VERIFICATION_MANIFEST_DISAGREEMENT`. Run the stored suite to confirm that
its narrower 23 executions still pass.

**Owner.** Mechanism 7: `fixtures/counterexamples.json`,
`tests/mutations.py::execute()` / `run_all()`, and mutation-result claims.

### 11. [P2] The resource receipt contains summaries, not raw resource evidence or exact commands

**Evidence.** `proof/resources.json` has summary counters but no `command`, raw
capture, capture-tool identity, stdout, or stderr field anywhere. The author
record's statement that exact resource counters and commands are retained is
therefore false. For example, generation records 63,275,567,591 cycles over
17.59 seconds while replay records 922,180,518 over 67.47 seconds; without the raw
tool output, invocation boundary, counter availability, and capture method, these
numbers cannot be independently interpreted or attributed. They carry no
performance credit, but they are not the promised raw evidence.

**Reproduction.** Recursively list keys in `proof/resources.json`; `command`,
`raw`, `capture_tool`, `stdout`, and `stderr` are all absent. The fresh replay
confirms proof bytes, not these manually retained resource summaries.

**Owner.** Resource plane: `proof/resources.json`, the author resource claim, and
mechanism 8 proof/sealing surface.

## Eight author mechanisms — explicit retain/reject disposition

| # | Author mechanism | Disposition | Narrow fact retained / reason rejected |
| ---: | --- | --- | --- |
| 1 | Integrated claim/evidence authority | **REJECT** | Seven status enums, exact referenced bytes, local counts, candidate zeros, and every explicit credit `NONE` are retained. Controls, obligations, remainder, generation, receipt kinds/schema, and extra fields are fail-open (Finding 3). |
| 2 | Complete pinned-tree genesis | **REJECT** | The 3,608-row tree, 404 subtree OIDs, root tree, and 96 selected blob relations reproduce. The named commit-to-tree edge is unproved (Finding 6). |
| 3 | Source-preserving projection and consuming dialects | **REJECT** | Stored fragments reconstruct exact bytes. Fourteen accepted units still contain markup, and CSS/Yacc/Lex/WebIDL acceptance is structurally unsound (Finding 2). |
| 4 | Failure-total atomic denominator and child rederivation | **REJECT** | The 2,565 stored rows, totals, digest, and reverse-link recomputation reproduce as representation facts. Source steps/excluded units are missing and the child uses the same extractor (Findings 1 and 7). |
| 5 | WPT direct/prefix/blob relation and classification | **REJECT** | Exact block counts, 16,147 resolved path/blob rows, resolved provenance, conservative classes, and zero-test boundary are retained. All 601 unresolved occurrences lack route/provenance rows (Finding 5). |
| 6 | Closed candidate absence | **REJECT** | The two epoch-local names do lstat absent and credit remains none. They are not the governing candidate roots (Finding 8). |
| 7 | External executable mutation closure | **REJECT** | 23 executions and their digest reproduce. At least B-F10's supposed external mutation is ignored, and decisive acceptance classes remain uncovered (Finding 10). |
| 8 | Four-root replay and final seal | **REJECT as a closure mechanism** | Four distinct empty roots regenerate all eight artifacts byte-identically, and ordinary manifest/self-reference bytes reproduce. Runtime pre-execution closure, final receipt schema, and raw resources fail (Findings 4, 9, 11). Retain only byte replay. |

## Tranche, wave, feature, KISS, and greater-product disposition

| Plane | Disposition |
| --- | --- |
| Total tranche fit | **RED; assay direction retained.** A read-only CSS input assay is a legitimate small W0 experiment. This epoch cannot become a frozen design input because its occurrence universe, semantic consumers, provenance, and runtime authority are open. |
| VT.W0 wave fidelity | **RED/open; no exit.** The wave requires an exhaustive CSS/normative/WPT union, JSON oracle, profiles/hot leaves, controls, and four placeholders. CSS source/WPT occurrence totality is not closed, and all broader remainder stays RED/UNRUN/ABSENT. |
| Feature truth | **RED.** Exact immutable object bytes, resolved tree/blob rows, stored ledgers, zero-credit labels, and byte replay survive. Each of the eight owning mechanisms has a load-bearing defect as itemized above. |
| KISS/friction | **RED.** The 15.56 MB subject (including a 13.69 MB WPT artifact), same-extractor child, eight generated artifacts, mutation registry, proof registry, manifest, and resource summary create substantial assay machinery without an independent semantic census. A simpler source-isomorphic per-occurrence ledger plus one consuming parser boundary, one independent census oracle, and raw command receipts would prove more with less authority duplication. |
| Greater-product fit | **RED / no shipping edge.** No package or production consumer exists. Reuse would export accepted markup as CSS syntax, a deficient denominator, an unproved commit label, and fail-open observation authority, creating sand/second authority rather than a grammar-derived product invariant. Those edges are prohibited. |

## Exact terminal counts, credit, and movement

- Consecutive-clean count: **0**. This is not clean pass 1.
- Stored source ledger: **2,565** rows and **3,198** reverse links; retained only
  as stored representation facts, not the exhaustive denominator.
- Stored dispositions: **2,218** `PARSED_WITH_REVERSE_LINKS_RED`, **269**
  `BLOCKED_NO_REVERSE_LINK`, **64** `BLOCKED_TYPED_UNSUPPORTED`, **14**
  `EXCLUDED_NON_NORMATIVE`.
- Resolved WPT relation: **16,147** unique path/blob rows; WPT test count **0**;
  execution **UNRUN**; actual outcomes **0**. No WPT test or outcome credit.
- Credited candidates: **0**; complete candidates: **0**; candidate credit:
  **NONE**; candidate casualty: **NONE**.
- Every explicit credit: CSS **NONE**; CSS L4 **NONE**; Value Tape **NONE**;
  PT/BB lock **NONE**; candidate **NONE**; product **NONE**; production **NONE**.
- Remainder: JSON oracle **RED_UNRUN**; incumbent profiles/hot leaves
  **RED_UNRUN**; four private compiling placeholders **RED_ABSENT**; Lightning
  CSS control **UNRUN**; sonic-rs control **UNRUN**.
- Movement: no CSS, CSS-L4, JSON, VT.W0, tranche, PT/BB lock, control,
  benchmark, profile, WPT test/outcome, candidate ranking, BANK/RETIRE/ADVANCE,
  package, product, production, hostile-clean-pass, or campaign movement.

**Final disposition: RED; 11 findings (7 P1, 4 P2); clean count 0; 0 credited
candidates; 0 complete candidates; all credit NONE; all movement NONE.**
