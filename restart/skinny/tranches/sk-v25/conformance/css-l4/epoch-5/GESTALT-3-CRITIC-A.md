MODEL DECLARATION: gpt-5.6-sol / xhigh

# CSS L4 W0 epoch 5 — Gestalt 3 critic A

Date: 2026-07-19  
Role: fresh hostile critic A  
Model / effort: `gpt-5.6-sol` / `xhigh`  
Delegation: none

## Bound review surface

I assumed the corrected assay was wrong. I reviewed only the corrected epoch-5
subject and the three named governing inputs. I did not read or create any other
critic or later adjudication report. I made no source, test, data, proof, manifest,
receipt, product, package, campaign-ledger, or sibling-repository change. Replay
outputs were confined to `/tmp`; this report is the sole epoch-5 addition and is
inside the manifest's declared append-only `GESTALT-*.md` exclusion.

The required identities reproduce:

| Input | Required and observed SHA-256 |
| --- | --- |
| `LOCK-CONSTITUTION.md` | `b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5` |
| `waves/BB-VALUE-TAPE-P0.md` | `745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562` |
| epoch-4 `GESTALT-2-ADJUDICATION.md` | `728b5929543bc754b742be151761a8b5097d0274e37587b248ebd107836759fd` |
| epoch-5 `data/file-manifest.json` | `4f6d7157af13b99ac0218931dab9b659e8381ff492030790ca15899250fe5aef` |
| epoch-5 declared subject | `5acbb3e5b2ef27794cc713cfeddf8e567d7d84ac3761c7aa613a21dbf9aa4d7d` |
| epoch-5 `data/verification.json` | `bb0b0b3cc85c069e1320db362149454f4386aa81fba4fdf389e5635e2201d75f` |
| epoch-5 `AUTHOR.md` | `ed3dcf7b4156ab99ef6d78b5874ea5b45aca127121608a14419183747417ab74` |

The current final manifest contains 27 ordinary files totaling 15,562,415 bytes.
Its subject construction reproduces the required subject digest. The exact
current final receipt agrees with those four structural fields.

## Terminal disposition

**GESTALT 3 CRITIC A: RED — 9 findings (6 P1, 3 P2). Consecutive-clean count 0.**

The exact byte replay, selected root-tree reconstruction, retained source-fragment
digests, closed two-root candidate absence, zero WPT-test/outcome state, and four
empty-output-root byte equality are useful narrow facts. The claimed consuming
parsers, failure-total denominator, integrated claim authority, commit association,
and closed final receipt are not established. This is still an `INCOMPLETE_ASSAY`,
not a CSS implementation or a Value Tape candidate.

Candidate count is **0**; complete-candidate count is **0**; candidate/product/
CSS/CSS-L4/VT/PT/BB/control/profile/performance/package/production/campaign credit
is **NONE**. No candidate casualty exists. Movement is **NONE**. VT.W0 remains
born RED and may not exit.

## Findings

### 1. The 2,565-row ledger is not a failure-total source denominator

- **Severity:** P1
- **Evidence:** Epoch 4 retained 4,954 carrier/exclusion occurrences. Comparing
  those source intervals with every epoch-5 unit fragment gives: old `algorithm`
  78 / only 2 overlapped; `named-algorithm` 130 / 0; `idl` 137 / 2;
  `normative-dfn-value-addition` 691 / 15; `propdef` 660 / 659; `railroad` 49 /
  48; and `explicit-pre-exclusion` 2,763 / 0. Epoch 5 emits only four algorithm
  steps, no WebIDL declaration/member family, and zero detector-failure rows.
  `source.py:160-171` recognizes a small class whitelist; `source.py:266-271`
  silently skips every closed tag for which `_kind()` returns `None`;
  `source.py:267-268` silently skips unclosed tags; and `source.py:289-298`
  recognizes algorithm units only as `li` descendants of a recognized algorithm
  ancestor. Thus 2,565 is an exact output length for this detector, not a complete
  source-native failure total. Exact fragments and digests remain valid only for
  rows the detector chose to emit.
- **Reproduction:** Load epoch-4
  `data/observations.json::source_semantics.census` and epoch-5
  `data/denominator.json::units`; group the old rows by family and count an old
  `[path,start,end)` as represented only when it overlaps at least one new fragment
  on the same path. `jq '.source_semantics|{occurrence_count,family_counts}'` and
  `jq '{unit_count,family_counts,disposition_counts}'` reproduce the two ledgers;
  the interval join reproduces the counts above.
- **Owner:** `src/source.py`, `data/denominator.json`, and the failure-total claim
  in `README.md:37-43` / `AUTHOR.md:50-56,78-95`.

### 2. Same-extractor child replay cannot establish source totality

- **Severity:** P1
- **Evidence:** `src/denominator_probe.py:9-19` imports the same `source.denominator`
  used by the parent. `src/generate.py:42-49` compares its output with that child,
  while `src/assay.py:365-367` merely checks byte equality if the optional
  `_independent_denominator` value is present. The unit test is weaker still:
  `tests/test_assay.py:19-21` invokes the same function again in the same process.
  A coordinated detector omission therefore reproduces perfectly; finding 1 is a
  concrete example. A-10 proves only that an after-the-fact deletion differs from
  another run of the same implementation.
- **Reproduction:** `rg -n 'denominator\(|_independent_denominator' src tests`
  shows the single implementation on both sides. Remove no data: observe that all
  four proof runs retain the same omitted family coverage while reporting the same
  unit digest `868e551f...6a2e0`.
- **Owner:** `src/denominator_probe.py`, `src/generate.py`,
  `src/assay.py::validate_denominator()`, and `tests/test_assay.py`.

### 3. All four advertised consuming dialect parsers still overaccept malformed input

- **Severity:** P1
- **Evidence:** CSS punctuation `,/:;` is unconditionally tokenized as an operand
  (`parsers.py:186-188`); Yacc validates a token whitelist but not `%prec` syntax
  (`242-305`); Lex checks square brackets and a few strings but not ordinary group
  balance (`316-340`); WebIDL accepts any semicolon-delimited member containing a
  permissive character set (`418-489`). Fresh calls accepted all four malformed
  inputs:

  ```text
  css-vds  ";"                                      -> ACCEPTED
  yacc     "a: %prec ;"                             -> ACCEPTED
  lex      "%option case-insensitive\na a\n%%\n( return TOKEN\n" -> ACCEPTED
  webidl   "interface A { hello; };"                -> ACCEPTED
  ```

  These are recognizers, not consuming grammars for even a coherently defined
  supported subset. The 23-input registry covers one selected error in each area
  but misses these acceptance classes. Therefore `PARSED_WITH_REVERSE_LINKS_RED`
  cannot mean syntax was consumed. The stored `local_semantics = BLOCKED` boundary
  is retained and must not be laundered into CSS meaning.
- **Reproduction:** With bytecode disabled, import `parse_css_vds`, `parse_yacc`,
  `parse_lex`, and `parse_webidl` from `src/parsers.py` and call them on the four
  strings above; each returns AST-shaped unit rows instead of raising
  `UnsupportedDialect`.
- **Owner:** `src/parsers.py`, parser mutations in `fixtures/counterexamples.json`,
  and the consuming-parser claim in `README.md:33-36` / `AUTHOR.md:46-49`.

### 4. The integrated validator accepts fabricated control, movement, and obligation state

- **Severity:** P1
- **Evidence:** `validate_document()` validates the seven `claims` enums, evidence
  referents, selected artifact counts, and zero credit (`assay.py:405-443`), but it
  never validates `controls`, `routed_remainder`, or `inherited_obligations`.
  Starting from the stored document, I changed Lightning CSS correctness to
  `VALID`, selection to `SELECTED`, `VT.W0.JSON_ORACLE` to `ADVANCE`, and inherited
  obligations to `CLOSED`; `validate_document()` returned successfully. It also
  never validates `generation-receipt.json`, which is created after the document.
  Separately, `common.py:126-145` hash-checks epoch-4 `governing.json` but does not
  resolve the actual governing document paths/digests named inside it. The current
  governing files do match because this review checked them externally; the assay
  gate does not establish that fact.
- **Reproduction:** Load the stored observation and five evidence artifacts, apply
  the four field changes above, preserve all evidence bytes, then call
  `assay.validate_document(document,resolved,artifacts,inputs)`. The observed result
  was `ACCEPTED_FORGED_CONTROL_REMAINDER_AND_OBLIGATION_STATE` with exit 0.
- **Owner:** `src/assay.py::validate_document()`, `src/common.py::load_contract()`,
  and the integrated-authority claim in `README.md:24-26` / `AUTHOR.md:39-43`.

### 5. The catalog proves a tree root, not that the named commit owns that root

- **Severity:** P1
- **Evidence:** The compressed catalog contains only `commit`, `entries`,
  `root_tree`, `schema_version`, and `truncated`. `validate_tree_catalog()`
  correctly reconstructs every subtree and root OID from the entries
  (`assay.py:86-124`), and `genesis()` correctly recomputes the 96 selected blob
  OIDs against that tree (`127-151`). But the commit and root are co-declared in
  the same captured JSON/contract. No Git commit object is retained, parsed, or
  rehashed, and no independent commit-to-tree relation is checked. The mechanism
  therefore establishes exact membership in tree
  `75bf19c016ed98126381508073de6893c9f756f5`, but not that commit
  `c7573530343759ace8e46438a1fa2c44515b5554` points to it. Worktree exclusion is
  valid after that narrower root is accepted.
- **Reproduction:** `gzip -dc inputs/objects/sha256/58/875994... | jq 'keys'`
  shows the absent commit object; `rg -n 'commit|validate_tree_catalog' src/assay.py`
  shows that validation receives only `expected_root` and never recomputes the
  commit OID.
- **Owner:** `inputs/contract.json::csswg_tree`, the compressed tree catalog,
  `src/assay.py::validate_tree_catalog()`, and `README.md:27-32` /
  `AUTHOR.md:28-35`.

### 6. The WPT union is reproducible, but direct-route closure and failure provenance are overstated

- **Severity:** P2
- **Evidence:** The 1,905 `DECLARED` direct blocks contain 29,051 declared path
  occurrences, of which only 28,450 resolve. The ledger retains 526 unique
  unmapped route strings, meaning 601 unresolved occurrences were collapsed into
  a set and one digest (`assay.py:208,254-263,278-279`); those failures have no
  per-occurrence spec/path/fragments/digest rows. The exact matched union is 16,103
  direct unique routes plus 273 `wpt-rest` routes, with 229 overlapping and 44
  rest-only, yielding 16,147 unique rows. Thus the 16,147 relation and all matched
  row OIDs/provenance reproduce, but the prose “all 1,905 ... resolve” is not
  occurrence-total. Classification remains explicitly path-only and conservative;
  WPT-test count 0, execution `UNRUN`, and outcomes 0 are correct.
- **Reproduction:** Sum `.declared_path_count` and `.resolved_path_count` over
  `directives[]|select(.kind=="WPT" and .status=="DECLARED")`; group each row's
  `source_directives[][4]` into direct/rest membership. The totals above reproduce.
- **Owner:** `src/assay.py::wpt_prefix_blobs()`,
  `data/wpt-prefix-blobs.json`, and `README.md:44-49` / `AUTHOR.md:57-63`.

### 7. Unsealed site startup executes before the claimed runtime closure and injects an external repository

- **Severity:** P1
- **Evidence:** The exact README command starts with `sys.flags.no_site == 0`,
  `site` and `sitecustomize` already present in `sys.modules`, and this `sys.path`:
  the stdlib zip, stdlib, `lib-dynload`,
  `/opt/homebrew/lib/python3.14/site-packages`, then
  `/Users/mkbabb/Programming/fourier-analysis/src`. The last path is injected by
  unsealed `/opt/homebrew/lib/python3.14/site-packages/_fourier_analysis.pth`
  (SHA-256 `ce412c36...77ea3`). `common.py:190-193` deliberately excludes
  `site-packages`, so neither that `.pth` byte string nor the injected external
  tree participates in the 2,495-file digest. Python `.pth` processing can execute
  import lines before the script; the current file happens to contain only the
  external path, but the gate would not detect a mutation before it executed.
  After that startup, the launcher adds epoch-5 `src` to `sys.path`, imports subject
  file `common.py`, and only then calls `verify_runtime()` (`launcher.py:45-48`).
  `common.py` has also already imported and executed multiple stdlib modules before
  hashing them. Therefore the claimed closed, pre-import runtime is false. On the
  current host, the exact binary, flags, five-variable environment, 2,495-file
  selected stdlib digest, seven absolute non-system dynamic nodes, and absence of
  current `@rpath`/`@loader_path` extension edges reproduce; those are post-startup
  observations over a selection that omits a live startup input.
- **Reproduction:** Run the README `env -i ... python3 -I -B -X utf8
  -X warn_default_encoding` with `-c` printing `sys.flags.no_site`, membership of
  `site`/`sitecustomize` in `sys.modules`, and `sys.path`; the values above
  reproduce. `find /opt/homebrew/lib/python3.14/site-packages -name '*.pth'` and
  `sha256sum` identify the injected file. `nl -ba src/launcher.py | sed -n
  '22,50p'` and `nl -ba src/common.py | sed -n '1,16p;149,249p'` show the later,
  excluding verifier. The frozen replay still passes, demonstrating the omission.
- **Owner:** `src/launcher.py`, `src/common.py`, and the pre-import claim in
  `README.md:72-76` / `AUTHOR.md:119-128`.

### 8. The excluded final receipt has a fail-open semantic verifier

- **Severity:** P2
- **Evidence:** Excluding the manifest self-reference, final receipt, and root
  `GESTALT-*.md` reviews is explicit and the current manifest is structurally
  complete for ordinary files (`manifest.py:20-54`). But
  `validate_receipt()` checks only `subject_definition`, manifest digest, subject
  digest, file count, and total bytes (`67-72`). It ignores schema version, status,
  exact key set, and arbitrary added credit. A copy with `schema_version = 999`,
  `status = PRODUCT_VALID`, and `unauthorized_credit = ADVANCE` passed. Because the
  receipt is excluded from the subject, `manifest --verify-receipt` is not itself
  a closed verifier of the receipt's semantics. The externally required SHA-256 in
  this review does authenticate the current honest receipt; that external fact is
  retained.
- **Reproduction:** Load `data/verification.json`, alter the three fields above,
  and call `manifest.validate_receipt(mutated, manifest.verify(),
  sha256(MANIFEST.read_bytes()))`; the observed result was
  `ACCEPTED_FORGED_RECEIPT_SCHEMA_STATUS_AND_EXTRA_CREDIT` with exit 0.
- **Owner:** `src/manifest.py::validate_receipt()` and mechanism 8's final-receipt
  claim.

### 9. Resource “commands and exact counters” are sealed prose, not validated evidence

- **Severity:** P2
- **Evidence:** `proof/resources.json` contains summary numbers but no command
  strings, raw command output, measurement-tool identity, sample rows, or receipt
  linking those numbers to an execution. No source or test generates or validates
  this file; the ordinary manifest only hashes its bytes. This contradicts
  `AUTHOR.md:136-137`, which says exact resource counters and commands are retained.
  The reported 17.59-second generation / 67.47-second proof and RSS summaries are
  therefore narrative observations. They earn no profile, benchmark, performance,
  or control credit, consistent with the stored terminal boundary.
- **Reproduction:** `jq 'keys, .generation|keys, .replay|keys' proof/resources.json`
  shows summaries only; `rg -n 'resources.json' src tests` finds no producer or
  validator.
- **Owner:** `proof/resources.json`, its absent producer/validator, and
  `AUTHOR.md:130-137`.

## Eight author-mechanism dispositions

| # | Author mechanism | Critic-A disposition |
| ---: | --- | --- |
| 1 | Integrated claim/evidence validator | **REJECT.** Exact referent and selected count checks survive; controls, routed remainder, inherited obligations, governing referents, and generation receipt remain fail-open. |
| 2 | Pinned-tree genesis | **REJECT as stated.** **Retain** exact 3,608-entry root-tree reconstruction and 96 selected blob memberships; the named commit-to-tree edge is unproved. |
| 3 | Source-preserving consuming parsers | **REJECT.** **Retain** the projection/span mechanism for emitted units; all four dialect parsers have direct overaccept counterexamples. |
| 4 | Failure-total atomic denominator and child rederivation | **REJECT.** **Retain** exact representation of the 2,565 emitted rows and deterministic same-extractor digest only. |
| 5 | WPT direct/prefix blob relation and conservative classification | **RETAIN NARROWLY.** The 16,147 matched path/blob union and zero-test boundary reproduce; unresolved occurrence provenance and prose route closure do not. |
| 6 | Two-root candidate absence | **RETAIN NARROWLY.** Exactly `candidate` and `candidates` are absent, no future inspector exists, counts are zero, and no candidate/product inference is allowed. |
| 7 | Twenty-three external counterexample executions | **RETAIN AS EXECUTION ACCOUNTING, REJECT AS CLOSURE.** All 23 expected rejections reproduce, but findings 3 and 4 supply missing hostile inputs that the registry accepts. |
| 8 | Four-root replay and final subject seal | **RETAIN four-root byte equality and the externally pinned current seal; REJECT a closed local final-receipt verifier.** |

## Independent replay and retained facts

The following safe scoped replays passed on the frozen host:

| Check | Result |
| --- | --- |
| `src/launcher.py manifest --verify-receipt` | PASS; 27 files, 15,562,415 bytes, subject `5acbb3e5...a4d7d` |
| `src/launcher.py test` | PASS; 6/6 tests in 17.224 s |
| fresh `src/launcher.py proof` in `/tmp` | `PASS_TWO_COMPLETE_TWO_ROOT_REPLAY_PAIRS`; four runs, baseline `2ff4987b...aa1b0`, no stored or pair mismatch |
| immutable epoch-4 object store | 109 logical/physical digest identities remain closed through the replay |
| candidate roots | both declared roots absent; 0 candidates, 0 complete candidates |
| WPT claim boundary | prefix/path/blob relation only; test membership blocked, execution `UNRUN`, outcomes 0 |

These checks establish deterministic execution against the current exact bytes.
They do not establish CSS semantics, executed WPT, candidate completeness, any
PT/BB lock, a profile, a control ratio, or product credit.

## Tranche, wave, KISS, and greater-product judgment

| Plane | Judgment |
| --- | --- |
| Tranche fit | **RED; direction retained.** A read-only CSS specification-input assay is appropriate VT.W0 census work, but this correction did not complete the authorized semantic and claim-boundary repairs. |
| Wave fidelity | **RED/open.** The wave requires an exhaustive atomic CSS/WPT union before design freeze. Whole old carrier families fall outside the denominator, WPT failures are occurrence-incomplete, and all broader JSON/profile/control/four-placeholder work remains explicitly RED or absent. |
| Feature truth | **RED with narrow bytes retained.** Source-fragment digests, root-tree membership, matched WPT rows, zero candidates, and deterministic output are real. Consuming syntax, denominator totality, integrated authority, commit association, and closed receipt semantics are not. |
| KISS / friction | **RED.** The assay uses 2,175 lines of Python, a 13.7 MB generated WPT ledger, roughly 15.3 MB per generated root, about 400 MB reported generation RSS, and repeated same-extractor processes. The costly repetitions prove byte stability while the omitted independent oracle and permissive parsers leave the load-bearing fact unproved. The parser/detector stack must not become a second CSS authority. |
| Greater-product fit | **RED / no shipping edge.** No package, production consumer, candidate, or candidate graph exists. Feeding `PARSED_WITH_REVERSE_LINKS_RED`, forged control/remainder state, or the unproved commit label downstream would create a second authority and corrupt later type/coverage decisions. Reuse is prohibited until repair and fresh review. |

## Required repair before another hostile pass

1. Anchor a source-native occurrence universe independently of the extractor and
   account for every prior carrier/exclusion family; every disappearance needs an
   exact typed supersession, not silence.
2. Replace the four structural recognizers with consuming grammars for an explicit
   supported subset, and add the four acceptance counterexamples above through the
   real integrated mutation gate.
3. Make one validator own the complete observation/control/remainder/obligation/
   receipt schemas and resolve the actual governing document bytes.
4. Retain and verify the CSSWG commit object (or narrow every claim to the already
   reconstructed root tree), and retain occurrence-level provenance for each
   unresolved WPT declaration.
5. Move runtime verification behind a minimal independently trusted launcher or
   narrow the receipt to a post-import self-check; make the final excluded receipt
   exact-schema closed.
6. Regenerate in empty roots, reseal ordinary bytes and the final receipt, and
   begin a fresh hostile pass. No product, candidate, package, control, profile,
   performance, or campaign work is authorized by this report.

**Exact critic-A count: 9 findings — 6 P1, 3 P2. Clean state: RED;
consecutive-clean count 0. Candidate state: 0 candidates, 0 complete candidates,
credit NONE. Movement state: NONE.**
