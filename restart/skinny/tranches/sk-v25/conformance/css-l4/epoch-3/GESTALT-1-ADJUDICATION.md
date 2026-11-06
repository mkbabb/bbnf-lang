# CSS L4 W0 epoch 3 — Gestalt 1 adjudication

Date: 2026-07-19  
Role: fresh third-agent adjudicator  
Task: `/root/css_w0_epoch3_g1_adjudicator`  
Model / effort: `gpt-5.6-sol` / `xhigh`  
Turn: first adjudicator turn spawned from `/root`  
Session: shared Codex workspace session rooted at
`/Users/mkbabb/Programming/bbnf-lang`; no separate opaque session identifier is
exposed to this agent  
Delegation: none  
Files written: this adjudication only

## Review boundary

Before inspecting the repository I reported the model, effort, task, turn, and
available session metadata. I read the lock constitution and governing wave in
full; the epoch-3 implementation, tests, fixture, manifests, receipts, generated
artifacts, retained input objects, and input JSON; and exactly
`GESTALT-1-CRITIC-A.md` and `GESTALT-1-CRITIC-B.md`. I did not read AUTHOR, a
RUN/CAMPAIGN/CHALLENGE/WALLS ledger, an earlier adjudication, or another review.
No network operation, fetch, retry, pin change, source repair, artifact rewrite,
or production edit was performed.

Every generated JSON document was decoded with duplicate-key rejection and
recursively traversed. All 105 retained objects were read, length-checked, and
SHA-256-checked against the 106 logical authority records. Decisive critic
counterexamples were rerun independently against the stored artifacts, declared
CSSWG checkout, and implementation functions. The current checkouts were used as
observation inputs, not credited as epoch-owned clean-root inputs.

## Terminal verdict

**GESTALT 1: RED. Consecutive-clean count: 0.**

All ten Critic A findings and all ten Critic B findings are **CORRECT** at their
adjudicated scope. B-F06 requires one boundary clarification: an input census is
allowed to emit downstream RED rows, so `UNMAPPED` and `UNPROVEN` values need not
be suppressed. The correct defect is that the only prerequisite gate is an empty
`root_blockers` list even when locally detectable source/dialect/inventory losses
are absent from that list, while `author_gate: PASS` and
`input_epoch_reproducible: true` can survive. Those labels prove byte replay, not
semantic cleanliness.

This epoch is an **input assay**, not a VT candidate. Candidate count is **0** and
complete-candidate count is **0**. No implementation, CSS-coverage, CSS-L4,
VT.W0, PT/BB lock, WPT-outcome, control, performance, package, product, hostile-
pass, or production credit is awarded.

No production, package, source, downstream, tranche, wave, or campaign movement
is authorized. Correct findings reopen the CSS W0 design cluster. A repaired
successor must begin again at hostile pass 1; these bytes cannot accumulate a
clean count.

## Retained machine facts

These facts survive as observations only:

- `inputs/authorities.json` contains 106 logical records referring to 105 unique
  objects. All 67,196,482 retained object bytes match their recorded lengths and
  content-addressed names; the manifest/object digest sets are equal.
- The six checked artifact byte identities reproduce as stored:

  | Artifact | Bytes | SHA-256 |
  | --- | ---: | --- |
  | `scope.json` | 12,058,423 | `6b9cceb0a9016dad077023f9be34563b28782f8aca3afd121283cd348aa466fe` |
  | `carriers.json` | 11,986,795 | `35b4289e9346dacb2d16073468717d89337148173f27fa4f10003202f1aa12a9` |
  | `dialects.json` | 11,720,876 | `da330bc3b018357b040c4c2b0e204f1c613ec76670f51088c20887851b59288b` |
  | `modules.json` | 15,939,592 | `028724633ad66fce242e5ae8038655d1aa15bff46bd14f66083c5ad46c189544` |
  | `matrix.json` | 11,481,625 | `7c0572371be7a54a9bbef65ba3858a175e55c6595faa19f88963d7dbec82a60a` |
  | `boundary.json` | 1,166 | `18306f408ea7a0de9d8b1248af32f127316d834236b53f35901f3580b1d26c77` |
- The current file manifest verifies 141 non-review files, 130,631,391 bytes, and
  subject digest
  `b0e115d879ca6b9a14c7c59e73653e9891410d58e8d1532e7b1ec835e3848597`.
  Root `GESTALT-*.md` files are excluded by its existing construction.
- The declared CSSWG checkout was clean at
  `c7573530343759ace8e46438a1fa2c44515b5554`; the declared WPT checkout was clean
  at `37cd7ff74eb974fd41600bf7bc01d37576abf8b5`, tree
  `12fa64c9afe86e199ee0e737755b6507f363ceff`. The executing CPython was 3.14.5 at
  the required binary digest. These are prepared-host facts, not clean-root input
  reconstruction.
- All 8,796 stored matrix rows remain `OBLIGATION`. BBNF, fixtures, and owner are
  `UNMAPPED`; responsible modules are null; WPT has zero actual outcomes;
  recovery, diagnostics, and serialization are `UNPROVEN`; both external controls
  remain correctness `UNPROVEN`, selection `UNRUN`, and performance `UNRUN`; no
  candidate exists.

These byte and status facts are retained. The interpretation that they prove a
complete semantic inventory, strict parser, exact ownership relation, sealed
clean-root replay, or candidate boundary is rejected.

## Finding-by-finding adjudication

### Critic A

| Finding | Decision | Reproduced basis |
| --- | --- | --- |
| A-01 | **CORRECT** | Mutating every compact BBNF expectation to `MAPPED`, every WPT owner to `PASS`, behavior planes to `OBSERVED`, fixtures to `MAPPED`, and owners to `ASSIGNED` with nonexistent `fabricated.owner`, then rederiving terminal labels, was accepted by `matrix.validate(..., modules, csswg)`: `8796 EVIDENCED OBSERVATION_COMPLETE fabricated.owner`. No evidence object was added. `WPT FAIL` is also wrongly listed as evidenced. Current stored rows themselves remain honestly open. |
| A-02 | **CORRECT** | Stored dependency counts are zero for `css3-mediaqueries` and `css-color-3`, and one informative edge for `css-fonts-3`, although retained rendered HTML contains live normative bibliography/grammar edges; the mediaqueries source explicitly points to CSS2 grammar with rendered `[CSS21]<!--{{!CSS21}}-->`. An independent source-key multimap found 38 collisions and six collision keys used by current normative occurrences: `COMPOSITING`, `CSS-SHAPES`, `CSS-TEXT-4`, `CSS-VALUES`, `MEDIAQUERIES`, and `WEB-ANIMATIONS`. `dict.setdefault` silently chooses one source. |
| A-03 | **CORRECT** | The CSS parser accepted `a )`, `a ]`, and bare `]`; Yacc accepted `a: ??? ;`, `a: %bogus b ;`, `a: b | | c ;`, and an unbalanced bracket RHS; Lex accepted unterminated classes and bogus options/patterns; WebIDL accepted leading/trailing garbage. These recognizers do not justify the advertised strict-language gate. |
| A-04 | **CORRECT** | Of 49 stored railroad units, 34 have a tree envelope ending before a descendant; maximum omitted suffix is 349 characters. `css-images-4::railroad::1` reports tree end 36 while a child ends at 151; its matrix source covers 32 bytes for a 154-character carrier. The stored prefix digest is exact, but the semantic-tree span is not. |
| A-05 | **CORRECT** | The `(spec_id, anchor)` grouping yields 10 `UNRUN` owner records, 502 `UNRUN` rows, and 123,827 implied row-route associations. The null-anchor `css-masking-1` owner attaches 500 routes to 192 rows, or 96,000 associations. This is section/document fanout, not feature/assertion ownership. The WPT tree receipt and zero-outcome disclosure remain valid. |
| A-06 | **CORRECT** | Matrix totality compares only with `semantic_inventory(modules)` produced by the same recognizers. Current Flexbox and Grid sources normatively define `display` values immediately after informative partial tables, but the generated modules and matrix contain no `display` entity/row for either module. Current algorithm losses independently confirm that source semantics outside `_kind()` vanish rather than become open rows. |
| A-07 | **CORRECT** | Both split-string `include_bytes!(concat!(... "ma", "trix.json"))` and an unscanned `oracle.bin` containing `{"stage":"semantic-matrix"}` passed with zero violations. The allowlisted `data/observations.json` does not exist, and the positive test checks only its spelling. `NO_CANDIDATE_PRESENT` remains true; future build-input isolation is unproved. |
| A-08 | **CORRECT** | CSSWG and WPT are hard-coded external prepared-machine paths. `require_checkout()` verifies only `rev-parse HEAD`, not cleanliness or acquisition; `git` is inherited from `PATH`; the 96 selected CSSWG source digests have zero intersection with the retained authority-object digests. Current local checkouts were clean and local artifact replay is useful, but the epoch does not reconstruct sealed inputs in a clean root or prove the full toolchain identity. |
| A-09 | **CORRECT** | `critic_finding_coverage` is an English-description key map, and the boundary test proves only that the expected names exist. The executable negative corpus has ten CSS strings and four railroad strings; it omits the accepted matrix, dependency, dialect, span, WPT, boundary, sealing, and receipt counterexamples. Repeating the same 24 tests is reproducibility, not independent finding coverage. |
| A-10 | **CORRECT** | The generated artifacts occupy 63,188,477 bytes and the subject 130,631,391 bytes. There are 1,914 cloned entity-field occurrences and 1,946 repeated row-to-source references; only two plane vectors and two terminal decisions serve all 8,796 rows. Exact row identities are necessary, but cloned immutable payloads are not. This is a KISS/resource judgment grounded in reproduced structure, not a claim that evidence bytes may be deleted. |

### Critic B

| Finding | Decision | Reproduced basis |
| --- | --- | --- |
| B-F01 | **CORRECT** | The two retained Lex carriers have respectively 39 nonblank/1,296-byte and 24 nonblank/829-byte post-`%%` rule sections, none represented as rule semantics. Sixty-eight emitted pre-rule macros contain 62 `{macro}` occurrences, while their definition IDs have zero closure rows. Malformed Lex and Yacc probes also parse. This is current semantic loss hidden by `PARSED`. |
| B-F02 | **CORRECT** | `css-ruby-1::propdef::1` has own classes `informative partial propdef` yet is marked `NORMATIVE` and creates three rows. The two cited railroad carriers under `div.note` are marked normative and create one row each: five contaminated matrix rows total. Independently rescanning the selected sources found 39 live `[[!...]]` dependencies under `note`, `example`, `informative`, or `non-normative` ancestry that the dependency classifier does not demote. |
| B-F03 | **CORRECT** | The pinned Grid Item Placement Algorithm phrase has no containing algorithm carrier and `css-grid-1` has zero algorithm carriers. The cited CSSOM conversion algorithm phrases likewise have no containing carrier; its two stored carriers are elsewhere. The 292 present algorithm-step rows are therefore not a total algorithm-family census. |
| B-F04 | **CORRECT** | The selected `css-variables-1` source contains live `<wpt-rest>` at byte 37,155. `scope.json` contains no record or rejected-directive disposition for it; only 172 explicit unique routes are retained. The critic correctly does not promote its conservative 73-path difference to a final missing-set count. Silently dropping the directive alone proves the union incomplete. |
| B-F05 | **CORRECT** | Relevance decisions are global bibliography-key labels, not occurrence-qualified decisions. The selected CSS2 scanner semantics describe the highest code point using normative `UNICODE` and `ISO10646` references at the Lex carrier boundary, yet both are classified non-parsing and neither authority is among the eight retained parsing authorities. Parsing-dependency closure is incomplete. |
| B-F06 | **CORRECT** | `require_clean()` consults only `root_blockers`; modules and matrix return empty blocker arrays despite 1,564 unmapped, 37 ambiguous-import, 15 ambiguous-local reference occurrences, three unresolved extension bases, 601 already-unmapped WPT routes, and the locally missing semantics in B-F01 through B-F05. It is valid for a census to carry explicit downstream RED obligations. It is not valid to let `author_gate: PASS`, zero local blockers, or `input_epoch_reproducible` imply semantic cleanliness when local detector failures are not represented. |
| B-F07 | **CORRECT** | None of the 96 selected CSSWG source hashes is retained as an epoch object, and no CSSWG checkout or WPT Git object appears in the file manifest. Two empty output roots do not make the reused external input roots clean or sealed. Exact prepared-host replay is retained; clean-root reconstruction is not. |
| B-F08 | **CORRECT** | This independently converges with A-07: the observation allowlist names a nonexistent file, indirect/binary inputs bypass the scanner, and no resolved build/package/generated-artifact graph is cratered. Current absence of a candidate is a state fact, not isolation proof. |
| B-F09 | **CORRECT** | `verification.json` records 138 files, 130,606,247 bytes, and subject digest `14eb8b32d414ff95bc4f06b41854ec2c6e0989504acfadad2e99575eedb523eb`; the current manifest records 141 files, 130,631,391 bytes, and a different digest. The 25,144-byte delta exactly equals current README + AUTHOR + `verification.json`. The stored receipt cannot be the final proof of the current sealed subject. |
| B-F10 | **CORRECT** | Reproduced counts are 781 entity carriers with 5,425 source field occurrences expanded to 7,339 module occurrences, plus 8,796 rows over 6,850 source identities, one reused eight times. This converges with A-10. The judgment is limited to removable representation amplification; the retained authority objects and atomic obligation identities remain necessary evidence. |

## Merged mechanisms

The twenty findings reduce to nine owning mechanisms without discarding any
counterexample:

1. **Success spellings are not evidence.** Matrix enums and derived labels form a
   second, fail-open authority unless every positive state resolves to checked
   evidence and a real owner.
2. **Source membership is not occurrence complete.** Rendered bibliography forms,
   ambiguous source keys, global relevance decisions, and missing parsing
   authorities make dependency closure neither total nor unique.
3. **Normativity must be one inherited structural state.** Carrier classification,
   dependency classification, and WPT declaration handling currently disagree and
   omit own-class and `note` state.
4. **Dialect envelopes are not semantic parsers.** CSS VDS, Yacc, Lex, WebIDL, and
   railroad span defects delete syntax, rules, references, or source extent while
   still reporting `PARSED`.
5. **The semantic inventory needs an independent denominator.** The current
   implementation validates its row set against itself and omits normative value
   definitions and algorithm families.
6. **WPT membership and ownership are separate proofs.** `wpt-rest` must first be
   expanded; routes must then be related to exact features/assertions, and only an
   executed outcome can move a conformance plane.
7. **Replay and candidate isolation must follow actual input edges.** Prepared
   checkouts, inherited tools, substring scans, and nonexistent allowlist entries
   are not clean-root reconstruction or a resolved build/package crater.
8. **Mutation and receipt claims must close over final bytes.** Prose finding keys,
   duplicated test runs, and a pre-final verification receipt cannot establish
   hostile coverage or final sealing.
9. **Evidence should be normalized, not amplified.** Interned immutable source,
   field, status, and decision records preserve exact atomic rows while reducing
   memory, bytes, and the blast radius of semantic errors.

## Four-plane and total-product analysis

| Plane | Adjudicated result |
| --- | --- |
| Tranche fit | **RED; direction retained.** A read-only CSS standards assay belongs in VT.W0, but the current 130.6 MB assay is not the smallest complete route while source totality and its evidence gates remain unsound. JSON, incumbent profiling/hot-leaf attribution, four private compiling placeholders, and external RED gates remain absent. |
| Wave fidelity | **RED.** The exhaustive no-exception CSS union is disproved by lost dependencies, normativity, Lex rules, algorithms, `wpt-rest`, parsing authorities, and WPT ownership. Every semantic matrix row is still open, controls remain unrun, and no profile or candidate exists. |
| Feature truth | **RED with narrow receipts retained.** Authority objects, artifact hashes, current manifest, CPython pin, and prepared-host checkout pins reproduce. Strict parsing, exact semantic spans, independent totality, evidence-bearing positive statuses, clean-root replay, and boundary enforcement do not. |
| Greater-product effect | **RED / no shipping edge.** No production or package edge exists, which avoids a current product casualty. The proposed observations/boundary plane can nevertheless become a second semantic authority, candidate isolation is unenforced, and duplicated artifacts impose needless cost and dirigibility debt. |
| Total tranche/product | **RED.** No PT-E/J/C/K/R/V or BB-1/2/3/7 implementation is present or proved here. JSON/CSS co-equality, exact generated types, Rust/TS/WASM/ARM surfaces, packages, consumers, profiles, hard-stop ratios, four independent candidates, and two clean hostile passes are untouched. Candidate and complete-candidate counts remain zero. |

## Exact ordered KISS repair prescription

Repair belongs in a successor; do not patch production or call this epoch a
candidate.

1. **Make the claim boundary executable first.** Replace ambiguous `PASS` labels
   with three disjoint states: local observation validity, downstream semantic
   obligation state, and candidate/product state. Preserve honest open rows, but
   make any known local detector loss a local blocker. `input_epoch_reproducible`
   must mean only byte replay and must never imply semantic completion.
2. **Seal the minimum actual inputs for clean-root replay.** Retain the exact 96
   selected CSSWG source byte objects plus the exact WPT tree/path/blob material
   needed to reproduce route membership, or a deterministic content-addressed
   bundle that reconstructs them. Pin and verify the invoked Git/tool executables,
   reject dirty inputs, remove private absolute paths from the proof contract, and
   prove reconstruction from an empty input/output root.
3. **Use one source tokenizer and one inherited normativity state.** Recognize both
   Bikeshed and selected rendered-HTML bibliography forms. Carry comment, deleted,
   inserted/current, own and ancestor `note`, `example`, `informative`, and
   `non-normative` state into carriers, dependencies, algorithms, and WPT
   directives. Add the five current row casualties and 39 dependency casualties as
   mandatory mutations.
4. **Make dependency selection occurrence-qualified and collision-total.** Build a
   multimap, fail on every ambiguous live key, and retain one reviewed decision per
   source occurrence. No `setdefault` winner and no global parsing-relevance label
   may choose an edge. Retain exact Unicode/ISO10646 parsing-authority bytes and
   every other authority reached by the closed occurrence graph.
5. **Replace envelope recognizers with consuming dialect parsers.** CSS VDS must
   reject unmatched closers; Yacc must parse its grammar; Lex must parse definitions,
   both `%%` boundaries, rules/actions, and macro references; WebIDL must consume
   the whole carrier; railroad envelopes must cover the maximum descendant span.
   Each parser returns a source-exact AST or a typed blocker, never a partial
   `PARSED` success.
6. **Create an independent source-semantic census before modules or matrix.** Its
   source-native occurrence families must include property/descriptor tables,
   normative `dfn` value additions, productions, Lex/Yacc rules, complete WebIDL,
   named algorithms and structural steps, railroad diagrams, and explicit
   exclusions. Compare generated semantic rows with this independent occurrence
   ledger; every occurrence has exactly one mapped, excluded-with-reason, or local-
   blocker disposition.
7. **Complete WPT membership before ownership.** Interpret every supported WPT
   directive including `wpt-rest`; reject unknown directives; bind retained paths
   and blob identities. Then relate tests by exact feature/assertion metadata or a
   reviewed source-qualified relation. Document/null-anchor Cartesian fanout is
   forbidden. Keep all outcomes `UNRUN` until tests actually execute.
8. **Make every positive matrix state evidence-bearing.** A mapped BBNF plane must
   resolve to grammar/type receipts; fixture and WPT states to exact executed
   artifacts; recovery/diagnostic/serialization states to observations; and an
   assigned owner to an existing module in the relevant candidate graph. A WPT
   `FAIL` is discrepancy evidence, never completion evidence. Validate evidence
   content and ownership, not status spelling.
9. **Replace the substring boundary with a real crater.** Remove the nonexistent
   observations allowlist entry until a deliberately minimal, source-receipted
   observation artifact exists. For each future candidate, inspect resolved source,
   macro/build-script, package, generated-artifact, binary, symlink, and runtime
   read graphs; allow exact digest-qualified inputs only and fail closed on indirect
   reads.
10. **Turn every adjudicated counterexample into an executable external mutation.**
    Map each A-01…A-10 and B-F01…B-F10 ID to a concrete test, command, expected
    typed failure, and owning invariant. Delete the prose-only coverage test. Run
    the independent mutations once per regenerated artifact set; repeated identical
    suites do not count as independent coverage.
11. **Normalize evidence after semantics pass.** Intern source identities, field
    structures, plane vectors, and terminal decisions while retaining atomic row
    IDs and exact reverse links. Compare bytes/RSS/edges against a simpler
    normalized control and delete any layer without an independent invariant.
12. **Seal final receipts last.** Generate README/author note and all governed
    artifacts, freeze the non-review manifest, then produce a verification receipt
    whose subject definition explicitly handles its own self-reference. Prove the
    final manifest and receipt agree. Root `GESTALT-*.md` remains excluded, so this
    adjudication must not change the existing subject identity.
13. **Finish the unimplemented VT.W0 remainder.** Add the co-equal JSON oracle,
    no-inline/inlined incumbent profiles and hot-leaf attribution, four private
    compiling placeholders with external RED tests, and refreshed Lightning CSS
    and sonic-rs control freezes. These remain assay work and award no candidate
    credit.
14. **Submit the corrected successor to a fresh Gestalt 1.** Only after all prior
    steps reproduce from sealed empty roots may two new critics and a new
    adjudicator start a clean-count attempt. Any finding resets the count to zero.

## Final disposition

Bank only the exact primary-byte receipts, current artifact identities, prepared-
host replay observation, current all-open status census, and the fact that no
candidate exists. Supersede the unsound semantic gates, incomplete inventory,
boundary inspector, mutation-coverage claim, and stale final-proof claim in the
ordered repair above.

**Terminal state: RED. Clean count: 0. Movement authorization: NONE.**
