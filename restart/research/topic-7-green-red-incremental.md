# Topic 7 - Green/red trees, incremental parsing, and fault tolerance

Research scope: rowan/rust-analyzer green-red syntax trees, tree-sitter edit
reuse, Salsa query incrementality, parser fault tolerance, and the restart's
tape/direct substrate.

Source count: 8 primary or official sources.

Adversarial finding count: 5.

Provenance gaps: 2 named sources could not be verified from primary locations
inside this research slot; they are routed in §2 and §6, not asserted as facts.

## §1 — Settled position in the restart

1. Output-shape authority: every research artefact carries §1-§7, with §1
   citing path:line for every settled claim and rendering the current claim
   verbatim or near-verbatim (`restart/research/INDEX.md:18-34`).
2. Topic authority: Topic 7 is "Green/red trees + incremental parsing + fault
   tolerance" (`restart/research/INDEX.md:121`).
3. Topic locks: Topic 7 anchors on Lock 1 substrate, carry-incremental LSP
   fallback, and Lock 14 yaml graceful onboarding
   (`restart/research/INDEX.md:123-124`).
4. Topic question: the restart positions "tape + direct-to-struct as a single
   substrate" and must face rust-analyzer green/red trees, tree-sitter
   incremental edits, rowan parent-pointer red nodes, and yaml syntax-error LSP
   behavior (`restart/research/INDEX.md:125`).
5. Topic source set: rowan, rust-analyzer architecture, Ungar/Adams,
   HelpMate, Wagner/Graham, tree-sitter, and Salsa are the named sources
   (`restart/research/INDEX.md:126-133`).
6. Adversarial obligation: every research agent must surface at least one §6
   adversarial finding even if SOTA mostly converges
   (`restart/research/INDEX.md:149-153`).
7. Voice obligation: research prose must be calibrated, direct, path-cited,
   and free of placeholder wording (`restart/research/INDEX.md:155-157`).
8. Style baseline: project writing is pragmatic, economical, clear, and
   calibrated (`docs/precepts/instructions/STYLE.md:3-16`).
9. Style guard: vague attribution, promotional warmth, outline closers,
   mechanical boldface, and title-case inflation are anti-patterns
   (`docs/precepts/instructions/STYLE.md:58-73`).
10. Lessons baseline: "Substrate Without Consumer Is Not Progress"; every
    substrate change must carry a same-wave consumer or an explicitly declared
    brittleness window (`docs/precepts/instructions/LESSONS-LEARNED.md:17-26`).
11. Lessons baseline: "Contracts Need Producer And Consumer Gates"; wire
    contracts close on both producer output and consumer acceptance
    (`docs/precepts/instructions/LESSONS-LEARNED.md:74-80`).
12. Lessons baseline: "Runtime Truth Beats Source Claims"; source text is not
    runtime proof where generation, deployment, or services intervene
    (`docs/precepts/instructions/LESSONS-LEARNED.md:82-90`).
13. V4 carry-baseline: the V4 cohort returned READY with zero open punch items,
    all V1 cross-target conflicts closed, and all V3 punch items closed
    (`restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:104-112`).
14. V4 authority: the MASTER-PLAN trio carries executable authority for
    tranches A through J after V4 (`restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:120-122`).
15. V5.1 carry-baseline: yaml onboarding was closed by naming `yaml.bbnf`,
    permitting only grammar source plus Cargo metadata, and forbidding Rust
    source, parser registry, path registry, host shim, and declaration-crate
    onboarding (`restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:60-64`).
16. V5.1 carry-baseline: the yaml A->F->J trajectory is present and names
    gates from metadata admission through recovery/LSP and publication
    (`restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:69-72`).
17. PASS-3 V5 carry-baseline: PASS-3 was directionally correct and could return
    to READY if amendment did not alter tape substrate, path split, fixture
    separation, or generated budget model
    (`restart/audit/hardening/HARDENING-PASS-3-V5.md:363-381`).
18. PASS-3 V5 carry-baseline: the missing examples included yaml onboarding,
    pointer/select, incremental recovery, debug/DAP identity, and A->F->J
    progression (`restart/audit/hardening/HARDENING-PASS-3-V5.md:392-430`).
19. Lock 1: "Tape is the substrate, properly unioned with direct-to-struct;
    columnar SoA is dead; orthogonal codepaths and parallel substrates are
    dead" (`restart/locks/14-LOCKS.md:34`).
20. Lock 1 tape shape: tape is a "contiguous parsed-token-stream-with-payload
    arena, unioned with direct-to-struct typed values that borrow into it
    (`&'i Tape<'i>` + index)" (`restart/locks/14-LOCKS.md:34`).
21. Lock 1 fault rule: plans that resurrect parallel substrates or implement
    tape with consumer-later sequencing are faults; same-wave consumer wiring
    plus direct-to-struct union is honored (`restart/locks/14-LOCKS.md:34`).
22. Lock 14: "Full grammar generalisation; zero overfitting"; the substrate
    carries zero grammar-specific code (`restart/locks/14-LOCKS.md:60`).
23. Lock 14 onboarding: every grammar plugs in through grammar source,
    workspace metadata, and only rare declaration-crate escape valves
    (`restart/locks/14-LOCKS.md:60`).
24. Lock 14 yaml gate: adding a new grammar is a config plus grammar-source
    change with no generic-crate code change (`restart/locks/14-LOCKS.md:60`).
25. README tape claim: the failure was implementation rather than naming; the
    greenfield substrate is "a proper tape + direct-to-struct union, called
    tape" (`restart/README.md:285-287`).
26. README tape shape: tape is a contiguous parsed-token stream and a
    typed-value-borrow target (`restart/README.md:289-292`).
27. README tape payload: tape carries token discriminant, source span, payload
    offset, and structural pointer (`restart/README.md:294`).
28. README typed value shape: direct values borrow into tape with `kind`,
    `span`, `tape: &'i Tape<'i>`, and `idx`
    (`restart/README.md:296-305`).
29. README materialisation claim: `value.as_str()` indexes tape, slices source,
    and returns `&'i str` in constant time (`restart/README.md:308`).
30. README slice-borrow claim: source borrow is primary; `parse_in` and
    `parse_owned` are opt-in arena and owned escapes (`restart/README.md:310`).
31. README PASS-3 claim: PASS-3 specifies tape layout, typed-value-borrow
    shape, and materialisation cost (`restart/README.md:312`).
32. README unity claim: "No orthogonal codepath; no parallel substrate; no
    Vec<OpenFrame> ladder. One representation; one materialisation surface; one
    Visitor pattern" (`restart/README.md:314`).
33. README incremental claim: batch incremental parsing is opt-in, while
    LSP-class consumers always use it (`restart/README.md:342-344`).
34. README incremental model: "Treesitter-style: stable node identity per parsed
    token; diff-against-prior-tree algorithm; minimal re-parse window per edit;
    lossless concrete syntax tree (rowan-inspired)" (`restart/README.md:344`).
35. README PASS-3 incremental handoff: PASS-3 specifies per-token stable
    identity, tree-edit primitives, diff algorithm, LSP integration, propagation
    to type inference and cost model, and e-graph cache survival where
    invariants hold (`restart/README.md:346`).
36. README VM/LSP claim: incremental edits replay through the VM for stepped
    diagnostics, and LSP exposes "show parse trace" (`restart/README.md:348`).
37. README tree-sitter absorption: tree-sitter contributes ERROR/MISSING
    recovery, lossless CST, external scanners, query DSL, and incremental
    parsing (`restart/README.md:361`).
38. README rust-analyzer absorption: rust-analyzer contributes Salsa-style
    incremental computation, ungrammar declarative grammars, rowan-style
    lossless trees, and `chalk_ir` reference (`restart/README.md:362`).
39. README rowan absorption: rowan contributes lossless CST representation
    (`restart/README.md:373`).
40. README Lock 1 carry: tape is the greenfield substrate, unioned with
    direct-to-struct and no parallel substrate (`restart/README.md:383`).
41. README Lock 14 carry: the future-grammar onboarding test for `yaml.bbnf`
    is the verification gate (`restart/README.md:396`).
42. README final posture: the substrate is the "tape + direct-to-struct
    slice-borrow union" and the future-grammar onboarding test is two surfaces
    (`restart/README.md:471-473`).
43. PASS-3 identity claim: every public node has tape identity and every tape
    node can project through `ValueRef` (`restart/audit/pass-3-runtime/PASS-3.md:184`).
44. PASS-3 debug claim: Debug and DAP must reuse tape identity; trace events
    carry `SnapshotId`, `TapeId`, node kind, and source span when the tape node
    exists (`restart/audit/pass-3-runtime/PASS-3.md:186`).
45. PASS-3 span fallback: span-only fallback is allowed only inside a
    parse-failed region before a stable recovery node exists, and the fallback
    reason is debug-only (`restart/audit/pass-3-runtime/PASS-3.md:186`).
46. PASS-3 recovery claim: `@error(recover = ...)` and `@layout` are in;
    standalone `@recover` is only a migration alias if kept
    (`restart/audit/pass-3-runtime/PASS-3.md:188-190`).
47. PASS-3 current LSP claim: existing LSP applies incremental edits but full
    reparses/reanalyzes after updates; that is a useful entry shape, not the
    final incremental parse design (`restart/audit/pass-3-runtime/PASS-3.md:192`).
48. PASS-3 snapshot target: `DocumentSnapshot` contains `SnapshotId`, `Rope`,
    `TapeOwned`, `DiagnosticSet`, and `SemanticIndex`
    (`restart/audit/pass-3-runtime/PASS-3.md:194-201`).
49. PASS-3 reparse target: `ReparsePlan` has `Reuse` unchanged tape ranges and
    `Reparse` dirty ranges with anchors
    (`restart/audit/pass-3-runtime/PASS-3.md:203-206`).
50. PASS-3 worked path: deleting a JSON member value builds a `ReparsePlan`,
    skips to a sync token, emits `BBNF-RECOVERY001`, inserts a recovered node
    into tape, and keeps unchanged ranges (`restart/audit/pass-3-runtime/PASS-3.md:209-233`).
51. PASS-3 fallback path: deleting closing delimiters causes anchor matching to
    fail, full parse fallback with `anchor_miss_unbalanced_scope`, fallback
    ledger increment, silent default LSP output, and debug-only reason
    (`restart/audit/pass-3-runtime/PASS-3.md:235-240`).
52. PASS-3 fallback rule: full parse fallback is allowed when anchors fail, but
    bench/dev output must report fallback rates and users see stable
    diagnostics rather than implementation warnings
    (`restart/audit/pass-3-runtime/PASS-3.md:242`).
53. PASS-3 dataset gates: JSON, CSS, and BBNF edit corpora have reuse targets
    and full-reparse fallback ceilings (`restart/audit/pass-3-runtime/PASS-3.md:244-251`).
54. PASS-3 LSP policy: fallback ledgers go to bench output, not LSP
    `Diagnostic` items or `showMessage`; debug diagnostics are disabled in
    shipped builds (`restart/audit/pass-3-runtime/PASS-3.md:253`).
55. PASS-3 yaml fixture separation: Lock 14 onboarding admits exactly
    `yaml.bbnf` plus one workspace metadata block; fixtures are parity-phase
    evidence (`restart/audit/pass-3-runtime/PASS-3.md:383-388`).
56. PASS-3 yaml row: the yaml runtime, visitor, path schema, host route, and
    diagnostics are generated from `yaml.bbnf` plus metadata, with zero Rust
    per-grammar code (`restart/audit/pass-3-runtime/PASS-3.md:405-407`).
57. Architecture server contracts: `DocumentSnapshot` is the immutable source
    snapshot, `ReparsePlan` is the incremental parse plan, and `DiagnosticSet`
    is the recoverable editor display set (`restart/ARCHITECTURE.md:252-260`).
58. Architecture incremental carry: PASS-3 makes incremental parsing opt-in for
    batch and always-on for LSP; README says the same
    (`restart/ARCHITECTURE.md:262-264`).
59. Architecture yaml allowed changes: yaml onboarding admits only
    `grammars/yaml.bbnf` and one Cargo metadata block
    (`restart/ARCHITECTURE.md:1287-1294`).
60. Architecture yaml forbidden changes: handwritten Rust source, package
    declarations, manual parser/path registries, host shims, and onboarding
    declaration crates are forbidden (`restart/ARCHITECTURE.md:1296-1305`).
61. Architecture yaml walkthrough: grammar source carries yaml rules,
    block-bodied host functions, and `@error(recover = ...)`; metadata supplies
    runtime/path/visitor/host/diagnostic parameters
    (`restart/ARCHITECTURE.md:1320-1327`).
62. Architecture yaml matrix: `YamlRoot` borrows over `&'i Tape<'i>` and
    generated yaml runtime files include layout/error plus host route if
    metadata declares one (`restart/ARCHITECTURE.md:1340-1351`).
63. MASTER yaml trajectory: yaml is a receiving-gate proof, not a special yaml
    implementation path (`restart/MASTER-PLAN.md:208-211`).
64. MASTER yaml handoff: A admits grammar+metadata, B hosts generated yaml on
    tape/direct, C represents recovery facts without grammar dispatch, D proves
    `@error(recover = ...)`, F emits runtime, and I consumes same yaml
    diagnostics in recovery/LSP (`restart/MASTER-PLAN.md:215-224`).
65. MASTER Tranche I goal: diagnostics and editor behavior ship over the same
    pipeline and runtime contracts (`restart/MASTER-PLAN.md:501-504`).
66. MASTER Tranche I inheritance: README incremental rule and PASS-3
    recovery/LSP contract feed I (`restart/MASTER-PLAN.md:506-512`).
67. MASTER Tranche I waves: I.W1 owns incremental source snapshots and reparse
    plans, I.W2 owns LSP diagnostics, and I.W4 owns CLI/LSP parity
    (`restart/MASTER-PLAN.md:516-522`).
68. MASTER Tranche I hard close: `cargo test -p bbnf-language-server
    incremental diagnostics` is required (`restart/MASTER-PLAN.md:524-528`).
69. MASTER risk ledger: direct-to-struct bypassing tape is closed by B/F tests,
    and LSP incremental parser divergence is closed by I CLI/LSP parity tests
    (`restart/MASTER-PLAN.md:747-754`).
70. MASTER carry ledger: yaml onboarding routes to A/F/G/J, and the gate is
    "yaml source + workspace metadata plus generated runtime only"
    (`restart/MASTER-PLAN.md:772`).
71. MASTER friction ledger: adding yaml means two surfaces only, generated
    runtime/path/visitor/host outputs and bench manifest are derivatives, and
    manual Rust registration faults emit metadata/grammar-name diagnostics
    (`restart/MASTER-PLAN.md:798`).

## §2 — SOTA literature deep-dive

1. Source register.
2. [S1] rowan crate official documentation and source pages:
   `https://docs.rs/rowan/latest/rowan/`,
   `https://docs.rs/rowan/latest/rowan/struct.GreenNode.html`,
   `https://docs.rs/rowan/latest/rowan/api/struct.SyntaxNode.html`,
   `https://docs.rs/rowan/latest/rowan/cursor/index.html`, and
   `https://docs.rs/rowan/latest/rowan/struct.NodeCache.html`.
3. [S2] rust-analyzer official syntax design:
   `https://rust-analyzer.github.io/book/contributing/syntax.html`.
4. [S3] rust-analyzer official architecture design:
   `https://rust-analyzer.github.io/book/contributing/architecture.html`.
5. [S4] Wagner, T. A. and Graham, S. L. 1998, "Efficient and Flexible
   Incremental Parsing," TOPLAS author/project PDF:
   `https://harmonia.cs.berkeley.edu/papers/twagner-parsing.pdf`.
6. [S5] tree-sitter official repository README and advanced parsing docs:
   `https://github.com/tree-sitter/tree-sitter` and
   `https://tree-sitter.github.io/tree-sitter/using-parsers/3-advanced-parsing.html`.
7. [S6] tree-sitter official special-node and binding docs:
   `https://tree-sitter.github.io/tree-sitter/using-parsers/queries/1-syntax.html`
   and `https://tree-sitter.github.io/py-tree-sitter/classes/tree_sitter.Node.html`.
8. [S7] Salsa official book source:
   `https://raw.githubusercontent.com/salsa-rs/salsa/master/book/src/overview.md`
   and
   `https://raw.githubusercontent.com/salsa-rs/salsa/master/book/src/how_salsa_works.md`.
9. [S8] Salsa crate official docs:
   `https://docs.rs/salsa/latest/salsa/`.
10. Provenance gap [G1]: no primary source verified the exact requested
    Ungar & Adams 1994 cache/data-stall title. Search surfaced later
    Ungar/Adams manycore work and unrelated 1994 pipeline papers, but no
    primary match for the named paper. This gap affects cache-locality analogy
    only; the restart's tape locality claim is judged against rowan,
    tree-sitter, Wagner/Graham, and Salsa instead.
11. Provenance gap [G2]: no primary source verified "Brand et al. 2003
    HelpMate parsing framework." Search hits were false positives around
    common English "helpmate" usage. No architectural claim below relies on
    HelpMate.

12. Rowan: load-bearing claims.
13. Rowan's crate surface says it is "a generic library for lossless syntax
    trees" and exposes `SyntaxNode`, `SyntaxToken`, `GreenNode`,
    `GreenToken`, `NodeCache`, text ranges, and syntax kinds [S1].
14. `GreenNode` is documented as an internal node in an immutable tree with
    node/token children; it carries `kind`, `text_len`, and child operations
    such as replace, insert, remove, and splice [S1].
15. `NodeCache` is an interner for green tokens and green nodes [S1].
16. `SyntaxNode` is the rowan red view: it can be created from a `GreenNode`
    root, returns `text_range`, exposes `green`, and carries navigation
    through `parent`, `ancestors`, children, siblings, descendants, preorder
    traversal, token-at-offset, and range-covering queries [S1].
17. The cursor module states the operational red-layer invariant most plainly:
    it is a zipper over a purely functional green tree; a cursor node points to
    a `GreenNode` and a parent `SyntaxNode`, allowing ancestor/descendant
    iteration and cheap absolute offsets [S1].
18. Design tradeoff: rowan gives immutable, shareable green data and red
    navigation context without storing parent pointers inside the green tree.
19. Design pressure on bbnf: a single owning representation can still need
    transient contextual views. Rowan's red layer is not a rival substrate; it
    is a view that reconstructs parent/offset context around immutable data.

20. Rust-analyzer syntax design: load-bearing claims.
21. The syntax design requires parsing to be lossless, resilient on invalid
    input, performance-sensitive, and isolated from the syntax tree so parser
    and tree representation can vary independently [S2].
22. Rust-analyzer names three syntax layers: GreenNodes, SyntaxNodes/red nodes,
    and AST [S2].
23. Only green nodes store actual data; red nodes and AST are views into the
    green tree [S2].
24. Rust-analyzer keeps syntax trees semi-transient: the frontend does not keep
    all syntax trees in memory and lowers them to more compact, rigid
    representations that can map back to syntax when needed [S2].
25. The green tree is untyped, stores full token text, and recovers original
    text by concatenating token texts in order [S2].
26. Design tradeoff: rust-analyzer accepts tree-child lookup costs and optional
    AST accessors in exchange for full-fidelity syntax and resilience.
27. Design pressure on bbnf: direct-to-struct values should be AST-like views
    over tape. The tape should own full-fidelity bytes and recovery markers;
    typed values should not become an independent edit model.

28. Rust-analyzer architecture: load-bearing claims.
29. The architecture states the parser transforms one flat stream of events
    into another flat stream of events, independent of tree structure and token
    representation [S3].
30. It states parsing never fails; the parser produces `(T, Vec<Error>)` rather
    than `Result<T, Error>` [S3].
31. The syntax crate uses rowan, AST provides type-safe API over raw rowan
    trees, and ungrammar generates syntax kinds and AST modules [S3].
32. The syntax crate is independent from Salsa and LSP; this API boundary lets
    tools work with syntax without semantic build context [S3].
33. Syntax trees are value types, fully determined by syntax node contents, and
    should not store semantic information because IDE edits and refactors
    transform trees [S3].
34. Syntax trees are incomplete by design; an AST method may return `Option`
    at runtime even when grammar forbids absence [S3].
35. Rust-analyzer uses Salsa for incremental and on-demand computation; inputs
    are supplied by the analyzer client and derived data comes from inputs [S3].
36. Design tradeoff: parse resilience and syntax boundaries precede semantic
    incrementality. Salsa is downstream from syntax.
37. Design pressure on bbnf: `DocumentSnapshot` and tape should be parser
    products; Salsa-like query state should depend on them rather than be
    embedded into tape nodes.

38. Wagner/Graham: load-bearing claims.
39. Wagner and Graham present incremental parsing for arbitrary textual and
    structural modifications, not only single-site edits or cursor prefixes
    [S4].
40. Their paper states that, in common cases such as changing identifier
    spelling, the parser makes no parse-tree modifications [S4].
41. The model has reference, actual, and current versions, and nodes expose
    `has_changes` with local/nested modes so clients can visit changed regions
    only [S4].
42. The incremental parser works over persistent program structure and seeks
    subtree reuse; the paper distinguishes state matching from sentential-form
    reuse [S4].
43. Corollary 5.1.2 states the algorithm produces the same parse tree as a batch
    parser reading the same terminal yield [S4].
44. The performance section says grammar choice matters greatly: unbounded
    sequences must be represented as associative sequences so nodes are
    reachable in logarithmic rather than linear time [S4].
45. The node-reuse section defines reuse paths: context reuse is top-down,
    content reuse is bottom-up, and absence of such a path warrants a new
    nonterminal name [S4].
46. Design tradeoff: correctness against batch parse is necessary, but total
    environment response also depends on stable node identity, balanced syntax
    representation, and user annotation preservation.
47. Design pressure on bbnf: "stable node identity per parsed token" is weaker
    than reuse-path accounting. Incremental tape IDs need proof of reuse, not
    span equality alone.

48. Tree-sitter: load-bearing claims.
49. The official README defines tree-sitter as a parser generator and
    incremental parsing library that builds concrete syntax trees and updates
    them as source is edited [S5].
50. Tree-sitter aims to parse on every editor keystroke and to provide useful
    results in the presence of syntax errors [S5].
51. The advanced parsing docs give the edit protocol: apply `TSInputEdit` to
    the old `TSTree`, then call parse again with the old tree so the new tree
    internally shares structure with the old one [S5].
52. Stored `TSNode` handles need the same edit operation if the client wants to
    keep using those handles; otherwise clients normally refetch from the new
    tree [S5].
53. Tree copies are cheap by atomic reference count, but individual `TSTree`
    instances are not thread safe; copy for concurrent use [S5].
54. Tree-sitter's query docs define `(ERROR)` nodes for unrecognized text and
    `(MISSING)` nodes for recovery by inserting missing tokens; missing nodes
    are zero-width and need separate queries from `(ERROR)` [S6].
55. Binding docs expose `has_changes`, `has_error`, `is_error`, `is_missing`,
    `parent`, `parse_state`, `next_parse_state`, byte ranges, points, and
    grammar ids [S6].
56. Binding docs state node IDs are unique within a tree, and reused nodes keep
    the same ID across a new tree based on an older tree [S6].
57. Design tradeoff: tree-sitter gives efficient syntax reuse and error-tolerant
    CSTs, while clients must treat node handles as tree-version scoped.
58. Design pressure on bbnf: `TapeId` must be snapshot-scoped, with explicit
    old-to-new reuse mapping. A global ID by byte span would overclaim.

59. Salsa: load-bearing claims.
60. Salsa's overview states its goal is efficient incremental recomputation and
    names rust-analyzer as a use case for recompiling quickly while typing [S7].
61. Salsa separates an outer loop that mutates inputs from deterministic
    program computation; input mutation happens outside tracked computation
    [S7].
62. Salsa stores computation values in a database and consults the database
    after input changes to reuse values [S7].
63. Tracked functions track which inputs they access and memoize their return
    values; the red-green algorithm decides whether re-execution is needed
    [S7].
64. Tracked structs are matched across executions, usually by creation order,
    and `#[id]` fields allow matching when items reorder [S7].
65. Accumulators provide side-channel diagnostics separate from the main return
    value [S7].
66. Salsa's crate docs expose `Database`, `Revision`, `Durability`,
    `DatabaseKeyIndex`, `Event`, `input`, `tracked`, `interned`, and
    `accumulator` as first-class vocabulary [S8].
67. Design tradeoff: Salsa does not replace a parser or syntax tree. It
    memoizes deterministic derived computations over explicit inputs.
68. Design pressure on bbnf: recovery diagnostics can use accumulator-like
    side channels, but parser recovery facts must still live in the tape/snapshot
    contract so LSP, CLI, visitors, and path queries see the same state.

69. Synthesis answer to the engagement question.
70. Tape + direct-to-struct survives contact with rowan if "union" means one
    owning lossless tape plus contextual/typed projections, not one Rust type
    that carries every navigation and semantic role.
71. Tape + direct-to-struct survives contact with tree-sitter if `TapeId` is
    snapshot-scoped and reuse is proved through edit anchors, recovery nodes,
    and old-to-new mapping.
72. Tape + direct-to-struct survives contact with Salsa if semantic and cost
    incrementality are query layers over `DocumentSnapshot`, not fields stored
    inside tape nodes.
73. With a yaml syntax error, bbnf's differentiator is not the mere presence of
    ERROR/MISSING nodes; tree-sitter already has those.
74. The differentiator is a single generated runtime identity that carries the
    recovered syntax node, source span, diagnostic code, typed placeholder or
    recovered value surface, visitor `VisitTypes::ERROR` behavior, path-schema
    visibility, LSP/CLI parity, and fallback ledger from the same grammar plus
    metadata source.
75. Therefore yaml onboarding remains two-surface: `yaml.bbnf` and metadata
    declare syntax, recovery, host route, and generated budgets; the syntax
    error does not force a Rust registry, hand host shim, or parallel parser.

## §3 — Convergence points

1. Lossless fault tolerance converges.
2. Restart claim: incremental parsing is lossless and rowan-inspired
   (`restart/README.md:344`).
3. Restart claim: tree-sitter's ERROR/MISSING recovery and lossless CST are
   absorbed into runtime and language-server surfaces (`restart/README.md:361`).
4. SOTA evidence: rust-analyzer requires lossless and resilient parsing [S2];
   tree-sitter exposes ERROR and MISSING nodes [S6].
5. Match: bbnf is correct to carry recovery nodes as real syntax/tape state,
   not as parser warnings discarded before LSP.

6. Green-data plus typed-view architecture converges.
7. Restart claim: typed values borrow into tape through `&'i Tape<'i>` plus
   index (`restart/README.md:296-305`; `restart/locks/14-LOCKS.md:34`).
8. SOTA evidence: rust-analyzer uses green data, red nodes, and AST views [S2];
   rowan `SyntaxNode` wraps green data with parent and offset context [S1].
9. Match: bbnf's direct structs can be the typed AST-like layer as long as the
   tape owns the source spans, recovery flags, and identity.

10. Snapshot plus reparse planning converges.
11. Restart claim: `DocumentSnapshot` owns text, tape, diagnostics, and semantic
    index (`restart/audit/pass-3-runtime/PASS-3.md:194-201`).
12. Restart claim: `ReparsePlan` carries reuse ranges, dirty ranges, and anchors
    (`restart/audit/pass-3-runtime/PASS-3.md:203-206`).
13. SOTA evidence: tree-sitter edits the old tree, reparses with it, and shares
    structure [S5]; Wagner/Graham uses persistent versions and changed-node
    queries [S4].
14. Match: bbnf's snapshot model is positioned in the same family as
    tree-sitter and Wagner/Graham.

15. Fallback accounting converges.
16. Restart claim: fallback ledgers go to bench output and debug-only channels,
    not user diagnostics (`restart/audit/pass-3-runtime/PASS-3.md:235-253`).
17. SOTA evidence: tree-sitter makes syntax errors queryable while keeping parse
    products usable [S5][S6].
18. Match: users receive stable syntax diagnostics; implementation fallback is
    engineering telemetry.

19. Query-based downstream incrementality converges.
20. Restart claim: partial reparses propagate to type inference and cost model,
    with e-graph caches surviving where invariants hold (`restart/README.md:346`).
21. SOTA evidence: rust-analyzer keeps syntax independent from Salsa/LSP [S3];
    Salsa memoizes deterministic tracked functions and reuses values after input
    changes [S7].
22. Match: bbnf should route semantic/cost invalidation through query keys over
    snapshot/tape facts, not through a parser-specific side store.

23. Declarative future grammar onboarding converges.
24. Restart claim: yaml onboarding is grammar source plus metadata only
    (`restart/ARCHITECTURE.md:1287-1305`; `restart/MASTER-PLAN.md:215-224`).
25. SOTA evidence: rust-analyzer uses ungrammar to generate syntax kinds and
    AST modules [S3]; tree-sitter generates parsers and node type metadata from
    grammar definitions [S5].
26. Match: the restart's two-surface yaml route is coherent with generated
    parser ecosystems if generated recovery/path/typed metadata is complete.

## §4 — Divergence points

1. Divergence: bbnf names a tape/direct "union"; rowan/rust-analyzer names
   green/red/AST separation.
2. Restart claim: "One representation; one materialisation surface; one Visitor
   pattern" (`restart/README.md:314`).
3. SOTA evidence: rowan green nodes own immutable data, red `SyntaxNode`s carry
   parent and offset context, and AST is a type-safe view [S1][S2].
4. Reason: bbnf can keep one owning representation, but it still needs red-like
   cursors and typed direct views. The divergence is principled only if the
   docs say "one owning representation" rather than "no contextual view."

5. Divergence: bbnf's tape is a token stream with payload arena; rowan stores
   full token text in green tokens.
6. Restart claim: tape carries source spans and payload offsets into arenas
   (`restart/README.md:294`), while materialisation slices the original source
   (`restart/README.md:308`).
7. SOTA evidence: rust-analyzer green tokens store full token text and recover
   original text by concatenation [S2].
8. Reason: bbnf optimizes slice-borrow runtime and direct value materialisation.
   The divergence is valid for runtime parsers, but the design must prove
   fault-tolerant CST fidelity without relying on copied token text.

9. Divergence: bbnf says "stable node identity per parsed token"; tree-sitter
   and Wagner/Graham stabilize reused syntax nodes through reuse proof.
10. Restart claim: stable node identity is "per parsed token"
    (`restart/README.md:344`).
11. SOTA evidence: tree-sitter node IDs are tree-scoped and reused across trees
    only when nodes are reused [S6]; Wagner/Graham requires reuse paths [S4].
12. Reason: bbnf's phrase is too weak and slightly mislocated. Identity should
    attach to tape syntax/recovery nodes, with old-to-new reuse mapping.

13. Divergence: bbnf routes recovery declarations through BBNF metadata and
    generated runtime; tree-sitter has built-in error recovery and grammar
    conflict-cost machinery.
14. Restart claim: `@error(recover = ...)` is the authoring surface
    (`restart/audit/pass-3-runtime/PASS-3.md:188-190`).
15. SOTA evidence: tree-sitter inserts ERROR and MISSING nodes using internal
    recovery costs [S6].
16. Reason: bbnf's grammar-authoritative posture needs declarative recovery
    facts so direct values, visitor defaults, LSP diagnostics, and path schemas
    share the same generated contract.

17. Divergence: bbnf mentions e-graph cache survival from parse edits;
    rust-analyzer/Salsa push semantic reuse into dependency-tracked queries.
18. Restart claim: e-graph caches survive across edits where invariants hold
    (`restart/README.md:346`).
19. SOTA evidence: Salsa recomputation depends on tracked input reads, database
    revisions, and deterministic query functions [S7][S8].
20. Reason: cache survival cannot be a parser promise. It must be a query-layer
    promise with invalidation gates keyed by snapshot/tape facts.

## §5 — Refinements to fold

1. Refinement R1.
2. Target file:line: `restart/README.md:314`.
3. Current text: "One representation; one materialisation surface; one Visitor
   pattern."
4. Proposed text: "One owning representation; one generated materialisation
   surface; one Visitor pattern. Red-like cursors, direct typed roots, and AST
   adapters may be transient views over tape, but they do not own independent
   parse identity or recovery state."
5. Rationale: rowan separates immutable green data from red parent/offset
   context without creating a rival syntax substrate [S1][S2].

6. Refinement R2.
7. Target file:line: `restart/README.md:344`.
8. Current text: "Treesitter-style: stable node identity per parsed token;
   diff-against-prior-tree algorithm; minimal re-parse window per edit;
   lossless concrete syntax tree (rowan-inspired)."
9. Proposed text: "Treesitter-style: snapshot-scoped stable `TapeId` per
   syntax/recovery node, old-to-new reuse mapping only when anchors prove
   reuse; diff-against-prior-tree algorithm; minimal re-parse window per edit;
   lossless concrete syntax tree (rowan-inspired)."
10. Rationale: tree-sitter node IDs survive into a new tree only for reused
    nodes [S6], and Wagner/Graham requires reuse paths [S4].

11. Refinement R3.
12. Target file:line: `restart/README.md:346`.
13. Current text: "the e-graph caches survive across edits where invariants
    hold."
14. Proposed text: "type-inference, cost-model, and e-graph query caches reuse
    only through dependency keys over `DocumentSnapshot`, `TapeId` reuse maps,
    and semantic facts; cache survival is reported with invalidation reason when
    a parse edit crosses those keys."
15. Rationale: Salsa makes reuse a query/database decision, not a parser-local
    assertion [S7][S8].

16. Refinement R4.
17. Target file:line: `restart/audit/pass-3-runtime/PASS-3.md:184`.
18. Current text: "every public node has tape identity and every tape node can
    be projected through `ValueRef`."
19. Proposed text: "every public node has snapshot-scoped tape identity, every
    tape node can be projected through `ValueRef`, and cross-snapshot identity
    exists only through a `ReparsePlan` reuse map."
20. Rationale: tree-sitter IDs are tree-scoped, while reused nodes carry the
    same ID across trees only after reuse [S6].

21. Refinement R5.
22. Target file:line: `restart/audit/pass-3-runtime/PASS-3.md:203-206`.
23. Current text: `ReparsePlan` has `Reuse { unchanged: Vec<TapeRange> }` and
    `Reparse { dirty: Vec<TextRange>, anchors: Vec<TapeId> }`.
24. Proposed text: extend the sketch with `reuse_map: Vec<(OldTapeId,
    NewTapeId)>`, `fallback_reason: Option<FallbackReason>`, and
    `invalidated_queries: QueryInvalidationSet`.
25. Rationale: Wagner/Graham's reuse-path model and Salsa's dependency model
    both require explicit proof objects, not only dirty ranges [S4][S7].

26. Refinement R6.
27. Target file:line: `restart/audit/pass-3-runtime/PASS-3.md:227-233`.
28. Current text: the recovered member-value node is flagged as recovered in
    tape and visitors can observe recovery.
29. Proposed text: add that recovered tape nodes carry `RecoveryKind::{Error,
    Missing, Substituted}`, diagnostic code, sync token, typed placeholder
    policy, and `VisitTypes::ERROR` behavior.
30. Rationale: tree-sitter distinguishes ERROR and zero-width MISSING nodes
    [S6]; bbnf needs the richer typed surface to beat mere CST recovery.

31. Refinement R7.
32. Target file:line: `restart/MASTER-PLAN.md:519`.
33. Current text: "Edited seed grammar reparses changed region."
34. Proposed text: "Edited seed grammar reparses changed region or reports a
    named fallback reason, reuse-map absence, and silent LSP behavior in the
    `incremental/edit_anchor` ledger."
35. Rationale: tree-sitter requires old-tree edits and handle refresh [S5],
    while PASS-3 already hides fallback from normal LSP output
    (`restart/audit/pass-3-runtime/PASS-3.md:235-253`).

36. Refinement R8.
37. Target file:line: `restart/MASTER-PLAN.md:798`.
38. Current text: adding yaml friction asks "Where do I register yaml in Rust?"
39. Proposed text: append a yaml syntax-error friction row: target user =
    grammar author; mental model = "The grammar is admitted even while a sample
    edit is malformed"; confusion point = "Why did LSP keep a typed `YamlRoot`
    when indentation is broken?"; artefact = recovery cookbook plus
    `DocumentSnapshot` trace; diagnostic = `BBNF-RECOVERY001` plus hidden
    fallback reason when anchors fail.
40. Rationale: the engagement question asks what bbnf carries during yaml syntax
    error; tree-sitter already covers generic ERROR/MISSING nodes [S6].

## §6 — Adversarial findings

1. Finding A1: "one representation" is too strong unless scoped to ownership.
2. Contradicted lock: Lock 1, insofar as readers may interpret "one
   representation" as forbidding red/cursor views (`restart/README.md:314`;
   `restart/locks/14-LOCKS.md:34`).
3. SOTA evidence: rowan's red `SyntaxNode` points to a green node and a parent
   syntax node, providing ancestors and offsets over immutable green data [S1].
4. Proposed amendment: replace "one representation" with "one owning
   representation" and explicitly permit transient red-like views without
   independent parse identity.
5. Receiving phase: Phase 2 README/PASS-3 fold.

6. Finding A2: stable identity "per parsed token" is underspecified.
7. Contradicted lock: carry-incremental LSP fallback in Topic 7 and README
   incremental posture (`restart/README.md:344-346`).
8. SOTA evidence: tree-sitter IDs are unique within a tree and carry over only
   when old nodes are reused [S6]; Wagner/Graham grounds reuse in paths [S4].
9. Proposed amendment: make `TapeId` snapshot-scoped and require old-to-new
   reuse maps in `ReparsePlan`.
10. Receiving phase: Phase 2 PASS-3 and Tranche I surgery.

11. Finding A3: bbnf must not claim unique value from ERROR/MISSING nodes.
12. Contradicted lock: no settled lock is false, but the Topic 7 engagement
    question would overclaim if answered as "bbnf carries recovery nodes" alone
    (`restart/README.md:361`; `restart/audit/pass-3-runtime/PASS-3.md:227-233`).
13. SOTA evidence: tree-sitter already exposes ERROR and MISSING nodes [S6].
14. Proposed amendment: state the differentiator as typed recovery over the
    same tape/direct identity, with diagnostic, visitor, path, CLI/LSP parity,
    and metadata-derived yaml onboarding carried together.
15. Receiving phase: Phase 2 README/PASS-3/MASTER clarification.

16. Finding A4: e-graph cache survival is too parser-local as written.
17. Contradicted lock: no lock is contradicted; the README claim is too weakly
    gated (`restart/README.md:346`).
18. SOTA evidence: Salsa reuse belongs to tracked deterministic computations
    over inputs and database revisions [S7][S8].
19. Proposed amendment: route all type/cost/e-graph reuse through query
    invalidation keys over `DocumentSnapshot`, `TapeId` reuse maps, and semantic
    facts.
20. Receiving phase: Phase 2 README/PASS-3 fold; Tranche I query tests.

21. Finding A5: the named Ungar/Adams 1994 and HelpMate sources are provenance
    gaps in this pass.
22. Contradicted lock: the SOTA anchoring obligation, not an architectural lock
    (`restart/research/INDEX.md:24`; `restart/research/INDEX.md:126-133`).
23. SOTA evidence: no primary source was found for the exact named titles; no
    claim in this file rests on those sources.
24. Proposed amendment: keep them in a provenance-gap row until a Phase 2
    librarian verifies exact bibliographic entries or removes them from the
    Topic 7 source expectation.
25. Receiving phase: Phase 2 research-fold bibliography cleanup.

## §7 — Surgery proposals

1. Surgery S1: README ownership wording.
2. Target file:line: `restart/README.md:314`.
3. Directive: replace "One representation; one materialisation surface; one
   Visitor pattern." with the R1 proposed text.
4. Acceptance gate: `rg -n "one owning representation|red-like cursors|direct
   typed roots|independent parse identity" restart/README.md`.
5. Dependency: §5 R1 and §6 A1.

6. Surgery S2: README incremental identity wording.
7. Target file:line: `restart/README.md:344`.
8. Directive: replace "stable node identity per parsed token" with
   "snapshot-scoped stable `TapeId` per syntax/recovery node, old-to-new reuse
   mapping only when anchors prove reuse."
9. Acceptance gate: `rg -n "snapshot-scoped stable.*TapeId|old-to-new reuse
   mapping|anchors prove reuse" restart/README.md restart/audit/pass-3-runtime/PASS-3.md`.
10. Dependency: §5 R2 and §6 A2.

11. Surgery S3: cache-survival query gate.
12. Target file:line: `restart/README.md:346`.
13. Directive: replace parser-local cache survival wording with the R3 proposed
    query-key wording.
14. Acceptance gate: `rg -n "QueryInvalidationSet|DocumentSnapshot.*TapeId
    reuse|cache.*invalidation reason|e-graph query caches" restart/README.md
    restart/audit/pass-3-runtime/PASS-3.md`.
15. Dependency: §5 R3 and §6 A4.

16. Surgery S4: PASS-3 tape identity scope.
17. Target file:line: `restart/audit/pass-3-runtime/PASS-3.md:184`.
18. Directive: add snapshot-scoped identity and cross-snapshot reuse-map
    language from R4.
19. Acceptance gate: `rg -n "snapshot-scoped tape identity|cross-snapshot
    identity|ReparsePlan.*reuse map" restart/audit/pass-3-runtime/PASS-3.md`.
20. Dependency: §5 R4 and §6 A2.

21. Surgery S5: PASS-3 `ReparsePlan` proof fields.
22. Target file:line: `restart/audit/pass-3-runtime/PASS-3.md:203-206`.
23. Directive: extend the code sketch with `reuse_map`, `fallback_reason`, and
    `invalidated_queries`.
24. Acceptance gate: `rg -n "reuse_map|fallback_reason|invalidated_queries"
    restart/audit/pass-3-runtime/PASS-3.md`.
25. Dependency: §5 R5 and §6 A2/A4.

26. Surgery S6: PASS-3 recovery node shape.
27. Target file:line: `restart/audit/pass-3-runtime/PASS-3.md:227-233`.
28. Directive: add `RecoveryKind::{Error, Missing, Substituted}`, diagnostic
    code, sync token, typed placeholder policy, and visitor behavior.
29. Acceptance gate: `rg -n "RecoveryKind|Missing|Substituted|typed
    placeholder|VisitTypes::ERROR" restart/audit/pass-3-runtime/PASS-3.md`.
30. Dependency: §5 R6 and §6 A3.

31. Surgery S7: MASTER Tranche I fallback ledger gate.
32. Target file:line: `restart/MASTER-PLAN.md:519`.
33. Directive: change the I.W1 consumer gate to include named fallback reason,
    absent reuse-map evidence, and silent LSP behavior.
34. Acceptance gate: `rg -n "fallback reason|reuse-map absence|silent LSP
    behavior|incremental/edit_anchor" restart/MASTER-PLAN.md`.
35. Dependency: §5 R7 and §6 A2.

36. Surgery S8: yaml syntax-error friction row.
37. Target file:line: `restart/MASTER-PLAN.md:798`.
38. Directive: add the yaml syntax-error friction row from R8.
39. Acceptance gate: `rg -n "YamlRoot.*indentation|BBNF-RECOVERY001|fallback
    reason|DocumentSnapshot.*trace|grammar author" restart/MASTER-PLAN.md`.
40. Dependency: §5 R8 and §6 A3.

41. Surgery S9: bibliography provenance cleanup.
42. Target file:line: `restart/research/INDEX.md:129-130`.
43. Directive: Phase 2 librarian either supplies primary URLs for Ungar/Adams
    1994 and HelpMate 2003 or annotates them as optional provenance gaps.
44. Acceptance gate: `rg -n "Ungar|HelpMate|provenance gap|primary URL"
    restart/research/INDEX.md restart/research/topic-7-green-red-incremental.md`.
45. Dependency: §6 A5.

46. Routed residue.
47. No architecture lock is overturned. Lock 1 survives with ownership wording.
48. Lock 14 survives; yaml syntax errors strengthen the two-surface proof by
    requiring recovery facts to emerge from grammar plus metadata.
49. Phase 2 should route implementation gates to Tranche I, with README/PASS-3
    wording folded before code starts.
