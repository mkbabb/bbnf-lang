# Scoped Lock 14 Allowance History

- `sk-v9-real-typed-w1` remains a historical, scoped allowance for the
  Apache/CITM measured typed row-table admission only. It does not authorize
  grammar, runtime, codegen, SIMD, fixture, direct-output, generated
  typed-output, or generic-crate behavior changes.
- SK-V12 supersedes the allowance surface for generated non-JSON evidence:
  `css_l4/declaration_values/direct_to_struct/main` is admitted as a
  same-plane fact-stream row, with `lock14=pass:lock14_baseline::validate`,
  but it is not full CSS parity, universal grammar closure, or a generic-crate
  exception. Evidence: `skinny/RESULTS.md:94`, `skinny/REDRESS.md:3824`-`3840`.
- All new generated non-JSON allowances are governed by Lock 14's generated
  output rule below and remain G-Omega/Pass Omega edits, not T-P3 direct edits.

# Hardening pass — plan set

You are auditing a freshly drafted plan set for the bbnf-lang BA-restart. The plan composes BA / BB / BC (and optionally BD+) tranches that re-architect the parser fleet from first principles toward grammar-agnostic, multi-backend, sonic-class direct-to-struct parsing.

The plan exists. Your task is to challenge and harden it: ratify what is sound, surface what is unsound, identify what is missing, and recommend what must change before execution begins.

You are not implementing. You are auditing.

## Subjects

The plan documents under audit:

- `docs/tranches/BA/BA.md` — the restart tranche
- `docs/tranches/BA/waves/W*.md` — wave-level specifications
- `docs/tranches/BB/BB.md` — successor tranche
- `docs/tranches/BC/BC.md` — successor-of-successor tranche
- Optionally: `docs/tranches/BD/BD.md` if drafted (TS/WASM emergence)

Read each end-to-end before producing any audit output.

Read also for context (do not audit, but use as ground truth):

- `audit/HARDENING-SYNTHESIS-2026-05-03.md` — codebase audit synthesis from the prior pass
- `audit/SOTA-2026-05-03.md` — sonic-rs / simdjson / lightning-css research
- `audit/CENSUS-2026-05-03.md` — kill-list of grammar-specific code, tape residue, dupes, god modules
- `audit/MODULES-2026-05-03.md` — per-file fates and 17-step pipeline ordering
- `audit/RESTART-SKETCH-2026-05-03.md` — JSON parse trace + post-restart pipeline sketch
- `docs/HARDENING-AUDIT-PROMPT.md` — codebase-audit prompt (for methodological symmetry)

## CH7 Overfit-Prune lens binding

Per `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62`-`87`, every plan + every
REDRESS entry + every audit + every hardening cycle at every CHALLENGE phase runs
CH1-CH7, not CH1-CH6. CH7 (Overfit-Prune) is a first-class lens with the same
blocking authority as CH1 (Correctness). A CH7 REJECT triggers (a) immediate plan
revise for plan artefacts, OR (b) immediate redress revert with a new REDRESS
entry for implementation artefacts. CH7 cannot be carried as
"acknowledged but not blocking".

The CH7 lens scans for: fabricated baselines; cited-but-absent surface text;
counter-surface fabrication (asserting prose into a document that does not
contain it; meta-CH7 collision pattern per
`restart/audit/totality/p1/1F-coherence-scan.md:64,83,100,109,117` COH-012);
SK-V14 cohort 32:69 = 31.7% refutation density preservation; anti-paper-close
anchor enumeration. Authority: T-P1 V5 §6.1 disposition (carrier: in-preface
clause, NOT Lock 17 — preserves the 16-lock count per
`restart/prompts/totality/PASS-3-SYNTHESIS.md:210`).

Evidence: `restart/audit/totality/p1/hardening/V1/CH7.md:64`,
`restart/audit/totality/p1/hardening/V1/CH7.md:180`-`181`,
`restart/audit/totality/p1/hardening/V1/CH7.md:208`,
`restart/audit/totality/p1/hardening/V1/CH7.md:218`,
`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:604`-`619`,
`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:48` (CH7
3-cycle LOCK in T-P2 cohort).

## Gestalt — sixteen locks

The plan must reflect these sixteen architectural commitments faithfully. Any wave that violates one is a fault. Locks 1–14 are the original architectural commitments; Locks 15 and 16 land 2026-05-12 after the V9.2 lazy-tape refutation and the six-agent comparative-profile cohort (see `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md` for the empirical synthesis).

1. **Tape is the substrate, properly unioned with direct-to-struct; columnar SoA is dead; orthogonal codepaths and parallel substrates are dead.** Tape is the greenfield's parsed event projection, unioned with direct-to-struct typed values that borrow into it (`&'i Tape<'i>` + cursor) when a retained document exists. The projection may be an offset tape, event tape, or collapsed-stage event sink; direct-only `SinkOnly` retains no queryable document identity. The 2,000-commit prior failure was implementation, not concept: orthogonal codepaths (the Vec<OpenFrame>::clone parallel substrate that produced the 86.07% samply pathology); type ambivalence (tape and OpenFrame and direct-to-struct competing for the same role); substrate-first/consumer-later (Era V failure mode); columnar SoA designed in AV.04 archaeology but never activated. The greenfield's tape lives at `runtime/src/tape/`; typed-value records borrow into it; per-grammar runtime modules (template-emitted at `runtime/src/grammars/<name>/`) emit accessors; one materialisation surface; one Visitor pattern; no parallel substrate. A SIMD mask stream is a transient producer, not a retained sidecar; if structural offsets are retained, the structural projection IS the tape. Columnar SoA stays buried. Plans that resurrect parallel substrates (OpenFrame ladders; columnar SoA; type-ambivalent dual representations) or implement tape with consumer-later sequencing are faults; plans that implement tape properly with same-wave consumer wiring + direct-to-struct union are honoured. **2026-05-04 reframe**: the prior restart's wholesale retirement of the tape name was an over-correction against the implementation failure; the user has confirmed tape is the right substrate when implemented properly. Lock 1's spirit (no parallel substrate; no orthogonal codepath; no Vec<OpenFrame>::clone pathology) holds; the no-rename clause is amended.

    **2026-05-21 v+1 substrate-ceiling fold**: Skinny Track 2 remains a
    substrate-ceiling probe, not a second substrate. Track 2 measures whether
    the same `runtime::tape` + `bbnf-simd` APIs can reach the SOTA envelope
    when hand-coded against the APIs codegen will emit; it does not authorize
    hidden runtime identity, parser-owned sidecars, or a parallel representation.
    Evidence: `restart/skinny/BENCH.md:71`-`107`,
    `restart/skinny/BENCH.md:121`-`136`,
    `restart/audit/totality/p1/1C-runtime-evidence.md:91`.

    Lazy-offset tape with sparse flags is admitted as scoped JSON evidence
    under this tape/direct union. It proves an offset-tape/direct shape can be
    viable for the measured JSON lane, not that Lock 1 is universally closed
    for every grammar or backend. Evidence: `skinny/REDRESS.md:246`-`256`,
    `skinny/RESULTS.md:98`-`144`,
    `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:45`-`46`.

    Fact streams are output-plane contracts, not retained internal sidecars.
    A generated fact stream such as `css_l4_declaration_value_fact_stream` may
    be admitted only with strict comparator/oracle provenance and gate-consumed
    telemetry; it does not by itself close a retained runtime substrate claim.
    Evidence: `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:96`-`98`,
    `skinny/RESULTS.md:94`, `restart/audit/totality/p1/1A-substrate-evidence.md:45`-`46`.

    **v+1 FactStream 5th substrate category (LAC-1E-14)**: `FactStream` is the
    5th admitted-product category at the Lock 1 substrate manifest, alongside
    `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage`. A fact-stream
    row carries `substrate_target = admitted_fact_output` per the manifest
    vocabulary below; comparator/oracle provenance and gate-consumed telemetry
    remain mandatory per the fact-stream paragraph above. The 5th category is a
    substrate-manifest classification only; it is NOT a 6th `BackendShape`
    variant. The 5-shape `BackendShape` search domain at Lock 10 holds:
    `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`. Adding a
    6th `BackendShape` variant remains G-Omega gated per Lock 10 v+1 and PASS-3
    §8.1. This amendment resolves the CSS L4 declaration-values
    substrate-classification gap surfaced at
    `restart/audit/totality/p1/1C-runtime-evidence.md:102` (1C-D5) and
    `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH2.md:87` (F2
    zero-profile-evidence carry). Evidence:
    `restart/audit/totality/p1/1E-locks-evidence.md:124`,
    `skinny/RESULTS.md:94`.

    Every e-graph candidate, backend rewrite, imported scanner plan, union
    candidate, and SIMD consumer must declare `substrate_target`,
    `retention_lifetime`, and `policy_owner`. Allowed targets are
    `local_temp_only`, `existing_tape`, `direct_sink`, and
    `admitted_fact_output`; allowed lifetimes are `local_loop`,
    `generated_function`, and `output_row`; allowed owners are
    `generated_grammar`, `caller_data`, and `none`. Any retained class/mask
    stream, parser-owned cursor/list state, public substrate API, `UnionTape`,
    or second tape is rejected unless G-Omega explicitly amends Lock 1.
    Evidence: `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:77`-`98`.

    REDRESS 96/97/98 are binding substrate-ceiling history. Full class-column
    vectors, streaming structural cursors, class-lane-only replays,
    parser-owned sidecars, and `UnionTape`-style retained structures are not
    shortlist-safe without a fresh material differential, scalar/checkasm or
    equality proof, same-wave consumer, strict row gate, rollback path, and
    abrogate threshold. Evidence: `skinny/REDRESS.md:2910`-`2940`,
    `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:120`-`137`.

    **2026-05-23 v+1 substrate-union ELEVATION (LAC-2F-V5-02; T-P3 §3C
    amendment surface)**: no cross-call retained classifier state. Period.
    Quote-mask, escape-mask, structural-mask, class-stream, prev-state byte,
    prefix-XOR carry word, or any prefix carry of any kind — none is admissible
    under Lock 1 substrate-union. Carry MUST stay within a single chunk-call
    boundary. The closure of REDRESS 96 / 97 / 98 (retained class-column /
    streaming structural cursor / class-lane-only on M5 Max) generalises to ALL
    transient classifier-state primitives, not just the three falsified shapes.
    Every Layer 1 primitive declares
    `retention_lifetime ∈ {transient-single-call, retained-within-chunk,
    retained-across-call-boundary}`; the third value is the REJECT class under
    Lock 1 v+1. Any future SIMD primitive that proposes cross-call
    classifier-state retention is REJECT under Lock 1 v+1 without further
    measurement. The per-call composed form (e.g. Gap PTG-PREV-IN-STRING-LOCK1,
    `scan_string_with_carry_64`) is the admissible substrate-union-compatible
    primitive; the per-call SIMD ceiling sits structurally below simdjson's
    published 1 GB/s by construction. Evidence:
    `restart/audit/totality/p2/2F-parse-that-gaps.md:519`,
    `restart/audit/totality/p2/2F-parse-that-gaps.md:490`,
    `restart/audit/totality/p2/2B-primitive-vocabulary.md:233`-`306`,
    `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:182`-`192`
    (cohort §4 row 4 STRONGEST AMENDMENT SURFACE).

2. **Layout lowering is the canonical IR pass name**. The term replaces *type projection / type collapsing / type inference / type elaboration / TypeMap / StructLayout / TypeDesc / schema synthesis* everywhere. Old terms appear only in archived docs. The IR module is `passes::layout` (canonical unprefixed path under the `passes` crate per the README's workspace shape); the IR record is `Layout`; the trait that consumes it is `LayoutSink`. HM/CSP type checking is a subroutine of layout lowering, never a public peer pass; `LayoutFacts` is the public side-table. Any plan section referring to a retired term, to the stale `bbnf-ir/` prefix, or to `TypeFacts` as a public artefact is a fault.

    **v+1 live-state clarification**: `LayoutFacts.backend_shape` is the live
    side-table evidence today; `Layout` and `LayoutSink` remain V1 public
    API-freeze obligations unless Pass Omega removes those names from this
    lock. A wave may not claim Lock 2 closure by pointing only to
    `LayoutFacts` while the public `Layout` / `LayoutSink` names remain absent.
    Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:64`,
    `restart/audit/totality/p1/1B-codegen-evidence.md:37`.

3. **Cursor-parse + byte-skip unified, with cursor branch elided when path is empty**. One parse implementation. Cursor consultation generates byte-skip when consult returns `Skip`. The empty-path case (`__EAGER_EMPTY_PATH`) elides cursor calls entirely so the eager fast path pays no consultation cost. Any plan that bifurcates byte-skip and cursor-parse into two implementations is a fault.

    **v+1 verification clause**: empty-path elision is not closed until a
    generated-code unit/golden test proves the empty path emits no cursor calls
    or equivalent consult symbols. Absence claims without captured command
    output remain UNKNOWN verification actions, not lock closure.
    Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:65`,
    `restart/audit/totality/p1/1E-locks-evidence.md:125`.

4. **Per-domain orthogonal optimization**. CSP type/layout inference, e-graph rewriting, pattern miners, shape analysis, and cost model compose by output-piping. No unified hypergraph. Each lives in its own crate (egraph + csp-solver path-deps until stable). Egglog-style Datalog/equality-saturation fusion is a known SOTA pressure, not an omitted option; V1 rejects that fusion because diagnostics, public proof records, monotone bridge boundaries, and independent stabilization gates must stay owned by the domain that produced them. Fusion remains a post-V1 research comparison, not the governing architecture. Any plan that fuses CSP and e-graph into one solver is a fault. **V1 type system folds higher-rank polymorphism via DK13 algorithmic completeness (Dunfield-Krishnaswami 2013); GADT user-facing surface lands V1: pattern-match arms admit branch-local-equality refinements (`Pattern @ where T = U -> Block` per the §6 BBNF grammar amendment); OutsideIn(X)-style implication constraints solved at `passes/types/` carry the equalities through to `LayoutFacts`; the user-facing diagnostic `BBNF-LOCAL-EQUALITY-ANNOTATION` is emitted when a match-arm refinement annotation is missing or ill-typed. Closures capture by `&'i` reference only; capture-by-move is forbidden in V1; `Fn` / `FnMut` / `FnOnce` discrimination is not exposed at the BBNF surface in V1.**

5. **IR + per-backend lower**. Codegen emits a backend-agnostic typed IR; per-backend lowerers produce native source. There is no source-emit-per-backend duplication; there is no trait-based emitter walking grammar directly. The IR is the contract. Any plan whose Rust codegen and TS codegen do not share an IR is a fault. **TS and WASM backends defer post-V1; V1 ships the Rust impl only via the formal `Backend` trait at `restart/ARCHITECTURE.md` §7.5. The trait enforces this lock's per-backend boundary and enables seamless V2 addition of `WasmBackend` and `TsBackend` without re-architecting BIR or codegen.**

6. **xtask emits committed source artefacts**. No proc-macro façade. css_l4.rs at 107 K LOC is greppable on disk. Build is fast incremental because expansion is not at compile time. Any plan that proposes proc-macro for codegen output (other than the `path` / `path-ts` proc-macro shells, which are different) is a fault. **The egraph crate has no direct dependency on csp-solver; the bridge surface lives at `passes::bridge` and is invoked by passes that compose both crates. egraph and csp-solver compose by output-piping, never by import — Lock 4's per-domain orthogonality holds at the dependency-graph level as well as the algorithmic level.**

    **v+1 regen round-trip discipline (LAC-1E-13)**: every file carrying
    `// @generated by skinny bbnf-codegen` (or equivalent rostered header) MUST
    (a) trace to a rostered xtask emission (`cargo xtask regen-{grammar}`); (b)
    emit byte-equivalent output when regenerated from grammar source + workspace
    metadata; (c) reject hand-patching per memory `[clean-regen-discipline]`.
    The round-trip clean check distinguishes real codegen from fake `@generated`
    `include_str!` templates (the SK-V13 W1b CSS L4 pathology). R4 CSS L4 is
    the first instance; the family extends to JSON / Sheets / BBNF / EBNF / BNF
    / CSV / Math. Evidence:
    `restart/skinny/tranches/sk-v14/SYNTHESIS.md:96` R4,
    `restart/skinny/tranches/sk-v14/SYNTHESIS.md:110`-`120` P-1 fake @generated
    recurrence vector,
    `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md:153,184`
    (8 fake-codegen providers in `skinny/crates/codegen/src/`).

7. **`crates/path/` is the consolidated path crate**. The runtime cursor engine merges INTO it; the existing `crates/core/src/path/` directory empties. The Rust `pointer!` proc-macro lives here. **`crates/path-core/` (non-proc-macro) exists as a published sibling crate at V1 J.W3, alongside `crates/path/` (the runtime + Rust proc-macro shell) and `crates/parse-that/` (the parser combinator + regex family) — `path-core` is the sole deduplication mechanism for the path-AST + compile logic. `crates/path-ts/` defers post-V1 alongside the TS-native parse+runtime fork.** Any plan that names `crates/bbnf-path/` (with prefix) is stale; any plan with three proc-macro shells is a fault.

8. **Surpass sonic-rs / simdjson / lightning-css**. AU is never mentioned. Every perf gate names a specific competitor's number on a specific dataset on a specific platform. simdjson On-Demand 7 GB/s (JSON parse). sonic-rs M1 Pro twitter 436 µs (parse-to-typed-struct). lightning-css 4.16 ms Bootstrap (CSS). Plans that reference AU's bench numbers are stale. **V1 SOTA close gates measure the Rust-line only at H.W3 and H.W4; WASM SOTA defers post-V1. The H tranche carries five waves (H.W0-H.W4) after the V1-FOLD-CANDIDATES Tier 4 wave-count drop. No measurement-pending WASM anchor lands in V1; the WASM lower-and-bench programme awaits the V2 `WasmBackend: Backend` impl.**

    **v+1 row-plane accounting**: SOTA is row-plane specific. JSON
    `parse_only`, `direct_to_struct`, and `real_typed_struct` rows are separate
    gates with same-plane strict comparators; CSS L4 declaration-values is a
    SK-V12 `PASS-ADMIT` row on `css_l4_declaration_value_fact_stream`, not full
    CSS parity, not universal grammar closure, and not SK-V13 close authority.
    Evidence: `skinny/RESULTS.md:5`-`35`, `skinny/RESULTS.md:94`,
    `restart/skinny/tranches/sk-v13/SYNTHESIS.md:38`-`57`,
    `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95`-`110`.

    **v+1 audit-overlay column binding (LAC-1E-16)**: every gate-consumed
    `skinny/RESULTS.md` row carries four required schema columns:
    `track2_entry_point`, `comparator_plane`, `per_iter_equality`,
    `audit_overlay_verdict`. `xtask gate-json` REJECTS any row missing any of
    the four — an admitted row missing a required column is no admit at all.
    Falsifiability gate companion to the row-plane accounting above. Evidence:
    `restart/skinny/tranches/sk-v14/SYNTHESIS.md:240`-`255`,
    `restart/skinny/tranches/sk-v14/SYNTHESIS.md:230`,
    `restart/audit/totality/p1/1E-locks-evidence.md:126`.

    **v+1 numeric abrogate-gate binding (T2A-LAC-V1-05; V3 F-V3-CH4-B
    numeric-bound at `restart/audit/totality/p2/2D-cost-model.md:151`-`162`)**:
    every gate-consumed comparator + bench run carries the 6 abrogate-gate
    numerics from T2A-LAC-V1-05: e-graph saturation ≤50000 nodes / ≤10000
    classes / ≤30 iter; CSP timeout ≤1 s/grammar; stale-cost ≤30%;
    generated-LOC growth bound to `loc_budget`; row regression admit;
    parity/checkasm failure. Any abrogate-gate trip rejects the wave; numbers
    are uniform across cohort dossiers 2A:192 + 2C:303-305 + 2D:142-149.
    Evidence:
    `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:141`-`145`
    (F-V3-CH4-B numeric-bind).

    Comparator-plane provenance is mandatory. Same-run Rust strict sonic rows
    can anchor current JSON gates; simdjson, yyjson, asmjson, lightningcss, and
    other sidecars are strict anchors only when the same corpus, output plane,
    host, strictness, freshness, sidecar status, and gate-consumed artifact
    provenance match the candidate row. Historical, absent, permissive,
    x86-only, or different-plane rows are architecture pressure or comparator
    notes, not gate anchors. Evidence: `restart/skinny/BENCH.md:678`-`684`,
    `skinny/RESULTS.md:149`.

    Non-JSON telemetry must feed the bench gate, not prose. A non-JSON row may
    enter `skinny/RESULTS.md` only through the `BENCH.md` Section 8 post-bench
    gate shape or a dedicated companion report consumed by that gate family,
    with JSON guard proof when JSON rows can be affected. The legacy JSON
    `gate --check-results` renderer alone is insufficient for an appended
    non-JSON row. Evidence: `restart/skinny/BENCH.md:1498`-`1512`,
    `restart/skinny/BENCH.md:1534`-`1545`,
    `skinny/REDRESS.md:3836`-`3840`.

    Direct digest hashing is a semantic-output contract. Byte-hash or SIMD
    sub-hash acceleration is admissible only when Track 1, Track 2, serde, and
    sonic strict equality hold for the same semantic output plane and no prior
    A/GO guard silently demotes. Evidence:
    `restart/audit/totality/p2/2F-parse-that-gaps.md:252`,
    `restart/audit/totality/p1/1D-skinny-lessons.md:107`.

9. **Slice-borrow primary; bumpalo + owned escape hatches**. Default API is `&'i str` slices + `Cow<'i, str>` for transformations (lightning-css model). Bumpalo arena is opt-in via `parse_in(input, &bump)` (sonic-rs model). Owned (no-borrow) is opt-in via `parse_owned(input)` (serde-json escape). The three are surfaces over the same parse implementation; the lifetime parameter is the discriminant. Any plan that allocates eagerly into bumpalo without justifying why borrowing won't suffice is a fault.

    **v+1 skinny-scope clarification**: the skinny facade does not prove the
    full Lock 9 surface. `parse_in(input, &bump)` and true generated owned
    documents remain V1 runtime obligations until runtime API tests prove the
    bump and owned surfaces share the same parse implementation and lifetime
    discipline. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:71`,
    `restart/audit/totality/p1/1E-locks-evidence.md:104`.

10. **Pratt + SIMD auto-detected; materialization plan also auto-detected via cost model**. No `@pratt` or `@simd` directives. Optimizer mines grammar shape (left-recursive operator chains → Pratt) and leaf-pattern shape (charclass / keyword set / regex → SIMD scanner) and emits accordingly. Cost model decides when SIMD overhead is worth the dispatch cost AND derives `LayoutFacts.backend_shape ∈ {EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` per-rule from existing Grammar IR facts (first-set disjointness, output mode, transitive `@error(recover)` use, `@host fn` decoded-at-parse, `@layout` scope, target features) per ARCHITECTURE.md §7.3's 8-priority decision tree. No new BBNF directive carries the materialization plan; `backend_shape` is a side-table field, not a surface annotation. Any plan that requires grammar authors to annotate Pratt, SIMD, or materialization plan is a fault. **The V1 BBNF grammar formalises six directives: `Directive = ImportDecl | HostFn | ErrorDecl | LayoutDecl | PrettyDecl | TokenDecl ;` — replacing the prior three-directive form `Directive = HostFn | ErrorDecl | LayoutDecl ;`. `@import` carries cross-file grammar composition; `@host fn` carries typed host primitives; `@error(recover = ...)` carries recovery vocabulary; `@layout` carries layout policies (with `@ws` folded into `@layout(ws = ...)`); `@pretty` carries pretty-printing strategy with the verbatim vocabulary `compact`, `group`, `indent`, `hardbreak`, `sep(...)`, and `block` preserved from the 30+ extant grammar sites; `@token` carries atomic-token markers binding to the BIR scanner. `@debug` is a host primitive, not a directive. Standalone `@recover` retires; absorbed by `@error(recover = ...)`. `@pratt`, `@simd`, `@transducer`, `@rewrite`, and `@unicode` retire. **BBNF V1 also includes function values + lambda literals (`|x| body`) + closure capture by `&'i`; function types `fn(T) -> U` are first-class members of the `Type` non-terminal.**

    **v+1 decision-engine and cost-evidence clause**: the five
    `BackendShape` variants remain the V1 search domain. A new
    `BackendShape`, new directive, or new BIR variant is not admitted by cost
    evidence and remains G-Omega gated. The current P1-P8 cascade and thin
    `CostFacts` are diagnostics or compatibility evidence only until the
    resolver generates backend-plan candidates, consumes generated grammar
    facts, filters infeasible plans, records selected and rejected alternatives,
    and extracts with active cost evidence. Evidence:
    `restart/audit/totality/p1/1B-codegen-evidence.md:36`-`39`,
    `restart/audit/totality/p2/2D-cost-model.md:188`-`190`.

    Regex/HIR facts are required where regex or scanner plans influence
    backend-shape or scanner selection. Opaque pattern strings alone cannot
    justify SIMD, scanner-plan import, or backend-shape admission. Exact fact
    schema belongs in `ARCHITECTURE.md`, but Lock 10 treats stale/static
    fallback and opaque-string-only selection as non-admitting evidence.
    Evidence: `restart/audit/totality/p2/2F-parse-that-gaps.md:251`,
    `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:79`.

    Decision-engine and SIMD/substrate candidates fail closed on e-graph cap,
    CSP timeout, stale cost evidence over 30 percent, generated LOC overrun,
    admitted-row regression, or any scalar/checkasm/equality failure.
    Evidence: `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:103`-`115`.

    **v+1 cohort-wide `BBNF_SIMD_STRICT=1` precondition (V3 F-V3-CH4-A)**:
    SIMD admissibility under Lock 10 requires `BBNF_SIMD_STRICT=1` cohort-wide,
    not merely per-primitive. Institutionalized at three load-bearing cohort
    sites with mutual cross-references:
    `restart/audit/totality/p2/2A-sota-landscape.md:192`,
    `restart/audit/totality/p2/2C-grammar-neutrality.md:303`-`305`,
    `restart/audit/totality/p2/2D-cost-model.md:142`-`149`. Non-strict parity is
    exploratory only and cannot admit a primitive, route, or row at the
    decision-engine layer (per Lock 16 v+1 admission checkasm rule). Evidence:
    `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:74`
    (F-V3-CH4-A discharge).

    **v+1 regex/HIR fact mandate (LAC-2F-V5-04; strengthens V4 LAC-2F-03)**:
    regex/HIR facts are MANDATORY inputs to CSP/egraph/cost selection. Opaque
    pattern strings of the form `SinkOnlyExpr::RegexProgram { pattern: String }`
    (`crates/codegen/src/lower/sink_only.rs:19`-`93`) are insufficient for
    backend-shape or scanner selection. The decision-engine consumes
    regex-engine HIR facts (state count, lazy-DFA viability, NFA branching,
    Aho-Corasick eligibility) from the absorption-wave-resolved `bbnf-regex`
    crate (LAC-2F-V5-01 Q1 + SK-V14 W11 absorption). Opaque-pattern-only
    selection is non-admitting at the cost-model layer. Evidence:
    `restart/audit/totality/p2/2F-parse-that-gaps.md:521`,
    `restart/audit/totality/p2/2D-cost-model.md:120` (T2D-REGEX-NFA-DFA-PLAN).

11. **Path-deps for incubating sister crates**. egraph + egraph-derive + csp-solver + parse-that (with `parse-that-regex` as its regex sub-crate) as path-deps in workspace until each API stabilises; promote to registry once stable. bbnf-simd + bootstrap + analysis + lsp stay workspace-internal. ser + gorgeous archive at `archive/<crate>/`, removed from workspace, source preserved. **`parse-that` is the canonical name for the published parser combinator + regex family; the legacy `bbnf-regex` crate renames to `parse-that-regex` and publishes as such. The `crates/bbnf-regex/` directory rename to `crates/parse-that-regex/` is a follow-up commit; documentation uses the new name now.**

    **Lock 11 v+1 workspace verification**: root legacy workspace drift is not
    skinny truth. A.W0/A.W1 closure requires `cargo metadata` or equivalent
    artifact proof that `ser`, `gorgeous`, `simd-scan`, `bbnf-path`, and
    `bbnf-path-ts` match the archive/rename/removal state this lock names.
    Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:73`,
    `restart/audit/totality/p1/1E-locks-evidence.md:105`.

12. **ser + gorgeous archive BEFORE A.W0**. Clean slate is the precondition for the A tranche to begin. Any plan that interleaves the archive ceremony with A waves is a fault. **The archive ceremony cites `pre-restart-2026-05-04` as the source-of-truth tag; legacy `BA-` / `BB-` / `BC-` / `BD-` slot drift retires under the canonical `A-` / `B-` / `C-` / `D-` tranche letters.**

    **Lock 12 v+1 archive verification**: the archive ceremony remains a hard
    precondition; root workspace membership after archive must be proven by a
    committed metadata transcript or Pass Omega-equivalent evidence, not by
    stale prose. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:74`,
    `restart/audit/totality/p1/1E-locks-evidence.md:105`.

13. **No god directories; cohesive encapsulation at every level**. Every directory partitions one cohesive concern; siblings are peer partitions of that concern; sub-modules express finer partitions. Per-level surface APIs are uniform across siblings. The standard is set by sonic-rs (`src/{parser, value, serde, util, lazyvalue, ...}`), lightning-css (`src/{rules, properties, selector, declaration, traits, ...}`), and simdjson (`{dom, ondemand, generic, ...}`): each top-level directory names a concern, expresses it through 4–10 children at the next level, and each child carries a uniform sub-API (e.g., every property module exports `Property` enum + `parse` + `print` + `Visit` impls). bbnf must match this discipline. A 16-sibling directory mixing per-grammar subdirs with generic mechanism files (e.g., today's `crates/core/src/runtime/`) is a god directory and is a fault. Files >500 LOC outside `generated/` are forbidden; directories with >10 immediate children mixing concerns are forbidden; sibling-API divergence (one module exports `parse` + `emit`, the next exports `compile` + `walk`) is forbidden.

    **v+1 exception discipline**: generated files are exempt only when they are
    rostered generated artifacts with per-wave generated-LOC budgets and
    regeneration checks. Bench/report/gate files may exceed 500 LOC only under
    an explicit gate-surface budget and committed LOC transcript. The 500 LOC
    ceiling remains binding for non-generated production modules; directory
    fanout is a violation only when the inventory proves mixed concerns, not
    merely many cohesive ISA/test partitions. Evidence:
    `skinny/REDRESS.md:299`-`312`,
    `restart/audit/totality/p1/1F-anti-pattern.md:31`-`32`,
    `restart/audit/totality/p1/1E-locks-evidence.md:106`.

14. **Full grammar generalisation; zero overfitting**. The substrate carries ZERO grammar-specific code. Every grammar plugs into the fleet via three declarative surfaces only: (a) a grammar source file (`<name>.bbnf`), (b) workspace metadata declaring its strategy (recognisers, host fns, output-dir, pratt eligibility, simd eligibility, etc., per Lock 5's IR contract), and (c) optionally a per-grammar declaration crate (`crates/<grammar>/`) carrying host-fn implementations. Generic crates — `bbnf-parse`, `bbnf-codegen`, `bbnf-runtime`, `bbnf-ir`, `path`, `path-core`, `egraph`, `csp-solver`, `parse-that-regex`, `parse-that`, `bbnf-simd`, `analysis`, `lsp` — carry ZERO `match grammar { Json => ..., CssL4 => ..., ... }` arms; ZERO grammar-named modules; ZERO grammar-specific types in their public APIs; ZERO per-grammar feature flags. Per-grammar runtime modules (value, document, view, kind) are emitted from a single grammar-agnostic generator template that consumes (grammar source + workspace metadata) and produces typed Rust; hand-written per-grammar runtime files are forbidden. Per-grammar deviations (CSS L4 colour-function emit; BBNF Pratt operators; Sheets array literals) are encoded in the grammar metadata + source, NOT in branching code in any other crate. Adding a new grammar is a config + grammar-source change with NO code change in any generic or other-grammar crate. The current overfitting mess — CSS L4 14-variant `OpenFrame`; BBNF aggregator `pub use bbnf::*`; Sheets arena fallbacks; per-grammar registry arms in `bbnf-ir`; `shape_dict_bbnf.rs`; `crates/core/src/css_types.rs`; per-grammar runtime/<g>/ hand-written modules — is the failure mode this lock prevents from recurring. Any plan, tranche, wave, or commit that introduces grammar-specific code in a generic crate, or any new hand-written per-grammar runtime file, is a fault regardless of its other merits. Verification commands: `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,parse-that-regex,parse-that,bbnf-simd,analysis,lsp}/src/` returns ZERO; `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns ZERO per-grammar dirs (all generated from template); `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' crates/` returns ZERO matches in non-generated source under generic-crate paths.

    **v+1 generated-output allowance**: generated files under
    `runtime/src/grammars/<name>/` may contain grammar names only when emitted
    from the rostered generator using grammar source plus workspace metadata.
    This allowance does not extend to hand-coded provider enums, root aliases,
    generic-crate grammar branches, grammar-named public types in generic APIs,
    tests/proof fixtures routed through generic roots, or grammar-shaped policy
    mining. Evidence: `restart/audit/totality/p2/2C-grammar-neutrality.md:184`,
    `restart/audit/totality/p1/1C-runtime-evidence.md:79`-`85`.

    The generated-output allowance is bound to the Lock 6 v+1 regen round-trip
    clean check (LAC-1E-13). A file under `runtime/src/grammars/<name>/`
    carrying `// @generated` survives Lock 14 only when `cargo xtask
    regen-{grammar}` produces byte-equivalent output from grammar source +
    workspace metadata; hand-patched generated files are Lock 14 violations.
    Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:123`,
    `restart/skinny/tranches/sk-v14/SYNTHESIS.md:96`.

    Generic crates consume generated provider manifests, generated
    sink/fact/value/flag surfaces, and generated grammar facts. They may not
    hand-code `RuntimeProvider::{Json, CssL4DeclarationValues}`, JSON/CSS
    renderer branches, JSON punctuation alphabets, object/array/pair/string/
    number/bool/null role mining, hardcoded sink callback names, or
    grammar-specific feature flags. Evidence:
    `restart/audit/totality/p1/1B-codegen-evidence.md:58`-`60`,
    `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:57`-`65`.

    **Per-wave gate enforcement**: any wave touching generic crates, generated
    provider manifests, primitive policy manifests, runtime roots, codegen
    templates, decision-engine facts, or shared `bbnf-simd` consumers must run
    a Lock 14 baseline gate plus a grammar-name and grammar-shape leak census
    in the same wave. At minimum, the gate checks generated provider registry,
    grammar-shape role mining, generated sink/fact/value/flag ownership,
    primitive policy source, one strict CSS L4 positive row, both Sheets and
    BBNF-self fail-closed negative-control witnesses or admitted generated-role
    fact rows when claiming fleet-wide transfer, and decision-engine generated
    facts. With only one of Sheets or BBNF-self, the claim is scoped to the
    witnessed grammars and may not use fleet-wide grammar-neutral wording.
    Evidence:
    `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:53`-`75`,
    `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:47`-`51`.

    Shared `bbnf-simd`, parse-that, and future regex APIs expose
    grammar-neutral facts and primitives only. Quote, escape, control,
    delimiter, number, string, and no-string/no-number policy must come from
    generated grammar config or caller data, not hardcoded JSON/CSS constants.
    A primitive claimed grammar-neutral must exercise at least one non-JSON
    consumer or record a measured deletion/rejection. Evidence:
    `restart/skinny/tranches/sk-v13/SYNTHESIS.md:226`-`230`,
    `restart/audit/totality/p2/2C-grammar-neutrality.md:188`,
    `restart/audit/totality/p2/2F-parse-that-gaps.md:249`.

    **v+1 Pattern H per-tranche census (LAC-1E-15)**: every tranche commits a
    Pattern H file-count transcript via `find crates/core/src/runtime
    -mindepth 2 -type f -name '*.rs' \| wc -l` (and the skinny mirror
    equivalent). The bound command MUST omit `-maxdepth 2` so the four files
    living at depth 3 under `google_sheets/document/{path_query.rs, mod.rs,
    canonical.rs, view.rs}` are counted; verified at HEAD `e12c5323d`: the
    corrected command returns 67 (the asserted Pattern H total per
    `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md:26`-`56`),
    while the `-mindepth 2 -maxdepth 2` form returns 63 and contradicts the
    asserted figure. Tranche +N over prior tranche MUST trace to (a) a
    grammar-roster change (e.g. css_pretty +7 from SK-V13 to SK-V14) OR (b) a
    sub-wave count update (e.g. PRUNE-4 9 sub-waves). Substrate templates at
    `crates/core/src/runtime/builder_template.rs:13`-`31` and
    `crates/core/src/runtime/arena_template.rs:1`-`31` MUST NOT enshrine
    hot-grammar opt-outs in doc-comments — the opt-out doc-comment passages
    are themselves Lock 14 violations under "any plan that introduces
    grammar-specific code in a generic crate is a fault" per the lock body
    above. Pattern H 67-file recurrence is the category-scale failure Lock 14
    was authored to prevent. Evidence:
    `restart/audit/totality/p1/1E-locks-evidence.md:125`,
    `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md:10`-`12`,
    `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md:41`-`56`,
    `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md:153`-`157`.

    **v+1 abstract-primitive sibling (LAC-2F-V5-03)**: `byte_class_from_range_64`
    (PTG-RANGE-CLASS-PRIMITIVE) is pinned as a sibling of
    `byte_class_from_eq_set_64` in the abstract-primitive declaration list.
    The two-primitive split (set ≤8 vs inclusive range) is the load-bearing
    grammar-neutral generalization vehicle for digit-run / UTF-8-continuation
    / CSS hex / BBNF identifier classification. Per memory feedback
    `[regex-generalized]`, the range primitive lives in `bbnf-simd`, not
    `bbnf-lang`. Evidence:
    `restart/audit/totality/p2/2F-parse-that-gaps.md:520`.

15. **Build-profile discipline + fusion discipline + i-cache residency: LTO + codegen-units = 1 + force-inline hot leaves + ~20 KiB hot-function ceiling**. Every bbnf-generated runtime crate declares `[profile.release] lto = "fat"` (verify via `cargo build --release -v 2>&1 | grep '\-C lto'` returns `lto=fat`, not `lto=thin`; per `skinny/profile/skinny-expanded/PROFILE-REPORT.md` the binary built with `lto=thin` regressed twitter throughput 11780→5521 Mbps — Lock 15 enforcement gap), `codegen-units = 1`, `panic = "abort"` (or `"unwind"` only if the grammar carries `@error(recover = ...)`), `debug = true` (per `feedback_samply_symbols` for samply-resolvable profiles). Three co-load-bearing dimensions, each falsifiable:
    - **LTO + codegen-units = 1**: sonic-rs's INLINED hot-leaf count is 1-2 because LTO fuses the entire SIMD inner kernel into `parse_object`/`parse_array`; without LTO sonic-rs's NOINLINE wall-clock falls 2.1-3.2× on M5 Max (`skinny/profile/sonic-rs-v2/PROFILE-REPORT.md`). The codegen template inversion yields only half its gain without LTO.
    - **Force-inline hot leaves**: yyjson achieves 0.91 c/B twitter (29.5K Mbps on M5 Max, beating simdjson DOM's 1.142 c/B and 24K Mbps) WITHOUT SIMD via `always_inline` everywhere and the entire parser fitting in ~18 KiB i-cache resident (`skinny/profile/yyjson/PROFILE-REPORT.md`). The codegen template emits `#[inline(always)]` on Grammar IR's call-graph hot path (rules with self-time + transitive-call self-time ≥ threshold per `LayoutFacts.hot_call_graph`, cost-model-derived from prior bench runs); the threshold is grammar-agnostic and not a directive. Diagnostic `BBNF-FORCE-INLINE-MISSED` fires when a rule mined as hot-path lacks `#[inline(always)]` in generated output.
    - **I-cache budget**: target hot-function size ≤ 20 KiB post-LTO. Diagnostic `BBNF-ICACHE-BUDGET-EXCEEDED` fires when `cargo asm` reports fused-function size exceeds budget (yyjson reference: ~18 KiB). **Budget already met for JSON** per Wave 2 Agent 3 evidence (`skinny/profile/wave2-asm/PROFILE-REPORT.md` Appendix C): `parse_value_at` is a single 7,304-byte hot function (RVA `0x2460..0x40e8`, 1,826 mnemonics) under the current workspace `[profile.release]` (`opt-level=3`, `lto=thin`, `codegen-units=1`, `debug=true`). Sub-budget; the open question is whether `lto=fat` enforcement lands the budget-overrun warning for other grammars at codegen time.
    - The discipline applies to (a) the bbnf-generated runtime, (b) the `bbnf-simd` primitive crate, (c) any user-side crate consuming the generated parser in throughput-sensitive contexts.
    - Workspace metadata `[workspace.metadata.bbnf.grammars.<name>.profile]` carries optional per-grammar overrides only with a documented measurement justification. Plans that ship release profiles without LTO are faults; verification: `cargo build --release -v 2>&1 | grep -E '\-C lto=(fat|true)' | wc -l` must equal the number of workspace members.

    **v+1 scope clarification**: skinny release profile evidence proves skinny
    enforcement only. Root workspace thin-LTO or profile drift remains a V1
    migration gap until the root release build proves `lto=fat`,
    `codegen-units=1`, panic policy, and debug-symbol requirements for every
    generated runtime and throughput-sensitive consumer. JSON `parse_value_at`
    i-cache evidence is scoped JSON evidence, not a blanket grammar closure.
    Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:77`,
    `skinny/REDRESS.md:258`-`264`,
    `restart/HANDOFF.md:132`-`134`.

16. **SIMD/ASM admissibility allowlist; handwritten ASM only for missing intrinsics; abstract primitive lifts from dav1d/ffmpeg/VLC**. Admissible SIMD primitives are an explicit allowlist with citations to published architectures (papers, blog posts with named techniques, or established library implementations). The allowlist for V1 (extensible only by appending; see `MASTER-PLAN.md` §4 admissible-SIMD-primitives table):
    - **arm64 NEON byte classify**: `vqtbl4q_u8` 4-table 64-byte lookup (Lemire 2019); `vqtbl1q_u8` 1-table (sonic-rs baseline).
    - **arm64 NEON movemask**: `vshrn_n_u16` + `vsriq_n_u8` + `vzip1q_u8` interleaved-vector pattern (Validark 2024).
    - **arm64 NEON loads + shifts**: `vld1q_u8_x4` quad-load (Arm A64 ISA); `vbslq_u8` branchless mask-select; `vextq_u8` 1D cross-lane byte-shift (Arm A64 ISA; dav1d filter-overlap lineage; **abstract primitive: cross-chunk byte-context propagation** — applies to ANY grammar with chunk-spanning tokens, not just JSON).
    - **arm64 NEON arithmetic**: `vcntq_u8` + `vaddvq_u8` byte popcount; `vqaddq_u8` / `vqsubq_u8` saturating add/sub (Arm A64 ISA; **abstract primitive: branchless overflow-clamped accumulation** — applies to ANY grammar's number primitive).
    - **arm64 NEON multiply-accumulate**: `udot` / `sdot` 4-byte dot-product (Arm A64 ISA Armv8.2-A; **abstract primitive: byte-window multiply-accumulate, lifted from dav1d's FIR filter** — applies to ANY grammar's digit-block decode, not just JSON: JSON `number`, CSS L4 `<number>`, TOML/INI/SQL integer literals, Sheets formulas).
    - **arm64 NEON LD4-interleaved 4-channel classifier** (NEW 2026-05-12, post-Wave-1-NEON-research): `vld4q_u8` + per-channel `vqtbl1q_u8` + `vshrn`/`vsri`/`zip1` movemask. Validark 2024 demonstrated ~10% drop in simdjson stage1 c/B on Apple arm64. **Abstract primitive: parallel-channel byte classification**; M5 Max-specific lever asmjson cannot use (AVX-512 only). Citation: validark.dev/posts/interleaved-vectors-on-arm/; simdjson PR #2333.
    - **arm64 NEON ternary bitwise** (NEW 2026-05-12; ARMv8.2-A SHA3 extension): `vbcaxq_u8` (Bit-Clear-And-XOR = `Vn EOR (Vm AND NOT Va)`) and `veor3q_u8` (3-way XOR). Equivalent to AVX-512 `vpternlogq` on arm64; collapses 2-op `bic + eor` into 1-op `bcax`. ~12-18% inner-loop reduction op-count. Available on every M-series (M1+) and Neoverse-V1/V2 (Graviton3/4). sonic-rs does NOT use these. Citation: Arm Architecture Reference Manual ARMv8.2-A SHA3.
    - **arm64 NEON set-membership (NEON port of SVE2 `svmatch_u8`)** (NEW 2026-05-12): `vceqq_u8` + `vorrq_u8` reduction tree for set membership against 16-byte alphabet. Portable equivalent of SVE2 `svmatch_u8` (Lemire 2026, Graviton4); same source ships on M5 Max NEON and dispatches to native MATCH on SVE2 hosts. Citation: Lemire 2026 "The fastest way to match characters on ARM processors".
    - **arm64 NEON cache hints**: `STNP` non-temporal pair-store (kernel `clear_page` lineage; tape-stream write); `PRFM PLDL2STRM` / `PLDL1KEEP` tuned prefetch (Arm A64 ISA; tape-walker prefetch ahead-of-cursor).
    - **x86_64 AVX-512 VBMI2**: `_mm512_mask_compressstoreu_epi8` (Lemire 2022; simdjson `icelake/simd.h:157` explicitly leaves unused for portability); `_mm512_ternarylogic_epi64` 3-input boolean (Sneller branchless-AVX-512); `vpermi2b` 128-byte byte-shuffle (simdjson icelake); `_mm512_alignr_epi8` cross-window carry.
    - **x86_64 AVX-512 k-mask arithmetic family** (NEW 2026-05-12, post-Wave-1-research): `_kandn_mask64`, `_kxor_mask64`, `_kxnor_mask64`, `_kshiftrq`, `_ktestq` (Travis Downs kreg-facts blog; AVX-512F base). Keep classifier masks in k0..k7 across state transitions; spill only on EOB. asmjson uses **only** `korq` + `kmovq` (~4 store+load eliminated per chunk). Citation: travisdowns.github.io/blog/2019/12/05/kreg-facts.html + 2020/05/26/kreg2.html.
    - **x86_64 AVX-512 VPCLMULQDQ at 512-bit lane** (NEW 2026-05-12; Ice Lake+ Intel, Zen 3+ AMD): adopt simdjson's prefix-XOR string-bitmap primitive at 4× width vs simdjson's 128-bit `_mm_clmulepi64_si128`. asmjson uses cmp+branch on backslash per byte (no prefix-XOR primitive); we add the primitive AND the width. Citation: WikiChip VPCLMULQDQ; BranchFree.org "Quote pairs with PCLMULQDQ" (2019); Linux kernel CRC-32C reaches 45-60 GB/s vs ~7-8 GB/s SSE4.2 with this primitive — same multiplier on prefix-XOR.
    - **x86_64 AVX-IFMA `vpmadd52luq` / `vpmadd52huq`** (NEW 2026-05-12; Sapphire Rapids+ Intel, Zen 4+ AMD): Eisel-Lemire mantissa multiplication for parse_number. asmjson dispatches number tokens to a Rust `JsonWriter` vtable (no number parse in asm); we keep mantissa-mul in vector lanes and return f64 directly. ~3× on number-heavy corpora (canada, mesh, marine_ik, numbers). Citation: WikiChip AVX-512_IFMA; Lemire 2024 Sapphire Rapids vs Zen 4 JSON.
    - **x86_64 AVX-512 VNNI `vpdpbusd`** (NEW 2026-05-12; Cascade Lake+, Zen 4+): byte×byte→i32 dot product, 4 bytes per int32 lane. For parse_number digit-block accumulation: 16-digit chunk → 4 lanes of `(d3*1000 + d2*100 + d1*10 + d0)` via one dot product. Citation: Lemire 2023 "Parsing integers quickly with AVX-512".
    - **x86_64 AVX-512 BITALG `vpshufbitqmb` + `vpopcntb`** (NEW 2026-05-12; Ice Lake+, Zen 4+): bit-gather 8 selected bits per 64-bit lane into k-mask (inverse of `vpcompressb`); per-byte popcount. One-µop multi-class classify that replaces `vptestmb + vpermb + vpmovb2m` triples. Per-state classification becomes data, not code. Citation: WikiChip AVX-512_BITALG.
    - **x86_64 AVX-512 GFNI**: `vgf2p8affineqb` arbitrary 8-bit affine transformation in 1 µop (Wojciech Mula 2018-2024; Intel GFNI Technology Guide 2018; **abstract primitive: single-op character classification, 2× over PSHUFB**; applies to ANY grammar's structural-byte classify).
    - **x86_64 AVX-2 + BMI2**: `_mm_clmulepi64_si128` prefix-XOR via CLMUL (simdjson original); `_mm256_shuffle_epi8` (sonic-rs `src/util/arch/x86_64.rs`); `_pdep_u64` parallel-deposit + `_pext_u64` parallel-extract (Mula branchfree.org 2018; **abstract primitive: bits-to-indexes / indexes-to-bits compaction** — applies to ANY structural-bitmap-to-offset-stream conversion).
    - **portable scalar**: SWAR 8-byte classify (asmjson #8 lineage; `word.wrapping_sub(0x2020202020202020) >> 7` for whitespace; `word ^ 0x2222222222222222` for quote; **abstract primitive: byte-class detection without SIMD**; correctness floor).
    - **hash primitive**: `ahash` crate (AES-NI on x86_64; NEON-AES on arm64); used by `path!` dictionary lookups and object-key hash; identical mechanism to sonic-rs's `ahash`-shaped hashing; not a bespoke implementation.
    
    Handwritten `asm!` blocks are admissible **only** when the equivalent intrinsic is absent from `core::arch::*` (current set: arm64 `ldp`/`stp`/`stnp` pair-load/store, `PRFM PLDL2STRM`/`PLDL1KEEP` prefetch variants, asmjson-style `r10`-direct-threading FSM entry; ffmpeg `x86inc.asm` macro corpus vendored verbatim at `crates/bbnf-simd/ext/x86/x86inc.asm`); new entries require documented measurement justification + citation to a published architecture. Hand-tuned undocumented intrinsic loops without an architectural name are forbidden as magic.
    
    **Abstract primitive lifts**: dav1d's pixel-arithmetic kernels do not translate to JSON (T14-T17 of the catalog: motion compensation, IDCT, loop filter, film grain — all are pixel-domain). But the *primitive operations* underneath them DO translate: cross-lane permute (`vextq_u8`), multiply-accumulate (`udot`), saturating arithmetic, cache hints, mask-register state machines. Each generalizes to byte-stream parsing for arbitrary grammars; the per-grammar selection is cost-model-derived from Grammar IR (alphabet size, number-token presence, string-token presence, chunk-spanning-token presence). The msac entropy decoder's `cnt/buf/end` cross-chunk refill pattern (`/tmp/dav1d-research/dav1d/src/x86/msac.asm:80-220`) is the one genuinely transferable algorithmic insight beyond what simdjson/sonic-rs/yyjson already demonstrate.
    
    Every SIMD primitive carries a unit-parity test against the scalar reference and a corpus-parity test against the expanded skinny corpus recorded in `restart/skinny/BENCH.md` §3 and `skinny/RESULTS.md` in `crates/bbnf-simd/tests/` (per `feedback_no_inline_tests`). Verification: every `core::arch::*` use-site and every `asm!` block in `crates/bbnf-simd/` traces to a citation in the Lock 16 allowlist or in the current skinny SOTA-BEAT synthesis.

    **v+1 primitive manifest**: every `core::arch::*`, `target_feature`, and
    `asm!` use-site in `bbnf-simd`, parse-that facades, generated scanners, or
    collapsed-stage code maps to a manifest row containing stable primitive id,
    abstract primitive name, primary ISA/library citation, hardware gate,
    scalar reference, strict checkasm/parity command, corpus/equality parity,
    grammar policy source, substrate target, retention lifetime, policy owner,
    same-wave production consumer, expected row/feature gate, LOC/risk,
    rollback path, abrogate threshold, and final disposition. Evidence:
    `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:150`-`180`,
    `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:89`-`101`.

    Admission checkasm commands run with `BBNF_SIMD_STRICT=1`. Non-strict
    parity is exploratory only and cannot admit a primitive, route, or row.
    Every scalar/checkasm/equality failure rejects the candidate for that wave.
    Evidence: `skinny/REDRESS.md:3621`-`3625`,
    `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:47`-`58`.

    `escape_mask_64` is an admitted correctness prerequisite, not a production
    SIMD/ASM throughput primitive. Its checkasm-backed state covers the
    historical xorshift falsifier and scanner parity cases, but it admits a
    row only when a JSON/CSS string or escape consumer wires it in the same
    wave and moves or rejects the named row under strict comparator evidence.
    Evidence: `skinny/REDRESS.md:3603`-`3632`,
    `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:91`-`92`,
    `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:49`.

    At close, every source-present primitive is exactly one of `wired`,
    `deleted`, `scalar-delegate-non-ASM`, or
    `architectural-block-with-REDRESS`. `inventory_demoted_with_evidence` is
    historical evidence only. Support-only hint modules, unconsumed prefix/next
    bitmap bodies, cache hints without exact caller placement, and orphan
    `asm!`/intrinsic files do not close Lock 16. Evidence:
    `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:194`-`206`,
    `restart/skinny/tranches/sk-v13/SYNTHESIS.md:84`-`93`.

    `CollapsedStage` is admissible only as a concrete emitted transient
    strategy with scalar reference, strict parity/checkasm, feature gate,
    local temporary lifetime, and same-wave measured consumer. AVX-512
    literature is x86 architecture pressure and cannot close M5/aarch64 rows.

    **v+1 predicate hardening (LAC-2D-06; CH5 F-CH5-V1-03)**: the live
    `admits_collapsed_stage` predicate at `skinny/crates/passes/src/lib.rs:874`-`876`
    MUST co-require `target.arch == x86` alongside `target.avx512bw` and
    `Entry(_)`, refusing aarch64 admission via cross-build
    `target.avx512bw` inheritance. Marker-string lowerers at
    `skinny/crates/codegen/src/lower/collapsed_stage.rs:15`-`17` are not
    admissible (P1-1B-D6). Every `BackendExpr` node / rewrite guard /
    extraction result MUST declare
    `substrate_target ∈ {local_temp_only, existing_tape, direct_sink,
    admitted_fact_output}` per Lock 1 v+1 manifest. E-graph extraction MUST
    reject plans whose `substrate_target` is not one of the four admitted
    values. Until a generated aarch64 strategy lands (UNKNOWN-2D-05 +
    2E source-backed aarch64 candidate), `CollapsedStage` admission is
    mechanically refused on aarch64.
    Evidence: `restart/audit/totality/p2/2D-cost-model.md:265`,
    `restart/audit/totality/p2/2D-cost-model.md:123`,
    `restart/audit/totality/p2/2D-cost-model.md:191`,
    `restart/skinny/tranches/sk-v13/SYNTHESIS.md:223`-`230`,
    `skinny/crates/passes/src/lib.rs:874`-`876`.

    Native `svmatch_u8` is SVE2-only. The existing Lock 16 NEON set-membership
    row remains a separate NEON reduction-tree port only if the manifest names
    the NEON implementation, scalar oracle, strict checkasm, hardware gate,
    and consumer row. Evidence:
    `restart/audit/totality/p2/2E-host-arch-esoterica.md:270`,
    `restart/locks/LOCKS.md:290`.

    PMULL/CSSC, union, ASM-gen, cache-hint, parse-that, and hardware facade
    routes require material-differential text against prior REDRESS rows,
    micro-prove-first evidence where applicable, grammar-policy proof, and a
    same-wave production consumer or measured deletion/rejection. Evidence:
    `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:120`-`137`,
    `skinny/REDRESS.md:3766`-`3820`,
    `skinny/REDRESS.md:3864`-`3868`.

    **v+1 `bbnf-regex::Dfa` admissibility (LAC-2F-V5-01)**: contingent on
    absorption-wave Q1 resolution (SK-V14 W11), the manifest gains a
    `bbnf-regex::Dfa` admissibility row. Admissibility requires (a) a scalar
    reference (Hoehrmann/Thompson straightforward construction at
    `regex-engine.md:28`-`44`); (b) checkasm-parity equivalence to
    `regex_automata::meta::Regex::find` over the byte stream; (c) a same-wave
    consumer (host-fn or leaf-parser dispatch site).

    **CH3 pre-flight reflex (V6 F-CH3-2F-08, LOW prophylactic)**: before any
    `bbnf-regex::Dfa` admissibility row dispatches, the absorption wave MUST
    (i) execute a CH3-class REDRESS regression scan over `skinny/REDRESS.md`
    and `restart/skinny/tranches/sk-v{1..14}/` for any prior DFA / NFA /
    Aho-Corasick / regex-engine admission attempt, recording the result inline
    as a precondition; (ii) the amendment MUST carry an explicit REDRESS
    pre-block citation listing — at minimum the routes the forward absorption
    MUST NOT re-open: REDRESS 96 (retained class-column substrate, falsified
    per `skinny/REDRESS.md:2797`-`2848`), REDRESS 97 (streaming structural
    cursor, falsified per `:2852`-`2906`), REDRESS 98
    (`G-W3-UNION-SUBSTRATE` retired per `:2910`-`2950`), plus any prior
    regex-shaped admission attempt surfaced by clause (i). This restores
    parity with LAC-2A-V1-01 / LAC-2D-05 / LAC-2E-04 / LAC-2F-V5-02 /
    LAC-2F-V5-04, which all carry the REDRESS pre-block citation inside
    amendment text. Evidence:
    `restart/audit/totality/p2/2F-parse-that-gaps.md:518`,
    `restart/audit/totality/p2/2F-parse-that-gaps.md:23` (v6_fold F-CH3-2F-08).

## v+1 Governance Boundary

The v+1 text above is active only because Pass Omega CHALLENGE converged and
G-Omega authorized CRUD operations on governance surfaces. No implementation
wave may use v+1 wording as permission to add a directive, add a BIR variant,
add or retire a lock, expand `BackendShape`, add a public substrate API,
retain a sidecar, or bypass the owning skinny SPEC gate. Evidence:
`restart/prompts/pass-contracts/PASS-OMEGA.md:86`-`108`,
`restart/prompts/ORCHESTRATOR.md:165`-`172`,
`restart/audit/totality/astral/V1/G-OMEGA-SIGNOFF.md`.

## Lanes

Produce one document per lane plus a synthesis. Each lane has scaffolded items the auditor must produce.

### Lane 1 — Lock-Adherence

Walk every plan document. For each of the twelve locks, cite path:line in the plan where the lock is honored, and path:line where it is violated (or absent). For violations, recommend the surgical edit that closes the violation.

For each lock, end with a verdict: **honored / violated-with-recommendation / silent (must add)**.

### Lane 2 — Sequencing Discipline

The Era V failure mode (substrate-then-substrate-then-ship, never substrate-then-consumer-then-ship) was the genesis of seven dead substrate crates between AV and AX. The new plan must not repeat it.

For every wave in BA / BB / BC:
- What does this wave produce?
- Who consumes it, and when?
- If the consumer arrives in a later wave, is the substrate compileable + tested + benchable in this wave's gate?
- If the consumer never arrives in the plan, why is the substrate landing?

Flag any wave whose deliverable lands without a same-wave or next-wave consumer. Recommend either: (a) add the consumer to the wave, (b) merge the wave into the consuming wave, (c) cut the substrate from the plan.

### Lane 3 — Cohesion

Every wave's exit-criteria must be achievable from prior waves' outputs. Walk the wave dependency graph:
- W0 produces X. W1 consumes X via Y mechanism. Is Y specified?
- W1's exit-criteria reference Z. Is Z produced by W0 or W1's body?
- Are any wave's gates impossible to verify from artifacts the wave creates?

Identify orphan exit-criteria (gates that test invariants no wave produces) and orphan deliverables (wave outputs no later wave consumes).

### Lane 4 — SOTA Anchoring

For every perf gate in the plan, verify it cites a specific SOTA number with platform + dataset:

- ✓ "≤ 500 µs to parse twitter.json on M1 Pro, beating sonic-rs's 436 µs"
- ✗ "≥ AU bench parity"
- ✗ "≥ baseline"
- ✗ "≥ pre-W3"

Flag any gate that does not name a competitor's number. Recommend the specific number to substitute, sourced from `audit/SOTA-2026-05-03.md`.

### Lane 5 — Grammar-Authoritative Discipline

Per-grammar code in supposedly-generic crates is the GESTALT § grammar-authoritative violation. Walk the plan for any wave deliverable that:
- Hardcodes grammar idents in `bbnf-ir`
- Adds per-grammar match arms in non-codegen files
- Adds per-grammar feature flags
- Names a module after a grammar

Also: walk the plan for any wave that does NOT excise the existing violations enumerated in `audit/CENSUS-2026-05-03.md` §2 (css_types.rs, ir/registry/strategy.rs:130-185, ir/passes/audit/payload_coverage.rs:69, ir/passes/recognizers/shape_dict_bbnf.rs).

Recommend per-violation surgery; recommend tranche-and-wave for each excision.

### Lane 6 — Generated-Code Budget

Per-tranche LOC budget for `crates/core/src/grammar/generated/`. The current 168 K LOC across 9 grammars is the starting point. Layout lowering may grow some files (typed-enum variants explode) and shrink others (dispatch indirection retires).

For each wave:
- Does it grow generated LOC? Estimate.
- Is the growth justified (typed payloads carrying real data) or accidental (generator regression)?
- Is there a per-wave budget check in the gate?

Flag any wave that is silent on generated-code impact. Recommend a budget check (e.g., "css_l4.rs ≤ 110 K LOC; bbnf.rs ≤ 22 K; net delta ≤ +5%").

### Lane 7 — Friction Forecast

Forecast where users and grammar authors hit the new API and do not understand it. For each:
- The API surface (signature + docstring as planned)
- The user mental model required
- The point of greatest confusion
- The educational artefact the plan should produce (cookbook entry, doc page, error message hint)

Specifically forecast friction at:
- `pointer!["a", "b", 1]` syntax (compile-time path AST)
- `parse(input)` vs `parse_in(input, bump)` vs `parse_owned(input)` (lifetime escape hatches)
- Layout lowering errors (rule X has no resolvable layout because Y)
- Pratt auto-detection misfiring on a grammar shape the optimizer should not have classified as Pratt

Recommend at least three error messages (verbatim) the plan should commit to.

### Lane 8 — Carry & Deferral Audit

Every plan item deferred to a later tranche must:
- Name the receiving tranche (no "future tranche", no "AZ-V" fictional successors)
- State what blocks it from this tranche (specific dependency)
- Have a corresponding gate in the receiving tranche

Walk every "deferred to BB", "carry from BA.W3", "see BC.W1" in the plan. Verify each:
- Names a real, drafted tranche
- States the blocker concretely
- Lands in the receiving tranche's gate list

Flag every dangling carry. Recommend either: (a) move forward into current tranche, (b) explicit landing in receiving tranche's W?.M? gate, (c) cut entirely.

## Invariants of the audit

§1. **No metalanguage in audit docs**. Reference plan content by path:section (e.g., "BA.W2.M3 fails because…"); never reference commits, conversation history, or the plan's draft history.

§2. **Audit voice is direct, archaic-permissive**. Match the project's voice ("hereupon", "begotten", "thereof"). Avoid corporate hedging ("might want to consider"). State faults directly.

§3. **Citations are path:line, not paraphrase**. Every claim about the plan cites where it lives. Every claim about the codebase cites where it lives.

§4. **Recommendations are surgical**. Not "improve cohesion"; instead "merge BA.W4 into BA.W3 because W4 has no consumer of W3's output that BA itself uses; relocate W5 ↑ to fill the slot".

§5. **Verdicts are ratifiable or actionable**. Not "this is concerning"; instead "honored", "violated-with-rec-X", or "silent-must-add-Y".

## Execution discipline

§ED1. Each lane is one document. Filename: `audit/HARDENING-PLAN-2026-MM-DD-NN-<lane-slug>.md`.

§ED2. Plus one synthesis: `audit/HARDENING-PLAN-SYNTHESIS-2026-MM-DD.md` referencing each lane and tabulating cross-lane verdicts.

§ED3. Lanes can dispatch in parallel; synthesis lands after.

§ED4. HARD CAP per lane: 25 minutes. Synthesis: 15 minutes. At 0.9N commit progress, at N halt and report.

§ED5. The synthesis ends with a punch list: ordered, surgical, ready to act on. Each item names its plan-doc target and the specific edit.

§ED6. No hedges. No "consider". No "might". The plan is either right or wrong on each lane.

## Voice locks

§V1. Archaic diction is welcome.

§V2. State the fault. State the surgery. Move on.

§V3. The auditor is not a collaborator on the plan; the auditor is its first adversary. Ratify what survives; cut what doesn't.

§V4. No restating of the plan back to the user. Cite path:line and proceed.

## Failure modes to avoid

D1. **Restating the plan as audit**. The audit document recapitulates the plan in its own voice instead of identifying faults. Symptom: "BA.W2 plans to do X; this is good because Y." Audit is not summary.

D2. **Soft verdicts**. "Could be tightened", "may benefit from review". Either it's a fault (with surgery) or it's not.

D3. **Paragraph-level critiques**. "The optimization layering section needs more depth." Cite the line; specify the addition.

D4. **Ignoring locks**. The twelve locks above are settled. The audit does not relitigate them; it verifies the plan honors them.

D5. **Carry-blindness**. Treating every "deferred to BB" as legitimate without auditing whether BB has the gate. Era V's failure mode replicated.

D6. **Friction-vagueness**. "Users may find this confusing." Specify the user, the mental model, the point of confusion, the verbatim error message.

D7. **SOTA-erasure**. Accepting "≥ baseline" as a perf gate. Every gate names a competitor's number.

D8. **Genericity-erasure**. Accepting per-grammar code in generic crates because "the plan says we'll fix it later". Cite the planned fix or flag the deferral.

## Reading list (in order)

1. `docs/tranches/BA/BA.md`
2. `docs/tranches/BA/waves/*.md`
3. `docs/tranches/BB/BB.md`
4. `docs/tranches/BC/BC.md`
5. `docs/tranches/BD/BD.md` if drafted
6. `audit/HARDENING-SYNTHESIS-2026-05-03.md`
7. `audit/SOTA-2026-05-03.md`
8. `audit/CENSUS-2026-05-03.md`
9. `audit/MODULES-2026-05-03.md`
10. `audit/RESTART-SKETCH-2026-05-03.md`
11. `docs/tranches/meta-audit/archaeology/era-IV-tape-first.md` — tape arc archaeology (peak)
12. `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md` — substrate-first/consumer-later failure-mode anatomy
13. `docs/tranches/AV/research/04-columnar-soa.md` — kind-partitioned columnar SoA spec (designed, never activated; cited so the auditor can verify Lock 1 is honoured)

## Methodology

Per lane:

1. Open the plan documents (subjects).
2. Walk the lane's question with the plan in front of you.
3. For every claim, cite path:line.
4. For every fault, recommend the surgical edit.
5. End with a per-lane verdict tabulation: items honored, violated, silent.
6. Commit the lane document.

Synthesis:

1. Read all eight lane documents.
2. Tabulate cross-lane verdicts (the same plan section may be honored on Lane 4 and violated on Lane 6).
3. Produce a punch list — one entry per surgery, in execution order, with target path:line and verbatim edit.
4. Recommend whether the plan is ready to execute, ready after surgery, or requires re-draft.

## Provenance

This prompt is for hardening the BA-restart plan set drafted after the 2026-05-03 codebase audit. The twelve locks are user verdicts on the architectural commitments that govern BA-restart. Do not relitigate the locks; verify the plan honors them.

The codebase-side companion prompt is `docs/HARDENING-AUDIT-PROMPT.md` (audits the codebase, not the plan).
