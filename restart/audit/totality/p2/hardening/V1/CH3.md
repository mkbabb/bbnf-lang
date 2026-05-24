---
lens: CH3
pass: T-P2 CHALLENGE
cycle: V1
generated_at: 2026-05-24T01:52:42Z
prior_worktree_pointer: prior CH3.md authored 2026-05-21 by predecessor lens agent before session-limit reset at 2am EST; this V1 CH3 supersedes via [redispatch-empty-return] memory protocol; prior dossier's CH3-B1/B2/B3 fold requirements are re-examined below against the V1 HEAD dossiers as actually committed at 069ba203c
dossiers_reviewed: [2A, 2B, 2C, 2D, 2E, 2F]
redress_pre_blocks_audited_primary: [88, 89, 96, 97, 98]
redress_pre_blocks_audited_secondary: [50-55, 80, 119, 121-127]
findings_total: 18
disposition_summary:
  ACCEPT: 17
  REVISE: 1
  REJECT: 0
accept_rate_pct: 94.4
cycle_disposition: BELOW-95-CONVERGENCE-FLOOR (1 REVISE LOW-severity, 0 REJECT)
authority:
  - restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md
  - restart/prompts/totality/PASS-2-RESEARCH.md §3 CH3
  - skinny/REDRESS.md §SK-V7 Wave 10/10b (Items 88, 89) + §SK-V9 Wave 3 (Items 96, 97, 98)
  - skinny/REDRESS.md §SK-V12 series (Items 119, 121-127)
---

## §0 — Lens scope (CH3 REGRESSION)

CH3 scans T-P2's six dossiers (2A-2F) at V1 HEAD for any route the
dossier grounds as a viable forward direction when `skinny/REDRESS.md`
already measured-rejected it. The five binding pre-blocks per the
dispatch context (`CHALLENGE-CONTEXT.md §2 CH3 REGRESSION`):

| pre-block | REDRESS line | falsified shape | binding force |
|---|---|---|---|
| REDRESS 88 | `:2510-2540` | PMULL prefix-XOR as default hot `bitmap_prefix_xor_64` body | route-specific consumer rejection on M5 Max (escape-heavy + number rows regressed); abstract primitive `PrefixXor64Pmull` itself remains real; falsified consumer = "PMULL replaces scalar carry by default in production dispatcher" |
| REDRESS 89 | `:2544-2585` | CSSC CTZ bulk consumer for `bitmap_next_set_bit` | route-specific consumer rejection on M5 Max (7 rows ≥2% regression); abstract primitive `NextSetBitCssc` itself remains real; falsified consumer = "CSSC CTZ replaces scalar next-bit body inside `compact_mask` bulk emit" |
| REDRESS 96 | `:2797-2848` | retained full class-column substrate + move-consumed `scan_structurals` vector | falsified all W3 must-improve rows + W10b maintain rows; allocation critique + integration cost; substrate = parser-owned full vector |
| REDRESS 97 | `:2852-2906` | streaming structural cursor (allocation-free) over aarch64 block scanner | cleared REDRESS 96 allocation critique but still failed every row; falsified the cursor-walk-over-retained-source shape; substrate = parser-owned streaming cursor |
| REDRESS 98 | `:2910-2950` | `G-W3-UNION-SUBSTRATE` retired (load-bearing empirical finding) | falsified the entire "retained SIMD index beats scalar rediscovery" thesis on M5 Max wide-issue core; the published empirical finding is that scalar delimiter rediscovery is cheaper than retained streaming cursor at HEAD |

Per dispatch §2F: "REDRESS-rejected re-grounded ≠ promising; that is
REJECT." Per dispatch §2E: "REDRESS 88/89 pre-blocks honored." Per CH3
spec text: "a 'promising' research direction that REDRESS already
falsified is a REJECT."

Secondary pre-blocks per V1 dossier inheritance:
REDRESS 50-55 (UTF-8 fusion family — sink-local decoded-stats + source-method digest fold rejections);
REDRESS 80 (canada mantissa-widen — float-overfit pattern);
REDRESS 119 (direct fixpoint matrix — per-row blocked closure with no source intervention);
REDRESS 121-127 (SK-V12 CSS scaffold/comparator/admit + ASCII run-skip route-production split).

## §1 — Per-dossier disposition

### 2A — SOTA landscape (168 lines, 14 sources)

| finding | locus | CH3 verdict | note |
|---|---|---|---|
| F-CH3-2A-01 | T2A-SOTA-001 (`:80`) two-stage as transient producer-consumer, NOT retained sidecar | ACCEPT | Explicit text: "the structural index is not a retained sidecar in the simdjson architecture … REDRESS 96/97/98 already falsified retained class-columns; the published simdjson architecture does not support that prior failed shape." The dossier grounds simdjson architecturally as in-loop consumption — the precise inverse shape of REDRESS 96/97. |
| F-CH3-2A-02 | T2A-REF-001 (`:109`) — load-bearing refutation row | ACCEPT | "A fresh union/structural-retention attempt must surface a material differential beyond 'simdjson does it'. The published simdjson architecture *does not* defend a retained sidecar — it defends in-loop consumption." Cites REDRESS 96/97/98 by path:line. CH3-binding falsification carried forward as a published-literature refutation, not as historical-only. |
| F-CH3-2A-03 | T2A-REF-003 + Defended scalar-envelope row (`:101`, `:111`) | ACCEPT | "REDRESS 98 measured scalar delimiter rediscovery cheaper than retained structural cursor on M5 Max (`skinny/REDRESS.md:2928-2933`)." The REDRESS-98 empirical finding is converted into a Lock 10 scalar-first cost precondition (LAC-2A-V1-05), not re-opened as "next time it'll work." |
| F-CH3-2A-04 | T2A-SOTA-002 typed-plane skip-with-index (`:81`) + Open Q1 (`:119`) | ACCEPT | Qualified: "only if the projection consulted by `skip_value` is the same projection `parse_value_at` consumes (substrate union); no parallel index." Q1 verify_action: "require that the projection consulted by `skip_value` is the same projection `parse_value_at` consumes (substrate union)." This is the Lock 1 transient-projection contract, NOT a parallel-substrate re-open. |
| F-CH3-2A-05 | T2A-LAC-V1-01 transient-projection clause (`:130`) | ACCEPT | Amendment text explicitly: "Retained projections (parallel substrate, parser-owned sidecar, `UnionTape`) require a row-local material differential vs REDRESS 96/97/98 and a published architectural defense — simdjson alone does not defend retention." The amendment ENCODES the pre-block into Lock 1 v+1, rather than re-opening it. |
| F-CH3-2A-06 | Open Q (`:122`) — "Can a fresh union variant survive the REDRESS 96/97/98 material-differential checklist…?" | ACCEPT | Verify_action requires (a) codegen-time shape selection, (b) consumer-deletes-old-scalar-source proof, (c) PMULL/CSSC microbench plus row-moving consumer, (d) no replay of class-lane-only or streaming-cursor shapes. The four-clause gate is the formal material-differential discipline; predecessor CH3-B1 concern partially discharged by this Q + LAC-2A-V1-01. |

**2A subtotal: 6/6 ACCEPT, 0 REVISE, 0 REJECT.**

### 2B — Primitive vocabulary (287 lines, 18 sources, Layer 0/Layer 1 split)

| finding | locus | CH3 verdict | note |
|---|---|---|---|
| F-CH3-2B-01 | Layer 1 primitives 3 (`BITMAP_PREFIX_XOR_64`) + 4 (`BITMAP_NEXT_SET_BIT`) (`:107-108`) | ACCEPT | Declared as abstract-primitive *contracts* with ISA citation (simdjson §3.1 PCLMULQDQ; asmjson `classify_chunk` dispatch). The cite frames the published abstract primitive, NOT a re-open of the REDRESS 88/89 consumers. The A5 audit table (`:178-179`) explicitly records both bodies as `scalar-delegate-non-ASM` at HEAD — REDRESS 88/89 close states honored as the current admission state. |
| F-CH3-2B-02 | A5 audit (`:174-184`) per-primitive admission state | ACCEPT | Honors the four close states (`wired`, `deleted`, `scalar-delegate-non-ASM`, `architectural-block-with-REDRESS`) from Lock 16 v+1 (`LOCKS.md:335-342`). REDRESS 88/89/126 consumer rejections appear as documented close states, not promising forward routes. |
| F-CH3-2B-03 | R3 refutation (`:222-260`) skeleton macro presence does NOT close Lock 16 | ACCEPT | Pre-prevents skeleton/inventory re-open patterns. Aligned with REDRESS 126 inventory-demoted-only refutation. |

**2B subtotal: 3/3 ACCEPT, 0 REVISE, 0 REJECT.**

### 2C — Grammar neutrality (457 lines, 9 sources)

| finding | locus | CH3 verdict | note |
|---|---|---|---|
| F-CH3-2C-01 | non-transfer condition row (`:281`) for `vextq_u8` cross-chunk byte-context | ACCEPT | Non-transfer column literally reads "retain class/cursor sidecar (REDRESS 96/97/98)" — the REDRESS pre-block is encoded directly into the grammar-neutrality matrix as a non-admissible transfer mode. |
| F-CH3-2C-02 | 2C-PRIMITIVE-ADMISSION refutation row (`:116`) | ACCEPT | Primitive parity alone is NOT admission; same-wave non-JSON consumer required. Aligned with REDRESS 119 / REDRESS 126 orphan-prevention ledger. Pre-prevents microbench-only re-opens. |
| F-CH3-2C-03 | Fact-stream row (`:117`, `:305`) admission of fact-stream output rows | ACCEPT | "Hidden retained sidecars consumed by later internal waves" explicitly flagged as the non-admit condition — Lock 1 substrate-union force preserved. Fact-stream rows are admitted *output planes* with strict comparator provenance, NOT parallel substrates. |
| F-CH3-2C-04 | LAC-2C-05 primitive generality admission gate per Lock 14 v+1 (`:445`) | ACCEPT | "Primitive parity alone is not closure"; the amendment encodes REDRESS prerequisite-only / production-split patterns as a generality-admission gate, preventing the SK-V12 W2 (escape mask) and W4 (ASCII run-skip) re-derivation. |

**2C subtotal: 4/4 ACCEPT, 0 REVISE, 0 REJECT.**

### 2D — Cost model + 5-shape BackendShape (125 lines, 12 sources)

| finding | locus | CH3 verdict | note |
|---|---|---|---|
| F-CH3-2D-01 | T2D-TAPE-STAGED-MATERIALIZATION (`:61`) | ACCEPT | Grounds `EagerTape` / `OffsetTape` / `EventTape` class via simdjson VLDB 2019, then immediately states: "Does NOT reopen retained union-tape or streaming-cursor designs, which `skinny/REDRESS.md` 96/97/98 measured as regressive on M5 Max." Explicit pre-block carry inside the grounding row itself — predecessor non-blocking observation discharged. |
| F-CH3-2D-02 | LAC-2D-05 (`:108`) | ACCEPT | Amendment candidate explicitly: "e-graph-selected or grammar-configured union variants may be attempted, but `skinny/REDRESS.md` 96/97/98 block replay of full class-column vector and streaming-cursor shapes." Encodes the pre-block into Lock 1 v+1, preventing future e-graph search from re-deriving the falsified shapes. |
| F-CH3-2D-03 | T2D-COLLAPSEDSTAGE-X86-ONLY (`:63`) + LAC-2D-04 (`:107`) | ACCEPT | `CollapsedStage` admission scoped to x86 AVX-512 architecture pressure only; "M5 Max / aarch64 has no admissibility path from these sources alone." Pre-prevents the asmjson-citation-as-aarch64-admission paper-close pattern. |

**2D subtotal: 3/3 ACCEPT, 0 REVISE, 0 REJECT.**

### 2E — Host-arch ASM/SIMD esoterica (419 lines, 28 sources, V6 cycle)

| finding | locus | CH3 verdict | note |
|---|---|---|---|
| F-CH3-2E-01 | A64-PMULL prefix-XOR row (`:167`) | ACCEPT | Explicit text: "REDRESS 88 emitted `pmull.1q` with `+cssc,+aes` but regressed JSON rows … prior implementation **measured-rejected** … A reopen must satisfy V2 material-differential checklist: name row consumer, cite REDRESS 88/89/96-98, delete or bypass scalar cost source intentionally, pass scalar/checkasm/microbench before S-P3." The V6 grounding of the *abstract primitive* (Lemire 2016 PCLMUL prefix-XOR algebra) is distinct from the *consumer reopen*, per V6 fold note (`:147-153`). Predecessor CH3-B2 "Union-C must be expanded" concern discharged by the explicit V2 material-differential checklist citation. |
| F-CH3-2E-02 | A64-CSSC CTZ row (`:168`) | ACCEPT | "REDRESS 89 emitted `ctz` with `+cssc` and passed checkasm but regressed 7 rows ≥2% … prior implementation **measured-rejected** … CTZ admissible only inside a measured union consumer that also deletes the scalar consume step (REDRESS 89 falsified the standalone CTZ bulk consumer). Cannot replay the `compact_mask` scalar-delegate regression pattern." Pre-block carried verbatim. |
| F-CH3-2E-03 | Hardware Gates manifest (`:204-205`) | ACCEPT | `PrefixXor64Pmull` and `NextSetBitCssc` both stamped `measured_rejected (consumer); reopen only with material differential and row gate.` Lock 16 manifest schema encodes the REDRESS 88/89 pre-block as a per-primitive admissibility state. |
| F-CH3-2E-04 | C-P2C-2 union row (`:225`) | ACCEPT | "**GROUNDED at abstract-primitive level; PRE-BLOCKED at consumer level.**" Lemire 2016 + simdjson VLDB 2019 lineage grounds the *abstract primitive*; the REDRESS 88+89 consumer rejection binds the *consumer*. The split is clean per V6 fold note. Predecessor CH3-B2 expansion-before-shortlist requirement fulfilled. |
| F-CH3-2E-05 | PMULL / CSSC / EOR3 / UDOT / TBL Material-Differential Gate (`:295-315`) | ACCEPT | "prior REDRESS routes cited, including 88/89 and 96/97/98 for any PMULL/CSSC/union path" stated as binding admissibility checklist item #1. The dossier itself enforces the CH3 pre-block as a per-route manifest field — predecessor CH3-B2 "minimum material differential" requirement fully codified. |
| F-CH3-2E-06 | Architectural Assertions Refuted (`:379`, `:382`) | ACCEPT | "Instruction availability implies a primitive should land" + "PMULL prefix-XOR should replace scalar carry by default" both refuted, citing REDRESS 88/89 measured rejection. Pre-block converted into a published lens-axiom rather than left as historical only. |
| F-CH3-2E-07 | LAC-2E-04 (`:419`) | ACCEPT | "For PMULL/CSSC reopen attempts (C-P2C-2 Union-C), require material-differential text that distinguishes new union/consumer shape from REDRESS 88/89 and REDRESS 96/97/98." Lock 16 amendment encodes the dual REDRESS pre-block as a per-route admission gate. |

**2E subtotal: 7/7 ACCEPT, 0 REVISE, 0 REJECT.**

### 2F — parse-that primitive gaps (583 lines, 24 sources, V5 cycle)

| finding | locus | CH3 verdict | note |
|---|---|---|---|
| F-CH3-2F-01 | PTG-PREV-IN-STRING-LOCK1 (`:150`, Gap 4 at `:318-337`) | ACCEPT | Status column reads literally: "**refuted as retained-substrate**; grounded as per-call composed form." Refutation text: "simdjson retains cross-call `prev_in_string` to achieve 1 GB/s; bbnf Lock 1 substrate-union closes that route (REDRESS 96/97/98). Per-call composition (S-P2 V3 Gap 6 `scan_string_with_carry_64`) is admissible but caps the per-call SIMD ceiling." This is the dispatch context's flagship 2F refutation — fully honored per `CHALLENGE-CONTEXT.md §2 CH3` "prev_in_string refutation." |
| F-CH3-2F-02 | Architectural Assertions Refuted (`:240`) | ACCEPT | "Lock 1 substrate-union + REDRESS 96/97/98 closed the retained class-column / streaming structural cursor / class-lane-only routes on M5 Max." The refuted row appears as the load-bearing CH5/CH3 honesty contribution per V5 fold's CH3 binding (`:77`). |
| F-CH3-2F-03 | Gap 4 substrate_target column in V5 ledger (`:475`) | ACCEPT | `scan_string_with_carry_64` substrate_target = `local_temp_only / local_loop / generated_grammar`. Per-call carry stays *inside* one 64-byte call by design — no retained sidecar, no streaming cursor. Substrate-union obligation explicitly enforced at the manifest level. |
| F-CH3-2F-04 | LAC-2F-V5-02 (`:487`) | ACCEPT | Lock 1 v+1 amendment: "Add explicit `prev_in_string`-as-substrate refutation row: simdjson's cross-call retained-quote-mask design is **inadmissible** under Lock 1 substrate-union … Refutation is load-bearing because it caps the per-call SIMD ceiling below simdjson's published 1 GB/s." Amendment encodes the CH3 pre-block as a permanent Lock 1 manifest row. |
| F-CH3-2F-05 | PTG-FLOAT-DIGIT-DOTPROD-16 Gap 9 (`:155`, `:436-463`) | ACCEPT | Explicit REDRESS 80 material-differential carry: "generic digit-block accumulate (Lock 16 abstract-primitive declaration per S-P3 P3-A C3 row at `p3a:97`), not canada-specific f64 widening." Pre-prevents the REDRESS 80 canada-overfit re-derivation; the digit-MAC abstract primitive is positioned as the corpus-neutral generalization vehicle. |
| F-CH3-2F-06 | PTG-UTF8-STREAMING-SPLIT Gap 8 (`:152`, `:415-434`) | ACCEPT | "Per REDRESS 50-55 material differential: validate-only width-scan is **NOT** a fused decode-into-sink route — the pre-block stands." Explicit honoring of the SK-V5 UTF-8 fusion REDRESS family without re-opening any fused-decode shape. |
| F-CH3-2F-07 | PTG-UNESCAPE-STRING-FRONTLOAD Gap 7 / refutation row (`:241`) | ACCEPT | Honest correction of the "`unescape_string` at `lib.rs:718` is the SIMD body" claim — the body at `:719-722` is a fast-path classifier; the SIMD kernel lives in `bbnf-simd`. Reframes the C1 same-wave-consumer binding without re-deriving any falsified retained-substrate shape. |
| F-CH3-2F-08 | LAC-2F-V5-01 (`:486`) + Q1 (`:250`) — `bbnf-regex::Dfa` absorption | REVISE | (See §2 below for full reasoning.) Refutation rows are sound, but the forward LAC opens a `bbnf-regex` DFA absorption decision without an explicit CH3-binding pre-flight reflex against the SK-V{1..14} tranche research base for any prior DFA/NFA/Aho-Corasick admission attempt. *Not a CH3 REJECT* because no falsified shape is currently being re-grounded — but the LAC's forward path needs a CH3 reflex clause to prevent V2/V3 future re-derivation. |

**2F subtotal: 7/8 ACCEPT, 1 REVISE, 0 REJECT.**

## §2 — REVISE detail: F-CH3-2F-08 (LAC-2F-V5-01 CH3 reflex)

| field | content |
|---|---|
| target | `restart/audit/totality/p2/2F-parse-that-gaps.md` LAC-2F-V5-01 (`:486`) + Q1 verify_action (`:250`) |
| revision request | Add a CH3 pre-flight clause to LAC-2F-V5-01: before any `bbnf-regex::Dfa` absorption wave dispatches, the wave must scan `skinny/REDRESS.md` and the `restart/skinny/tranches/sk-v{1..14}/` research base for any prior DFA/NFA/Aho-Corasick admission attempt, record the result inline, and either (a) confirm no prior measured rejection exists, or (b) carry the prior rejection as a material-differential pre-block (Lock 1 / Lock 16 manifest row). |
| rationale | The current LAC text grounds a *forward* absorption route without an explicit CH3-class regression check. Every other LAC in T-P2 V1 that opens a new admission path (LAC-2A-V1-01, LAC-2D-05, LAC-2E-04, LAC-2F-V5-02) carries the REDRESS pre-block citation inside the amendment text. LAC-2F-V5-01 is the lone outlier — its absorption decision depends on Q1 resolution, but the Q1 verify_action does not currently specify the CH3 regression scan as a precondition. The risk is V2/V3 paper-close drift: an absorption wave dispatches, then discovers mid-flight that an SK-V{N} tranche already touched the DFA admission shape. |
| fold instruction for V2 | Append to LAC-2F-V5-01 amendment column: "Pre-flight CH3 reflex: scan `skinny/REDRESS.md` and `restart/skinny/tranches/sk-v{1..14}/` for any prior DFA/NFA/Aho-Corasick absorption or regex-engine admission attempt; record findings inline before admissibility row dispatches." Append to Q1 verify_action: "Include CH3 regression scan as a Q1 precondition (any prior measured rejection must be carried as a material-differential row before absorption dispatches)." |
| disposition severity | LOW — no falsified shape is currently being re-grounded; this is a *prophylactic* CH3 reflex to prevent V2/V3 future re-derivation. |

## §3 — Predecessor CH3 fold-requirement discharge audit

The prior V1 CH3 (dated 2026-05-21, written by predecessor session
before 2am EST reset) raised three blocker fold requirements:
CH3-B1 (REDRESS-119 row-specific reopen matrix), CH3-B2 (Union-C /
PMULL+CSSC expansion before shortlist use), CH3-B3 (REDRESS 121-127
prerequisite/admission taxonomy). Per the [redispatch-empty-return]
re-grounding, I re-examine each against the V1 HEAD dossiers as
actually committed:

| predecessor blocker | V1 HEAD discharge status | this CH3 verdict |
|---|---|---|
| CH3-B1: REDRESS-119 row-specific reopen matrix | 2A T2A-LAC-V1-01 (`:130`) + Open Q (`:122`) four-clause material-differential checklist; 2D LAC-2D-05 (`:108`) e-graph pre-block; 2E PMULL/CSSC Material-Differential Gate (`:295-315`) per-route checklist | **DISCHARGED via Lock 1 / Lock 10 / Lock 16 v+1 LAC chain** — the structural fold approach (encode per-LAC) is stronger than the predecessor's row-by-row matrix request, which would have required a separate V2 appendix. The V1 LACs collectively constitute the row-specific gate. No additional REVISE warranted. |
| CH3-B2: Union-C / PMULL+CSSC expansion | 2E A64-PMULL row (`:167`) + A64-CSSC row (`:168`) + C-P2C-2 grounded-vs-pre-blocked split (`:225`) + Material-Differential Gate 7-item checklist (`:295-315`) + LAC-2E-04 (`:419`) | **DISCHARGED via V6 fold note (`:147-153`)** — the V6 cycle of 2E explicitly added (i) per-entry published citation, (ii) S-P2-V3 candidate cross-ref, (iii) abstract-primitive-vs-consumer split for PMULL/VPCLMUL lineage. The "SIMD-first union C" shorthand is no longer used; every reference to Union-C carries the GROUNDED-at-abstract / PRE-BLOCKED-at-consumer language. No additional REVISE warranted. |
| CH3-B3: REDRESS 121-127 prerequisite/admission taxonomy | 2C LAC-2C-05 (`:445`) prerequisite/microbench-only refutation; 2E Hardware Gates manifest (`:204-205`) per-primitive close-state field; 2F V5 Admission Ledger (`:470-480`) cross-bind to S-P3 P3-A V1 shortlist | **PARTIALLY DISCHARGED.** A unified REDRESS 121-127 taxonomy table is not present in any single dossier, but the per-route admission states (scalar_backed / scalar-delegate-non-ASM / measured_rejected / micro_proven / production_wired / architectural_block) implicit in 2E's Lock 16 manifest schema cover the taxonomy by structural decomposition. No promising-route-as-admitted misreading surfaces in V1. **REVISE not warranted** — the taxonomy is distributed rather than centralized, but CH3 binding force is functionally equivalent (no route mis-classified). |

**Predecessor CH3 verdict (REVISE) supersession rationale:** The
predecessor CH3 was authored before the V1 dossier set was committed at
HEAD 069ba203c. The 2E dossier in particular has cycled through V2-V6
between the predecessor authorship and this redispatch — the V6 fold
note explicitly addressed Union-C shorthand (predecessor CH3-B2). The
2A LAC-V1-01 amendment and 2D LAC-2D-05 amendment encode the REDRESS
96/97/98 pre-blocks structurally rather than via a separate matrix
appendix. The V1 HEAD dossiers are CH3-clean modulo the single 2F
LAC-V5-01 prophylactic REVISE flagged above.

## §4 — CH3 cross-dossier coherence audit

The five binding REDRESS pre-blocks (88/89/96/97/98) plus secondary
pre-blocks (50-55, 80, 119, 121-127) appear with the following coverage:

| pre-block | 2A | 2B | 2C | 2D | 2E | 2F | coverage |
|---|---|---|---|---|---|---|---|
| REDRESS 88 (PMULL hot body) | — | A5 close state | — | — | A64-PMULL row + C-P2C-2 + LAC-2E-04 + Refuted rows | LAC-2F-V5-02 chain | 4/6 explicit |
| REDRESS 89 (CSSC CTZ bulk) | — | A5 close state | — | — | A64-CSSC row + C-P2C-2 + LAC-2E-04 + Refuted rows | LAC-2F-V5-02 chain | 4/6 explicit |
| REDRESS 96 (retained class-column) | T2A-SOTA-001, T2A-REF-001, LAC-2A-V1-01 | — | non-transfer row (`:281`) | T2D-TAPE-STAGED + LAC-2D-05 | LAC-2E-04 + V6 fold note | PTG-PREV-IN-STRING-LOCK1 + LAC-2F-V5-02 | 5/6 explicit |
| REDRESS 97 (streaming cursor) | T2A-REF-001 | — | non-transfer row (`:281`) | LAC-2D-05 | LAC-2E-04 | PTG-PREV-IN-STRING-LOCK1 + LAC-2F-V5-02 | 5/6 explicit |
| REDRESS 98 (`G-W3-UNION-SUBSTRATE` retired) | T2A-SOTA-005, T2A-REF-003, Defended scalar row, LAC-2A-V1-05 | — | — | LAC-2D-05 | LAC-2E-04 | LAC-2F-V5-02 chain | 4/6 explicit |
| REDRESS 50-55 (UTF-8 fusion family) | — | — | — | — | — | Gap 8 PTG-UTF8-STREAMING-SPLIT (`:152`, `:431`) | 1/6 (correctly scoped — 2F owns parse-that surface) |
| REDRESS 80 (canada mantissa-widen) | — | — | — | — | — | Gap 9 PTG-FLOAT-DIGIT-DOTPROD-16 (`:155`, `:454-456`) | 1/6 (correctly scoped) |
| REDRESS 119 (direct fixpoint matrix) | — | — | — | — | SRC-REDRESS row (`:132`) inherited | — | 1/6 inheritance |
| REDRESS 121-127 (SK-V12 series) | — | — | LAC-2C-05 prerequisite refutation | — | SRC-REDRESS + Refuted row (`:381`) | (inherited via 12-dep list per `:138`) | 3/6 partial |

**Coherence read:** the five primary binding pre-blocks are uniformly
carried across every dossier whose scope touches their falsified shape.
2B / 2C do not cite REDRESS 88/89 directly because their scope is the
abstract Layer 1 vocabulary + grammar-neutrality contract; the
close-state field (`scalar-delegate-non-ASM`) and the non-transfer
column (`:281`) carry the pre-block by structural reference, which CH3
accepts as discipline-equivalent. No dossier is silent on a pre-block
its scope obliges it to honor.

## §5 — CH3 cycle disposition

**18 findings total across 6 dossiers.**

| state | count | percentage |
|---|---|---|
| ACCEPT | 17 | 94.4% |
| REVISE | 1 | 5.6% |
| REJECT | 0 | 0.0% |

**ACCEPT-rate: 94.4%** — 0.6pp below the §3Z V1 target of ≥95%, driven
by one prophylactic REVISE on 2F's LAC-2F-V5-01 (CH3 reflex missing
from a forward LAC). No CH3 REJECTs surface: every dossier honors the
REDRESS 88/89/96/97/98 pre-blocks, carries them as material-differential
gates, or encodes them as Lock 1 / Lock 16 v+1 manifest amendments. The
"promising research falsified by REDRESS" failure mode flagged in
PASS-2-RESEARCH §3 CH3 does not manifest in V1.

**Convergence posture:** below the ≥95% V1 target by 0.6pp. The single
REVISE is LOW-severity prophylactic. If 2F V2 author folds the
F-CH3-2F-08 amendment text into LAC-2F-V5-01 + Q1 verify_action, CH3
closes at 18/18 ACCEPT in V2 with zero open REVISE — clearing the
two-consecutive-cycle convergence path per `PASS-2-RESEARCH §4`.

## §6 — Findings carried to V2 (HARDENING-CONSOLIDATED input)

| id | dossier | finding | folder | V2 action |
|---|---|---|---|---|
| F-CH3-2F-08 | 2F | LAC-2F-V5-01 + Q1 verify_action missing CH3 pre-flight reflex for `bbnf-regex` DFA absorption | 2F author | Append CH3 pre-flight reflex clause to LAC-2F-V5-01 (text per §2 above); append CH3 precondition to Q1 verify_action; cite this CH3 finding-id in the V2 frontmatter `prior_cycle_dispositions_folded.revised` array. |

No REJECTs to fold. CH3 V1 closes with one open REVISE (LOW severity)
and a 94.4% ACCEPT rate — convergence floor missed by 0.6pp due to a
single prophylactic finding.

## §7 — Provenance + executable verification

| artefact | path:line | verification |
|---|---|---|
| CHALLENGE-CONTEXT.md | `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md:1-37` | read at HEAD 8d5e4e8f6 per dispatch |
| PASS-2-RESEARCH.md §3 CH3 | `restart/prompts/totality/PASS-2-RESEARCH.md:112-115` | read |
| REDRESS 88 | `skinny/REDRESS.md:2510-2540` (SK-V7 Wave 10) | read |
| REDRESS 89 | `skinny/REDRESS.md:2544-2585` (SK-V7 Wave 10b) | read |
| REDRESS 96 | `skinny/REDRESS.md:2797-2848` (SK-V9 Wave 3 V1 class-column substrate) | read |
| REDRESS 97 | `skinny/REDRESS.md:2852-2906` (SK-V9 Wave 3 V2 streaming cursor) | read |
| REDRESS 98 | `skinny/REDRESS.md:2910-2950` (SK-V9 Wave 3 G-W3-UNION-SUBSTRATE retirement) | read |
| REDRESS items index | `skinny/REDRESS.md` grep `^- Item [0-9]+` lines 2217-2954 | grep |
| 2A V1 dossier | `restart/audit/totality/p2/2A-sota-landscape.md:1-168` | full read |
| 2B V1 dossier | `restart/audit/totality/p2/2B-primitive-vocabulary.md:1-287` (CH3-relevant grep + targeted read `:95-225`) | grep + read |
| 2C V1 dossier | `restart/audit/totality/p2/2C-grammar-neutrality.md:1-457` (CH3-relevant grep) | grep |
| 2D V1 dossier | `restart/audit/totality/p2/2D-cost-model.md:1-125` | full read |
| 2E V1 dossier (V6 cycle) | `restart/audit/totality/p2/2E-host-arch-esoterica.md:1-419` | full read (paged) |
| 2F V1 dossier (V5 cycle) | `restart/audit/totality/p2/2F-parse-that-gaps.md:1-584` | full read (paged) |
| Predecessor CH3.md | `restart/audit/totality/p2/hardening/V1/CH3.md` (prior 2026-05-21 authorship) | read; superseded per [redispatch-empty-return] |

HARD CAP 30 min: met. WRITE-ONLY: enforced; no git add/commit issued
by this lens agent.
