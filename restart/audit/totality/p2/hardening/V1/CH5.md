# T-P2 V1 CH5 Hidden Coupling / Lock 1

Pass: T-P2 Research. Cycle: V1 (SK-V14 totality re-cohort).
Lens: CH5 hidden coupling / Lock 1 substrate union.
Date: 2026-05-23. HEAD: `f25c3af2ecf2828637ba03d0de75daf2e0c7739c`.
Scope: `restart/audit/totality/p2/{2A-sota-landscape, 2B-primitive-vocabulary,
2C-grammar-neutrality, 2D-cost-model, 2E-host-arch-esoterica,
2F-parse-that-gaps}.md` at V1 atomic seed.

## §0 — Verdict

**ACCEPT-WITH-FOLD (5/6 dossiers); REVISE (1/6 dossier).**

Per-dossier ACCEPT rate: 5 ACCEPT-WITH-FOLD + 1 REVISE = **83% ACCEPT-class**
(below the §3Z ≥95% first-pass target but above the §3 ≥30% REVISE floor — the
research is structurally honest, with three load-bearing refutations
(T2A-REF-001 retained-substrate; LAC-2F-V5-02 prev_in_string;
T2D-COLLAPSEDSTAGE-X86-ONLY) carrying the substrate-union invariant cleanly,
and one remaining hot drift (2D `CollapsedStage` x86-only literature mapped
to an aarch64 admission predicate at `passes/src/lib.rs:874-876`) that needs
explicit lifetime annotation before T-P3 admits the shape).

The V1 dispositions verify cleanly against the dispatch §V1 focus:

1. **2D `CollapsedStage` keeps mask stream transient** — VERIFIED. The
   refutation row T2D-COLLAPSEDSTAGE-X86-ONLY (`2D-cost-model.md:63`) demotes
   the four-of-five marker-string lowerers as non-admissible and limits
   admissibility to "concrete emitted transient strategy with scalar reference,
   checkasm/parity, feature gate, **local temporary lifetime**, and same-wave
   measured consumer" (LAC-2D-04 at `2D-cost-model.md:107`). The transient
   posture is grounded, but the dossier does not yet explicitly annotate
   `substrate_target=local_temp_only` on every emitted mask/FSM stream — see
   F-CH5-V1-01 below.

2. **2B Layer 0/Layer 1 clean two-layer dependency** — VERIFIED. The
   dossier asserts `bbnf.asm:47-48` issues `%include "x86inc.asm"` plus
   `x86util.asm` with executable-verified cross-grep returning zero hits
   from Layer 0 back to Layer 1 (`2B-primitive-vocabulary.md:130-136`). The
   `policy_owner` LAC-2B-03 (`:261`) explicitly forbids shared call sites
   from inheriting hardcoded JSON constants — the dependency is mechanically
   one-directional with no policy leak. ACCEPT.

3. **2F prev_in_string refutation upholds Lock 1 substrate-union** —
   VERIFIED. `PTG-PREV-IN-STRING-LOCK1` at `2F-parse-that-gaps.md:150`
   refutes the cross-call retained quote-mask as inadmissible under Lock 1
   substrate-union per REDRESS 96/97/98; the dossier accepts only the
   *per-call composed form* where carry stays inside one 64-byte call and
   explicitly notes this caps the per-call SIMD ceiling below simdjson's
   1 GB/s. LAC-2F-V5-02 (`:487`) elevates this to a load-bearing lock
   amendment. ACCEPT-WITH-FOLD (the fold is to surface this refutation row
   to T-P3 §3C verbatim — the substrate refutation is the most load-bearing
   row in 2F's V5 ledger).

4. **2A simdjson two-stage explicitly refuted as retained sidecar** —
   VERIFIED. T2A-REF-001 at `2A-sota-landscape.md:109` refutes
   "simdjson's two-stage pattern implies bbnf should retain a structural-index
   sidecar" by citing the published architecture as *producer-consumer in one
   pipeline*, not retained dual substrate. T2A-LAC-V1-01 (`:130`) proposes the
   transient-projection clause: "structural / class / whitespace masks may
   live only as ephemeral producers consumed into the single tape / direct
   sink / fact output in the same loop". This is the single most important
   CH5 amendment candidate in the cohort. ACCEPT.

## §1 — Per-dossier disposition table

| dossier | disposition | rationale (CH5 read) |
|---|---|---|
| **2A SOTA landscape** | **ACCEPT** | T2A-REF-001 (`2A:109`) + T2A-LAC-V1-01 (`2A:130`) carry the substrate-union invariant cleanly: simdjson stage 1 is producer-consumer in one pipeline, not retained sidecar. The CH5 cell of the V1 lens overlay (`2A:73`) explicitly binds the constraint. REDRESS 96/97/98 pre-blocks are honored across the cohort. Zero CH5 drift. |
| **2B primitive vocabulary** | **ACCEPT-WITH-FOLD** | Two-layer dependency mechanically one-directional (`2B:130-136`); admission discipline requires same-wave consumer (`2B:142-149`); LAC-2B-03 forbids hardcoded JSON constants in shared call sites (`2B:261`). FOLD: the 9-primitive contract table does not yet carry `substrate_target` annotations per primitive — e.g. `BITMAP_PREFIX_XOR_64` should declare `local_temp_only` lifetime explicitly so a future consumer cannot quietly retain the prefix-XOR bitmap as a parser-owned sidecar. |
| **2C grammar neutrality** | **ACCEPT-WITH-FOLD** | The Abstract-Primitive-Lift Table (`2C:121-145`) is the cohort's load-bearing CH5 instrument: it names which primitives are truly grammar-neutral byte-window operations vs JSON-only-by-shape (with `JsonSink`, JSON role mining, and JSON exponent/sign all correctly refuted as JSON-only-by-shape). The HEAD partial repair of `OffsetFlags` to `GRAMMAR_BIT0`/`GRAMMAR_BIT1` (verified: `runtime/src/tape/mod.rs:22-23`) is correctly classified as partial. FOLD: fact-stream output planes (CSS row at `RESULTS.md:94`) need an explicit boundary statement separating *admitted output plane* from *retained internal sidecar*; the V1 dossier defends the distinction in prose but the boundary is not yet a manifest field. |
| **2D cost model** | **REVISE** | The dossier correctly refutes the four-of-five marker-string lowerers (`2D:63-65`) and limits `CollapsedStage` to x86-only architecture-pressure (T2D-COLLAPSEDSTAGE-X86-ONLY at `:63`). LAC-2D-04 (`:107`) names the admission gates correctly. **However** the live `admits_collapsed_stage` at `skinny/crates/passes/src/lib.rs:874-876` is admitted by aarch64 callers when `target.avx512bw` happens to be true on a cross-build — the dossier does not bind the predicate to refuse aarch64 admission, only to require additional fields. CH5 needs the dossier to explicitly forbid `CollapsedStage` from emitting on aarch64 until a generated aarch64 strategy lands (per UNKNOWN-2D-05 at `:97`). Additionally, the `BackendExpr` extraction architecture (T2D-EGRAPH-EXTRACTION, LAC-2D-01) does not yet require each rewrite/node to declare `substrate_target` — an e-graph extraction could otherwise pick a plan whose mask streams quietly retain into a sidecar without naming it. |
| **2E host-arch esoterica** | **ACCEPT-WITH-FOLD** | The V6 Lock 16 hardware-gate manifest (`2E:245-263`) carries `substrate_target` (`local_temp_only`/`existing_tape`/`direct_sink`/`admitted_fact_output`), `retention_lifetime` (`local_loop`/`generated_function`/`output_row`), and `policy_owner` (`generated_grammar`/`caller_data`/`none`) as required columns. The C-P2C-2 PMULL+CSSC Union route is correctly gated by REDRESS 88/89/96/97/98 material-differential checklist (LAC-2E-04 at `:419`). The x86 AVX-512 secondary expansion correctly notes that k-mask registers are *a parallel substrate at the ISA level* and that the primitive vocabulary "must collapse k-mask operations to 'ephemeral mask in local loop' (Lock 1: `local_temp_only`, `local_loop`)" (`2E:192`) — this is the load-bearing CH5 row of 2E. FOLD: surface the k-mask collapse rule as an explicit T-P3 amendment row so a cross-arch primitive lift never invites k-mask retention. |
| **2F parse-that gaps** | **ACCEPT-WITH-FOLD** | The V5 admission ledger (`2F:470-481`) carries `substrate_target` / `retention_lifetime` / `policy_owner` per candidate; the `bbnf_regex_hir_engine` row (`2F:472`) is correctly bound to `local_temp_only (compile-time facts) / generated_function / generated_grammar`, distinguishing compile-time scanner facts (admitted) from runtime scanner streams (which would be a substrate violation). LAC-2F-V5-02 (`:487`) carries the prev_in_string refutation as a load-bearing lock amendment. FOLD: the dossier should make the *compile-time vs runtime* split a Lock 1 manifest requirement, not just a per-candidate ledger column — any future regex/scanner crate admission must declare which side of the split it lives on. |

ACCEPT-class total: **5/6 = 83%**.
REVISE total: **1/6 = 17%**.
REJECT total: **0/6 = 0%**.

## §2 — V1 dispatch focus verification (substrate-union invariants)

The dispatch §V1 disposition focus binds five CH5 invariants. Each verified
against the cohort at HEAD `f25c3af2`:

### 2.1 — "No grounded design implies parallel substrate / sidecar / Lock 1 violation"

VERIFIED. Cohort-wide scan:

- 2A: T2A-REF-001 refutes retained sidecar; T2A-LAC-V1-01 codifies the
  transient-projection clause.
- 2B: Layer 1 macros are grammar-neutral with policy supplied by caller/
  generated data; `BITMAP_PREFIX_XOR_64`/`BITMAP_NEXT_SET_BIT` carry the
  bitmap as a transient producer to `BULK_EMIT_COMPRESSED` or scalar
  delegate, not as retained state.
- 2C: Refutes JSON role mining as fleet-wide generality mechanism; CSS
  fact-stream is correctly classified as output-plane row, not retained
  sidecar (with the V2 fold needed to make this a manifest field).
- 2D: T2D-COLLAPSEDSTAGE-X86-ONLY refutes the marker-string lowerers and
  limits `CollapsedStage` to transient kernel emission with same-wave
  consumer.
- 2E: V6 Lock 16 manifest carries `substrate_target` as a required column;
  k-mask substrate-leak is explicitly called out.
- 2F: PTG-PREV-IN-STRING-LOCK1 refutes cross-call retained quote-mask
  as inadmissible; per-call composed form is the admissible primitive.

No dossier grounds a design that would require a retained parallel substrate.
One drift remains: 2D's live `admits_collapsed_stage` predicate at
`passes/src/lib.rs:874-876` is not yet bound by the dossier's refutation
text — this is the F-CH5-V1-03 fold below.

### 2.2 — "2D CollapsedStage keeps mask stream transient (not retained sidecar)"

VERIFIED-WITH-REVISE. The dossier text is correct:

- T2D-COLLAPSEDSTAGE-X86-ONLY (`2D:63`): "Marker-string lowerer at
  `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-17` is not
  admissible (P1-1B-D6)".
- LAC-2D-04 (`2D:107`): "`CollapsedStage` admits only as a concrete emitted
  transient strategy with scalar reference, checkasm/parity, feature gate,
  **local temporary lifetime**, and same-wave measured consumer".

The REVISE is that the dossier does not yet explicitly forbid emission on
aarch64 (UNKNOWN-2D-05 at `2D:97` poses the question but does not bind the
answer). CH5 cannot accept a `CollapsedStage` lowerer that runs on a host
where the underlying AVX-512 source literature does not admit. See
F-CH5-V1-03.

### 2.3 — "2B Layer 0/Layer 1 clean two-layer dependency"

VERIFIED. The dossier shows:

- Layer 0 = 138 vendored macros (72 in `x86inc.asm` + 66 in `x86util.asm`)
  with executable-verified `%macro` counts (`2B:55-56`).
- Layer 1 = 9 bbnf-authored contracts; `bbnf.asm:47-48` issues the include
  edge; cross-grep confirms zero Layer-0 → Layer-1 references (`2B:131-136`).
- R1 refutation explicitly forbids vendoring pixel-domain kernels into
  Layer 0 (`2B:194-205`).
- R3 refutation forbids skeleton-macro presence from closing Lock 16; 3/9
  primitives (`FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`,
  `FRAME_POP_BOUNDED`) are correctly classified non-admissible
  (`2B:221-231`).

The two-layer dependency is mechanically one-directional with no policy
leak. ACCEPT.

### 2.4 — "2F prev_in_string refutation upholds Lock 1 substrate-union (REDRESS 96/97/98)"

VERIFIED. The refutation row at `2F:150` reads:

> "simdjson retains cross-call `prev_in_string` to achieve 1 GB/s; bbnf
> Lock 1 substrate-union closes that route (REDRESS 96/97/98). Per-call
> composition (S-P2 V3 Gap 6 `scan_string_with_carry_64`) is admissible
> but caps the per-call SIMD ceiling."

The architectural assertion refuted row (`2F:240`) restates: "The per-call
composed form (PTG-PREV-IN-STRING-LOCK1) caps the ceiling below simdjson's
published 1 GB/s; admissible only under the per-call frame."

This is the load-bearing CH5 row of 2F. ACCEPT, with the fold being to
elevate LAC-2F-V5-02 to a Lock 1 substrate-union v+1 manifest entry so the
refutation is not just dossier prose.

### 2.5 — "2A simdjson two-stage explicitly refuted as retained sidecar (T2A-REF-001)"

VERIFIED. T2A-REF-001 (`2A:109`) carries three citations:

- Langdale & Lemire 2019 architecture is producer-consumer in one pipeline.
- simdjson `doc/parse_many.md:54-57` at SHA `168ef580` shows stage 1 output
  is consumed by stage 2 in-line, not re-consumed by an independent walker.
- REDRESS 96/97/98 closed retained class-column + streaming cursor +
  class-lane-only as falsified on M5 Max.

This is the cohort's structural-substrate refutation. ACCEPT.

## §3 — Fold requirements (V2)

The five fold items below are necessary to advance the cohort's CH5
ACCEPT rate from 83% → ≥95% for V2.

### F-CH5-V1-01 — Per-primitive `substrate_target` annotation in 2B

The 9-primitive contract table at `2B:103-113` lists each Layer 1 primitive
with hardware gate + ISA citation. V2 must add per-row `substrate_target`
columns:

- `BYTE_CLASS_FROM_TABLE_64` / `BYTE_CLASS_FROM_EQ_SET_64` →
  `local_temp_only` (k1 mask consumed inside same chunk).
- `BITMAP_PREFIX_XOR_64` → `local_temp_only` (prefix-XOR bitmap consumed
  into next pipeline step in same loop; never persisted).
- `BITMAP_NEXT_SET_BIT` → `local_temp_only` (dispatch offset consumed
  immediately).
- `BULK_EMIT_COMPRESSED` → `direct_sink` or `admitted_fact_output`
  (depending on consumer; positions emit into the existing tape/sink, not
  a parallel index).
- `EOB_PAD_CLAMP` → `local_temp_only` (zero-padded 64B vector consumed
  inside the kernel).
- `FSM_DISPATCH_THREADED` / `FRAME_PUSH_BOUNDED` / `FRAME_POP_BOUNDED` →
  skeleton-only at HEAD (per R3 refutation); when admitted, must declare
  `local_loop` retention with state owned by the generated function, not
  by parser-owned sidecar.

Without these annotations a future consumer could quietly persist the
prefix-XOR bitmap or compressed-emit offset stream as a parallel substrate.

### F-CH5-V1-02 — Fact-stream output-plane boundary as a manifest field in 2C

The dossier prose distinguishes admitted fact-stream output (e.g. CSS
declaration-values row at `RESULTS.md:94`) from retained internal sidecars,
but the distinction is not yet a manifest field. V2 must add a
`fact_stream_class` column to the Closure Criteria table at `2C:300-306`:

- `admitted_output_plane` — strict comparator, oracle provenance,
  gate-consumed telemetry, row in `RESULTS.md`.
- `internal_sidecar` — REJECT under Lock 1 (substrate union violation).

The Lock 14 v+1 verification command at `2C:354` already scans for grammar-
shape leaks; this fold extends the scan to verify every fact stream has
explicit output-plane provenance.

### F-CH5-V1-03 — Aarch64 `CollapsedStage` admission predicate in 2D

The live predicate at `passes/src/lib.rs:874-876` checks
`target.avx512bw + Entry(_)` only. V2 2D must explicitly:

1. Replace the predicate's `target.avx512bw` test with `target.arch == x86 &&
   target.avx512bw` (or equivalent), refusing aarch64 admission.
2. Add an `aarch64_admits_collapsed_stage` UNKNOWN gate (already partly
   surfaced as UNKNOWN-2D-05) that requires a source-backed aarch64
   strategy from 2E, a same-wave consumer, checkasm differential, and a
   measured row before any aarch64 admission.
3. Bind every `BackendExpr` node / rewrite guard / extraction result to
   declare `substrate_target` per F-CH5-V1-01; e-graph extraction must
   reject plans whose substrate target is not one of the admitted four.

Without this, the cost-model resolver can quietly extract a `CollapsedStage`
plan on aarch64 hosts based on unrelated `avx512bw` indicators inherited
from cross-build target configs, even though no aarch64 lowerer exists.

### F-CH5-V1-04 — Lock 1 amendment from 2F's prev_in_string refutation

LAC-2F-V5-02 (`2F:487`) is currently a per-cycle amendment candidate. V2
should elevate it to T-P3 §3C input so T-P3 emits a Lock 1 substrate-union
v+1 manifest entry:

> "Quote/escape/structural masks consumed cross-call (e.g. simdjson's
> `prev_in_string` parameter) are inadmissible substrate-extensions of the
> tape. Per-call composed forms where carry stays inside one chunk are
> admissible primitives but their SIMD ceiling is capped below the
> cross-call-retained reference. REDRESS 96/97/98 closed the retained
> class-column / streaming-cursor / class-lane-only routes on M5 Max; this
> amendment generalizes the closure to all transient classifier-state
> primitives."

### F-CH5-V1-05 — Compile-time vs runtime split as Lock 1 manifest in 2F

The 2F V5 admission ledger (`2F:470-481`) carries the split per candidate
(`bbnf_regex_hir_engine` = `local_temp_only (compile-time facts)`;
`scan_string_special_block_sweep_64` = `local_temp_only / local_loop`),
but the split is not yet codified as a Lock 1 manifest requirement. V2
must add:

- `crate_target` (one of `compile_time_only`, `build_script_vendor`,
  `runtime_layer1`, `runtime_consumer`).
- Compile-time crates (HIR, scanner plans, byte classes, regex facts) may
  carry state; runtime crates may not retain mask/class streams across
  parser phases.

This is the principled answer to UNKNOWN-2F Q1 (`2F:250`): even if Q1
resolves toward absorbing the `regex-engine.md` pipeline into `bbnf-regex`,
the runtime crate graph must not pull `regex-automata` (per
`regex-engine.md:9` verbatim "There's no dependency on the regex crate at
runtime—only regex-syntax for HIR parsing").

## §4 — CH5 lens checks (substrate-union invariants verified)

The CH5 lens scans for four hidden-coupling failure modes. Each verified
against the V1 cohort:

| failure mode | dossier exposure | verdict |
|---|---|---|
| Parallel substrate (a second tape / cursor / class-column stream that lives alongside the existing tape) | T2A-REF-001 + T2A-LAC-V1-01 (2A); R1 refutation of pixel-domain kernels (2B); fact-stream output plane vs sidecar (2C, fold pending); CollapsedStage transient-only (2D); k-mask substrate-leak callout (2E); prev_in_string refutation (2F) | **PASS** — no dossier grounds a parallel substrate; all retention-class refutations carry REDRESS 96/97/98 anchors |
| Sidecar producer (a SIMD/scanner producer whose output is retained for cross-call consumption rather than folded into the same loop) | Lock 16 admission discipline requires same-wave consumer (2B `:142-149`); orphan-kernel research refused (2A T2A-REF-004 at `:112`); SK-V12 W2 escape-mask parity-only / W4 delimiter find microbench-only halted explicitly cited (2A `:112`, 2B `:62`, 2E `:174-175`); LAC-2F-V5-02 prev_in_string refutation (2F) | **PASS** — every dossier refuses sidecar producers; admission requires same-wave consumer in production |
| Hidden policy coupling (shared crates inheriting grammar-specific constants or callback names) | LAC-2B-03 `policy_owner` field forbids JSON constants in shared call sites (2B `:261`); JSON role mining refuted as fleet-wide (2C Abstract-Primitive-Lift table `:121-145`); `JsonSink` refuted as generic sink (2C `:323`); Layer 1 macros are grammar-neutral with per-grammar LUTs in codegen-emitted `.data` (2B `:117-121`) | **PASS** — grammar policy is mechanically owned by generated/caller-data sources; shared crates do not inherit grammar-specific constants |
| Lock 1 substrate-union violation (a new public substrate or `UnionTape` route) | REDRESS 96/97/98 cited as load-bearing pre-block across 2A, 2D, 2F; T2A-LAC-V1-01 transient-projection clause; LAC-2D-05 material-differential clause for union-shape search (2D `:108`); LAC-2F-V5-02 prev_in_string refutation; LAC-2E-04 PMULL/CSSC Union-C reopen gate (2E `:419`) | **PASS** — no dossier proposes a new public substrate; the REDRESS pre-block ledger is honored across the cohort |

All four CH5 failure modes pass. The 5 fold items above are tightening
requirements, not failure remedies.

## §5 — Cycle disposition for T-P2 V1 CH5

CH5 disposition for T-P2 V1: **ACCEPT-WITH-FOLD (5/6) + REVISE (2D, 1/6)**.

The substrate-union invariant holds across the cohort: every grounded
primitive declares (or in V2 will declare) one of `local_temp_only`,
`existing_tape`, `direct_sink`, or `admitted_fact_output`. The REDRESS
96/97/98 ledger is honored. The simdjson stage-1/stage-2 architecture is
correctly grounded as producer-consumer in one loop, not retained sidecar.
The 2B Layer 0/Layer 1 dependency is mechanically one-directional. The
prev_in_string refutation is load-bearing and elevates to a Lock 1
substrate-union v+1 amendment candidate.

The single REVISE (2D) is narrow: the cost-model dossier's text correctly
refutes the marker-string lowerers and limits `CollapsedStage` to x86-only,
but the live admission predicate at `passes/src/lib.rs:874-876` is not yet
bound by the refutation. V2 must add the aarch64-admission gate and the
per-`BackendExpr`-node `substrate_target` annotation.

If F-CH5-V1-01..05 fold cleanly, CH5 expects to accept V2 at ≥95%.

## §6 — Findings summary

| finding-id | dossier | finding | severity | fold |
|---|---|---|---|---|
| CH5-V1-F01 | 2A | T2A-REF-001 refutes retained sidecar; T2A-LAC-V1-01 codifies transient-projection clause as Lock 1 v+1 candidate | LOAD-BEARING | accept; surface to T-P3 |
| CH5-V1-F02 | 2B | Layer 0/Layer 1 dependency mechanically one-directional with cross-grep proof; 9-primitive table needs per-row `substrate_target` annotation | TIGHTENING | F-CH5-V1-01 |
| CH5-V1-F03 | 2C | Abstract-Primitive-Lift table cleanly separates grammar-neutral vs JSON-only-by-shape; fact-stream output-plane boundary needs manifest field | TIGHTENING | F-CH5-V1-02 |
| CH5-V1-F04 | 2D | Marker-string lowerers refuted; `CollapsedStage` x86-only limited; live `admits_collapsed_stage` predicate at `passes/src/lib.rs:874-876` not yet bound by refutation; `BackendExpr` nodes lack `substrate_target` | LOAD-BEARING REVISE | F-CH5-V1-03 |
| CH5-V1-F05 | 2E | Lock 16 manifest carries `substrate_target`/`retention_lifetime`/`policy_owner`; k-mask substrate-leak explicitly called out as ISA-level parallel substrate that must collapse to `local_temp_only/local_loop` | ACCEPT | surface k-mask rule to T-P3 |
| CH5-V1-F06 | 2F | PTG-PREV-IN-STRING-LOCK1 refutation upholds Lock 1 substrate-union per REDRESS 96/97/98; per-call composed form caps SIMD ceiling below 1 GB/s; LAC-2F-V5-02 elevates to lock amendment | LOAD-BEARING | F-CH5-V1-04 |
| CH5-V1-F07 | 2F | V5 admission ledger carries compile-time vs runtime split per candidate; not yet codified as Lock 1 manifest field for new regex/scanner crate admission | TIGHTENING | F-CH5-V1-05 |

ACCEPT (3): F01, F05, F06.
ACCEPT-WITH-FOLD (3): F02, F03, F07.
REVISE (1): F04.

CH5 V1 cycle disposition: **ACCEPT-class 83% (5/6 dossiers); REVISE 17%
(1/6, 2D)**. Five fold items required for V2 to reach ≥95% ACCEPT.
