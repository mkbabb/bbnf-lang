# CH5 HIDDEN COUPLING — SK-V9 S-P2 Research V1 cohort disposition

Pass: S-P2 Research CHALLENGE. Cycle: V1.
Lens: CH5 — HIDDEN COUPLING (Lock 1 substrate union audit).
Date: 2026-05-18.
Scope: per-report substrate-cardinality audit of the six P2 artefacts
committed at `25d34741`, applying SC-6's cardinality discriminant
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`
§2.2): "the deciding question is single and falsifiable — after the
union lands, is the offset-tape still constructed? … the discriminant
is cardinality." A proposal that holds substrate count at one is
Lock-1-honoured; a proposal that constructs the structural projection
*alongside* the offset-tape raises cardinality to two and replicates
the `Vec<OpenFrame>::clone` pathology in SIMD costume.

---

## §1 — Method (Lock-1 cardinality audit)

### §1.1 — The cardinality predicate, restated for application

Lock 1 (`restart/locks/LOCKS.md:34`) names three failure modes:
(a) orthogonal codepaths, (b) type-ambivalent dual representations,
(c) substrate-first/consumer-later. The 2026-05-04 reframe codifies the
spirit as: *"no parallel substrate; no orthogonal codepath; no
`Vec<OpenFrame>::clone` pathology."* SC-6 §2.2 translates the spirit
into a single falsifiable test:

> After the proposal lands, is *one* object retained as the queryable
> substrate, or are *two* objects (a structural index plus the
> offset-tape) each retained, each consulted independently, each
> writable by the parser? Cardinality at one ⇒ Lock-1 honoured.
> Cardinality at two ⇒ parallel substrate ⇒ Lock-1 violated.

The corollary, from §2.3 reasoning chain #4: a structural index
retained *alongside* a still-built offset-tape is the forbidden case
the amendment must continue to forbid. The lens is the same for SIMD
mask streams (transient producer, fine), sidecar columns (parallel
substrate, forbidden), and renamed scanner products (Lock 1 violation
in costume).

### §1.2 — The seven sub-questions the lens applies

Per the task contract, each P2 report is audited on the applicable
sub-set of:

1. P2-A — does the union event-model replace the offset-tape, or run
   alongside?
2. P2-A — does the co-indexed class column count as a separate
   substrate or as an extension of the offset-tape?
3. P2-B — does `ValueRef` borrow the tape (transient over substrate,
   not sidecar)?
4. P2-B — do proof witnesses honestly stay off the production path
   under `cfg(feature = "proof")` / `cfg(test)`?
5. P2-D — does each ASM kernel avoid creating a parallel producer?
   Specifically: `unescape_uxxxx_x4_neon`, SHA3 EOR3 prefix-XOR fold.
6. P2-E — is the same-wave codec consumer `unescape_string` truly
   the only codec path, or does a second codec run alongside?
7. P2-C — is the admission methodology a pure gate/report layer
   (no substrate change implied)?
8. P2-F — does the three-intervention `>SOTA` path imply a parallel
   substrate at any layer?
9. Track 1 ≡ Track 2 — does any P2 proposal conflate the two tracks
   as a SOTA gate?

### §1.3 — Verdict vocabulary

Each disposition row carries one of: **HONOURED** (Lock-1 cardinality
at one, no coupling risk), **HONOURED-WITH-CONDITION** (cardinality
at one only if the named gate holds in S-P3 / wave dispatch),
**VIOLATION** (cardinality at two, or a sidecar/parallel producer
explicit in the proposal), **DEFERRED** (the proposal does not touch
the substrate; CH5 is silent).

---

## §2 — Disposition table per report

### §2.1 — P2-A: Union event-model

`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md`.

| # | Lens question | Citation | Disposition | Verdict |
|---:|---|---|---|---|
| A.1 | Does P2-A §2.1's "transient producer consumed by move" REPLACE the offset-tape's structural role, or run alongside? | `skv9-p2-A-union-event-model.md:160-188` (§2.1 thesis) + `:182-184` ("The structural index is **consumed by move** during parse and is not retained: it never reaches the view") | The structural index is *not retained*. The parser walks it, dispatches on it, and emits parser-event cursors into the tape; the index itself does not survive parse. The tape carries the retained substrate (offsets + classes + flags + payloads). Cardinality at one: only the tape survives. The framing matches Lock 1's licence at §1.3 ("A SIMD mask stream is a transient producer, not a retained sidecar") — the index is the mask stream, the tape is the substrate. The §2.1 sentence is verbatim: "The structural index is a *transient producer*, exactly as Lock 1 already permits." | HONOURED |
| A.2 | Does the co-indexed `classes: Vec<u8>` column on the tape count as a separate substrate or as an extension of the offset-tape? | `skv9-p2-A-union-event-model.md:192-205` (§2.2 data layout) + `:186-188` ("The tape gains a class column; the offset column's cardinality is unchanged") | The class column is *on the same `Tape<'input>` struct* as the offsets, co-indexed cursor-for-cursor with `offsets: Vec<u32>`. It is not a sibling structure with its own queryable identity. SC-6 §2.2's cardinality discriminant resolves to one substrate: `Tape<'input>` is the one retained object; `classes` is a column inside it, no different than `flag_cursors`/`flag_values` are columns inside it today. The framing matches Lock 1's "one materialisation surface; one Visitor pattern." | HONOURED |
| A.3 | Does the structural alphabet (seven JSON bytes) versus the parser-event class set (seven JSON ordinals) introduce *two* opaque class spaces — i.e., type ambivalence (Lock 1 failure mode (b))? | `skv9-p2-A-union-event-model.md:223-258` (§2.3 producer/consumer class table) | Two class spaces exist, but they are spatially separated: the *structural* class lives only inside `bbnf-simd::StructuralIndex` (the transient producer); the *parser-event* class lives only inside `Tape::classes`. The mapping between them happens at parse-emit time. No retained object carries both. This is *not* type ambivalence (which would be: two retained representations of the same role competing). It is the dav1d/asmjson Layer-1 vs Layer-2 split: scanner classes are codegen data, event classes are tape data. Lock 14 holds because the substrate (`runtime/src/tape/`) stores `Vec<u8>` and exposes `class_at(cursor) -> u8`, never matching on byte (`:208-213`). | HONOURED |
| A.4 | Does the "parser reads `index.positions[i]` … emits class into the tape" sequencing risk drift into a still-built offset-tape sidecar during implementation (SC-6 R1 risk)? | `skv9-p2-A-union-event-model.md:281-302` (§2.5 `consume_structural` removal) + `:430-445` (LOC envelope) | The proposal explicitly states `consume_structural` is *deleted*, not bypassed (line 281: "Today `consume_structural`… is the per-byte scalar rediscovery. Under the alternate model, the parser consults the structural index instead"). The risk register at §6 of SC-6 names exactly this drift (R1); P2-A's LOC table shows `consume_structural` and `at_cursor`'s byte match as *-70 LOC deletion*, with the only retained representation being the new class column. The discipline is correctly named, but the falsifier at §4.4 #1 ("`consume_structural` self-time > 5%") is the post-implementation guard. **Condition**: S-P3's dispatch contract must explicitly forbid retaining `index.positions` past parse-emit; if a future wave retains the index *and* the tape's offsets, cardinality climbs to two. | HONOURED-WITH-CONDITION |
| A.5 | Does the BackendShape five-variant enum gain a sixth variant ("UnionTape") that would split the substrate node? | `skv9-p2-A-union-event-model.md:522-525` (§6 last paragraph: "no new `BackendShape` variant") | P2-A states verbatim: "no new `BackendShape` variant … no new BIR variant … no new directive … no public substrate API … no `UnionTape` public type". This matches SC-6 §3 amendment R2 mitigation. The class column is a *representation refinement* of `OffsetTape`, not a sixth shape. No substrate node multiplies. | HONOURED |
| A.6 | Does the cross-grammar admission section (§3) introduce per-grammar substrate branching that would couple the substrate to grammar identity (Lock 14 cross-coupling)? | `skv9-p2-A-union-event-model.md:303-356` (§3 CSS L4 + Sheets + BBNF-self + empty-alphabet) | The cross-grammar admission keeps the substrate generic. CSS L4 (11 bytes), Sheets (7 bytes), BBNF-self (14 bytes), and empty-alphabet (regex / whitespace-significant) all consume the same `Tape<'input>` shape with the same `classes: Vec<u8>` column; only the per-grammar StructuralAlphabet data table differs. Empty-alphabet routes to `EagerTape` via `derive_backend_shape` step 4 (`:351-356`). No substrate variant per grammar; no `match grammar` arm in generic code. | HONOURED |
| A.7 | Does the §6 REDRESS pre-block citation list (REDRESS 50/51/53/60-72/82/83/84/88/89/92) honestly close all open routes, or are any sidecar routes left unguarded? | `skv9-p2-A-union-event-model.md:451-530` (§6 REDRESS citations) | REDRESS 50 (parser-written aux tables), 51 (parser-local byte-class cursor), 53 (parser-local mask cursor / second scanner), 60-72 (retained-parse sidecar producers) are each addressed with an explicit falsifier ("if a pass other than the parser writes `classes`, this fails REDRESS 50"; "if `ParserState` gains a cursor field other than `state.cursor: usize`…", "if a second `compact_mask`-class call site appears inside the parser…"). The class column is written *only* at the existing `emit_plain_offset` call site, not by a second pass; the producer cardinality is one. The risk-register coverage is complete for the cited routes. | HONOURED |

**P2-A sub-verdict**: HONOURED (with one A.4 condition routed to S-P3
dispatch contract).

---

### §2.2 — P2-B: Retained class/event grammar + ValueRef proof

`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md`.

| # | Lens question | Citation | Disposition | Verdict |
|---:|---|---|---|---|
| B.1 | Does `ValueRef<'tape, 'src, G: EventGrammar>` borrow the *tape* (transient over substrate) or the *source* (sidecar lifetime)? | `skv9-p2-B-retained-grammar-proof.md:124-128` (struct definition) + `:204-238` (§2.1 lifetime semantics) | The struct field is `tape: &'tape Tape<'src>` and the cursor is a `u32` ordinal, not a pointer. The lifetime constraint `'src: 'tape` says the source outlives the tape; the cursor never holds `&'src [u8]` directly; resolution to source bytes happens *through* the tape (`tape.source()`). This is the SC-3 §2.6 no-sidecar-lifetime property verbatim. The 12-byte stack footprint (4 + 8 + 0) confirms it owns nothing. Lock 1's "transient over substrate, not sidecar" predicate is satisfied. | HONOURED |
| B.2 | Does `ValueRef` introduce a second cursor identity (parser-owned alongside the tape's own cursor)? | `skv9-p2-B-retained-grammar-proof.md:241-262` (§2.2 Lock-1 compliance) + `:272-289` (§2.3 what the cursor cannot do) | §2.2.2 verbatim: "It is not a *second* artefact. The cursor is *the cursor of the substrate*." The proof parameterises an *existing* cursor abstraction by a compile-time grammar marker; it does not introduce a new cursor type. The Rename from `K = AnyKind` to `G: EventGrammar = AnyGrammar` is a `PhantomData` marker change only; no layout, no second cursor. §2.3 documents three negative forms (no `'static` cursor, no `Vec<ValueRef>` pool that escapes the tape's lifetime, no source-outliving cursor) — each rejected by the borrow checker. | HONOURED |
| B.3 | Do the proof witnesses (`event_grammar_witness.rs` for JSON + Sheets) honestly stay off the production path, or is there a release-mode leak? | `skv9-p2-B-retained-grammar-proof.md:60-77` (§1.2 owner files table) + `:147-149,:325-326` (witness `cfg` gate) + `:566-573` (R4 risk + mitigation: "`rg 'event_grammar\|event_grammar_witness' skinny/crates/bbnf-bench/` returns zero") | The witnesses are gated behind `#[cfg(any(test, feature = "proof"))]` on the parent `pub mod` declaration in `lib.rs` (§1.2). The proof's R4 mitigation names two layers of defence: (a) the `cfg` gate on the parent module; (b) explicit `rg` verification that `bbnf-bench` does not import them. The `cargo bench -p bbnf-bench` surface cannot reach the witness files. The proof's §5.1 formally dispositions the same-wave-consumer rule: *"the rule binds substrates, not contracts; the proof is a contract, not a substrate; therefore the rule is silent."* This is precise and Lock-1-honest. **Condition**: S-P3 + wave dispatch must verify the `cfg` gate is on the *parent* module (so witness files cannot be selected individually), and CI must include the `rg` audit on every redress commit. | HONOURED-WITH-CONDITION |
| B.4 | Does the witness directory (`grammars/sheets_witness/` or `grammars/css_l4_witness/`) risk reading as the start of a hand-written per-grammar runtime module — Lock 14's *hand-written per-grammar runtime files are forbidden* clause? | `skv9-p2-B-retained-grammar-proof.md:568-570` (R2 risk + mitigation: "`find skinny/crates/runtime/src/grammars/sheets_witness -mindepth 1 -maxdepth 1` returns only `event_grammar_witness.rs` and `mod.rs`") | The R2 mitigation is the explicit `_witness` directory-suffix discipline: the directory name carries the "this is not a runtime module" semantics. The R2 falsifier is a `find` command that asserts no `scan.rs`/`parser.rs`/`generated.rs`/`view.rs` siblings exist. This is a Lock-14 boundary, not a Lock-1 cardinality question, but it is adjacent: a *misread* of the witness as a runtime module would imply a parallel substrate (a hand-written Sheets runtime alongside the generated one). The R2 verification is correct and necessary. | HONOURED |
| B.5 | Does the trait `EventGrammar` add a new public substrate API surface that would expand Lock 1's "one substrate, no orthogonal codepaths" beyond the union? | `skv9-p2-B-retained-grammar-proof.md:79-138` (§1.3 trait sketch) + `:469-507` (§5 what this proof unlocks) + `:565-567` (R1 risk + mitigation) | The trait is deliberately minimal (four members: `STRUCTURAL_CLASS_COUNT`, `FactId`, `admits_fact`, `admits_class`); it has no `step_into`, no `event_kind`, no `class_at`. R1's mitigation: "trait is in `runtime/src/tape/` under a `cfg(any(test, feature = "proof"))` gate; it has no production caller". §5 explicitly states the proof does *not* unlock SC-3 Tier A migration: "It does not unlock: Tier A SC-3 Tier A migration. That still requires its own S-P3 plan with same-wave production consumer." Cardinality at one is preserved because the trait surfaces are non-production at proof depth. | HONOURED |
| B.6 | Does the proof reuse the offset-tape sidecar that REDRESS 50-72 rejected (parser-written aux side tables, sidecar producers)? | `skv9-p2-B-retained-grammar-proof.md:376-465` (§4 differential vs REDRESS 60-72) | §4.1 enumerates every REDRESS 60-72 entry with its rejection reason; §4.2 names the structural differential on five axes (no production consumer, no measurement surface, no edit to `generated.rs`/`scan.rs`/`parser.rs`/`view.rs`/templates, no new BIR variant/directive/BackendShape, cursor unchanged). The differential is correctly stated; REDRESS 71 (the *admitted* schema-source `DirectBuild` typed parser) is correctly distinguished from the rejected family. The proof's `EventGrammar::admits_fact` matrix is the SC-3 §2.3 retained-fact matrix (not REDRESS 71's `DirectBuildField` payload — §6.2 R5 names this distinction). | HONOURED |

**P2-B sub-verdict**: HONOURED (with one B.3 condition on the `cfg`
gate placement + CI `rg` audit).

---

### §2.3 — P2-C: Apache + CITM admission methodology

`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md`.

| # | Lens question | Citation | Disposition | Verdict |
|---:|---|---|---|---|
| C.1 | Does P2-C's admission methodology implicitly require a substrate change (e.g., a new tape column, a new parser path), or is it a pure gate/report layer? | `skv9-p2-C-apache-citm-admission.md:331-355` (§4.1 owner files) + `:495-501` ("Substrate union (Lock 1) is unaffected: the wave produces no new substrate, no sidecar, no parallel tape, no parser-owned cursor") | The owner-file table lists: `report.rs:SK_V8_OPEN_BASELINE` (baseline constants), `gate.rs` regression test (assertion flip), `RESULTS.md` (rendered output), `target/skv9-w{n}/criterion/` (capture artefacts), `REDRESS.md` (new entry), `HANDOFF.md` (candidate row move), `LOCKS.md` (Lock 14 parent-diff allowance scope). Out of scope (explicit): `runtime/`, `bbnf-simd/`, `codegen/` (except byte-identical regen if rerun), `xtask/real_typed_schema.rs`. The methodology is *exclusively* a gate/report layer over an existing typed-DirectBuild path (already W2-admitted at REDRESS 71). No substrate touched. Cardinality unchanged. | HONOURED |
| C.2 | Does the same-run Criterion four-id capture (`track1_real_typed_struct`, `track2_real_typed_struct`, `sonic_rs_real_typed_struct`, `serde_json_real_typed_struct`) introduce a parallel producer at the bench layer? | `skv9-p2-C-apache-citm-admission.md:134-152` (§2.2 same-run anchor) | The four Criterion ids are *measurement contexts*, not parser producers. Each runs the existing parser (generated DirectBuild for track 1; serde for track 2/oracle; sonic-rs for the anchor; serde for the floor). No new parser, no new substrate, no new cursor. The "Track 2 oracle is serde, not a structurally-independent typed parser" wording (§2.4) is honest: serde walks a parser-decoded value stream, the generated DirectBuild visits tape positions — *different implementations of the same job*, not parallel substrates. The Track 1 ≡ Track 2 question (lens sub-question 9) is addressed honestly: P2-C does *not* claim Track 1 ≡ Track 2 as a SOTA gate; it explicitly names sonic-rs strict as the comparator anchor and serde-as-oracle as structurally-different-at-implementation-level. | HONOURED |
| C.3 | Does the row-table admission re-open any of REDRESS 60-72 (retained-parse routes), 82 (W4 unicode quartet), 83 (StringBlock16), 88 (PMULL), 89 (CSSC CTZ bulk)? | `skv9-p2-C-apache-citm-admission.md:447-489` (§6 pre-block risk + REDRESS citations) | §6 enumerates each REDRESS pre-block and names why P2-C does not re-open it: no retained parsing surface, no semantic string facts, no hand-authored typed sink, no parser-owned scratch, no sidecar, no cap-16 extension, no SOTA-close overclaim (admission criterion is *measured-row throughput*, not "SOTA close"). REDRESS 71 (admitted host/API typed DirectBuild from schema) is the *route* P2-C consumes; not a re-open. REDRESS 91 (canada blocker) remains binding ("the wave does not weaken the canada checksum-mismatch route-out"). | HONOURED |
| C.4 | Does the PMU evidence gap (typed track not measured per the V3 probe binding) admit a parallel-measurement pathway that would couple typed admission to a future probe extension? | `skv9-p2-C-apache-citm-admission.md:241-265` (§2.8 PMU evidence for typed track) | The methodology states PMU c/B for typed rows does *not* exist and that this is *not required* for `A / GO` admission because PMU is currently a diagnostic non-producer (`diagnostic_nonproducer_status=...pmu+cycles:nonproducer`). A typed-probe extension is named as *optional* future work, not a hidden coupling. No parallel measurement substrate. | HONOURED |
| C.5 | Does the generalisation to `github_events` / `gsoc-2018` / `instruments` (§5) imply a substrate change for those rows? | `skv9-p2-C-apache-citm-admission.md:395-437` (§5 generalisation) | §5 says generalisation to other rows requires "a host/API typed output schema must exist, and full-fixture DirectBuild-vs-serde parity must pass." That is the REDRESS-71 admitted prerequisite — no substrate change, only schema authoring. Canada (REDRESS 91 blocker), random (synthetic), numbers (stressor), unicode/string corpora (scanner probes) are correctly *excluded* from generalisation. | HONOURED |
| C.6 | Does the §2.3 wave-id bump path ("`SK_V9_W{n}_BASELINE`") risk creating a parallel baseline constant alongside the SK-V8-open baseline — substrate-of-baselines? | `skv9-p2-C-apache-citm-admission.md:154-174` (§2.3 run-id provenance, two paths) | Two paths are offered: co-promotion (rewrite the whole baseline under a fresh run-id) or wave-id bump (new baseline constant, old becomes a named guard table). §2.3 names the latter as "the cleaner architectural path because it separates telemetry-lock recovery (W0) from first behaviour wave (W{n})." The "named guard table" framing is *not* a parallel substrate — it is the explicit handoff from one named run to the next. The baseline constants are documentation, not retained parsing substrate. | HONOURED |

**P2-C sub-verdict**: HONOURED (gate/report layer only; CH5 lens
verdict ACCEPT).

---

### §2.4 — P2-D: aarch64 ASM opportunities

`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md`.

| # | Lens question | Citation | Disposition | Verdict |
|---:|---|---|---|---|
| D.1 | Does the `unescape_uxxxx_x4_neon` consumer wiring create a parallel producer alongside the scalar `read_hex_unit_scalar` (a sidecar codec running alongside the production codec)? | `skv9-p2-D-aarch64-asm-opportunities.md:373-417` (§3.5 wiring proposal) + `:830-839` (§6.4 orphan-rejection rule, same-wave consumer table) | The §3.5 wiring proposal binds the NEON x4 codec into the *union-substrate primary write path* (P2-A scope) — the codec runs *once per retained tape cell*, not alongside the scalar codec. The scalar reference at `parse-that-regex/src/lib.rs:945` becomes the fallback / parity oracle (per the dav1d discipline at §6.3 invariant 1), not a co-running consumer. The same-wave-consumer table at §6.4 names the consumer as "the union-substrate string-content materialiser at the tape-cell projection layer" — one consumer, one producer. No sidecar. **However**: the §3.5 framing depends on P2-A's union substrate landing in the same wave. If the codec ships alone (no union substrate), the consumer would default to `unescape_string` (the existing parser-owned materialiser), and that is exactly the REDRESS-82-rejected "parser-owned per-quartet helper" shape. **Condition**: S-P3 must enforce the same-wave coupling — codec ships only if its union-substrate-consumer ships in the same commit. If sequenced separately, the consumer must explicitly bind to `unescape_string` (P2-E §4.1) with the REDRESS-82 differential gate. | HONOURED-WITH-CONDITION |
| D.2 | Does the SHA3 EOR3 fold for prefix-XOR (§5.3.1) create a *new* producer in `scan_structurals`, or replace the existing scalar prefix-XOR fold? | `skv9-p2-D-aarch64-asm-opportunities.md:636-668` (§5.3.1 VEXT-based prefix mask + EOR3 fold) | §5.3.1 is explicit: "this is a fold of the existing scalar ladder via SHA3, not a substitution with a 64-bit carryless multiply" (line 657-658). The 3 EOR3 ops *replace* the 6-stage scalar XOR ladder at `bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs`. One producer, refined intrinsic. Material differential vs REDRESS 88 named at §5.3.1: PMULL.1Q is 4-cycle latency (the rejected body); SHA3 EOR3 is 1-cycle latency (the proposal). No second prefix-XOR producer; the scalar reference remains as parity oracle per dav1d discipline. | HONOURED |
| D.3 | Does the §5 structural-bitmap chain (dead-SIMD-scanner wiring) introduce a retained second producer of structural positions alongside the offset-tape? | `skv9-p2-D-aarch64-asm-opportunities.md:565-748` (§5 dead-SIMD-scanner wiring) + `:704-721` (§5.4 the wiring is the same-wave consumer) | §5.4 explicitly frames the wiring as *replacing* the recursive-descent parser body with a cursor-over-bitmap walker: "the wave-class change is **substrate**, not parser: the recursive-descent parser is *replaced* by the cursor-over-bitmap walker." This is P2-A's union substrate, re-stated from the ASM angle. The bitmap (structural-index) is consumed by the typed event cursor and projected into `OffsetTape`; one retained object. Material differential vs REDRESS 28 + 33 named at §5.5: REDRESS 28's consumer was the parser's tiny-string dispatch (parser hot loop); SK-V9's consumer is the union substrate (different code path). | HONOURED |
| D.4 | Does the 32-byte string-block scan widening (§4) create a parallel scanner alongside the existing 16-byte `scan_string_special_block`? | `skv9-p2-D-aarch64-asm-opportunities.md:455-498` (§4.2 32-byte widening) + `:516-535` (§4.3 differential vs REDRESS 83) | §4.3 #2 is explicit: "The widening lives in the existing 16-byte primitive's *successor*, not a wrapper. REDRESS 83 was a JSON-specific 16-byte wrapper layered on top; SK-V9's proposal is a 32-byte primitive replacing the 16-byte primitive at the producer site (or a `scan_string_special_block_32` variant called by the existing 16-byte producer when span is long)." The "or a 32-byte variant called by the 16-byte producer when span is long" phrasing admits a *cost-model-derived dispatch* between two block widths — but the dispatch is on a single producer (the string-block scanner), the consumer is the existing `match_string_at_quote_trusted_utf8`, and no retained data structure carries both widths. Cardinality at one. **Note**: the "called by the existing 16-byte producer when span is long" framing is benign as long as it remains a width-dispatch inside one producer and is not allowed to drift to two co-running producers writing different mask streams. **Condition**: S-P3 dispatch contract states the dispatch is internal to the producer, not a second mask-output. | HONOURED-WITH-CONDITION |
| D.5 | Does the CSSC CTZ consumer-side admission at §4.4 (different call site from REDRESS 89's `bulk_emit_positions_64`) reopen the rejected primitive class? | `skv9-p2-D-aarch64-asm-opportunities.md:536-562` (§4.4 consumer-side CSSC CTZ) + REDRESS 89 differential | §4.4 names three differentials: different call site (string-block scanner first-set extract vs `bulk_emit_positions_64`), different failure profile (REDRESS 89 regressed *winning* numeric rows; §4.4 targets *losing* unicode rows under guard), different consumer (union-substrate string-mask consumer vs structural-scan bulk-emit pipeline). The framing is correct — but the §4.4 framing depends on P2-A union substrate. If sequenced separately, the consumer drifts back to the structural-scan path that REDRESS 89 named. **Condition**: same as D.1 — same-wave coupling with P2-A. | HONOURED-WITH-CONDITION |
| D.6 | Does the dav1d-process-discipline gap (§6.2 missing checkasm tests for `unescape_uxxxx`, `string_block`, `digit_mac`, `movemask::movemask_u8x16`) admit a kernel wiring without parity proof — i.e., a producer landing without its scalar oracle? | `skv9-p2-D-aarch64-asm-opportunities.md:778-812` (§6.2 primitives lacking a checkasm gate) | §6.2 names the gap explicitly and the §6.4 orphan-rejection rule binds: "**before wiring any new primitive into a hot path, the primitive ships a `checkasm_<name>.rs` differential test**." This is the dav1d Layer-1 invariant. The gap is *named*, not silently elided. **Condition**: S-P3 dispatch contract requires each §3 / §4 / §5 admission to ship its checkasm test in the same commit. If a kernel wiring lands without its checkasm test, that is the same-wave-consumer violation Lock 1's spirit forbids (substrate-first/consumer-later → kernel-first/parity-later). | HONOURED-WITH-CONDITION |
| D.7 | Does the §6.3 invariant 2-5 gap (missing forced feature masks, ABI shims, recoverable fault handlers, cycle-counter source binding) admit kernel landings that bypass the dav1d discipline at the host level? | `skv9-p2-D-aarch64-asm-opportunities.md:801-820` (§6.3 five-invariant gate, §6.4 admission posture) | §6.3 deferral is explicit: "the fuller invariant 2-5 closure is SK-V10+ work per the SK-V7 A3 §2 menu and skv6-B2; deferring those does **not** block §3/§4 admission because the existing checkasm parity harness covers correctness for the same-class primitives already in tree." Deferral is named, not hidden. CH5 is silent on host-side instrumentation gaps that do not multiply substrates; this is a CH6 (anti-paper-close) and CH4 (cost) concern, not CH5. | DEFERRED |
| D.8 | Do the four still-blocked primitives (`BULK_EMIT_COMPRESSED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, `FSM_DISPATCH_THREADED`) risk landing as orphans (substrate-without-consumer pathology)? | `skv9-p2-D-aarch64-asm-opportunities.md:101-109` (§1.3 measured-but-orphaned) + `:822-829` (§6.4) | §1.3 + §6.4 verbatim: these four primitives stay `blocked_no_consumer` through SK-V9 P2 — they unlock only with a CollapsedStage codegen consumer, which is *out of scope* for SK-V9 (the V9.5 PSI excavation rejected Rust-codegen-of-automata). The orphan-rejection rule is correctly applied. | HONOURED |

**P2-D sub-verdict**: HONOURED (with D.1/D.4/D.5/D.6 conditions all
routing to same-wave coupling discipline in S-P3 dispatch).

---

### §2.5 — P2-E: Unicode-escape codec primitive

`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-E-unicode-escape-codec.md`.

| # | Lens question | Citation | Disposition | Verdict |
|---:|---|---|---|---|
| E.1 | Is the same-wave codec consumer (`unescape_string` per §4.1) the ONLY codec path post-wiring, or does a parallel codec run alongside? | `skv9-p2-E-unicode-escape-codec.md:339-389` (§4.1 JSON consumer) + `:429-440` (§4.3 what the consumer plan does NOT add) | §4.1 binds the codec into `unescape_string` at `parse-that-regex/src/lib.rs:718-810`, replacing the existing `Some(b'u')` arm at lines 775-786. The existing 4-quartet batch path at line 386 (`unescape_four_unicode_escapes`) becomes the 4-quartet binding of the *same* primitive. `decode_unicode_escape` at line 302 calls the same kernel. *Three call sites, one kernel body, one specialisation per binding tuple* (const-generic). §4.3 explicitly forbids: no retained sidecar over `\u` positions ("would violate Lock 1 per CH5 of S-P2 CHALLENGE"); no second escape-classifier scan; no JSON-specific allocator strategy in the codec path. Cardinality at one consumer (per call site, monomorphised by LTO). | HONOURED |
| E.2 | Does the §3 directory module (`crates/bbnf-simd/src/aarch64/escape_codec/`) with five const-generic specialisations (`hex_x4_neon`, `hex_x8_neon`, `hex_variable_neon`, `scalar`, `surrogate_join`) create a parallel-kernel substrate? | `skv9-p2-E-unicode-escape-codec.md:200-220` (§3.1 directory module) + `:319-329` (§3.5 same-arch fallback discipline) | The directory module is one Layer-1 primitive class with multiple codegen-emitted specialisations — exactly the dav1d Layer-1 pattern (one primitive, N host bodies, one scalar reference). Each `*.rs` file is a const-generic-parameterised body of the *same* primitive, not a parallel producer. §3.5: "the kernel is `#[cfg(target_arch = "aarch64")]` gated, with the scalar reference at `escape_codec/scalar.rs` standing as the cross-arch fallback. No runtime feature detection." The compile-time const-generic parameters survive both arms; one kernel, N bindings, one fallback. Cardinality at one. | HONOURED |
| E.3 | Does the §4.2 CSS L4 consumer sketch (Lock-14 demonstration via a second consumer) admit a second runtime path that would couple to a second substrate? | `skv9-p2-E-unicode-escape-codec.md:391-428` (§4.2 CSS L4 consumer sketch) | The CSS L4 sketch binds the codec into `bbnf-css/src/tokenizer/escape.rs::consume_escaped` under a different const-generic parameter tuple (`Range::new(1,6), SurrogatePolicy::None, Terminator::WhitespaceOrNonHex`). The CSS L4 parser is a *separate grammar's parser* — not a second consumer on the *same* substrate, but the SAME primitive used by a different substrate. This is Lock 14's "primitive-shared-across-grammars" model. No Lock-1 substrate cardinality question is raised; it is a CH2 (generality) and CH4 (cost) question. | HONOURED |
| E.4 | Does the surrogate-pair join (§3.4) introduce stateful coupling between two consecutive `\uXXXX` decodes (i.e., a sidecar state machine over the codec)? | `skv9-p2-E-unicode-escape-codec.md:295-318` (§3.4 surrogate-pair join) | The surrogate join is scalar algebra (`0x10000 + ((high - 0xD800) << 10) | (low - 0xDC00)`) running on a *pair of consecutive results* from the codec. No retained state machine; no sidecar; the join algebra is per-pair, not across the corpus. The §3.4 framing "stays scalar (one shift, one subtract, one OR — three ALU ops; this does not vectorise because it depends on a *pair* of consecutive results, not a parallel batch)" is honest. The branchless surrogate detection (range comparators against `0xD800..=0xDBFF` and `0xDC00..=0xDFFF`) is a per-codepoint mask, not a sidecar. | HONOURED |
| E.5 | Does the existing kernel removal at `unescape_uxxxx.rs` (−215 LOC per §7.1) leave any orphan callers that would invoke the *old* kernel alongside the new `escape_codec` directory module — i.e., two codec paths coexisting? | `skv9-p2-E-unicode-escape-codec.md:557-571` (§7.1 LOC envelope, "Existing kernel removal at `unescape_uxxxx.rs` −215") | The LOC table records the existing kernel as *removed* (−215 LOC superseded by `escape_codec/hex_x4_neon.rs`). The §4.1 wiring states the existing 4-quartet batch path "becomes the 4-quartet binding of the same primitive" — i.e., the old call sites migrate to the new kernel; the old kernel is deleted. The R8 risk row at §7.2 names this as LOW correctness risk under checkasm parity. **Condition**: S-P3 dispatch must verify that no caller retains a reference to `bbnf_simd::aarch64::unescape_uxxxx::*` after the wave; if any caller remains, two codec producers coexist. | HONOURED-WITH-CONDITION |
| E.6 | Does the REDRESS 82 differential (§5) honestly distinguish the new design from the rejected single-quartet wrapper, particularly on the substrate-coupling axis? | `skv9-p2-E-unicode-escape-codec.md:441-472` (§5 REDRESS 82 differential) | §5 names five orthogonal axes (shape: primitive class vs classifier wrapper; surface: full hex-decoder vs per-quartet wrapper; genericity: const-generic codegen vs JSON instantiation; consumer cardinality: two grammars vs one; evidence: P1-V3 xctrace vs SK-V6 samply). The substrate-coupling axis (the load-bearing one for CH5) is: REDRESS 82 was a parser-owned per-quartet helper; P2-E is a primitive class with a union-substrate-aware consumer. The framing matches P2-A §2.1 + §2.5 (the structural index is consumed by move; the codec runs at tape-cell projection). | HONOURED |
| E.7 | Does the §6.2 projected Mbps table conflate Track 1 ≡ Track 2 as a SOTA gate (the explicit CH5 lens sub-question 9)? | `skv9-p2-E-unicode-escape-codec.md:498-525` (§6 falsifiability gate) | §6 names per-row thresholds against *sonic-strict* only — the legitimate strict-vs-strict comparator. Track 2 Mbps is named in §6.1 baseline for context (e.g., y_string_unicode Track 2 5,602 vs Track 1 5,428) but is NOT used as a SOTA gate; the gate is `Track 1 ≥ sonic-strict × slack`. No Track 1 ≡ Track 2 conflation. | HONOURED |

**P2-E sub-verdict**: HONOURED (with E.5 condition on orphan caller
audit before wave commit).

---

### §2.6 — P2-F: SOTA teardown for >SOTA path

`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-F-sota-teardown-m5max.md`.

| # | Lens question | Citation | Disposition | Verdict |
|---:|---|---|---|---|
| F.1 | Does the >SOTA three-intervention path (§7: I = union substrate; II = fused unicode codec; III = NEON tiny-string + ASM next-bit) imply a parallel substrate at any layer? | `skv9-p2-F-sota-teardown-m5max.md:441-555` (§7 the >SOTA path) | Intervention I is P2-A verbatim: "`scan_structurals` becomes the one producer of retained structural positions. The `Vec<u32>` of offsets is the tape's offset column; an aligned `classes: Vec<u8>` column records opaque structural-class ordinals." (§7.1). One producer, one substrate. Intervention II is P2-E verbatim with the consumer at the retained-parse string match path AND DirectBuild field-fact emit site (§7.2). One codec primitive, two call sites, one specialisation per binding. Intervention III is P2-D verbatim: `match_tiny_plain_string` cost-fact-gated NEON wiring + `BITMAP_NEXT_SET_BIT` consumer wiring (§7.3). Each kernel ships with its same-wave consumer per Lock 14. The §7.4 sequencing table confirms cardinality at one substrate (the tape) and shows the cumulative impact across row classes. | HONOURED |
| F.2 | Does the §2 corpus partitioning (number-heavy WIN; string/key-dense LOSS; unicode-heavy LOSS; borderline) imply a per-class substrate dispatch — i.e., different substrates for different corpus classes? | `skv9-p2-F-sota-teardown-m5max.md:50-216` (§2 parse-speed per corpus class) | The partitioning is *empirical taxonomy*, not architectural dispatch. The hot leaves differ by corpus class, but the substrate (the tape) is one. The interventions in §7 apply to all rows (some maintain WIN, some close LOSS); no per-class substrate. | HONOURED |
| F.3 | Does the §3 typed-product-plane discussion (where bbnf wins on every admitted row) conflate Track 1 ≡ Track 2 — the explicit CH5 lens sub-question 9? | `skv9-p2-F-sota-teardown-m5max.md:218-289` (§3 node-speed competitive position) | §3 reports Track 1 Mbps only (the bbnf typed parser). Sonic-strict typed is the comparator. Track 2 is not used as a SOTA gate. The "+12.4% per dispatch context" parenthetical on the twitter row is a note on cycle-counter variation, not a Track 1 ≡ Track 2 claim. The "Why bbnf wins on node speed" reasoning (§3 bullets 1-3) names architectural advantages (single allocation, no DOM hand-off, tape-fed structural cursor) — *one* substrate's properties, not a Track 2 conflation. | HONOURED |
| F.4 | Does the §4 asmjson anchor discussion (asmjson as non-anchored sidecar planning signal) honestly disclaim asmjson as a parallel substrate-comparator versus admitting it implicitly? | `skv9-p2-F-sota-teardown-m5max.md:291-325` (§4 the asmjson anchor question) | §4 is explicit: asmjson "cannot stand as a SOTA-beat anchor row in RESULTS.md … be cited as a strict comparator delta … substitute for same-run criterion evidence on this host." It *can* be cited for architectural pattern evidence, cross-ISA architecture lift (for x86 tranche), non-anchored speed ceiling for the Zen-4 follow-up. The classification is correct per the V3-V6 strictness rules (same-host, same-run, strict-plane). No parallel substrate; no implicit anchor. | HONOURED |
| F.5 | Does the §5 architecture-lessons table (simdjson: consume the index; sonic-rs: cost-fact-gated NEON tiny-string; yyjson: `__force_inline` fusion; asmjson: bounded-stack DPDA) propose any pattern that would couple bbnf to a parallel substrate? | `skv9-p2-F-sota-teardown-m5max.md:327-389` (§5 architecture lessons) | The simdjson lesson is "consume the index that scan_structurals already produces" — *removing* the discard, *retaining* the index INSTEAD of constructing a separate offset-tape. This is P2-A union substrate; cardinality unchanged. The sonic-rs lesson is cost-fact-gated NEON kernel selection — primitive admission, not substrate change. The yyjson lesson is codegen-emitted fusion of unicode decode into the string-walk loop — P2-E codec; consumer in `unescape_string`, not a sidecar. The asmjson lesson is bounded-stack DPDA — *correctly* dispositioned as out-of-scope for SK-V9 (the V9.5-PSI binding rejected Rust-emitted DPDAs; admission only via hand-authored aarch64 NASM, deferred to SK-V7 Wave 3 successor). No parallel substrate implied. | HONOURED |
| F.6 | Does the §6 dav1d-process-discipline recap admit a coupling between the bbnf-simd checkasm harness and a sidecar test substrate? | `skv9-p2-F-sota-teardown-m5max.md:391-438` (§6 dav1d/FFmpeg/VLC discipline recap) | §6 names the four-tuple commit discipline ((scalar reference, checkasm test, hot-path consumer, ABI-hardened ASM)) as the gold standard. The bbnf-simd `checkasm_parity.rs` harness is identified as "the closest in-tree analogue to the dav1d discipline." The discipline is correctly applied; no sidecar test substrate (the checkasm harness is *one* parity oracle per primitive, not a parallel test substrate). | HONOURED |
| F.7 | Does the §7.4 sequencing-and->SOTA-target framing imply a parallel-target gate (multiple SOTA gates running in parallel) versus a single ordered close criterion? | `skv9-p2-F-sota-teardown-m5max.md:534-554` (§7.4 sequencing and >SOTA target) | The sequencing is "Intervention I → II → III"; each ships with its same-wave consumer and scalar oracle parity. The >SOTA gate is one criterion: "strictly above sonic-rs + simdjson NEON + yyjson on every row, on both parse_only and real_typed_struct planes, with matched RFC-8259 strictness, with same-run criterion evidence." Three comparators, one gate. asmjson remains a non-anchored sidecar (correctly outside the gate). Track 1 ≡ Track 2 is not used; only Track 1 versus strict comparators. No parallel-target gate. | HONOURED |

**P2-F sub-verdict**: HONOURED (no parallel substrate; the >SOTA path
is the integrated P2-A + P2-E + P2-D synthesis at cardinality one).

---

## §3 — Aggregate verdict

### §3.1 — Per-report verdict summary

| Report | HONOURED rows | HONOURED-WITH-CONDITION rows | VIOLATION rows | DEFERRED rows | Sub-verdict |
|---|---:|---:|---:|---:|---|
| P2-A — Union event-model | 6 | 1 | 0 | 0 | HONOURED |
| P2-B — Retained grammar proof | 5 | 1 | 0 | 0 | HONOURED |
| P2-C — Apache + CITM admission | 6 | 0 | 0 | 0 | HONOURED |
| P2-D — aarch64 ASM opportunities | 3 | 4 | 0 | 1 | HONOURED |
| P2-E — Unicode-escape codec | 6 | 1 | 0 | 0 | HONOURED |
| P2-F — SOTA teardown / >SOTA path | 7 | 0 | 0 | 0 | HONOURED |
| **Cohort total** | **33** | **7** | **0** | **1** | **HONOURED** |

41 dispositions over six reports (≥30 required by the task contract;
≥5 per report enforced — minimum is P2-D at 8, including one
DEFERRED). Zero VIOLATIONS. Seven HONOURED-WITH-CONDITION rows route
to S-P3 dispatch contract.

### §3.2 — Lock-1 cardinality verdict for the cohort

Every P2 proposal that touches the substrate holds substrate
cardinality at one. The structural index in P2-A is a transient
producer consumed by move; the class column is a co-indexed column on
the existing `Tape<'input>` struct, not a sibling structure; the
`ValueRef` cursor in P2-B borrows the tape (transient over substrate);
the proof witnesses are `cfg`-gated off the production path; the P2-C
admission methodology is a pure gate/report layer; the P2-D ASM
kernels are scalar-oracle-backed Layer-1 primitives whose consumers
are P2-A's union substrate (codec at tape-cell projection;
structural-bitmap at typed event cursor; tiny-string equality at
dispatch); the P2-E codec is one primitive class with five const-generic
specialisations and three call sites, all monomorphised by LTO; the
P2-F >SOTA path is the integrated P2-A + P2-E + P2-D synthesis at
cardinality one.

No proposal conflates Track 1 ≡ Track 2 as a SOTA gate. Every gate is
Track 1 versus strict-comparator (sonic-rs strict primarily,
simdjson NEON and yyjson as additional comparators in P2-F).
Track 2 / serde is named as oracle (P2-C) or guard (P2-A) but never
as the close criterion.

The CH5 cohort verdict is **ACCEPT** for V1. The seven
HONOURED-WITH-CONDITION rows do not block acceptance; they route to
S-P3 dispatch as explicit same-wave coupling discipline (the dav1d
four-tuple commit shape — primitive + scalar reference + checkasm
parity + consumer wiring in one commit, never deferred).

### §3.3 — Where SC-6 R1 (sidecar drift during implementation) would fire

The audit confirms SC-6 §6 R1 is *correctly named* in the cohort: the
union concept is singular substrate; the implementation risk is an
interim consumer that retains both representations. P2-A §6 names the
falsifier explicitly ("the offset-tape constructor and old offset-append
API are *deleted*, not merely *unused*"). The cohort-level guard is the
seven HONOURED-WITH-CONDITION conditions — they are the implementation
discipline the V1 design declares but does not yet enforce.

---

## §4 — Specific coupling risks requiring V2 fold

The CH5 lens surfaces seven specific risks that S-P2 V1 declares
correctly at the design level but that require explicit S-P3 / wave
dispatch discipline to enforce. Each is named here with its V2-fold
recommendation.

### §4.1 — Risk R-CH5-1: Sidecar drift in P2-A implementation

**Disposition row**: A.4 (HONOURED-WITH-CONDITION).
**Risk**: An interim consumer that was written against the old
offset-tape API forces the implementation to *also* materialise the
old offsets alongside the union substrate — recreating the parallel
substrate.
**V2-fold**: S-P3 must declare the deletion order verbatim:
`consume_structural` deletion (`-50 LOC regen`), `at_cursor`
byte-match deletion (`-15 LOC regen`), `push_plain_offset` API
deletion (`-20 LOC source`). The wave dispatch's redress phase must
verify via `rg` that no caller references the deleted APIs after the
commit. SC-6 §6 R1 mitigation language ("a `rg`/code-review check
that the offset-tape constructor and old offset-append API are
*deleted*, not merely *unused*") is the verbatim folding target.

### §4.2 — Risk R-CH5-2: Proof witness cfg-gate placement

**Disposition row**: B.3 (HONOURED-WITH-CONDITION).
**Risk**: If `#[cfg(any(test, feature = "proof"))]` is applied at the
individual witness file level rather than at the parent `pub mod` in
`lib.rs`, a future agent enabling a different feature could pull in
one witness without the gate.
**V2-fold**: S-P3 wave dispatch states the `cfg` gate is at the
parent `pub mod` in `runtime/src/lib.rs` only; per-file `cfg` gates
are not admissible. CI must include the `rg 'event_grammar|event_grammar_witness'
skinny/crates/bbnf-bench/` audit as a green-on-zero-hits assertion.

### §4.3 — Risk R-CH5-3: Codec consumer drift if P2-D §3 ships before P2-A

**Disposition row**: D.1 (HONOURED-WITH-CONDITION).
**Risk**: The P2-D §3 framing depends on P2-A's union substrate
being the consumer. If the codec ships in a wave that does NOT
include the union substrate, the only consumer available is
`unescape_string` (the existing parser-owned materialiser), and the
wiring becomes the REDRESS-82-rejected parser-owned per-quartet
helper.
**V2-fold**: S-P3 sequencing per P2-F §7.4 (I → II → III) is the
correct ordering — the union substrate (I = P2-A) must ship first.
If P2-E's codec ships *with* P2-A's substrate, the consumer is the
union-substrate tape-cell projection. If P2-E's codec ships *without*
P2-A (a later wave), the consumer must explicitly bind to
`unescape_string` per P2-E §4.1, AND the REDRESS-82 differential at
P2-E §5 must be the wave's admission gate. S-P3 must declare which
of the two consumer bindings applies, not leave it ambiguous.

### §4.4 — Risk R-CH5-4: 32-byte string-block width-dispatch drift

**Disposition row**: D.4 (HONOURED-WITH-CONDITION).
**Risk**: P2-D §4.2's framing admits two dispatch shapes: (a) a
32-byte primitive *replacing* the 16-byte primitive; (b) a 32-byte
variant *called by* the 16-byte producer when span is long. Shape
(b) is benign if the dispatch is internal to one producer; it
becomes a parallel producer if both widths are allowed to write
distinct mask streams that survive into the consumer.
**V2-fold**: S-P3 must declare which dispatch shape lands. If (a),
the 16-byte producer is deleted (one producer, one width). If (b),
the dispatch lives inside `scan_string_special_block`'s body and
returns *one* `StringSpecialBlock` per call (different lane count
under the hood, but one external mask). Two mask outputs from a
single call site are forbidden.

### §4.5 — Risk R-CH5-5: CSSC CTZ consumer drift

**Disposition row**: D.5 (HONOURED-WITH-CONDITION).
**Risk**: Same shape as D.1 — the consumer is the union-substrate
string-mask path (P2-A scope), not REDRESS-89's `bulk_emit_positions_64`.
If P2-A does not ship in the same wave, the only consumer available
is the structural-scan bulk-emit pipeline that REDRESS 89 named.
**V2-fold**: Same as R-CH5-3 — S-P3 must declare same-wave coupling
with P2-A. The §4.4 admission language ("targets LOSS rows under
guard") is correct but requires the union-substrate consumer to
hold.

### §4.6 — Risk R-CH5-6: Checkasm parity for kernels lacking tests

**Disposition row**: D.6 (HONOURED-WITH-CONDITION).
**Risk**: P2-D §6.2 names four kernels lacking checkasm tests
(`unescape_uxxxx`, `string_block`, `digit_mac`, `movemask`). If any
of these is wired into a hot path without its checkasm test landing
in the same commit, the dav1d Layer-1 invariant ((scalar reference,
checkasm test, hot-path consumer, ABI-hardened ASM)) is broken.
That is the same-wave-consumer rule applied at the *kernel parity*
layer — primitive-without-parity is the equivalent of
substrate-without-consumer.
**V2-fold**: S-P3 must declare per-kernel that the missing
checkasm test lands in the same commit as the consumer wiring. The
`crates/bbnf-simd/tests/checkasm_<name>.rs` filename pattern is the
contract; CI must enforce that every `aarch64/<name>.rs` body has a
matching `tests/checkasm_<name>.rs` test before any caller in
`generated.rs` / `parser.rs` / `unescape_string` references the
NEON entry point.

### §4.7 — Risk R-CH5-7: Codec-kernel removal orphan callers

**Disposition row**: E.5 (HONOURED-WITH-CONDITION).
**Risk**: The −215 LOC kernel removal at `unescape_uxxxx.rs`
(superseded by `escape_codec/hex_x4_neon.rs`) leaves any orphan
caller that references the old kernel as a parallel codec producer
alongside the new one.
**V2-fold**: S-P3 wave dispatch states the removal verification:
`rg 'bbnf_simd::aarch64::unescape_uxxxx::' skinny/crates/`
must return zero matches after the wave commit, except inside the
`escape_codec/` directory itself (re-exports during migration are
admissible only inside the new owner). The verification is part of
the same-wave admission gate.

### §4.8 — V2-fold synthesis

The seven risks share one shape: *the V1 designs are Lock-1-honoured
at the architectural level, but Lock-1 implementation discipline is
declared as project-level intent rather than as per-wave dispatch
contract.* The S-P2 V2 fold should not redesign anything; it should
**propagate the seven HONOURED-WITH-CONDITION conditions into the
S-P3 plan's dispatch contract**, where each becomes an explicit
falsifier on the wave's redress phase. The dav1d four-tuple commit
shape (primitive + scalar reference + checkasm parity + consumer
wiring) is the recurring V2-fold target across the seven risks.

The cohort is admissible at V1. The S-P3 plan must carry the seven
conditions verbatim into its wave-dispatch contracts; without that
propagation, the V1 design declares the discipline that V2 / S-P3
must enforce.

---

## §5 — Sources

- `restart/locks/LOCKS.md:34` — Lock 1 verbatim (substrate union; spirit at the 2026-05-04 reframe).
- `restart/locks/LOCKS.md:60` — Lock 14 verbatim (grammar generalisation; ZERO grammar-specific code in generic crates).
- `restart/locks/LOCKS.md:69-94` — Lock 16 verbatim (SIMD/ASM admissibility allowlist; Layer-0/Layer-1 vocabulary).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md` §§1-7 — the SC-6 cardinality test, Lock-1-amendment proposal, StructuralAlphabet generalisation, capability-dispatched primitive layer, risk register R1-R6.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md` — the union-substrate design SC-6 ratifies.
- `restart/prompts/ORCHESTRATOR.md` §3W — the CH5 HIDDEN COUPLING lens definition.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md` — the cohort's P2-A artefact.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md` — the cohort's P2-B artefact.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md` — the cohort's P2-C artefact.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md` — the cohort's P2-D artefact.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-E-unicode-escape-codec.md` — the cohort's P2-E artefact.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-F-sota-teardown-m5max.md` — the cohort's P2-F artefact.
- `skinny/REDRESS.md` Items 28, 33, 50-72, 82-89, 91-93 — pre-blocked routes the cohort honours.
