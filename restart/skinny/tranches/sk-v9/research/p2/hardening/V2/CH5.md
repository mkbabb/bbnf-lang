# CH5 HIDDEN COUPLING — SK-V9 S-P2 Research V2 verify

Pass: S-P2 Research CHALLENGE. Cycle: V2 (candidate second consecutive
ACCEPT cycle per ORCHESTRATOR §3Z; V1 was cycle 1).
Lens: CH5 — HIDDEN COUPLING (Lock 1 substrate-union cardinality audit).
Date: 2026-05-18.
Scope: verify the V2 fold (commit `c6de46a5`) addresses the seven
HONOURED-WITH-CONDITION rows from V1 CH5 (R-CH5-1 through R-CH5-7), and
re-audit the six folded reports for any newly-introduced parallel
substrate, sidecar, or Track-1≡Track-2 dishonesty.

V1 CH5 verdict: ACCEPT — 33 HONOURED + 7 HONOURED-WITH-CONDITION + 0
VIOLATION + 1 DEFERRED (41 dispositions). The V1 lens found zero
cardinality-two designs; the seven conditions were implementation
discipline declared as project intent rather than per-wave dispatch
contract. CH5 is the only one of the six S-P2 lenses that already
converged at V1; this V2 verify confirms the fold did not regress it.

---

## §1 — Seven-condition resolution

The cardinality discriminant (SC-6 §2.2): after the proposal lands, is
*one* object retained as the queryable substrate, or *two*? V1 found
cardinality one across all six reports. The seven conditions each named
an implementation-phase drift that, if uncaught, would climb cardinality
to two. The V2 fold's task on each was *not* a redesign — it was to
promote the condition from prose into a wave-dispatch falsifier. Each is
verified below against the folded report text.

### §1.1 — R-CH5-1: P2-A `consume_structural` deletion verification

**V1 row**: A.4 (HONOURED-WITH-CONDITION). **Risk**: an interim consumer
written against the old offset-tape API forces the implementation to
*also* materialise the old offsets — recreating the parallel substrate.

**V2 fold — RESOLVED.** P2-A §2.5
(`skv9-p2-A-union-event-model.md:347-376`) now carries the deletion at
an *explicit, citable* coordinate. The function is named at
`generated.rs:292-306` (verified against the live tree:
`skinny/crates/runtime/src/grammars/json/generated.rs:292-306` holds
`fn consume_structural(state, byte) -> Option<u32>` exactly — the cite
is byte-accurate, not approximate). The V1 row cited `:281` and
`:430-445`; the V2 fold tightens this to the precise 15-line span and
adds the codegen-template coordinate
(`json_templates/parser.rs`). The R-CH5-1 falsifier is stated verbatim
(`:368-376`): the production hot path contains zero callers after the
wave commit; verified by `rg -n 'consume_structural'
skinny/crates/runtime/src/` returning zero outside the deletion diff,
AND `rg -n 'consume_structural' skinny/crates/codegen/src/` returning
zero after regen. The deletion-of-emission (not deletion-of-output) is
the correct Lock-1 falsifier — a regen that re-emits the function would
fail the codegen-side `rg`. Cardinality stays at one: the SIMD scanner
emits the index, the parser consumes by move, no second producer of
structural positions. **The condition is folded as a dual-`rg` wave
gate.**

### §1.2 — R-CH5-2: P2-B proof-witness cfg gate at parent `pub mod`

**V1 row**: B.3 (HONOURED-WITH-CONDITION). **Risk**: a per-file
`#[cfg(...)]` lets a future agent enabling a different feature pull one
witness onto the production path.

**V2 fold — RESOLVED.** P2-B §1.2 owner-files table
(`skv9-p2-B-retained-grammar-proof.md:68`) now states the gate location
explicitly and rejects the alternative: "The `cfg(any(test, feature =
"proof"))` gate is applied **once, at the parent `pub mod` …
declaration in `lib.rs`** — never per-file inside the witness modules
themselves. Per-file gating is rejected: it produces duplicated
attributes, makes feature drift silent (one file gated, sibling
un-gated), and breaks `rg` audits keyed on the parent declaration
line. R-CH5-2 binds the gate to the parent `pub mod` site." The
condition is named by its R-id inside the report — the fold is
traceable. The witness module docstrings (`:148-150`) restate the
gate. Cardinality at one is preserved: the witnesses are non-production
at proof depth; the `EventGrammar` trait has no production caller. **The
condition is folded as an explicit gate-placement rule + R-id binding.**

### §1.3 — R-CH5-3: P2-D §3 codec consumer drift bound to P2-A

**V1 row**: D.1 (HONOURED-WITH-CONDITION). **Risk**: if the codec ships
in a wave without the union substrate, the only consumer is
`unescape_string` (parser-owned materialiser) — the REDRESS-82-rejected
per-quartet helper shape.

**V2 fold — RESOLVED.** P2-D `:453-462` adds a named "Same-wave consumer
binding (CH3 / no-orphan)" paragraph: "the §3 codec broadening's
same-wave consumer is the **P2-A union substrate** typed-event / tape
consumer. The codec proposal **blocks on P2-A landing in the same wave
OR fails CH5**: if P2-A doesn't land simultaneously, the codec
broadening ships as a primitive without its production consumer — a
REDRESS-82-style orphan — and must be held back." This is the V1
V2-fold recommendation verbatim — the codec ships only with P2-A, and
absent P2-A the broadening "only reduces fall-through traffic in the
*parser-owned* helper, which is the shape REDRESS 82 rejected." The
fold makes the orphan condition a *blocking* precondition, not advisory
guidance. **The condition is folded as a hard same-wave block.**

### §1.4 — R-CH5-4: P2-D §4.2 32-byte block-scan width-dispatch

**V1 row**: D.4 (HONOURED-WITH-CONDITION). **Risk**: P2-D §4.2 admitted
two dispatch shapes — (a) a 32-byte primitive *replacing* the 16-byte
one; (b) a 32-byte variant *called by* the 16-byte producer when span
is long. Shape (b) becomes a parallel producer if both widths write
distinct mask streams that survive into the consumer.

**V2 fold — RESOLVED, with a residual note.** P2-D §4.3
(`:626-637`) retains the dual phrasing at #2 ("a 32-byte primitive
replacing the 16-byte primitive at the producer site (or a
`scan_string_special_block_32` variant called by the existing 16-byte
producer when span is long)") — the fold did *not* strip the
ambiguity at the design layer. **However**, §4.3 #3 closes the
cardinality question regardless of which shape lands: "Same-wave
consumer is the existing `match_string_at_quote_trusted_utf8`, not a
new wrapper. No sidecar primitive, no parallel substrate (Lock 1
compliance per SC-6)." The §4.3 LOC slice table names exactly one
external consumer (`match_string_at_quote_trusted_utf8` at
`parse-that-regex/src/lib.rs:162`) and one mask product per call. The
"successor, not wrapper" framing at #2 is the Lock-1-honest reading:
shape (b) is a width-dispatch *internal* to one producer body, not two
co-running producers. The V1 V2-fold recommendation ("S-P3 must declare
which dispatch shape lands") is satisfied at the cardinality level —
the report binds *both* shapes to one external consumer and one mask
output — and the final shape selection is correctly deferred to S-P3
with the cost-set ("LOC and risk are preliminary; the final cost-set is
authored by S-P3"). Cardinality at one holds under either shape. **The
condition is folded as a one-consumer / one-mask binding; shape
selection routes to S-P3 — acceptable, because cardinality does not
depend on the shape.**

### §1.5 — R-CH5-5: P2-D §4.4 CSSC CTZ consumer bound to P2-A

**V1 row**: D.5 (HONOURED-WITH-CONDITION). **Risk**: same shape as
R-CH5-3 — if P2-A does not ship the same wave, the only consumer is
REDRESS-89's `bulk_emit_positions_64` structural-scan bulk path.

**V2 fold — RESOLVED.** P2-D §4.4 (`:692-699`) now states: "**This
slice blocks on P2-A landing** — the string-mask consumer that makes
the CTZ extract non-orphan is P2-A union-substrate scope; absent P2-A
in the same wave, this slice does not ship." The risk class is named
HIGH (`:691`) precisely because REDRESS 89 rejected the structurally
adjacent body; the falsification gate against the W10b six-row WIN
block (`canada, citm_catalog, instruments, marine_ik, mesh, numbers`)
is named a "hard blocking precondition." The three-axis differential
(different call site / different failure profile / different consumer)
is retained from V1. The fold converts the V1 advisory ("same as
R-CH5-3") into a hard same-wave block with a named six-row guard.
**The condition is folded as a hard same-wave block + named WIN-row
falsifier.**

### §1.6 — R-CH5-6: P2-D §6.2 missing checkasm tests — dispatch ownership

**V1 row**: D.6 (HONOURED-WITH-CONDITION). **Risk**: a kernel wired
into a hot path without its checkasm test in the same commit breaks the
dav1d Layer-1 four-tuple — kernel-first/parity-later is the
substrate-first/consumer-later pathology at the kernel layer.

**V2 fold — RESOLVED.** P2-D adds a *new* sub-section §6.2.1 "Dispatch
ownership for the missing checkasm tests" (`:1010-1024`). Each of the
five named gaps (`checkasm_unescape_uxxxx`, `checkasm_string_block`,
`checkasm_match_tiny_plain_string`, `checkasm_digit_mac`,
`checkasm_movemask`) is assigned an explicit authoring wave with a
rationale. The admission rule is stated verbatim: "the wave that
broadens / widens / wires the primitive authors the primitive's
checkasm test as part of the same wave — the test is a precondition of
the broadening, not a follow-up." `checkasm_digit_mac` is correctly
held as carried-forward (not dropped) because `digit_mac` has no §3-§5
consumer this wave — assigning its test to a no-consumer wave would
itself be a paper-close. This is the V1 V2-fold recommendation
("S-P3 must declare per-kernel that the missing checkasm test lands in
the same commit") promoted into a per-kernel ownership table. **The
condition is folded as a new §6.2.1 ownership table — no test left
ownerless.**

### §1.7 — R-CH5-7: P2-E kernel-removal orphan-caller audit

**V1 row**: E.5 (HONOURED-WITH-CONDITION). **Risk**: the −215 LOC
removal of `unescape_uxxxx.rs` leaves an orphan caller referencing the
old kernel — two codec producers coexisting.

**V2 fold — RESOLVED, with a process-discipline observation.** P2-E §7.1
slice S11 (`skv9-p2-E-unicode-escape-codec.md:731`) makes the removal
an *explicit ordered slice*: "Existing kernel removal at
`unescape_uxxxx.rs` (superseded by S2) … Lands LAST, only after S7 is
green … the removal is the consumer migration." The §7.1 net-LOC line
(`:736`) records `−215` as the superseded kernel + W4-attempt residue.
The slice ordering is the load-bearing fold: S7 (the production
consumer migration onto the new kernel) must be green *before* S11
deletes the old kernel — so at no point do two codec producers coexist
with live callers; the old kernel is dead before it is removed. This is
the correct Lock-1 sequencing (consumer-migration-before-substrate-
removal). **Observation**: the V1 V2-fold recommendation named an
explicit verification command (`rg 'bbnf_simd::aarch64::unescape_uxxxx::'
skinny/crates/` returns zero after the wave). The folded P2-E §7.1
expresses the discipline via *slice ordering* (S11-after-S7) rather
than via that named `rg` gate. The slice ordering is sufficient for
cardinality — S7 migrates every caller, S11 deletes the now-dead
kernel — but the explicit post-wave `rg` falsifier would be a tighter
gate. This is a paragraph-level tightening for S-P3's dispatch contract,
not a CH5 violation: cardinality at one is structurally guaranteed by
the ordering. **The condition is folded as ordered slices S7→S11;
recommend S-P3 add the named `rg` falsifier to the wave gate.**

### §1.8 — Seven-condition resolution summary

| R-id | V1 row | V2 fold location | Resolution |
|---|---|---|---|
| R-CH5-1 | A.4 | P2-A §2.5 `:347-376` | RESOLVED — dual-`rg` gate, byte-accurate `:292-306` cite |
| R-CH5-2 | B.3 | P2-B §1.2 `:68` | RESOLVED — parent `pub mod` gate, per-file rejected |
| R-CH5-3 | D.1 | P2-D `:453-462` | RESOLVED — hard same-wave block on P2-A |
| R-CH5-4 | D.4 | P2-D §4.3 `:626-637` | RESOLVED — one-consumer/one-mask binding; shape→S-P3 |
| R-CH5-5 | D.5 | P2-D §4.4 `:692-699` | RESOLVED — hard same-wave block + WIN-row guard |
| R-CH5-6 | D.6 | P2-D §6.2.1 `:1010-1024` | RESOLVED — new ownership table, no ownerless test |
| R-CH5-7 | E.5 | P2-E §7.1 S11 `:731` | RESOLVED — ordered S7→S11; recommend named `rg` gate |

Seven of seven RESOLVED. Zero conditions regressed to VIOLATION. R-CH5-7
carries one paragraph-level tightening recommendation (named `rg`
falsifier) that does not block — the slice ordering already guarantees
cardinality one.

---

## §2 — V2 dispositions

The V2 verify re-audits the six folded reports for *newly-introduced*
coupling. The V1 fold inputs were F1-F6 (the consolidated load-bearing
defects). CH5's concern is narrow: did any F1-F6 fold introduce a
sidecar, a parallel substrate, a second producer, or a Track-1≡Track-2
conflation that the V1 lens did not see? Each disposition below is one
fold-delta examined.

### §2.1 — P2-A folds

| # | V2 fold delta | Citation | Verdict |
|---:|---|---|---|
| V-A.1 | §0 declares the V2 fold scope (F4 per-slice cost + F5 Lock-14 + CH3/CH5 REVISEs). The §0 line "substrate cardinality discipline reinforced" — does the reinforcement add a substrate? | `:5-6` | No substrate added. §0 is a scope declaration; the reinforcement is the §2.5 falsifier tightening (R-CH5-1). Cardinality unchanged. **HONOURED** |
| V-A.2 | §2.5 the `consume_structural` deletion cite tightened from `:281`/`:430-445` to the precise `:292-306` span. Does the tighter cite imply a different deletion target? | `:349-353` vs live `generated.rs:292-306` | The cite is now byte-accurate (verified against tree). Same function, same deletion. No new substrate. **HONOURED** |
| V-A.3 | §2.5 adds the dual-`rg` falsifier (runtime + codegen). Does the codegen-side `rg` imply a second codegen path? | `:368-376` | One codegen template (`json_templates/parser.rs`); the `rg` asserts the template *loses* the emission. One producer, one template. **HONOURED** |
| V-A.4 | §2.5 generic-template / JSON-codegen-output naming paragraph (F5 Lock-14). Does naming `walk_container_at_class` vs `parse_object` split the walker into two? | `:391-400` | One walker; two *names* for one symbol — the generic template form and the JSON codegen output. The generic substrate sees only the primitive class + ordinal. No second walker, no second substrate. **HONOURED** |
| V-A.5 | F4 per-slice cost discipline — eight intervention slices gain minute caps + revert (CH4 fold). Does any slice introduce a sidecar producer? | §5 slice table (A.1-A.8) | The slice table is a cost decomposition of one design; each slice is a part of the single union-substrate landing. Slice A.3 (`consume_structural` removal + emit-site class write) is the deletion + the one class-column write. No slice constructs a parallel substrate. **HONOURED** |
| V-A.6 | The §6 falsifier "`consume_structural` self-time > 5%" retained from V1 — does the F4 fold weaken it? | `:522` | Falsifier retained verbatim; F4 added cost discipline *around* it, not in place of it. **HONOURED** |
| V-A.7 | §3 cross-grammar admission retained; F5 added Lock-14 prose. Does the prose add per-grammar substrate branching? | §3 (CSS L4 / Sheets / BBNF-self / empty-alphabet) | Generic substrate unchanged; only the per-grammar StructuralAlphabet data table differs. No `match grammar` arm. Cardinality one across all four grammars. **HONOURED** |

### §2.2 — P2-B folds

| # | V2 fold delta | Citation | Verdict |
|---:|---|---|---|
| V-B.1 | §0 declares fold scope (AnyGrammar + per-slice cost + cfg-gate location). | `:6` | Scope declaration; no substrate. **HONOURED** |
| V-B.2 | §1.2 owner-files table — the cfg-gate-location text added (R-CH5-2). Does the rejection of per-file gating add a file? | `:68` | The text *removes* a degree of freedom (per-file gating); it adds no file. The witness file count is unchanged. **HONOURED** |
| V-B.3 | The third witness line `_proof_compiles::<AnyGrammar>` (F5 AnyGrammar declaration). Does `AnyGrammar` become a third retained substrate? | `:298-301` | `AnyGrammar` is the empty-grammar `EventGrammar` instance — a compile-only proof witness, not a runtime substrate. It proves the trait admits the no-structural-facts case. `cfg`-gated like the other two witnesses. No retained object. **HONOURED** |
| V-B.4 | The witness directory naming (`*_witness`) retained from V1. Does the AnyGrammar addition need a third witness directory? | `:64-67` | `AnyGrammar` is declared in `tape/event_grammar.rs` (the trait's own file), not a new grammar directory — it is grammar-*absent*, so no `grammars/<name>_witness/` dir. No new directory. **HONOURED** |
| V-B.5 | F4 per-slice cost — P2-B already carried slice caps at V1 (the consolidated F4 notes "all reports except P2-B"). Does the V2 fold touch P2-B's slices? | F4 line in consolidated | P2-B's slice discipline was already present; V2 added only the AnyGrammar line + the cfg-gate text. No slice-level substrate change. **HONOURED** |
| V-B.6 | The `EventGrammar` trait surface — four members, unchanged from V1. Does AnyGrammar require a fifth member? | `:79-138` trait sketch | `AnyGrammar` satisfies the existing four-member trait with `STRUCTURAL_CLASS_COUNT = 0` / empty `admits_*`. No new trait member; no new public API. **HONOURED** |

### §2.3 — P2-C folds

| # | V2 fold delta | Citation | Verdict |
|---:|---|---|---|
| V-C.1 | F4 per-slice LOC break-out (V1 was aggregate-only). Does the break-out introduce a parallel measurement substrate? | §4 owner-files / LOC table | The break-out decomposes the *same* gate/report-layer work into named slices. No `runtime/`, no `bbnf-simd/`, no substrate touched (the V1 C.1 disposition holds). **HONOURED** |
| V-C.2 | F5 cross-grammar transposition prose + Track-2-oracle JSON-specificity acknowledgment. Does the acknowledgment conflate Track 1 ≡ Track 2? | §2.4 Track-2-oracle prose | The acknowledgment *strengthens* the honesty: serde-as-oracle is named structurally-different-at-implementation-level, JSON-specific. The fold makes the non-conflation explicit. No Track-1≡Track-2 gate. **HONOURED** |
| V-C.3 | The four-id same-run Criterion capture retained. Does the LOC break-out add a fifth measurement id? | §2.2 same-run anchor | Four ids unchanged (track1 / track2 / sonic-rs / serde-json). The break-out is a cost decomposition, not a new capture. **HONOURED** |
| V-C.4 | P2-C remained ACCEPT at V1 CH5 (six HONOURED, zero conditions). Did the F4/F5 fold introduce any CH5 risk? | whole report | The fold was paragraph-level (LOC break-out + prose). No substrate, no producer, no sidecar. CH5 silent at V1; remains silent at V2. **HONOURED** |
| V-C.5 | §2.3 wave-id bump path retained. Does the LOC break-out multiply baseline constants? | §2.3 run-id provenance | The named-guard-table framing is unchanged; baseline constants are documentation, not parsing substrate. **HONOURED** |

### §2.4 — P2-D folds

| # | V2 fold delta | Citation | Verdict |
|---:|---|---|---|
| V-D.1 | F1 — the §2.1 wiring claim corrected (the kernel IS wired at `parse-that-regex/src/lib.rs:402`). Does correcting the claim reveal a second codec producer? | `:31-35`, F1 in consolidated | The correction is honesty, not a new producer: the kernel was *always* wired (`unescape_four_unicode_escapes`). The V1 lens at D.1 already audited the consumer cardinality as one. The F1 fix removes a false "unwired" claim; it does not add a producer. **HONOURED** |
| V-D.2 | §3 same-wave consumer binding paragraph added (R-CH5-3). Does the "blocks on P2-A" binding create a substrate dependency edge that doubles substrates? | `:453-462` | The binding *prevents* a second substrate: it forbids the codec shipping without P2-A's union substrate (which would force the parser-owned-helper shape). The dependency edge is a no-orphan guard, not a substrate. **HONOURED** |
| V-D.3 | F4 per-opportunity LOC + risk class + "final cost-set authored by S-P3" deferral. Does the deferral leave a substrate undefined? | §3/§4.3/§4.4 LOC tables | The deferral is of the *cost-set*, not the substrate shape. The substrate (P2-A union tape) is fully specified; only the LOC envelope is preliminary. No substrate ambiguity. **HONOURED** |
| V-D.4 | §4.3 32-byte widening retained the dual dispatch phrasing (#2). Does the un-stripped ambiguity admit a parallel producer? | `:626-637` | §4.3 #3 binds both shapes to one external consumer + one mask output ("no sidecar primitive, no parallel substrate"). The ambiguity is shape-internal; cardinality is one under either reading. See §1.4. **HONOURED** |
| V-D.5 | §4.3 LOC table — the `interesting`-mask producer-side OR-fold slice. Does moving the `t\|e\|c\|n` collapse from consumer to producer create a second mask? | `:651` slice 4 | The fold *moves* work from consumer to producer; it does not duplicate it. One `interesting` mask, computed once, in the producer instead of the consumer. Fewer masks in flight, not more. **HONOURED** |
| V-D.6 | §4.4 CSSC CTZ — the hard "blocks on P2-A landing" added (R-CH5-5). Same analysis as V-D.2. | `:692-699` | No-orphan guard; prevents the structural-scan-path consumer drift. No substrate. **HONOURED** |
| V-D.7 | §5.3.1 SHA3 EOR3 prefix-XOR fold retained. Does the F-fold touch it? | `:636-668` (V1 cite) | Unchanged from V1 D.2 (one producer, refined intrinsic, scalar oracle retained). The V2 fold did not touch §5.3.1. **HONOURED** |
| V-D.8 | §6.2.1 new sub-section — per-kernel checkasm ownership table (R-CH5-6). Does adding a test-ownership table add a test substrate? | `:1010-1024` | The checkasm tests are parity oracles, one per primitive — `tests/` differential harness, not a runtime substrate. The ownership table assigns existing-gap tests to waves; it creates no parallel test substrate (the V1 F.6 disposition on the checkasm harness holds). **HONOURED** |
| V-D.9 | §6.2.1 — `checkasm_digit_mac` carried forward (no §3-§5 consumer this wave). Is the carry an orphan? | `:1020` | The carry is explicitly NOT a paper-close: the test is assigned to "the first SK-V9+ wave that wires `digit_mac` into a numeric-token consumer." Ownership named, not dropped. This is the correct anti-orphan discipline. **HONOURED** |
| V-D.10 | §6.3 five-invariant gate — invariant 2-5 deferral retained. Does the F4 fold change the deferral? | `:1040-1048` | Deferral to SK-V10+ unchanged; named, not hidden. CH5 silent (host-side instrumentation does not multiply substrates; this is CH6/CH4 territory). **DEFERRED** (CH5 silent, as at V1 D.7) |

### §2.5 — P2-E folds

| # | V2 fold delta | Citation | Verdict |
|---:|---|---|---|
| V-E.1 | F2 — §6.1/§6.2 PMU rederivation from the actual TSV (the V1 column was fabricated). Does the rederived PMU data introduce a parallel measurement path? | §6.2 PMU-rederived rows; `:746-748` | The rederivation corrects numbers; it adds no measurement substrate. PMU is a diagnostic non-producer (V1 C.4 disposition). The rederived projections (15,423 / 7,837 Mbps NEAR-FAIL; unicode_mixed 63.7% FAIL) are honest c/B arithmetic over one TSV. No parallel path. **HONOURED** |
| V-E.2 | §6.4 same-wave conditional admission rule (F2/F6) — unicode rows admit only with a paired scanner intervention. Does the conditional admission imply a second scanner substrate? | `:700-710`, `:945` | The conditional names a *future* paired scanner knob as the admission gate; it does not ship a second scanner in this wave. If no scanner lands, the row stays NO-GO. The conditional is a gate, not a substrate. **HONOURED** |
| V-E.3 | F4 — §7.1 per-slice table: eleven slices with caps + revert + named same-wave consumer. Does any slice construct a sidecar? | `:710-732` | Each slice is a part of one codec landing. S1-S5 are five const-generic bodies of *one* primitive class; S6 is the parity test; S7-S11 are consumer-wiring + codegen + removal. No slice builds a parallel codec. The V1 E.1/E.2 dispositions (one primitive, five specialisations, monomorphised) hold. **HONOURED** |
| V-E.4 | §7.1 slice S11 — kernel removal ordered LAST, after S7 green (R-CH5-7). Does the ordering leave a window of two coexisting producers? | `:731` | S7 migrates every production caller onto the new kernel; S11 then deletes the now-dead old kernel. No window of two *live* producers — the old kernel is dead before removal. See §1.7. **HONOURED** |
| V-E.5 | E.4 TOML no-consumer disposition (F4). Does the TOML scaffold (S3 `hex_x8_neon`) land as an orphan kernel? | §7.1 S3; `:799` | S3 is explicitly scaffold-only — "no production consumer depends on it (TOML is scaffold-only)"; its same-wave consumer is the S6 checkasm gate. A compile-validated body whose only consumer is its parity test is the admitted scaffold shape (V1 E.3 logic), not an orphan substrate. **HONOURED** |
| V-E.6 | F5 — scaffold-vs-production-consumer naming. §7.4 summary: "1 production + 2 scaffolds (CSS L4, TOML)." Does naming three consumers imply three substrates? | `:799` | Three *call sites* of one primitive; one production (JSON), two scaffolds. The V1 E.1 disposition (per-call-site monomorphisation, one kernel body) holds. Three bindings, one primitive, one substrate. **HONOURED** |
| V-E.7 | §6.1 rederived c/B no longer fabricated. Does the corrected baseline change the Track-1≡Track-2 posture? | §6 falsifiability gate | The gate remains Track 1 ≥ sonic-strict × slack (V1 E.7). The rederivation touched c/B numbers, not the comparator. No Track-1≡Track-2 conflation introduced. **HONOURED** |

### §2.6 — P2-F folds

| # | V2 fold delta | Citation | Verdict |
|---:|---|---|---|
| V-F.1 | F3 — §0 declares the synthesis-overreach walk-back. §7.4 reframed from a sequenced wave plan to an inter-report dependency graph. Does the dependency graph imply parallel substrates? | `:624-637`, `:528-555` | The dependency graph records "which P2 finding a given finding depends on" (I ← P2-A; III ← P2-A). It is a DAG of *research dependencies*, carrying "no per-slice minute cost" and no wave sequence. One substrate (the tape) across all nodes. The reframe *removes* the S-P3-scope sequencing, it adds nothing. **HONOURED** |
| V-F.2 | F3 — §7.2 DirectBuild emit-site expansion stripped. The V1 §7.2 wired the codec at a "DirectBuild field-fact emit site" (REDRESS 66-69 territory). Does the strip leave a dangling consumer? | `:637-647`, `:503` | The strip *removes* the REDRESS-66-69-reopening expansion. §7.2 now names the codec consumer as the retained-parse string-match path only — the P2-E §4.1 consumer. The codec consumer cardinality drops from two (string-match + DirectBuild emit) to one. The strip *reduces* coupling. **HONOURED** |
| V-F.3 | F3 — §7.3 admission 1 / admission 2 stripped (admission 1 was REDRESS 33's exact rejected shape). Does the strip leave a substrate without its admission? | §0 walk-back, `:628` | The strip removes the REDRESS-33-reopening admissions; the remaining §7 path is P2-A + P2-E + P2-D at cardinality one (V1 F.1 disposition). No substrate orphaned — the stripped admissions were *reopens*, not substrates. **HONOURED** |
| V-F.4 | F3 — §3 "Room to widen the lead" synthesis-grade claims walked back, deferred to S-P3. Does the walk-back leave a Track-1≡Track-2 claim? | §3 node-speed prose | §3 reports Track 1 Mbps vs sonic-strict only (V1 F.3 disposition); the walk-back removes synthesis-grade *projection*, not the comparator honesty. No Track-1≡Track-2 conflation. **HONOURED** |
| V-F.5 | F6 — owner resolution for the no-owner gap between P2-D and P2-A on REDRESS 28+33. Does assigning an owner add a substrate? | §7.4 dependency graph | The owner resolution assigns the REDRESS-28+33 routes to P2-A's union substrate as their consumer (the dependency-graph edge III ← P2-A). One substrate; the resolution is an edge, not a node. **HONOURED** |
| V-F.6 | §4 asmjson anchor — the sidecar classification retained (`historical:sk-v7-sidecar-profile`, "never an admission anchor"). Does the F3 fold weaken the sidecar disclaimer? | `:37-39`, `:300-315` | The asmjson rows are still classified as non-anchored sidecar planning signal; the disclaimer is intact. asmjson is a *measurement* sidecar (diagnostic only), not a parallel parsing substrate — the term "sidecar" here is the strictness-classification sense, correctly disclaimed. **HONOURED** |
| V-F.7 | §7 the >SOTA path retained as P2-A + P2-E + P2-D synthesis. Does stripping §7.2/§7.3 leave the path incoherent (a substrate with no path)? | §7, §0 | The path is now the three interventions at cardinality one, with the DirectBuild expansion and the REDRESS-33 admissions removed. The path is *narrower* and Lock-1-cleaner than V1. One substrate, three interventions, one >SOTA gate vs strict comparators. **HONOURED** |

### §2.7 — V2 disposition count

| Report | HONOURED | DEFERRED (CH5 silent) | VIOLATION | New-coupling rows |
|---|---:|---:|---:|---:|
| P2-A | 7 | 0 | 0 | 0 |
| P2-B | 6 | 0 | 0 | 0 |
| P2-C | 5 | 0 | 0 | 0 |
| P2-D | 9 | 1 | 0 | 0 |
| P2-E | 7 | 0 | 0 | 0 |
| P2-F | 7 | 0 | 0 | 0 |
| **Total** | **41** | **1** | **0** | **0** |

42 V2 dispositions (≥25 required by the task contract; ≥5 per report —
minimum is P2-C at 5). Zero VIOLATIONS. One DEFERRED (P2-D §6.3
invariant 2-5 host instrumentation — CH5 silent, identical to V1 D.7).
Zero rows where a V2 fold introduced new coupling.

---

## §3 — Aggregate verdict

### §3.1 — Seven-condition verdict

All seven V1 HONOURED-WITH-CONDITION rows (R-CH5-1 … R-CH5-7) are
RESOLVED in the V2 fold. The fold did exactly what the V1 §4.8 synthesis
prescribed: it did **not** redesign anything; it propagated each
condition from design-level prose into an explicit per-wave dispatch
falsifier (dual `rg` gate, parent-`pub mod` gate placement, hard
same-wave block, one-consumer/one-mask binding, per-kernel checkasm
ownership table, ordered consumer-migration-then-removal slices).

R-CH5-7 carries one paragraph-level tightening recommendation: P2-E §7.1
expresses the orphan-caller discipline through slice ordering (S7→S11)
rather than the V1-recommended named `rg 'bbnf_simd::aarch64::
unescape_uxxxx::'` post-wave falsifier. The ordering structurally
guarantees cardinality one; the recommendation is to add the named
falsifier to S-P3's wave gate as a belt-and-braces check. This does not
block — it is a §4 carry, not a CH5 fault.

### §3.2 — New-coupling verdict

No V2 fold introduced a new sidecar, a parallel substrate, a second
producer, or a Track-1≡Track-2 dishonesty. The folds were of two kinds,
both cardinality-preserving:

- **Subtractive folds** (P2-F F3): the §7.2 DirectBuild emit-site
  expansion and the §7.3 REDRESS-33-reopening admissions were
  *stripped*. P2-F's codec consumer cardinality dropped from two
  (string-match + DirectBuild field-fact emit) to one. The V2 P2-F is
  Lock-1-*cleaner* than V1 P2-F.
- **Discipline-promoting folds** (P2-A/B/D/E): each condition became an
  explicit falsifier or ordered-slice gate. None added a substrate; all
  *constrained* the implementation toward cardinality one.

The F1 P2-D wiring correction (the kernel was always wired at
`parse-that-regex/src/lib.rs:402`) is an honesty fix, not a producer
addition — the consumer cardinality the V1 lens audited at D.1 is
unchanged.

The F2 P2-E PMU rederivation corrects fabricated c/B numbers; PMU
remains a diagnostic non-producer. No measurement substrate added.

### §3.3 — Substrate cardinality across all V2 designs

Cardinality stays at **one** across all six V2-folded reports:

- P2-A — the SIMD structural index is a transient producer consumed by
  move; the class column is a co-indexed column on the one `Tape<'input>`
  struct; `consume_structural` is *deleted* (verified `:292-306`).
- P2-B — `AnyGrammar` is a compile-only `cfg`-gated proof witness, not a
  third runtime substrate; `ValueRef` borrows the tape; the four-member
  `EventGrammar` trait gained no member.
- P2-C — gate/report layer only; the F4 LOC break-out touched no
  substrate.
- P2-D — every ASM kernel is a scalar-oracle-backed Layer-1 primitive
  whose same-wave consumer is P2-A's union substrate (codec, string-mask,
  CTZ extract all hard-bound to P2-A landing); the 32-byte widening binds
  to one external consumer and one mask output; `digit_mac`'s test is
  carried-forward, not orphaned.
- P2-E — one codec primitive class, five const-generic specialisations,
  three call sites; the old `unescape_uxxxx.rs` kernel is removed
  *after* consumer migration (S7→S11); no window of two live producers.
- P2-F — the >SOTA path is the integrated P2-A + P2-E + P2-D synthesis
  at cardinality one, now narrower after the §7.2/§7.3 strips; §7.4 is a
  research-dependency DAG, not a wave plan.

No proposal conflates Track 1 ≡ Track 2 as a SOTA gate. Every gate is
Track 1 versus strict comparator (sonic-rs strict; simdjson NEON and
yyjson as additional comparators in P2-F). Track 2 / serde is named as
oracle (P2-C) or guard (P2-A) but never as the close criterion. P2-C's
F5 fold strengthened the non-conflation by explicitly acknowledging
serde-as-oracle's JSON-specificity.

### §3.4 — CH5 V2 cohort verdict

**ACCEPT.** 42 V2 dispositions: 41 HONOURED + 1 DEFERRED (CH5-silent) +
0 VIOLATION. Seven of seven V1 conditions RESOLVED. Zero new coupling
introduced by the V2 fold.

Per ORCHESTRATOR §3Z: V1 CH5 was ACCEPT (cycle 1). V2 CH5 is ACCEPT
(cycle 2). CH5 has now produced **two consecutive ACCEPT cycles** — it
is converged. CH5 places no fold demand on a hypothetical V3; any V3
CHALLENGE re-runs CH5 only as a regression check, not a fold target.

The V2 fold's discipline matches the SC-6 §6 R1 sidecar-drift
mitigation precisely: the union concept was singular substrate at V1,
and the V2 fold makes the implementation-phase cardinality-one guarantee
enforceable per-wave rather than declared as project intent. The dav1d
four-tuple commit shape (primitive + scalar reference + checkasm parity
+ consumer wiring, in one commit) is now expressed as a falsifier in
every report that admits a primitive.

---

## §4 — New coupling risks

The V2 verify found **no new coupling risk** introduced by the fold.
The §1 conditions were RESOLVED, not re-opened, and §2 found zero
new-coupling rows. Two items carry forward to S-P3 — neither is a CH5
fault; both are tightenings that make an already-cardinality-one design
falsifiable at the wave gate.

### §4.1 — Carry C-CH5-V2-1: R-CH5-7 named `rg` falsifier (paragraph-level)

P2-E §7.1 slice S11 guarantees orphan-free kernel removal via slice
ordering (S7 consumer migration → S11 deletion). The V1 V2-fold
recommendation also named an explicit post-wave falsifier: `rg
'bbnf_simd::aarch64::unescape_uxxxx::' skinny/crates/` returns zero
after the wave commit, except inside the new `escape_codec/` owner. The
folded P2-E expresses the discipline through ordering but does not state
that named `rg` gate. **Recommendation**: S-P3's P2-E wave dispatch
contract adds the named `rg` falsifier to the S11 gate. This is a
belt-and-braces check; the slice ordering already guarantees
cardinality one. Not blocking.

### §4.2 — Carry C-CH5-V2-2: R-CH5-4 dispatch-shape selection (S-P3 scope)

P2-D §4.3 #2 retains both 32-byte dispatch shapes — (a) replace the
16-byte primitive; (b) a `_32` variant called by the 16-byte producer.
§4.3 #3 binds *both* to one external consumer and one mask output, so
cardinality is one regardless. The shape selection is correctly deferred
to S-P3 with the cost-set. **Recommendation**: S-P3's P2-D wave dispatch
contract declares which shape lands and, if shape (b), states verbatim
that the dispatch is internal to one producer body returning one
`StringSpecialBlock` per call — two mask outputs from a single call site
are forbidden. This is the V1 R-CH5-4 V2-fold language; it routes
correctly to S-P3. Not blocking — cardinality does not depend on the
shape.

### §4.3 — Standing observations (not faults)

- The F1 P2-D wiring correction means the §3 codec broadening is a
  *broadening of an existing x4 path*, not a fresh wiring. The
  same-wave-consumer binding (R-CH5-3) correctly treats the broadened
  fall-through traffic as needing P2-A's union substrate to avoid the
  parser-owned-helper shape. S-P3 must hold the §3 codec broadening and
  P2-A in one wave; this is already a hard block in the folded P2-D
  `:453-462` — recorded here only so S-P3's sequencing carries it.
- P2-D §6.3 invariant 2-5 (forced feature masks, ABI shims, recoverable
  fault handlers, cycle-counter source) remains DEFERRED to SK-V10+.
  CH5 is silent (host-side instrumentation does not multiply
  substrates); this is a CH4/CH6 concern. Recorded for cross-lens
  completeness only.

---

## §5 — Sources

- `restart/skinny/tranches/sk-v9/research/p2/hardening/V1/CH5.md` — the V1 CH5 disposition (41 rows, 7 conditions).
- `restart/skinny/tranches/sk-v9/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md` — the V1 consolidated verdict + F1-F6 fold targets + V2 fold path.
- `restart/locks/LOCKS.md:34` — Lock 1 verbatim (substrate union; cardinality; 2026-05-04 reframe).
- `restart/locks/LOCKS.md:60` — Lock 14 (grammar generalisation).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md` §2.2 — the cardinality discriminant; §6 R1 sidecar-drift risk.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md` — V2-folded P2-A (§0, §2.5 `:347-400`).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md` — V2-folded P2-B (§0, §1.2 `:68`, `:298-301`).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md` — V2-folded P2-C.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md` — V2-folded P2-D (`:453-462`, §4.3 `:626-651`, §4.4 `:692-699`, §6.2.1 `:1010-1024`).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-E-unicode-escape-codec.md` — V2-folded P2-E (§7.1 `:710-732`).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-F-sota-teardown-m5max.md` — V2-folded P2-F (§0 `:624-647`, §7.4 `:528-555`).
- `skinny/crates/runtime/src/grammars/json/generated.rs:292-306` — the live `consume_structural` function (R-CH5-1 cite verified byte-accurate).
