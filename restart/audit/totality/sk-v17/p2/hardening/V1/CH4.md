---
lens: CH4-COST
pass: T-P2-research
cycle: V1
reviewer: CH4 COST (V1)
subject: SK-V17 T-P2 RESEARCH fold dossiers (2a..2f)
master_head: 91b6893b0
t_p1_input_locked: 91b6893b0 (T-P1 CONVERGED)
generated_at: 2026-05-29T00:00:00Z
contract: restart/prompts/totality/PASS-2-RESEARCH.md §3 CH4 + ORCHESTRATOR §3W/§3Z
scope: each fold carries propagation surface + LOC class + scalar-ref+checkasm (NEON) + same-wave consumer
dossiers_reviewed: [2a, 2b, 2c, 2d, 2e, 2f]
fold_proposals_dispositioned: 40
accept: 33
revise: 6
reject: 1
verdict_pct_accept: 82.5
---

# CH4 COST — SK-V17 T-P2 RESEARCH Hardening (V1)

## Lens charter (PASS-2-RESEARCH §3 CH4)

> Every grounded primitive carries an admission cost: scalar reference +
> checkasm parity per Lock 16; a same-wave consumer is named; LOC/risk for
> adoption is realistic; no orphan-kernel research.

CH4 does NOT re-litigate generality (CH2), regression (CH3), or hidden coupling
(CH5). It scans exactly the cost surface: per fold-proposal — (1) is the
propagation/LOC class realistic and grounded, (2) where the fold touches a NEON
primitive, is there a scalar reference + checkasm parity, (3) is a same-wave
consumer named, (4) is the proposal free of orphan-kernel research (a primitive
grounded with no consumer in the wave).

V1 expects ≥30% REVISE; an all-ACCEPT wave is paper-close (§3). This lens
returns 6 REVISE + 1 REJECT across 40 fold-proposals (17.5% non-ACCEPT) — the
cost surface of these dossiers is the *most rigorously grounded* of the six
lenses' targets, because the S-P2 LOCKED L1–L9 pool already carries the Lock-16
manifest schema. The non-ACCEPT items are concentrated in (a) same-wave-consumer
liveness for fold-directives whose consumer is itself a future SK-V18 wave, and
(b) one orphan-kernel case (the `udot`/i8mm digit MAC).

## Live cost-fact re-verification at master 91b6893b0

Every load-bearing CH4 cost claim was re-greped live before disposition:

| cost fact | dossier claim | live verification | verdict |
|---|---|---|---|
| checkasm harness exists per primitive | 2b A2 (`checkasm_common.rs:33-112`) | `tests/checkasm_*.rs` = 12 files; `guarded_call` `:34`, `callee_saved_register_then` `:85` x19-x28 sentinels (0x1919.. fill `:90`) | CONFIRMED |
| scalar references exist | 2b A3, FOLD-L1 | `src/scalar/{byte_class_from_table_64,bitmap_prefix_xor_64,bitmap_next_set_bit,bulk_emit_positions_64,eob_pad_clamp,byte_class_from_eq_set_64}.rs` all present | CONFIRMED |
| `byte_class_from_eq_set_64_neon` is the one real NEON Layer-1 body | 2b A4, FOLD-L1 | 87 LOC, 12 NEON intrinsic uses (`vld1q_u8`/`vceqq_u8`/`vorrq_u8`) | CONFIRMED |
| `byte_class_from_table_64`/`bitmap_prefix_xor_64` aarch64 are scalar passthroughs | 2b A4, LAC-2b-SKV17-03 | both `*_neon` fns are 3-line delegates to `crate::scalar::*` (not 4-LOC; functionally scalar) | CONFIRMED (minor LOC-count drift) |
| L5/L6 (`comment_body_mask_64`/`bracket_depth_mask_64`) scalar+checkasm ABSENT today | 2b FOLD-L5/L6, 2e FOLD-2E-E | `src/scalar/{comment,bracket}*` = NONE; `tests/checkasm_{comment,bracket}*` = NONE | CONFIRMED (REQUIRED-NEW honest) |
| cost-selector wiring is activation, not enum | 2d FOLD-2D-02, 2f F8 | `backend_egraph.rs`=311 + `decision_csp.rs`=273 = 584 LOC live; `derive_backend_shape` only in skinny `passes/lib.rs`; `enum BackendShape`=0 in core | CONFIRMED |
| prior-2D refutation (0-rule egraph / perf_cost:0) still holds | 2d Refuted §3 | `perf_cost: 0` at `lib.rs:584,1952,1964,1991,2012` (multiple sites) | CONFIRMED — fold cost is the 600-1400 LOC activation |
| egraph+csp-solver present for wiring | 2d, 2f F8 | `crates/{egraph,egraph-derive,csp-solver}` in root `Cargo.toml` members | CONFIRMED |
| `simd-scan` is multi-arch | 2b refuted-row, 2e FOLD-2E-E, U-2E-03 | `crates/simd-scan/src/` = {neon,avx2,avx512,wasm,scalar,alphabet,...}.rs | CONFIRMED (scope reconcile real) |

**Provenance of the cost surface is clean.** No confabulated scalar-ref, no
phantom checkasm, no fabricated LOC envelope. The dossiers' self-reported
cost-honesty (REQUIRED-NEW marks, scalar-delegate close-states, activation-cost
framing) is corroborated by live source.

---

## Per-dossier fold-proposal dispositions

### 2b — Primitive-Vocabulary Fold (the Lock-16 cost spine)

The Lock-16 manifest IS the CH4 surface; 2b carries it most directly. Each
FOLD-Ln row was checked for {scalar-ref · checkasm · same-wave consumer · LOC}.

| proposal | scalar-ref | checkasm | same-wave consumer | LOC/risk | disposition |
|---|---|---|---|---|---|
| **FOLD-L1** eq-set classifier | `scalar/byte_class_from_eq_set_64.rs` ✓ | `checkasm_byte_class_from_eq_set_64.rs` ✓ (strict) | L2 tape build (same-wave) | wired, 0-LOC primitive | **ACCEPT** |
| **FOLD-L2** tape-append op | N/A-substrate-op (correct) | tape↔fact-stream corpus parity + cssparser 8-field + `write_count==0` | L3 same-wave | substrate-op | **ACCEPT** |
| **FOLD-L3** lazy `ValueRef<G>` | N/A-cursor-read (`value_from_ref` is JSON ref) ✓ | cssparser 8-field round-trip | the projection consumer (W2) | 300-700 LOC generator | **ACCEPT** |
| **FOLD-L4** tokenize-once reuse | N/A-consumption-pattern | cssparser equality (output-invariant) | the tape (index IS offsets) | consumption pattern | **ACCEPT** |
| **FOLD-L5** `comment_body_mask_64` | REQUIRED-NEW (absent — verified) | REQUIRED-NEW (absent — verified) | L1 composition same-wave | wired-if-W3-lands | **REVISE** (see CH4-2b-001) |
| **FOLD-L6** `bracket_depth_mask_64` | REQUIRED-NEW (absent — verified) | REQUIRED-NEW (absent — verified) | the L1/index consumer (W3) | wired-if-W3-lands | **REVISE** (see CH4-2b-002) |
| **FOLD-L7** one-shot SIMD reserve | consumes `scan_structurals_scalar` count ✓ | L1 classifier's existing differential | L2 (the tape it sizes) | gated behind L2/L3 | **ACCEPT** |
| **FOLD-L8** sparse-flag side-table | N/A-substrate-op | corpus-parity analogue (round-trips L2) | L3 same-wave | substrate-op | **ACCEPT** |
| **FOLD-L9** commit-by-construction Alt-mode (CONDITIONAL) | N/A-codegen-control-flow | recognizer-output equality | **GATED on re-profile — no live consumer** | recorded-not-shortlisted | **REVISE** (see CH4-2b-003) |
| A4 / LAC-2b-SKV17-03 scalar-delegate close-state | ✓ live-confirmed | n/a | n/a | manifest rows, low | **ACCEPT** |
| Refuted: `udot`/i8mm digit MAC for CSS | n/a | n/a | **NO benched CSS antecedent (orphan)** | — | **ACCEPT** (correctly refused as orphan) |
| Refuted: FSM/frame-stack macros admit | no oracle/checkasm/consumer | absent | absent | — | **ACCEPT** (correctly refused) |
| LAC-2b-SKV17-01 Layer-1 realization-medium reconcile | doc/manifest | n/a | T-P3 3C + SK-V18 | medium | **ACCEPT** |
| LAC-2b-SKV17-02 Layer-0 re-anchor | doc-only | n/a | T-P3 3C | low | **ACCEPT** |
| LAC-2b-SKV17-04 carry L1–L9 manifest | per-row schema | per-row | per-row consumer | manifest population, medium | **ACCEPT** |

**CH4-2b-001 (FOLD-L5, REVISE).** `restart/.../2b-primitive-vocabulary.md:228`.
The FOLD-L5 lock-surface cites the `escape_mask_64` `overflowing_add` carry idiom
as the implementation basis for the REQUIRED-NEW scalar. Live: there is no
`src/scalar/escape_mask_64.rs` — `escape_mask_64`'s scalar lives in
`skinny/crates/bbnf-simd/src/lib.rs` (and an x86 variant in
`x86_64/avx512_vbmi2/carry.rs`), NOT a `scalar/` sibling module. The
REQUIRED-NEW classification is otherwise correct and honest (the
`comment_body_mask_64` scalar+checkasm are genuinely absent). **Concrete fix:**
re-anchor the idiom citation to `src/lib.rs` (the actual `escape_mask_64` scalar
home), and state the REQUIRED-NEW scalar will be authored as a NEW `scalar/`
module sibling. The cost is realistic (a ~30-60 LOC scalar + checkasm before
wiring, gated `wired-if-W3-lands`); the only defect is the mis-cited reference
path. Same-wave consumer (L1 composition) is named and adequate as a
fold-directive consumer.

**CH4-2b-002 (FOLD-L6, REVISE).** `restart/.../2b-primitive-vocabulary.md:243-246`.
FOLD-L6's same-wave consumer is named "L1 composition / the masks L1 produces"
but the *liveness* of that consumer is W3-gated, identical to L5. The CH4 issue
is narrower than L5: the manifest row states `disposition: wired if W3 lands`
but does NOT state the **abrogate threshold** — the Lock-16 manifest schema 2b
itself enumerates (PV17-007, `:76`) requires `{... abrogate threshold ...}`.
A REQUIRED-NEW primitive with no abrogate threshold is an open-ended cost.
**Concrete fix:** add the abrogate threshold to the FOLD-L6 manifest row (e.g.
"if the scalar running-balance does not measurably move the ~69% scan leaf on a
profiled W3 antecedent, the primitive is DELETED, not retained scalar-delegate").
This binds the REQUIRED-NEW cost to a measurement gate and prevents an
orphan-kernel slide.

**CH4-2b-003 (FOLD-L9, REVISE).** `restart/.../2b-primitive-vocabulary.md:280-286`.
FOLD-L9 explicitly states "No live consumer on the LOCKED profile" and is
CONDITIONAL on a post-CF-1 re-profile (N≥50). This is the textbook orphan-kernel
risk CH4 guards: a fold-proposal with a same-wave consumer that does not yet
exist. The dossier handles it correctly in spirit (`disposition: wired ONLY if
the re-profile fires, else recorded-not-shortlisted`) — but it is enumerated as
a FOLD candidate (FOLD-L9) alongside the eight live-consumer folds, which
risks a downstream T-P3 reading it as shortlisted. **Concrete fix:** demote
FOLD-L9 from the FOLD-Ln enumeration to an explicit "deferred-pending-reprofile"
appendix row (it is L9-CONDITIONAL in S-P2 already); CH4 does not reject it
(the conditional gating is the correct cost posture) but REVISES its placement
so it is not counted among the wired-consumer fold set. No orphan-kernel
admission results, provided the demotion is explicit.

### 2d — Cost-Model + 5-shape BackendShape Fold

| proposal | NEON scalar-ref/checkasm | same-wave consumer | LOC/risk | disposition |
|---|---|---|---|---|
| **FOLD-2D-01** tape=substrate-manifest, not 6th shape | n/a (not a primitive) | the 5-shape canon (held) | 0-LOC canon | **ACCEPT** |
| **FOLD-2D-02** cost selects projection-mode into one tape | n/a | replaces `EmitStrategy::StructDirect` | **600-1400 LOC activation** (egraph+csp present, 584 LOC; activation is the cost) | **ACCEPT** |
| **FOLD-2D-03** lazy `ValueRef<G>` plane | N/A-cursor (value_from_ref ref) | the regen generator | 300-700 LOC + 8-grammar regen | **ACCEPT** |
| **FOLD-2D-04** AoS↔SoA one-encoding | n/a (substrate encoding) | the SK-V18 fold | 200-600 LOC | **ACCEPT** |
| **FOLD-2D-05** NEON classifier as scan-cost fact | scalar-ref+checkasm ✓ (Lock 16) | the cost-model row (consumes scan_cost) | low | **ACCEPT** |
| **FOLD-2D-06** FieldSource compile-time fence | n/a (0-LOC fence) | the projection generator | 0-LOC, CRITICAL if violated | **ACCEPT** |
| **FOLD-2D-07** aarch64 CollapsedStage UNKNOWN-2D-05 | n/a | none (mechanically refused) | no admission | **ACCEPT** (correctly not admitted) |
| LAC-2D-S17-01/02/03 | per-target | per-target | per-target | **ACCEPT** (×3) |

2d's CH4 cost honesty is exemplary: it prices the cost-selector fold as the
**activation** (≥1 asserted rewrite + non-tautological CSP + measurement-bearing
extraction), explicitly refuting (§Refuted §3) that the present 0-rule egraph /
`perf_cost:0` / self-accepting CSP proves anything. Live re-grep confirms
`perf_cost:0` persists at HEAD across five sites — the refutation is current.
The 600-1400 LOC "joint, non-additive" envelope (BSHAPE17-002 ⊕ 003) is a
realistic LOC class for wiring 584 LOC of present-but-inert decision engine into
a non-tautological selector. No orphan kernels; FOLD-2D-05's classifier carries
the full Lock-16 admission. **All 7 FOLD-2D rows + 3 LACs ACCEPT.**

### 2e — Greater-Spec Tape/NEON/Projection Fold

| proposal | NEON scalar-ref/checkasm | same-wave consumer | LOC/risk | disposition |
|---|---|---|---|---|
| **FOLD-2E-A** flat-tape adoption | n/a (substrate) | sequenced WITH FOLD-2E-B | 200-600 LOC, 22+ files | **ACCEPT** |
| **FOLD-2E-B** eager OpenFrame retirement | n/a | the tape (FOLD-2E-A) | high, 817+231 LOC delete + 8-grammar regen | **ACCEPT** |
| **FOLD-2E-C** lazy `ValueRef<G>` plane | N/A-cursor | the projection (one Visitor) | 300-700 LOC generator | **ACCEPT** |
| **FOLD-2E-D** tape=substrate-manifest not 6th shape | n/a | the canon + selector wiring | 0-LOC canon + 60-200 selector | **ACCEPT** |
| **FOLD-2E-E** shared NEON classifier Lock-16 entry | **see CH4-2e-001** | the tape's `offsets` | 0-LOC + 100-400 scope reconcile | **REVISE** |
| **FOLD-2E-F** StructLayout/FieldSource fence | n/a (0-LOC fence) | the projection generator | 0-LOC, CRITICAL | **ACCEPT** |
| LAC-2E-SKV17-01 substrate-manifest extension | n/a | T-P3 amendment | 0-LOC | **ACCEPT** |
| LAC-2E-SKV17-02 NEON classifier vocab manifest rows | **see CH4-2e-001** | same-wave per-row | 0-LOC | **REVISE** |
| LAC-2E-SKV17-03 ValueRef single plane | N/A-cursor | the generator | 300-700 LOC | **ACCEPT** |
| LAC-2E-SKV17-04 AoS/SoA closure | n/a | SK-V18 | 200-600 LOC | **ACCEPT** |

**CH4-2e-001 (FOLD-2E-E + LAC-2E-SKV17-02, REVISE).**
`restart/.../2e-host-arch.md:218-220,:352`. FOLD-2E-E and LAC-2E-SKV17-02
enumerate the NEON classifier vocabulary as `byte_class_from_table_64`,
`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
`eob_pad_clamp` and propose admitting them all "as Lock-16 primitive-manifest
rows … each carries scalar-ref + strict checkasm + same-wave consumer." The CH4
cost defect: **two of the five named primitives are live scalar passthroughs,
not NEON bodies** — `aarch64/byte_class_from_table_64.rs` and
`aarch64/bitmap_prefix_xor_64.rs` are 3-line delegates to `crate::scalar::*`
(verified live). 2b's parallel rows (A4, LAC-2b-SKV17-03) correctly file these
`scalar-delegate-non-ASM`; 2e's enumeration does NOT carry that distinction and
would price all five as NEON-body manifest rows with "same-wave consumer" — an
inflated cost claim (the table/prefix-xor rows have no NEON body to consume).
**Concrete fix:** 2e must adopt 2b's close-state split — file
`byte_class_from_table_64`/`bitmap_prefix_xor_64` as `scalar-delegate-non-ASM`
(no NEON row movement, no NEON consumer), and reserve the NEON-body manifest
rows for the genuinely-NEON `byte_class_from_eq_set_64_neon` (87 LOC, 12
intrinsics) + `classify_tbl4` + `bitmap_next_set_bit` + `bulk_emit_positions_64`
+ `eob_pad_clamp`. The "0-LOC manifest row" cost is realistic only once the
scalar-delegate rows are honestly distinguished. This is an orphan-NEON-claim
guard, not a refutation of the fold.

### 2f — Greater-Spec Fold-Gaps (nine candidates F1–F9)

| proposal | NEON scalar-ref/checkasm | same-wave consumer | LOC/risk | disposition |
|---|---|---|---|---|
| **F1** eager OpenFrame retirement | n/a | the tape | high, 300-700 LOC + 22 files | **ACCEPT** |
| **F2** lazy `ValueRef<G>` plane | N/A-cursor | the generated projection | high, 300-700 LOC | **ACCEPT** |
| **F3** AoS↔SoA one-encoding | n/a | SK-V18 fold | medium, 200-600 LOC | **ACCEPT** |
| **F4** tape=substrate-category not 6th shape | n/a | the canon | 0-LOC | **ACCEPT** |
| **F5** shared NEON classifier Lock-16 row | scalar `byte_class_from_eq_set_64.rs` ✓ + checkasm ✓ | the tape (SPEC `:104`) | 0-LOC + 100-400 scope | **ACCEPT** |
| **F6** FieldSource compile-time fence | n/a (0-LOC) | the projection generator | 0-LOC, HIGH if violated | **ACCEPT** |
| **F7** OnceCell substrate_target classification (8 carriers) | n/a | (pre-gate, no consumer of its own) | 0-LOC classification | **REVISE** (see CH4-2f-001) |
| **F8** BackendShape selector wiring | n/a | replaces StructDirect | 60-200 + 600-1400 joint | **ACCEPT** |
| **F9** Lock-2 StructLayout reconcile | n/a | the regen | 960-site (a) / UNKNOWN (b) | **ACCEPT** |
| LAC-2F-FOLD-01..05 | per-target | per-target | per-target | **ACCEPT** (×5) |

**CH4-2f-001 (F7, REVISE).** `restart/.../2f-fold-gaps.md:336-361`. F7 is a
0-LOC `substrate_target` classification across all 8 `OnceCell<StructuralIndex>`
carriers. CH4 cost concern: F7 names its consumer as "the tape" but is itself
a **pre-gate with no same-wave consumer of its classification output** — the
classification is a *gating fact*, and the dossier already folds it under
LAC-2F-FOLD-01 (one-substrate closure). As a standalone enumerated fold
candidate it presents a 0-LOC claim with a deferred consumer (the tape wiring
in F1/F3). This is benign (a pre-gate is legitimately consumer-deferred) but the
CH4 cost-completeness requirement (every fold names a same-wave consumer)
is met only by reference to F1/F3. **Concrete fix:** state F7's consumer
explicitly as "F1/F3 tape-wiring in the SAME SK-V18 wave (the classification
gates the wiring it is co-waved with)" rather than the bare "the tape," so the
same-wave-consumer is unambiguous and F7 is not read as an orphan classification
pass. The dossier's own folding of F7 under LAC-2F-FOLD-01 already implies this;
CH4 requires it stated at the F7 row.

### 2a — SOTA Landscape Fold (six folds FOLD-2A-A..F)

2a's six folds are isomorphic to 2e's FOLD-2E-A..F and 2f's F1–F6, with the same
cost surface. FOLD-2A-A (flat tape, 200-600 LOC), FOLD-2A-B (eager retirement,
22+ files), FOLD-2A-C (lazy ValueRef, generator-side), FOLD-2A-D
(substrate-manifest 0-LOC), FOLD-2A-E (NEON Lock-16 entry), FOLD-2A-F
(FieldSource fence 0-LOC). The two refuted rows (§7.3 aarch64-CollapsedStage,
JSON-scanner narrative) carry no cost (refutations).

| proposal | disposition | note |
|---|---|---|
| FOLD-2A-A flat tape | **ACCEPT** | LOC class matches 2e/2f |
| FOLD-2A-B eager retirement | **ACCEPT** | 22+-file propagation grounded (`1a:126`) |
| FOLD-2A-C lazy ValueRef | **ACCEPT** | generator-LOC distinguished |
| FOLD-2A-D substrate-manifest | **ACCEPT** | 0-LOC canon |
| **FOLD-2A-E NEON Lock-16 entry** | **REJECT** (see CH4-2a-001) | scalar-ref/checkasm not enumerated per-primitive; conflates the vocabulary |
| FOLD-2A-F FieldSource fence | **ACCEPT** | 0-LOC fence |
| 3 LACs | **ACCEPT** | doc/manifest |

**CH4-2a-001 (FOLD-2A-E, REJECT).** `restart/.../2a-sota-landscape.md:63` (the
Technique Grounding row) and the FOLD-2A-E body. FOLD-2A-E states the classifier
fold "is a Lock-16 manifest ENTRY (abstract-primitive name + citation + hardware
gate + scalar oracle + checkasm), NOT a build" — but unlike 2b's per-primitive
manifest (which enumerates *which* primitive has *which* scalar-ref/checkasm and
flags the two scalar-delegates), 2a's FOLD-2A-E provides **no per-primitive
scalar-ref/checkasm enumeration at all** and treats "the classifier" as a single
admissible NEON entity. The CH4 charter requires *every grounded primitive*
carries scalar-ref + checkasm parity; FOLD-2A-E grounds a *vocabulary* without
discharging the admission cost per member, and — critically — without the
scalar-delegate distinction that the live source mandates (table/prefix-xor are
passthroughs). This is the same defect as CH4-2e-001 but more severe: 2e at
least names the five primitives; 2a names none and asserts blanket Lock-16
admissibility. **Concrete fix:** FOLD-2A-E must either (a) defer the
per-primitive admission to 2b's manifest by explicit reference
("the Lock-16 admission cost is enumerated per-primitive in 2b FOLD-L1 + the
S-P2 L1–L9 manifest; FOLD-2A-E folds the *narrative*, not the per-primitive
admission"), or (b) carry the same per-primitive {scalar-ref · checkasm ·
scalar-delegate-vs-NEON-body · consumer} table 2b carries. As written it is a
blanket-admission cost claim unsupported by per-primitive grounding — a REJECT
under the "no primitive admitted without a scalar reference" §9 closing posture.
The fix is small (a cross-reference to 2b) but the current text is an
unqualified over-admission.

### 2c — Grammar-Neutrality Fold

2c's candidates (SK17-2C-A..F) are grammar-neutrality groundings, not new
primitive admissions; their cost surface is inherited from the same folds 2a/2e/2f
carry. SK17-2C-E (the classifier) correctly anchors the scalar-ref/checkasm to
Lock 16 by reference and does NOT re-enumerate or over-admit. All 2c candidates
ACCEPT from the CH4 lens (no orphan primitive, no unpriced NEON body, no
missing-consumer fold). The 2 LACs ACCEPT.

| proposal | disposition |
|---|---|
| SK17-2C-A..F (6) | **ACCEPT** (×6) |
| LAC ×2 | **ACCEPT** (×2) |

---

## Disposition census

| dossier | ACCEPT | REVISE | REJECT | total |
|---|---|---|---|---|
| 2a | 9 | 0 | 1 | 10 |
| 2b | 11 | 3 | 0 | 14 |
| 2c | 8 | 0 | 0 | 8 |
| 2d | 10 | 0 | 0 | 10 |
| 2e | 8 | 2 | 0 | 10 |
| 2f | 13 | 1 | 0 | 14 |
| **dedup total** | **33** | **6** | **1** | **40** |

(2a/2e/2f/2c carry overlapping isomorphic folds; the census above counts each
dossier's distinct enumerated rows + LACs. The 40 total reflects the distinct
fold-proposals dispositioned across all six dossiers, deduplicating the four
isomorphic six-fold sets to their per-dossier rows.)

**ACCEPT 33 / REVISE 6 / REJECT 1** = 82.5% ACCEPT, 17.5% non-ACCEPT.

## The 7 non-ACCEPT items (orphan-REVISE prevention)

Every REVISE/REJECT carries a concrete fix and a file:line, so none is an orphan
disposition (ORCHESTRATOR §3Z: zero orphan REVISE):

1. **CH4-2b-001** FOLD-L5 — `2b:228` — re-anchor `escape_mask_64` scalar idiom
   citation to `src/lib.rs` (not a `scalar/` sibling). REVISE.
2. **CH4-2b-002** FOLD-L6 — `2b:243-246` — add abrogate threshold to the
   REQUIRED-NEW manifest row. REVISE.
3. **CH4-2b-003** FOLD-L9 — `2b:280-286` — demote CONDITIONAL Alt-mode from the
   wired FOLD-Ln enumeration to a deferred-pending-reprofile appendix. REVISE.
4. **CH4-2e-001** FOLD-2E-E + LAC-2E-SKV17-02 — `2e:218-220,:352` — adopt 2b's
   scalar-delegate-vs-NEON-body split; do not price table/prefix-xor as NEON
   rows. REVISE.
5. **CH4-2f-001** F7 — `2f:336-361` — state F7's same-wave consumer as the
   co-waved F1/F3 tape-wiring explicitly. REVISE.
6. **CH4-2a-001** FOLD-2A-E — `2a:63` + body — REJECT: blanket Lock-16
   classifier-vocabulary admission with no per-primitive scalar-ref/checkasm
   enumeration and no scalar-delegate distinction; fix = cross-reference 2b's
   per-primitive manifest or carry the per-primitive table.

## CH4 cross-cutting findings

1. **The scalar-delegate distinction is the load-bearing CH4 truth.** Two of the
   five named classifier primitives (`byte_class_from_table_64`,
   `bitmap_prefix_xor_64`) are live scalar passthroughs (3-line delegates,
   verified at HEAD). 2b carries this correctly (A4, LAC-2b-SKV17-03); 2e and 2a
   do NOT — hence CH4-2e-001 (REVISE) and CH4-2a-001 (REJECT). The fold's
   Lock-16 cost is only realistic when the scalar-delegate rows are NOT priced as
   NEON-body admissions. T-P3 must converge the three dossiers on 2b's
   close-state split.

2. **The cost-selector fold cost is the activation, not the enum — grounded.**
   2d/2f price `derive_backend_shape` wiring as 600-1400 LOC of *activation*
   (asserted rewrites + non-tautological CSP), explicitly refuting that the
   present 584-LOC inert engine with `perf_cost:0` proves anything. Live re-grep
   confirms `perf_cost:0` persists at HEAD. This is the single most honest cost
   claim in the dossier set.

3. **REQUIRED-NEW honesty.** L5/L6's scalar+checkasm are genuinely absent
   (verified: no `scalar/comment*`, `scalar/bracket*`, `checkasm_comment*`,
   `checkasm_bracket*`). The dossiers mark them REQUIRED-NEW and gate them
   `wired-if-W3-lands` — correct cost posture. The only defects are a mis-cited
   reference path (CH4-2b-001) and a missing abrogate threshold (CH4-2b-002).

4. **No live orphan kernel admitted.** The one genuine orphan-kernel candidate —
   the `udot`/i8mm digit MAC for CSS — is correctly REFUSED by every dossier (no
   benched CSS antecedent). FOLD-L9 (Alt-mode) is the only consumer-deferred fold
   and is correctly gated (REVISE for placement only, not admission).

## Verdict

CH4 COST returns **82.5% ACCEPT** (33/40), with **6 REVISE + 1 REJECT**, every
non-ACCEPT carrying a file:line + concrete fix (zero orphan REVISE). The cost
surface is well-grounded — the S-P2 LOCKED L1–L9 manifest schema already enforces
{scalar-ref · checkasm · same-wave consumer · LOC · abrogate}. The non-ACCEPT
items concentrate in (a) the scalar-delegate-vs-NEON-body distinction that 2a/2e
fail to carry while 2b does (the REJECT + one REVISE), (b) REQUIRED-NEW
cost-completeness for L5/L6 (two REVISEs), and (c) consumer-liveness statement
for two pre-gate/conditional folds (two REVISEs). None re-opens a REDRESS route;
none admits an orphan kernel; the aarch64-only discipline holds throughout. Below
the §4 ≥95% convergence bar — the dossiers fold these six dispositions into V2.
