# CH5 HIDDEN COUPLING — SK-V9 S-P2 Research V3 re-verify

Pass: S-P2 Research CHALLENGE. Cycle: V3 (regression re-verification;
CH5 converged at V1 + V2 — two consecutive ACCEPT per ORCHESTRATOR §3Z).
Lens: CH5 — HIDDEN COUPLING (Lock 1 substrate-union cardinality audit).
Date: 2026-05-18.
Scope: the V3 fold (commit `212971a3`) landed 8 surgical edits across
P2-D (5) and P2-F (3) — all CH3/CH6 residual closures. CH5 converged at
V2; this V3 verify confirms the 8 surgical edits introduce no new
sidecar, no parallel substrate, no second producer, no Track-1≡Track-2
conflation, and no Lock-1 cardinality shift.

V2 CH5 verdict: ACCEPT — 41 HONOURED + 1 DEFERRED (CH5-silent) + 0
VIOLATION; 7/7 V1 conditions RESOLVED. Per ORCHESTRATOR §3Z, CH5 placed
**zero** fold demand on V3 — any V3 CHALLENGE re-runs CH5 only as a
regression check, not a fold target. The V3 fold (`212971a3`) targets
CH3 (93.0%) and CH6 (90.6%) residuals exclusively; CH5 was not a fold
recipient. The V3 verify's only job is to confirm the CH3/CH6 surgery
did not collaterally regress CH5's cardinality-one guarantee.

---

## §1 — V3-edit Lock-1 audit

The cardinality discriminant (SC-6 §2.2): after the edit lands, is *one*
object retained as the queryable substrate, or *two*? A pure
citation/gate/wording edit cannot move cardinality — it adds no
producer, no retained object, no codepath. Each of the 8 V3 edits is
audited below against that discriminant. The full edit set is the
`212971a3` diff: P2-D `+45/−9` lines, P2-F `+33/−8` lines, no file
created, no design section restructured.

### §1.1 — P2-D edit 1: §5.3.1 EOR3 six-row no-regression gate

**Edit** (`skv9-p2-D:857-864`): the EOR3 slice's S-P3 admission gains a
"no-regression maintain gate on the six W10b WIN-block rows (`canada`,
`citm_catalog`, `instruments`, `marine_ik`, `mesh`, `numbers`) as a hard
blocking precondition." The text explicitly states it *mirrors* the
§4.4 CSSC CTZ slice's falsification posture (`:695-697`).

**Lock-1 audit.** This is a falsifier-tightening, not a substrate edit.
The discriminant question — "does the gate imply a parallel substrate?"
— is answered by what the gate *gates*: it gates the *admission* of one
EOR3 body into one producer (`bitmap_prefix_xor_64`'s SHA3-gated path,
`:840-844`). A no-regression gate is a *pass/fail predicate on a single
bench run* of the six WIN rows; it constructs nothing. It is structurally
identical to the §4.4 CSSC CTZ gate which V2 CH5 already audited at V-D.6
as a "no-orphan guard, not a substrate" — the V3 edit imports that exact
posture verbatim. The §5.3.1 slice still "blocks on P2-A — its only
consumer is the §5 structural-bitmap producer" (`:864-866`, unchanged
from V2). One producer (`bitmap_prefix_xor_64`), one SHA3-gated body
addition, one consumer (P2-A's §5 structural-bitmap producer), one
scalar oracle retained. The gate adds a *precondition*; it adds no
object. **Cardinality unchanged — one.**

### §1.2 — P2-D edit 2: §5.3.1 EOR3 latency citation + caveat

**Edit** (`skv9-p2-D:818-822`): the EOR3-vs-PMULL latency claim gains a
parenthetical citing "ARM DDI 0487 FEAT_SHA3 / FEAT_PMULL instruction
descriptions" and a caveat that "M5 Max P-core specifics are unpublished
by Apple — treat the absolute cycle counts as a host-capability-gated
estimate, the monotonic *ordering* EOR3 < PMULL is the load-bearing
claim."

**Lock-1 audit.** Pure citation + epistemic-honesty caveat. It cites a
published ISA manual for a latency claim and disclaims the unpublished
absolute. It touches no producer, no consumer, no substrate — it is
prose attached to a comparison of two intrinsics already in the design.
CH5-irrelevant on its face; the audit confirms it introduces no object.
**Cardinality unchanged — one.**

### §1.3 — P2-D edit 3: §6.3 per-primitive checkasm vs deferred-infra reword

**Edit** (`skv9-p2-D:1049-1062`): §6.3 reworded to distinguish two
deferral classes — (a) the *per-primitive checkasm tests*, which are
**not** deferred (each is a same-wave admission precondition per
§6.2.1); (b) the *broader host-instrumentation infrastructure* —
invariants 2-5 (forced feature masks, AAPCS64 ABI shim, fault
trampoline, cycle-counter binding) — which **is** SK-V10+ deferred.

**Lock-1 audit.** This is a wording disambiguation of an existing
deferral. V2 CH5 already audited the §6.2.1 checkasm ownership table at
V-D.8 ("the checkasm tests are parity oracles, one per primitive …
`tests/` differential harness, not a runtime substrate") and the
invariant 2-5 deferral at V-D.10 ("DEFERRED — CH5 silent; host-side
instrumentation does not multiply substrates"). The V3 edit *sharpens
the boundary* between those two — it does not move it. The reword makes
explicit that the checkasm tests are not in the deferred bucket; this
*tightens* the no-orphan discipline (a test cannot be silently deferred
out of its broadening wave) and does not relax it. Neither the parity
harness nor the host-instrumentation infrastructure is a runtime
substrate; the reword changes the partition labelling, not the object
count. **Cardinality unchanged — one. CH5 stays silent on invariant 2-5
exactly as at V2 D.10.**

### §1.4 — P2-D edit 4: §5.5 + §8 REDRESS 28/33 line-range citations

**Edit** (`skv9-p2-D:946-952`, `:1113`): REDRESS 28 gains the cite
`REDRESS.md:324-337`; REDRESS 33 gains `REDRESS.md:394-418`; §8's source
list expands the entry-28/33 lines with one-clause descriptions.

**Lock-1 audit.** Pure citation-precision edit — byte-range coordinates
appended to two already-named REDRESS entries. The §5.5 "material
differential against REDRESS 28 + 33" prose body is unchanged; only the
two entry names gain `:line-line` anchors. No producer, no consumer, no
substrate touched. This is the V3 analogue of the V2 R-CH5-1 cite
tightening (V-A.2: "the cite is now byte-accurate … same function, same
deletion, no new substrate"). **Cardinality unchanged — one.**

### §1.5 — P2-D edit 5: §0 footer cascade-sequencing constraint

**Edit** (`skv9-p2-D:1165-1171`): the §0 footer gains a
"Cascade-sequencing constraint" bullet: P2-D's four "block on P2-A
landing OR fail CH5" slices (§3 codec broadening, §4.4 CSSC CTZ, §5.3.1
EOR3 ladder, §5.4 dead-SIMD-scanner wiring) "collectively create a
wave-sequencing constraint S-P3 must honour: P2-A must land in the same
wave as any of these P2-D consumer slices … the wave may not be split."

**Lock-1 audit.** This is the load-bearing edit to scrutinise — it is
the V3 edit nearest to substrate territory. The audit conclusion: it is
the *opposite* of a coupling introduction. The four slices it names were
each *already* audited by V2 CH5 as carrying a hard same-wave block on
P2-A (R-CH5-3 §3 codec at V-D.2; R-CH5-5 §4.4 CSSC CTZ at V-D.6; §5.3.1
EOR3 at V-D.7; §5 dead-scanner at §5). The V3 edit *aggregates* four
pre-existing per-slice blocks into one §0-footer statement of their
joint consequence. It records a *constraint on wave composition* — "all
four P2-D consumer slices must co-land with P2-A" — which is the precise
Lock-1 no-orphan / no-substrate-first discipline made explicit at the
sequencing layer. A constraint that *forbids* shipping P2-D consumers
without their P2-A substrate is a cardinality-*preserving* rule: it
prevents the very drift (P2-D primitive shipping into a parser-owned
REDRESS-rejected helper because P2-A did not land) that would climb
cardinality to two. The bullet adds no object — it adds a sequencing
falsifier. The phrase "the wave may not be split" binds S-P3, not a
producer. **Cardinality unchanged — one. The edit hardens Lock-1
compliance.**

### §1.6 — P2-F edit 1: §5.2 inline REDRESS-33 pre-block citation

**Edit** (`skv9-p2-F:355-358`): §5.2's sonic-rs
`match_tiny_plain_string`-class lesson gains an inline note that "the
dispatch-site NEON wiring shape it describes is pre-blocked by
`skinny/REDRESS.md` entry 33 (`REDRESS.md:394-418` …), and any S-P3
attempt to wire it carries the REDRESS-33 material-differential gate."

**Lock-1 audit.** This edit *demotes* a SOTA-competitor observation from
implied-admission to explicit-lesson. V2 CH5 audited P2-F's posture at
V-F.4 (synthesis-grade claims walked back to S-P3) and §3.2 (P2-F V2 is
Lock-1-cleaner than V1 after the §7.2/§7.3 strips). The V3 edit
continues that subtractive trajectory: it explicitly marks the §5.2
sonic-rs lesson as "a SOTA architecture *lesson*, not an admission" and
binds any future wiring attempt to a REDRESS-33 gate. A pre-block
citation cannot add a substrate — it forbids one shape (the wrong-call-
site NEON wiring REDRESS 33 rejected) from being mistaken for an
admitted producer. **Cardinality unchanged — one. The edit narrows the
admissible surface.**

### §1.7 — P2-F edit 2: §2.1 ContainerNext + §5.4 CollapsedStage cites

**Edit** (`skv9-p2-F:86-90`, `:382-387`): the §2.1 ContainerNext
reference gains the enum-definition cite (`generated.rs:341`, consumed
`:134-135`, emitted by `consume_array_next` at `:348-375`); the §5.4
CollapsedStage reference is anchored to "the fifth `BackendShape`
variant … at `restart/ARCHITECTURE.md` §7.3 (`LayoutFacts.backend_shape`,
enum at `ARCHITECTURE.md:1086`)."

**Lock-1 audit.** Pure path-anchor citations on two terms already in the
report. ContainerNext is named at its live generated-code coordinate;
CollapsedStage is anchored to its design-corpus enum definition. Neither
edit introduces ContainerNext or CollapsedStage as a *new* object — both
are pre-existing: ContainerNext is the V9.5 Wave-2 close already in
`generated.rs`; CollapsedStage is the fifth `BackendShape` variant
already enumerated in `LayoutFacts.backend_shape ∈ {EagerTape,
OffsetTape, EventTape, SinkOnly, CollapsedStage}` (Lock 10 verbatim).
`BackendShape` is a per-rule side-table field on the *one* layout
facts table — five shapes are five *modes of the one substrate*, not
five substrates (the SC-6 §1.3 reading: "the projection may be an offset
tape, event tape, or collapsed-stage event sink"). Citing the enum
coordinate does not multiply the substrate; it locates the existing
five-variant field. **Cardinality unchanged — one.**

### §1.8 — P2-F edit 3: §5 asmjson primitive-vocabulary path anchor

**Edit** (`skv9-p2-F:309-313`): the asmjson §5 primitive-vocabulary
reference gains "the canonical primitive-class taxonomy this report
inherits is anchored at … `skv9-p1-v3-B-xctrace-time-profiler.md` §1.5."

**Lock-1 audit.** Pure cross-report path anchor. The asmjson sidecar
classification (`historical:sk-v7-sidecar-profile`, "never an admission
anchor") audited at V2 V-F.6 is untouched; this edit only adds a path to
the P1-V3-B taxonomy the report already inherited. asmjson remains a
measurement sidecar in the strictness-classification sense, correctly
disclaimed — V2 V-F.6's disposition holds verbatim. No object added.
**Cardinality unchanged — one.**

### §1.9 — V3-edit audit summary

| # | Report | Edit | Class | Cardinality |
|---|---|---|---|---|
| E1 | P2-D §5.3.1 | EOR3 six-row no-regression gate | gate (falsifier) | one — unchanged |
| E2 | P2-D §5.3.1 | EOR3 latency cite + caveat | citation | one — unchanged |
| E3 | P2-D §6.3 | per-primitive vs deferred-infra reword | wording | one — unchanged |
| E4 | P2-D §5.5/§8 | REDRESS 28/33 line-range cites | citation | one — unchanged |
| E5 | P2-D §0 | cascade-sequencing constraint bullet | gate (sequencing) | one — hardened |
| E6 | P2-F §5.2 | REDRESS-33 inline pre-block cite | citation | one — narrowed |
| E7 | P2-F §2.1/§5.4 | ContainerNext + CollapsedStage cites | citation | one — unchanged |
| E8 | P2-F §5 | asmjson taxonomy path anchor | citation | one — unchanged |

Eight of eight edits are pure citation (E2, E4, E6, E7, E8), gate
(E1, E5), or wording (E3). Zero design changes. Zero new producers.
Zero substrate-cardinality shifts. Two edits (E5, E6) *hardened*
Lock-1 compliance by aggregating / narrowing pre-existing
no-orphan blocks.

---

## §2 — V3 dispositions

The V3 verify audits each of the 8 surgical edits as one disposition,
plus per-report regression-sweep dispositions confirming the
non-edited substrate sections of P2-D and P2-F did not drift, plus a
sweep of the four CH5-untouched reports (P2-A/B/C/E unchanged in
`212971a3`).

### §2.1 — Per-edit dispositions (the 8 V3 edits)

| # | V3 edit | Citation | Verdict |
|---:|---|---|---|
| W-1 | E1 — §5.3.1 EOR3 six-row no-regression gate. Does the gate imply a parallel substrate? | `skv9-p2-D:857-864` | The gate is a pass/fail predicate on one bench run of six WIN rows; it gates *admission* of one EOR3 body into one producer (`bitmap_prefix_xor_64`). Structurally identical to the §4.4 CSSC CTZ gate audited at V2 V-D.6 as "no-orphan guard, not a substrate." No object constructed. **HONOURED** |
| W-2 | E2 — §5.3.1 EOR3 latency cite + M5-Max-unpublished caveat. | `skv9-p2-D:818-822` | Pure citation to ARM DDI 0487 + epistemic caveat. Touches no producer/consumer/substrate. **HONOURED** |
| W-3 | E3 — §6.3 reword distinguishing per-primitive checkasm (same-wave precondition) from deferred invariants 2-5. Does the reword move the deferral boundary? | `skv9-p2-D:1049-1062` | The reword sharpens an existing partition; it does not move it. Tightens the no-orphan discipline (checkasm cannot be silently deferred out of its broadening wave). Neither bucket is a runtime substrate. **HONOURED** |
| W-4 | E4 — §5.5/§8 REDRESS 28/33 `:line-line` cites. | `skv9-p2-D:946-952`, `:1113` | Pure citation precision — byte-range anchors on two already-named REDRESS entries. V3 analogue of the V2 R-CH5-1 cite tightening (V-A.2). **HONOURED** |
| W-5 | E5 — §0 cascade-sequencing constraint: four P2-D consumer slices must co-land with P2-A; "the wave may not be split." Does aggregating four blocks create coupling? | `skv9-p2-D:1165-1171` | The edit aggregates four *pre-existing* per-slice same-wave P2-A blocks (R-CH5-3, R-CH5-5, §5.3.1, §5) into one §0-footer joint statement. It records a wave-composition constraint that *forbids* shipping P2-D consumers without their P2-A substrate — the precise no-substrate-first discipline. Cardinality-*preserving*; hardens Lock 1. **HONOURED** |
| W-6 | E6 — §5.2 inline REDRESS-33 pre-block cite; sonic-rs lesson explicitly "not an admission." | `skv9-p2-F:355-358` | Demotes a SOTA observation from implied-admission to explicit-lesson + binds future wiring to a REDRESS-33 gate. Continues P2-F's V2 subtractive trajectory (V-F.2/V-F.3). Narrows the admissible surface. No substrate. **HONOURED** |
| W-7 | E7 — §2.1 ContainerNext (`generated.rs:341`) + §5.4 CollapsedStage (`ARCHITECTURE.md §7.3`, enum `:1086`) cites. Do five `BackendShape` variants imply five substrates? | `skv9-p2-F:86-90`, `:382-387` | Both terms pre-exist; the cites locate their live coordinates. `BackendShape ∈ {EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` is a per-rule side-table field on the *one* `LayoutFacts` table — five modes of one substrate (SC-6 §1.3: "the projection may be an offset tape, event tape, or collapsed-stage event sink"), not five substrates. **HONOURED** |
| W-8 | E8 — §5 asmjson primitive-vocabulary path anchor to P1-V3-B §1.5. | `skv9-p2-F:309-313` | Pure cross-report path anchor. asmjson sidecar classification (V2 V-F.6) untouched; asmjson stays a measurement sidecar, correctly disclaimed. **HONOURED** |

### §2.2 — P2-D regression sweep (CH5-untouched substrate sections)

| # | Section verified unchanged by `212971a3` | Citation | Verdict |
|---:|---|---|---|
| W-9 | §2.5 `consume_structural` deletion (R-CH5-1) — the V2 dual-`rg` gate + byte-accurate `:292-306` cite. | P2-D §2.5 (untouched in V3 diff) | The V3 diff does not touch §2.5. R-CH5-1's RESOLVED state holds; one SIMD producer, parser consumes by move, deletion-of-emission gate intact. **HONOURED** |
| W-10 | §3 codec same-wave consumer binding (R-CH5-3) — the hard P2-A block. | P2-D `:453-462` (untouched) | The V3 diff does not touch `:453-462`. The §0 E5 cascade bullet *re-cites* the §3 block — consistent, not contradictory. R-CH5-3 RESOLVED state holds. **HONOURED** |
| W-11 | §4.3 32-byte widening one-consumer/one-mask binding (R-CH5-4). | P2-D §4.3 `:626-651` (untouched) | The V3 diff does not touch §4.3. R-CH5-4's one-external-consumer / one-mask binding holds; shape selection still routes to S-P3. **HONOURED** |
| W-12 | §6.2.1 per-kernel checkasm ownership table (R-CH5-6). | P2-D `:1010-1024` (untouched) | The V3 diff does not touch §6.2.1; the E3 §6.3 reword *references* §6.2.1 ("per §6.2.1 each missing differential is a same-wave admission precondition") — consistent. R-CH5-6 RESOLVED state holds; no ownerless test. **HONOURED** |
| W-13 | §6.3 invariant 2-5 deferral — host instrumentation. | P2-D `:1049-1062` (E3 reword) | E3 reworded §6.3 but preserved the invariant 2-5 → SK-V10+ deferral, now explicitly partitioned from the non-deferred checkasm tests. CH5 silent on host instrumentation (V2 D.10 disposition holds verbatim). **DEFERRED (CH5 silent)** |

### §2.3 — P2-F regression sweep (CH5-untouched substrate sections)

| # | Section verified unchanged by `212971a3` | Citation | Verdict |
|---:|---|---|---|
| W-14 | §7.2 DirectBuild emit-site strip (V2 F3 subtractive fold). | P2-F §7.2 (untouched in V3 diff) | The V3 diff does not touch §7.2. The codec consumer cardinality stays at one (string-match path only); the REDRESS-66-69-reopening expansion stays stripped. **HONOURED** |
| W-15 | §7.3 REDRESS-33 admission strip + §7.4 research-dependency DAG. | P2-F §7.3/§7.4 (untouched) | The V3 diff does not touch §7.3/§7.4. §7.4 stays a research-dependency DAG (no wave plan, no per-slice cost). The E6 §5.2 REDRESS-33 inline cite is *consistent* with the §7.3 strip — both narrow the same surface. **HONOURED** |
| W-16 | §4 asmjson sidecar classification (`historical:sk-v7-sidecar-profile`). | P2-F §4 (untouched), E8 anchor at `:309-313` | E8 adds a taxonomy path anchor; the sidecar disclaimer at §4 is untouched. asmjson stays a measurement sidecar, never a parsing substrate (V2 V-F.6 holds). **HONOURED** |

### §2.4 — Cross-report no-drift sweep (P2-A/B/C/E)

| # | Report | Citation | Verdict |
|---:|---|---|---|
| W-17 | P2-A — not in the `212971a3` diff. | `git show 212971a3 --stat` (P2-A absent) | P2-A unchanged since the V2 fold. The V2 CH5 cardinality-one verdict (SIMD index transient producer; class column co-indexed on one `Tape<'input>`; `consume_structural` deleted) holds without re-audit. **HONOURED** |
| W-18 | P2-B — not in the `212971a3` diff. | stat (P2-B absent) | P2-B unchanged. `AnyGrammar` stays a compile-only `cfg`-gated proof witness; four-member `EventGrammar` trait. V2 CH5 verdict holds. **HONOURED** |
| W-19 | P2-C — not in the `212971a3` diff. | stat (P2-C absent) | P2-C unchanged. Gate/report layer only; serde-as-oracle non-conflation intact. V2 CH5 verdict holds. **HONOURED** |
| W-20 | P2-E — not in the `212971a3` diff. | stat (P2-E absent) | P2-E unchanged. One codec primitive, five const-generic specialisations; S7→S11 ordered kernel removal (R-CH5-7). V2 CH5 verdict holds. The R-CH5-7 paragraph-level `rg`-falsifier tightening carries forward to S-P3 unchanged. **HONOURED** |

### §2.5 — V3 disposition count

| Source | HONOURED | DEFERRED (CH5 silent) | VIOLATION |
|---|---:|---:|---:|
| 8 V3 edits (W-1 … W-8) | 8 | 0 | 0 |
| P2-D regression sweep (W-9 … W-13) | 4 | 1 | 0 |
| P2-F regression sweep (W-14 … W-16) | 3 | 0 | 0 |
| Cross-report no-drift (W-17 … W-20) | 4 | 0 | 0 |
| **Total** | **19** | **1** | **0** |

20 V3 dispositions (≥15 required by the task contract). 19 HONOURED +
1 DEFERRED (CH5-silent: P2-D §6.3 invariant 2-5 host instrumentation,
identical to V2 D.10 / V1 D.7) + 0 VIOLATION. Zero dispositions where a
V3 edit introduced new coupling.

---

## §3 — Aggregate verdict

**ACCEPT.** 20 V3 dispositions: 19 HONOURED + 1 DEFERRED (CH5-silent) +
0 VIOLATION.

### §3.1 — The 8 V3 edits are pure citation / gate / wording

Every one of the 8 surgical edits in `212971a3` falls into one of three
classes, none of which can shift substrate cardinality:

- **Citation** (E2, E4, E6, E7, E8 — five edits): ARM DDI 0487 anchor;
  REDRESS 28/33 `:line-line` ranges; REDRESS-33 inline pre-block;
  ContainerNext `generated.rs:341` + CollapsedStage `ARCHITECTURE.md
  §7.3` enum cite; asmjson taxonomy path anchor. A citation adds a
  coordinate; it constructs no object.
- **Gate** (E1, E5 — two edits): the §5.3.1 EOR3 six-row no-regression
  maintain gate; the §0 cascade-sequencing constraint. A gate is a
  pass/fail predicate or a wave-composition rule; it gates *admission*,
  it builds no substrate. Both gates *harden* Lock 1 — E1 by importing
  the §4.4 CSSC CTZ no-orphan posture, E5 by aggregating four
  pre-existing same-wave-P2-A blocks into one explicit "the wave may
  not be split" constraint.
- **Wording** (E3 — one edit): the §6.3 reword sharpening the partition
  between non-deferred per-primitive checkasm tests and deferred
  invariant 2-5 host instrumentation. It relabels a partition; it moves
  no object.

No edit is a design change. No edit adds a producer. No edit adds a
retained queryable object. The V3 fold is, by construction, the
narrowest possible CH3/CH6 residual closure — it touched 86 lines across
two reports, all citation/gate/wording.

### §3.2 — The §5.3.1 EOR3 no-regression gate implies no parallel substrate

The task's specific question. The §5.3.1 EOR3 gate (E1) gates the S-P3
*admission* of one `veor3q_u8` shift-XOR ladder body into one producer
— `bitmap_prefix_xor_64`'s SHA3-gated path (`skv9-p2-D:840-844`). The
gate's content is "the six W10b WIN rows must hold, else no EOR3 body
ships." This is a *correctness/no-regression predicate on a single bench
run*; it is structurally the §4.4 CSSC CTZ gate, which V2 CH5 audited at
V-D.6 as "a no-orphan guard, not a substrate." The EOR3 slice still
declares "its only consumer is the §5 structural-bitmap producer, which
is P2-A union-substrate scope" (`:864-866`) — one consumer, P2-A's one
union substrate. The SHA3-gated EOR3 body is "host-cap-conditional,
predicate-guarded, scalar fallback unconditional" (`:843-844`) — the
same admissibility shape as `digit_mac` and the AES gadget: one producer
with a host-gated faster path and an unconditional scalar oracle. A
host-capability predicate selecting one of two bodies *inside one
producer* is not two producers — it is Lock-16's allowlist shape, and
the scalar fallback is the parity oracle, not a parallel substrate. The
gate implies no parallel substrate.

### §3.3 — Substrate cardinality stays at one across V3-folded P2-D + P2-F

Cardinality stays at **one** across both V3-folded reports:

- **P2-D** — every ASM kernel (§3 codec, §4.3 32-byte widening, §4.4
  CSSC CTZ, §5.3.1 EOR3, §5 dead-scanner) is a scalar-oracle-backed
  Layer-1 primitive whose same-wave consumer is P2-A's one union
  substrate. The V3 E5 cascade-sequencing bullet makes the joint "all
  four P2-D consumer slices co-land with P2-A" constraint explicit —
  this is a cardinality-*one* enforcement, forbidding the P2-A-absent
  drift that would create a parser-owned second producer. The E1 EOR3
  gate adds a no-regression precondition on one producer's host-gated
  body. The E3 §6.3 reword preserves the invariant 2-5 deferral as
  CH5-silent host instrumentation. No P2-D edit adds a substrate.
- **P2-F** — the >SOTA path stays the integrated P2-A + P2-E + P2-D
  synthesis at cardinality one. The E6 §5.2 REDRESS-33 inline pre-block
  *narrows* the admissible surface (the sonic-rs dispatch-site NEON
  lesson is explicitly "not an admission"); the E7 ContainerNext /
  CollapsedStage cites locate pre-existing objects (ContainerNext is the
  V9.5 Wave-2 close; CollapsedStage is the fifth `BackendShape` mode of
  the one substrate, not a fifth substrate); the E8 asmjson anchor
  leaves the sidecar disclaimer intact. P2-F V3 is, if anything,
  marginally Lock-1-cleaner than P2-F V2 — E6 continues the subtractive
  trajectory.

No V3 edit conflates Track 1 ≡ Track 2 — none of the 8 edits touches a
comparator. Every gate remains Track 1 versus strict competitor.

### §3.4 — CH5 V3 cohort verdict

**ACCEPT.** Per ORCHESTRATOR §3Z: V1 CH5 ACCEPT (cycle 1), V2 CH5 ACCEPT
(cycle 2), V3 CH5 ACCEPT (regression re-verify). CH5 was converged at
V2 and placed zero fold demand on V3; the V3 fold targeted CH3/CH6
residuals only. This V3 verify confirms the 8 CH3/CH6 surgical edits did
not collaterally regress CH5: all 8 are citation/gate/wording, two
(E5, E6) actively hardened Lock-1 compliance, and substrate cardinality
holds at one across both V3-folded reports. CH5 remains converged.

---

## §4 — New coupling risks

The V3 verify found **no new coupling risk** introduced by the 8-edit
fold. Two observations carry forward to S-P3 — neither is a CH5 fault;
both are inherited from V2 unchanged plus one V3-specific reading.

### §4.1 — Carry C-CH5-V3-1: cascade-sequencing constraint is a hard S-P3 input (not a fault)

The V3 E5 §0 cascade-sequencing bullet (`skv9-p2-D:1165-1171`) is the
single most CH5-relevant V3 edit, and it is *favourable* — it converts
four scattered per-slice "block on P2-A" clauses into one explicit
wave-composition constraint: P2-A must co-land with any of §3 codec
broadening / §4.4 CSSC CTZ / §5.3.1 EOR3 / §5 dead-scanner wiring, and
"the wave may not be split." **Observation for S-P3**: this constraint
must appear verbatim in S-P3's P2-D wave dispatch contract as a
sequencing falsifier — if a S-P3 wave plan schedules any of the four
P2-D consumer slices in a wave that does not also land P2-A, the wave
plan fails CH5 at the S-P3 CHALLENGE. The V3 edit *states* the
constraint; S-P3 must *gate* on it. Not blocking on the V3 verify — it
is a forward carry, and the constraint as worded is already a hard
"may not be split."

### §4.2 — Carry C-CH5-V3-2: V2 carries C-CH5-V2-1 and C-CH5-V2-2 still stand

The two V2 §4 carries are untouched by the V3 fold (P2-E and P2-D §4.3
were not in the `212971a3` diff):

- **C-CH5-V2-1** (R-CH5-7 named `rg` falsifier) — P2-E §7.1 S11
  expresses orphan-free kernel removal via S7→S11 slice ordering; S-P3
  should add the named `rg 'bbnf_simd::aarch64::unescape_uxxxx::'`
  post-wave falsifier as belt-and-braces. Unchanged by V3.
- **C-CH5-V2-2** (R-CH5-4 dispatch-shape selection) — P2-D §4.3 retains
  both 32-byte dispatch shapes, both bound to one external consumer +
  one mask; S-P3 declares which shape lands and, if shape (b), states
  the dispatch is internal to one producer body returning one
  `StringSpecialBlock` per call. Unchanged by V3.

Both remain paragraph-level S-P3-dispatch tightenings on
already-cardinality-one designs. Neither blocks.

### §4.3 — Standing observation (not a fault)

The E3 §6.3 reword's distinction between non-deferred per-primitive
checkasm tests and deferred invariant 2-5 host instrumentation is
*tighter* than the V2 text — it removes a possible ambiguity that a
future agent might have read §6.3's "deferral" as covering the checkasm
tests too, which would have re-opened the R-CH5-6 substrate-first/
consumer-later pathology at the test layer. The V3 reword closes that
ambiguity. Recorded as a positive — the V3 fold improved CH5's posture
here, it did not regress it.

---

## §5 — Sources

- `restart/skinny/tranches/sk-v9/research/p2/hardening/V2/CH5.md` — the V2 CH5 disposition (42 rows, 7/7 conditions RESOLVED, ACCEPT).
- `restart/locks/LOCKS.md:34` — Lock 1 verbatim (substrate union; cardinality; 2026-05-04 reframe; no parallel substrate / no orthogonal codepath).
- `restart/locks/LOCKS.md:60` — Lock 14 (full grammar generalisation; zero overfitting).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md` §1.3 / §2.2 — "the projection may be an offset tape, event tape, or collapsed-stage event sink"; the cardinality discriminant.
- commit `212971a3` (`docs(sk-v9-p2-v3): fold V2 CHALLENGE residuals — 8 surgical edits`) — the V3 fold under audit; `git show 212971a3 --stat`: P2-D `+45/−9`, P2-F `+33/−8`, two files, no file created.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md` — V3-folded P2-D (§5.3.1 `:818-822`, `:857-866`; §5.5 `:946-952`; §6.3 `:1049-1062`; §8 `:1113`; §0 footer `:1165-1184`).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-F-sota-teardown-m5max.md` — V3-folded P2-F (§2.1 `:86-90`; §5 `:309-313`; §5.2 `:355-358`; §5.4 `:382-387`; §0 footer `:671-688`).
- `skinny/crates/runtime/src/grammars/json/generated.rs:341` — the live `ContainerNext` enum (P2-F E7 cite).
- `restart/ARCHITECTURE.md` §7.3 — `LayoutFacts.backend_shape` five-variant `BackendShape` enum (P2-F E7 CollapsedStage cite).
