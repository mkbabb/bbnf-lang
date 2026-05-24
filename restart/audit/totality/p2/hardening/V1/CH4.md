---
lens: CH4
lens_name: COST / admission discipline
pass: T-P2-research
cycle: V1
hardening_authority: restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md
spec_authority: restart/prompts/totality/PASS-2-RESEARCH.md §3 CH4
agent_role: write-only adversary
generated_at: 2026-05-23
verdict: REVISE
accept_rate: 33% (2/6 dossiers ACCEPT, 3/6 REVISE, 1/6 REJECT-IN-PART)
findings_count: 11
load_bearing_findings: [CH4-F1, CH4-F2, CH4-F4, CH4-F7]
artefacts_reviewed: [2A-sota-landscape.md, 2B-primitive-vocabulary.md, 2C-grammar-neutrality.md, 2D-cost-model.md, 2E-host-arch-esoterica.md, 2F-parse-that-gaps.md]
---

# T-P2 V1 CH4 — COST / Admission Discipline

Lens: CH4 COST per PASS-2-RESEARCH.md §3.
Authority: CHALLENGE-CONTEXT.md §2 CH4 binding — every grounded primitive
carries admission cost (scalar reference + checkasm parity per Lock 16);
same-wave consumer named; LOC/risk realistic; no orphan-kernel research.
WRITE-ONLY; aggregator commits eight V1 hardening files atomically.

## §0 — Verdict

**REVISE** (33% ACCEPT-rate; 2/6 dossiers admit at-cycle; 3/6 require V2
fold; 1/6 carries one disqualifying orphan-kernel row).

The dossier cohort uniformly *declares* the Lock 16 admission contract —
scalar reference + checkasm parity + same-wave consumer + hardware gate +
published citation — and 2A/2B make the contract operationally legible.
But the same cohort does not uniformly *honour* its own contract:

1. 2B explicitly catalogues 3/9 Layer-1 primitives as SKELETON-only
   (`FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`)
   with no scalar reference and no checkasm test at HEAD. That is an
   admitted orphan-kernel triple inside the dossier whose own A4 section
   defines orphan-kernel as inadmissible. This is the binding CH4 fail.
2. 2D admits four of five `BackendShape` lowerers as marker-string
   placeholders (`EagerTape`, `OffsetTape`, `EventTape`, `CollapsedStage`
   at `crates/codegen/src/lower/{eager,offset,event,collapsed}_tape.rs`)
   while still grounding the five-shape enum as "the V1 candidate set".
   A search domain four of whose five members have no lowerer is not a
   cost-extractable candidate set; it is a manifest of orphan kernels at
   the shape layer.
3. 2A, 2C, 2E, 2F price their grounded primitives at "admission discipline
   binds" rather than "<candidate-id> admits per these cells: scalar
   path, checkasm path, BBNF_SIMD_STRICT status, hardware gate, same-wave
   consumer path, row gate, LOC envelope, rollback path." V1's CH4 close
   asked for the latter; V1 delivers the former.

V1 is a useful research cycle and folds forward as discipline carrier; it
does not pass CH4 as written. The blockers are operational, not
architectural — convert the prose Lock 16 contract into a per-candidate
admission ledger that T-P3 can read directly into wave gates.

## §1 — Per-dossier disposition

| dossier | CH4 verdict | rationale (one line) |
|---|---|---|
| 2A-sota-landscape.md | ACCEPT (qualified) | Names the process discipline correctly; T2A-LAC-V1-03 binds Lock 16 manifest schema; T2A-LAC-V1-05 binds fail-closed abrogate caps for e-graph saturation / CSP timeout / stale-cost / generated LOC / row regression / parity failure. Schema-bind is V1-grade. The qualifier: it does not itself populate the manifest for any specific primitive — that obligation lands on 2B/2E. |
| 2B-primitive-vocabulary.md | REJECT-IN-PART | A4 declares the four-cell admission contract per primitive; A5 audits 6/9 admitted-shape vs. 3/9 SKELETON-only at HEAD (`FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`). The dossier accepts as evidence rows three contracts that its own A4 says are inadmissible. Open Research Question OQ-2 already asks whether `FRAME_PUSH/POP_BOUNDED` should ship admission artefacts or be deleted. The required V2 action is binary: each SKELETON contract ships scalar + checkasm + same-wave consumer in a single wave, or is deleted from `bbnf.asm`. R3 says exactly this; the dossier must enact it, not just declare it. |
| 2C-grammar-neutrality.md | REVISE | Correctly refuses primitive parity alone and requires generated-grammar policy plus same-wave CSS/JSON/Sheets/BBNF-self consumers. The 7-step onboarding test is the right CH4 instrument. The revise: per-grammar onboarding rows do not yet quantify the generated LOC envelope, the changed-crate set, the parity-harness count, or the rollback surface. Risk labels are coarse (`low`/`med`/`high`) rather than evidence-anchored to candidate cost facts. |
| 2D-cost-model.md | REVISE | Correctly refutes the P1..P8 cascade as the optimizer and binds `CostFacts` to an active objective/frontier schema. LAC-2D-02 / LAC-2D-04 / LAC-2D-05 are CH4-shaped amendment candidates. The revise: V1 names only one abrogate threshold (`>30% stale/static fallback` rule) and leaves egraph node cap, egraph iteration cap, CSP timeout ceiling, generated-LOC growth threshold, row-regression threshold, and parity-harness failure disposition unbound. 2A's T2A-LAC-V1-05 names these caps explicitly — 2D should adopt them as its own load-bearing schema rather than ground them at LAC-only granularity. The four-of-five marker-string lowerers are also a CH4 blocker, not just a code-quality observation. |
| 2E-host-arch-esoterica.md | REVISE | Properly conservative: TBL admitted, ASCII run-skip micro-proven not admitted, PMULL/CSSC route-specific reopens, UDOT/EOR3/LD4/cache-hints inventory until same-wave consumer exists, SVE2 MATCH out of NEON scope. The revise: 5 active S-P2 V3 candidates (C-P2C-2/3/4/5/8) need explicit per-candidate adoption-cost rows — body LOC range, scalar-oracle status PRESENT/ABSENT, checkasm cell status PRESENT/ABSENT/EXTENSION, hardware gate, named consumer file:function, expected row delta, abrogate condition. Coarse adoption-cost prose at "route-specific reopen" / "inventory until consumer exists" granularity does not gate-extract for T-P3. |
| 2F-parse-that-gaps.md | REVISE | Identifies the largest hidden adoption cost (the `bbnf-regex` absorption of the Thompson-NFA / DFA / Hopcroft pipeline at `docs/parse-that/regex-engine.md:15-25` not implemented at HEAD; lazy-DFA fallback at `regex-engine.md:62` absent). Nine primitive gaps catalogued, each with a published-primitive citation + upstream-or-vendor decision. The revise: gap rows do not yet price the import in LOC ranges (e.g. NFA construction ≈, lazy-DFA ≈, Hopcroft ≈), crate boundaries (`bbnf-regex` vs `bbnf-simd` vs `parse-that-regex`), license/version pin for any vendored body, parity-harness count, and first row consumer. The V4 admission ledger format the frontmatter references is the right schema; populate it row-by-row in V2. |

## §2 — Load-bearing findings (per binding evidence)

### CH4-F1 — 2B Layer-1 SKELETON triple = inadmissible at HEAD (orphan-kernel admitted as evidence row)

**Source.** 2B-primitive-vocabulary.md §A4 (admission contract: scalar
reference + checkasm parity + same-wave consumer + hardware gate +
citation); §A5 audit table (6/9 admitted-shape vs. 3/9 SKELETON-only);
§R3 ("skeleton macro presence closes Lock 16 — refuted"). Local
verification: `crates/bbnf-simd/ext/x86/bbnf.asm` declares 9 `%macro`
contracts; `crates/bbnf-simd/src/scalar/` carries 6 scalar bodies
(`bitmap_next_set_bit.rs`, `bitmap_prefix_xor_64.rs`,
`bulk_emit_positions_64.rs`, `byte_class_from_eq_set_64.rs`,
`byte_class_from_table_64.rs`, `eob_pad_clamp.rs`). The 3-row delta is
exactly the SKELETON triple. Checkasm `tests/checkasm_*.rs` count
matches: 6 admitted, 3 SKELETON.

**CH4 disposition.** REJECT-IN-PART. The dossier correctly admits the
3/9 SKELETON state but advances them as part of the 9-contract Layer-1
vocabulary without enacting R3's deletion/wire-or-delete obligation.
"Skeleton-contract only" is not a Lock 16 close state — Lock 16 v+1 at
`restart/locks/LOCKS.md:335-342` admits exactly four close states
(`wired`, `deleted`, `scalar-delegate-non-ASM`,
`architectural-block-with-REDRESS`). The SKELETON triple is none of
those. The V2 fold action is binary: per primitive, either ship scalar
oracle + checkasm cell + same-wave consumer in a single wave, or delete
the contract declaration from `bbnf.asm`. 2B's own OQ-1 (FSM scalar
oracle as switch-statement equivalent) and OQ-2 (audit
`skinny/crates/runtime/src/` for an open-frames consumer for
FRAME_PUSH/POP_BOUNDED) are the exact V2 verify-actions that resolve
this finding.

### CH4-F2 — 2D four-of-five lowerers ship as marker strings = `BackendShape` candidate set is paper-architecture

**Source.** 2D-cost-model.md row `P1-1B-D6` refuted; table-row
"Four of five lowerers carrying real logic" refuted. Local verification:
`crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15-17`
each emit `rule {name} -> <shape>` marker strings.

**CH4 disposition.** REVISE. 2D acknowledges this as a refuted assertion
but does not bind it to the abrogate ledger. CH4 reads it as a candidate
set blocker: search-and-extract over a five-shape `BackendShape` is only
defensible when the candidates themselves admit; four of five emitting
marker strings means the cost extractor has no real costs to extract for
80% of its domain. The dossier's resolution must either (a) implement
the four lowerers in the SK-V14 horizon (LOC budget required), or (b)
amend Lock 10 to retire the four non-implemented shapes from the V1
candidate set and admit `SinkOnly` as the only present shape; the
five-shape enum then becomes an aspirational manifest, not a search
domain. 2D's LAC-2D-04 already pins `CollapsedStage` admissibility
behind concrete kernel emission + scalar oracle + checkasm cell + same-
wave consumer; extend the same admission condition to `EagerTape`,
`OffsetTape`, `EventTape`.

### CH4-F3 — Abrogate criteria named but not bound as gates

**Source.** 2D LAC-2D-02 names the cost schema axes (objective vector,
frontier/dominance, scalarisation, extraction method, evidence
freshness, stale/static fallback marker) but binds only one numeric
threshold (`>30% stale/static fallback`). 2A T2A-LAC-V1-05 binds the
fuller abrogate set (e-graph saturation ≤50000 nodes / ≤10000 classes /
≤30 iter; CSP timeout ≤1 s/grammar; stale-cost ≤30%; generated LOC
growth; row regression; parity/checkasm failure).

**CH4 disposition.** REVISE. The fuller abrogate set lives in 2A; 2D
references the abrogate concept but does not adopt the same numeric
floors. V2 should fold T2A-LAC-V1-05's caps into 2D's LAC-2D-02 (or
publish them as a separate LAC-2D-06) so abrogate gates are testable in
one place and the same numbers anchor every dossier's cost ledger.
"Useful but isolated" rule (V1 §Evidence) is now V2-fold-required.

### CH4-F4 — Per-candidate adoption-cost ledger missing across 2A/2B/2C/2E/2F

**Source.** PASS-2-RESEARCH.md §3 CH4: "every grounded primitive
carries admission cost ... same-wave consumer named; LOC/risk realistic;
no orphan-kernel research." V1 cohort delivers admission *discipline*
prose without per-candidate adoption-cost rows. 2A T2A-LAC-V1-03 names
the eight-cell manifest schema (abstract primitive name + published
citation + hardware gate + scalar reference + checkasm cell + corpus
parity + same-wave consumer + row admission/measured rejection) but
the manifest is unpopulated for any specific primitive in any dossier.

**CH4 disposition.** REVISE. The single-largest V2 work item. Each of
the following candidates needs a populated manifest row before T-P3
admission: 2B's 9 Layer-1 contracts (6 admitted-shape + 3 SKELETON);
2D's 5 `BackendShape` variants (1 admitted + 4 marker-string); 2E's 5
S-P2 V3 LOCKED candidates (C-P2C-2 PMULL/CSSC structural union, C-P2C-3
UDOT digit MAC, C-P2C-4 TBL/TBX escape decode, C-P2C-5 string-special
64-byte context, C-P2C-8 parse-attribution profile rebuild gate) plus
inventory entries (UDOT digit MAC, ASCII run-skip, EOR3/BCAX/LD4, cache
hints); 2F's 9 primitive gaps (2 regex/HIR, 4 SIMD-scan, 2 string, 1
float). Total ≈ 28 ledger rows. Each row carries the 2A T2A-LAC-V1-03
eight cells + 2A T2A-LAC-V1-05 abrogate cells + 2C 7-step onboarding-
test status + 2F V4 admission-ledger LOC envelope + crate boundary +
license/version pin + rollback path.

### CH4-F5 — `BBNF_SIMD_STRICT=1` flag named only in 2B, not propagated as cohort-wide checkasm precondition

**Source.** 2B §A4 #2 ("strict mode `BBNF_SIMD_STRICT=1` is mandatory
for admission per `LOCKS.md:320-322`"). 2A T2A-LAC-V1-03 cites the
checkasm differential cell but does not require the strict-mode flag.
2D/2E/2F do not reference the flag.

**CH4 disposition.** REVISE (minor). V2 must propagate
`BBNF_SIMD_STRICT=1` as a cohort-wide checkasm-cell precondition: any
primitive's admission row carries `BBNF_SIMD_STRICT=on` when admitted
through SIMD path. Cohort consistency matters because Lock 16
admissibility is one contract for all dossiers, not a per-dossier
variant.

### CH4-F6 — Same-wave consumer named at family granularity, not file:function granularity

**Source.** 2A T2A-LAC-V1-03 ("same-wave production consumer"); 2B §A4
#3 ("same-wave consumer NAMED — a runtime/codegen path that consumes
the primitive in production"); 2C generalisation rule ("same-wave
CSS/JSON/Sheets/BBNF-self consumers"); 2E inventory pattern ("until
same-wave consumer exists"); 2F V5 fold ("same-wave consumer named per
S-P3 P3-A V1 cell-(c) bindings"). None of the cited consumer references
land at `crate::module::function` granularity; they land at family
granularity ("a generated grammar path", "a row-moving consumer",
"cell-(c)").

**CH4 disposition.** REVISE. V2 admission-ledger rows must name the
consumer at `crate::module::function` (or `path/to/file.rs:line`)
granularity. Family-granular naming is paper-close cover for the
orphan-kernel risk it claims to refuse. PASS-2-RESEARCH.md §3 CH4
"same-wave consumer named per primitive" reads at function granularity
because that is the only granularity at which "no orphan-kernel
research" is verifiable.

### CH4-F7 — LOC/risk realism absent from every dossier

**Source.** PASS-2-RESEARCH.md §3 CH4 "LOC/risk for adoption is
realistic". 2A, 2B, 2C, 2D, 2E, 2F: zero LOC ranges, zero crate-touch
counts, zero generated-size deltas, zero parity-test counts. 2F V5
references the V4 admission ledger format as the per-row schema but
does not populate LOC envelopes.

**CH4 disposition.** REVISE. V2 admission-ledger rows must include
approximate LOC ranges per primitive (scalar oracle LOC, checkasm cell
LOC, same-wave consumer LOC), touched-crate set (`bbnf-simd`,
`bbnf-regex`, `parse-that-regex`, `codegen`, `passes`, `ir`, `runtime`,
`bbnf-bench`), generated-size delta envelope, equality-test count
expected, and rollback path (commit-revert / feature-gate-off /
substrate-disable). Numbers can be approximate (±50%); absence is the
defect.

### CH4-F8 — Admission state vocabulary not normalised across cohort

**Source.** V1 CH4 carry-forward §Blockers #6 ("V2 should normalize
vocabulary: source-backed, scalar-backed, checkasm-backed,
micro-proven, production-wired, row-admitted, measured-rejected, and
architectural-block are distinct states"). V1 dossiers use
heterogeneous vocabulary ("grounded", "admitted", "wired", "inventory",
"candidate", "skeleton", "scalar-delegate-non-ASM",
"architectural-block-with-REDRESS", "architecture pressure", "process
grounded", "partial"). The same artefact-state appears under multiple
names; distinct artefact-states appear under one name.

**CH4 disposition.** REVISE. V2 must publish a single normalised state
vocabulary in the dispatch context or one of the dossier frontmatters,
then enforce it across the six dossiers. Suggested minimum vocabulary:
`source-present-only` (citation exists, no local artefact),
`scalar-backed` (scalar reference exists), `checkasm-backed` (scalar +
parity differential exist), `micro-proven` (microbench cited), `wave-
admitted` (production caller wired same-wave), `row-admitted`
(production row measured improved), `measured-rejected` (parity-equal
but row-regressed), `architectural-block-with-REDRESS` (REDRESS row
binds), `deleted` (contract removed from manifest). Lock 16 v+1's four
close states map cleanly onto the last four.

### CH4-F9 — 2F regex/HIR import not priced; lazy-DFA fallback unscoped

**Source.** 2F V5 lists two regex/HIR gaps. The Thompson-NFA → DFA →
Hopcroft pipeline at `docs/parse-that/regex-engine.md:15-25` is *not*
implemented at HEAD; lazy-DFA fallback at `regex-engine.md:62` is
absent. `bbnf-regex` is a 322-line classifier crate with no DFA, no
NFA, no `find_at`. The upstream-or-vendor decision is named ("regex
gaps upstream into `bbnf-regex`") but no LOC envelope is given for the
NFA construction body, the DFA conversion body, the Hopcroft
minimization body, or the lazy-DFA fallback.

**CH4 disposition.** REVISE. V2 must price each regex-import body: ≈
LOC range, license/version pin if vendored (regex-syntax / regex-
automata / RE2-derived), parity-harness count vs `regex-automata` and
RE2 reference, first row consumer (which `parse-that-regex` consumer
calls `Dfa::compile()`?), and abrogate gate (lazy-DFA falls back to
NFA at what state count?). 2F's V5 frontmatter binds the V4 admission
ledger format as the per-row schema; that schema must be populated for
the regex/HIR gaps before T-P3 can scope the bbnf-regex absorption.

### CH4-F10 — 2C generalisation rows lack per-grammar parity-harness count

**Source.** 2C 7-step onboarding test (T-P2 2C-primitive-onboarding-
test) requires CSS L4 + Sheets + BBNF-self transfer evidence; per-
grammar GrammarConfig fields enumerated. The onboarding test does not
quantify how many parity tests per grammar must pass before fleet-wide
admission.

**CH4 disposition.** REVISE. V2 onboarding-test rows must bind a
minimum parity-harness count per grammar (e.g. "CSS L4: ≥3 declaration-
value rows; Sheets: ≥3 formula rows; BBNF-self: ≥3 grammar rows") and
name the corpus paths. Absence converts the 7-step onboarding test
from a verifiable gate into a discretionary checklist.

### CH4-F11 — 2E adoption-cost rows present at coarse granularity for inventory entries (UDOT/EOR3/LD4/cache hints)

**Source.** 2E §Executive Summary: "PMULL/CSSC are route-specific
reopens, UDOT/EOR3/LD4/cache hints remain inventory until a same-wave
consumer exists". 2E does not enumerate per-entry adoption-cost rows
for the inventory set.

**CH4 disposition.** REVISE. V2 must publish a per-inventory-entry
adoption-cost row for each of UDOT digit MAC, EOR3 ternary bitwise,
BCAX ternary bitwise, LD4 interleaved classify, cache hints (PRFM
discipline), ASCII run-skip. Each row carries: (a) abstract primitive
name; (b) Arm intrinsic and ACLE feature macro; (c) hardware gate; (d)
scalar oracle status PRESENT/ABSENT; (e) checkasm cell status PRESENT/
ABSENT/EXTENSION; (f) candidate same-wave consumer path; (g) expected
row gate / measured-rejection threshold; (h) abrogate condition. 2E's
SVE2 MATCH refutation is correctly out of scope and does not need a
ledger row.

## §3 — Cycle disposition (per §3Z auto-convergence)

| metric | V1 outcome | §3Z target | gap |
|---|---|---|---|
| CH4 ACCEPT-rate per dossier | 33% (2/6) | ≥95% | -62 pp |
| Open critical defects (CH4-load-bearing) | 4 (CH4-F1, F2, F4, F7) | 0 | -4 |
| Orphan-kernel research admitted as evidence | 3 (Layer-1 SKELETON triple) | 0 | -3 |
| Marker-string lowerers admitted in candidate set | 4 (EagerTape, OffsetTape, EventTape, CollapsedStage) | 0 | -4 |
| Per-candidate admission ledger rows populated | 0 / ≈28 | ≈28 / ≈28 | -28 |
| Abrogate-gate numeric thresholds bound | 1 (`>30% stale fallback`) | ≥6 (egraph nodes, egraph iter, CSP timeout, stale cost, generated LOC, row regression) | -5 |

**Cycle disposition: REVISE.** V1 does not converge under §3Z. V2 is
required; CH4 fold-requirements are §4 below.

The V1 cycle does not show paper-folding (no row draws the same REVISE
across three cycles — V1 is the first cycle); §4 escalation is not
triggered. Hard ceiling V ≤ 5 (§4): V2 is on track if §4 fold lands
fully; V3 may be needed for ledger population if 2A/2B/2C/2D/2E/2F
authors split the 28-row work.

## §4 — V2 fold requirements (per dossier, CH4-bound)

1. **2A author.** Promote T2A-LAC-V1-03 (Lock 16 manifest schema) +
   T2A-LAC-V1-05 (fail-closed abrogate caps) from amendment-candidate
   rows into a populated cohort-wide template. Publish the template as
   a dispatch artefact or as 2A §5. The template anchors every other
   dossier's ledger rows.

2. **2B author.** Enact §R3: for each of `FSM_DISPATCH_THREADED`,
   `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, choose binary disposition
   (ship scalar + checkasm + same-wave consumer in V2 wave, OR delete
   from `bbnf.asm`). OQ-1 verify-action is the FSM scalar oracle build
   (switch-statement equivalent); OQ-2 verify-action is the
   `skinny/crates/runtime/src/` open-frames consumer audit. Either
   action closes the SKELETON row. Populate the 9-row Layer-1
   admission ledger using the 2A template.

3. **2C author.** Bind the 7-step onboarding test to a numeric parity-
   harness floor per grammar (CSS L4 / Sheets / BBNF-self) and name
   the corpus paths. Convert risk labels (`low`/`med`/`high`) into
   evidence-anchored references.

4. **2D author.** For `EagerTape`, `OffsetTape`, `EventTape`,
   `CollapsedStage`: bind to LAC-2D-04-style admissibility (concrete
   kernel emission + scalar oracle + checkasm cell + same-wave
   consumer) OR amend Lock 10 to retire from V1 candidate set. Adopt
   2A T2A-LAC-V1-05 abrogate caps into LAC-2D-02 (or publish as LAC-
   2D-06). Populate the 5-row `BackendShape` admission ledger.

5. **2E author.** Populate per-candidate adoption-cost rows for the 5
   S-P2 V3 LOCKED candidates (C-P2C-2/3/4/5/8) and for each inventory
   entry (UDOT digit MAC, EOR3, BCAX, LD4, cache hints, ASCII run-
   skip). Each row carries the 2A T2A-LAC-V1-03 eight cells. SVE2
   MATCH out-of-scope correctly carries no row.

6. **2F author.** Price each of the 9 primitive gaps using the V4
   admission ledger format (already named in V5 frontmatter). Regex/
   HIR gaps require LOC ranges for NFA / DFA / Hopcroft / lazy-DFA
   bodies, license/version pin for any vendored body, parity-harness
   count vs `regex-automata` and RE2 reference, first row consumer at
   `crate::module::function` granularity, and lazy-DFA abrogate gate
   (state-count threshold).

7. **Cohort.** Normalise admission state vocabulary (CH4-F8) at the
   dispatch level. Propagate `BBNF_SIMD_STRICT=1` as cohort-wide
   checkasm-cell precondition (CH4-F5). Same-wave consumer references
   land at `crate::module::function` granularity, not family
   granularity (CH4-F6).

## §5 — Accepted carry-forward (what V1 got right and V2 must preserve)

- **Admission contract.** Lock 16 v+1's five cells (scalar reference,
  checkasm parity, same-wave consumer, hardware gate, published
  citation) are correctly identified as the admission gate and are
  uniformly cited across the cohort.
- **Orphan-kernel refusal as principle.** 2B §R3, 2C generalisation
  rule, 2E inventory pattern, and 2F V5 fold all correctly refuse
  orphan-kernel research as a class. The defect is enaction, not
  principle.
- **Refutation as load-bearing output.** 2A T2A-REF-001..005, 2B
  R1..R4, 2D refuted-rows table, 2E refutations, 2F three load-
  bearing refutations honour PASS-2-RESEARCH.md §3 §8 #5 ("refutation
  is a first-class output"). CH4 reads the refutation discipline as
  intact.
- **DAV1D/FFmpeg/VLC process spine.** 2A T2A-PROC-001 / T2A-PROC-002
  ground the checkasm differential discipline in primary literature
  at pinned SHAs (FFmpeg `08571418`, dav1d `1718ff9a`, simdjson
  `168ef580`). The process anchor is robust and should not be
  re-litigated in V2.
- **Skeleton state honestly disclosed.** 2B §A5 publishes the 6/9 vs.
  3/9 SKELETON audit at HEAD. CH4 reads honest disclosure as the
  reason the SKELETON triple appears here as a REJECT-IN-PART row
  rather than as a hidden defect; the disclosure is correct, the
  disposition is the V2 work.
- **Abrogate caps named, even if not fully bound.** 2A T2A-LAC-V1-05
  enumerates the abrogate axes (e-graph saturation / CSP timeout /
  stale-cost / generated LOC / row regression / parity failure). V2
  binds them numerically.

## §6 — Closing

V1 establishes the Lock 16 admission contract and discloses the
SKELETON/marker-string defects honestly. It does not yet convert the
contract into a populated per-candidate admission ledger that T-P3 can
read directly into wave gates. The V2 fold is operational (populate
≈28 ledger rows, normalise vocabulary, bind abrogate gates
numerically, deliver `crate::module::function`-granular consumer
names, enact the SKELETON triple disposition); it is not architectural.
A V2 that completes §4 lands ≥95% ACCEPT for CH4 on its first cycle.

CH4 verdict for V1: **REVISE**. The accepted carry-forward is the
admission discipline itself; the REVISE work is its enaction.

## §7 — Authority register

- `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md` (HEAD = 8d5e4e8f6)
- `restart/prompts/totality/PASS-2-RESEARCH.md` §3 CH4
- `restart/locks/LOCKS.md` Lock 16 v+1 (lines 282-360, especially 335-342 close-state vocabulary)
- `restart/audit/totality/p2/2A-sota-landscape.md` (T2A-LAC-V1-03, T2A-LAC-V1-05)
- `restart/audit/totality/p2/2B-primitive-vocabulary.md` (§A4 admission contract, §A5 audit at HEAD, §R3 SKELETON refutation, OQ-1, OQ-2)
- `restart/audit/totality/p2/2C-grammar-neutrality.md` (7-step onboarding test, generalisation rule)
- `restart/audit/totality/p2/2D-cost-model.md` (LAC-2D-02, LAC-2D-04, LAC-2D-05; P1-1B-D6 refutation; four-marker-lowerer table row)
- `restart/audit/totality/p2/2E-host-arch-esoterica.md` (S-P2 V3 LOCKED candidates C-P2C-2/3/4/5/8; inventory entries UDOT/EOR3/BCAX/LD4/cache hints/ASCII run-skip; SVE2 MATCH refutation)
- `restart/audit/totality/p2/2F-parse-that-gaps.md` (9 primitive gaps; V4 admission ledger format; bbnf-regex 322-line classifier vs Thompson-NFA / DFA / Hopcroft pipeline gap)
- `skinny/crates/bbnf-simd/ext/x86/bbnf.asm` (9 `%macro` declarations)
- `skinny/crates/bbnf-simd/src/scalar/` (6 scalar bodies at HEAD)
- `skinny/crates/bbnf-simd/tests/checkasm_*.rs` (6 checkasm cells at HEAD)
- `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15-17` (marker-string lowerers)
- `skinny/REDRESS.md` lines 2823-2940 (REDRESS 96/97/98 retained-shape closures); 3603-3632 (W2 escape mask parity-only); 3766-3820 (W4 delimiter find microbench-only halted)
