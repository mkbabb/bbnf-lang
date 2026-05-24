# SK-V14 S-P3 V1 CHALLENGE CH4 — Cost Lens

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V1. Lens: CH4 COST.
Date: 2026-05-23.
Lens scope: every wave carries (LOC budget + hard cap + phase breakdown research/plan/redress per SKINNY-TRIUMVIRATE.md + same-wave-consumer per primitive); wave count ≤ 12 (skinny-bracket ceiling per ORCHESTRATOR.md §3Z); shortlist ≤ 8; CF-3 3-gate admission cell wired correctly per candidate; W6 9-sub-wave folding (PRUNE-4) carries cumulative cap.
Authority: `PASS-3-SYNTHESIS-PLAN.md §3` CH4 (`SK-V14 P3 CHALLENGE-CONTEXT.md §2` CH4 specialisation; `SKINNY-TRIUMVIRATE.md §7` per-phase caps; `ORCHESTRATOR.md §3Z` skinny-bracket ceiling); `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CHALLENGE-CONTEXT.md`.
Discipline: write-only; no git add/commit; aggregator commits.
HARD CAP: 30 min.

## §1 — Synthesis (concrete; cites P3 artefact line, SPEC section, contract §, ORCHESTRATOR ceiling)

### §1.1 — CH4 cost-lens binding clauses derived from authority

Per `PASS-3-SYNTHESIS-PLAN.md §3` CH4 verbatim:

> "CH4 COST — does every wave carry a LOC budget, a hard cap, a phase
> breakdown (research / plan / redress per SKINNY-TRIUMVIRATE.md), and a
> same-wave-consumer requirement per primitive? Is the wave count ≤ 12
> (the skinny-bracket ceiling per ORCHESTRATOR.md §3Z)? Is the
> shortlist ≤ 8?"

The V1 CHALLENGE-CONTEXT §2 amplification adds two binding clauses
specific to V1 disposition focus:

- **CF-3 admission 3-gate cell per candidate** (post-S-P2 LOCK carry-forward discipline) must be wired correctly per S-P2 V3 §6.1.
- **W6 9-sub-wave folding (PRUNE-4) carries cumulative cap** so the single-wave-slot count doesn't paper over an 810-min implementation block.

Per `SKINNY-TRIUMVIRATE.md §7` the per-phase cap table is the binding floor:

| Phase | Cap | Commit type |
|---|---:|---|
| Research (6 parallel) | 30 min each (wall: 30 min) | `docs(sk-v{N}-wave{W}-research):` |
| CHALLENGE (optional, 6 parallel) | 60 min wall | `docs(sk-v{N}-wave{W}-challenge):` |
| Plan (1-2 agents) | 30 min | `docs(sk-v{N}-wave{W}-plan):` |
| Redress (1 agent) | 75 min (60 impl + 15 measure) | `feat(...)` or `docs(...-redress):` |
| Wave total | ~3-4 hours wall | 3-4 commits per wave |

Per `ORCHESTRATOR.md §3Z` and `SKINNY-TRIUMVIRATE.md §3` the skinny-bracket
ceiling is 12 waves; a bracket exceeding 12 escalates `BLOCKED`.

Per `SKINNY-TRIUMVIRATE.md §8` the same-wave-consumer rule is load-bearing
verbatim: "Every redress commit that lands a primitive / kernel / new
generated path MUST include the hot-path caller that exercises it." No
exception.

### §1.2 — Authority traversal: where each clause must land

| CH4 clause | Authoritative landing | Verification site |
|---|---|---|
| LOC budget per wave | SPEC §2 wave-manifest "Source/edit LOC budget" column | `SPEC.md:235-248` |
| Hard cap per wave | SPEC §2 wave-manifest "Implementation/redress cap" column | `SPEC.md:235-248` |
| Phase breakdown (research/plan/redress) | DISPATCH-PROMPT §1 + SPEC §2 phase-cap table | `SPEC.md:263-273` |
| Same-wave-consumer per primitive | SPEC §1 non-negotiable + per-wave "Same-wave consumer:" line | `SPEC.md:216, 363-365, 434-436, ...` |
| Wave count ≤ 12 | SPEC §2 manifest row count W0..W11 | `SPEC.md:235-248` (12 rows) |
| Shortlist ≤ 8 | P3-A §2 candidate-shortlist row count C1..C8 | `p3a:167-178` (8 rows) |
| CF-3 3-gate admission cell per candidate | P3-A §2.1 carry-forward census table | `p3a:170-178` |
| W6 9-sub-wave cumulative cap | SPEC §2 W6 row + §9 W6 sub-wave manifest | `SPEC.md:243` ("≤810 min cumulative"), `SPEC.md:706-719` |

## §2 — Deliverable (per-clause CH4 disposition)

Eight CH4 dispositions, one per binding clause, with verbatim path:line
verification and explicit ACCEPT / REVISE / REJECT verdict.

### §2.1 — Every wave carries a LOC budget — VERDICT: ACCEPT

**Verification.** SPEC §2 wave manifest (`SPEC.md:235-248`) carries a
"Source/edit LOC budget" column for all 12 waves W0..W11. Spot-check:

- W0: "0 production behavior LOC; reauthorized telemetry gate/report/Lock14 scope per Section 3 accounting; ≤250 report/gate/test/doc LOC" (`SPEC.md:237`).
- W1: "≤1.08k C-2 source/test LOC + ≤500 C-5 part-A revert (delete-heavy); total ≤1.58k" (`SPEC.md:238`).
- W2: "≤2.0k C-3 part-A source/test LOC; generated output named separately" (`SPEC.md:239`).
- W5: "≤1.4k C-1 part-A source/test LOC" (`SPEC.md:242`).
- W6: "≤2.0k C-1 part-B aggregate across 9 sub-waves (avg ~220 LOC/grammar; generated output uncounted)" (`SPEC.md:243`).
- W11: "0 source LOC; docs/RESULTS/REDRESS/HANDOFF/SPEC reconciliation only" (`SPEC.md:248`).

Aggregate envelope explicit at `SPEC.md:258-261`: C-1 2.8k-3.4k; C-2
600-1.08k; C-3 1.2k-2.0k; C-4 800-1.4k; C-5 250-500; aggregate
~5.65k-8.38k; overflow >20% escalates per `[generated-size-budget]`.

**Compliance gate.** Every wave row has a LOC budget cell populated.
Generated outputs explicitly exempted from source LOC budget per
`SPEC.md:252-254` ("Generated outputs do not consume the source LOC
budget, but every generated file must be named, diff-audited, and
included in the revert slice"). Overflow handling at `SPEC.md:255-256`:
"A wave plan that exceeds either its LOC budget or the 90-minute
implementation / redress cap must split before dispatch or return
REVISE." Per `[generated-size-budget]` (per-tranche line-count budget)
the discipline is binding.

**Verdict: ACCEPT.** All 12 wave rows carry LOC budget; aggregate
envelope cited; overflow handling explicit.

### §2.2 — Every wave carries a hard cap — VERDICT: ACCEPT (with one REVISE on W6 aggregate clarity)

**Verification.** SPEC §2 wave manifest "Implementation/redress cap"
column carries ≤90 min for every wave W0..W11.

- W0..W5 + W7..W11: ≤90 min per row (`SPEC.md:237-248`).
- W6: "≤90 min per sub-wave (W6.1..W6.9); aggregate ≤810 min" (`SPEC.md:243`).

Phase caps at `SPEC.md:263-273` reproduce `SKINNY-TRIUMVIRATE.md §7`:
research ≤30 min/agent (max 6 agents); plan ≤30 min; CHALLENGE 60-90
min when first-of-class/substrate-touching/primitive/high-risk; redress
60 impl + 15 measure = 75 min, 90 min hard ceiling.

Rerun ceilings at `SPEC.md:275-292` carry per-wave focused-verification
slots + one gate-refresh; extra reruns beyond the ceiling become REDRESS
cost evidence, not retry room.

**REVISE finding (W6 cumulative cap surface).** `SPEC.md:243` reports
"aggregate ≤810 min" inside the manifest cell, which is consistent with
P3-B's §2.1 binding ("≤90 min per sub-pass × 9 = ≤810 min cumulative
cap" at `p3b:81`), but the cumulative cap surface should ALSO appear in
SPEC §9 W6's per-sub-wave section header (currently `SPEC.md:706-719`
enumerates 9 sub-waves with risk classification but does not explicitly
re-cite the 810-min aggregate ceiling at the sub-wave-manifest level).
Per V1 CHALLENGE-CONTEXT §2 "W6 9-sub-wave folding (PRUNE-4) carries
cumulative cap" — the cumulative cap exists in the manifest cell, but
the W6 per-sub-wave table at `SPEC.md:708-718` should add a footnote or
trailing line re-asserting the 810-min aggregate ceiling to prevent
single-sub-wave LOC overflow from going unbudgeted at sub-wave dispatch
time. Folds into V2 dispatch as a clarity REVISE.

**Verdict: ACCEPT** (with the W6-aggregate-restatement REVISE folded
into V2). All 12 wave rows carry a hard cap; phase caps verbatim from
SKINNY-TRIUMVIRATE §7; rerun ceilings present per wave.

### §2.3 — Phase breakdown (research/plan/redress) per SKINNY-TRIUMVIRATE — VERDICT: ACCEPT

**Verification.** SPEC §2 phase-cap table at `SPEC.md:263-273`
reproduces SKINNY-TRIUMVIRATE §7 verbatim with all four phases
(research / plan / CHALLENGE / redress) and their respective caps.

`SPEC.md:218` non-negotiable: "Research, plan, CHALLENGE when required,
and redress remain distinct phases per `SKINNY-TRIUMVIRATE.md §9`
triumvirate-role-separation."

`SPEC.md:270` redress cap explicit: "60 impl + 15 measure = 75 min; 90
min hard ceiling including source edits, generation, verification,
RESULTS/REDRESS updates, and rollback."

`SPEC.md:272-273` overflow handling: "If a planned implementation
cannot fit the 90-min redress cap, the plan must split before dispatch
or return REVISE."

DISPATCH-PROMPT.md (per P3-F §2.2 structure declaration at
`p3f:198-208`) carries §1 per-wave triumvirate contract + §2 phase caps
+ §3 same-wave consumer mandate. Cross-referenced from the SPEC at
`SPEC.md:1136-1137`.

**Verdict: ACCEPT.** Phase breakdown verbatim from SKINNY-TRIUMVIRATE
§7 + role-separation per §9 binding; triumvirate-role-separation
non-negotiable.

### §2.4 — Same-wave-consumer requirement per primitive — VERDICT: ACCEPT

**Verification.** SPEC §1 non-negotiable at `SPEC.md:216` verbatim:

> "No primitive, kernel, generated path, or substrate representation
> without a same-wave hot-path consumer (per S-P2 V3 §6.1 CF-3 3-gate
> cell: scalar-reference status / checkasm-parity expectation /
> same-wave-consumer NAMED)."

Per-wave "Same-wave consumer:" line present in every W0..W11 section:

- W0: "`xtask gate-json` consumes every emitted telemetry field..." (`SPEC.md:363-365`).
- W1: "`xtask gate-json` consumes the rebound comparator columns + per-iter equality column..." (`SPEC.md:434-436`).
- W2: "`cargo xtask regen-css` itself + `check-css-l4-*` CI invocations..." (`SPEC.md:493-495`).
- W3: "the loader at `bbnf-bench/src/css_l4_corpus.rs` consumes the new corpora..." (`SPEC.md:545-546`).
- W4: "`cargo xtask regen-css` re-emission of the deleted runtime twins is the consumer..." (`SPEC.md:603-605`).
- W5: "`cargo xtask regen-css` (W2-emitted) becomes the production consumer of the new `GrammarProvider` trait dispatch..." (`SPEC.md:662-664`).
- W6: "per-sub-wave: the per-grammar parser test suite + the per-grammar bench rows consume the newly-emitted runtime..." (`SPEC.md:748-750`).
- W7: "the runtime divergence on the named pre-wave row is the consumer..." (`SPEC.md:813-815`).
- W8: "every admitted CSS L4 row has a generated Track 1 grammar-derived consumer + an independent lightningcss/cssparser oracle..." (`SPEC.md:875-877`).
- W9: "selected JSON direct + typed rows consume generated Track 1 direct or typed work + independent Track 2 proof..." (`SPEC.md:935-937`).
- W10: "the distinct parse_only path + the `sonic_rs::Skipper` comparator both consume the same generated_json emission..." (`SPEC.md:995-997`).
- W11: "close checklist and document reconciliation" (`SPEC.md:1048`).

Per `SKINNY-TRIUMVIRATE.md §8` verbatim ("If the consumer wire-up is
omitted: the primitive is an orphan kernel. REJECT and record in
REDRESS. No exception"). The SK-V5 failure shape (Class A NEON kernel
parity-green but unwired) is the named regression-prevention discipline.

**Verdict: ACCEPT.** All 12 waves carry an explicit "Same-wave
consumer:" line; SPEC §1 non-negotiable binds CF-3 3-gate cell discipline
to every primitive landing.

### §2.5 — Wave count ≤ 12 (skinny-bracket ceiling) — VERDICT: ACCEPT

**Verification.** SPEC §2 wave manifest rows: W0, W1, W2, W3, W4, W5,
W6, W7, W8, W9, W10, W11 — count = 12 (`SPEC.md:235-248`). Matches the
`ORCHESTRATOR.md §3Z` + `SKINNY-TRIUMVIRATE.md §3` 12-wave ceiling
verbatim.

Per P3-B §1.2 Phase 9: "The 12-wave total (W0..W11) is at the §3Z
ceiling" (`p3b:62`). Per P3-F §1.2: "The R1-R10 obligation × the S-P0
prune-list = the following pack: ... Total: 12 waves (W0..W11)"
(`p3f:32-53`). Cross-witnessed.

Per `SKINNY-TRIUMVIRATE.md §3`: "If the SK-V{N} bracket has > 12 waves
without convergence, the orchestrator escalates to user with `BLOCKED`."
SPEC §2.1 + §16 carry the per-wave conditional-dispatch chain to keep
the wave count fixed at 12 with W6's 9 sub-waves folded under a single
top-level wave slot.

**Verdict: ACCEPT.** Wave count = 12 at the ceiling; 9 W6 sub-waves
correctly folded under single wave slot to preserve the count.

### §2.6 — Shortlist ≤ 8 — VERDICT: ACCEPT

**Verification.** P3-A §2.1 candidate-shortlist table at `p3a:167-178`
enumerates 8 candidates:

| # | Candidate (canonical) |
|---|---|
| C1 | `long_string_body_simd_scan` |
| C2 | `structural_index_singular_substrate_consumer` |
| C3 | `digit_block_simd_accumulate` |
| C4 | `unicode_escape_neon_nibble_decode` |
| C5 | `parse_attribution_envelope_cracker` |
| C6 | `force_inline_lto_envelope_discipline` |
| C7 | `ascii_whitespace_skip_64` |
| C8 | `BackendShape::SinkOnly` activation |

Count = 8 at the `PASS-3-SYNTHESIS-PLAN.md §2` ≤8 ceiling (`p3a:5,
:167`). NF-CH6-4 canonical-name binding consolidations explicit at
`p3a:172` (C1 = P2-A C2 ∪ P2-E Gap 1 ∪ P2-F C1+C2) — preventing the
three orthogonal SIMD bodies inflation per S-P2 V3 §6.2.

**Verdict: ACCEPT.** Shortlist = 8 at the ceiling; canonical-name
binding correctly applied to consolidate convergent identifiers.

### §2.7 — CF-3 3-gate admission cell per candidate (CH4 V3 carry-forward) — VERDICT: ACCEPT

**Verification.** P3-A §2.1 shortlist table column "CF-3 3-gate cell"
populated for all 8 candidates (`p3a:170-178`):

| # | Scalar-ref status | Checkasm-parity status | Same-wave consumer NAMED |
|---|---|---|---|
| C1 | PRESENT (`string_block.rs:31`) | EXTENSION (sibling-shape template at `tests/checkasm_*`) | `parse_that_regex::skip_string_plain_trusted` at `lib.rs:547` |
| C2 | PRESENT (`scan.rs:32`) | EXTENSION (`skip_value_index_consumer_parity.rs`) | `parse_object_value_at_direct`, `parse_array_element_at_direct`, `DirectParser::skip_value` |
| C3 | PRESENT (`digit_mac.rs:5-22` scalar; sibling `byte_class_from_range_64.rs` queued per §2.Y) | EXTENSION (new `checkasm_digit_mac_x16.rs`) | direct-plane number kernel in `parse_array_element_at_direct`; typed-plane `parse_vec_cap_10800_scalar_f64`; CSS L4 `<number>` consumer |
| C4 | PRESENT (`unescape_uxxxx.rs:40` scalar; `read_hex_unit_scalar` at `lib.rs:945`) | EXTENSION (new `checkasm_unescape_uxxxx_x8.rs`) | `y_string_unicode` parse_only + `unicode_escapes` + `unicode_mixed` rows; CSS L4 escaped-ident |
| C5 | N/A (process discipline; envelope IS scalar) | N/A (process discipline; verification is cfg_attr flip + interactive samply) | 12 dispatch-envelope-internal primitives NAMED verbatim at `p3a:120` |
| C6 | N/A (build invariant; envelope IS scalar) | N/A (build invariant; verification is cargo asm + samply) | codegen template + cargo asm + samply re-record paired with C5 |
| C7 | PRESENT (`byte_class_from_eq_set_64.rs:1` scalar) | EXTENSION (sibling of existing `checkasm_ascii_set_member_find_64.rs`) | every JSON value-position prelude; CSS L4 declaration-value whitespace |
| C8 | PRESENT (`generated.rs:425-462` direct-emit body; `Tape::offset_bytes/flag_bytes/offset_capacity_bytes`) | EXTENSION (`sink_only_elision_parity.rs`) | 8 P1-B direct-plane rows where envelope is 70%+ top-1 |

P3-A §2.1 carry-forward census at `p3a:182` reads: "Carry-forward CF-3
3-gate completeness: 8/8 candidates carry the 3-gate cell explicitly.
Per CH4 V3 §2.2 + HARDENING-S-P2-V3-CONSOLIDATED §6.1." Cross-witnessed
verbatim binding to S-P2 V3 §6.1.

SPEC §1 non-negotiable at `SPEC.md:216` carries the CF-3 3-gate cell
into wave-program admission discipline ("No primitive ... without a
same-wave hot-path consumer (per S-P2 V3 §6.1 CF-3 3-gate cell:
scalar-reference status / checkasm-parity expectation /
same-wave-consumer NAMED").

**Verdict: ACCEPT.** All 8 candidates carry the explicit 3-gate cell;
process-discipline candidates (C5/C6) correctly mark N/A on
scalar-ref/checkasm with explicit substitution evidence (cfg_attr flip,
cargo asm); CF-3 carry-forward from S-P2 V3 §6.1 wired into SPEC §1
non-negotiable.

### §2.8 — W6 9-sub-wave folding carries cumulative cap — VERDICT: ACCEPT (with the §2.2 W6-aggregate-restatement REVISE noted)

**Verification.** SPEC §2 W6 row (`SPEC.md:243`) carries explicit
cumulative cap: "≤90 min per sub-wave (W6.1..W6.9); aggregate ≤810
min". SPEC §9 W6 section (`SPEC.md:682-770`) enumerates 9 sub-waves
W6.1..W6.9 by grammar name (`math, csv, bnf, ebnf, css_pretty, css_l4,
google_sheets, bbnf, json`) with per-sub-wave file count + risk
classification + topological order (substrate-before-consumer + guard-
rows-before-risk-rows).

Per P3-B §2.1 (`p3b:81`) the sub-wave cap discipline is binding: "≤90
min per sub-pass × 9 = ≤810 min cumulative cap; sub-pass split-before-
dispatch if any single sub-pass overflows the 90-min cap per SK-V8 SPEC
§2." The SPEC inherits this verbatim.

P3-A §2.2 architectural-sequencing item 3 (`p3a:199`) re-cites the
9-not-8 sub-wave count per S-P0 A6 finding. SPEC §9 entry gate at
`SPEC.md:703` carries: "PRUNE-4 sub-wave count is 9 NOT 8 per S-P0 §2.3
(`css_pretty` is the +1 over the SK-V13 baseline's 8)."

W6 aggregate exit gate at `SPEC.md:739-747` carries the forward
invariant + the 67-file collapse + the Pattern H opt-out enshrinement
discharge + the `LegacyPath` shim removal.

**Disposition note (V2 fold).** The 810-min cumulative cap is correctly
cited in the SPEC §2 manifest cell (`SPEC.md:243`) but is NOT
re-asserted explicitly in the SPEC §9 W6 sub-wave table at
`SPEC.md:706-718`. This is a clarity REVISE only (the cap exists; it
just isn't re-anchored at the sub-wave manifest header). V2 fold target:
add a one-line footnote to the W6 sub-wave table reading "Cumulative
W6.1..W6.9 cap: ≤810 min per SPEC §2 W6 manifest cell + P3-B §2.1
binding; per-sub-wave cap: ≤90 min." Folds with §2.2 REVISE.

**Verdict: ACCEPT** (with the V2-fold REVISE noted under §2.2 above —
single clarity adjustment, not a structural defect).

## §3 — Falsifiability binding (named verification commands per disposition)

Per `PASS-3-SYNTHESIS-PLAN.md §3` CH4 disposition vocabulary (ACCEPT /
REVISE / REJECT) + LAC-1E-12 executable-verification procedural
addendum.

| Clause | Verification command (executable at HEAD) | Expected output |
|---|---|---|
| LOC budget per wave | `grep -c '^| W' /Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/SPEC.md` (count of `^| W` rows in §2 manifest) | ≥12 wave rows |
| Hard cap per wave | `grep -cE '≤[0-9]+ min' SPEC.md` in §2 manifest table region | ≥12 cap citations |
| Phase breakdown | `grep -c 'SKINNY-TRIUMVIRATE.md §[79]' SPEC.md` (phase-cap binding citations) | ≥2 (one in §1 non-negotiable, one in §2 phase-cap table) |
| Same-wave consumer per wave | `grep -c 'Same-wave consumer:' SPEC.md` | =12 (one per W0..W11) |
| Wave count ≤ 12 | `grep -c '^| W[0-9]' SPEC.md` in §2 manifest | =12 |
| Shortlist ≤ 8 | `grep -c '^| C[1-9] ' p3a-candidate-shortlist.md` in §2.1 carry-forward census | =8 |
| CF-3 3-gate per candidate | `grep -c 'scalar-ref.*checkasm.*consumer' p3a-candidate-shortlist.md` | ≥8 (one per shortlist row, plus the carry-forward binding statement) |
| W6 cumulative cap | `grep -c '810 min' SPEC.md` | ≥1 |

Each disposition above carries the named verification command per
`[doc-integration-style]` + `[no-meta-doc-references]` + LAC-1E-12
("any cited path:line in any wave's plan or redress MUST be re-executed
at HEAD before commit").

## §4 — Pre-blocked routes (CH4-specific)

Per CH4 V3 §3 inheritance + S-P2 V3 §6.1 CF-3 carry-forward, the
following CH4 anti-patterns are pre-blocked from any V2 dispatch
amendment:

1. **Wave count inflation** — adding a 13th wave (e.g., promoting a W6
   sub-wave to top-level) breaches `ORCHESTRATOR.md §3Z` ceiling and
   triggers `BLOCKED` escalation. V2 must fold any new scope into the
   existing W0..W11 slots, not append.

2. **Shortlist inflation** — adding a 9th candidate breaches `PASS-3-
   SYNTHESIS-PLAN.md §2` ≤8 cap. V2 must consolidate per NF-CH6-4
   canonical-name binding (S-P2 V3 §6.2), not append.

3. **Missing same-wave-consumer** — any wave whose redress phase admits
   a primitive without naming its hot-path consumer in the same commit
   re-opens the SK-V5 orphan-kernel failure shape. Per
   `SKINNY-TRIUMVIRATE.md §8`: "No exception."

4. **Missing 3-gate CH4 admission cell** — per S-P2 V3 §6.1 CF-3
   binding (carry-forward to S-P3), every shortlisted candidate's
   admission manifest carries (scalar-ref status / checkasm-parity
   expectation / same-wave-consumer NAMED). V2 cannot drop any of the
   three columns from any candidate row.

5. **W6 sub-wave dispatch without cumulative-cap awareness** — if a
   single W6.N sub-wave consumes >90 min, the per-sub-wave cap binds
   split-before-dispatch per `p3b:81` + `SPEC.md:243`. The 810-min
   aggregate is not retry room.

6. **LOC budget overflow without REVISE** — per `SPEC.md:255-256`: "A
   wave plan that exceeds either its LOC budget or the 90-minute
   implementation / redress cap must split before dispatch or return
   REVISE." Silent overflow is REJECT.

7. **Phase-role merger** — per `SKINNY-TRIUMVIRATE.md §9` + SPEC §1
   non-negotiable at `SPEC.md:218`: research / plan / CHALLENGE /
   redress phases remain distinct commits. Merging plan + redress into
   one commit re-opens the SK-V5 failure shape per V3 triumvirate-
   discipline feedback.

## §5 — Sources

### §5.1 — V1 CHALLENGE-CONTEXT authority

- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CHALLENGE-CONTEXT.md` (43 lines; §0 authority + §1 artefacts + §2 V1 disposition focus + §3 discipline + §4 output structure).

### §5.2 — Contract authority

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3` CH4 verbatim (`PASS-3-SYNTHESIS-PLAN.md:128-132`).
- `restart/prompts/ORCHESTRATOR.md §3Z` (cohort LOCK convergence rule + V≤5 ceiling).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md §3` (12-wave ceiling), `§7` (phase caps), `§8` (same-wave-consumer rule), `§9` (triumvirate role separation).

### §5.3 — P3 artefacts under CH4 review

- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:5, 167-178, 182` (≤8 shortlist + CF-3 3-gate cell column + carry-forward census).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md:62, 81` (12-wave ceiling + 810-min cumulative cap).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md` (per-wave gate cap framing).
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md` (LOC budget for telemetry).
- `restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md` (per-wave REDRESS pre-block surface).
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md:32-53, 64-122` (12-wave pack + CF-3 + F-V2-P1ABC-RERECORD wiring).

### §5.4 — SPEC + DISPATCH-PROMPT under review

- `restart/skinny/tranches/sk-v14/SPEC.md:216` (CF-3 3-gate non-negotiable).
- `restart/skinny/tranches/sk-v14/SPEC.md:218` (triumvirate-role-separation non-negotiable).
- `restart/skinny/tranches/sk-v14/SPEC.md:235-248` (12-wave manifest with LOC budget + hard cap columns).
- `restart/skinny/tranches/sk-v14/SPEC.md:252-256` (generated-output exemption + overflow-split-or-REVISE).
- `restart/skinny/tranches/sk-v14/SPEC.md:258-261` (aggregate envelope + 20%-overflow escalation per `[generated-size-budget]`).
- `restart/skinny/tranches/sk-v14/SPEC.md:263-273` (phase-cap table verbatim from SKINNY-TRIUMVIRATE §7).
- `restart/skinny/tranches/sk-v14/SPEC.md:275-292` (per-wave rerun ceilings).
- `restart/skinny/tranches/sk-v14/SPEC.md:363-365, 434-436, 493-495, 545-546, 603-605, 662-664, 748-750, 813-815, 875-877, 935-937, 995-997, 1048` (per-wave Same-wave consumer lines W0..W11).
- `restart/skinny/tranches/sk-v14/SPEC.md:706-718` (W6 sub-wave manifest; clarity-REVISE target for explicit 810-min restatement).
- `restart/skinny/tranches/sk-v14/SPEC.md:739-747` (W6 aggregate exit gate).

### §5.5 — S-P2 carry-forward authority (CF-3 binding)

- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.1` (CF-3 3-gate admission cell binding verbatim).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.2` (NF-CH6-4 canonical-name binding for shortlist consolidation).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.3` (F-V2-P1ABC-RERECORD Stage-0 wave commitment per LOC + cap discipline).

### §5.6 — Memory feedback honored

- `[no-deferrals]` — phase-cap overflow forbids deferral to future tranches; in-pass split-or-REVISE is binding.
- `[dispatch-hard-cap]` — every dispatch carries cap; CH4 lens enforces.
- `[triumvirate-discipline]` — research / plan / redress role separation forbids merger.
- `[generated-size-budget]` — per-tranche line-count budget; overflow >20% blocks wave per SPEC §2.
- `[execute-planned-architecture]` — same-wave-consumer rule prevents orphan-kernel retreat.

---

## §6 — Lens disposition summary

| § | Clause | Verdict |
|---|---|---|
| §2.1 | Every wave carries a LOC budget | ACCEPT |
| §2.2 | Every wave carries a hard cap | ACCEPT (with W6-aggregate-restatement REVISE for V2 clarity fold) |
| §2.3 | Phase breakdown (research/plan/redress) | ACCEPT |
| §2.4 | Same-wave-consumer per primitive | ACCEPT |
| §2.5 | Wave count ≤ 12 | ACCEPT |
| §2.6 | Shortlist ≤ 8 | ACCEPT |
| §2.7 | CF-3 3-gate admission cell per candidate | ACCEPT |
| §2.8 | W6 9-sub-wave cumulative cap | ACCEPT (with §2.2 V2-fold REVISE noted) |

**CH4 ACCEPT-rate: 8/8 = 100%** (with 1 single-line clarity REVISE for V2 fold — adds explicit 810-min cumulative-cap restatement to SPEC §9 W6 sub-wave table header; does not block §3Z LOCK).

**Cycle disposition: V1 ACCEPT-bearing.** CH4 cost-lens converges on V1: every binding clause from `PASS-3-SYNTHESIS-PLAN.md §3` CH4 + V1 CHALLENGE-CONTEXT §2 (CF-3 + W6 cumulative cap) is honored across the 8 P3 artefacts + SPEC + DISPATCH-PROMPT. Single clarity REVISE for V2 fold is the §2.2 W6 sub-wave restatement; cycle is otherwise §3Z-ready for the aggregator's first-pass ACCEPT count.

**Key findings:**

1. **All 12 wave rows carry LOC budget + ≤90-min cap** (W6 carries ≤810-min cumulative; others ≤90 min each); generated outputs exempted; aggregate envelope ~5.65k-8.38k cited with 20%-overflow escalation.

2. **Same-wave-consumer line present in every W0..W11 section** of the SPEC (12/12); SPEC §1 non-negotiable binds CF-3 3-gate cell discipline to every primitive landing; no orphan-kernel risk.

3. **All 8 shortlist candidates carry CF-3 3-gate cell** (scalar-ref status / checkasm-parity expectation / same-wave-consumer NAMED) with process-discipline candidates (C5/C6) correctly marking N/A with substitution evidence cited.

4. **Wave count = 12 exactly at the ceiling**; W6's 9 sub-waves correctly folded under one top-level slot; sub-wave cumulative cap (810 min) cited in SPEC §2 manifest cell but should be re-cited in SPEC §9 W6 sub-wave table header for clarity (V2 fold).

5. **Phase caps verbatim from SKINNY-TRIUMVIRATE §7**; phase-role separation per §9 binding in SPEC §1 non-negotiable; CHALLENGE phase optional but recommended for first-of-class interventions per the contract.

6. **CF-3 3-gate carry-forward from S-P2 V3 §6.1 wired correctly** into SPEC §1 non-negotiable + P3-A §2.1 shortlist table; no candidate is admitted without the 3-gate cell.

7. **W6 cumulative-cap discipline holds** at the manifest-cell level (`SPEC.md:243` ≤810 min) and at the per-sub-wave level (≤90 min); P3-B binding at `p3b:81` ("sub-pass split-before-dispatch if any single sub-pass overflows the 90-min cap") prevents single-sub-wave LOC blow-out; V2 should add explicit 810-min restatement at SPEC §9 sub-wave table header.

8. **No CH4-binding clause is missing or stub-coded**; the V1 cycle is CH4-coherent; the single clarity REVISE for V2 is a one-line footnote addition, not a structural defect.
