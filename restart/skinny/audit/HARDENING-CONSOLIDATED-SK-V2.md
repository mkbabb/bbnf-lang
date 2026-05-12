# HARDENING-CONSOLIDATED-SK-V2 — Post-Iteration Synthesis

SK-V2 consolidation against the post-iteration skinny corpus + the `skinny/REDRESS.md` 19-item amendment landing + the `skinny/RESULTS.md` regenerated full-run measurements. Six parallel agents dispatched: five per-quadrant SK-V2 hardeners + one lazy-tape architectural design agent.

## §1 Cycle identification

| Field | Value |
|---|---|
| Cycle | SK-V2 (post-iteration; predecessor SK-V1 closed at SK-AMENDMENT-REQUIRED-NARROW with 20-item C1-C20 cross-quadrant punch list) |
| Trigger | User iteration cycle landed 19 redress items; two false routes invalidated empirically (dispatch-table-as-canonical; 12-byte skipless token); host-call probe split disposition (dispatch passes; eager-decode MASKING); bench rerun returns G/NO-GO at ~54-74% sonic-rs |
| Cohort size | 6 agents — 5 per-quadrant hardeners + 1 lazy-tape designer |
| Reports landed | `HARDENING-{SUBSTRATE,COMPILER,BENCH,WORKSPACE,INDEX}-SK-V2.md` + `LAZY-TAPE-DESIGN.md` |
| Iteration corpus | `skinny/REDRESS.md` (19 implemented items + 2 false-route invalidations), `skinny/RESULTS.md` (NO-GO outcome G across three corpora) |
| Lens stack | A-K (V1 HARDENING.md) + L (premise fidelity) + M (falsifiability) + N (graduation mechanicality) per skinny HARDENING.md §4 |

## §2 Cohort verdict matrix

| Target | Final decision | Lens L (premise) | Lens M (falsifiability) | Lens N (mechanicality) | SK-V2 punch-list size | SK-V1 punch-list disposition |
|---|---|---|---|---|---:|---|
| SUBSTRATE | SK-AMENDMENT-REQUIRED-NARROW | FAITHFUL structurally; one §0 premise framing now empirically falsified | honoured-with-narrow-amendment | MECHANICAL with new lazy-offset surface under-classified | ~10 items | 6 of 18 STILL-OPEN |
| COMPILER | SK-AMENDMENT-REQUIRED-NARROW | FAITHFUL+MASKING propagation gap (§2.2 row 155, §3.2 row 219 stale) | honoured | MECHANICAL with iteration constraint (V1 lazy-decode binding) | 15 items | 0 of 11 closed |
| BENCH | SK-AMENDMENT-REQUIRED-NARROW | FAITHFUL with row-text disposition needed | honoured (matrix returned NO-GO empirically) | honoured | (large; see report) | partial (iteration absorbed some) |
| WORKSPACE | SK-AMENDMENT-REQUIRED-NARROW | n/a | n/a | MECHANICAL, but SK-V1 punch list NOT APPLIED (16 items) | ~16 items | 0 of 16 closed |
| INDEX | SK-AMENDMENT-REQUIRED-NARROW | n/a (cross-quadrant ratifier) | n/a | MECHANICAL with deviation-ledger gap (lazy-tape not in ledger) | ~8 items | partial |
| **Cohort** | **SK-AMENDMENT-REQUIRED-NARROW** | — | — | — | **~60 narrow items** | — |

Plus: `LAZY-TAPE-DESIGN.md` — 845-line design proposal for the V1 Lock 1 amendment surface that the iteration evidence identified as the remaining honest route.

No quadrant returned SK-RE-DRAFT or SK-AMENDMENT-REQUIRED-BROAD. **Cohort verdict: SK-AMENDMENT-REQUIRED-NARROW**.

## §3 The dominant SK-V2 finding

The iteration cycle directed effort at the **runnable prototype** (`skinny/`) and produced 19 redress items in the implementation + the spec surfaces it most directly touches (COMPILER §1.3, BENCH §7.8.1/§7.8.2, SUBSTRATE §1.2 + §313 area, INDEX deviation ledger rows 6+7, ARCH §1433 area). It did **not** apply the SK-V1 audit punch list to the rest of the cohort.

Cross-quadrant disposition of the SK-V1 C1-C20 list:

| SK-V1 item | Status | Notes |
|---|---|---|
| C1 (LOC reconciliation BENCH↔WORKSPACE↔INDEX) | **REFUTED** | User reports `bbnf-bench` at 1993/2000 LOC per lint-loc. SK-V1 predicted Track 2 measurement-driven would push past cap; the implementation came in tight. WORKSPACE 2,000-cap holds. |
| C2 (F-band classification gap) | STILL-OPEN | No iteration evidence touched the matrix structure |
| C3 (F-noise rationale) | STILL-OPEN | Same |
| C4 (cold-cache primitives) | STILL-OPEN | Cold-cache probe is report-only; primitive correctness not yet fixed |
| C5 (stale H-outcome refs) | STILL-OPEN | Editorial |
| C6 (TapeBuilder cite) | partial | SUBSTRATE §8 contract present; BENCH cross-ref still missing |
| C7 (cross-platform plan divergence) | **SUPERSEDED** | dispatch_table_plan invalidated empirically by REDRESS item 17; PEXT remains tranche-H carry but is narrower |
| C8 (eager-decode band ambiguity) | TRANSFORMED | Disposition migrated to COMPILER §1.3 which now carries an internal predictive-vs-empirical contradiction |
| C9 (CI runner discount over-engineering) | STILL-OPEN | Not addressed |
| C10 (passes::layout/types path) | **CLOSED with residue** | §4.3+§4.5 use long path; §4.1 row still short |
| C11 (Probe A pseudo-precision) | STILL-OPEN | Editorial |
| C12 (peak RSS projection) | STILL-OPEN | The 3× threshold holds; projection calculation not added |
| C13 (parse signature drift) | partial | SUBSTRATE-side; not fully settled |
| C14 (Lock 14 surface count) | STILL-OPEN | INDEX-side |
| C15 (threshold preview notation) | STILL-OPEN | Pre-redress notation preserved |
| C16 (JSON recognizer Lock 14 fence) | STILL-OPEN | COMPILER §5.4/§5.5 unchanged |
| C17 (pipeline shim location) | STILL-OPEN | WORKSPACE-side |
| C18 (host_registry sentinel) | STILL-OPEN | WORKSPACE-side |
| C19 (single-plan extraction plurality) | **MUTATED** | Iteration invalidated the alternates the SK-V1 plurality was about; INDEX bullet 4 wording now refers to nonexistent dispatch-table candidate |
| C20 (COMPILER §2.2/§3.2 redress propagation) | **STILL-OPEN AND ELEVATED** | Stale text now empirically refuted (text says 2% median; measurements show 18-42% deltas). The propagation gap is now load-bearing. |

**Summary**: 1 REFUTED (good — measurement closes the speculative gap), 2 SUPERSEDED (good — alternate-route invalidation), 1 CLOSED-with-residue, 4 partial/transformed/mutated (audit findings still apply but the surface shifted), 12 STILL-OPEN.

## §4 New cross-quadrant SK-V2 items (D1-D8)

Items deduplicated across the 5 per-target reports:

### D1 — §1.3 internal predictive-vs-empirical contradiction (COMPILER + Lens L; load-bearing)
COMPILER §1.3 bullet 1 reads in two voices: predictive ("expected delta 5-15%") and empirical ("exceeds the expected bands"). Adjacent paragraphs with no explicit resolution. **Surgery**: rewrite bullet to lead with empirical finding; demote prior expectation to history. Cross-references COMPILER N1 + BENCH §7.8.1 + REDRESS item 19.

### D2 — Stale §2.2/§3.2 row text refuted empirically (COMPILER + Lens L; load-bearing; C20 elevated)
COMPILER §2.2 row 155 + §3.2 row 219 still cite "2% median" predicate; measurements show 18-42% deltas. **Surgery**: replace cells with empirical MASKING disposition + V1 lazy-decode constraint citation.

### D3 — Lazy-tape route not in INDEX deviation ledger (INDEX + Lens A; load-bearing)
INDEX §"Open contradictions" carries 7 deviation rows; the lazy-offset tape route — identified by REDRESS as the remaining honest substrate path — is not represented. INDEX is the cross-quadrant ratifier; the omission is structural. **Surgery**: add ledger row "Eager-tape substrate ceiling at ~1.6× sonic-rs; V1 closure via lazy-offset tape mode is a Lock 1 amendment surface (see LAZY-TAPE-DESIGN.md)".

### D4 — §0 premise framing now empirically falsified (SUBSTRATE + Lens L)
SUBSTRATE §0 reads: "If JSON cannot reach SOTA-parity through this substrate, that is strong negative evidence for JSON-class tape/SIMD throughput". The bench has returned NO-GO three times; the hypothetical is answered. **Surgery**: update §0 stance to record the empirical outcome and route forward to LAZY-TAPE-DESIGN.md.

### D5 — INDEX §"What the skinny is NOT testing" cost-model row refers to invalidated probe (INDEX + Lens A)
INDEX row "Cost-model + e-graph rewrites" describes "BENCH carries a small alternate-plan stub" — singular; iteration invalidated the dispatch-table candidate; INDEX bullet 4 now refers to nonexistent shape. **Surgery**: revise to "BENCH carries alternate-plan probes (scalar fallback empirically wins-by canonical; dispatch-table invalidated per REDRESS item 17; PEXT-on-x86_64 reserved for tranche H).

### D6 — WORKSPACE 16-item punch list zero-applied (WORKSPACE + Lane 6)
WORKSPACE quadrant returned SK-V2 with zero of 16 SK-V1 surgical edits applied. The iteration directed effort at the runnable prototype, which empirically refuted SK-V1 C1 (LOC reconciliation) — the 2,000 cap held. But the other 15 WORKSPACE items remain. **Surgery**: dedicated WORKSPACE amendment cycle.

### D7 — §5.3 + §7 row hedge survives despite measurement disposition (COMPILER + Lens F + Lens L)
§5.3 "egraph rewrite" and §7 "cost-model" rows carry "Potentially masking" hedge; RESULTS empirically disposes them (canonical wins vs scalar alternate by 38-52%). **Surgery**: harden to "Empirically FAITHFUL on M1 Pro per skinny/RESULTS.md".

### D8 — LAZY-TAPE-DESIGN.md introduces a Lock 1 amendment surface that requires V1-corpus dispatch (ARCH + Lock 1)
The 845-line design proposal at LAZY-TAPE-DESIGN.md proposes admitting `TapeMode: Eager | Lazy` as a per-grammar metadata flag, with the structural-index buffer serving as the tape in lazy mode. This requires:
- Lock 1 amendment surface (admit both eager-token and lazy-offset modes under "tape substrate")
- ARCH §7.2 TapeEmit BIR row payload reconsider (eager vs lazy modes)
- ARCH §9.1 Tape invariants rewritten
- SUBSTRATE.md §1 rewritten for the dual-mode contract
- COMPILER.md §3 BIR subset changes for lazy mode

**This is V1-corpus dispatch territory** — V9.x hardening cycle would absorb the amendment. Skinny SK-V2 surfaces the design; V1 hardening would ratify.

## §5 Lens disposition (cohort-level)

| Lens | SK-V1 verdict | SK-V2 verdict | Notes |
|---|---|---|---|
| A — Inter-document narrative coherence | honoured-with-narrow-amendment | AMENDMENT-REQUIRED-NARROW | D1, D3, D5 — INDEX is the cross-quadrant ratifier; the iteration's narrative landings stayed in the quadrant files |
| B — Vocabulary drift | honoured-with-narrow-amendment | honoured-with-narrow-amendment | Persists from SK-V1 |
| C — Worked-example scarcity | honoured | honoured | Iteration added concrete RESULTS rows; positive evidence |
| D — Coverage gaps | honoured | honoured | Iteration extended probe coverage |
| E — Architectural axiom cumulative consistency | honoured-with-narrow-amendment | AMENDMENT-REQUIRED-NARROW | D4, D8 — §0 premise stance + lazy-tape Lock 1 amendment unrepresented |
| F — LLM bias | honoured-with-narrow-amendment | AMENDMENT-REQUIRED-NARROW | D7 — "Potentially masking until..." hedge survives despite measurement |
| G — Overfitting | honoured | honoured | CSS prior probe optionality survives; no overfitting newly surfaced |
| H — Hallucination + provenance | AMENDMENT-REQUIRED-NARROW (cold-cache primitives) | STILL-OPEN | C4 not addressed by iteration |
| I — Contrivance / over-engineering | honoured-with-narrow-amendment | honoured | The iteration confirmed the prototype was tight |
| J — Host-language leverage | honoured | honoured (sharpened) | Iteration ratified host-leverage by measurement (function-pointer table regressed; LLVM-managed match wins) |
| K — Meta-grammar discipline | honoured-with-narrow-amendment | honoured-with-narrow-amendment | C16 + COMPILER punch item 7 still open |
| **L — Premise fidelity** | FAITHFUL with two MASKING signals | **MASKING with narrow propagation gap** | D2, D7 — measurement disposition not fully propagated to row text |
| **M — Falsifiability** | honoured-with-narrow-amendment | **honoured (ratified by measurement)** | The matrix returned NO-GO empirically — Lens M's load-bearing function (preventing confirmation-bias dispatch) is empirically met |
| **N — Graduation mechanicality** | MECHANICAL with named inversions | MECHANICAL with new lazy-tape surface | All 7 prior deviations close mechanically; lazy-tape design adds an 8th (Lock 1 amendment surface) |

The most important Lens disposition change vs SK-V1:

- **Lens M ratified by empirical outcome** — the matrix produced an honest NO-GO; the threshold matrix is not a confirmation-bias engine; the dispatch protocol works.
- **Lens L sharpened by measurement** — three rows moved from MASKING-pending to empirical FAITHFUL on M1 Pro (alternate_scalar_plan confirms canonical wins); two rows transformed into stale-text-refutes-measurement (§2.2 row 155 + §3.2 row 219); §1.3 bullet 1 carries the internal contradiction (D1).
- **Lens H still has C4 (cold-cache primitives) open** — the technical-correctness fault SK-V1 surfaced is unaddressed.

## §6 The architectural pivot

LAZY-TAPE-DESIGN.md proposes the architectural amendment the iteration evidence identified as the remaining honest route. Key design moves:

| Move | Current (eager-tape) | Lazy-offset proposal |
|---|---|---|
| Tape contents | `Vec<TapeToken>` (16 bytes/token) | `Box<[u32]>` of structural offsets (4 bytes/offset) |
| Token kind | Stored in `flags` byte | Derived from `source[offsets[cursor]]` byte |
| Sibling skip | Patched at parse | Walked at traversal via depth-counter |
| Parse-time write bandwidth | ~16 bytes/token × token count | ~4 bytes/offset × offset count |
| Lock 1 disposition | Current commitment | Amendment surface (admit `TapeMode: Eager \| Lazy`) |
| V1 grammars affected | All | Per-grammar opt-in via metadata; default eager |
| Predicted T1 twitter improvement | baseline 12.5K Mbps | ~14-16K Mbps (LAZY-TAPE-DESIGN.md §9 risk register) |
| Falsifiability | Same matrix; same gates | Same matrix; if T1 < 14K Mbps post-implementation, lazy-tape claim refuted |

The design preserves the eager-tape mode for grammars (CSS, BBNF-self, Sheets) where layout/recovery require stored payload classes. JSON SOTA-class grammars opt in to lazy mode.

This is a Lock 1 amendment surface, not a skinny-only change. The V1-corpus dispatch path:
1. SK-V2 amendment cycle absorbs the 60-item punch list at the spec text level.
2. Lock 1 amendment proposal dispatched against the V9.x V1-corpus hardening cycle.
3. If Lock 1 amendment ratifies, lazy-tape implementation lands in skinny (1-2 weeks).
4. Re-bench. If T1 > 14K Mbps, dispatch tranche B with lazy-mode opt-in default for JSON. If T1 < 14K Mbps, refute the architectural claim and route SOTA-BEAT to V1 H tranche body as ASPIRATIONAL.

## §7 Carry forward to SK-V3 amendment cycle

The 60+ narrow items distribute across cohorts:

| Cohort | Items | Mode | Wall budget |
|---|---|---|---|
| COMPILER text-propagation | C20 (elevated), N1-N6, SK-V1 carries 1-11 | Direct edits | ~90 min |
| WORKSPACE punch list zero-applied | 16 SK-V1 items | Dedicated WORKSPACE amendment cycle | ~60 min |
| INDEX cross-quadrant ratifier updates | D3, D5, C14, C15, C19 | Direct edits | ~30 min |
| SUBSTRATE §0 premise + lazy-tape surface introduction | D4, lazy-tape mention | Direct edits | ~30 min |
| BENCH-side residual | C2, C3, C4, C5, C9, C11, C12 | Direct edits | ~60 min |
| Lock 1 amendment dispatch | LAZY-TAPE-DESIGN proposal | V1-corpus dispatch (separate orchestrator phase) | ~3-5 hours |

Total skinny-spec amendment work: ~4-5 hours focused. Lock 1 amendment dispatch is separate V1 work.

## §8 What's empirically settled (the load-bearing wins)

| Settled | Evidence |
|---|---|
| Codegen is not the bottleneck | T1/T2 ratio 1.005-1.055 across all three corpora (RESULTS.md) |
| Structural scan is not the bottleneck | Canada scan 66565 Mbps vs 40000 Mbps floor (1.66× headroom) |
| dispatch-table-as-canonical is invalidated | REDRESS item 17 — duplicate probe; real function-pointer table regressed |
| 12-byte skipless token is invalidated | REDRESS item 18 — mixed parse results; canonical stays 16-byte |
| Host-call dispatch overhead is fine | Probe A 0.7 ns/call (<= 50 ns target) |
| Eager string decode is parse-time-expensive | Probe B 64-82% T1 across corpora — MASKING signal |
| Substrate materialisation is the bottleneck | T2 ≈ T1 ≈ 60% of sonic-rs; the remaining gap is per-token write cost on the eager-tape architecture |
| LLVM owns branch-table lowering | REDRESS item 17 — function-pointer table regressed; canonical Rust `match` wins |
| Lazy-decode is a V1 closure constraint, not optional | Probe B MASKING + Lock 9 already commits the `Cow` model |

These are not iteration losses. They are iteration *wins* — the prototype empirically refuted speculative claims and produced a sharper architectural conclusion: the SOTA-beat path requires lazy-offset tape, not eager-tape micro-optimization.

## §9 Decision points for the user

Three forks:

| Path | What it commits | Cost | Earns |
|---|---|---|---|
| **(a) Land the 60-item SK-V2 text-propagation amendment only** | Closes the spec-text propagation gap; preserves outcome G honestly | ~4-5 hours focused | Audit-trail-clean SK-V2-CLOSED state; clear documentation that the eager-tape ceiling is empirically pinned |
| **(b) Land SK-V2 amendments AND dispatch the lazy-tape Lock 1 amendment proposal** | Spec-text closure + V1-corpus Lock 1 amendment dispatch + lazy-tape implementation | ~4-5 hours + V1 hardening cycle (V9.x) + 1-2 weeks implementation | High probability of outcome C; moderate probability of outcome B; potential closure of the SOTA-beat gap structurally |
| **(c) Accept eager-tape ceiling; route SOTA-BEAT to V1 H tranche body as ASPIRATIONAL** | Outcome G ratified; V1 dispatches tranche A-J with SOTA-PARITY (not BEAT) commitment for JSON | ~4-5 hours (SK-V2 amendments only) | V1 dispatches faster but commits to a SOTA outcome below sonic-rs |

The SK-V1 cohort recommended path (b) speculatively; the SK-V2 cohort + iteration evidence has empirically validated that path (b) is the architecturally correct route — the eager-tape ceiling is now measurement-confirmed. Path (c) is the honest fallback if the user prefers shipping speed over SOTA-beat.

## §10 Final readiness

> **Decision: SK-AMENDMENT-REQUIRED-NARROW** at the skinny-corpus level.
>
> The iteration cycle directed effort at the runnable prototype and produced 19 redress items that empirically settled the architectural diagnosis: the eager-tape substrate has a structural ceiling at ~1.6× sonic-rs time. Two false routes (dispatch-table-as-canonical; 12-byte token) are settled empirically. The host-call probe split produced a clean disposition (dispatch passes; eager-decode MASKING). Codegen is empirically separable from substrate (T1≈T2 across all three corpora). The matrix returned NO-GO outcome G — Lens M's load-bearing falsifiability function is empirically met.
>
> The skinny corpus survives this audit at the architectural level. The remaining work is:
> 1. **Text-propagation**: ~60 narrow items distributing across COMPILER (15), WORKSPACE (16), INDEX (8), SUBSTRATE (10), BENCH (residual). All REINVENT, none DISCARD, none architectural.
> 2. **Lock 1 amendment dispatch**: LAZY-TAPE-DESIGN.md proposes the architectural amendment the iteration evidence identified as the remaining honest route. This is a V1-corpus hardening cycle, not skinny-only.
>
> No quadrant returned SK-RE-DRAFT or SK-AMENDMENT-REQUIRED-BROAD. The skinny is buildable, measurable, and falsifiable — and the empirical falsifier has fired honestly. The corpus is one mechanical text-propagation cycle from SK-V2-CLOSED.
>
> Hereupon: user adjudicates between paths (a), (b), (c) per §9. The hardening cohort + lazy-tape designer have produced the evidence base; the architectural decision is now political (which V1 SOTA commitment ships) not technical.

---

**SK-V2 cohort totals**: 5 quadrant audit reports (1,884 lines) + 1 lazy-tape design proposal (845 lines) = 2,729 lines independent audit + design content. 60+ cross-quadrant narrow items deduplicated; one architectural amendment proposal dispatched for V1-corpus consumption.
