# HARDENING-CONSOLIDATED-SK-V1 — Post-Redress

SK-V1 consolidation for the post-redress skinny corpus. Synthesises the five per-target reports against the redressed `restart/skinny/{INDEX,SUBSTRATE,COMPILER,BENCH,WORKSPACE}.md`.

A pre-redress consolidation exists at `HARDENING-CONSOLIDATED-SK-V1-pre-redress.md` for archaeology — its line citations refer to the pre-redress state and should not be used as authority for the current corpus.

## §1 Cycle identification

| Field | Value |
|---|---|
| Cycle | SK-V1 (post-redress dispatch) |
| Trigger | 5 parallel hardener agents per `restart/skinny/HARDENING.md` §2 SUITE invocation, dispatched after the 15-item redress landed |
| Cohort size | 5 quadrants (SUBSTRATE, COMPILER, BENCH, WORKSPACE, INDEX) |
| Per-target reports | `HARDENING-{TARGET}-SK-V1.md` in this directory |
| Lens stack applied | A-K (V1 `restart/prompts/HARDENING.md`) + L (premise fidelity) + M (falsifiability) + N (graduation mechanicality) per skinny `HARDENING.md` §4 |
| Skinny corpus state at audit | post-redress (after the 15-item amendment landing closed the prior author-side challenge) |
| Total report LOC | 1,664 lines across 5 target reports |

## §2 Cohort verdict matrix

| Target | Final decision | Lens L (premise) | Lens M (falsifiability) | Lens N (mechanicality) | Punch-list size |
|---|---|---|---|---|---:|
| SUBSTRATE | SK-AMENDMENT-REQUIRED-NARROW | FAITHFUL with one MASKING residue | honoured | MECHANICAL with named inversion | ~9 surgical items |
| COMPILER | SK-AMENDMENT-REQUIRED-NARROW | FAITHFUL with band-rationale clarification | honoured-with-narrow-amendment | MECHANICAL with named inversion | ~12 surgical items |
| BENCH | SK-AMENDMENT-REQUIRED-NARROW | FAITHFUL with x86_64 plan-divergence MASKING | honoured-with-narrow-amendment | honoured | 15 surgical items |
| WORKSPACE | SK-AMENDMENT-REQUIRED-NARROW | n/a (workspace is logistics) | n/a | MECHANICAL | ~10 surgical items |
| INDEX | SK-AMENDMENT-REQUIRED-NARROW | n/a (cross-quadrant ratifier) | n/a | MECHANICAL with named inversion | ~7 surgical items |
| **Cohort** | **SK-AMENDMENT-REQUIRED-NARROW** | — | — | — | **~53 items, all narrow** |

No quadrant returned SK-RE-DRAFT or SK-AMENDMENT-REQUIRED-BROAD. The cohort verdict is **SK-AMENDMENT-REQUIRED-NARROW**: the 15-item redress closed the load-bearing defects, the new state is internally cohesive enough to amend rather than rewrite, but the SK-V1 cohort surfaced ~53 narrow propagation faults the redress missed.

## §3 Cross-quadrant punch list (deduplicated)

The five per-target reports collapsed into one cross-quadrant punch list. Items that surfaced in multiple quadrants are merged. Items numbered C1-C20 below; per-target reports carry their own internal numbering.

### C1 — Track 2 LOC reconciliation across BENCH ↔ WORKSPACE ↔ INDEX (cross-quadrant)
**Surfaced by**: BENCH §3.6 (item 11), WORKSPACE Lane 6, INDEX Lens A.
**Issue**: BENCH §11.1 dropped the Track 2 ≤ 500 LOC cap (now measurement-driven, expected 800-1,500 LOC) and added optional CSS prior probe (≤ 600 LOC); WORKSPACE.md row 9 still caps `bbnf-bench` at 2,000 LOC; INDEX.md headline cites 31,400 LOC handwritten.
**Surgery**: WORKSPACE row 9 → 3,000-3,500 LOC; INDEX headline → ~32,500-33,000 LOC; recompute the §10 omission table accordingly.

### C2 — F-band classification gap (Lens M; load-bearing)
**Surfaced by**: BENCH §3.M.
**Issue**: §6.2.1 classification order has no outcome matching `Track 2 ∈ (S × 1.05, S × 1.10] AND Track 1 > Track 2 × 1.10` (substrate borderline-weak + codegen gap is unclassifiable).
**Surgery**: add F-codegen-gap row OR collapse F-positive/F-noise/F-codegen-gap into single F outcome with Track 1 sub-band reported in action text.

### C3 — F-noise rationale hand-waved (Lens M; load-bearing)
**Surfaced by**: BENCH §3.M.
**Issue**: "criterion `noise_threshold(0.02)` plus 5% headroom" conflates iteration-to-iteration drift with track-to-track ratio — does not derive the (1.05, 1.10] band.
**Surgery**: replace with measurement-driven boundary "Track 1 95% CI upper bound overlaps Track 2 × 1.05".

### C4 — Cold-cache eviction primitives wrong (Lens H; AMENDMENT-REQUIRED-NARROW)
**Surfaced by**: BENCH §3.H.
**Issue**: §7.8.3 names `core::arch::aarch64::__dsb` (which is a barrier, not eviction) and `_mm_clflush` without 64-byte stride iteration; TLB and branch-predictor cooling absent.
**Surgery**: replace with correct aarch64 (`dc civac` loop or `clear_cache` syscall) and x86_64 (stride loop over hot-data ranges); add explicit "TLB and branch-predictor not cooled — dCache + iCache delta only" qualifier.

### C5 — Stale H-outcome references in BENCH (Lane 3; editorial)
**Surfaced by**: BENCH §4 cross-quadrant invariants.
**Issue**: §6.3 line 675 ("Outcomes G and H exist...") and §10.2 line 1347 outcome-ID enumeration `<A|B|C|D|E|F|G|H|I|J|K|L>` are pre-redress; H was collapsed into G; M was added; F split into F-positive/F-noise.
**Surgery**: replace with current outcome set `<A|B|C|D|E|F-positive|F-noise|G|I|J|K|L|M>`; rephrase §6.3 line 675 to reference current NO-GO outcomes (G/I/J/K/L/M).

### C6 — TapeBuilder API not cited from BENCH §1.2 (Lens N)
**Surfaced by**: BENCH §3.L (item 10), SUBSTRATE.
**Issue**: SUBSTRATE.md §8 commits `TapeBuilder<'a>` as the API the BENCH agent uses for Track 2; INDEX.md ledger row 6 classifies the SK-V1-vs-V1 deviation as MECHANICAL with named inversion — but the inversion *is* `TapeBuilder`. BENCH.md §1.2 references `runtime::tape` generically and does not cite §8 SUBSTRATE.
**Surgery**: BENCH §1.2 cross-reference SUBSTRATE §8 + INDEX deviation ledger row 6.

### C7 — Cross-platform plan divergence not in probability mapping (Lens L; load-bearing)
**Surfaced by**: BENCH §3.L (item 9).
**Issue**: §7.8.2 alternate_pext_mask_plan probe describes "Inverted dominance" branch (alternate < canonical × 0.90 on x86_64) but §10.3 probability mapping doesn't represent this. A skinny on M1 Pro could land outcome A while the V1 SOTA close gates on x86_64 (per Lock 8) miss because canonical is pessimal there.
**Surgery**: add §10.3 row "alternate_pext_mask_plan < canonical × 0.90 on x86_64 → MASKING: cross-platform plan divergence; V1-SOTA-beat probability on Intel line drops by 0.10-0.20".

### C8 — Eager-decode band rationale ambiguous (Lens L)
**Surfaced by**: BENCH §3.L (item 12).
**Issue**: §7.8.1 Probe B per-corpus bands (5-15% twitter, 3-8% citm, < 2% canada) measure "the eager-decode work + dispatch overhead summed", but the spec doesn't decompose. Steelman ambiguity: is the band the dispatch overhead atop eager work, or the total cost?
**Surgery**: clarify as "the additional cost of registry-routed eager decode atop the eager-decode work itself".

### C9 — CI runner discount over-engineering (Lens I)
**Surfaced by**: BENCH §3.I.
**Issue**: §8.3-§8.4 introduces threshold scaling per CI runner with `runners.toml` and gate logic; the user always re-runs locally on NO-GO so the discount provides only early-warning. Adds ~50 LOC of gate logic + a per-runner table; load-bearing role unclear.
**Surgery**: collapse to "CI bench is advisory non-gating; local bench is authoritative"; remove `runners.toml`; reclaim ~50 LOC from gate.rs budget.

### C10 — passes::layout/types path drift (cross-quadrant: COMPILER vs WORKSPACE)
**Surfaced by**: COMPILER (Lane 1, Lane 3), WORKSPACE Lane 1.
**Issue**: COMPILER.md §4.5 lists files at `passes/src/types/...` (algorithm_w.rs, unify.rs, scheme.rs, facts.rs, diagnostic.rs); WORKSPACE.md §4.4 places them under `passes/src/layout/` with no `types/` subdir. The HM-hierarchy-inversion deviation places HM at top-level in skinny but the file path inconsistency is a Lock 13 + Lock 1-cohesion residue.
**Surgery**: settle on `passes/src/layout/types/` (matches WORKSPACE; allows V1 `passes::layout` wrapper without rename); update COMPILER §4.5; update WORKSPACE §4.4 to enumerate the subdir contents.

### C11 — Probe A pseudo-precision (Lens F)
**Surfaced by**: BENCH §3.F (item 6).
**Issue**: §7.8.1 Probe A threshold "≤ 50 ns/call canonically ~10-30 ns" softens commitment with hand-waved precision.
**Surgery**: replace with "M1 Pro virtual call ~5-10 ns; bounds-check + registry lookup adds ~20-40 ns; total per registry call ~30-50 ns" (or cite a microbench source).

### C12 — Peak RSS forward-projection missing (Lens M; item 8)
**Surfaced by**: BENCH §3.M.
**Issue**: §9.6 outcome M (peak RSS > 3× competitor on canada) NO-GO branch is asserted as "safety net" without forward-projection. The 3× threshold likely never fires (substrate target ~5-7 MB ≈ 1× sonic-rs canada peak), making the gate ceremonial.
**Surgery**: add §9.6 forward-projection calculation "tape (8-byte tokens × ~280K offsets) ≈ 2.24 MB; payload arena empty; typed root ≈ 3-5 MB; total ~5-7 MB ≈ 1× sonic-rs; M outcome's 3× threshold is a safety net, not a primary gate".

### C13 — Parse signature drift (cross-quadrant: SUBSTRATE vs COMPILER vs ARCH)
**Surfaced by**: SUBSTRATE Lane 1.
**Issue**: SUBSTRATE.md §1.3 line 117 says `parse(&'a [u8])`, §4.3 says `parse(&'a str) -> JsonDocument<'a>`, COMPILER.md says `pub fn parse<'i>(input: &'i str)`, ARCH §3.1 says `parse<'a>(&self, input: &'a [u8])`. Four inconsistent signatures.
**Surgery**: settle the public skinny entry as `pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError>`; update SUBSTRATE §1.3 + §4.3; cite ARCH §3.1 for the V1 `&[u8]` carrier as a deferral.

### C14 — Lock 14 surface count silent in INDEX (Lane 1)
**Surfaced by**: INDEX Lane 1.
**Issue**: INDEX implicitly relies on Lock 14's three declarative surfaces (grammar source + workspace metadata + optional declaration crate) but never names the Lock 14 onboarding test or the §5.6 declaration-crate fence; the deviation ledger row for `wasm = false` only restates the metadata flag.
**Surgery**: add one-line note in INDEX §"Cross-quadrant invariants" — "Onboarding contract: two surfaces (`json.bbnf` + workspace metadata); §5.6 fence empty for the skinny per Lock 14".

### C15 — Threshold preview at INDEX uses pre-redress notation (Lens A)
**Surfaced by**: INDEX Lens A, BENCH §4.
**Issue**: INDEX §"What the skinny is testing" 3-row outcome table uses `S × 0.95` (pre-redress); BENCH §6.1 outcome A binds on `BEAT_BOUND = min(S × 0.95, T_README)`. The preview understates outcome A.
**Surgery**: INDEX preview table → `Track 2 ≤ BEAT_BOUND` for outcome A; cross-reference BENCH §6.1 BEAT_BOUND definition.

### C16 — JSON hand-curated recognizer Lock 14 fence (Lane 1)
**Surfaced by**: COMPILER Lane 1, WORKSPACE Lane 5.
**Issue**: COMPILER.md §5.4 places JSON-specific recognizer code in `passes/src/recognizers/json_handcurated.rs` — grammar-specific code in a generic crate. Skinny needs this to run cheaply, but Lock 14 forbids it.
**Surgery**: rename to `passes/src/recognizers/skinny_json_curate.rs` with explicit "skinny-only; deletes at V1 graduation when telemetry-driven miner lands at H.W2" header comment; add to INDEX deviation ledger as MECHANICAL (deletion, not movement).

### C17 — Pipeline shim location drift (Lane 3)
**Surfaced by**: WORKSPACE Lane 3.
**Issue**: §1.1 says `pipeline` shim is inlined into `xtask::regen` + `bbnf::compile` (200 LOC); §7 stub policy says it lives at `crates/bbnf/src/parse/pipeline.rs`; the two locations disagree.
**Surgery**: settle on `xtask/src/regen.rs` (~120 LOC) + `bbnf/src/compile.rs` (~80 LOC) — both already named in §1.1; remove the §7 row pointing to `bbnf/src/parse/pipeline.rs`.

### C18 — host_registry sentinel non-canonical (Lane 1)
**Surfaced by**: WORKSPACE Lane 1.
**Issue**: §3 sketch sets `[workspace.metadata.bbnf.host_fns] default_registry = "host::primitives"` even though §1.1 says `host` is inlined as 50 LOC stub `bbnf::host_stubs`. The metadata key names a non-existent crate path.
**Surgery**: settle metadata sentinel as `default_registry = "skinny-stub"` with V1 closure noted in §10 omissions; OR keep `host::primitives` but document the validator accepts a non-existent path during skinny.

### C19 — Single-plan extraction wording drift (Lens A)
**Surfaced by**: INDEX Lens A.
**Issue**: INDEX §"Cross-quadrant invariants" bullet 4 says "BENCH carries a small alternate-plan stub" (singular); BENCH §7.8.2 describes three alternates. The plurality differs.
**Surgery**: INDEX bullet 4 → "BENCH carries small alternate-plan stubs (scalar, dispatch-table, and on x86_64 a plausibly-better PEXT mask) to bound whether this cut masks JSON throughput cost".

### C20 — Compiler §1.3 alignment with BENCH probe shape (cross-quadrant)
**Surfaced by**: COMPILER (Lane 3).
**Issue**: COMPILER §1.3 bullet 1 was rewritten in the redress to reference TWO probes (`host_call_dispatch_overhead`, `host_call_eager_decode`); bullet 1 reads cleanly but COMPILER §2.2 row 147 still says "Potentially masking until BENCH's one-host-fn JSON variant proves the `CallHost` registry path stays within 2% median..." — pre-redress phrasing referencing the old single-probe 2% threshold. Cohesion fault.
**Surgery**: COMPILER §2.2 + §3.2 → match the §1.3 two-probe shape; remove the 2% reference.

## §4 LOC budget reconciliation

The redress dropped the Track 2 LOC cap (BENCH.md §1.2) without propagating to WORKSPACE.md or INDEX.md:

| Quadrant | Pre-redress | Post-redress | Reconciliation |
|---|---:|---:|---|
| BENCH §11.1 (`bbnf-bench` internals) | ~1,510 LOC | ~2,200 LOC + Track 2 measurement-driven (800-1,500) + CSS prior optional (600) = up to ~4,300 | C1 |
| WORKSPACE row 9 (`bbnf-bench` skinny budget) | 2,000 LOC | unchanged (stale) | C1 |
| INDEX headline | 31,400 LOC | unchanged (stale) | C1 |

Reconciliation target: WORKSPACE row 9 → 3,000-3,500 LOC; INDEX headline → ~32,500-33,000 LOC. The total skinny budget remains comfortably within Lock 13's per-file ≤ 500 LOC cap (the Track 2 file split is allowed under `track2/json/`); the budget growth is structural, not a discipline failure.

## §5 Punch list disposition by source

| # | Cross-quadrant item | SUBSTRATE | COMPILER | BENCH | WORKSPACE | INDEX |
|---:|---|:-:|:-:|:-:|:-:|:-:|
| C1 | LOC reconciliation | | | ✓ | ✓ | ✓ |
| C2 | F-band gap | | | ✓ | | |
| C3 | F-noise rationale | | | ✓ | | |
| C4 | Cold-cache primitives | | | ✓ | | |
| C5 | Stale H-outcome refs | | | ✓ | | |
| C6 | TapeBuilder cite | ✓ | | ✓ | | ✓ |
| C7 | Cross-platform plan divergence | | | ✓ | | |
| C8 | Eager-decode band ambiguity | | ✓ | ✓ | | |
| C9 | CI runner discount over-engineering | | | ✓ | | |
| C10 | passes::layout/types path drift | | ✓ | | ✓ | |
| C11 | Probe A pseudo-precision | | | ✓ | | |
| C12 | Peak RSS projection | | | ✓ | | |
| C13 | Parse signature drift | ✓ | ✓ | | | |
| C14 | Lock 14 surface count | | | | | ✓ |
| C15 | Threshold preview notation | | | | | ✓ |
| C16 | JSON recognizer Lock 14 fence | | ✓ | | ✓ | |
| C17 | Pipeline shim location | | | | ✓ | |
| C18 | host_registry sentinel | | | | ✓ | |
| C19 | Single-plan extraction plurality | | | | | ✓ |
| C20 | Compiler §2.2/§3.2 redress propagation | | ✓ | | | |
| **Total** | | **2** | **5** | **11** | **5** | **5** |

The deduplication: 53 per-target items → 20 cross-quadrant items. ~35% of the per-target items collapsed into a smaller cross-quadrant set, indicating the per-quadrant audits found meaningful overlapping issues.

## §6 Lens disposition (cohort-level)

| Lens | Cohort verdict | Drivers |
|---|---|---|
| A — Inter-document narrative coherence | honoured-with-narrow-amendment | C1, C5, C15, C19 |
| B — Vocabulary drift | honoured-with-narrow-amendment | C1, C13, C20 |
| C — Worked-example scarcity | honoured | — |
| D — Coverage gaps | honoured | — |
| E — Architectural axiom cumulative consistency | honoured-with-narrow-amendment | C10, C16, C18 |
| F — LLM bias | honoured-with-narrow-amendment | C11 |
| G — Overfitting | honoured | (CSS prior probe is the load-bearing anti-overfit lever; KEEP) |
| H — Hallucination + provenance | AMENDMENT-REQUIRED-NARROW | C4 (technical-correctness fault on cold-cache primitives) |
| I — Contrivance / over-engineering | honoured-with-narrow-amendment | C9 |
| J — Host-language leverage | honoured | — |
| K — Meta-grammar discipline | honoured-with-narrow-amendment | C16, C18 |
| **L — Premise fidelity** | FAITHFUL with two MASKING signals | C7 (cross-platform plan divergence), C8 (band ambiguity) |
| **M — Falsifiability** | honoured-with-narrow-amendment | C2 (classification gap), C3 (band rationale), C12 (projection) |
| **N — Graduation mechanicality** | MECHANICAL with named inversions | C6, C13, INDEX deviation ledger rows for Box<[T]> + HM hierarchy survive steelman |

The two new MASKING signals at Lens L are both narrow:
- **C7** (cross-platform plan divergence on x86_64): the alternate_pext_mask_plan probe surfaces the issue but the probability mapping doesn't represent it. Surgery: §10.3 row addition.
- **C8** (eager-decode band rationale): the bands are defensible but the rationale is ambiguous between dispatch overhead and total cost. Surgery: editorial clarification.

Neither MASKING is structural; both close mechanically.

## §7 What survived the redress (the load-bearing wins)

The 15-item author-side redress closed the prior challenge's load-bearing defects. SK-V1 ratifies these post-redress decisions:

1. **BEAT_BOUND construction** (outcome A binds on `min(S × 0.95, T_README)`). Closes the prior outcome-A ratification hole. Ratified by Lens M.
2. **Outcome G + H collapse**. Eliminates a redundant matrix row; the codegen ratio on a failing substrate is determined by the substrate, not by codegen design. Ratified by Lens M and Lane 4.
3. **Outcome F split into F-positive / F-noise**. The split is a real distinction (positive codegen finding vs noise within bench measurement) but the F-noise rationale needs sharpening (C3).
4. **Outcome M (peak RSS > 3× competitor on canada)**. Upgrades memory residency from report-only to gated NO-GO. The 3× threshold is generous (C12), but the gate exists.
5. **Per-corpus SIMD parity hash**. Closes the prior "twitter-only parity check" gap. Ratified by Lens H.
6. **Canada-binding SIMD throughput floor**. The kernel is stressed hardest on the largest input. Ratified by Lens H and Lane 4.
7. **schema_version field + enforcement gate**. Closes the prior "silent schema drift" gap. Ratified by Lens M.
8. **Two host-call probes (dispatch_overhead + eager_decode)**. Tests two distinct masking modes with defensible per-corpus bands. Lens L FAITHFUL with band-rationale clarification (C8).
9. **Alternate-plan probes reframed as confirmatory + one plausibly-better candidate**. The PEXT-mask plan tests V1 cost-model output on x86_64. Lens L FAITHFUL with cross-platform MASKING propagation needed (C7).
10. **CSS prior probe (substrate-only walker)**. Strongest anti-overfit lever in the post-redress spec. Ratified by Lens G.
11. **Cold_first_parse probe (report-only)**. Eviction primitives are wrong (C4), but the probe shape is correct.
12. **PGO disclosure**. Honest about what the bench does and does not test. Ratified by Lens H.
13. **Track 2 LOC ceiling dropped, substrate-API correspondence checklist added**. The checklist gates on what Track 2 calls, not how short it is. Ratified by Lens L.
14. **Box<[TapeToken]> deviation in the ledger**. The MECHANICAL with named inversion classification survives steelman. Ratified by Lens N.
15. **HM hierarchy inversion in the ledger**. The MECHANICAL with named inversion classification survives steelman. Ratified by Lens N.

## §8 Carry forward to SK-V2 amendment cycle

The SK-V2 cycle owns the 20-item cross-quadrant punch list above. Recommended dispatch:

| Cohort | Items | Mode | Wall budget |
|---|---|---|---|
| BENCH-side editorial | C5, C8, C11, C12, C15 | Direct edits to BENCH §6.3, §10.2, §7.8.1, §9.6 | ~30 min |
| BENCH-side mechanical | C2, C3, C4, C7 | F-band gap close + measurement-driven F-noise + cold-cache primitive correction + probability mapping row | ~60 min |
| Cross-quadrant (BENCH ↔ WORKSPACE ↔ INDEX) | C1 | LOC reconciliation across three quadrants | ~30 min |
| Cross-quadrant (SUBSTRATE ↔ COMPILER ↔ ARCH) | C13 | Parse signature settlement | ~20 min |
| COMPILER-side cohesion | C10, C16, C20 | passes::layout/types path; recognizer fence; §2.2/§3.2 redress propagation | ~30 min |
| WORKSPACE + INDEX | C14, C17, C18, C19 | Lock 14 surface; pipeline shim location; host_registry sentinel; single-plan plurality | ~30 min |
| Apparatus reduction | C9 | CI runner discount collapse | ~15 min |
| BENCH-side cross-ref | C6 (TapeBuilder cite) | One-line addition | ~5 min |

Total amendment wall: ~3.5-4 hours of focused author-side or amendment-agent work. After SK-V2 amendments land, dispatch SK-V2 verification cohort against the post-amendment corpus.

## §9 Lens disposition for V1 SOTA-beat probability

The skinny's purpose is to update the V1 SOTA-beat probability with measurement evidence. SK-V1 audit verdict: **the post-redress skinny will produce a defensible probability update once SK-V2 amendments land**. The pre-amendment matrix has two falsifiability holes (C2, C3) that compromise the verdict's classification reliability; one Lens-L MASKING signal (C7) that biases the update toward V1 SOTA-beat optimism on the x86_64 line.

Without SK-V2 amendments, the skinny dispatch would update probabilities incorrectly. With SK-V2 amendments, the skinny is a genuinely defensible prior-validation device.

## §10 Cross-target ratification of the redressed deviation ledger

The 7-row deviation ledger at INDEX.md §"Open contradictions" — extended from 5 rows pre-redress to 7 rows post-redress (Box<[T]> sealing, HM hierarchy inversion) — survives Lens N steelman across all five quadrants. Specifically:

| Deviation | INDEX row | Verifier (per-target) | Verdict |
|---|---|---|---|
| HM hierarchy inversion | row 1 | COMPILER §3.HM-hierarchy + SUBSTRATE §3.skinny-deviation | MECHANICAL with named inversion |
| JSON host-fn-free | row 2 | COMPILER §3.host-fn-free + BENCH §3.L | MECHANICAL with two-probe masking bound |
| parse-that-regex directory layout | row 3 | WORKSPACE §3.parse-that-regex + Lock 13 | MECHANICAL (one-time directory promotion) |
| `passes` HM-only constraint | row 4 | COMPILER §4.HM-only + WORKSPACE §2.1 | MECHANICAL (additive at V1) |
| `wasm = false` metadata | row 5 | WORKSPACE §3.wasm-false + Lock 5 | MECHANICAL (V2 flag flip) |
| `Tape<'input>` Box<[T]> sealing | row 6 (new) | SUBSTRATE §1.2 + INDEX §10 | MECHANICAL with named inversion (TapeBuilder snapshot pattern) |
| HM-as-top-level inversion of Lock 2 producer direction | row 7 (new) | COMPILER §4.4 + §9.1 + INDEX §10 | MECHANICAL with named inversion (Tranche D wrapper) |

All seven deviations close MECHANICALLY at V1. No deviation requires re-architecture. Lens N ratifies the cohort.

## §11 Cohort total counts

- Total per-target reports: 5 (SUBSTRATE 361 lines; COMPILER 448 lines; BENCH 239 lines; WORKSPACE 369 lines; INDEX 247 lines)
- Total cross-quadrant punch-list items (deduplicated): 20 (C1-C20)
- Lens-load-bearing defects (Lens L MASKING + Lens M falsifiability + Lens H provenance): C2, C3, C4, C7, C8, C12 = 6 items
- Editorial / cohesion items: C5, C11, C15, C19, C20 = 5 items
- Cross-quadrant reconciliation items: C1, C6, C13, C17 = 4 items
- Apparatus / over-engineering items: C9, C16, C18 = 3 items
- Workspace metadata items: C10, C14 = 2 items

## §12 Failure-mode analysis

Two per-target reports flagged Lens H as AMENDMENT-REQUIRED-NARROW (BENCH C4) — the cold-cache primitive correctness. This is a technical-correctness fault, not a discipline fault: the spec author named the wrong intrinsic. The fix is one paragraph rewrite, but Lens H's "non-existent papers / wrong-line citations / unverified externals" definition matches the fault exactly. The other 4 per-target reports returned Lens H honoured.

No quadrant returned Lens M as anything stronger than honoured-with-narrow-amendment. The matrix has multiple honest NO-GO branches (G, I, J, K, L, M) the skinny could plausibly land in. Lens M's load-bearing function (preventing confirmation-bias dispatch) is met. The two falsifiability holes (C2 classification gap, C3 F-noise rationale) are sharpenings, not architectural faults.

No quadrant returned Lens N as ANTI-MECHANICAL. All 7 deviation-ledger rows close mechanically. The graduation path remains additive; no skinny code requires rewrite at V1.

## §13 Final readiness

> **Decision: SK-AMENDMENT-REQUIRED-NARROW**
>
> The post-redress skinny corpus is internally cohesive and surface-level coherent enough to amend, not rewrite. The 15-item author-side redress closed the prior load-bearing challenges (BEAT_BOUND construction, F-band split, G+H collapse, M outcome, per-corpus SIMD parity, schema_version, host-call probe split, alternate-plan reframing, CSS prior probe, cold_first_parse probe, PGO disclosure, Track 2 LOC ceiling drop, Box<[T]> + HM hierarchy deviation ledger rows). SK-V1 cohort surfaces ~53 narrow propagation faults that deduplicate to 20 cross-quadrant punch-list items (C1-C20).
>
> The cohort verdict is **SK-AMENDMENT-REQUIRED-NARROW**. No quadrant returned SK-RE-DRAFT or SK-AMENDMENT-REQUIRED-BROAD. The defects are narrow and mechanically closable through editorial / threshold / cross-reference / cohesion edits; none requires architectural rework.
>
> Two MASKING signals at Lens L (C7 cross-platform plan divergence, C8 eager-decode band ambiguity) require attention before the skinny can produce a defensible V1 SOTA-beat probability update. Two falsifiability gaps at Lens M (C2 classification-order hole, C3 F-noise rationale) require attention before the matrix can return reliable verdicts. One Lens H technical-correctness fault (C4 cold-cache primitives) requires a one-paragraph rewrite. All other items are mechanical closure, cross-quadrant reconciliation, or editorial cohesion.
>
> Hereupon: dispatch SK-V2 amendment cycle against the 20-item C1-C20 punch list per the §8 dispatch table. After SK-V2 amendments land, dispatch SK-V2 verification cohort to confirm closure. After SK-V2 returns SK-READY, the skinny implementation phase begins per `INDEX.md §"Decision protocol"` step 1.
>
> The skinny's prior-validation function survives this audit. The corpus is buildable, measurable, and falsifiable. SK-V1's job was to verify those three claims survive an independent audit before any LOC is written; the verdict is that all three survive with narrow amendment, and the skinny is a defensible prior-validation device once C1-C20 land.
