# SK-V9 S-P1 V3 — CH4 COST Lens

Pass: S-P1 Profile. Cycle: V3.
Date: 2026-05-18.
Lens: CH4 COST (LOC budget, risk class, wave alignment, hard cap, same-wave
consumer, revert protocol — per `restart/prompts/ORCHESTRATOR.md` §3W and §8
"No contrivance — smallest change that achieves elegance + performance").
Scope: the six committed P1-V3 artefacts at
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`, commit
`c6fb0342`.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

---

## §1 Method — cost-audit protocol

CH4 reads every proposed intervention or downstream consequence surfaced by
the six V3 artefacts and checks each against the six §8 non-negotiables that
bear on cost:

1. **LOC budget.** Is a concrete line-count budget declared for the
   intervention, *and* is the budget plausible given the named owner files
   and the SK-V7/SK-V8 LOC-vs-outcome precedents (e.g. SK-V7 W3 capacity-hint
   `~Vec::with_capacity` retrofit landed in REDRESS 81 at small-LOC scale and
   admitted; SK-V7 W10 / REDRESS 88 PMULL prefix-XOR was larger and rejected)?
2. **Risk class.** Is the intervention named at one of {low: docs-only / role
   reframe, medium: kernel port behind a checkasm gate, high: substrate or
   generated-runtime hot-path edit} — and is the named class correct given
   the owner paths it touches?
3. **Wave alignment.** Is the wave the intervention targets named (V9 W1 /
   W2 / V10) AND is the wave's entry gate the intervention can satisfy named?
   "Future-wave" without a gate is paper-deferral per §8.
4. **Hard cap.** Is the orchestrator's per-dispatch minute budget stated
   (per `ORCHESTRATOR.md` §9 — substantive pass 45 min/agent, triumvirate
   wave ≤90 min) for any landing the intervention proposes, *and* did the
   producing V3 agent itself respect a stated cap?
5. **Same-wave consumer.** Per `ORCHESTRATOR.md` §8 ("Same-wave consumer —
   no orphan kernel") and the SK-V5 W5 orphan-kernel failure (REDRESS 50-55),
   does each proposed kernel/primitive name the hot-path call-site that
   consumes it in the SAME commit?
6. **Revert protocol.** Is the revert path stated? An intervention without a
   revert is a one-way door at landing.

Plus the V3-specific cost axes the dispatch flagged:

7. **Re-capture cost.** PMU re-runs are V4-convergence-relevant; is the
   wall-clock cost of an 17×2 re-capture stated, so the orchestrator can
   budget the V3.2 / V4 cycle?
8. **Cleanup risk tiering.** Doc archive-moves and code deletions have
   different risk classes; are they separated in cost terms?
9. **Cascade risk on docs.** Multi-doc surgical edits (P1-V3-F's 19 edits)
   carry a cascade risk if one edit alters terminology used by another;
   is each edit single-shot, or do later edits depend on earlier ones?

Per V3 artefact, this CH4 dispositions ≥5 cost claims/gaps. The full set is
in §2. The aggregate verdict is in §3. The V4 fold list is in §4.

---

## §2 Per-report disposition table

Thirty-five dispositions, indexed `D{nn}`. Each names the missing or
inadequate cost field (LOC / risk / wave / cap / consumer / revert /
re-capture / tier / cascade) and the disposition. The "ref" column cites
file:section in the V3 artefact.

### §2.1 P1-V3-A xctrace CPU Counters (`skv9-p1-v3-A-xctrace-cpu-counters.md`)

| # | Cost claim or gap | Missing cost field | Disposition | Ref |
|---|---|---|---|---|
| D01 | New probe binary at `bbnf-bench/src/bin/xctrace_probe.rs` is committed to workspace | LOC count of probe (cited "one Cargo bin target on the existing `bbnf-bench` crate"; no LOC; no `cargo build` time impact stated) | REVISE — name the probe LOC delta and the Cargo manifest delta; even a probe is in-tree LOC that counts toward generated-size budget per `generated-size-budget` feedback | §1.1, §7 |
| D02 | Per-corpus iteration count tuned: twitter 4000 / y_string_unicode 12000 / etc. | Wall-clock cost of one full 17×2 capture sweep is named ("~12 min" in §5 reproduction) but the cost of a V3.2 / V4 re-capture (PASS-1-PROFILE §4 requires "two consecutive cycles ≥95% ACCEPT") is not folded into the V4 budget | REVISE — state the convergence cost: V3 = 12 min, V3.2 confirmation = 12 min, plus per-cycle CHALLENGE wall = ~90 min; total V3+V3.2 ≈ 3.5 hours wall before `G-S-P1-RERUN-CONVERGED` can fire | §1.4, §5 |
| D03 | Per-symbol PMC attribution declared unrunnable on this host (xctrace export schema closed) | Cost of the alternative (Instruments.app GUI inspection per .trace OR a `kperf`-enabled binary OR `#[inline(never)]` probe rebuild) is named but neither LOC-budgeted nor wave-aligned | REVISE — the §6.2 fallback "third-party `m1cpu` / `applepmuctr` patches" silently requires root+SIP-relaxed boot; that is a host-infra cost (probably zero LOC, but high operator-toil), not a code cost — flag as out-of-budget for SK-V9 entirely | §6.1, §6.2 |
| D04 | Probe imports `runtime::generated_json::parse` and `bbnf_bench::track2::json::parse` "exactly as the existing `json_parity` Criterion bench does" | No same-wave consumer claim (because this is a profile probe, not a kernel) — but the consumer rule still applies to the *workspace* impact: who consumes `xctrace_probe`? V3 P1-V3-B does, transitively | ACCEPT — probe is a *profiling tool*, not a primitive; the consumer-rule applies only to kernels per `ORCHESTRATOR.md` §8 | §1.1 |
| D05 | Processor Trace template is BLOCKED by version skew; recorded "for completeness in case a later wave wants instruction-level traces" | "Later wave" is paper-deferral per `no-deferrals` feedback unless a wave is named | REVISE — name the wave that would consume Processor Trace data (likely V10 unicode-validation kernel design) OR delete the §6.3 mention as out-of-scope; do not leave as orphan future-promise | §6.3 |

### §2.2 P1-V3-B xctrace Time Profiler (`skv9-p1-v3-B-xctrace-time-profiler.md`)

| # | Cost claim or gap | Missing cost field | Disposition | Ref |
|---|---|---|---|---|
| D06 | Per-row iteration counts dramatically larger than V3-A (twitter 12 000 vs 4 000, y_string_unicode 220 000 vs 12 000) for 2.5 s steady-state window | Re-capture cost is much higher than A: `--time-limit 2500ms` × 34 captures = ≥85 s pure capture + export pipeline; total wall not stated | REVISE — name the V3-B total wall for one cycle so the V3.2 confirmation cycle can be budgeted; gate-cost transparency per `ORCHESTRATOR.md` §9 | §1.3, §1.4 |
| D07 | Per-symbol class taxonomy declared "substrate-neutral primitive vocabulary `S-P1` is producing" with classes such as `string_tiny_scan`, `simd_movemask`, `consume_structural` | No LOC budget for the aggregator script (`/tmp/skv9-xctrace-v3/aggregate.py`); that script is part of the reproducibility surface | REVISE — quote the aggregate.py LOC or commit it to the workspace; otherwise the reproducibility claim per CH4 is fragile across hosts | §1.5 |
| D08 | Top-8 per-row symbol table includes a *new* primitive vocabulary (`string_tiny_scan`, `whitespace_skip`, `simd_movemask`, `dispatch_value`, `consume_structural`, …) that S-P2 will consume | Each class is a *finding*, not an *intervention*, so no LOC/risk; but **every class implicitly becomes an S-P2 candidate kernel** — and not one carries a forward LOC estimate for the eventual kernel port | REVISE — for the four load-bearing classes the report names as gap-closers (`string_tiny_scan` at 30–56% in 11 rows; `whitespace_skip` 4–16%; `simd_movemask` 5–14%; `number_digit_scan` 19–21% on number rows) carry forward a *cost-class budget* per class for S-P2 to consume | §2 tables |
| D09 | "Processor Trace coverage: 0/3 — BLOCKED by Apple toolchain library skew" | Cost of the toolchain repair (matching Instruments.app version to macOS 26.4 device-side library) is unstated; this is a real V4 friction | REVISE — either name the toolchain-repair cost (a developer-tools upgrade — host-infra, zero LOC) or close the BLOCKED line as "deferred-out-of-S-P1" | §header, §4 |
| D10 | Probe binary is the same as V3-A's (`xctrace_probe.rs`) | Shared probe is good for KISS, but the build flags differ: V3-B requires `lto=fat` + `codegen-units=1` + `split-debuginfo=packed` (per §header), V3-A says only `target-cpu=native` + `debug=true` | REVISE — name the build-flag delta as a profile-cost: V3-A's build is reusable across captures but V3-B's `lto=fat` build adds compile-time (~3-5 min link on cold cargo); flag this in the V4 re-capture cost ledger | §header vs V3-A §header |

### §2.3 P1-V3-C Hot-Leaf Attribution (`skv9-p1-v3-C-hot-leaf-attribution.md`)

| # | Cost claim or gap | Missing cost field | Disposition | Ref |
|---|---|---|---|---|
| D11 | Eight-class structural classifier (structural_scan / string_scan / number_parse / escape_handling / tape_write / allocation / sync_overhead / traversal_other) | No LOC budget for the kernel work each class implies for V9 W1; only diagnostic counts named | REVISE — for each named class with a quantified loss ($string\_scan$ → 21-49% T1-defused share; $escape\_handling$ → 47.5% on unicode_escapes), state the kernel-port LOC envelope drawn from SK-V7 W3 (small LOC, admitted) vs W4 (REDRESS 82, ~modest LOC, rejected) precedents | §1.3 |
| D12 | SC-4 string-plane 75% claim "directionally correct but the literal 75% is not measurable post-W0 without xctrace cycles" | The §5.3 unicode_escapes "46.9% per-quote reduction to reach sonic × 0.90" is an *intervention size estimate*, not an LOC or wall-clock cost; the implied kernel work is open-ended | REVISE — name a kernel-LOC envelope for the masked-bitmap string-scan rewrite (the alternative shape SC-4 implies). Without it the §6 "directional" finding cannot guide V9 W1 cost-bind | §5.3 (referenced in V3-D §6.1) |
| D13 | SC-1 verdict: non-fusion claim holds at symbol layer; share-of-self-time claim "unfalsified at the samply layer and remains contingent on V3-A cycle-precision" | The falsification path "targeted `#[inline(never)]` build" is named but not LOC-budgeted, nor wave-aligned (is it an S-P1 probe, an S-P2 candidate, or a V4 re-profile?) | REVISE — name the wave + LOC for the `#[inline(never)]` probe build OR explicitly classify it as profile-only infra (per `build-infra-first` feedback) and put it in V4's pre-wave window | §4 |
| D14 | Track 2 hot-leaf attribution declared "samply-insufficient" | Cost of the Track 2 dedicated samply capture (a new bench binary or filter) is unstated; the report leaves it as "to refine after sibling xctrace captures land" | REVISE — V3-B in fact captured Track 2 (per its §2 tables); update V3-C to consume V3-B Track 2 columns in V3.2, or admit that V3-C's V3 commit is partial; either way name the V3.2 re-attribution cost | §1.2, §2.3 |
| D15 | Eight V2-shallowness bullets in §6 declare what V2 missed | No risk-classification per bullet; some are doc-only updates (#6 harness noise), some imply substrate changes (#7 cycle-precision needed) | REVISE — split §6 into low-risk (doc) and medium-risk (re-profile / probe-build) per `feedback_dispatch_hard_cap` discipline; otherwise the V3.2 dispatch cannot budget time-per-bullet | §6 |

### §2.4 P1-V3-D Structural Breakdown (`skv9-p1-v3-D-structural-breakdown.md`)

| # | Cost claim or gap | Missing cost field | Disposition | Ref |
|---|---|---|---|---|
| D16 | §6.6 explicit wave proposals: "V9 W1: string-plane cost cut (per-quote ~10–15%) — moves 9 of 11 parse_only losers to parity" | **No LOC budget. No risk class. No same-wave consumer named.** Wave alignment given (V9 W1) but entry/exit gate is named only in throughput-delta terms (Δ_p → sonic × 0.90), not in owner-path or kernel-LOC terms | REJECT — this is the single most consequential proposal in the V3 cohort, and it carries none of the §8 non-negotiables. SK-V7 W3 admitted (small LOC, REDRESS 81); SK-V6 wave-3 candidates 7-12 were rejected one-by-one (REDRESS 60-69, comparable kernel-LOC scales). Without an LOC budget and a named owner-file (`runtime/src/grammars/json/generated.rs` `match_tiny_plain_string_with_cap` vs `parse-that-regex/src/lib.rs` `match_string_at_quote_trusted_utf8`?) the W1 dispatch cannot be cost-bound | §6.1, §6.6 |
| D17 | §6.2 "V9 W2: digest-sink truth pass — independent of string plane; needed for direct-plane LOSSes" | Wave alignment named; no LOC; no risk class; no same-wave consumer; revert protocol absent | REJECT — same shape as D16. Direct-plane is decorrelated from string-plane (r = −0.033 in §4) which is a *gap-class finding*, not a wave plan. Without owner-files (digest sink lives at `bbnf-bench/src/direct_struct.rs:124`; "digest producer" is unclear) the W2 entry gate cannot be formed | §6.2, §6.6 |
| D18 | §6.3 "V10: unicode validation kernel — required only for unicode_mixed/unicode_escapes after W1 lands. Defer until W1 demonstrates the floor lift" | Explicit deferral to a *future iteration* (V10). Per `no-deferrals` feedback this is a paper-close pattern | REVISE — V10 deferral is admissible *only* if cost is named for V10 entry AND V10 is not framed as conditional on W1's outcome (which makes the V10 plan a contingent paper-close). Recast as "V9 W3 unicode validation if W1 sufficient lift" with an LOC budget tied to SK-V7 W4 single-quartet rejected precedent (REDRESS 82, modest LOC) | §6.2, §6.6 |
| D19 | §5.3 reduction table: 7 of 11 rows clear at 10% per-quote cut, 9 at 25%, 2 (unicode_mixed/escapes) need 30-50% + unrelated unicode work | The "%" reduction figure is a *target*, not a cost; the cost of *achieving* a 10% per-quote reduction in the masked-bitmap rewrite is not stated | REVISE — bind each %-reduction target to a kernel-LOC envelope; SK-V7 W3 capacity-hint LOC (small, admitted) is the precedent for "10%-ish lift admission size" | §5.3 |
| D20 | §6.4 "Direct plane: do not chase by string-plane wave … digest producer; see P1-V3-A/B xctrace lanes for that capture" | The direct-plane wave (W2 per §6.6) defers its own profile to V3-A/B — but neither V3-A nor V3-B captured the digest-producer specifically; they captured `parse_only` Track 1/Track 2 only. **This is a circular cost reference: W2 awaits a profile that V3 did not produce** | REJECT — name a missing direct-plane profile sub-task (V3.2 P1-V3-A2 or new probe) with LOC + wall cost, OR fold the W2 plan back into the W1 wave; do not leave W2 dangling on absent evidence | §6.4 cross-ref with V3-A §1, V3-B §1.2 |
| D21 | §5.1 OLS regression: `ns_per_byte = 8.64 * (q/B) + 1.47 * (n/B) + 0.410`. Quote marginal cost ~8.6 ns/quote | The "implied per-quote cost reduction" is treated as the wave knob, but the OLS is fit on 17 data points across heterogeneous grammars — confidence intervals are not given | REVISE — name a falsifiability gate: at minimum, an out-of-sample bench row (e.g. a synthetic quote-heavy fixture) that confirms the 8.64 ns/quote coefficient. Otherwise W1's exit-gate (D16) rests on an extrapolation | §5.1 |

### §2.5 P1-V3-E Legacy Cleanup Audit (`skv9-p1-v3-E-legacy-cleanup-audit.md`)

| # | Cost claim or gap | Missing cost field | Disposition | Ref |
|---|---|---|---|---|
| D22 | "524 ARCHIVE-MOVE files" doc count + "~700 src + ~160 test LOC SAFE-TO-DELETE" code count | Doc moves are git-mv operations (low-risk, mechanical); code deletions touch active workspace (medium-risk, must pass `cargo test`). **The two are not separately hard-capped or LOC-bounded** in the §4 recommended sequence | REVISE — split into two separate dispatches with distinct caps: (a) doc archive ~30 min (mechanical git-mv + path-rewrite of ~16 active-doc hits per §3); (b) code SAFE-TO-DELETE ~45 min with `cargo test --workspace` gate. The §4.3 "sequence recommendation" runs them through a single CHALLENGE pass; that conflates risk tiers per `feedback_dispatch_hard_cap` | §2.8, §4.3 |
| D23 | §3 "~16 hits in active (non-archive) docs require CRUD-wave path rewrites" | "CRUD wave" deferred to Pass Omega per the report; SK-V7 RESTRUCTURE "deferred-Omega bucket" cited | ACCEPT — explicit Pass Omega wave alignment, gate-correct per `ORCHESTRATOR.md` §6 G-Omega gate | §3 |
| D24 | 14 x86_64 orphan SIMD kernel files SAFE-TO-DELETE per REDRESS 50-55 + Lock 16 admission rule | LOC per file estimated (~440 src + ~80 tests for 14 src files = avg 31 LOC/file) but no commit-level grouping (one PR per family or one big delete?) | REVISE — name the commit granularity: one commit per ISA family (avx2, avx512_vbmi2, avx512_gfni, avx512_vpclmul, avx512_vnni, avx512_bitalg, avx512_kmask, avx_ifma) so each is bisectable, OR one bulk delete with explicit revert plan. The §4.3 step 5 says "separate commit so any test regression is bisectable" — fine for the boundary between doc + code, but does not address intra-code granularity | §2.1, §4.3 |
| D25 | R5 risk: "sk-v8 alpha/ and HANDOFF.md are KEEP-IF-CITED" — pending verification | Verification cost (one `rg` invocation, ~30 sec) is named implicitly but not in the §4.3 sequence | ACCEPT — low-cost gate, can fold into step 2 of §4.3 | §6 R5 |
| D26 | "No `cargo test` / `cargo build` was run during this audit (read-only contract)" + §7 "Any execution path must validate `cargo test --workspace --profile ax-iter` and `cargo run -p xtask --release -- check-json`" | This is the medium-risk gate for the §2 code deletions. **Wall-clock cost of `cargo test --workspace --profile ax-iter` is not stated** — and per `iter-profile-always` feedback ax-iter is the iteration profile, so it should be the dispatch budget | REVISE — name `cargo test --workspace --profile ax-iter` wall (5-15 min typical) so the cleanup-code dispatch hard cap includes the gate validation time | §7 |
| D27 | §4.1 archive structure: `restart/skinny/archive/sk-v3.5/`, `archive/sk-v5/`, etc. — five tranche directories moved en bloc | The cumulative `git mv` count is "~524 files" but no per-commit cap (one commit per tranche, or one giant tree move?) | REVISE — recommend one commit per archived tranche (5 commits) to keep `git log --follow` clean per SK-V7 RESTRUCTURE precedent, with a 5-min cap per commit including the path-rewrite pre-step | §4.1 |
| D28 | R6: "aarch64::digit_mac.rs parse_4_digits has multiple test refs but ZERO production callers" — SAFE-TO-DELETE | Risk class is medium (deleting tests too); revert protocol absent | REVISE — name a revert protocol: `git revert <commit>` is the bisectable path, fine; but if the deletion exposes a downstream test failure on x86_64 builds (per R8: `unimplemented!()` shells gated by cfg-target), the revert must also rollback the parent-module declaration deletion. Name this in the commit message | §6 R6, R8 |

### §2.6 P1-V3-F Redress Reconciliation (`skv9-p1-v3-F-redress-reconciliation.md`)

| # | Cost claim or gap | Missing cost field | Disposition | Ref |
|---|---|---|---|---|
| D29 | "19 surgical doc edits proposed. SPEC.md (8), HANDOFF.md (6), DISPATCH-PROMPT.md (5)" | Each edit framed as "paragraph- or list-level replacement; none touch source". **No per-edit LOC tally; no per-edit cap; total wall not stated** | REVISE — name the wall as a single Edit dispatch (the 19 edits are tightly coupled — they all replace V2-BLOCKED-language with V3-IN-FLIGHT-language; serialise into one ≤30 min dispatch per `feedback_dispatch_hard_cap`) | §4.4 |
| D30 | The 19 edits described as "single-shot LOC-bound surgical changes" implicitly | **Cascade risk**: Edit A of SPEC changes status to "post-S-P1-V3 in flight"; Edit B then names the V3 evidence root; Edit H rewrites §4 with V3 sibling deliverables. If P1-V3-C's "samply-insufficient" Track 2 finding upgrades in V3.2, Edit H needs re-editing | REVISE — declare cascade boundary: Edits A, B, D are pure status-vocabulary and stable; Edits E-H reference the V3 evidence root and re-trigger on V3.2. Sequence as two waves: status-vocab first, evidence-bound second after V3 CHALLENGE consolidation | §4.1 Edit A-I, §4.2 Edit A-F, §4.3 Edit A-E |
| D31 | §1.3 proposed "one-paragraph clarification" inserting "Direct hardware-counter reads — `perf` on Linux, `xctrace` with the `cpu-counters` template on macOS (full Xcode required), or privileged `powermetrics` — are admitted as real PMU sources" | No LOC count; "one paragraph" — fine for a clarification; **but the clarification is to `restart/prompts/skinny/PASS-1-PROFILE.md`** which is in the orchestrator's **read-only** scope per `ORCHESTRATOR.md` §7. **The orchestrator does not own pass-prompt edits.** | REJECT — the clarification is to a pass prompt; pass-prompt authoring is "a distinct directed task" (§7) and falls outside SK-V9 S-P1. Either drop the clarification proposal or escalate as a separate cross-pass task; do not bundle into the SPEC/HANDOFF/DISPATCH 19-edit set | §1.3, §4.1 vs ORCHESTRATOR §7 |
| D32 | "All eight HANDOFF §5 items are STILL-LOAD-BEARING … pre-block list is correct; it is merely incomplete relative to the underlying ledger" — four umbrella additions proposed | LOC small (four bulleted paragraphs); risk LOW (additive doc edit); same-wave consumer = the HANDOFF document itself | ACCEPT — low-risk additive doc edit with clear consumer (the next agent reading §5); folds into the same Edit dispatch as the 19 edits | §3.2 |
| D33 | §5.2 "V3 alone is insufficient; the gate requires two-consecutive ACCEPT, which means at least one repeat cycle" — V3 + V3.2 | The cost of the V3.2 cycle is named only at the gate-shape level (≥95% ACCEPT × 2) — not at the wall-clock or LOC level | REVISE — name V3.2 cost = (V3-A re-capture 12 min) + (V3-B re-capture, wall unstated per D06) + (V3-C re-attribute, paper-only) + (V3-E cleanup, deferred) + (V3-F, paper-only) + (CHALLENGE 90 min wall) — folded total ≈ 2-3 hours wall. Bind this in the orchestrator's V3.2 dispatch | §5.2 |
| D34 | §6.5 "Class-umbrella creep risk: medium — the four umbrellas compress ~20 specific REDRESS rejections into umbrella sentences" | Risk class named (medium) and mitigation named (each umbrella cites specific REDRESS entries; keep alpha-C-redress-digest.md as the binding-by-reference detail) | ACCEPT — risk class correct, mitigation concrete, no further cost binding required | §6.5 |
| D35 | §6.7 "Risk the V3 evidence root does NOT converge" — escalates V3.2/V3.3/V3.4 to V5 hard ceiling per PASS-1-PROFILE §4 | Multi-cycle cost: V3, V3.2, V3.3, V3.4, V3.5 each at ≈2-3 hours wall = up to 15 hours wall before BLOCKED escalation per `ORCHESTRATOR.md` §3Z. **Not stated as a worst-case budget anywhere in V3-F** | REVISE — name the V5 worst-case S-P1 wall ceiling so the orchestrator can pre-decide between "iterate" and "escalate at V4 if CH1/CH2 surface new defects" per `feedback_abrogate_before_patch` | §6.7 + ORCHESTRATOR §3Z hard ceiling |

---

## §3 Aggregate verdict

ACCEPT 5 (D04, D23, D25, D32, D34).
REVISE 26 (D01-D03, D05-D15, D18-D22, D24, D26-D30, D33, D35).
REJECT 4 (D16, D17, D20, D31).

Pass-V3 ACCEPT rate (CH4 only): 5/35 = **14.3%**.

CH4 verdict: **REVISE — V3 cohort does not meet the §8 cost non-negotiables
on the load-bearing interventions**.

The V3 cohort is research-strong (the PMU rows, the structural correlation,
the OLS regression, the hot-leaf taxonomy are all measured truth) but
**plan-weak**: the four cost-critical proposals (D16 V9 W1 string-plane cut;
D17 V9 W2 digest-sink pass; D18 V10 unicode kernel; D20 W2 circular evidence
reference) carry **none** of the six §8 non-negotiables CH4 enforces (LOC
budget, risk class, named owner files, same-wave consumer, revert protocol,
hard cap). These are S-P3 candidates dressed as S-P1 findings — but S-P1's
own §4 / §9 closing posture is "S-P1 produces evidence, and S-P2 produces
the hypotheses the evidence will or will not support." V3-D's §6.6
"three V9/V10 waves, ranked" pre-empts S-P3.

The cleanup pass (V3-E) is well-structured but conflates doc-archive risk
with code-deletion risk in a single sequence (D22, D26); the doc edits
(V3-F) carry a cascade risk that is acknowledged but not bracketed (D30),
and one proposed edit crosses the orchestrator's read-only scope into pass
prompts (D31).

The PMU re-capture cost (D02, D06, D33, D35) is the single most under-stated
cost vector across the V3 cohort: convergence requires V3+V3.2 minimum,
worst-case V3 through V3.5 = ~15 hours wall, none of which is named in any
V3 artefact.

---

## §4 Specific cost gaps requiring V4 fold

V4 must fold the following before re-dispatch:

### §4.1 Wave-plan interventions must carry the full §8 set or be deferred to S-P3

Per the S-P1 contract closing in `PASS-1-PROFILE.md` §9 ("S-P1 produces
evidence, and S-P2 produces the hypotheses"), the V3-D §6 wave proposals
must be **stripped from the S-P1 artefact** and handed to S-P2 with a
finding-only frame; OR each one must carry all six §8 non-negotiables for
V4 CH4 to ACCEPT.

If retained in V4, each of D16 / D17 / D18 must add:

1. LOC budget (drawn from SK-V7 W3 admit precedent ≈ small LOC vs SK-V6 W3
   rejected precedents at comparable LOC).
2. Risk class (medium for kernel ports; high for substrate touches to
   `runtime/src/grammars/json/generated.rs`).
3. Owner-files named at file:symbol granularity
   (`generated.rs:178 match_tiny_plain_string_with_cap` is one candidate per
   V3-B's twitter/track1 row #1).
4. Same-wave consumer: the hot-path call-site exercised by the kernel in the
   SAME commit. For W1 string-plane, this is the call from
   `dispatch_value` (at `generated.rs:47`) into `match_tiny_plain_string`.
5. Revert protocol: `git revert <SHA>` is the baseline; if the kernel adds a
   new `parse-that-regex` shape, the revert must also undo the consumer
   re-wiring in the same commit.
6. Hard cap per `ORCHESTRATOR.md` §9: triumvirate wave ≤90 min, plus
   pre-wave research + plan caps.

### §4.2 Re-capture cost must be the V4 dispatch's budget axis

PMU-cycle re-capture is the convergence bottleneck. V4 must declare:

- V3-A re-capture wall = 12 min (named).
- V3-B re-capture wall (unstated in V3-B; estimate from §1.3 iteration
  counts: 34 × 2500 ms = 85 s minimum, plus build + export overhead).
- Build flag difference: V3-B requires `lto=fat` + `codegen-units=1` which
  adds 3-5 min cold-link per V4 cycle.
- Aggregated V3.2 cycle wall ≈ 2-3 hours.
- Worst-case V3-through-V3.5 wall ≈ 10-15 hours per `ORCHESTRATOR.md` §3Z
  V5 ceiling.

These four numbers belong in the SPEC §4 Interlock row hard-cap column,
*not* the current `<=90 min` placeholder which silently undercounts.

### §4.3 Cleanup risk tiering must split V3-E into two dispatches

V4 must reframe V3-E §4.3 sequence as two distinct dispatches:

- **Dispatch E1 — doc archive + active-doc path rewrite.** Low risk;
  mechanical `git mv` of 524 files + ~16 active-doc CRUD rewrites; one
  commit per archived tranche (5 commits) per D27; ≤30 min total.
- **Dispatch E2 — code SAFE-TO-DELETE.** Medium risk; ~700 src LOC + ~160
  test LOC across ~19 src files + 1 fossil dir; commits granularised per
  ISA family per D24; gate validation = `cargo test --workspace --profile
  ax-iter` + `xtask check-json` + `xtask check-real-typed` + `xtask
  check-conformance`; ≤45 min including gate wall per D26.

The two dispatches must NOT run in the same commit; per `feedback_clean_
regen_discipline` and `feedback_no_workarounds`, code deletions land as
their own bisectable unit.

### §4.4 V3-F doc-edit dispatch must hold within the orchestrator's scope

V4 must drop D31's proposed PASS-1-PROFILE.md amendment from the V3-F edit
set; pass-prompt authoring is out of `ORCHESTRATOR.md` §7's orchestrator
scope. The 19 edits to SPEC / HANDOFF / DISPATCH-PROMPT stay; they fold into
one ≤30 min dispatch, sequenced into two waves per D30 (status-vocabulary
first, evidence-root second after V3 CHALLENGE consolidates).

### §4.5 OLS extrapolation requires a falsifiability gate

V3-D §5.1's `ns_per_byte = 8.64 * (q/B) + 1.47 * (n/B) + 0.410` regression
underwrites W1's exit-gate (the 10-15% per-quote reduction target). V4 must
either bind the regression to an out-of-sample fixture row (a synthetic
quote-heavy corpus the existing 17 do not contain) or downgrade the §6.6
"single-knob wave" framing from "moves 9 of 11 losers" to "may move 9 of 11
losers" pending falsification.

### §4.6 Track 2 attribution must be folded from V3-B into V3-C in V3.2

V3-C §1.2 declares Track 2 "samply-insufficient pending sibling V3-A
xctrace"; V3-B in fact captured Track 2 with full per-symbol attribution
(its §2 includes `bbnf_bench::track2::json::Parser::parse_value_at` and
`bbnf_bench::track2::json::match_tiny_plain_string` rows for every corpus).
V4 must fold V3-B's Track 2 columns into V3-C's classifier (cost: ~30 min,
paper-only, no re-capture); without this, V3-C's "Track 2 partial samply
evidence only" framing under-reports the V3 evidence root.

### §4.7 Processor Trace + per-symbol PMC must be classified out-of-S-P1

V3-A §6.2 ("kperf + root + SIP-relaxed boot") and §6.3 ("Processor Trace
library skew") are host-infra blockers, not S-P1 cost items. V4 must mark
them OUT-OF-SCOPE for SK-V9 entirely, not "for completeness in case a later
wave wants instruction-level traces" (which is the paper-deferral CH6 + CH4
both reject).

---

## §5 Sources

- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-A-xctrace-cpu-counters.md`
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-profiler.md`
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-C-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-D-structural-breakdown.md`
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-E-legacy-cleanup-audit.md`
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-F-redress-reconciliation.md`
- `restart/prompts/ORCHESTRATOR.md` §3W (CH4 contract), §8 (non-negotiables),
  §9 (hard caps), §3Z (V5 ceiling).
- `restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH4 (reproducibility),
  §4 (convergence), §9 (closing posture).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH4 (wave LOC budget,
  hard cap, same-wave consumer per primitive).
- `restart/skinny/tranches/sk-v9/SPEC.md` §1 non-negotiables, §2 wave
  manifest, §4 interlock.
- `restart/skinny/tranches/sk-v9/HANDOFF.md` §3 candidate boundaries,
  §5 pre-blocked routes.
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`
  (V2 CH4 REVISE precedent).
- `skinny/REDRESS.md`: SK-V7 W3 capacity-hint admit (REDRESS 81, lines
  2250-2284 — small LOC + same-wave consumer + checkasm gate + measurement
  evidence = ACCEPT precedent); SK-V7 W4 single-quartet unicode reject
  (REDRESS 82, lines 2286-2310 — modest LOC + falsifiability-gate fail =
  REJECT precedent); SK-V5 Wave 5 orphan-kernel rule (REDRESS 50-55);
  PMULL prefix-XOR reject (REDRESS 88); CTZ bulk reject (REDRESS 89).
- Memory feedback: `dispatch-hard-cap`, `no-deferrals`, `same-wave-consumer`
  via §8, `generated-size-budget`, `single-cargo-per-target`,
  `iter-profile-always`, `abrogate-before-patch`.
