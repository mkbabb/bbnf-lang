# SK-V21 DEFERRED-LEDGER — the final fold

Date: 2026-07-05, at freeze. This ledger supersedes the SK-V20 DEFERRED-LEDGER as the ledger of
record. Opening census: research/35 (the SK-V20 ledger reconciled line-by-line at HEAD
`05302109d`); folded here with every pass-1/2/3 discovery. The discipline, binding: **every row
carries either CLOSED (with the closing evidence) or a named SPEC wave AND a named close gate** —
no row may be carried without an owner, and no wave may reopen a closed row by renaming it. Wave
coordinates cite the frozen **SPEC.md** (freeze confirmed at pass 4, CRIT-spec-architecture-v4
100/100; SPEC-DRAFT-P1.md superseded), §11 manifest order
**TW0 → TW-S → TW1a → TW1b → TW1c → TW2 → TW4.1 → {TW3 ∥ TW4.2} → TW5 → TW6**; TW0-internal
owners carry §11.1's W0.x sequencing coordinates (SPEC.md:648-675).

---

## §0 — The D-G pre-block reconcile (C6's content, carried by name so no wave reopens it)

Per SPEC §11 TW0 first commit (§11.1 W0.0): the four REDRESS ids and their carried family meanings are entered
here permanently. These are REJECTED routes under the frozen architecture — any future proposal
matching a family below must cite this entry and name why its coupling differs, or it does not open.

| REDRESS id | Family | Carried meaning (the rejected route) |
|---|---|---|
| 51 | SK-V16 pre-block 50-55, retained-shortcut | The retained-parse-shortcut sibling of 53: a parse fast-path retained beside the canonical path |
| 53 | SK-V16 pre-block 50-55, retained-shortcut (canonical instance) | A structural index retained as a parallel vector alongside the ONE tape — a second substrate by another name |
| 246 | SK-V16 pre-block 242-247, fixed-shape/Unicode-retry | One-quartet Unicode/object-pair promotion under the old framing |
| 247 | SK-V16 pre-block 242-247, fixed-shape/Unicode-retry | Fixed-shape retry under the old framing |

Owner: **TW0 first commit — §11.1 W0.0** (doc-only; also places the `sk-v19-frozen` tag and the
supersession pointer). Gate: the commit exists on master; skinny becomes architecture-read-only
(SPEC §11 TW0, §11.1 W0.0).

## §1 — SK-V20 chronic items C1-C6

| # | Item | Final disposition | Wave | Gate | Evidence |
|---|---|---|---|---|---|
| C1 | AoS `TapeRec` → SoA tape | REVERSED to **AoS hardening** — the SoA pivot is the dig's hardest-won negative result (−60-76%, Era VI); single-writer `frame_depth`, `sib_skip` side-plane, AU primitive set restored; SoA may return only via a fresh measurement naming why its coupling differs | TW1b | Lock 1 falsified two ways (grep-zero `TapeStructBuilder` + runtime single-owner `frame_depth` assert); JSON+Sheets oracles field-for-field; no cold regression vs TW0 baseline | research/35 N19; research/14; research/09 §C1; SPEC §4, §11 TW1b |
| C2 | The C fold — ONE value-plane generator | Split across generator unification and the value plane; fused sink default, lazy cursor its own lane, one path alphabet | TW2 + TW3 | ONE emit path serves all 9 grammars (TW2); every `->` reaches the emitter, fused sink default with no cold regression per-corpus (TW3) | SPEC §4, §9, §11 |
| C3 | BackendShape / decision engine | RIGHT-SIZED: greedy tree-cost `Extractor` is the PERMANENT default; the one net-new artifact is the measured `KernelCostTable` bound to the LIVE `KernelShape::select` ladder; tautological CSP second stage retired; escalation only behind the Y.5 ablation | TW4.1 + TW4.2 | Table feeds live selector with `cargo expand` witness (TW4.1, liveness-only by settled fact — no current-fleet flip); cutover-gate + Lock-14 full-system (TW4.2); the ≥3%-win clause binds at TW6's CSV flip | research/27; PROTO-A2 D2 (N\*=3, prototypes/PROTO-A2-kernel-pmu.md:119); SPEC §5.1, §11 |
| C4 | Scanner unification (three schemes, dup movemasks) | Folds into the kernel-table unification: lo/hi nibble two-LUT, one kernel family | TW4.2 | Per-kernel differential parity + bench over the whole dispatch table | research/35 C4; SPEC §5, §11 TW4.2 |
| C5 | Lock-16 single-movemask co-gate | `vshrn` single-pack co-landed with C4; movemask spelling is a kernel-shape-dependent cost-model CELL, not a global unification (live path already `vaddv`) | TW4.2 | Structural co-gate green | SPEC §5.1 (FLIP-3 re-scope), §11 TW4.2 |
| C6 | D-G pre-block reconcile | Made EXECUTABLE — §0 above is its ledger half; the commit spec is fully enumerated | TW0 W0.0 | Doc-only commit on master: §0 entry + `sk-v19-frozen` tag + supersession pointer; unblocks every workstream and every TW wave | SPEC §11 TW0, §11.1 W0.0; research/35 C6; CRIT-freeze-audit.md:74-79 |

## §2 — Open SK-V19 waves (the W-map, folded)

| SK-V19 wave | Final disposition | Wave | Evidence |
|---|---|---|---|
| W0 substrate_target classification | TW1 entry gate, unchanged | TW1a/b | research/35 §2 |
| W1 eager-builder retirement | ~1,080 hits, 95.1% regen-owned — true hand surface ~40-60 template/type sites, then regen; one fenced parity-gated commit with the tape | TW1b | research/38 §1 (A2); SPEC §4, §11 TW1b |
| W2 AoS→SoA | = C1, REVERSED to AoS hardening | TW1b | §1 above |
| W3 C fold + value-api derive | = C2 | TW2+TW3 | §1 above |
| W4 StructRegistry projection fence | With projection-totality invariant (`PROJECTION_DIRECT_TO_STRUCT.len() == materializers == consumers`) | TW3 | SPEC §11 TW3; research/09 §D4 |
| W5a-c BackendShape + cost model | = C3, right-sized | TW4.1/4.2 | §1 above |
| W5d x86/wasm production deletion | Pure deletion (952 LOC + 92 stamped AVX2 hits) | TW0 W0.6 | research/16 item 6; SPEC §8.4, §11.1 W0.6 |
| W6a/b/c/e leak collapse | DONE pre-campaign (`f4633d468`, `36ca10646`); the green-by-exclusion caveat becomes TW0's five-lock audit — until the split lands, the widened Lock-14 keys on directory paths inside `core` | TW0 W0.3 (audit) | research/35 §2 W6 row; SPEC §8.3, §11.1 W0.3 |
| W6d StructLayout→Layout rename | 960 hits, 88.5% regen-owned; mechanical tail | TW1c | research/38 §1; SPEC §11 TW1c |

## §3 — SK-V20 audit rows N1-N18

| # | Item | Final disposition | Wave | Gate | Evidence |
|---|---|---|---|---|---|
| N1 | `json_templates` false-`@generated` couriers | Couriers derived-or-owned under the one generator; paired with the N25 idempotency rule and the WART-C JSON fold (§6) | TW2 | `regen --check` clean; widened Lock-14 over codegen | research/16 items 7-8; SPEC §11 TW2 |
| N2 | Quiet bench re-capture | Instrument built and adjudicated at pass 2 (ratios certified); ONLY the loadavg<2 absolute leg remains | TW0 W0.11 | Quiet truth-floor session ledgered (loadavg<2 verified in-log): cold CSS+JSON both sides, PMU absolutes, samply flame, per-corpus-shape reporting | SPEC.md:19-32; §11.1 W0.11 |
| N3 | JSON warm/cold conflation | CLOSED IN KIND: `json_canon_bench` exists, produced research/33, rebuilt at pass 2 with digest-parity assert PASSING; graduates to `crates/` | TW0 W0.11 (quiet leg only) | Parity assert folded (done); quiet re-capture | research/33; PROTO-A1; SPEC §2, §12 |
| N4 | Five grammars with zero oracle | Oracle completion, CSV vs the `csv` crate first | TW6 | Zero grammars without an oracle | SPEC §9, §11 TW6 |
| N5 | Dead/lying oracle surfaces | Oracle hygiene + W3C rekey | TW0 W0.9 | Locks/tests green at TW0 close | SPEC §11.1 W0.9 |
| N6 | `css_l4.rs` 108,406 lines (7× budget) | Regression trace attacks line count ITSELF (the B2 lesson: relocation is not reduction); `generated_loc_budget` enforced in `regen --check` | TW2 | Budgets met at TW2 close | research/16 item 7; research/12,14; SPEC §11 TW2 |
| N7 | Two-`bbnf-regex` collision + ad-hoc FIRST scanner | ONE regex home — delete the skinny 322-line facade; canonical engine via the totality path patch; parse-that 1.0.0 cut is TS-only, no Rust impact; NO smallbox re-import (count=0 proven) | TW2 | Grep-zero on the retired crate; build green | research/17; PROTO-A3 D2; SPEC §7.3 |
| N8 | Dead kernels behind green checkasm / GrowOnly realloc churn | RESOLVED NEGATIVE by P1 (the seventh presize loss): DELETE `Sampled`/`Exact`/`OneShotSimd`, keep `GrowOnly`; the family has zero `core` occurrences — the delete closes in `crates/simd-scan`/`crates/ir`; remaining dead kernels (`unescape_uxxxx`, two dead `asm!` blocks, lying `Scalar` label) wired-or-deleted under checkasm liveness | TW1c (delete) + TW4.2 (liveness) | Grep-zero on retired names (TW1c); checkasm iterates EVERY registered variant (TW4.2) | SPEC §2, §5.4, §11 TW1c; research/38 §1 |
| N9 | Type-system vaporware (no real HM) | Honesty struck the claim; the build is §6's ordered steps, width fix first | TW5 | 9 grammars type end-to-end, zero `unwrap_or(Span)` AND correct width per annotation; host-signature falsifier fires | research/28; SPEC §6, §11 TW5 |
| N10 | Value API: three substrates, four path APIs, dead `Deserializer` | Two lanes, one path alphabet (the four JSON-pointer-shaped implementations collapse); serde bridge rides the cursor; facet rejected | TW3 | TW3 close gate (§1 C2 row) | research/16 item 4; SPEC §4 |
| N11 | Lock violations (4, 15, 12, 6) | Lock 4 fix re-vendors against csc411 upstream (never forks) with diff checkpoint (W0.5); Lock 15 + target-cpu OWNED by TW0, sole owner (W0.4); Lock 12 archive decision executed (W0.6); Lock 6 = TW2 | TW0 W0.3/W0.4/W0.5/W0.6 (+TW2 for Lock 6) | Five-lock falsifier audit green — each falsifier demonstrably FIRES on its phantom, then reads green | research/18 §5a; SPEC §7.2, §8.3-8.4, §11.1 W0.3-W0.6 |
| N12 | Inline `#[cfg(test)]` in src/ | Scope-corrected 7 core + 21+ skinny (incl. the four perf_cost test sites) | TW0 W0.7 | Zero `#[cfg(test)]` in src/; suite green | research/16 item 5; SPEC §10, §11.1 W0.7 |
| N13 | Unused language surface (closures/GrammarCall/@host) | `@host` declaration surface moves INTO TW5 with its consumer; the wider closure surface is land-or-delete | TW5 (@host) + TW6 (rest) | Host-signature gate fires on falsifier grammar (TW5); zero live host hatches not expressible as grammar (TW6) | SPEC §6, §11 TW5/TW6 |
| N14 | CSS depth closure (3 tape kinds) | Derived typed catalog from W3C/MDN machine-readable data + explicit Unparsed/Custom escape hatch; comparator re-pinned at TW-S first | TW3 | Full-depth CSS oracle green | research/24; SPEC §4 |
| N15 | Full-tree-touch bench plane absent | Folded into the TW0 truth-floor session | TW0 W0.11 | Plane exists in the quiet baseline ledger | SPEC §2, §11.1 W0.11 |
| N16 | MASTER-PLAN receiver-row drift (§13.6) | Exact correction spelled out (W6 label consumed by w6abe/w6c) | TW0 W0.8 | Doc reconcile commit; no surviving plan text contradicts the frozen SPEC | research/16 item 9; SPEC §11.1 W0.8 |
| N17 | `bbnf-path` registry promise (JSON sidecar) | Rides the one-alphabet path plane | TW3 | TW3 close gate | research/35 N17 |
| N18 | Empty-husk crates | `bootstrap` deleted, x86/wasm bodies out (W0.6); topology moves (TW1a/TW2); Locks 7/11/13 vacuous-falsifier fix folded into the five-lock audit (W0.3) | TW0 W0.3/W0.6 + TW1a/TW2 | Five-lock audit; split gate (TW1a) | research/20 §1, §2b; SPEC §8.3-8.4, §11.1 W0.3/W0.6 |

## §4 — Dig resurrections (research/35 §5, N19-N27)

| # | Item | Final disposition | Wave | Gate | Evidence |
|---|---|---|---|---|---|
| N19 | AoS-beats-SoA negative result vs C1 | RESOLVED in the dig's favor — C1 reversed (§1); the SPEC states the reconcile explicitly | TW1b | §1 C1 gate | SPEC §4; research/14 |
| N20 | Structural bitmap pre-scan (twice-buried architecture) | RESOLVED NEGATIVE — P1 is the seventh and terminal loss; DELETE decided; a CONSUMED stage1→stage2 index (different design, never refuted) may return only via the §5 cost table with a measured win | TW1c (delete); cost-table door | Grep-zero retired names; any return carries a measured win | SPEC §2; research/04, 06, 21 |
| N21 | Y.13 consumer-invariant gate template | ADOPTED as standing wave-close law — every substrate wave's gate instantiates it, liveness AND measured win | TW0 W0.2 (adoption) → all waves | §1 law binds every TW1-TW4 close; every subsequent wave-close cites the template | `Y/Y.md:326-333` via research/15; SPEC §1, §11.1 W0.2 |
| N22 | csp-solver `node_budget` freezing guard survival | Verified present in the re-vendored crate BEFORE any escalation is considered | TW0 W0.5 checkpoint / TW4.2 precondition | Guard verified before the ablation gate opens | research/15, 18; SPEC §5.3, §11.1 W0.5 |
| N23 | Y.5 connected-components CSP decomposition | The FIRST implementation step of the escalation ablation — the separability instrument | TW4.2 | Ablation gate: escalate only if greedy provably differs from optimum on the real corpus | research/15, 27; SPEC §5.3 |
| N24 | Rank-1 dead-substrate reclaim inventory (dta.rs, tape::visitor, orphan StructBuilder) | One-session `rg` sweep; delete with the topology moves | TW1c (+TW0 W0.6 deletions) | Grep-zero at TW1c close — the retired-name gate closes once, presize family included | research/10; SPEC §8.4, §11 TW1c, §11.1 W0.6 |
| N25 | Idempotent-on-disk codegen rule (B6.W0, 192× payoff) | Hard rule for every codegen surface | TW2 | `regen --check` idempotency assertion | research/13; SPEC §10, §11 TW2 |
| N26 | Rust-stack edition/toolchain axis (no SK-V20 antecedent) | Own wave granted, PRICED (five edits applied in-worktree; divan EXIT 0 both channels) | TW-S (TW-S.1-5) | Skinny + surviving crates build on pinned stable, edition 2024; cold bench parity vs TW0 baseline | research/19; PROTO-A3; SPEC §3 |
| N27 | csc411 active-upstream coordination | Diff-against-upstream checkpoint precedes the Lock-4 fix; re-vendor, never fork; 0.2.0 API break pre-staged mechanically | TW0 W0.5 | Checkpoint recorded before Lock-4 lands | research/18, 36; SPEC §7.2, §11.1 W0.5 |

## §5 — Deferred by design (authority unchanged, re-verified)

| Item | Authority | Standing |
|---|---|---|
| TS/WASM backends | Lock 5 (post-V1) | Stays deferred; the parse-that TS 1.0.0 break has no bearing (bbnf has no TS backend to break) — research/17; research/35 §4 |
| Profile-guided specialization | deferral-audit-6 §4 | Precondition SATISFIED when TW4.1 lands the KernelCostTable — the FIRST candidate re-examined at TW4.2 close, per the original authority's own wording |
| TS path runtime (`bbnf-path-ts` full) | deferral-audit-5 §3 | Rides the WASM decision; stays deferred |

## §6 — Pass-loop discoveries (found and dispositioned inside SK-V21 itself)

Every item the three passes surfaced, with its terminal state. CLOSED means the closing evidence
exists in this directory; wave-owned means the residue is an execution deliverable.

| Discovery (pass) | Final disposition | Wave / status | Evidence |
|---|---|---|---|
| Presize family live-and-losing; Sampled canada +9.3% (1) | Sampled overturned to +1.25% TIE under paired CIs; family DELETED for KISS; decision CLOSED | TW1c executes the delete (decision ledgered at W0.6); TW0 W0.11 quiet-absolute leg only | PROTO-P1; PROTO-A1 D1; SPEC §2, §11.1 W0.6 |
| canada sign contradiction — P2 0.79× vs report 33 (1) | SETTLED: fused LOSES canada (1.252× wall, 1.137× cyc/B); the float tax is TW4.1's target | TW4.1 gate: canada eager cold beats sonic (Eisel-Lemire) | prototypes/PROTO-A1-bench-adjudication.md:88; research/33:55,63-69 |
| Fold-over-lazy-cursor as the one-substrate default (1→2) | REFUTED — loses sonic 2.3-4.9×, own sink 3.9-4.2×; default is the DISTINCT fused sink; recorded, never re-benched; any revival requires fresh measurement naming why it differs | CLOSED; TW3 records the resolution | PROTO-A1 D3; SPEC §4 |
| AU floor arithmetic (0.615 inversion) + dual-floor ambiguity (1) | CLOSED as spec edits: 1.63 cyc/byte with 09:144 provenance; ONE governing floor = TW0 cold baseline; AU demoted to directional context; AU-relative capture taken (parse_tape twitter 1.5654) | CLOSED; TW0 ledgers the context capture | CRIT-spec-honesty; PROTO-A1 D4 (:21,156); SPEC §2 |
| TW1 bundling / seam order self-defer (1→2) | TW1 unbundled to TW1a/b/c; split-first ADJUDICATED sound (81.6% of `core` regen-owned; counts are regen artifacts — amendments A1/A2 bound in) | CLOSED | research/38; SPEC §4, §8, §11 |
| TW3↔TW4 decode coupling hidden behind the parallel bar (1) | TW4 split: TW4.1 lands before TW3 closes; coupling carried openly | CLOSED (manifest order) | SPEC §11 |
| P7 phantom fork — measurement disqualified (1) | Adjudicated: dispatch un-forked/structural at HEAD; true walls three SHARED defects; walls A/B then DISSOLVED at pass 2 (7/7 toml round-trip); WART C widened to the two-site JSON fold | TW6: in-tree regen wiring (WsPolicy through `lower_program`, interpreter into the `RegexProgram` arm, JSON fold at submodule `efc6b875f`) | CRIT-P7-adjudication; prototypes/PROTO-B1-toml-real.md:18,97; SPEC §9 |
| P4 FLIP-3 movemask inconsistency (1) | INAPPLICABLE to the live path (already `vaddv`); the spelling is a kernel-shape-dependent cost-model CELL; TW0 sweep item WITHDRAWN | CLOSED; cell rides the KernelCostTable | PROTO-A2; SPEC §5.1 |
| P4 consumer-dead cost table (1→2) | Live crossover N\*=3, NO current-fleet flip — measured fact; flip-consumer bound to TW6's CSV (|S|≤2), named in the table's landing commit | TW4.1 (liveness gate, relaxed per NEW-R8) + TW6 (flip exercise, where the ≥3% clause binds) | PROTO-A2 D2 (:119,129,138); SPEC §5.1, §11 |
| P5 asm doors unpriced (1→2) | Both doors PRICED: door (a) `vshrn` intrinsic +34.6/36.5% beats hand asm ~20-28%; binding resource = 8 SIMD→GPR crossings (~2.6× the ALU lever); asm lane OBVIATED; per-kernel gate stands unexercised | CLOSED; TW0 PMU absolute re-capture only | prototypes/PROTO-A2-kernel-pmu.md:55; SPEC §0, §5.4 |
| P6 width destruction (22/33 land I64) + wrong root cause (1→2) | Root cause CORRECTED (view.rs:141 span destruction, not literals.rs); fix BUILT and verified (wrap.rs helper, +45/−15, all 22 Nu8→U8, i64=0) | TW5 step 0 re-lands the proven patch + FnCall-args preservation; TW5 gate asserts WIDTH, not just Span-free | PROTO-B3 (:10-16,48,53); SPEC §6, §11 TW5 |
| @host unifier synthetic-only; Record/Sum absent from corpus (1) | Explicit disposition: HostSigTable + `@host` surface land in the SAME wave with a falsifier grammar; HM steps 4-6 DEFERRED until a fleet grammar demands them — never silent committed scope | TW5 (@host); TW5/TW6 (consumers) | SPEC §6 |
| P3 stack scope MIXED; smallbox coupling claim (1→2) | Scope decided (surviving-crates-only); smallbox `cargo tree` count=0 through the canonical regex subtree — coupling recommendation FALSIFIED; migration APPLIED (5 edits); divan EXIT 0 both channels | CLOSED; TW-S executes; TW-S.2 = criterion-ingestion re-path (60+ refs) | PROTO-A3 D2/D4; SPEC §3 |
| Nightly-vs-stable channel ratio (2) | LOAD-CONFOUNDED (nightly window loadavg 19-25) — explicitly not decision-grade | TW0 W0.11 un-confounded capture — MUST run before TW-S retires the nightly pin (a §11.1 hard ordering) | SPEC §3, §11.1 W0.11; SPEC.md:227-228 |
| Environmental mandate never met (2) | Adjudicated: ratio-certified decisions, directional absolutes; quiet re-cert is a TW0 EXECUTION gate; no pass-2 decision reopens | TW0 W0.11 quiet session | SPEC.md:19-32 |
| Honesty Defects A/B/C (2) | All reconciled in place (asm flagship sentence; benched-negative default relabel; Sampled "deleted for KISS, not null") | CLOSED (verified by v3 re-score 94) | CRIT-spec-honesty-v3; CONVERGENCE-LEDGER pass 3 |
| NEW-R8 TW4.1 gate contradiction (3) | RESOLVED: cost-table leg closes on liveness alone (no-flip is settled fact); ≥3%-win binds at TW6's CSV; Eisel-Lemire leg keeps liveness-AND-win | CLOSED (spec edit, verified FOLDED EXACTLY at pass-4 confirmation) | CONVERGENCE-LEDGER:170-180; SPEC §11 TW4.1; CRIT-spec-architecture-v4.md:17-27 |
| NEW-R9 §4 lane phrasing (3) | RESOLVED: "two lanes, only one of them tape-side" | CLOSED (verified folded at pass-4 confirmation) | CONVERGENCE-LEDGER:181-185; CRIT-spec-architecture-v4.md |
| CSS SIMD tokenization severity (1, carried) | Ring-fenced: NOT TW4 implementation scope; enters only via a costed per-grammar decision once the TW0 measurement loop is live — a scheduled decision point, not a design unknown | TW4.2 (decision point) | SPEC §13.1; CRIT-freeze-audit.md:116-123 |
| TW0 internal breadth (~14 workstreams) (2→3) | CLOSED AT FREEZE — the frozen SPEC sequences TW0 internally: §11.1 orders W0.0-W0.12 with dependency edges and three hard orderings (W0.0 first; W0.11 after W0.4+W0.6; W0.11 before TW-S); the freeze audit's one least-crisp residual retired in the frozen document itself | CLOSED | SPEC.md:648-675,715; CRIT-freeze-audit.md:95-108 |
| parse-that LR/packrat GO/NO-GO (gap G3) (1) | NO-GO, dated 2026-07-05 — bbnf eliminates left-recursion at compile time; parse-that released from the bbnf keep | TW0 W0.10 records the outbound letter | research/37 §6; SPEC §7.1, §11.1 W0.10 |
| csp-solver P0 transfer question (1) | Does NOT transfer — `ac3.rs:128` unreachable from every bbnf call path; gates nothing; 0.2.0 break pre-staged | TW0 W0.5 (§7.2 checkpoint) | research/36; SPEC §7.2, §11.1 W0.5 |
| Instruments graduating | `json_canon_bench` (parity assert folded, passing; W0.11's instrument of record) and the P5 checkasm-shaped parity harness graduate to `crates/` | TW0 W0.11 / TW4.2 assets | SPEC §12, §11.1 W0.11 |

## §7 — The empty-chronic-ledger assertion

Accounting at freeze: 4 pre-block ids permanently rejected (§0); 6 chronic items — six of six
dispositioned with a named wave and gate, zero carried open (§1); 9 SK-V19 wave rows folded (§2);
18 SK-V20 audit rows — eighteen of eighteen owned (§3); 9 dig resurrections — two resolved as
closed negatives (N19, N20), seven bound to waves (§4); 3 deferred-by-design items with unchanged
authority and one named re-examination trigger (§5); 23 pass-loop discoveries — eleven CLOSED
outright with evidence in this directory (the fold-over-cursor refutation, the AU-floor and
dual-floor edits, the TW1 unbundling, the decode-coupling split, FLIP-3, the door pricing, the P3
scope-and-smallbox closure, the honesty defects, NEW-R8 and NEW-R9 both verified folded exactly
at the pass-4 confirmation, and the TW0 sequencing — closed in the frozen document itself at
§11.1), the remaining twelve carrying only wave-owned execution residue with the deciding
evidence already on disk (§6).

**The chronic ledger is EMPTY.** No item in this document is chronically deferred: every row is
either CLOSED with its closing evidence cited, or owned by a named wave of the frozen manifest
with a named close gate — and the four execution residuals of consequence are exactly the four the
freeze audit certified (TW0 quiet re-certification at W0.11; TW-S cold parity; TW5 step-0 re-land;
TW6 regen wiring + CSV flip — CRIT-freeze-audit.md:126-130). The freeze confirmed at pass 4
(CRIT-spec-architecture-v4 100/100; SPEC.md the document of record) with this accounting standing
unamended. Under the §1 standing law, no future tranche may open a "deferred" successor to any row
here without either citing its close evidence as stale or naming the wave that failed its gate.
The category this ledger existed to track — items deferred without an owner — has zero members at
freeze.
