# 09 — The DEFERRED-LEDGER re-home table

Walks `sk-v21/DEFERRED-LEDGER.md` (the final fold: §0 pre-block, §1 C1-C6, §2 the SK-V19 W-map,
§3 N1-N18, §4 N19-N27, §5 deferred-by-design, §6 the 23 pass-loop discoveries) row by row.
Every row already carried a SK-V21 `TW*` coordinate; `sk-v22/SPEC.md` §1.2 re-mapped every
`TW*` to a `GW*` coordinate at graduation. This table adds the third link: `GW*` → the SK-V23
`TB*`/`PT-R*`/`PT-T*` coordinate, per `sk-v23/SPEC-DRAFT.md` §3-§4. Where a row's disposition was
already CLOSED at SK-V21 freeze with no execution residue, the SK-V23 owner is recorded as
"CLOSED — historical" (no new wave opens it; TB1.W4's continuity audit re-affirms it stands).
Where a row's SK-V21→SK-V22 wave never got an unambiguous SK-V23 restatement, it is flagged
**ORPHAN** below and repeated in the loud summary at the foot of this document.

Coordinate chain notation: `TWx (sk-v21) → GWy (sk-v22, SPEC.md:line) → TBz/PT-Rz/PT-Tz
(sk-v23, SPEC-DRAFT.md:line)`.

---

## §0 — The D-G pre-block reconcile (4 REDRESS ids)

| Row | SK-V21 wave | SK-V22 GW | SK-V23 owner | Gate | Note |
|---|---|---|---|---|---|
| REDRESS 51/53 (retained-shortcut family — DEFERRED-LEDGER.md:22-24) | TW0 W0.0 | GW0.a (SPEC.md:64) | **TB1.W0** (tag cut, the D-G reconcile now actually executes — SPEC-DRAFT.md:207-210) + **TB1.W4** (registry clearance — SPEC-DRAFT.md:227-231, ledger item 16) | TB1.W0 commit exists + TB1's Lock-14 planted-phantom fire | Permanently rejected route, carried by name — no future TB may reopen without citing this row |
| REDRESS 246/247 (fixed-shape/Unicode-retry family — DEFERRED-LEDGER.md:25-26) | TW0 W0.0 | GW0.a (SPEC.md:64) | **TB1.W0** + **TB1.W4** (same mechanism as above) | Same | Same standing rejection |

---

## §1 — SK-V20 chronic items C1-C6

| # | Item | SK-V21 wave | SK-V22 GW | SK-V23 owner | Gate | Note |
|---|---|---|---|---|---|---|
| C1 | AoS `TapeRec` (REVERSED, AoS hardening) | TW1b | GW1, "COLLAPSES TO A GATE" (SPEC.md:78) | **TB2.W4** (Lock-1 falsified two ways — SPEC-DRAFT.md:264-266) | Grep-zero `TapeStructBuilder` + runtime single-owner `frame_depth` assert | Carried as **settled evidence** in the §1 delineation table ("Runtime tape (AoS) | CARRY" — SPEC-DRAFT.md:99) |
| C2 | The C fold — ONE value-plane generator | TW2 + TW3 | GW2a/GW2b (SPEC.md:80) + GW4 (SPEC.md:82) | **TB3.W0/W1** (generator) + **TB4.W2** (value plane) | TB3 GW2a in-tree gate + TB4 value-plane consumer-invariant gate | Split matches SK-V22's own C2 split |
| C3 | BackendShape / decision engine, right-sized | TW4.1 + TW4.2 | GW3 (SPEC.md:81) + GW4 (SPEC.md:83) | **TB5.W1** (KernelCostTable greenfield dispatch) + **TB5.W4** (decision engine right-sized) | TB5 close gate (checkasm-liveness + measured win per kernel) | — |
| C4 | Scanner unification | TW4.2 | GW4 (SPEC.md:83) | **TB5.W1/W2** (cost-table + SOTA NEON uplift folds the unification) | Same TB5 close gate | — |
| C5 | Lock-16 single-movemask co-gate (vshrn) | TW4.2 | GW4 (SPEC.md:83) | **TB5.W2** ("the vshrn-nibble-domain movemask wherever the vaddv anti-pattern survives" — SPEC-DRAFT.md:401-403) | Structural co-gate green, byte-exact vs scalar reference | — |
| C6 | D-G pre-block reconcile | TW0 W0.0 | GW0.a (SPEC.md:64) | **TB1.W0** | Doc-only commit exists on the graduated tree | Same mechanism as §0 above — C6 and §0 are one executable unit |

---

## §2 — SK-V19 wave rows (the W-map, folded)

| SK-V19 wave | SK-V21 wave | SK-V22 GW | SK-V23 owner | Gate | Note |
|---|---|---|---|---|---|
| W0 substrate_target classification | TW1a/b | TW1a SUPERSEDED (SPEC.md:77); TW1b→GW1 | **CLOSED — historical**, subsumed: skinny's topology already IS the classified substrate (SPEC-DRAFT.md §1 table, "Runtime tape (AoS) CARRY") | n/a | No action needed; the classification is already fact in the graduated tree |
| W1 eager-builder retirement (~1,080 hits) | TW1b | GW1, collapses to gate (SPEC.md:78) | **TB2.W4** (grep-zero `TapeStructBuilder`) | Same C1 gate | **CLOSED — historical** in substance: skinny already has no eager builder; TB2.W4 only re-verifies |
| W2 AoS→SoA | = C1 | = C1 | = C1 → **TB2.W4** | — | dedup with C1 |
| W3 C fold + value-api derive | = C2 | = C2 | = C2 → **TB3.W0/W1 + TB4.W2** | — | dedup with C2 |
| W4 StructRegistry projection fence | TW3 | GW4 (SPEC.md:82) | **TB4.W2** ("projection-totality invariant executable" — SPEC-DRAFT.md:347) | `PROJECTION_DIRECT_TO_STRUCT.len() == materializers == consumers` | — |
| W5a-c BackendShape + cost model | = C3 | = C3 | = C3 → **TB5.W1 + TB5.W4** | — | dedup with C3 |
| W5d x86/wasm production deletion | TW0 W0.6 | GW0.c, SUPERSEDED by whole-tree archival (SPEC.md:70) | **TB1.W2** (the 502 x86 hits die with `crates/` removal) + **TB1.W3/§2** (the successor: full x86 abrogation across both repos supersedes the narrow W5d scope) | TB1 close gate + the new x86 anti-regrowth guard firing on a plant | SK-V23 **widens** this row's scope past its SK-V21 shape — worth noting as scope growth, not mere re-homing |
| W6a/b/c/e leak collapse | TW0 W0.3 (audit) | GW0.d (SPEC.md:67) | **TB1** close gate (Lock-14 re-keyed, FIRES on planted phantom — SPEC-DRAFT.md:235-238) | Lock-14 audit green | Commits (`f4633d468`, `36ca10646`) already landed pre-campaign; only the audit-verification residue carries forward |
| W6d StructLayout→Layout rename | TW1c | GW1, "MOSTLY SUPERSEDED" (SPEC.md:79) | **TB2.W4** (hygiene tail, grep-zero retired names) | grep-zero | — |

---

## §3 — SK-V20 audit rows N1-N18

| # | Item | SK-V21 wave | SK-V22 GW | SK-V23 owner | Gate | Note |
|---|---|---|---|---|---|---|
| N1 | `json_templates` false-`@generated` couriers | TW2 | GW2a (SPEC.md:61,148) | **TB3.W1** (`json_templates/` + `json_sink_direct/` delete — SPEC-DRAFT.md:293-295) | Differential oracle byte-identical, then delete | — |
| N2 | Quiet bench re-capture | TW0 W0.11 | SUPERSEDED-AS-AMENDED, DONE (research/04) + §5 law (SPEC.md:62,75) | **CLOSED** — carried verbatim as SK-V23 §0 governing preamble profiling standing law (SPEC-DRAFT.md:49-56) | n/a | loadavg<2 gate formally retired; no wave re-opens it |
| N3 | JSON warm/cold conflation | TW0 W0.11 (quiet leg only) | CLOSED IN KIND, graduates to `crates/` (SPEC.md:63) | **TB2.W2** (bench-harness shrink; `json_canon_bench` is the carried instrument of record) | Instrument present in the shrunk harness | Quiet leg closed by the §0 standing law |
| N4 | Five grammars with zero oracle | TW6 | GW6 (SPEC.md:85) | **TB6.W0** ("every grammar gains an oracle (zero grammars without one)" — SPEC-DRAFT.md:441-442) | Zero grammars without an oracle | — |
| N5 | Dead/lying oracle surfaces + W3C rekey | TW0 W0.9 | GW1 (SPEC.md:73) | **SOFT ORPHAN** — no SK-V23 wave names "oracle hygiene + W3C rekey" explicitly; the closest homes are TB2 (general hygiene, no oracle-specific wave item) and TB1.W4's differential-oracle harness (a *new* mechanism, not a hygiene pass over *existing* oracle surfaces) | — | **Flagged below** — genuinely diffused across TB2/TB1.W4/TB6.W0 with no single item claiming the "dead/lying" hygiene sweep |
| N6 | `css_l4.rs` 108,406 lines (7× budget) | TW2 | GW2a (SPEC.md:66) | **TB3.W0** (`generated_loc_budget` enforced — SPEC-DRAFT.md:285-286) | Budget gate in `regen --check` | **CLOSED — historical** in magnitude: skinny's own generated css_l4 is already 974 LOC (sk-v22 SPEC.md:80); TB3.W0 only keeps the budget gate live going forward |
| N7 | Two-`bbnf-regex` collision + ad-hoc FIRST scanner | TW2 | GW2b (SPEC.md:67,153) | **TB3.W2** (paired with **PT-R3.W1**) | Vendor `diff -r` clean + grep-zero fork | — |
| N8 | Dead kernels / `GrowOnly` realloc churn | TW1c (delete) + TW4.2 (liveness) | GW1 + GW4 (SPEC.md:68) | **TB2.W4** (delete, grep-zero) + **TB5.W1** ("the ~15 carried kernel bodies wired-or-retired per wire-before-retire" — SPEC-DRAFT.md:397-398) | grep-zero (TB2) + checkasm iterates every registered variant (TB5) | — |
| N9 | Type-system vaporware (no real HM) | TW5 | GW5 (SPEC.md:84) | **TB4.W1** (type plane greenfield) | Per-rule TypeDesc/registry equivalence, every corpus grammar | Scope GREW to full greenfield — SK-V23's one named override (SPEC-DRAFT.md:104-106) |
| N10 | Value API: three substrates, four path APIs, dead `Deserializer` | TW3 | GW4 (SPEC.md:82) | **TB4.W2** (`NodeClass`/`Classify` — SPEC-DRAFT.md:348-350) | Fused-sink + lazy-view parity, per retired member | — |
| N11 | Lock violations (4, 15, 12, 6) | TW0 W0.3/4/5/6 (+TW2 Lock 6) | GW0.d/GW1/GW0.b/GW0.c(+GW2a) (SPEC.md:71) | Lock 4 → **TB1.W1**; Lock 15+target-cpu → **TB2.W0**; Lock 12 → **TB1.W2**; Lock 6 → **TB3.W1** | Per-lock falsifier fires then reads green | Four sub-owners, split as in SK-V22 |
| N12 | Inline `#[cfg(test)]` in src/ | TW0 W0.7 | GW1, census corrected 75/34 (SPEC.md:71) | **TB2.W1** | Zero `#[cfg(test)]` under src/ | — |
| N13 | Unused language surface (closures/GrammarCall/@host) | TW5 (@host) + TW6 (rest) | GW5 + GW6 (SPEC.md:73) | **TB4.W1** (@host land-or-delete — ledger item 12) + **TB6.W3** (residual closure surface, land-or-formally-retire) | Host-signature falsifier gate (TB4) + feature-fold close (TB6) | — |
| N14 | CSS depth closure (3 tape kinds) | TW3 | GW4 (SPEC.md:74) | **TB4.W3** (N4 W3C/MDN typed CSS catalog + mandatory `var()`/Unparsed hatch) | Full-depth CSS oracle green | — |
| N15 | Full-tree-touch bench plane absent | TW0 W0.11 | Folded into truth-floor session (SPEC.md:75) | **CLOSED** — delivered by sk-v22 research/04 + SK-V23 §0 standing law | n/a | — |
| N16 | MASTER-PLAN receiver-row drift | TW0 W0.8 | GW0.e, SUPERSEDED (SPEC.md:72) | **CLOSED — moot**: plan corpus archives outright (TB1.W2); durable docs replace it (TB1.W4) | n/a | — |
| N17 | `bbnf-path` registry promise (JSON sidecar) | TW3 | GW4 (SPEC.md:77) | **TB4.W2** ("the bbnf-path cursor alphabet REBUILT natively" — SPEC-DRAFT.md:345) | TB4 close gate | — |
| N18 | Empty-husk crates | TW0 W0.3/0.6 + TW1a/TW2 | GW0.d/GW0.c/(TW1a superseded)/GW2a (SPEC.md:78) | **TB1.W2** (removals) + **TB3.W0** (topology, regen host) | TB1 close gate + TB3 in-tree gate | — |

---

## §4 — Dig resurrections N19-N27

| # | Item | SK-V21 wave | SK-V22 GW | SK-V23 owner | Gate | Note |
|---|---|---|---|---|---|---|
| N19 | AoS-beats-SoA vs C1 | TW1b | GW1 (SPEC.md §1 C1 row) | **CLOSED — historical**; carried as settled fact ("Runtime tape (AoS) CARRY" — SPEC-DRAFT.md:99) | = C1 gate | — |
| N20 | Structural bitmap pre-scan (twice-buried); the door | TW1c (delete) + cost-table door | GW1 + future cost-table | **TB2.W4** (delete, grep-zero retired names) + **SOFT ORPHAN on the door**: N20's specific "may return only via a measured win through the §5 cost table" clause is never explicitly re-stated as binding on TB5.W1's KernelCostTable in SPEC-DRAFT.md — TB5.W1 is the plausible mechanism but the row is not named there | grep-zero (delete half) | **Flagged below** — the revival-door half is undocumented in SK-V23, distinct from N20's CSS-specific cousin (item 19 in §6, which DOES land explicitly at TB5.W3) |
| N21 | Y.13 consumer-invariant gate template | TW0 W0.2 (adoption) → all waves | GW0 adoption, all waves (SPEC.md:66) | **CLOSED — carried verbatim** as SK-V23 §0 governing preamble ("The Y.13 consumer-invariant gate law (item 37)" — SPEC-DRAFT.md:45-48), binding every TB/PT close gate | n/a | — |
| N22 | csp-solver `node_budget` guard survival | TW0 W0.5 checkpoint / TW4.2 precondition | GW0.b / GW4 (SPEC.md:87) | **TB1.W1** (csc411 checkpoint) + **TB5.W4** (named gate: "N22 `node_budget` guard verified before any escalation" — SPEC-DRAFT.md:418-419) | Guard verified before ablation opens | Explicit named-gate carry — clean |
| N23 | Y.5 connected-components CSP decomposition | TW4.2 | GW4 (SPEC.md:88) | **TB5.W4** (named gate N23 — SPEC-DRAFT.md:419) | Ablation gate: escalate only if greedy provably differs on the real corpus | — |
| N24 | Rank-1 dead-substrate reclaim inventory | TW1c (+TW0 W0.6) | GW1 (+GW0.c) (SPEC.md:89) | **TB2.W4** (grep-zero) + **TB1.W2** (deletions) | Grep-zero at TB2 close | — |
| N25 | Idempotent-on-disk codegen rule | TW2 | GW2a (SPEC.md:90) | **TB3.W0** ("N25 idempotency" explicitly named — SPEC-DRAFT.md:286) | `regen --check` idempotency assertion | — |
| N26 | Rust-stack edition/toolchain axis | TW-S (TW-S.1-5) | GW1 (SPEC.md:76,91) | **TB2.W0** (PROTO-A3's five edits applied and committed — SPEC-DRAFT.md:251-252) | Whole-workspace pinned-stable edition-2024 build | — |
| N27 | csc411 active-upstream coordination | TW0 W0.5 | GW0.b (SPEC.md:92) | **TB1.W1** (csc411 checkpoint recorded before Lock-4 lands) | Checkpoint recorded | — |

---

## §5 — Deferred by design (authority unchanged)

| Item | Authority | SK-V21 standing | SK-V23 owner | Note |
|---|---|---|---|---|
| TS/WASM backends | Lock 5 (post-V1) | Stays deferred | **SOFT ORPHAN** — no SK-V23 wave or doc restates Lock 5's standing; nearest home is **TB1.W4**'s continuity audit (re-affirm, don't re-open) but the audit's own text (SPEC-DRAFT.md:227-231) does not name this row explicitly | **Flagged below** |
| Profile-guided specialization | deferral-audit-6 §4 | Precondition satisfied when TW4.1 lands KernelCostTable; re-examined at TW4.2 close | **TB5.W4** — explicitly named: "the §5 profile-guided-specialization re-examination trigger" (SPEC-DRAFT.md:420) | Clean carry, no orphan |
| TS path runtime (`bbnf-path-ts` full) | deferral-audit-5 §3 | Rides the WASM decision; stays deferred | **SOFT ORPHAN** — same gap as the TS/WASM row above; rides a decision (WASM) that itself has no SK-V23 restatement | **Flagged below** |

---

## §6 — Pass-loop discoveries (23 rows)

Eleven of twenty-three were CLOSED outright at SK-V21 freeze with no execution residue
(DEFERRED-LEDGER.md:139-145); those are marked CLOSED below with no SK-V23 wave action needed.
The remaining twelve carry wave-owned residue, re-homed here.

| Discovery | SK-V21 disposition | SK-V21 wave | SK-V22 GW | SK-V23 owner | Note |
|---|---|---|---|---|---|
| Presize family (Sampled) delete | DELETED for KISS | TW1c executes; TW0 W0.11 quiet-absolute leg | GW1 | **TB2.W4** | Quiet leg closed by §0 standing law |
| canada sign contradiction | SETTLED, fused loses canada | TW4.1 gate | GW3 | **TB5.W0** (headline gate: canada eager cold beats sonic) | — |
| Fold-over-lazy-cursor as default | REFUTED | CLOSED; TW3 records | GW4 | **CLOSED — historical**, carried as settled fact in TB4.W2 ("the fused eager sink stays DEFAULT" — SPEC-DRAFT.md:346) | — |
| AU floor arithmetic | CLOSED as spec edits | TW0 ledgers capture | GW0 | **CLOSED — historical**, no re-homing needed | — |
| TW1 bundling / seam self-defer | CLOSED, unbundled sound | — | GW1 restructuring | **CLOSED — moot**: TW1a/b/c itself superseded by skinny's pre-existing topology | — |
| TW3↔TW4 decode coupling | CLOSED (manifest order) | — | GW4 | **CLOSED — carried openly** in the §5 interlock graph (TB4∥TB5, TB5 "entry deps: TB4 optional-parallel — the value plane furnishes `materialize_f64`" — SPEC-DRAFT.md:388-389) | — |
| P7 phantom fork adjudication | Adjudicated, walls dissolved | TW6 (regen wiring) | GW6 in sk-v21 ledger text, but content matches "walls in-tree" which sk-v22 §1.2 explicitly folds into **GW2a** ("GW2a absorbs G1 (walls in-tree)" — sk-v22 SPEC.md:80) | **TB3.W1** (WsPolicy, leaf-regex interpreter, WART-C JSON fold — SPEC-DRAFT.md:289-295), NOT TB6 | **Genuine re-home worth flagging**: the row's literal SK-V21 wave label (TW6) does not match its eventual SK-V22/23 content-owner (GW2a→TB3.W1); a naive rename would misfile it under TB6 |
| P4 FLIP-3 movemask inconsistency | CLOSED, cell rides cost table | — | GW3 | **TB5.W1** (cost-model cell) | — |
| P4 consumer-dead cost table | Live crossover N*=3, no fleet flip | TW4.1 (liveness) + TW6 (flip) | GW3 + GW6 | **TB5.W1** (liveness) + **TB6.W2** ("this is where the TW4.1 cost-table liveness relaxation's win-clause finally BINDS" — SPEC-DRAFT.md:446-448) | — |
| P5 asm doors priced | CLOSED; both doors priced | TW0 PMU re-capture only | — | **TB5.W2** (standing discipline: vshrn obviates hand asm, "do not re-attempt" — SPEC-DRAFT.md:401-402) | — |
| P6 width destruction | Root cause corrected, fix built | TW5 step 0 re-lands | GW5 | **TB4.W1** ("the B3 width patch re-lands as one wave-item within this" — SPEC-DRAFT.md:330-331) | — |
| @host unifier synthetic-only | Explicit disposition | TW5 (@host) | GW5 | **TB4.W1** (re-homed per ledger item 12/16) | — |
| P3 stack scope / smallbox coupling (bbnf-side) | CLOSED; migration applied | TW-S executes | GW1 | **TB2.W0** | Distinct from parse-that's OWN smallbox[nightly] item (PT-R1.W1) — do not conflate |
| Nightly-vs-stable channel ratio | LOAD-CONFOUNDED | TW0 W0.11 un-confounded capture | GW1 | **TB2.W4** ("paired old-pin/new-pin cold capture LEDGERED" — SPEC-DRAFT.md:265-266) | — |
| Environmental mandate never met | Adjudicated, ratio-certified | TW0 W0.11 | SUPERSEDED-AS-AMENDED | **CLOSED** — §0 standing law | — |
| Honesty Defects A/B/C | CLOSED (v3 rescore 94) | — | — | **CLOSED — historical** | — |
| NEW-R8 TW4.1 gate contradiction | CLOSED, spec edit folded | — | GW3 + GW6 | **TB5.W1 + TB6.W2** (same coupling as the P4 cost-table row above) | — |
| NEW-R9 §4 lane phrasing | CLOSED, folded at pass-4 | — | — | **CLOSED — carried in TB4.W2's design language** (NodeClass/Classify, "two lanes, only one tape-side") | — |
| CSS SIMD tokenization severity | Ring-fenced, not TW4 scope | TW4.2 (decision point) | GW4 | **TB5.W3** (CSS tokenize-once structural index, "P2, FOLD-AMBITIOUS... research sub-wave" — SPEC-DRAFT.md:411-415) | — |
| TW0 internal breadth (~14 workstreams) | CLOSED AT FREEZE | — | — | **CLOSED — superseded** by TB1's own W0-W5 sequencing | — |
| parse-that LR/packrat GO/NO-GO | NO-GO dated 2026-07-05 | TW0 W0.10 (letter) | GW0.f | **TB1.W4** (SEAM-NOGO seam record) + **PT-R1.W0** (WDM/LR tier deletion executes) | — |
| csp-solver P0 transfer question | Does NOT transfer | TW0 W0.5 checkpoint | GW0.b | **TB1.W1** | — |
| Instruments graduating | `json_canon_bench` + checkasm-shaped parity harness | TW0 W0.11 / TW4.2 assets | GW1 / GW4 | **TB2.W2** (bench-harness shrink carries `json_canon_bench`) + **TB5** close gate (checkasm-liveness is the harness's spiritual carry-forward) | — |

---

## THE ORPHAN SUMMARY (loud — these rows have no crisp SK-V23 owner)

Three genuine gaps survive the walk, plus one scope-widening worth flagging as a re-home
rather than a pure carry:

1. **N5 — dead/lying oracle surfaces + W3C rekey (§3).** No SK-V23 tranche wave claims an
   "oracle hygiene sweep over EXISTING surfaces." TB1.W4 builds a *new* differential-oracle
   harness (~400 LOC); TB2 relocates tests; TB6.W0 guarantees every fleet grammar eventually
   HAS an oracle. None of the three is "audit the oracles that already exist and are
   dead/lying." **Recommend**: fold into TB1.W4 explicitly (it is already build-infra-first and
   already the oracle-mechanism wave) or TB6.W0 (already touches every oracle in the fleet).
2. **§5 — TS/WASM backends (Lock 5) and TS path runtime, both "deferred by design."** SK-V23's
   text never restates Lock 5's standing or the WASM decision it rides on. This is arguably
   correct behavior for a genuinely-standing deferral (nothing contradicts it, so nothing need
   re-state it) — but per the DEFERRED-LEDGER's own standing law ("no future tranche may open a
   'deferred' successor... without either citing its close evidence as stale or naming the wave
   that failed its gate," DEFERRED-LEDGER.md:153-154), TB1.W4's continuity audit should name
   BOTH rows explicitly as "authority unchanged, re-verified" rather than leaving them
   unmentioned. **Recommend**: TB1.W4's audit output names these two rows verbatim.
3. **N20's revival door (§4).** The structural-bitmap-pre-scan door ("may return only via the
   §5 cost table with a measured win") is never bound to TB5.W1's KernelCostTable in the
   SK-V23 text — unlike its CSS-specific cousin (§6's "CSS SIMD tokenization severity" row,
   which DOES land explicitly at TB5.W3). **Recommend**: TB5.W1's charter gain one clause
   naming N20 as the general-purpose door, distinct from TB5.W3's CSS-specific one, so a future
   session doesn't rediscover the twice-buried architecture a third time.

**Not an orphan, but a genuine re-home worth flagging**: the P7 phantom-fork discovery (§6)
carries a literal `TW6` label in the SK-V21 ledger, but its content (WsPolicy threading, the
leaf-regex interpreter, the WART-C JSON fold) is SK-V22-relocated to `GW2a` and therefore
SK-V23-owned by **TB3.W1**, not TB6. A mechanical rename of `TW6→TB6` would misfile this row;
TB1.W4's continuity audit should walk content, not labels, exactly as this pass did.

## Coverage accounting

70 distinct rows walked (§0: 2 grouped families / 4 ids; §1: 6; §2: 9, two dedup'd against §1;
§3: 18; §4: 9; §5: 3; §6: 23). Of these: **58 carry a crisp SK-V23 TB/PT owner** (including
rows re-affirmed CLOSED with their closing mechanism named), **4 are flagged orphans/soft-orphans**
above, and the balance are dedup pairs (§2's W2/W3 against §1's C1/C2) or pure-historical CLOSED
rows needing no action. TB1.W4's own charter cites "~40" rows for its continuity audit
(SPEC-DRAFT.md:228) — the lower figure likely counts only rows carrying ACTIVE execution
residue (this pass's §1/§3/§4/§6-residual subset, ≈45-50) rather than every table entry
including fully-closed historical rows; either count converges on the same conclusion — **no
row is orphaned outright**, three are under-specified enough to need one clarifying sentence
each in TB1.W4's audit output.
