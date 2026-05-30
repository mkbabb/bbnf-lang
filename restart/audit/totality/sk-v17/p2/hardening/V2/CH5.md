---
lens: CH5 HIDDEN-COUPLING
pass: T-P2-research (SK-V17 totality)
cycle: V2
generated_at: 2026-05-29T00:00:00Z
reviewer_method: "PASS-2-RESEARCH §3 CH5 + ORCHESTRATOR §3W. Re-read all six V2 2X dossiers (2a..2f) + their prior_cycle_dispositions_folded blocks + the V1 CH5 disposition set (hardening/V1/CH5.md) + the V1 spec surfaces (restart/ARCHITECTURE.md plane-table :1804/:1088/:1411, restart/locks/LOCKS.md Lock 1 :75/:100-127/:137-149, Lock 10 :107-108/:599, Lock 14, Lock 16 :607). Every load-bearing substrate / sidecar / classifier-state / substrate_target / retention_lifetime claim re-anchored LIVE at master HEAD 91b6893b0 via grep -n/-c + sed -n. Each of the four V1 REVISEs traced from its disposition text to its V2 fold-site to confirm SUBSTANTIVE fold, not paper-fold."
master_head: 91b6893b0b61d1c3213d02afe4ec62f22c16ae38
t_p1_locked_sha: 91b6893b0b61d1c3213d02afe4ec62f22c16ae38
scope: "substrate-union (Lock 1) preserved — tape+projection ONE substrate; tape NOT a silent 6th BackendShape (Lock 10 5-shape canon); proposes substrate-manifest category OR G-Omega-gated 6th, never silent; mask stream = transient producer not retained sidecar; Layer 0/Layer 1 clean two-layer dependency not a coupling"
first_hygiene_action: "CH1-V5-001 (enumerated-filename residual) — re-verified RESOLVED on disk at 91b6893b0 (grep -c 'collapsed_stage}.rs' 1b = 3; grep ',collapsed}' {1a,1b,1e} = 0; collapsed_stage.rs exists, collapsed_tape.rs absent). All six dossiers report it correctly in their hygiene frontmatter. No fold required."
v1_revises_folded:
  CH5-V1-001: FOLDED-SUBSTANTIVE     # PayloadArena substrate_target=existing_tape/output_row/generated_grammar — 2b:209-219, 2f:515-520, 2e:148-152, LAC-2E-SKV17-04 2e:448
  CH5-V1-002: FOLDED-SUBSTANTIVE     # sparse-flag reworded to existing_tape (not "no substrate") — 2b:311-317
  CH5-V1-003: FOLDED-SUBSTANTIVE     # live arena.rs:47 StructRegistry::compound_kind_for_layout named across all 5 fence-carriers — 2f:354-367 (owner), 2a:313-316, 2c (refuted-row + Candidate-F), 2d:213-215, 2e:83-84/:328-329
  CH5-V1-004: FOLDED-SUBSTANTIVE     # 2a-E substrate_target=existing_tape is a PRE-condition (OnceCell pending), not settled — 2a:261-269, LAC-2A-SKV17-02 2a:417
disposition_counts:
  accept: 23
  revise: 1
  reject: 0
  total: 24
accept_rate_pct: 95.8
---

# CH5 HIDDEN-COUPLING — SK-V17 T-P2 V2

## Verdict

The four V1 hidden-coupling residuals are **substantively folded** — each
traced from its V1 disposition text through to a concrete V2 fold-site that
makes the previously-implicit substrate-manifest classification EXPLICIT. The
dossier set has crossed from "coupling-clean by assertion" (V1) to
"coupling-clean by manifest" (V2). The central CH5 charges all hold at V2:

- **No silent 6th `BackendShape`.** Every "6th"/"sixth" mention across all six
  dossiers is in the negative — refuted, fenced, or G-Omega-gated (verified by
  grep: zero unguarded proposals). The 5-shape canon
  `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` is held verbatim
  (`LOCKS.md:107-108`); the tape is the substrate-manifest CATEGORY the five
  shapes project FROM (LAC-1E-14 precedent, `LOCKS.md:100-116`). 2a/2c/2d/2e/2f
  each now carry the INDEPENDENT corroborating anchor (`admits_collapsed_stage`
  x86-bound, ARCH `:1151/:1206/:1282`) added per CH6-V1-V03 — the no-6th-shape
  verdict rests on TWO grounds, not the precedent-echo alone.
- **No parallel substrate / retained sidecar.** The AoS `TapeRec` ↔ SoA `Tape`
  dual is named a *transient fold-state only*, with the dual end-state explicitly
  flagged a Lock-1 violation (`2f:81`, `2a:112`, `2e:407`). The eager `OpenFrame`
  `Vec<OpenFrame>::clone` 86.07% pathology is the DELETION target. The mask stream
  is a transient producer (ARCH plane (4) `:1804`, re-verified live). The fact-stream
  is fenced oracle-only (`admitted_fact_output`, "NOT re-admitted into the one tape",
  `2b:196-202`).
- **No cross-call classifier state.** The Lock 1 v+1 ELEVATION
  (`LOCKS.md:137-149`, "no cross-call retained classifier state. Period.") is
  honoured; the classifier declares `retention_lifetime=transient-single-call`
  post-wiring; the `depth_carry` is init-0-per-parse, NEVER retained across calls.
- **Layer 0 / Layer 1 clean two-layer dependency.** 2b keeps the dependency
  one-directional; `CollapsedStage` spine x86-pinned + aarch64-refused; no hidden
  x86 close-path coupling.

The lens finds **one NEW residual** — a manifest-vocabulary-precision drift in
2b's FOLD-L5/L6 `retention:` prose labels — of exactly the
declaration-completeness class as the four (now-folded) V1 REVISEs. It is REVISE,
not REJECT: the prose body is coupling-correct; only the manifest token must align
with the Lock 1 v+1 enum so T-P3 cannot mis-read the lifetime.

§3 expects convergence toward all-ACCEPT by V2+; this lens returns **95.8% ACCEPT
on the CH5 axis** (1 REVISE of 24), clearing the ≥95% bar for this cycle on this
lens. The single REVISE is real and load-bearing for manifest-vocabulary
consistency.

---

## V1 REVISE fold-verification (the four traced to source)

| V1 finding | V2 fold-site (verified) | substantive? |
|---|---|---|
| **CH5-V1-001** PayloadArena substrate_target UNDECLARED | `2b:209-219` (FOLD-L3 dedicated "PayloadArena substrate declaration" para): `substrate_target=existing_tape` / `retention_lifetime=output_row` / `policy_owner=generated_grammar`, bounded by `PayloadArena.write_count==0`-on-re-readable-leaves. Mirrored `2f:515-520` (Defended-#2), `2e:148-152`, codified as `LAC-2E-SKV17-04` (`2e:448`). | YES — full four-token classification + the bound; "Implicit retention here would be the parallel-substrate hole Lock 1 forbids; the explicit declaration closes it." |
| **CH5-V1-002** sparse-flag "adds no substrate" mis-frame | `2b:311-317` (FOLD-L8 Lock-surface): reworded to "PRE-EXISTS and is part of the one substrate: `substrate_target=existing_tape` … `retention_lifetime=output_row` … this is not 'no substrate'; it adds no SECOND substrate." | YES — the exact reframe the V1 fix specified (part-of-one-substrate, not no-substrate). |
| **CH5-V1-003** fence names abstract lookup, not LIVE site (shared ×5) | Owner `2f:354-367` (F6 "Live coupling-site" para): names `crates/core/src/runtime/bbnf/arena.rs:47` `StructRegistry::compound_kind_for_layout(layout)`, states FOLD-B severs it, contrasts fence-clean `begin_compound` (grep-zero `StructRegistry`). Cross-ref'd `2a:313-316`, `2d:213-215`, `2e:83-84`/`:328-329`, `2c` (refuted-row :80 + Candidate-F :234). Live anchor re-verified: `arena.rs:47` present; `tape/mod.rs` `StructRegistry` grep = 0. | YES — present-tense wire named at every carrier; the fence is now falsifiable ("this present lookup is the coupling the fold removes"). |
| **CH5-V1-004** 2a-E `existing_tape` settled-vs-pending | `2a:261-269` ("Substrate_target is a PRE-condition, not settled"): the index is `existing_tape` ONLY after F7/U-2E-02 classify-before-wiring; TODAY it is retained `OnceCell<StructuralIndex>` (`json.rs:701`, re-verified) → `local_temp_only` (pending). Codified `LAC-2A-SKV17-02` (`2a:417`): `substrate_target` "RESOLVES to `existing_tape` only AFTER the F7/U-2E-02 OnceCell classify-before-wiring step." | YES — the internal inconsistency with 2f-F7 / 2e-U-2E-02 is resolved; 2a-E now states the pre-condition, not the settled value. |

All four are SUBSTANTIVE folds with new prose AND a manifest/LAC token, not
frontmatter-only acknowledgements. No paper-fold.

---

## Live-anchored coupling re-verifications (V2)

| Coupling-sensitive fact | Live check at `91b6893b0` | Result |
|---|---|---|
| 5-shape canon held verbatim; 6th G-Omega-gated | `LOCKS.md:107-108`, `:599`; grep "6th\|sixth" across 2X = all-negative | CONFIRMED |
| substrate_target / retention_lifetime / policy_owner manifest values | `LOCKS.md:118-127`; `retention_lifetime ∈ {transient-single-call, retained-within-chunk, retained-across-call-boundary}` `:147-149` | CONFIRMED — these are the canonical manifest tokens (the REVISE below turns on them) |
| Mask stream = transient producer | ARCH `:1804` plane (4) "Transient producer; never a retained sidecar … structural projection IS the tape" | CONFIRMED |
| LIVE arena.rs:47 coupling-site | `crates/core/src/runtime/bbnf/arena.rs:47` `match StructRegistry::compound_kind_for_layout(layout)` | CONFIRMED — present-tense wire FOLD-B severs |
| Tape path fence-clean | `tape/mod.rs:185-186` `begin_compound` reads `layout.rule_id & 0x1F`; `grep StructRegistry tape/mod.rs` = 0 | CONFIRMED |
| `OnceCell<StructuralIndex>` LIVE retained | `crates/core/src/grammar/generated/json.rs:701`-region (doc `:686`) | CONFIRMED — the F7 retention-site |
| SoA `Tape` retains `payloads: PayloadArena` + sparse `flag_cursors/flag_values` | `skinny/crates/runtime/src/tape/mod.rs:97-99` | CONFIRMED — the two members CH5-V1-001/002 classify |
| Lock 1 v+1 no-cross-call-carry | `LOCKS.md:137-149` | CONFIRMED |

---

## §3W lens-axis dispositions (V2, per dossier section)

### 2a — SOTA-landscape
| § / candidate | disposition | rationale (CH5 axis) |
|---|---|---|
| FOLD-2A-A flat-tape one-encoding | ACCEPT | AoS/SoA dual admissible "ONLY as transient fold-state" (`2a:112`); end-state = Lock-1 violation. No parallel substrate. |
| FOLD-2A-B eager OpenFrame retirement | ACCEPT | Deletes the 86.07% `Vec<OpenFrame>::clone` substrate; retirement REDUCES coupling. |
| FOLD-2A-C lazy `ValueRef<G>` plane | ACCEPT | View rides the one tape; PayloadArena now declared (CH5-V1-001 folded). |
| FOLD-2A-D tape = substrate-manifest not 6th shape | ACCEPT | Explicit "PROPOSE, do not silently add"; LAC-1E-14 precedent + independent `admits_collapsed_stage` corroborating anchor (`2a:205`). |
| FOLD-2A-E NEON classifier (was V1 REVISE CH5-V1-004) | ACCEPT | FOLDED: `substrate_target=existing_tape` is now stated as a PRE-condition resolved only after OnceCell classification (`2a:261-269`); internal inconsistency with 2f-F7/2e closed. |
| FOLD-2A-F StructRegistry fence (was V1 REVISE CH5-V1-003) | ACCEPT | FOLDED: names live `arena.rs:47` site (`2a:313-316`); fence now severs a present wire, not a hypothetical. |

### 2b — Primitive-vocabulary
| § / candidate | disposition | rationale (CH5 axis) |
|---|---|---|
| A1 two-layer vocabulary (Layer 0 vendored / Layer 1 Rust) | ACCEPT | One-directional dependency; realization-medium reconcile, not a coupling. |
| A4 `Vec<u32>` index IS the tape, no sidecar | ACCEPT | Transient-producer discipline; index == tape offsets. |
| FOLD-L2 `push_plain_offset` + fact-stream oracle fence | ACCEPT | Fact-stream fenced `admitted_fact_output`, "NOT re-admitted into the one tape" (`2b:196-202`); tape-append is `existing_tape`. Coupling-clean. |
| FOLD-L3 PayloadArena (was V1 REVISE CH5-V1-001) | ACCEPT | FOLDED: full four-token manifest classification + write_count bound (`2b:209-219`). |
| FOLD-L4 tokenize-once shared-scan | ACCEPT | Index == tape offsets OR `local_temp_only`; REDRESS-53 pre-block, scoped all 8 carriers. |
| **FOLD-L5 `comment_body_mask_64` / FOLD-L6 `bracket_depth_mask_64`** | **REVISE** (CH5-V2-001) | The fold BODY is coupling-correct (L6 `depth_carry` "init-0-per-parse, NEVER retained across calls" = `transient-single-call`; L5 region-fill within one block sequence). BUT the Lock-16 manifest surfaces carry the PROSE labels `retention: within-block-only` (`2b:262`) and `retention: within-call-only` (`2b:277`) — NEITHER is a canonical Lock 1 v+1 `retention_lifetime` enum value (`LOCKS.md:147-149`: `{transient-single-call, retained-within-chunk, retained-across-call-boundary}`). A manifest row labelled "within-block-only" could read to T-P3 as `retained-within-chunk` (an admissible-but-DIFFERENT lifetime) when the body asserts `transient-single-call`. 2b itself cites the canonical vocabulary correctly elsewhere (`2b:202` `existing_tape`; `2b:202`-region L9 `retained-across-call-boundary` REJECT class) — so this is local drift, not a coupling, but it weakens the manifest's falsifiability. **Fix:** in FOLD-L5 and FOLD-L6 Lock-16 surfaces, replace the prose `retention: within-block-only`/`within-call-only` with the canonical token `retention_lifetime=transient-single-call` (both are single-call-bounded per their own bodies), keeping the "within block sequence" / "within `scan_components_to_index` call" prose as the descriptive gloss. This makes the manifest token match the body and aligns with the L1-classifier row's `transient-single-call` (`2a:417`). |
| FOLD-L7 `OneShotSimd` capacity | ACCEPT | Sizes the EXISTING `offsets`; no second vector. |
| FOLD-L8 sparse-flag (was V1 REVISE CH5-V1-002) | ACCEPT | FOLDED: reworded to `existing_tape` member, "not 'no substrate'; adds no SECOND substrate" (`2b:311-317`). |
| FOLD-L9 Alt-mode (DEFERRED appendix) | ACCEPT | Recorded-not-shortlisted; rides D3 O(1) `offsets.len()` checkpoint / `truncate` rollback — no `split_off`, no `Vec<Vec>`, no new substrate. Demotion (CH4) keeps it out of the wired enumeration. |
| FSM/frame-stack + udot-CSS refuted | ACCEPT | Filed `architectural-block-with-REDRESS`; CollapsedStage x86-pinned + aarch64-refused. No orphan coupling. |

### 2c — Grammar-neutrality
| § / candidate | disposition | rationale (CH5 axis) |
|---|---|---|
| SK17-2C-A flat-tape grammar-neutral substrate | ACCEPT | Grammar-column-free; sparse flags only; bars the AV.04 dense class column. |
| SK17-2C-B OpenFrame retirement | ACCEPT | Reduces coupling; no-delete-before-replacement fence. |
| SK17-2C-C lazy `ValueRef<G>` | ACCEPT | Projection over the one tape; no second value tree. |
| SK17-2C-D tape = substrate-manifest not 6th shape | ACCEPT | "any proposal that reads as a 6th `BackendShape` is REJECT (no silent shape)" (`2c:187`); CH1-2C-01 ARCH:1803 ordinal disambiguation folded. |
| SK17-2C-E NEON classifier | ACCEPT | "no retained cross-call classifier state (Lock 1 v+1) — alphabet per-call constructed." CH2-V1-R4 eq-set-fan binding folded. |
| SK17-2C-F FieldSource fence (shared CH5-V1-003) | ACCEPT | FOLDED via the shared 2f-F6 owner; 2c refuted-row (`2c:80`) + Candidate-F (`2c:234`) name the runtime `StructRegistry::layout` walk as the refuted indirection; the live-site naming is owned by 2f and cross-referenced. |
| SK17-2C-ONBOARD future-grammar gate | ACCEPT | Reclassified as a verify_action with a live HEAD baseline (CH6-V1-V01 folded); a generic-crate grammar branch = coupling = FAIL. |

### 2d — Cost-model + 5-shape
| § / candidate | disposition | rationale (CH5 axis) |
|---|---|---|
| FOLD-2D-01 tape = substrate-manifest not 6th shape | ACCEPT | Exact LAC-1E-14 reuse; 5-shape domain held verbatim. |
| FOLD-2D-02 cost selects per-rule INTO the one tape | ACCEPT | "a shape selecting a NEW substrate is a CSP-INFEASIBLE plan"; e-graph rejects non-admitted `substrate_target`. CollapsedStage/FSM keeps the mask stream a transient producer — the CH5 §3W requirement on 2D. |
| FOLD-2D-03 lazy `ValueRef<G>` plane | ACCEPT | Read strategy over a retaining shape; not a shape. |
| FOLD-2D-04 AoS↔SoA single-encoding | ACCEPT | Dual end-state = Lock-1 violation. |
| FOLD-2D-05 NEON classifier scan-cost | ACCEPT | Never retains across calls; transient `Vec<u32>`. |
| FOLD-2D-06 FieldSource fence (shared CH5-V1-003) | ACCEPT | FOLDED: names live `arena.rs:47` site (`2d:213-215`); fence now present-tense-anchored. |
| FOLD-2D-07 aarch64 CollapsedStage UNKNOWN-2D-05 | ACCEPT | NEON under four LLVM shapes' scan-leaf FFI; no aarch64 CollapsedStage; x86 mechanically refused. |

### 2e — Host-arch esoterica
| § / candidate | disposition | rationale (CH5 axis) |
|---|---|---|
| FOLD-2E-A..F (six folds) | ACCEPT | Each substrate-coupling claim re-anchored; "Fold Coherence Note (CH5 hidden-coupling pre-empt)" persists (`2e:450-458`): folds co-dependent-not-orthogonal, "No fold implies a parallel substrate, a sidecar producer, or a Lock-1 violation." |
| FOLD-2E-B PayloadArena (was V1 REVISE CH5-V1-001) | ACCEPT | FOLDED: `payloads: PayloadArena` declared `existing_tape`/`output_row`/`generated_grammar` (`2e:148-152`), codified LAC-2E-SKV17-04. |
| FOLD-2E-F fence (was V1 REVISE CH5-V1-003) | ACCEPT | FOLDED: live `arena.rs:47` named (`2e:83-84`, `:328-329`); FOLD-B deletion severs it. |
| CH7-001 fabricated-claim fold | ACCEPT (CH5-adjacent) | The deletion of "recognizer beats lightningcss 2-3x" is a CH7/CH1 concern, not CH5; noted folded — no coupling implication either way. |

### 2f — Fold-gaps
| § / candidate | disposition | rationale (CH5 axis) |
|---|---|---|
| F1 OpenFrame retirement / F3 AoS↔SoA / F4 tape-substrate-not-6th | ACCEPT | F4 primary D-refutation; "propose, do NOT silently add a 6th" discharged in the negative; independent `admits_collapsed_stage` anchor (CH6-V1-V03 folded). |
| F2 lazy `ValueRef<G>` + PayloadArena (was CH5-V1-001) | ACCEPT | FOLDED: Defended-#2 declares PayloadArena `existing_tape`/`output_row`/`generated_grammar` (`2f:515-520`). |
| F5 NEON classifier manifest row | ACCEPT | `retention_lifetime=transient-single-call`, `substrate_target=existing_tape`, same-wave consumer = the tape (`2f:295-296`). Uses the CANONICAL token (contrast the L5/L6 drift). CH1-2F-01 alphabet-as-data re-anchor folded. |
| F6 fence (OWNER of shared CH5-V1-003) | ACCEPT | FOLDED: names `arena.rs:47` `StructRegistry::compound_kind_for_layout`, states FOLD-B severs it, contrasts fence-clean `begin_compound` grep-zero `StructRegistry` (`2f:354-367`). The fence is now falsifiable. |
| F7 OnceCell classification (all 8 carriers) | ACCEPT | Names live `OnceCell<StructuralIndex>` retention; classify-before-wiring; same-wave consumer co-waved with F1/F3 tape-wiring (CH4-2f-001 folded). The best CH5 row in the set. |
| F8 BackendShape selector wiring | ACCEPT | Selector outputs a projection mode, never a substrate; `substrate_target` binding on every `BackendExpr`. |
| F9 StructLayout rename | ACCEPT | Identifier reconcile; CH6-V1-V02 bounded-sizing route folded; grammar-blind; no coupling. |
| LAC-2F-FOLD-01..05 | ACCEPT | LAC-2F-FOLD-03 (classifier manifest row) uses canonical `substrate_target=existing_tape`/`retention_lifetime=transient-single-call`. No LAC proposes a parallel substrate or 6th shape. |

---

## The one hidden-coupling residual (REVISE, with concrete fix)

### CH5-V2-001 — FOLD-L5/L6 `retention:` prose labels are not canonical Lock 1 v+1 `retention_lifetime` enum values
**Where.** `2b:262` (FOLD-L5 Lock-16 surface) `retention: within-block-only`;
`2b:277` (FOLD-L6 Lock-16 surface) `retention: within-call-only`.
**Coupling (declaration-precision, not a live coupling).** The fold BODIES are
coupling-correct — L6's `depth_carry` is "threaded WITHIN a single
`scan_components_to_index` call, init-0-per-parse, NEVER retained across calls"
(`2b:268-269`); L5's region fill is within one block sequence. Both are
single-call-bounded, i.e. `transient-single-call` under the Lock 1 v+1 enum
(`LOCKS.md:147-149`: `{transient-single-call, retained-within-chunk,
retained-across-call-boundary}`). But the MANIFEST tokens "within-block-only" /
"within-call-only" are not enum members. A manifest row labelled "within-block-only"
admits a mis-read as `retained-within-chunk` — an *admissible-but-different*
lifetime that would NOT trip the Lock 1 v+1 REJECT test, weakening the
manifest's falsifiability. This is the same declaration-completeness class as
the four folded V1 REVISEs: the body is right; the manifest token must be made
canonical so T-P3 cannot inherit an ambiguously-classified retention.
**Why it is REVISE not REJECT.** No live cross-call retention is proposed (the
bodies forbid it); the only defect is the manifest token. 2b's sibling rows use
the canonical vocabulary correctly (FOLD-L9 cites `retained-across-call-boundary`
as the REJECT class at `2b:202`-region; F5/LAC-2F-FOLD-03 use
`transient-single-call`), so this is local drift.
**Fix (one edit per row, owned by 2b-L5/L6).** Replace `retention: within-block-only`
(L5) and `retention: within-call-only` (L6) with `retention_lifetime =
transient-single-call`, keeping the descriptive gloss ("within one block
sequence" / "within a single `scan_components_to_index` call") as prose. This
makes the manifest token match the body and aligns FOLD-L5/L6 with the
classifier row's `transient-single-call` (`2a:417`, `2f:296`).

---

## What the lens did NOT find (the strong-coupling axes that PASS at V2)

- **No silent 6th `BackendShape`.** Every D-fold proposes the substrate-manifest
  CATEGORY explicitly (LAC-1E-14) + an independent `admits_collapsed_stage`
  x86-binding corroborating anchor (verdict on TWO grounds, CH6-V1-V03 folded
  into 2a/2c/2d/2e/2f). 5-shape canon held verbatim (`LOCKS.md:107-108`).
- **No parallel substrate / sidecar.** AoS/SoA dual = transient fold-state only,
  end-state Lock-1 violation; eager OpenFrame = deletion target; mask stream =
  transient producer; fact-stream = oracle-only `admitted_fact_output`, not
  re-admitted into the tape; `Vec<u32>` index == tape offsets.
- **No cross-call classifier state.** Lock 1 v+1 ELEVATION honoured; classifier
  `transient-single-call`; `depth_carry` init-0-per-parse. (The L5/L6 manifest
  TOKEN must say so explicitly — CH5-V2-001 — but the BEHAVIOUR already conforms.)
- **PayloadArena + sparse-flag pair now declared.** Both retained tape members
  carry `substrate_target=existing_tape`/`retention_lifetime=output_row`/
  `policy_owner=generated_grammar` — part of the ONE substrate, not a second store
  (V1 CH5-V1-001/002 folded).
- **The StructRegistry fence is now falsifiable.** All five carriers name the LIVE
  `arena.rs:47` `StructRegistry::compound_kind_for_layout` wire FOLD-B severs;
  the tape path is fence-clean (`begin_compound` grep-zero `StructRegistry`).
  (V1 CH5-V1-003 folded.)
- **Layer 0 / Layer 1 clean two-layer dependency.** One-directional; CollapsedStage
  spine x86-pinned + aarch64-refused; no hidden x86 close-path coupling.
- **No re-opened REDRESS.** AZ-IV eager, StructRegistry indirection, fact-stream,
  broadcast, FNV, x86 — none re-grounded as viable.

---

## §3W disposition summary

- **ACCEPT: 23** (the six D-folds + tape-substrate category across all dossiers
  with the now-doubled corroboration; the four folded V1 REVISEs; the
  transient-producer / no-cross-call-state discipline; the OpenFrame retirement;
  F7 OnceCell classification; the Layer 0/Layer 1 cleanliness; 2e's coherence note;
  the deferred FOLD-L9 no-new-substrate property; the five 2f LACs).
- **REVISE: 1** (CH5-V2-001 — FOLD-L5/L6 `retention:` prose labels must be the
  canonical Lock 1 v+1 `retention_lifetime=transient-single-call` token).
- **REJECT: 0** (no fold implies a parallel substrate, a silent 6th shape, a
  retained sidecar, a cross-call classifier carry, or a Lock-1 violation; the fold
  direction is correct throughout; all four V1 residuals are substantively closed).
- **Accept-rate (CH5 axis): 23/24 = 95.8%.**

The single V2 REVISE is a manifest-vocabulary-precision defect — the L5/L6
retention BODIES already conform to Lock 1 v+1; only the manifest TOKEN must be
made canonical. Folding it completes the substrate manifest's lifetime-vocabulary
consistency so T-P3 inherits no ambiguously-labelled retention. The dossier set's
coupling posture is correct and now fully manifest-declared.

First hygiene action CH1-V5-001: re-verified RESOLVED on disk at `91b6893b0`; all
six dossiers report it correctly in their hygiene frontmatter; no edit required.
