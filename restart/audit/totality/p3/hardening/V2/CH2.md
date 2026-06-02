---
challenge_agent: CH2
name: GENERALITY
pass: T-P3-synthesis
cycle: V2
sk_cycle: SK-V18
verdict: REVISE
generated_at: 2026-06-01T21:10:00Z
owned_output: restart/audit/totality/p3/hardening/V2/CH2.md
---

# CH2 GENERALITY — SK-V18 T-P3 V2

Lens: CH2 GENERALITY. Subject: the 6 T-P3 synthesis artefacts under
`restart/audit/totality/p3` (3A, 3B, 3C-crystallisation + 3C-v+1-diff, 3D, 3E,
3F), audited against T-P1 evidence (`p1/`), T-P2 dossiers (`p2/`), and the V1
surfaces (`restart/ARCHITECTURE.md`, `MASTER-PLAN.md`, `locks/LOCKS.md`,
`MIGRATION.md`).

CH2 mandate: Lock 14 holds; 3A surface deltas + 3B waves generalise to non-JSON;
3E's story is concrete for CSS L4 / Sheets / BBNF-self / the 9-grammar fleet; 3C
accepts no JSON/CSS-narrowing amendment; the future-grammar onboarding test
survives. Spot-verify the most load-bearing deltas (a cited finding-id resolves;
a cited LOCKS section exists; the v+1 diff applies).

This is cycle V2. The on-disk artefacts already fold the V1/CH2 cycle (their
frontmatter records `V1-FOLD (CH2-R01)`, `V1-FOLD (CH2-R02)`, `V1-FOLD
(CH2-V1-R03)`). My charge is to re-verify those folds landed, re-run the
load-bearing spot-checks, and re-enumerate the lens dispositions against the
folded artefacts.

## Spot-verifications performed (load-bearing)

| check | result | evidence |
|---|---|---|
| **The 3C v+1 diff applies to current LOCKS.md** (V1/CH2-R01 was the binding FAIL) | **PASS — R01 FIXED** | `git apply --check` exits 0. Header `@@ -622,6 +622,33 @@`: old-side 6 = the six context lines (Lock 16 clause + blanks `:623`,`:624` + the `## v+1 Governance Boundary` region); new-side 33 = 6 context + 27 added. Body carries 6 ` ` context + 27 `+`. LOCKS.md `:623`/`:624` are both present as blanks; the addendum lands between blank `:624` and the heading `:625` with no compounded blank. |
| `measurement-valid` un-caveated closure word (V1/CH2-R03) | **PASS — R03 FIXED** | Every `measurement-valid` hit in 3B is now either the V1-FOLD note or the **negated** "do NOT carry … MEASUREMENT-VALID"; the positive MP-3B-SKV18-D10 verdict reads "CSS >SOTA is directionally-valid pending the H1 `css_canon_bench` re-lock; the overfit is IMPLEMENTATION (forks/replicas), not measurement." (`3B:200`,`:219`). |
| FORBIDDEN-token-set canonicalisation (V1/CH2-R02) | **PARTIAL — R02 NOT closed** | The alias-immune `*EventGrammar` glob (the load-bearing ask) now appears in ALL surfaces. BUT two **different base token sets** survive: `{CSS_,_RS,EventGrammar,*EventGrammar}` (3A:214, 3C-cryst:87, 3C-diff:71, 3E:334) vs `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` (3B:149, 3D:89). Two governance surfaces still specify two literally-different token lists for the same P4 fix. See CH2-V2-R01. |
| Cited finding-id resolves (1E:147 LAC-1E-V5-01 named-primitive (a)-(d) gate) | PASS | `1E:147` is the (a)-(d) gate row verbatim. |
| Cited finding-id resolves (1E:148 LAC-1E-V5-02 relocated-seam firewall) | PASS | `1E:148` `emit_shape_source==lowered_program`, md5 necessary-NOT-sufficient, PLANNED `runtime_target_rows_collapsed` co-gate. |
| Cited finding-id resolves (1E:152 LAC-1E-V5-06 green-by-exclusion) | PASS | `1E:152` writes `FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_,_RS,EventGrammar,*EventGrammar}` — the canonical form 3A/3C/3E carry, NOT 3B/3D's. |
| Cited finding-id resolves (2C:380-382 LAC-2C-SK18-01/02/03) | PASS | `2C:380` FORCED-demotion + inner-kernel-may-stay-neutral, `:381` fleet-scoped (3-witnessed → SK-V19), `:382` totality-tree 9-ident row-collapse + RED self-gate. |
| Cited finding-id resolves (2C:313 tree-walk-regresses refute) | PASS | "A generic IR tree-walk lowering can preserve the 94.1% CSS scan | refuted" — the correct grounding for the (a)-(d) escape. |
| Cited LOCKS section exists (Lock 14 @ 603/620; SK-V17 addendum 610-622; boundary 625) | PASS | LOCKS:620 carries the phantom-`<G>` "the `G:EventGrammar` type parameter is the generality vehicle" clause 3A-D01/3C-L01 strike; SK-V17 addendum 610-622; governance boundary 625. |
| Cited live code resolves (ARCH:1998 phantom `<G>` vehicle) | PASS | `ARCHITECTURE.md:1998` "type parameter is the generality vehicle; `@generated` per-grammar emission keeps". |
| Cited live code resolves (lower/mod.rs:18-24 5-shape select_lowering) | PASS | exactly 5 `BackendShape` arms dispatching on `cost.chosen` (the grammar-NEUTRAL cost axis, not a grammar tag). |
| Cited live code resolves (strategy.rs:137-185 9-ident leak) | PASS | 9 `idents:` rows (137-185); `:216` `for_grammar_with_manifest`. |
| 9-grammar fleet roster (3E-D16) matches reality | PASS | strategy.rs 9 idents + `crates/core/src/grammar/generated/*.rs` 9 files = {bbnf,bnf,css_l4,css_pretty,csv,ebnf,google_sheets,json,math}. |
| Verbatim-blob courier exists (3A-D05 / 3C-L06 / 3E L14-HC-14) | PASS | `runtime_generator.rs:701 const CSS_GENERATED_RS: &str = r#"`. |
| 16-lock count + 71-file Pattern-H recensus baseline | PASS | `grep -cE '^[0-9]+\. \*\*' LOCKS.md` = 16; `find crates/core/src/runtime -mindepth 2 … | wc -l` = 71 (the recensus baseline 3C-L13/3A-D12/3E-CH4 correctly cite vs the absolute-67 SK-V15 baseline). |

The two binding V1/CH2 defects (R01 non-applying diff, R03 un-caveated closure
word) are CLOSED and re-verified clean. The third (R02 token-set divergence) is
REDUCED — the alias-immune `*EventGrammar` glob, the load-bearing element, is now
universal — but NOT closed: a residual cross-document base-token divergence
survives across {3A,3C,3E,1E:152} vs {3B,3D,SK-V18-SPEC}. Every other
generality-bearing finding-id, LOCKS section, and live-code anchor I sampled
resolves exactly.

## Disposition enumeration (CH2 lens)

### 3A architecture (SK-V18 generality-bearing deltas)

| delta | verdict | basis |
|---|---|---|
| ARCH-3A-V4-SK18-D01 phantom `<G>` strike + re-anchor on Cursor trait + config-breadth classifier | ACCEPT | Re-anchors generality onto two axes the LOCKS:620 clause already names; `1A-SUB-023` census EMPTY of non-test `<G>`; ARCH:1998 anchor verified; no narrowing. |
| ARCH-3A-V4-SK18-D02 named-primitive (a)-(d) ARCHITECTURE-authoritative | ACCEPT | Keeps the 94.1% scan grammar-PARAMETERISED; refuted-tree-walk (2C:313) is the correct grounding; G1∧G2, not CSS-only. |
| ARCH-3A-V4-SK18-D03 un-fork: DELETE `RuntimeEmitterKind`, dispatch on `BackendShape` | ACCEPT | Dispatch on `cost.chosen` (lower/mod.rs:18 verified) — the cost-derived grammar-neutral axis; this is the generality backbone, not a JSON path. |
| ARCH-3A-V4-SK18-D04 relocated-seam firewall `emit_shape_source==lowered_program` + CSS second seam | ACCEPT | The grep-evasive neutrality falsifier; the CSS second seam `css_provider_source==generated` extends generality to the CSS provider channel. |
| ARCH-3A-V4-SK18-D05 verbatim-blob-courier prohibition | ACCEPT | Courier verified at runtime_generator.rs:701; bars a hand-written CSS `&str` from being credited as grammar-derived. |
| ARCH-3A-V4-SK18-D06 5-shape canon RE-FRAMED as POSITIVE neutral dispatch axis | ACCEPT | The strongest generality affirmation: JSON=SinkOnly, CSS=lowered scan IR, Sheets tower→same SinkOnlyExpr vocab; a sixth shape = overfit. Coherent with 3E-D16 / 3B-D05. |
| ARCH-3A-V4-SK18-D09 G6 retarget-not-author + neutral-inner-kernel + single-movemask | ACCEPT | Inner eq-set kernel carries byte-set as CALLER DATA (neutral) even under a CSS-scoped shell — the correct generality split. |
| ARCH-3A-V4-SK18-D10 CSS-scoped `css_balanced_component_scan` FORCED demotion | ACCEPT | Honest discharge of a one-grammar neutral name; base one-fan kernel stays neutral independently; no fabricated cross-grammar caller (3E-D14 confirms the two named non-CSS dischargers are parse-with-emit, structurally incompatible). |
| ARCH-3A-V4-SK18-D11 totality 9-ident leak + `css_types.rs` + green-by-exclusion fix | REVISE | Generality-correct (9-ident leak verified at strategy.rs:137-185; self-gate RED; D11a/D11b cost split honest). BUT D11 carries the `{CSS_,_RS,EventGrammar,*EventGrammar}` token set, which DIVERGES from 3B-P4 / 3D's `{GENERATED_RS,CSS_GENERATED_RS,…}`. The V1/CH2-R02 "identical across surfaces" correction is not met. See CH2-V2-R01. |
| ARCH-3A-V4-SK18-D13 Sheets precedence-tower negative control (by-exercise upgrade) | ACCEPT | Upgrades Sheets from by-construction to by-exercise; tower lowers to existing `SinkOnlyExpr` (no new IR); scoped, no fleet wording until SK-V19. The make-or-break generality stressor. |
| ARCH-3A-V4-SK18-D14 SK-V18 authority + HANDOFF-scope reconcile | ACCEPT | Routes the scope pivot (SK-V18=skinny generalization, SK-V19=crates/core fold); prevents fleet-wide misreading. |

(D07 aarch64-only, D08 CollapsedStage slot, D12 metalang+Pattern-H are
host/shape/census deltas tangential to the generality lens; CH2 takes no
position — owned by CH3/CH4/CH7.)

### 3B master-plan (waves generalise to non-JSON)

| delta | verdict | basis |
|---|---|---|
| MP-3B-SKV18-D01..D03 scope pivot + §13.7 12-wave GENERALIZATION block | ACCEPT | Waves map G1(JSON SinkOnly)→G2(CSS lowering, `CSS_GENERATED_RS` DELETED)→G3(un-fork dispatch on `BackendShape`)→PROVE(Sheets) — generalise beyond JSON with same-wave consumers and RED-exit falsifiers (3B:151-152). |
| MP-3B-SKV18-D04 P-cluster routing (P4-before-G2/G3 hard order) | ACCEPT | Green-by-exclusion gate fixed BEFORE the emitter is neutrality-scanned-as-authored; correct sequencing for generality (3B:149). |
| MP-3B-SKV18-D05 G-cluster (a)-(d) gate + 5-conjunct G3 un-fork exit | ACCEPT | `css_balanced_component_scan` FORCED-demoted; the un-fork reads `BackendShape` not a grammar tag — generality preserved per-wave (3B:195). |
| MP-3B-SKV18-D06 PROVE Sheets + H1 with BINDING FALLBACK `N` | ACCEPT | A Sheets shim ⇒ `N` (generalization NOT real), surfaced honestly, never paper-closed — the anti-narrowing fallback (3D:92 cross-ref). |
| MP-3B-SKV18-D07 SK-V19 totality-fold tee-up (9-ident leak, css_types, scanner asymmetry) | ACCEPT | The totality-tree leaks DEFERRED to SK-V19, not bolted into SK-V18; F.W5 "nine seed grammars" held as the SK-V18→SK-V19 obligation (3B:110). |
| MP-3B-SKV18-D08/D09 §25 order + F.W5 un-fork (3 grammars SK-V18 / 9 SK-V19) | ACCEPT | F.W5 explicitly NOT read as already-satisfied; the "nine seed grammars" claim is the SK-V18→SK-V19 obligation — fleet-scoping honest. |
| MP-3B-SKV18-D10 CSS verdict UPGRADED (directionally-valid pending H1) | ACCEPT (was REVISE in V1/CH2-R03) | The bare "measurement-valid" is gone; the verdict now reads "directionally-valid pending the H1 `css_canon_bench` re-lock; the overfit is implementation (forks/replicas), not measurement" (3B:200,:219). R03 closed. |
| MP-3B-SKV18-D04 / §13.7 P4 row FORBIDDEN token set | REVISE | The 3B P4 row writes `FORBIDDEN ⊇ {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` (3B:149) — the SK-V18-SPEC form, NOT 3A/3C/3E's 1E:152-sourced `{CSS_,_RS,…}` form. Two surfaces, two gates, for the one P4 fix. See CH2-V2-R01. |

### 3C LOCKS (no JSON/CSS-narrowing amendment; Lock 14 holds)

| disposition / clause | verdict | basis |
|---|---|---|
| D-SKV18-L14-named-primitive-gate (LAC-1E-V5-01 ACCEPT) | ACCEPT | Binds the (a)-(d) discipline; keeps the hot kernel grammar-PARAMETERISED — the inverse of JSON narrowing. |
| D-SKV18-L05-L10-unfork (LAC-1E-V5-02, 2D-V3-01/02 ACCEPT) | ACCEPT | `render(program)` dispatches on `backend_shape`; dispatch-on-source-family is REJECT — generality is the lock, not a narrowing. |
| D-SKV18-L14-neutrality-proof (LAC-1E-V5-03, 2C-SK18-01/02 ACCEPT/MODIFY) | ACCEPT | Forced-demotion + fleet-scoping in one clause; inner kernel may stay neutral; explicitly NO fleet-wide wording on <full-roster witness. |
| D-SKV18-L14-green-by-exclusion (LAC-1E-V5-06 ACCEPT) | REVISE | The clause carries `FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_,_RS,EventGrammar,*EventGrammar}` (3C-cryst:87 / 3C-diff:71) and self-claims "canonical across 3A-D11/3B-P4/the v+1 diff per CH2-V1-R02" — but 3B-P4 (and 3D) actually carry the `{GENERATED_RS,CSS_GENERATED_RS,…}` form. The "canonical" self-claim is false on the 3B leg. See CH2-V2-R01. |
| D-SKV18-L13-pattern-h-recensus (LAC-1E-V5-07, 2C-SK18-03 MODIFY) | ACCEPT | The 9-ident totality leak (verified) routed to STRUCTURAL full-row collapse over all 9 + widened regex at SK-V19; no narrowing; 71-file recensus baseline verified. |
| D-SKV18-L01-cursor-generality (1A-LOCK1-AMEND-001, 2D-V3-03 MODIFY) | ACCEPT | Strikes the phantom `<G>` vehicle, re-anchors on Cursor trait + config-breadth classifier (8/9 grammars); e-graph ≥1-rewrite guard verified live (`passes/src/backend_egraph.rs:191`-`193`). Generality survives the `<G>` delete. |
| L14-HC-07 / V4 onboarding-axes future-grammar test | ACCEPT | Source/metadata-only; Sheets/BBNF-self fail on a generic branch / new directive / sixth shape (3E V4 onboarding table). The onboarding test SURVIVES intact. |
| No JSON/CSS-narrowing amendment introduced | CONFIRMED | The live Lock 14 clause (LOCKS:603/620) and all 11 SK-V18 clauses bind generality OUTWARD (provider manifests, 9-grammar matrix, Sheets/BBNF-self receivers). Zero clause narrows the lock to JSON or CSS. |
| 21/21 candidates disposed, 0 silent drops | CONFIRMED | Disposition matrix accounts for 1E×7, 1A×1, 2C×3, 2D×4, 2E×3, 2F×3; the DEFER (LAC-2F-V3-03) names its re-entry trigger and is folded as an audit-scope note. |

### 3D skinny-fold / 3F migration-handoff (generality cross-refs)

| delta | verdict | basis |
|---|---|---|
| 3D-D01 JSON-guard-scope (51/51 is same-plane PoC, not fleet closure) | ACCEPT | Explicitly bars JSON from masquerading as generality proof. |
| 3D-D06 decision-engine selection-DEPTH under Sheets tower | ACCEPT | Five-shape canon preserved; the tower depth is the open generality stressor at G3 (re-framed from scaffold-rejection, honest). |
| 3D-D09 Sheets negative-control onboarding (shim ⇒ `N`) | ACCEPT | Monotonic generalization bridge; fleet wording waits for the witness; cross-ref 3E-D07. |
| 3D-D10 PRUNE-before-REBUILD sequencing | ACCEPT | No cross-scope violation; T-P3 proposes only, dispatches no wave. |
| 3D-D04 green-by-exclusion FORBIDDEN token set | REVISE | 3D:89 carries `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` (the SK-V18-SPEC form) while citing "per CH2-V1-R02". V1/CH2 named only 3A/3B/3C in R02; 3D is the un-flagged FOURTH carrier of the divergence and aligns with 3B, not the canonical 3A/3C/3E cluster. Part of CH2-V2-R01. |
| 3F-MH-005/006 HANDOFF SK-V18=generalization scope + blocker matrix | ACCEPT | Strikes the stale "SK-V18 adopts into crates/core" definition; routes fleet onboarding to SK-V19. |
| 3F-MH-012 phantom `<G>` DELETE (G4) + LOCKS:620 reconcile | ACCEPT | K-axis preserved; generality re-anchored; LOCKS edit correctly deferred to Pass Omega CRUD. |
| 3F-MH-013 `css_types.rs` RELOCATE-or-DELETE as SK-V19 | ACCEPT | The named generic-core mess is an explicit SK-V19 decision, not silently dropped, not narrowed into SK-V18. |

### 3E grammar-generalisation (the lens-primary artefact)

| delta | verdict | basis |
|---|---|---|
| 3E-D01..D11 (carried SK-V15 generality matrix) | ACCEPT | Non-JSON proof matrix, per-grammar BackendShape matrix, primitive transfer, Lock 14 hardening clauses — concrete for CSS/Sheets/BBNF-self. |
| 3E-D12 one-generator generalisation thesis | ACCEPT | Generality is an INPUT-SURFACE property (grammar source + metadata), proven by md5-distinct output from a neutral renderer; 2C:213 grounded; md5 carried as necessary-NOT-sufficient. |
| 3E-D13 named-primitive (a)-(d) neutrality discipline | ACCEPT | (a)-(c) prove grammar-COUPLING, (d) bounds SIZE; the escape is admissible ONLY under all four. |
| 3E-D14 css_balanced_component_scan FORCED demotion | ACCEPT | Base one-fan kernel structurally neutral; two-fan/shell CSS-scoped; the JSON `{}`/`[]` and Sheets `paren_expr` dischargers are parse-with-emit, structurally incompatible — the forced name IS the discharge. |
| 3E-D15 Sheets precedence-tower negative control | ACCEPT | 7-level tower lowers to existing `SinkOnlyExpr`; no relabeled courier can fake the recursive `CallRule`/`RepeatLoop` cascade; `Nu8`-tagged-alt correctly DEMOTED from the litmus (CSS 295× vs Sheets 21×, a shared construct). |
| 3E-D16 9-grammar BackendShape fleet matrix | ACCEPT | Roster verified against strategy.rs (9 idents) + generated/ (9 files); five-shape canon preserved; CollapsedStage in no dominant/secondary cell (correct, M5-Max-aarch64); math is a SECOND precedence-tower witness corroborating Sheets. |
| 3E-D17 relocated-seam firewall + CSS second seam | ACCEPT | md5-distinct necessary-NOT-sufficient; the firewall extends generality to the CSS provider source channel; the `RuntimeTarget` data-table relocation is the grep-evasive risk it closes. |
| 3E-D18 fleet-scoped neutrality wording | ACCEPT | Anti-overclaim discipline: <full-roster witness ⇒ scoped wording; 6 remaining grammars are SK-V19 receivers. Prevents the fold being narrated as already-fleet-proven. |
| 3E V4 onboarding table line 334 FORBIDDEN token set | NOTE (not a separate REVISE) | 3E:334 carries the canonical `{CSS_,_RS,EventGrammar,*EventGrammar}` — it is on the SAME side as 3A/3C/1E:152, so it is NOT a divergence source; it reinforces the canonical cluster. Folded into CH2-V2-R01's evidence, not charged as its own defect. |

## REJECTs

None. No uncited delta; no revived refuted route (the 13 REFUTED constraints in
the 3E frontmatter — tree-walk-preserves-94.1%, find_css_significant-wire-as-is,
neutral-name-on-one-grammar, checkasm-PASS-as-speedup, x86/AVX-512-closes-a-row,
eq-set-dual-consumer, md5-distinctness-alone, bracket_depth_mask — are all
carried as REFUTED; a targeted positive-revival grep over 3A/3B/3E returned
EMPTY); no silently-dropped candidate (3C disposes 21/21, the DEFER names its
re-entry trigger); no cross-scope violation (every totality-tree item is
explicitly routed to SK-V19; the monotonic skinny→totality fold is preserved in
3D/3F). Lock 14 holds; no JSON/CSS-narrowing amendment; the future-grammar
onboarding test survives.

## REVISE summary (with exact corrections)

- **CH2-V2-R01 (3B-master-plan-reconciliation.md:149 + 3D-skinny-fold.md:89,
  cross-bound to 3A:214 / 3C-locks-crystallisation.md:87 / 3C-locks-v+1-diff.md:71
  / 3E:334) — RESIDUAL FORBIDDEN-token-set divergence; the un-closed remainder of
  V1/CH2-R02.** The V1 fold landed the load-bearing part of R02: the alias-immune
  `*EventGrammar` glob is now present in ALL surfaces. But the **base** token set
  did not converge — it split along cited-source lines into two clusters:
  - `{CSS_,_RS,EventGrammar,*EventGrammar}` — sourced from T-P1 finding **1E:152**
    (LAC-1E-V5-06). Carried by 3A:214, 3C-cryst:87, 3C-diff:71, 3E:334.
  - `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` — sourced from the
    certified **SK-V18 SPEC** (`:137`,`:474`,`:711`,`:778`,`:828`). Carried by
    3B:149, 3D:89.

  These are not the same gate: `CSS_`/`_RS` are SUBSTRING tokens (broad — `_RS`
  catches every `*_RS`); `GENERATED_RS`/`CSS_GENERATED_RS` are FULL-IDENTIFIER
  tokens (narrow — they catch only the two named consts, with `*EventGrammar`
  doing the alias work). The `⊇` strict-superset framing softens the contradiction
  (each is a lower bound, so a union-honoring gate satisfies both), but the
  V1/CH2-R02 correction was explicit — "adopt one canonical token set across
  3A/3B/3C … they must be identical" — and they are not. Worse, 3C-cryst:87 and
  3C-diff:25 now self-CLAIM "canonical across 3A-D11/3B-P4/the v+1 diff", which is
  FALSE on the 3B leg (and 3D, which V1/CH2 never named, is a fourth divergent
  carrier).
  **Correction:** pick ONE canonical token set and write it verbatim in all five
  loci. Because both the SK-V18 SPEC and 1E:152 are legitimate evidence, the
  cleanest fix is to make the canonical gate the UNION — `FORBIDDEN_GENERIC_TOKENS
  ⊇ {CSS_, _RS, GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}` (the
  substring tokens subsume the full-id ones, so this is just the substring form
  plus the explicit consts the SPEC names) — and update the "canonical across …"
  self-claims to reference the union, OR formally reconcile 1E:152 and SK-V18-SPEC
  to a single form in Pass Omega CRUD and back-cite it. Either way 3B and 3D must
  carry the IDENTICAL string the 3A/3C/3E cluster carries. Owner: Pass Omega CRUD /
  the 3B+3D propagation owner. Severity: medium (precision/consistency; not a
  narrowing, not a refuted-route revival — the gate's alias-immunity is intact in
  both forms, so generality is not endangered, only the single-source-of-truth
  discipline).

## Verdict and census

R01 (non-applying v+1 diff — the binding G3 gate object) and R03 (un-caveated
"measurement-valid" closure word) are CLOSED and independently re-verified clean.
That is the substance of the V1/CH2 load: ACCEPT now dominates — the one-generator
generality thesis, the 9-grammar fleet matrix, the Sheets precedence-tower
negative control, the no-narrowing Lock 14, the 16-lock/5-shape canon, and the
surviving future-grammar onboarding test are all sound, and the v+1 diff applies.

The single residual is CH2-V2-R01: the FORBIDDEN-token-set base divergence, the
un-closed tail of R02. The folded artefacts harmonised the alias-immune glob but
not the base tokens; 3B and 3D track the SK-V18-SPEC form while 3A/3C/3E (and the
cited 1E:152) track the inventory form, and 3C falsely advertises its form as
already-canonical. This is a precision REVISE, not a generality breach — but it is
a real, citable two-surfaces-one-gate divergence and the exact class of defect R02
was opened to eliminate, so it cannot ACCEPT.

Census: ~36 dispositions judged under the CH2 lens (12 in 3A, 8 in 3B, 9 lock
dispositions + no-narrowing/onboarding confirmations in 3C, 8 in 3D/3F, 9 in 3E).
The REVISE load lands on four artefact loci — 3B-master-plan (P4 row), 3D-skinny-fold
(D04 row), 3A-architecture (D11), 3C-locks (green-by-exclusion clause + the false
"canonical" self-claim) — but they are ONE defect (CH2-V2-R01), the same token-set
string viewed from each surface. Three REVISE dispositions feed it (3A-D11, 3B-P4,
3C-green-by-exclusion); 3D-D04 is its fourth, previously-unflagged carrier.

TALLY accept=32 revise=4 reject=0
