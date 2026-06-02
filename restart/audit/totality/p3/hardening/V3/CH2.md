---
challenge_agent: CH2
name: GENERALITY
pass: T-P3-synthesis
cycle: V3
sk_cycle: SK-V18
verdict: REVISE
generated_at: 2026-06-01T21:40:00Z
owned_output: restart/audit/totality/p3/hardening/V3/CH2.md
---

# CH2 GENERALITY — SK-V18 T-P3 V3

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

This is cycle V3. The on-disk artefacts (mtimes 20:16-20:27) PREDATE the V2/CH2
verdict (20:35); their frontmatter records only `V1-FOLD` markers — there is NO
`V2-FOLD (CH2-V2-R01)` anywhere (`grep V2-FOLD|CH2-V2-R01 restart/audit/totality/p3/3?-*.md`
= EMPTY). So V3 reviews the SAME artefacts V2 reviewed: the V2 token-set REVISE
was NOT folded. My charge is to re-run the load-bearing spot-checks, confirm the
state of the V2 carry, and re-enumerate the lens dispositions.

## Spot-verifications performed (load-bearing)

| check | result | evidence |
|---|---|---|
| **The 3C v+1 diff applies to current LOCKS.md** | **PASS** | `awk`-extracted `/tmp/tp3-locks-v3.diff` (37 lines), `git apply --check` exits 0. Header `@@ -622,6 +622,33 @@`; inserts after the SK-V17 Lock-16 clause tail, before `## v+1 Governance Boundary` (`LOCKS.md:625`). R01 (the V1 binding FAIL) stays closed. |
| **FORBIDDEN-token-set base divergence (V1/CH2-R02 → V2/CH2-V2-R01)** | **FAIL — NOT folded** | Two literally-different base token sets survive UNCHANGED from V2: `{CSS_,_RS,EventGrammar,*EventGrammar}` at 3A:214, 3C-diff:71 (the binding LOCKS hunk), 3C-cryst:87, 3E:334 vs `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` at 3B:149, 3D:89, 3D:124. The "canonical across 3A-D11/3B-P4/v+1 diff" self-claim (3C-cryst:29, 3C-diff:25) is still FALSE on the 3B/3D legs. See CH2-V3-R01. |
| `measurement-valid` un-caveated closure word (V1/CH2-R03) | PASS — stays closed | 3B's positive MP-3B-SKV18-D10 verdict reads "directionally-valid pending the H1 `css_canon_bench` re-lock; the overfit is IMPLEMENTATION (forks/replicas), not measurement" (`3B:200`). |
| Cited finding-id resolves (1E:147 LAC-1E-V5-01 named-primitive (a)-(d) gate) | PASS | `1E:147` is the (a)-(d) gate row; binds the kernel grammar-PARAMETERISED. |
| Cited finding-id resolves (1E:152 LAC-1E-V5-06 green-by-exclusion) | PASS | `1E:152` writes the INVENTORY form `{CSS_,_RS,EventGrammar,*EventGrammar}` — the form 3A/3C/3E carry, NOT 3B/3D's. The inventory and the SPEC are themselves the two divergent sources. |
| Cited finding-id resolves (2C:213-219 grounded SK-V18-2C findings) | PASS | one-generator thesis (`:213`), (a)-(d) discipline (`:214`), forced-demotion (`:215`), Sheets tower (`:216`), 5-shape-beyond-JSON (`:217`), 9-grammar onboarding partial (`:218`), relocated-seam firewall (`:219`) — all `grounded`, all resolve verbatim. |
| Cited finding-id resolves (2C:313 tree-walk refute) | PASS | "A generic IR tree-walk lowering can preserve the 94.1% CSS scan | refuted" — the correct grounding for the (a)-(d) escape. |
| Cited finding-id resolves (2C:380-382 LAC-2C-SK18-01/02/03) | PASS | `:380` FORCED-demotion + inner-kernel-may-stay-neutral, `:381` fleet-scoped (3-witnessed → SK-V19), `:382` totality-tree 9-ident row-collapse + RED self-gate. |
| Cited LOCKS section exists (Lock 14 @ 603 + grammar-generalisation clause) | PASS | `LOCKS.md:603` is the zero-overfit Lock 14 (3-surface rule, 13-crate ZERO-grammar-arm command, `css_types.rs` named mess); the appended grammar-generalisation clause binds source/metadata-only onboarding "through CSS plus Sheets or BBNF-self … no new directive, BIR variant, sixth BackendShape." Generality binds OUTWARD. |
| Cited live code resolves (strategy.rs 9-ident leak) | PASS | EXACTLY 9 `idents:` data rows at `:137,:143,:149,:155,:161,:167,:173,:179,:185` (the 10th `idents:` at `:318` is the struct FIELD decl, not a row). The "9-ident leak" in 3A-D11/3C/3E-D16 is accurate. |
| Cited live code resolves (5-shape `select_lowering`) | PASS | `lower/mod.rs:18-24` dispatches on `cost.chosen` over exactly 5 `BackendShape` arms — the grammar-NEUTRAL cost axis, not a grammar tag. |
| 9-grammar fleet roster (3E-D16) matches reality | PASS | `crates/core/src/grammar/generated/*.rs` = 9 grammar files (`mod.rs` excluded) = {bbnf,bnf,css_l4,css_pretty,csv,ebnf,google_sheets,json,math}; matches strategy.rs 9 idents. |
| 16-lock count + 71-file Pattern-H baseline + stale-pattern scan | PASS | `grep -cE '^[0-9]+\. \*\*' LOCKS.md`=16; `find … runtime -mindepth 2 … *.rs`=71 (the recensus baseline 3C/3E correctly cite); the dispatch stale-pattern grep returns only `re-entry trigger` at 3C-cryst:142/159 — the REQUIRED DEFER-disposition phrasing for LAC-2F-V3-03 (PASS-3 §3C "the DEFER names its re-entry trigger"), a correct use, not a revived route. |
| 21/21 candidate disposition, 0 silent drop | PASS | All 7 LAC-1E-V5 (01-07) and all 4 LAC-2D-V3 (01-04) appear in the 3C matrix (some in combined clause rows, not as leading cells); 1E×7+1A×1+2C×3+2D×4+2E×3+2F×3=21 fully accounted. No silent drop; the DEFER (LAC-2F-V3-03) names its re-entry trigger. |

Every generality-bearing finding-id, LOCKS section, and live-code anchor I
sampled resolves exactly, and the v+1 diff applies clean. The SINGLE persisting
defect is the FORBIDDEN-token-set base divergence — the un-closed tail of R02 /
CH2-V2-R01 — which V3 did NOT fold and which is load-bearing because the
divergent string sits in the BINDING LOCKS hunk (3C-diff:71).

## Disposition enumeration (CH2 lens)

### 3A architecture (SK-V18 generality-bearing deltas)

| delta | verdict | basis |
|---|---|---|
| ARCH-3A-V4-SK18-D01 phantom `<G>` strike + re-anchor on Cursor trait + config-breadth classifier | ACCEPT | Re-anchors generality on two axes the LOCKS:620 clause already names; `1A-SUB-023` census EMPTY of non-test `<G>`; no narrowing. |
| ARCH-3A-V4-SK18-D02 named-primitive (a)-(d) ARCHITECTURE-authoritative | ACCEPT | Keeps the 94.1% scan grammar-PARAMETERISED; refuted-tree-walk (2C:313) is the correct grounding; G1∧G2, not CSS-only. |
| ARCH-3A-V4-SK18-D03 un-fork: DELETE `RuntimeEmitterKind`, dispatch on `BackendShape` | ACCEPT | Dispatch on `cost.chosen` (lower/mod.rs:18 verified) — the cost-derived grammar-neutral axis; the generality backbone, not a JSON path. |
| ARCH-3A-V4-SK18-D04 relocated-seam firewall `emit_shape_source==lowered_program` + CSS second seam | ACCEPT | The grep-evasive neutrality falsifier; the CSS second seam `css_provider_source==generated` extends generality to the CSS provider channel. |
| ARCH-3A-V4-SK18-D05 verbatim-blob-courier prohibition | ACCEPT | Courier verified at runtime_generator.rs:701; bars a hand-written CSS `&str` being credited as grammar-derived. |
| ARCH-3A-V4-SK18-D06 5-shape canon RE-FRAMED as POSITIVE neutral dispatch axis | ACCEPT | The strongest generality affirmation: JSON=SinkOnly, CSS=lowered scan IR, Sheets tower→same SinkOnlyExpr vocab; a sixth shape = overfit. Coherent with 3E-D16/2C:217. |
| ARCH-3A-V4-SK18-D09 G6 retarget-not-author + neutral-inner-kernel + single-movemask | ACCEPT | Inner eq-set kernel carries byte-set as CALLER DATA (neutral) even under a CSS-scoped shell — the correct generality split. |
| ARCH-3A-V4-SK18-D10 CSS-scoped `css_balanced_component_scan` FORCED demotion | ACCEPT | Honest discharge of a one-grammar neutral name; base one-fan kernel stays neutral independently (2C:215 confirms the two non-CSS dischargers are parse-with-emit, structurally incompatible). |
| ARCH-3A-V4-SK18-D11 totality 9-ident leak + `css_types.rs` + green-by-exclusion fix | REVISE | Generality-correct (9-ident leak verified at strategy.rs:137-185; D11a/D11b cost split honest). BUT 3A:214 carries `{CSS_,_RS,EventGrammar,*EventGrammar}`, which DIVERGES from 3B-P4/3D's `{GENERATED_RS,CSS_GENERATED_RS,…}`. The V1/CH2-R02 + V2/CH2-V2-R01 "identical across surfaces" correction is STILL not met. See CH2-V3-R01. |
| ARCH-3A-V4-SK18-D13 Sheets precedence-tower negative control (by-exercise upgrade) | ACCEPT | Upgrades Sheets from by-construction to by-exercise; tower lowers to existing `SinkOnlyExpr` (no new IR); scoped, no fleet wording until SK-V19. The make-or-break generality stressor. |
| ARCH-3A-V4-SK18-D14 SK-V18 authority + HANDOFF-scope reconcile | ACCEPT | Routes the scope pivot (SK-V18=skinny generalization, SK-V19=crates/core fold); prevents fleet-wide misreading. |

(D07 aarch64-only, D08 CollapsedStage slot, D12 metalang+Pattern-H are
host/shape/census deltas tangential to the generality lens; CH2 takes no
position — owned by CH3/CH4/CH7.)

### 3B master-plan (waves generalise to non-JSON)

| delta | verdict | basis |
|---|---|---|
| MP-3B-SKV18-D01..D03 scope pivot + §13.7 12-wave GENERALIZATION block | ACCEPT | Waves map G1(JSON SinkOnly)→G2(CSS lowering, `CSS_GENERATED_RS` DELETED)→G3(un-fork on `BackendShape`)→PROVE(Sheets) — generalise beyond JSON with same-wave consumers and RED-exit falsifiers (3B:151-152). |
| MP-3B-SKV18-D04 P-cluster routing (P4-before-G2/G3 hard order) | ACCEPT (delta) | Green-by-exclusion gate fixed BEFORE the emitter is neutrality-scanned-as-authored; correct sequencing for generality. |
| MP-3B-SKV18-D05 G-cluster (a)-(d) gate + 5-conjunct G3 un-fork exit | ACCEPT | The un-fork reads `BackendShape` not a grammar tag — generality preserved per-wave (3B:195). |
| MP-3B-SKV18-D06 PROVE Sheets + H1 with BINDING FALLBACK `N` | ACCEPT | A Sheets shim ⇒ `N` (generalization NOT real), surfaced honestly, never paper-closed — the anti-narrowing fallback. |
| MP-3B-SKV18-D07 SK-V19 totality-fold tee-up | ACCEPT | The totality-tree leaks DEFERRED to SK-V19, not bolted into SK-V18; F.W5 "nine seed grammars" held as the SK-V18→SK-V19 obligation. |
| MP-3B-SKV18-D08/D09 §25 order + F.W5 un-fork (3 grammars SK-V18 / 9 SK-V19) | ACCEPT | F.W5 explicitly NOT read as already-satisfied; fleet-scoping honest. |
| MP-3B-SKV18-D10 CSS verdict UPGRADED (directionally-valid pending H1) | ACCEPT | The bare "measurement-valid" is gone (3B:200); R03 closed and stays closed. |
| MP-3B-SKV18-D04 / §13.7 P4 row FORBIDDEN token set (3B:149) | REVISE | 3B:149 writes `FORBIDDEN ⊇ {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` — the SK-V18-SPEC form (`:137`,`:474`,`:711`), NOT 3A/3C/3E's 1E:152-sourced `{CSS_,_RS,…}` form. Two surfaces, two gates, for the one P4 fix. See CH2-V3-R01. |

### 3C LOCKS (no JSON/CSS-narrowing amendment; Lock 14 holds)

| disposition / clause | verdict | basis |
|---|---|---|
| D-SKV18-L14-named-primitive-gate (LAC-1E-V5-01 ACCEPT) | ACCEPT | Binds the (a)-(d) discipline; keeps the hot kernel grammar-PARAMETERISED — the inverse of JSON narrowing. |
| D-SKV18-L05-L10-unfork (LAC-1E-V5-02, 2D-V3-01/02 ACCEPT) | ACCEPT | `render(program)` dispatches on `backend_shape`; dispatch-on-source-family is REJECT — generality is the lock, not a narrowing. |
| D-SKV18-L14-neutrality-proof (LAC-1E-V5-03, 2C-SK18-01/02) | ACCEPT | Forced-demotion + fleet-scoping in one clause; inner kernel may stay neutral; explicitly NO fleet-wide wording on <full-roster witness. |
| D-SKV18-L14-green-by-exclusion (LAC-1E-V5-06 ACCEPT) | REVISE | The clause (3C-cryst:87 / the binding hunk 3C-diff:71) carries `{CSS_,_RS,EventGrammar,*EventGrammar}` and self-claims "canonical across 3A-D11/3B-P4/the v+1 diff per CH2-V1-R02" (3C-cryst:29, 3C-diff:25) — but 3B-P4 and 3D carry `{GENERATED_RS,CSS_GENERATED_RS,…}`. The "canonical" self-claim is FALSE on the 3B/3D legs, AND the binding LOCKS hunk would land the inventory form the certified SPEC explicitly superseded (`SPEC:712`). See CH2-V3-R01. |
| D-SKV18-L13-pattern-h-recensus (LAC-1E-V5-07, 2C-SK18-03 MODIFY) | ACCEPT | The 9-ident totality leak (verified) routed to STRUCTURAL full-row collapse over all 9 + widened regex at SK-V19; no narrowing; 71-file recensus baseline verified. |
| D-SKV18-L01-cursor-generality (1A-LOCK1-AMEND-001, 2D-V3-03 MODIFY) | ACCEPT | Strikes the phantom `<G>` vehicle, re-anchors on Cursor trait + config-breadth classifier (8/9 grammars). Generality survives the `<G>` delete. |
| L14-HC-07 / V4 onboarding-axes future-grammar test | ACCEPT | Source/metadata-only; Sheets/BBNF-self fail on a generic branch / new directive / sixth shape. The onboarding test SURVIVES intact. |
| No JSON/CSS-narrowing amendment introduced | CONFIRMED | The live Lock 14 clause (LOCKS:603 + grammar-generalisation clause) and all 11 SK-V18 clauses bind generality OUTWARD (provider manifests, 9-grammar matrix, Sheets/BBNF-self receivers). Zero clause narrows the lock to JSON or CSS. |
| 21/21 candidates disposed, 0 silent drops | CONFIRMED | 1E×7, 1A×1, 2C×3, 2D×4, 2E×3, 2F×3; all LAC-1E-V5-01..07 and LAC-2D-V3-01..04 present; the DEFER names its re-entry trigger. |

### 3D skinny-fold / 3F migration-handoff (generality cross-refs)

| delta | verdict | basis |
|---|---|---|
| 3D-D01 JSON-guard-scope (51/51 is same-plane PoC, not fleet closure) | ACCEPT | Explicitly bars JSON from masquerading as generality proof. |
| 3D-D06 decision-engine selection-DEPTH under Sheets tower | ACCEPT | Five-shape canon preserved; the tower depth is the open generality stressor at G3. |
| 3D-D09 Sheets negative-control onboarding (shim ⇒ `N`) | ACCEPT | Monotonic generalization bridge; fleet wording waits for the witness; cross-ref 3E-D07. |
| 3D-D10 PRUNE-before-REBUILD sequencing | ACCEPT | No cross-scope violation; T-P3 proposes only, dispatches no wave. |
| 3D-D04 green-by-exclusion FORBIDDEN token set (3D:89, 3D:124) | REVISE | 3D:89/124 carry `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` while citing "per CH2-V1-R02". 3D is the SK-V18-SPEC-aligned carrier divergent from the 3A/3C/3E cluster. Part of CH2-V3-R01 (V2 first flagged 3D as the un-named fourth carrier; still unfixed). |
| 3F-MH-005/006 HANDOFF SK-V18=generalization scope + blocker matrix | ACCEPT | Strikes the stale "SK-V18 adopts into crates/core" definition; routes fleet onboarding to SK-V19. |
| 3F-MH-012 phantom `<G>` DELETE (G4) + LOCKS:620 reconcile | ACCEPT | K-axis preserved; generality re-anchored; LOCKS edit correctly deferred to Pass Omega CRUD. |
| 3F-MH-013 `css_types.rs` RELOCATE-or-DELETE as SK-V19 | ACCEPT | The named generic-core mess is an explicit SK-V19 decision, not silently dropped, not narrowed into SK-V18. |

### 3E grammar-generalisation (the lens-primary artefact)

| delta | verdict | basis |
|---|---|---|
| 3E-D01..D11 (carried SK-V15 generality matrix) | ACCEPT | Non-JSON proof matrix, per-grammar BackendShape matrix, primitive transfer, Lock 14 hardening clauses — concrete for CSS/Sheets/BBNF-self. |
| 3E-D12 one-generator generalisation thesis | ACCEPT | Generality is an INPUT-SURFACE property (grammar source + metadata), proven by md5-distinct output from a neutral renderer; 2C:213 grounded; md5 carried as necessary-NOT-sufficient. |
| 3E-D13 named-primitive (a)-(d) neutrality discipline | ACCEPT | (a)-(c) prove grammar-COUPLING, (d) bounds SIZE; the escape is admissible ONLY under all four; 2C:214 grounded. |
| 3E-D14 css_balanced_component_scan FORCED demotion | ACCEPT | Base one-fan kernel structurally neutral; the JSON `{}`/`[]` and Sheets `paren_expr` dischargers are parse-with-emit, structurally incompatible (2C:215) — the forced name IS the discharge. |
| 3E-D15 Sheets precedence-tower negative control | ACCEPT | 7-level tower lowers to existing `SinkOnlyExpr`; no relabeled courier can fake the recursive `CallRule`/`RepeatLoop` cascade; `Nu8`-tagged-alt correctly DEMOTED (CSS 295× vs Sheets 21×, shared construct); 2C:216 grounded. |
| 3E-D16 9-grammar BackendShape fleet matrix | ACCEPT | Roster verified against strategy.rs (9 idents) + generated/ (9 files); five-shape canon preserved; CollapsedStage in no dominant/secondary cell (correct, M5-Max-aarch64); math is a SECOND precedence-tower witness corroborating Sheets; 2C:217 grounded. |
| 3E-D17 relocated-seam firewall + CSS second seam | ACCEPT | md5-distinct necessary-NOT-sufficient; the firewall extends generality to the CSS provider source channel; 2C:219 grounded. |
| 3E-D18 fleet-scoped neutrality wording | ACCEPT | Anti-overclaim discipline: <full-roster witness ⇒ scoped wording; 6 remaining grammars are SK-V19 receivers; 2C:381 (LAC-2C-SK18-02) grounded. Prevents the fold being narrated as already-fleet-proven. |
| 3E V4 onboarding table line 334 FORBIDDEN token set | NOTE (folded into CH2-V3-R01) | 3E:334 carries the canonical-cluster `{CSS_,_RS,EventGrammar,*EventGrammar}` — on the SAME side as 3A/3C/1E:152; reinforces the inventory cluster, not charged as its own defect. |

## REJECTs

None. No uncited delta. No revived refuted route: the 8 REFUTED constraints in
the 3E frontmatter (tree-walk-preserves-94.1%, find_css_significant-wire-as-is,
neutral-name-on-one-grammar, checkasm-PASS-as-speedup, x86/AVX-512-closes-a-row,
eq-set-dual-consumer, md5-distinctness-alone, bracket_depth_mask) are all carried
as REFUTED; a positive-revival grep over 3A/3B/3E returned EMPTY. No silently-
dropped candidate (3C disposes 21/21; LAC-1E-V5-01..07 and LAC-2D-V3-01..04 all
present; the DEFER names its re-entry trigger). No cross-scope violation (every
totality-tree item is routed to SK-V19; the monotonic skinny→totality fold is
preserved in 3D/3F). No directive, BIR variant, substrate, public substrate API,
retained sidecar, or sixth `BackendShape` enters the packet (verified against the
v+1 diff Executive Summary and the live 5-arm `lower/mod.rs`). Lock 14 holds; no
JSON/CSS-narrowing amendment; the future-grammar onboarding test survives.

## REVISE summary (with exact correction)

- **CH2-V3-R01 (3C-locks-v+1-diff.md:71 [the BINDING LOCKS hunk] + 3C-locks-crystallisation.md:87,:29
  + 3A-architecture-synthesis.md:214 + 3E:334 [inventory cluster] vs
  3B-master-plan-reconciliation.md:149 + 3D-skinny-fold.md:89,:124 [SPEC cluster])
  — the UN-FOLDED FORBIDDEN-token-set base divergence; the carried, still-open
  remainder of V1/CH2-R02 → V2/CH2-V2-R01.** The alias-immune `*EventGrammar`
  glob (the load-bearing element of R02) is present in ALL surfaces and the gate's
  alias-immunity is intact in both forms, so GENERALITY IS NOT ENDANGERED — this
  is a precision/single-source-of-truth REVISE, not a narrowing or a refuted-route
  revival. But the **base** token set never converged; it split into two clusters
  that are NOT the same gate:
  - `{CSS_, _RS, EventGrammar, *EventGrammar}` — broad SUBSTRING tokens (`_RS`
    catches every `*_RS` ident). Sourced from T-P1 **1E:152** (LAC-1E-V5-06).
    Carried by 3A:214, 3C-cryst:87, the BINDING v+1 hunk 3C-diff:71, 3E:334.
  - `{GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}` — narrow
    FULL-IDENTIFIER tokens. Sourced from the certified **SK-V18 SPEC**, which at
    `SPEC.md:712` DELIBERATELY refines the broad `_RS` substring to the
    courier-suffix `GENERATED_RS` ("the `_RS` token is scoped to the
    grammar-body-courier suffix `GENERATED_RS`, catching BOTH `CSS_GENERATED_RS`
    and `JSON_PARSE_ONLY_GENERATED_RS`"). Carried by 3B:149, 3D:89, 3D:124.

  Two faults compound and make this load-bearing rather than cosmetic. (i) The
  divergent string sits in the BINDING LOCKS hunk (3C-diff:71) — the text Pass
  Omega CRUD would land in LOCKS.md — so the amendment would bind the broad
  `_RS`-substring gate that the certified SPEC has already superseded with the
  narrow courier-suffix form, contradicting the plan rows (3B/3D) that follow the
  SPEC. (ii) 3C-cryst:29 and 3C-diff:25 self-CLAIM the inventory form is
  "canonical across 3A-D11/3B-P4/the v+1 diff," which is FALSE on the 3B and 3D
  legs — an uncorroborated single-source-of-truth assertion in the LOCKS
  singularity artefact. V2/CH2 opened CH2-V2-R01 to eliminate exactly this; no
  `V2-FOLD` landed; the defect persists verbatim.
  **Correction:** pick ONE canonical token set and write it byte-identically in
  all five loci AND the binding hunk. The cleanest fix adopts the certified
  SK-V18-SPEC form `FORBIDDEN_GENERIC_TOKENS ⊇ {GENERATED_RS, CSS_GENERATED_RS,
  EventGrammar, *EventGrammar}` (the SPEC's deliberate `:712` refinement is the
  more recent, narrower, false-positive-free gate, and is what the live
  `lock14_baseline.rs` P4 falsifier targets) — back-port it into 3A:214,
  3C-cryst:87, the BINDING 3C-diff:71 hunk, and 3E:334, and re-cite 1E:152 as the
  antecedent the SPEC refines (rather than the canonical form). Then rewrite the
  "canonical across …" self-claims to point at the single chosen string. If
  instead the 1E:152 substring form is kept, 3B and 3D must carry it identically
  and the SPEC `:712` refinement must be reconciled in Pass Omega CRUD with a
  back-cite. Either way, all five loci + the binding hunk must carry the IDENTICAL
  string, and the false "canonical" self-claim must be repaired. Owner: V4
  synthesis fold (the 3C+3B+3D+3A+3E token-set propagation owner) / Pass Omega
  CRUD. Severity: medium (precision + single-source-of-truth; the binding-hunk and
  certified-SPEC-contradiction elements elevate it above cosmetic).

## Verdict and census

R01 (non-applying v+1 diff) and R03 (un-caveated "measurement-valid") are CLOSED
and re-verified clean. The substance of the generality case is sound and ACCEPT-
dominant: the one-generator thesis (2C:213), the named-primitive (a)-(d)
discipline (2C:214), the css_balanced forced demotion (2C:215), the Sheets
precedence-tower negative control (2C:216), the 9-grammar fleet matrix (9 idents
+ 9 files verified, 5-shape canon preserved), the relocated-seam firewall + CSS
second seam (2C:219), the fleet-scoped wording (2C:381), the no-narrowing Lock 14
(LOCKS:603), the 16-lock/5-shape canon, the 21/21 candidate disposition, and the
surviving future-grammar onboarding test all resolve, and the v+1 diff applies.

The single, persisting residual is CH2-V3-R01: the FORBIDDEN-token-set base
divergence — the un-folded tail of R02 / CH2-V2-R01. V3 carried NO `V2-FOLD`
marker for it; the two clusters survive verbatim; the divergent string is in the
BINDING LOCKS hunk; the certified SK-V18 SPEC deliberately superseded the
inventory `_RS` form at `:712`; and 3C still falsely advertises its form as
already-canonical. This cannot ACCEPT — it is the exact two-surfaces-one-gate
defect R02 was opened to eliminate, now with a binding-hunk and SPEC-contradiction
amplifier, and it remained un-repaired through a full challenge cycle.

Census: ~36 dispositions judged under the CH2 lens (12 in 3A, 8 in 3B, 9 lock
dispositions + no-narrowing/onboarding confirmations in 3C, 8 in 3D/3F, 9 in 3E).
The REVISE load lands on four artefact loci — 3C-locks-v+1-diff (the binding
hunk), 3C-locks-crystallisation (the green-by-exclusion clause + the false
"canonical" self-claim), 3A-architecture (D11), 3B-master-plan (P4 row) — plus
3D-skinny-fold (D04). They are ONE defect (CH2-V3-R01), the same token-set string
viewed from each surface. Four REVISE dispositions feed it (3A-D11, 3B-P4,
3C-green-by-exclusion, 3D-D04).

TALLY accept=32 revise=4 reject=0
