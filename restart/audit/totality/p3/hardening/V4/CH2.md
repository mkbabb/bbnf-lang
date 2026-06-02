---
challenge_agent: CH2
name: GENERALITY
pass: T-P3-synthesis
cycle: V4
sk_cycle: SK-V18
verdict: ACCEPT
generated_at: 2026-06-01T21:55:00Z
owned_output: restart/audit/totality/p3/hardening/V4/CH2.md
---

# CH2 GENERALITY — SK-V18 T-P3 V4

Lens: CH2 GENERALITY. Subject: the 6 T-P3 synthesis artefacts under
`restart/audit/totality/p3` (3A-architecture, 3B-master-plan,
3C-locks-crystallisation + 3C-locks-v+1-diff, 3D-skinny-fold,
3E-grammar-generalisation, 3F-migration-handoff), audited against T-P1 evidence
(`p1/`), T-P2 dossiers (`p2/`), and the V1 surfaces (`restart/ARCHITECTURE.md`,
`MASTER-PLAN.md`, `locks/LOCKS.md`, `MIGRATION.md`).

CH2 mandate: Lock 14 holds; 3A surface deltas + 3B waves generalise to non-JSON;
3E's story is concrete for CSS L4 / Sheets / BBNF-self / the 9-grammar fleet; 3C
accepts no JSON/CSS-narrowing amendment; the future-grammar onboarding test
survives. Spot-verify the most load-bearing deltas (a cited finding-id resolves;
a cited LOCKS section exists; the v+1 diff applies).

This is cycle V4 (a confirmation challenge against the same SK-V18 target packet
hardened in V1-V3). The single carried CH2 defect entering V4 was **CH2-V3-R01**:
the FORBIDDEN_GENERIC_TOKENS base-set divergence (two literal token clusters,
one of them sitting in the BINDING LOCKS hunk, plus a false "canonical across …"
self-claim). V4's charge is to independently confirm whether that fold landed,
re-run every load-bearing spot-check, and re-enumerate the lens dispositions —
not to rubber-stamp the fold note.

## CH2-V3-R01 fold — INDEPENDENTLY VERIFIED CLOSED

The V3 REVISE is genuinely repaired, byte-checked across every locus (not merely
claimed by a fold note):

| surface | locus | live gate string | result |
|---|---|---|---|
| 3A-D11 | `3A-architecture-synthesis.md:217` | `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` | IDENTICAL |
| 3B-P4 | `3B-master-plan-reconciliation.md:149` | `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` | IDENTICAL |
| 3C-v+1-diff (BINDING hunk) | `3C-locks-v+1-diff.md:74` | `FORBIDDEN_GENERIC_TOKENS ⊇ {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` | IDENTICAL |
| 3C-cryst | `3C-locks-crystallisation.md:89` | `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` | IDENTICAL |
| 3D-D04 | `3D-skinny-fold.md:91` | `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` | IDENTICAL |
| 3E onboarding table | `3E-grammar-generalisation.md:337` | `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` | IDENTICAL |

The two prior clusters have collapsed to ONE. The only surviving
`{CSS_, _RS, …}` strings in the entire packet are inside the V3-FOLD CHANGELOG
notes that document the supersession (`3C-locks-v+1-diff.md:26`,
`3C-locks-crystallisation.md:30`) — they explicitly read "the broad
`{CSS_, _RS, …}` inventory form the hunk previously carried is superseded." No
LIVE gate carries the broad form (`grep '⊇ {CSS_' 3?-*.md` = EMPTY on live text).

The two compounding faults are both repaired:
- **(i) BINDING hunk.** The text Pass Omega CRUD would land in LOCKS.md
  (`3C-locks-v+1-diff.md:74`) now carries the certified SPEC form, scoped to the
  grammar-body-courier suffix and false-positive-free, NOT the broad `_RS`
  substring the SPEC superseded. The antecedent chain resolves: SK-V18 SPEC
  `:711`-`712` defines `{GENERATED_RS, CSS_GENERATED_RS, EventGrammar,
  *EventGrammar}` as the courier-suffix refinement of the `_RS` substring; 1E
  LAC-1E-V5-06 (`1E-locks-evidence.md:152`) is the broad inventory antecedent the
  SPEC refines. Both cite-targets exist verbatim.
- **(ii) False "canonical" self-claim.** `grep 'canonical across' 3C-*.md` now
  matches only inside the V3-FOLD note recording the repair ("the false
  `canonical across …` self-claim repaired to a single-source pointer",
  `3C-locks-crystallisation.md:30`). No live uncorroborated single-source-of-truth
  assertion remains.

## Required + load-bearing spot-verifications (V4)

| check | result | evidence |
|---|---|---|
| The 3C v+1 diff applies to current LOCKS.md | **PASS** | `awk`-extracted `/tmp/tp3-locks-v4ch2.diff` (37 lines); `git apply --check` exits 0. Header `@@ -622,6 +622,33 @@`; inserts the SK-V18 addendum after the SK-V17 Lock-16 clause tail (`LOCKS.md:622`) and before `## v+1 Governance Boundary` (`LOCKS.md:625`). |
| FORBIDDEN-token-set base convergence (CH2-V3-R01) | **PASS — folded** | Byte-identical across all 6 loci (table above); the broad form survives only in V3-FOLD changelog notes. |
| `measurement-valid` un-caveated word (CH2-R03) | PASS — stays closed | 3B keeps "directionally-valid pending H1 re-lock; overfit is IMPLEMENTATION not measurement" (MP-3B-SKV18-D10). |
| 16-lock count | PASS | `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` = 16. |
| Pattern-H runtime file census | PASS | `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` = 71 (the recensus baseline 3C-diff:84/3A-D12/3E-D17 correctly cite; the SK-V15-era absolute 67 is re-keyed to per-file provenance). |
| BackendShape five-variant canon (code) | PASS | `skinny/crates/ir/src/lib.rs:341`-`345` = `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`; `skinny/crates/codegen/src/lower/mod.rs:18`-`24` `select_lowering` dispatches on `cost.chosen` over exactly those 5 arms (grammar-NEUTRAL cost axis, not a grammar tag). |
| 9-grammar fleet roster (3E-D16) | PASS | `crates/core/src/grammar/generated/*.rs` (mod.rs excluded) = 9 = {bbnf,bnf,css_l4,css_pretty,csv,ebnf,google_sheets,json,math}; matches the 9 `idents:` rows at `crates/ir/src/registry/strategy.rs:137,143,149,155,161,167,173,179,185`. |
| 2C grounded generality findings (213-219) | PASS | `2C-grammar-neutrality.md:213`-`219` resolve verbatim: one-generator thesis, (a)-(d) discipline, css_balanced FORCED-demotion, Sheets precedence-tower negative control, 5-shape-beyond-JSON, 9-grammar onboarding (partial/SK-V18-witnessed-3), relocated-seam firewall. |
| 2C tree-walk refute (the (a)-(d) escape grounding) | PASS | `2C-grammar-neutrality.md:307`/`:313` refute "neutral-name-on-one-grammar proves neutrality" and "tree-walk preserves the 94.1% scan" — the correct refuted-route grounding for D-SKV18-L14. |
| Cited LOCKS section (Lock 14 @ :349 + grammar-generalisation receiver rule) | PASS | `LOCKS.md:349` is the Lock 14 receiver/onboarding clause cited by 3E-D04/D07/D08/D09 and the 3C disposition matrix; generality binds OUTWARD (provider manifests, 9-grammar matrix, Sheets/BBNF-self receivers). |
| Required stale-pattern rg over 3A..3F | PASS | The only match is the LAC-2F-V3-03 DEFER row's "re-entry trigger" phrasing (`3C-locks-crystallisation.md:144,:161`) — the REQUIRED PASS-3 §3C DEFER disposition form (the DEFER names its re-entry trigger), a correct use, not a revived route. |
| Boundary fault (live spec-surface edit) | PASS | `git status --short` on ARCHITECTURE/MASTER-PLAN/LOCKS/HANDOFF/MIGRATION = EMPTY. (docs/precepts is dirty but is not a T-P3 surface; unrelated state.) T-P3 proposes only. |

Every generality-bearing finding-id, LOCKS section, and live-code anchor I
sampled resolves exactly; the v+1 diff applies clean; and the single carried
defect is byte-verified closed.

## Disposition enumeration (CH2 lens)

### 3A architecture (SK-V18 generality deltas D01-D14)

| delta | verdict | basis |
|---|---|---|
| D01 phantom `<G>` strike + re-anchor on Cursor trait + config-breadth classifier (8/9) | ACCEPT | Re-anchors generality on the two axes `ARCHITECTURE.md:1998`/`:2005` already name; `<G>` has no non-test production animator; generality survives the strike. |
| D02 named-primitive (a)-(d) ARCHITECTURE-authoritative | ACCEPT | Keeps the 94.1% scan grammar-PARAMETERISED; refuted-tree-walk (2C:313) is the correct grounding; binds G1∧G2, not CSS-only. |
| D03 un-fork: DELETE `RuntimeEmitterKind`, dispatch on `BackendShape` | ACCEPT | Dispatch on `cost.chosen` (lower/mod.rs:18 verified) — the cost-derived grammar-neutral axis; the generality backbone, not a JSON path. |
| D04 relocated-seam firewall `emit_shape_source==lowered_program` + CSS second seam | ACCEPT | Grep-evasive neutrality falsifier; `css_provider_source==generated` extends generality to the CSS provider channel; totality `crates/core/css_l4/` correctly fenced to SK-V19. |
| D05 verbatim-blob-courier prohibition | ACCEPT | Bars a hand-written CSS `&str` (CSS_GENERATED_RS courier at runtime_generator.rs:701) being credited as grammar-derived. |
| D06 5-shape canon RE-FRAMED as POSITIVE neutral dispatch axis | ACCEPT | Strongest generality affirmation: JSON=SinkOnly, CSS=lowered scan IR, Sheets tower→same SinkOnlyExpr vocab; a sixth shape = overfit. |
| D09 G6 retarget-not-author + neutral-inner-kernel + single-movemask | ACCEPT | Inner eq-set kernel carries byte-set as CALLER DATA (neutral) even under a CSS-scoped shell — the correct generality split. |
| D10 CSS-scoped `css_balanced_component_scan` FORCED demotion | ACCEPT | Honest discharge of a one-grammar neutral name; base one-fan kernel stays neutral independently (2C:215). |
| D11 totality 9-ident leak + `css_types.rs` + green-by-exclusion fix (D11a/D11b cost split) | ACCEPT | 9-ident leak verified (strategy.rs:137-185); D11a (+15 skinny P4) / D11b (+217 SK-V19) cost split honest; token form now byte-identical (CH2-V3-R01 CLOSED). The V3 REVISE on this delta is repaired. |
| D13 Sheets precedence-tower negative control (by-exercise upgrade) | ACCEPT | Upgrades Sheets from by-construction to by-exercise; tower lowers to existing `SinkOnlyExpr` (no new IR); scoped, no fleet wording until SK-V19. The make-or-break generality stressor. |
| D14 SK-V18 authority + HANDOFF-scope reconcile | ACCEPT | Routes the scope pivot (SK-V18=skinny generalization, SK-V19=crates/core fold); prevents fleet-wide misreading. |

(D07 aarch64-only, D08 CollapsedStage slot, D12 metalang+Pattern-H census are
host/shape/census deltas tangential to the generality lens; owned by CH3/CH4/CH7;
CH2 takes no position.)

### 3B master-plan (waves generalise to non-JSON)

| delta | verdict | basis |
|---|---|---|
| MP-3B-SKV18-D01..D03 scope pivot + §13.7 12-wave GENERALIZATION block | ACCEPT | Waves map G1(JSON SinkOnly)→G2(CSS lowering, `CSS_GENERATED_RS` DELETED)→G3(un-fork on `BackendShape`)→PROVE(Sheets) with same-wave consumers + RED-exit falsifiers (3B:152,:156). |
| MP-3B-SKV18-D04 P-cluster routing (P4-before-G2/G3) + §13.7 P4 token set | ACCEPT | Green-by-exclusion gate sequenced before emitter is neutrality-scanned; the P4 row token set (3B:149) is now byte-identical to the binding hunk (CH2-V3-R01 leg CLOSED). |
| MP-3B-SKV18-D05 G-cluster (a)-(d) gate + 5-conjunct G3 un-fork exit | ACCEPT | Un-fork reads `BackendShape` not a grammar tag — generality preserved per-wave (3B:195). |
| MP-3B-SKV18-D06 PROVE Sheets + H1 with BINDING FALLBACK `N` | ACCEPT | A Sheets shim ⇒ `N` (generalization NOT real), surfaced honestly, never paper-closed — the anti-narrowing fallback. |
| MP-3B-SKV18-D07..D09 SK-V19 tee-up + §25 order + F.W5 un-fork (3 SK-V18 / 9 SK-V19) | ACCEPT | Totality-tree leaks DEFERRED to SK-V19; F.W5 "nine seed grammars" held as the SK-V18→SK-V19 obligation, NOT read as already-satisfied. |
| MP-3B-SKV18-D10 CSS verdict UPGRADED (directionally-valid pending H1) | ACCEPT | The bare "measurement-valid" is gone; R03 stays closed. |

### 3C LOCKS (no JSON/CSS-narrowing amendment; Lock 14 holds)

| disposition / clause | verdict | basis |
|---|---|---|
| D-SKV18-L14-named-primitive-gate (LAC-1E-V5-01) | ACCEPT | Binds (a)-(d); keeps the hot kernel grammar-PARAMETERISED — the inverse of JSON narrowing. |
| D-SKV18-L05-L10-unfork (LAC-1E-V5-02, 2D-V3-01/02) | ACCEPT | `render(program)` dispatches on `backend_shape`; dispatch-on-source-family is REJECT — generality is the lock. |
| D-SKV18-L14-neutrality-proof (LAC-1E-V5-03, 2C-SK18-01/02) | ACCEPT | Forced-demotion + fleet-scoping in one clause; inner kernel may stay neutral; NO fleet-wide wording on <full-roster witness. |
| D-SKV18-L14-green-by-exclusion (LAC-1E-V5-06) | ACCEPT | The clause (3C-cryst:89 / binding hunk 3C-diff:74) carries the certified SPEC form byte-identically; the false "canonical" self-claim repaired. CH2-V3-R01 CLOSED — this row was the V3 REVISE and is now clean. |
| D-SKV18-L13-pattern-h-recensus (LAC-1E-V5-07, 2C-SK18-03) | ACCEPT | 9-ident totality leak routed to STRUCTURAL full-row collapse over all 9 + widened regex at SK-V19; no narrowing; 71-file recensus verified. |
| D-SKV18-L01-cursor-generality (1A-LOCK1-AMEND-001, 2D-V3-03) | ACCEPT | Strikes phantom `<G>`, re-anchors on Cursor trait + config-breadth classifier; generality survives the delete. |
| L14-HC-07 future-grammar onboarding test | ACCEPT | Source/metadata-only; Sheets/BBNF-self fail on a generic branch / new directive / sixth shape. The onboarding test SURVIVES intact. |
| No JSON/CSS-narrowing amendment introduced | CONFIRMED | The live Lock 14 clause (LOCKS:349 + grammar-generalisation receiver rule) and all 11 SK-V18 clauses bind generality OUTWARD; zero clause narrows the lock to JSON or CSS. |
| 21/21 candidates disposed, 0 silent drops | CONFIRMED | 1E×7, 1A×1, 2C×3, 2D×4, 2E×3, 2F×3; LAC-1E-V5-01..07 and LAC-2D-V3-01..04 all present; the DEFER (LAC-2F-V3-03) names its re-entry trigger. |
| No directive / BIR variant / substrate / public substrate API / retained sidecar / sixth shape | CONFIRMED | v+1 Executive Summary (3C-diff:43-45) and the live 5-arm `lower/mod.rs:18-24` confirm; FactStream stays output-plane, not a sixth shape. |

### 3D skinny-fold / 3F migration-handoff (generality cross-refs)

| delta | verdict | basis |
|---|---|---|
| 3D-D01 JSON-guard-scope (51/51 is same-plane PoC, not fleet closure) | ACCEPT | Bars JSON from masquerading as generality proof. |
| 3D-D04 green-by-exclusion token set (3D:89,:91,:124) | ACCEPT | Now carries the certified SPEC form (CH2-V3-R01 leg CLOSED; the V3 REVISE on this delta is repaired; 3D:50 V3-FOLD consistency note confirms). |
| 3D-D06 / D09 decision-engine depth under Sheets tower + Sheets negative-control onboarding (shim ⇒ `N`) | ACCEPT | Five-shape canon preserved; monotonic generalization bridge; fleet wording waits for the witness. |
| 3D-D10 PRUNE-before-REBUILD sequencing | ACCEPT | No cross-scope violation; T-P3 proposes only, dispatches no wave. |
| 3F-MH HANDOFF SK-V18=generalization scope + blocker matrix + phantom `<G>` DELETE + `css_types.rs` SK-V19 reroute | ACCEPT | Strikes the stale "SK-V18 adopts into crates/core" definition; routes fleet onboarding + named generic-core mess to SK-V19; not silently dropped, not narrowed. |

### 3E grammar-generalisation (the lens-primary artefact)

| delta | verdict | basis |
|---|---|---|
| 3E-D01..D11 (carried SK-V15 generality matrix) | ACCEPT | Non-JSON proof matrix (CSS L4 positive + Sheets/BBNF-self negative controls), per-grammar BackendShape matrix, primitive transfer, Lock 14 hardening clauses; concrete receivers at `3E:103`-`123`. |
| 3E-D12 one-generator generalisation thesis | ACCEPT | Generality is an INPUT-SURFACE property (grammar source + metadata), proven by md5-distinct output from a neutral renderer; 2C:213 grounded; md5 carried as necessary-NOT-sufficient. |
| 3E-D13 named-primitive (a)-(d) neutrality discipline | ACCEPT | (a)-(c) prove grammar-COUPLING, (d) bounds SIZE; admissible ONLY under all four; 2C:214 grounded. |
| 3E-D14 css_balanced_component_scan FORCED demotion | ACCEPT | Base one-fan kernel structurally neutral; JSON `{}`/`[]` + Sheets `paren_expr` dischargers are parse-with-emit, structurally incompatible (2C:215) — the forced name IS the discharge. |
| 3E-D15 Sheets precedence-tower negative control | ACCEPT | 7-level tower lowers to existing `SinkOnlyExpr`; no relabeled courier can fake the recursive `CallRule`/`RepeatLoop` cascade; `Nu8`-tagged-alt correctly DEMOTED (CSS 295× vs Sheets 21×, shared); 2C:216 grounded. |
| 3E-D16 9-grammar BackendShape fleet matrix | ACCEPT | Roster verified (9 idents + 9 files); five-shape canon preserved; CollapsedStage in no dominant/secondary cell (correct, M5-Max-aarch64); math is a second precedence-tower witness corroborating Sheets; 2C:217 grounded. |
| 3E-D17 relocated-seam firewall + CSS second seam | ACCEPT | md5-distinct necessary-NOT-sufficient; extends generality to the CSS provider source channel; 2C:219 grounded. |
| 3E-D18 fleet-scoped neutrality wording | ACCEPT | Anti-overclaim: <full-roster witness ⇒ scoped wording; 6 remaining grammars are SK-V19 receivers; 2C:381 grounded. Prevents the fold being narrated as already-fleet-proven. |
| 3E onboarding-table token set (3E:337, was 3E:334) | ACCEPT | Now byte-identical to the binding hunk (3E:49 V3-FOLD note confirms; CH2-V3-R01 leg CLOSED). |

## REJECTs

None. No uncited delta. No revived refuted route: the 8 REFUTED constraints in
the 3E frontmatter (tree-walk-preserves-94.1%, find_css_significant-wire-as-is,
neutral-name-on-one-grammar, checkasm-PASS-as-speedup, x86/AVX-512-closes-a-row,
eq-set-dual-consumer, md5-distinctness-alone, bracket_depth_mask_64) are all
carried as REFUTED; a positive-revival grep over 3A/3B/3E returned EMPTY. No
silently-dropped candidate (3C disposes 21/21; the DEFER names its re-entry
trigger). No cross-scope violation (every totality-tree item routed to SK-V19;
the monotonic skinny→totality fold preserved in 3D/3F). No directive, BIR
variant, substrate, public substrate API, retained sidecar, or sixth
`BackendShape` enters the packet (verified against the v+1 Executive Summary and
the live 5-arm `lower/mod.rs`). Lock 14 holds; no JSON/CSS-narrowing amendment;
the future-grammar onboarding test survives.

## REVISEs

None. The single CH2 defect entering V4 — **CH2-V3-R01** (the
FORBIDDEN_GENERIC_TOKENS base-set divergence + the binding-hunk + the false
"canonical" self-claim) — is INDEPENDENTLY VERIFIED CLOSED: the token set is
byte-identical across all six loci (3A:217, 3B:149, 3C-diff:74 [the BINDING
hunk], 3C-cryst:89, 3D:91, 3E:337), the broad form survives only in V3-FOLD
changelog notes documenting its supersession, the binding hunk carries the
certified SK-V18 SPEC form (SPEC:711-712, antecedent 1E:152), the diff still
applies clean (`git apply --check` exit 0), and the false self-claim is repaired
to a single-source pointer. No new generality defect was found on independent
re-run.

Note on the cycle-V1 ≥30% REVISE expectation: that prior is a first-cycle
diversity heuristic. V4 is a confirmation cycle whose lone carried defect was
repaired; the four REVISE-feeding loci of CH2-V3-R01 (3A-D11, 3B-P4,
3C-green-by-exclusion, 3D-D04, plus the 3E onboarding row) are each now
byte-clean. Manufacturing a REVISE against byte-verified-converged evidence would
be dishonest. The generality case is sound and the verdict is ACCEPT.

## Verdict and census

R01 (non-applying v+1 diff), R03 (un-caveated "measurement-valid"), and now
CH2-V3-R01 (token-set base divergence) are all CLOSED and re-verified clean. The
substance of the generality case resolves end to end: the one-generator thesis
(2C:213), the named-primitive (a)-(d) discipline (2C:214), the css_balanced
forced demotion (2C:215), the Sheets precedence-tower negative control (2C:216),
the 9-grammar fleet matrix (9 idents + 9 files, 5-shape canon preserved), the
relocated-seam firewall + CSS second seam (2C:219), the fleet-scoped wording
(2C:381), the no-narrowing Lock 14 (LOCKS:349), the 16-lock / 5-shape canon, the
21/21 candidate disposition, and the surviving future-grammar onboarding test;
and the v+1 diff applies.

Census: ~40 dispositions judged under the CH2 lens (11 in 3A, 6 in 3B, 11 lock
dispositions + no-narrowing/onboarding/no-forbidden-surface confirmations in 3C,
6 in 3D/3F, 9 in 3E). All ACCEPT. Zero REVISE. Zero REJECT. The packet is
CH2-clean for SK-V18 T-P3 V4.

TALLY accept=43 revise=0 reject=0
