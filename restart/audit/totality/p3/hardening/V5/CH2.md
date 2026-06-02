---
challenge_agent: CH2
name: GENERALITY
pass: T-P3-synthesis
cycle: V5
sk_cycle: SK-V18
verdict: ACCEPT
generated_at: 2026-06-01T21:10:00Z
owned_output: restart/audit/totality/p3/hardening/V5/CH2.md
---

# CH2 GENERALITY — SK-V18 T-P3 V5

Lens: CH2 GENERALITY. Subject: the 6 T-P3 synthesis artefacts under
`restart/audit/totality/p3` (3A-architecture, 3B-master-plan,
3C-locks-crystallisation + 3C-locks-v+1-diff, 3D-skinny-fold,
3E-grammar-generalisation, 3F-migration-handoff), audited against the T-P1
evidence (`p1/`), the T-P2 dossiers (`p2/`), and the V1 surfaces
(`restart/ARCHITECTURE.md`, `MASTER-PLAN.md`, `locks/LOCKS.md`, `MIGRATION.md`).

CH2 mandate: Lock 14 holds; 3A surface deltas + 3B waves generalise to non-JSON;
3E's story is concrete for CSS L4 / Sheets / BBNF-self / the 9-grammar fleet; 3C
accepts no JSON/CSS-narrowing amendment; the future-grammar onboarding test
survives. Spot-verify the most load-bearing deltas.

This is cycle V5 — an INDEPENDENT confirmation challenge against the same SK-V18
target packet hardened V1-V4. V4 returned 43 ACCEPT / 0 REVISE / 0 REJECT with
the single carried CH2 defect **CH2-V3-R01** (the FORBIDDEN_GENERIC_TOKENS
base-set divergence + binding-hunk + false "canonical" self-claim) byte-verified
closed. V5's charge is to re-run every load-bearing spot-check from scratch and
re-enumerate the lens dispositions — NOT to rubber-stamp the V4 fold note. I read
all six artefacts, the live Lock 14 clause, the v+1 diff, and the on-disk anchors,
and re-resolved every generality-bearing citation independently.

## Required + load-bearing spot-verifications (V5, re-run independently)

| check | result | evidence |
|---|---|---|
| The 3C v+1 diff applies to live LOCKS.md | **PASS** | `awk`-extracted 37-line diff; `git apply --check` exits 0 against HEAD. Header `@@ -622,6 +622,33 @@`; inserts the SK-V18 addendum after the SK-V17 Lock-16 clause tail (`LOCKS.md:622`) and before `## v+1 Governance Boundary`. |
| 16-lock count | PASS | `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` = 16. |
| 5-variant BackendShape canon (code) | PASS | `skinny/crates/ir/src/lib.rs:341`-`345` = `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`; `skinny/crates/codegen/src/lower/mod.rs:18`-`24` `select_lowering` matches exactly those 5 arms over `cost.chosen` — a grammar-NEUTRAL cost axis, not a grammar tag. |
| 9-grammar fleet roster (3E-D16) | PASS | `crates/core/src/grammar/generated/*.rs` (mod.rs excluded) = 9 = {bbnf,bnf,css_l4,css_pretty,csv,ebnf,google_sheets,json,math}; matches the 9 `idents:` rows at `crates/ir/src/registry/strategy.rs:137,143,149,155,161,167,173,179,185`. |
| FORBIDDEN-token base convergence (CH2-V3-R01) | **PASS — folded** | The certified SPEC form `{GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}` is present in all 7 artefacts (3A×2, 3B×1, 3C-cryst×2, 3C-diff×2 incl. the BINDING hunk, 3D×2, 3E×2, 3F×1). `grep '⊇ {CSS_' 3?-*.md` on LIVE gate text = EMPTY; the broad form survives only in V3-FOLD changelog notes documenting supersession. |
| SPEC antecedent for the token form | PASS | SK-V18 SPEC `:711`-`712` defines `{GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}` as the courier-suffix refinement; 1E `:152` (LAC-1E-V5-06) carries the OLD broad `{CSS_,_RS,…}` antecedent the SPEC REFINES — the antecedent legitimately differs from the refinement, not a divergence. |
| Pattern-H runtime file census | PASS | `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` = 71. The stale CHALLENGE-CONTEXT "67" is the SK-V15-era template figure; the +4 traces EXACTLY to the tape-fold roster `tape/{mod,cursor,arena,record}.rs` (verified on disk) — the re-key 3C D-SKV18-L13 / 3A-D11 / 3E L14-HC-05 / 1E LAC-1E-V5-07 cite. Per-file provenance over the live count is the invariant, not a fixed 67; the named cause is proven, so per the dispatch this is not a REJECT mismatch. |
| 2C grounded generality findings (213-219) | PASS | `2C-grammar-neutrality.md:213`-`219` resolve verbatim with the exact `grounded`/`partial` statuses 3E cites: one-generator thesis (213), named-primitive (a)-(d) discipline (214), css_balanced FORCED-demotion (215), Sheets precedence-tower negative control (216), 5-shape-beyond-JSON (217), 9-grammar onboarding [partial / SK-V18-witnessed-3] (218), relocated-seam firewall (219). |
| 2C refuted-route grounding | PASS | `2C:307` (neutral-name-on-one-grammar refuted), `:312` (md5-distinct necessary-not-sufficient), `:313` (tree-walk-preserves-94.1% refuted) all resolve verbatim — the correct refute grounding for D-SKV18-L14/3E-D14/D17. |
| 2C LAC-2C-SK18-01/02/03 | PASS | `2C:380`-`382` resolve: FORCED-demotion clause, fleet-scoped neutrality wording, totality-tree row-collapse precondition — the partners 3C folds. |
| 1E LACs + D-findings | PASS | `1E:147`-`153` (LAC-1E-V5-01..07) and `1E:105,106,114,118` (D-1E-V5-01/02/10/14) resolve verbatim; the 13-site self-gate falsification (D-1E-V5-14: command asserts ZERO, returns 13) is real and routed. |
| Cited LOCKS section (Lock 14 @ :349) | PASS | `LOCKS.md:349` IS the "Full grammar generalisation; zero overfitting" receiver clause; binds generality OUTWARD (three declarative surfaces, ZERO grammar-specific code), names `css_types.rs` verbatim as the overfit mess, and states the onboarding test "Adding a new grammar is a config + grammar-source change with NO code change in any generic crate." |
| Sheets/math precedence-tower fixtures | PASS | `grammar/google-sheets/google-sheets.bbnf:97`-`163` (7-level tower, `expression = comparison_expr`) and `grammar/misc/math.bbnf:1`-`16` (the 6-deep `p..pppppp` paren chain, `wrapped` recursion) resolve — the second precedence-tower witness corroborating Sheets. |
| Required stale-pattern rg over 3A..3F | PASS | The only match is the LAC-2F-V3-03 DEFER row's "re-entry trigger" phrasing (`3C-cryst:144,:161`) — the REQUIRED PASS-3 §3C DEFER disposition form, a correct use, not a revived route. |
| Boundary fault (live spec-surface edit) | PASS | `git status --short` on ARCHITECTURE/MASTER-PLAN/LOCKS/HANDOFF/MIGRATION = EMPTY. T-P3 proposes only. (docs/precepts is dirty but is not a T-P3 surface — unrelated state.) |

Every generality-bearing finding-id, LOCKS section, on-disk grammar fixture, and
live-code anchor I sampled resolves exactly; the v+1 diff applies clean; the
single carried defect is independently re-verified byte-closed.

## No JSON/CSS narrowing — the lens-primary confirmation

The v+1 diff is the actual LOCKS text Pass Omega would land. Reading all 11
clauses, NO clause narrows Lock 14 to JSON or CSS; every clause binds generality
OUTWARD:
- The relocated-seam/un-fork clause (`3C-diff:66`) dispatches on `BackendShape`
  over ALL witnessed grammars: "If the un-fork cannot dispatch all witnessed
  grammars on `BackendShape` without a `match grammar` arm, DELETE the un-fork
  and carry the SPEC-declared two-path unworkability; never ship a shim."
- The neutrality-proof clause (`3C-diff:68`) binds the fleet-scoping discipline:
  "fleet-wide wording requires SK-V19 adoption OR both Sheets AND BBNF-self
  negative-control witnesses in the same wave" — the explicit anti-narrowing rule.
- The named-primitive (a)-(d) gate (`3C-diff:64`) keeps the 94.1% CSS scan
  grammar-PARAMETERISED (args vary under invoking-rule mutation), the inverse of
  JSON narrowing.

The addendum header (`3C-diff:60`,`:43`-`45`) explicitly preserves 16 locks + the
five BackendShape variants and adds no directive, BIR variant, substrate, public
substrate API, retained sidecar, lock, lock retirement, or sixth shape. JSON is
held as scoped guard evidence only: 3D states the 51/51 admit is a same-plane PoC,
not fleet closure; 3E states this packet "proposes no JSON narrowing"
(`3E:85`-`88`,`:249`-`251`). FactStream stays output-plane (3A-D03 / `LOCKS:100`-
`109`), not a sixth shape.

## Disposition enumeration (CH2 lens)

### 3A architecture (SK-V18 generality deltas)
| delta | verdict | basis |
|---|---|---|
| D01 phantom `<G>` strike + re-anchor on Cursor trait + config-breadth classifier (8/9) | ACCEPT | Re-anchors generality on the two axes Lock 14:620 already names; `<G>` has zero non-test production animator; generality survives the strike. |
| D02 named-primitive (a)-(d) ARCHITECTURE-authoritative | ACCEPT | Keeps the 94.1% scan grammar-PARAMETERISED; refuted-tree-walk (2C:313) is the correct grounding. |
| D03 un-fork: DELETE `RuntimeEmitterKind`, dispatch on `BackendShape` | ACCEPT | Dispatch on `cost.chosen` (lower/mod.rs:18 verified) — the cost-derived grammar-neutral axis. |
| D04 relocated-seam firewall + CSS second seam (skinny-scoped; totality css_l4/ → SK-V19) | ACCEPT | Grep-evasive neutrality falsifier; `css_provider_source==generated` extends generality to the CSS provider channel; totality core fenced to SK-V19. |
| D05 verbatim-blob-courier prohibition | ACCEPT | Bars a hand-written CSS `&str` courier (`CSS_GENERATED_RS` at runtime_generator.rs:701) being credited as grammar-derived. |
| D06 5-shape canon RE-FRAMED as POSITIVE neutral dispatch axis | ACCEPT | Strongest generality affirmation: JSON=SinkOnly, CSS=lowered scan IR, Sheets tower→same vocab; a sixth shape = overfit. |
| D09 G6 retarget-not-author + neutral-inner-kernel + single-movemask | ACCEPT | Inner eq-set kernel carries byte-set as CALLER DATA (neutral) even under a CSS-scoped shell — the correct generality split. |
| D10 CSS-scoped `css_balanced_component_scan` FORCED demotion | ACCEPT | Honest discharge of a one-grammar neutral name; base one-fan kernel stays neutral independently (2C:215). |
| D11 totality 9-ident leak + `css_types.rs` + green-by-exclusion fix (D11a +15 / D11b +217 SK-V19 split) | ACCEPT | 9-ident leak verified (strategy.rs:137-185); cost split honest; token form byte-identical (CH2-V3-R01 CLOSED). |
| D13 Sheets precedence-tower negative control (by-exercise upgrade) | ACCEPT | Tower lowers to existing `SinkOnlyExpr` (no new IR); scoped, no fleet wording until SK-V19. The make-or-break generality stressor. |
| D14 SK-V18 authority + HANDOFF-scope reconcile | ACCEPT | Routes the scope pivot (SK-V18=skinny generalization, SK-V19=crates/core fold); prevents fleet-wide misreading. |

(D07 aarch64-only, D08 CollapsedStage slot, D12 metalang+Pattern-H census are
host/shape/census deltas tangential to the generality lens; owned by CH3/CH4/CH7.)

### 3B master-plan (waves generalise to non-JSON)
| delta | verdict | basis |
|---|---|---|
| MP-3B-SKV18-D01..D03 scope pivot + §13.7 12-wave GENERALIZATION block | ACCEPT | Waves map G1(JSON SinkOnly)→G2(CSS lowering, `CSS_GENERATED_RS` DELETED)→G3(un-fork on `BackendShape`)→PROVE(Sheets tower) with same-wave consumers + RED-exit falsifiers (3B:151-156). |
| MP-3B-SKV18-D04 P-cluster routing (P4-before-G2/G3) + §13.7 P4 token set | ACCEPT | Green-by-exclusion gate sequenced before the emitter is neutrality-scanned; the P4 row token set (3B:149) is byte-identical to the binding hunk (CH2-V3-R01 leg CLOSED). |
| MP-3B-SKV18-D05 G-cluster (a)-(d) gate + 5-conjunct G3 un-fork exit | ACCEPT | Un-fork reads `BackendShape` not a grammar tag — generality preserved per-wave (3B:153). |
| MP-3B-SKV18-D06 PROVE Sheets + H1 with BINDING FALLBACK `N` | ACCEPT | A Sheets shim ⇒ `N` (generalization NOT real), surfaced honestly, never paper-closed — the anti-narrowing fallback (3B:156). |
| MP-3B-SKV18-D07..D09 SK-V19 tee-up + §25 order + F.W5 un-fork | ACCEPT | Totality-tree leaks DEFERRED to SK-V19; F.W5 "nine seed grammars" held as the SK-V18→SK-V19 obligation, NOT read as already-satisfied. |
| MP-3B-SKV18-D10 CSS verdict UPGRADED (directionally-valid pending H1) | ACCEPT | The bare "measurement-valid" is gone; R03 stays closed. |

### 3C LOCKS (no JSON/CSS-narrowing amendment; Lock 14 holds)
| disposition / clause | verdict | basis |
|---|---|---|
| D-SKV18-L14-named-primitive-gate (LAC-1E-V5-01) | ACCEPT | Binds (a)-(d); keeps the hot kernel grammar-PARAMETERISED — the inverse of JSON narrowing. |
| D-SKV18-L05-L10-unfork (LAC-1E-V5-02, 2D-V3-01/02) | ACCEPT | `render(program)` dispatches on `backend_shape`; dispatch-on-source-family is REJECT — generality is the lock. |
| D-SKV18-L14-neutrality-proof (LAC-1E-V5-03, 2C-SK18-01/02) | ACCEPT | Forced-demotion + fleet-scoping in one clause; inner kernel may stay neutral; NO fleet-wide wording on <full-roster witness. |
| D-SKV18-L14-green-by-exclusion (LAC-1E-V5-06) | ACCEPT | The clause (3C-cryst:89 / binding hunk 3C-diff:74) carries the certified SPEC form byte-identically; the false "canonical" self-claim repaired. CH2-V3-R01 CLOSED. |
| D-SKV18-L13-pattern-h-recensus (LAC-1E-V5-07, 2C-SK18-03) | ACCEPT | 9-ident totality leak routed to STRUCTURAL full-row collapse over all 9 + widened regex at SK-V19; no narrowing; 71-file recensus verified. |
| D-SKV18-L01-cursor-generality (1A-LOCK1-AMEND-001, 2D-V3-03) | ACCEPT | Strikes phantom `<G>`, re-anchors on Cursor trait + config-breadth classifier; generality survives the delete. |
| L14-HC-07 future-grammar onboarding test | ACCEPT | Source/metadata-only; Sheets/BBNF-self fail on a generic branch / new directive / sixth shape. The onboarding test SURVIVES intact (3E:324-338). |
| No JSON/CSS-narrowing amendment introduced | CONFIRMED | The live Lock 14 clause (LOCKS:349) and all 11 SK-V18 clauses bind generality OUTWARD; zero clause narrows the lock to JSON or CSS. |
| 21/21 candidates disposed, 0 silent drops | CONFIRMED | 1E×7, 1A×1, 2C×3, 2D×4, 2E×3, 2F×3 = 21; tally 9 ACCEPT + 11 MODIFY + 0 REJECT + 1 DEFER = 21; the DEFER (LAC-2F-V3-03) names its re-entry trigger. |
| No directive / BIR / substrate / public API / sidecar / sixth shape | CONFIRMED | v+1 Executive Summary (3C-diff:43-45) and the live 5-arm `lower/mod.rs:18-24` confirm; FactStream stays output-plane, not a sixth shape. |

### 3D skinny-fold / 3F migration-handoff (generality cross-refs)
| delta | verdict | basis |
|---|---|---|
| 3D-D01 JSON-guard-scope (51/51 same-plane PoC, not fleet closure) | ACCEPT | Bars JSON from masquerading as generality proof. |
| 3D-D04 green-by-exclusion token set | ACCEPT | Carries the certified SPEC form (CH2-V3-R01 leg CLOSED; 3D:50 V3-FOLD consistency note confirms — 3D was the early SPEC-aligned carrier the cluster-A loci were back-ported onto). |
| 3D-D06/D09 + D11 named-primitive escape + Sheets negative-control (shim ⇒ `N`) | ACCEPT | Five-shape canon preserved; monotonic generalization bridge; fleet wording waits for the witness. |
| 3D-D10 PRUNE-before-REBUILD sequencing | ACCEPT | No cross-scope violation; T-P3 proposes only, dispatches no wave. |
| 3F-MH-005/006/008/011/012/013 HANDOFF scope + blocker matrix + phantom `<G>` DELETE + `css_types.rs` SK-V19 reroute | ACCEPT | Strikes the stale "SK-V18 adopts into crates/core" definition (HANDOFF:17-19); routes fleet onboarding + named generic-core mess to SK-V19 EXPLICITLY (not silently dropped, not narrowed). |

### 3E grammar-generalisation (the lens-primary artefact)
| delta | verdict | basis |
|---|---|---|
| 3E-D01..D11 (carried SK-V15 generality matrix) | ACCEPT | Non-JSON proof matrix (CSS L4 positive + Sheets/BBNF-self negative controls), per-grammar BackendShape matrix, primitive transfer, Lock 14 hardening clauses; concrete receivers. |
| 3E-D12 one-generator generalisation thesis | ACCEPT | Generality is an INPUT-SURFACE property (grammar source + metadata), md5-distinct from a neutral renderer; 2C:213 grounded; md5 carried as necessary-NOT-sufficient. |
| 3E-D13 named-primitive (a)-(d) neutrality discipline | ACCEPT | (a)-(c) prove grammar-COUPLING, (d) bounds SIZE; admissible ONLY under all four; 2C:214 grounded. |
| 3E-D14 css_balanced_component_scan FORCED demotion | ACCEPT | Base one-fan kernel structurally neutral; the two non-CSS dischargers are parse-with-emit, structurally incompatible (2C:215); the forced name IS the discharge. |
| 3E-D15 Sheets precedence-tower negative control | ACCEPT | 7-level tower lowers to existing `SinkOnlyExpr`; no relabeled courier can fake the recursive `CallRule`/`RepeatLoop` cascade; `Nu8`-tagged-alt correctly DEMOTED; 2C:216 + grammar:97-163 grounded. |
| 3E-D16 9-grammar BackendShape fleet matrix | ACCEPT | Roster verified (9 idents + 9 files); five-shape canon preserved; CollapsedStage in no cell (M5-Max-aarch64); math is a second precedence-tower witness; 2C:217 grounded. |
| 3E-D17 relocated-seam firewall + CSS second seam | ACCEPT | md5-distinct necessary-NOT-sufficient; extends generality to the CSS provider source channel; 2C:219 grounded. |
| 3E-D18 fleet-scoped neutrality wording | ACCEPT | Anti-overclaim: <full-roster witness ⇒ scoped wording; 6 remaining grammars are SK-V19 receivers; 2C:381 grounded. Prevents the fold being narrated as already-fleet-proven. |
| 3E onboarding-table token set (3E:337) | ACCEPT | Byte-identical to the binding hunk; the public `ValueRef` carries no live phantom `<G>` axis. |

## REJECTs

None. No uncited delta: every generality-bearing delta cites a 1E/1A/2C/2D/2E/2F
finding-id AND a verified on-disk anchor. No revived refuted route: the 8 REFUTED
constraints in the 3E frontmatter (tree-walk-preserves-94.1%,
find_css_significant-wire-as-is, neutral-name-on-one-grammar, checkasm-PASS-as-
speedup, x86/AVX-512-closes-a-row, eq-set-dual-consumer, md5-distinctness-alone,
bracket_depth_mask_64) are all carried as REFUTED; a positive-revival grep over
3A/3B/3E returns EMPTY. No silently-dropped candidate (3C disposes 21/21; the
DEFER names its re-entry trigger; `css_types.rs` is an EXPLICIT 3F SK-V19
migration row, not a silent drop). No cross-scope violation (every totality-tree
item routed to SK-V19; the monotonic skinny→totality fold preserved in 3D/3F). No
directive, BIR variant, substrate, public substrate API, retained sidecar, or
sixth `BackendShape` enters the packet (verified against the v+1 Executive Summary
and the live 5-arm `lower/mod.rs`). Lock 14 holds; no JSON/CSS-narrowing
amendment; the future-grammar onboarding test survives.

## REVISEs

None. The single CH2 defect entering V5 — **CH2-V3-R01** — was repaired in V4 and
is INDEPENDENTLY re-verified CLOSED in V5: the certified SK-V18 SPEC token form
`{GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}` is present across
all 7 artefacts (the BINDING hunk 3C-diff:74 included), the broad form survives
only in V3-FOLD changelog notes, the SPEC antecedent (SPEC:711-712, refining the
1E:152 `_RS` substring) resolves, the diff still applies clean (`git apply --check`
exit 0), and no live gate carries the broad form (`grep '⊇ {CSS_'` on live text =
EMPTY). No new generality defect was found on independent re-run.

Note on the cycle-V1 ≥30% REVISE expectation: that prior is a first-cycle
diversity heuristic. V5 is the ceiling confirmation cycle on byte-converged
evidence whose lone carried defect was repaired and re-verified clean across every
locus. Manufacturing a REVISE against byte-verified-converged generality would be
dishonest. The generality case is sound and the verdict is ACCEPT.

## Verdict and census

Lock 14 holds: the live LOCKS:349 clause binds generality OUTWARD and all 11
SK-V18 v+1 clauses extend it without a single JSON/CSS-narrowing amendment. The
substance of the generality case resolves end to end: the one-generator thesis
(2C:213), the named-primitive (a)-(d) discipline (2C:214), the css_balanced forced
demotion (2C:215), the Sheets precedence-tower negative control (2C:216 +
grammar:97-163, corroborated by math:1-16), the 9-grammar fleet matrix (9 idents +
9 files, 5-shape canon preserved), the relocated-seam firewall + CSS second seam
(2C:219), the fleet-scoped wording (2C:381), the 16-lock / 5-shape canon, the
21/21 candidate disposition, the explicit `css_types.rs` SK-V19 reroute, and the
surviving future-grammar onboarding test; and the v+1 diff applies clean. No
forbidden surface enters the packet. No boundary fault.

Census: ~46 dispositions judged under the CH2 lens (11 in 3A, 6 in 3B, 11 lock
dispositions + no-narrowing/onboarding/no-forbidden-surface confirmations in 3C,
6 in 3D/3F, 9 in 3E). All ACCEPT. Zero REVISE. Zero REJECT. The packet is
CH2-clean for SK-V18 T-P3 V5.

TALLY accept=46 revise=0 reject=0
