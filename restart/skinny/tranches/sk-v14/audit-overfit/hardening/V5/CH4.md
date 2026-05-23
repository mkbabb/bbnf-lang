# CH4 COST — S-P0 Overfit Audit Hardening V5 Disposition

Lens binding: `restart/prompts/ORCHESTRATOR.md §3W CH4` ("LOC budget,
risk class, wave alignment, and hard cap are stated and realistic;
same-wave consumer present per kernel/primitive"). V5 confirming pass
at the V max=5 ceiling per `audit-overfit/hardening/V4/HARDENING-S-P0-
V4-CONSOLIDATED.md §2.3` Option B recommendation. The V4 → V5 surface
landed as a single SYNTHESIS-only commit at `037eec6b6` carrying two
prose edits inside §2.4 (F-V5-SYNTHESIS-1a + F-V5-SYNTHESIS-1b) per
the §2.2 Option B prescription; A1 + A2 + A3 + A4 + A5 + A6 STAND
verbatim. V5 verifies (1) F-V5-SYNTHESIS-1a closes the V4 CH4 §4
"(siblings)" framing note at the workspace-crate level; (2) the C-1..
C-5 envelope chain holds verbatim across six consecutive lens cycles
(V5 alpha-hardening → V1 → V2 → V3 → V4 → V5); (3) F-V5-SYNTHESIS-1b
collapses the residual closing sentence without disturbing the
C-3 / LOCKS.md attribution split; (4) the V5 fresh-finding scan
returns zero CH4-axis residue.

The V5 micro-fold packet inventory (verified against commit
`037eec6b6` tail diff):

1. **F-V5-SYNTHESIS-1a** — `SYNTHESIS-AUDIT-OVERFIT.md §2.4:318`
   sibling-framing precision. V4 prose read "(the two path roots
   are siblings)" — flagged by CH4 V4 §4 as documentary-loose
   because the LINT-GLOB ROOTS themselves (`runtime/src/grammars`
   at 3-deep from `skinny/crates/` and `codegen/src` at 2-deep
   from `skinny/crates/`) are NOT siblings; only their respective
   WORKSPACE-CRATE PARENTS (`runtime/` + `codegen/`) are direct
   siblings at depth 1. V5 replaces with "(the two path roots sit
   under sibling workspace crates `skinny/crates/runtime/` and
   `skinny/crates/codegen/`; the lint glob brace-expansion is
   path-pattern-based, not structural-relation-based)". The
   load-bearing disjoint-set claim (42 + 14 = 56 with zero
   overlap) is preserved verbatim, untouched. Net +3 / -1 lines
   in §2.4:318-321.

2. **F-V5-SYNTHESIS-1b** — `SYNTHESIS-AUDIT-OVERFIT.md §2.4:319-321`
   residual closing sentence deletion. The V4 prose closed with
   "The lint glob `codegen/src/**/*.rs` catches all 14 regardless;
   only the prose count needs the 8-vs-14 distinction." — a V2 →
   V3 precision-repair back-reference made redundant by F-V4-
   SYNTHESIS-1's explicit `8 providers+templates + 6 ancillary`
   decomposition inline at line 312. V5 removes the back-reference
   clause outright; the brace-expansion glob mechanism remains
   declared at §2.4:307. Net +0 / -2 lines.

## §0 — Disposition summary

- **V5 ACCEPT-rate: 100 % (19 / 19 sectioned dispositions).**
- **V5 REJECT count: 0.**
- **V5 REVISE count: 0.**
- **Critical findings: 0.**
- **CH4 V5 verdict: ACCEPT.** F-V5-SYNTHESIS-1a closes the V4
  §4 "(siblings)" framing note at the workspace-crate level
  precisely as CH4 V4 §4 recommended (the note flagged that
  `runtime/` and `codegen/` are direct siblings at depth 1 while
  the LINT-GLOB ROOTS themselves are not; F-V5-SYNTHESIS-1a
  reframes the parenthetical to bind the sibling relation to the
  workspace-crate level — the precise structural truth — and
  appends the path-pattern-based-not-structural-relation-based
  clarification that explicitly reconciles the brace-expansion
  glob's path-pattern semantics with the workspace-crate sibling
  framing). The load-bearing disjoint-set arithmetic (42 + 14 =
  56) is preserved verbatim. F-V5-SYNTHESIS-1b collapses the
  redundant closing sentence — pure cohesion polish — without
  disturbing the brace-expansion glob's declared scope or either
  C-3 / LOCKS.md attribution path. The SK-V14 SYNTHESIS C-1..C-5
  LOC envelopes (2.8k–3.4k / 600–1.08k / 1.2k–2.0k / 800–1.4k /
  250–500; total 5.65k–8.38k) hold verbatim verified by direct
  read of `restart/skinny/tranches/sk-v14/SYNTHESIS.md:271-275`;
  the §3.1 coverage table arithmetic remains 41 + 7 + 11 + 4 +
  11 = 74 verified by direct read of `SYNTHESIS-AUDIT-OVERFIT.md:
  345-350`; the PRUNE-4 sub-wave count remains 9 with the same
  9 × 30 = 270-min cluster wall-clock; the V5 alpha-hardening
  F-V5-α-E-1 roster-count-agnostic phrasing continues to absorb
  the V5 micro-edits as documentary precision refinements.

### §0.1 — V5 cost-axis verification points

The V5 micro-fold dispatch carries two explicit verification points;
both ratified by independent re-execution at HEAD `037eec6b6`:

**Point (1): F-V5-SYNTHESIS-1a closes the V4 CH4 §4 framing note
precisely at the workspace-crate level.** VERIFIED via direct read
of `SYNTHESIS-AUDIT-OVERFIT.md:318-321` at HEAD `037eec6b6`:

```
   with zero overlap (the two path roots sit under sibling workspace
   crates `skinny/crates/runtime/` and `skinny/crates/codegen/`; the
   lint glob brace-expansion is path-pattern-based, not
   structural-relation-based).
```

The V4 prose "(the two path roots are siblings)" is replaced by
the precise workspace-crate-level framing. This binds the sibling
relation to the depth-1 parent crates (`runtime/` and `codegen/`)
which ARE in fact direct siblings under `skinny/crates/`, while
explicitly disclaiming any structural-relation interpretation of
the lint glob itself (the brace-expansion is path-pattern-based,
i.e. it matches by path-prefix string, not by structural relation
between path roots). The disjoint-set arithmetic underlying the
56 file count remains independently re-verified at HEAD:

```
$ git grep -l '@generated by skinny bbnf-codegen' \
    skinny/crates/runtime/src/grammars/ | wc -l
      42

$ git grep -l '@generated by skinny bbnf-codegen' \
    skinny/crates/codegen/src/ | wc -l
      14

$ git grep -l '@generated by skinny bbnf-codegen' \
    skinny/crates/runtime/src/grammars/ \
    skinny/crates/codegen/src/ | wc -l
      56
```

42 + 14 = 56 (combined-grep sum); zero overlap re-confirmed.
Cost-axis impact: the lint glob brace-expansion mechanism is
unchanged (still `{runtime/src/grammars,codegen/src}/**/*.rs`
per §2.4:307 + §2.4:314); F-V5-SYNTHESIS-1a edits only the
parenthetical-decoration prose at §2.4:318-321 to bind the
sibling framing to the workspace-crate level — the structurally
correct framing — without perturbing the glob's declared scope.
**C-3 envelope HELD at 1.2k–2.0k**; **LOCKS.md attribution HELD**;
the +5-10 LOC absorption path either way unchanged.

**Point (2): F-V5-SYNTHESIS-1b deletes the redundant closing
sentence without disturbing the brace-expansion glob declaration
or attribution paths.** VERIFIED via direct read of the V4 → V5
diff at `037eec6b6`:

```
-   with zero overlap (the two path roots are siblings).
-   The lint glob `codegen/src/**/*.rs` catches all 14 regardless;
-   only the prose count needs the 8-vs-14 distinction. A runtime-only
+   with zero overlap (the two path roots sit under sibling workspace
+   crates `skinny/crates/runtime/` and `skinny/crates/codegen/`; the
+   lint glob brace-expansion is path-pattern-based, not
+   structural-relation-based). A runtime-only
```

The deleted sentence (V4 "The lint glob `codegen/src/**/*.rs`
catches all 14 regardless; only the prose count needs the 8-vs-14
distinction.") was a V2 → V3 precision-repair back-reference made
redundant by F-V4-SYNTHESIS-1's inline `8 providers+templates +
6 ancillary` decomposition at §2.4:312. The brace-expansion glob
remains declared at §2.4:307 + §2.4:314; the C-3 sub-task LOC
absorption envelope is unchanged; the LOCKS.md amendment
alternative is unchanged. Cost-axis impact: documentary cohesion
only — zero envelope drift, zero attribution-path drift, zero
mechanism drift. **C-3 envelope HELD at 1.2k–2.0k**; **LOCKS.md
zero-source alternative HELD**.

**Point (3, ratification): A1, A2, A3, A4, A5, A6 STAND verbatim
under V5 — V5 introduces zero axis-file edits.** VERIFIED via
direct diff at HEAD `037eec6b6`:

```
$ git diff f8e279877..037eec6b6 --stat \
    -- "restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-*.md"
(empty — zero bytes touched on all six axis files)

$ git diff f8e279877..037eec6b6 --stat \
    -- restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
 .../audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md | 7 ++++---
 1 file changed, 4 insertions(+), 3 deletions(-)
```

V5 commit roster is **SYNTHESIS-only** (zero axis touches), per
the V4 CONSOLIDATED §3 forecast ("Single SYNTHESIS V5 agent. Two
atomic prose edits at SYNTHESIS-AUDIT-OVERFIT.md §2.4:318 +
§2.4:319-321. ~5 minutes total edit time. A1 + A2 + A3 + A4 +
A5 + A6 STAND verbatim at V5"). The A1 8-finding ledger, A2
9-finding ledger, A3 30-finding ledger (11C/6H/5M/8L), A4
16-finding ledger, A5 4-finding ledger, A6 7-finding ledger all
map to the same C-1..C-5 cells they mapped to under V1 + V2 +
V3 + V4 ACCEPT — six consecutive cycles of axis-roster stability
(if the V5 alpha-hardening pass is included as the zeroth lens-
ratification cycle, the chain extends to six total cycles of
envelope stability).

### §0.2 — Six-cycle envelope stability now established

The SK-V14 SYNTHESIS C-1..C-5 LOC envelopes are now verified
verbatim across six consecutive cycles:

| Cycle | Verdict | Cross-cycle delta on C-1..C-5 |
| --- | --- | --- |
| **V5 alpha-hardening** | ACCEPT | α-E §2 ratified 5.65k–8.38k total |
| **V1** (S-P0 challenge) | ACCEPT | synthesis §3 C-1..C-5 verbatim from SK-V14 SYNTHESIS |
| **V2** | ACCEPT | 11 V2 fold packets envelope-neutral; co-derivation note REDUCES risk-weighting on PRUNE-4 |
| **V3** | ACCEPT | 5 V3 micro-folds envelope-neutral; two cross-flag closures (CH1 line-count, CH5 codegen prose precision) |
| **V4** | ACCEPT | 2 V4 micro-folds envelope-neutral; two orphan-REVISE closures (CH2 inclusion-relation, CH7 §1.5 inheritance-miss) |
| **V5** | ACCEPT | 2 V5 micro-folds envelope-neutral; CH4 §4 sibling framing closure + CH2 §4.2 cohesion polish |

The six-cycle chain operates as institutional evidence that the
SK-V14 prune-list cost surface is structurally stable — every
micro-fold cycle since V5 alpha-hardening has refined audit-
document precision without perturbing the C-1..C-5 envelopes,
the §3.1 coverage arithmetic, the PRUNE-4 sub-wave count, the
wall-clock caps, the §2.4 CH7-companion attribution paths, or
the brace-expansion lint glob mechanism. The S-P3 plan work that
consumes this surface can rely on the 5.65k–8.38k total LOC
budget, the 9-sub-wave PRUNE-4 size, the 30-min sub-wave wall-
clock cap, and the C-3 OR LOCKS.md attribution split as the
binding-stable inputs.

## §1 — Per-fold disposition table

| Fold | V4 → V5 change | V5 disposition | Cost-axis notes |
|---|---|---|---|
| **F-V5-SYNTHESIS-1a** | SYNTHESIS §2.4:318 closing parenthetical: `(the two path roots are siblings).` → `(the two path roots sit under sibling workspace crates skinny/crates/runtime/ and skinny/crates/codegen/; the lint glob brace-expansion is path-pattern-based, not structural-relation-based).` | ACCEPT | Closes V4 CH4 §4 informational note precisely; binds the sibling framing to the workspace-crate level (depth-1 parent crates `runtime/` + `codegen/` ARE direct siblings) and adds the path-pattern-vs-structural-relation disclaimer that reconciles the brace-expansion glob's path-pattern semantics with the sibling framing; load-bearing 42 + 14 = 56 disjoint-set arithmetic preserved verbatim; brace-expansion glob mechanism unchanged at §2.4:307 + §2.4:314; **C-3 envelope HELD at 1.2k–2.0k**; **LOCKS.md attribution HELD**. |
| **F-V5-SYNTHESIS-1b** | SYNTHESIS §2.4:319-321 closing sentence: `The lint glob codegen/src/**/*.rs catches all 14 regardless; only the prose count needs the 8-vs-14 distinction.` deleted | ACCEPT | Closes CH2 V4 §4.2 cohesion polish note; the V2 → V3 precision-repair back-reference is made redundant by F-V4-SYNTHESIS-1's inline `8 providers+templates + 6 ancillary` decomposition at §2.4:312; brace-expansion glob remains declared at §2.4:307 + §2.4:314; documentary cohesion only; **C-3 envelope HELD**; **LOCKS.md alternative HELD**; **+0 LOC delta in deletion target**. |
| **§3.1 coverage table arithmetic** | 41 + 7 + 11 + 4 + 11 = 74 | ACCEPT | Unchanged from V1 + V2 + V3 + V4; per-finding mapping intact; zero orphan; F-V5-SYNTHESIS-1a + F-V5-SYNTHESIS-1b touch §2.4 prose only, not the §3.1 count column. |
| **SK-V14 SYNTHESIS C-1..C-5 envelopes** | 2.8k–3.4k / 600–1.08k / 1.2k–2.0k / 800–1.4k / 250–500 | ACCEPT | Verified by direct read of `restart/skinny/tranches/sk-v14/SYNTHESIS.md:271-275`; total ≈ 5.65k–8.38k preserved; **six-version stable now** (V5 alpha → V1 → V2 → V3 → V4 → V5). |
| **C-1 PRUNE-4 sub-wave count** | 9 sub-waves (css_pretty included) | ACCEPT | F-V2-SYNTHESIS-2 co-derivation note continues to reduce risk-weighting (one evidence + three observer confirmations); 9 × 30 = 270-min cluster wall-clock HELD; **C-1 envelope HELD at 2.8k–3.4k**. |
| **§2.1 R4 → PRUNE-2 sequencing** | Unchanged | ACCEPT | Binding text intact; no V5 fold touched the A4 §4 quote or the SYNTHESIS §2.1 verbatim re-citation; envelope-neutral. |
| **§2.2 C-1 → C-4 sequencing** | Unchanged | ACCEPT | A5 §4.1 quote intact; F-V3-A5-1 + F-V3-SYNTHESIS-1 C-4 entry-gate invariant phrasing carries forward into V5 without revision; envelope-neutral. |
| **§2.3 PRUNE-4 sub-wave count = 9 not 8** | Unchanged | ACCEPT | A3 / A5 / A6 cross-confirms intact; F-V2-SYNTHESIS-2 co-derivation rationale preserved; 270-min cluster wall-clock unchanged. |
| **§2.4 CH7-companion attribution paths** | F-V5-SYNTHESIS-1a refines parenthetical decoration; F-V5-SYNTHESIS-1b deletes redundant closing sentence; brace-expansion lint glob mechanism unchanged | ACCEPT | Both attribution paths (C-3 200-LOC validator slice OR LOCKS.md amendment) HELD; the brace-expansion glob declared at §2.4:307 + re-cited at §2.4:314 covers both roots; **C-3 envelope HELD**; **LOCKS.md zero-source alternative HELD**. |
| **A1 / A2 / A3 / A4 / A5 / A6 unchanged** | STAND verbatim per V5 commit roster | ACCEPT | Verified via `git diff f8e279877..037eec6b6 --stat -- "…sk-v14-audit-overfit-*.md"` returning **empty** for all six axis files; all 8+9+30+16+4+7 = 74 findings map to same C-N cells as V1+V2+V3+V4 ACCEPT; envelope-neutral. |
| **C-5 → C-4 sequencing edge** | Unchanged from V4 reinforcement | ACCEPT | Sequencing edge binding through SK-V14 SYNTHESIS §0.3 R3 ("PRUNE waves before any new admit attempt"); V5 prose touches the §2.4 paragraph only, not the §3.1 sequencing manifest; envelope-neutral. |
| **Risk-weighting on PRUNE-4** | Preserved through V5 | ACCEPT | Three observer confirmations of one evidence (`css_pretty` addition); higher-confidence single delta; S-P3 can plan against without defensive over-allocation; **+30-min cluster wall-clock HELD**. |
| **V3 + V4 cross-flag closures (CH1 + CH5 + CH2 + CH7)** | All closed under V3 + V4; V5 does not re-open | ACCEPT | F-V3-A4-1 (CH1 line-count), F-V3-SYNTHESIS-2 (CH5 codegen prose precision), F-V4-SYNTHESIS-1 (CH2 inclusion-relation), F-V4-SYNTHESIS-2 (CH7 §1.5 inheritance-miss) all verified RETIRED in V4 CH4 §3.2; V5 makes no edits that would re-open any; envelope-neutral. |
| **V4 → V5 orphan REVISE closures (CH2 + CH4)** | F-V5-SYNTHESIS-1b closes CH2 §4.2 cohesion polish; F-V5-SYNTHESIS-1a closes CH4 §4 sibling framing | ACCEPT | Both V4 sub-threshold notes were single-paragraph SYNTHESIS prose touches classified by surfacing lens as below fatal threshold (CH2 cohesion polish; CH4 documentary decoration); both close cleanly under the §3.1 finding-count column unchanged and the §2.4 lint glob mechanism unchanged; envelope-neutral. |
| **Workspace-crate sibling framing precision** | V5 binds the sibling relation to depth-1 workspace crates (`runtime/` + `codegen/`); explicitly disclaims structural-relation interpretation of the path-pattern-based brace-expansion glob | ACCEPT | The CH4 V4 §4 note flagged exactly this framing imprecision; F-V5-SYNTHESIS-1a addresses it at the load-bearing prose surface; the structural truth (depth-1 parents ARE direct siblings; lint glob roots themselves are at differing sub-depths) is now explicit and the path-pattern-vs-structural-relation disclaimer makes the load-bearing nature of the brace-expansion glob's path-pattern semantics explicit. |
| **§3.1 §3.2 §3.3 no C-6 candidate** | Re-affirmed at line 363 ("No C-6 candidate is required") | ACCEPT | V5 introduces neither a new C-N candidate nor a new wave; the two micro-folds are entirely within the SYNTHESIS §2.4 prose surface; sub-wave count summary at §3.3 (PRUNE-1 / PRUNE-2 / PRUNE-3 / PRUNE-4 = 9 / PRUNE-5) unchanged. |
| **F-V5-α-E-1 roster-count-agnostic phrasing** | Continues to absorb V5 micro-edits | ACCEPT | V5 introduces no count-column re-arithmetic on the §3.1 finding totals; both micro-folds are decorative-prose / cohesion-polish refinements that the F-V5-α-E-1 phrasing already absorbs as documentary precision. |
| **Total V5 LOC change** | +4 / -3 (single file: SYNTHESIS-AUDIT-OVERFIT.md) | ACCEPT | Zero source LOC touched; 100 % audit-documentation prose; concentrated entirely within one paragraph (§2.4:318-321); minimal-surface fold roster; matches V4 commit shape (SYNTHESIS-only, single-paragraph). |
| **CH1 §3 style observation explicitly NOT folded** | V3 CONSOLIDATED §2.1 explicitly authorised both reading variants (compound Option B reading stands per V4 CONSOLIDATED §2.3) | ACCEPT | V5 commit message records "CH1 sec 3 style observation EXCLUDED (V3 CONSOLIDATED sec 2.1 authorised both reading variants; Option B compound reading stands)" — V5 honours the V3 dispensation; envelope-neutral; no fold-scope drift. |

## §2 — Critical findings

**None.** No CH4-axis CRITICAL surfaces among the two V5 micro-fold
packets.

The cost surface is intact under V5: both V5 folds are documentary-
precision refinements (sibling framing rebinding from path-root
level to workspace-crate level; redundant closing-sentence deletion).
Neither fold perturbs any C-N envelope, the §3.1 coverage
arithmetic, the PRUNE-4 sub-wave count, the 30-min wall-clock cap
per sub-wave, the §2.4 CH7-companion attribution paths, or the
lint glob brace-expansion mechanism. The V5 commit roster is
**SYNTHESIS-only** per the V4 CONSOLIDATED §3 forecast; the V5
axis roster predicted "A1 + A2 + A3 + A4 + A5 + A6 STAND verbatim
at V5" landed exactly as forecast.

Six-version unbroken envelope-stability chain:

- **V5 alpha-hardening** ACCEPT — α-E §2 ratified 5.65k–8.38k total.
- **V1** ACCEPT — synthesis §3 C-1..C-5 verbatim from SK-V14
  SYNTHESIS.
- **V2** ACCEPT — 11 V2 fold packets envelope-neutral; co-derivation
  note REDUCES risk-weighting on PRUNE-4.
- **V3** ACCEPT — 5 V3 micro-fold packets envelope-neutral; two
  cross-flag closures retire V2 hand-off concerns (CH1 + CH5).
- **V4** ACCEPT — 2 V4 micro-fold packets envelope-neutral; two
  orphan-REVISE closures retire V3 hand-off concerns (CH2
  inclusion-relation + CH7 §1.5 inheritance-miss).
- **V5** ACCEPT — 2 V5 micro-fold packets envelope-neutral; two
  V4 sub-threshold note closures (CH2 §4.2 cohesion polish + CH4
  §4 sibling framing).

## §3 — V6 fold recommendations

**None.** CH4 V5 returns 100 % ACCEPT; neither V5 fold introduces
LOC/risk/cap drift; SK-V14 SYNTHESIS C-1..C-5 hold verbatim across
six lens cycles; both V4 sub-threshold notes concerning CH4 + CH2
are closed by F-V5-SYNTHESIS-1a and F-V5-SYNTHESIS-1b respectively;
the CH1 §3 style observation remains explicitly out-of-scope per
the V3 CONSOLIDATED §2.1 dispensation honoured by the V5 commit
message. **Per V max=5 ceiling there is no V6 cycle**; this V5
confirming pass is the chain terminator.

### §3.1 — Informational notes (below action threshold; carried from V4 unchanged)

The two documentary propagations from V4 CH4 §3.1 (themselves
carried from V3) remain unchanged under V5 — neither is a cost
expansion, both are S-P3 plan-time propagations:

1. **SK-V14 SYNTHESIS §3 line 271 + §2 line 95** continues to cite
   "8 sub-waves" + "64 hand-written" (the audit-overfit SYNTHESIS
   at §2.3 + §3.3 already updates this to 9 / 67 for the
   `css_pretty` addition; S-P3 propagates the update with the
   +30-min wall-clock delta acknowledged within the existing C-1
   2.8k–3.4k envelope). V5 introduces no new documentary
   propagation requirement.

2. **§2.4 CH7-companion attribution** remains open between C-3
   sub-task (1.2k–2.0k headroom absorbs the +5-10 LOC lint glob
   walker extension) OR LOCKS.md amendment (zero source LOC for
   the companion lint; check-X subcommand pairing stays in C-3
   either way). CH4 V1 §3.1 + V2 §0.1 point 2 + V3 §0.1 point 3
   + V4 §0.1 point 1 + V5 §0.1 point 1 all preserve both
   attribution paths as envelope-neutral; S-P3 chooses at plan
   time.

### §3.2 — Cross-lens hand-off (V4 carry-overs closed; V5 introduces zero)

V5 closes both V4 sub-threshold notes inline:

- **CH4 §4 sibling framing:** F-V5-SYNTHESIS-1a retires the V4 §4
  informational note; the sibling framing is now bound to the
  workspace-crate level (`runtime/` + `codegen/` ARE direct
  siblings at depth 1) with explicit path-pattern-vs-structural-
  relation disclaimer; the load-bearing 42 + 14 = 56 disjoint-set
  arithmetic is preserved. CH4 V5 disposes the concern as CLOSED.

- **CH2 §4.2 cohesion polish:** F-V5-SYNTHESIS-1b retires the V4
  §4.2 informational note; the redundant closing sentence is
  deleted; the brace-expansion glob remains declared at §2.4:307
  + §2.4:314; the F-V4-SYNTHESIS-1 inline `8 providers+templates
  + 6 ancillary` decomposition at §2.4:312 already states the
  breakdown the deleted sentence was clarifying. CH2 V5 may
  dispose the concern as CLOSED.

Remaining V3 + V4 cross-lens bindings (CH1, CH3, CH5, CH6, CH7)
unchanged under V5 — each was envelope-neutral at V4 and remains so:

- **CH1 (CORRECTNESS):** F-V3-A4-1 line-count refresh holds; V5
  introduces zero new citations or counts that would require CH1
  re-verification beyond the F-V5-SYNTHESIS-1a parenthetical
  rewrite (already re-executed under §0.1 Point 1 above with the
  three `git grep` quotes returning 42, 14, 56 byte-identically
  to V4). The CH1 §3 style observation is explicitly out-of-scope
  per V3 CONSOLIDATED §2.1 dispensation.
- **CH3 (REGRESSION):** No V5 fold reclassifies a finding severity,
  re-opens a V13 audit-CLEAN route, or silently reverses a V13
  REDRESS REJECT; the 74-finding aggregate and severity distribution
  (31C / 20H / 12M / 11L) hold verbatim through six cycles.
- **CH5 (HIDDEN COUPLING):** The V4 substrate-union refusal
  strengthening (V4 CONSOLIDATED §3.3 — F-V4-SYNTHESIS-1's
  disjoint-set framing reinforced the parallel-substrate boundary
  by making the runtime-side / codegen-side population
  disjointness explicit) carries forward under V5; F-V5-SYNTHESIS-1a
  preserves the disjoint-set arithmetic verbatim and adds a
  path-pattern-vs-structural-relation disclaimer that strengthens
  rather than weakens the substrate-union refusal posture (the
  brace-expansion glob's path-pattern semantics are now explicit,
  precluding misreadings about implicit structural unification).
- **CH6 (ANTI-PAPER-CLOSE):** V4 F-V4-SYNTHESIS-1 + F-V4-SYNTHESIS-2
  paired V3-orphan closure pattern (which CH6 V4 §3.4 named the
  "institutional anti-paper-close exemplar") continues at V5 with
  the V4 sub-threshold notes closed at source rather than
  deferred — the V5 commit explicitly chose Option B per the V4
  CONSOLIDATED §2.3 recommendation, matching the V5 alpha-
  hardening precedent. CH6 V5 should re-confirm this pattern at
  the same 100 % rate.
- **CH7 (OVERFIT-PRUNE):** V4 closed the §1.5 inheritance-miss
  cross-flag; V5 introduces no new audit-self-overfit risk; the
  V5 micro-folds are documentary-precision refinements within
  the existing §2.4 surface, not new audit content.

### §3.3 — Six-cycle pattern observation (informational; CH4 has no fold action)

The V2 → V3 → V4 → V5 sequence has institutionalised two
patterns:

1. **Inheritance-remediation pattern** (action-class column edits
   and count-column edits must be simultaneously propagated into
   all per-paragraph closing summaries within the same axis file
   AND all cross-axis summary tables that cite the same finding) —
   originally surfaced by CH6 V3 §4 rec 2 as a forward-looking
   §3W companion gate addition.

2. **Belt-and-braces sub-threshold closure pattern** — V4 + V5
   demonstrate that sub-fatal informational notes should close at
   the immediately-next available cycle rather than deferring to
   later docs hygiene. V5 alpha-hardening established this
   precedent (per V4 CH6 §3.4 "positive exemplar"); V5 here
   reinforces it for the audit-overfit cycle. The cost differential
   is trivial (single SYNTHESIS V5 agent dispatch with two atomic
   prose edits ~5 min) and the institutional benefit is closure
   at source rather than burdening downstream SK-V{N+1} cycles.

From the CH4 cost-axis perspective, both patterns are **envelope-
neutral by construction** — every instance surfaced so far (V2
action-class; V3 8→14 codegen count + 3→4 scanner count; V4
inclusion-relation + 3→4 C-3 cell; V5 sibling framing + closing
sentence deletion) is a documentary-precision refinement, not a
count-column re-arithmetic. The §3.1 finding-count column (41 /
7 / 11 / 4 / 11 = 74) has held verbatim through six cycles
regardless of these prose refinements. The CH4 forward-binding
for SK-V{N+1} S-P0 cycles is that the institutional pattern stays
cost-neutral so long as fold rosters address prose-cell drift
only and never spill into count-column re-arithmetic; if a future
cycle ever surfaces a count-column inheritance miss (i.e. a §3.1
cell whose number itself is wrong), CH4's envelope-neutrality
posture would need re-evaluation.

## §4 — Fresh-finding scan

Per V5 dispatch scope ("Fresh-finding scan"), the V5 micro-fold
inventory was scanned for any new CH4-axis concern not surfaced
under V1, V2, V3, or V4:

- **LOC drift:** None. Both V5 folds are single-paragraph in-place
  edits (F-V5-SYNTHESIS-1a = +3 / -1 inside §2.4:318-321;
  F-V5-SYNTHESIS-1b = +0 / -2 closing-sentence deletion). The
  +4 / -3 commit-stat total is concentrated entirely in one
  paragraph; zero source LOC touched.
- **Risk-class drift:** None. No V5 fold reclassifies a finding
  severity. The V2 F-V2-A3-1 H3 HIGH→LOW reclassification
  remains; no V3, V4, or V5 reclassifications exist. No new
  VERY-HIGH risk surface introduced. The V5 sibling-framing
  refinement strengthens rather than perturbs the brace-expansion
  glob's load-bearing posture.
- **Hard-cap drift:** None. The 30-min sub-wave wall-clock cap
  remains bound to the C-1 PRUNE-4 9-sub-wave cluster at 270-min
  total. No V5 fold proposes a new wave; the 2 micro-folds are
  entirely within the audit-documentation prose surface.
- **CH7-companion attribution drift:** None. Both attribution paths
  (C-3 sub-task or LOCKS.md amendment) preserved verbatim;
  F-V5-SYNTHESIS-1a strengthens the prose precision around the
  brace-expansion glob's structural framing without changing the
  glob mechanism or LOC absorption envelope; F-V5-SYNTHESIS-1b
  collapses a redundant clause without disturbing either path.
- **Same-wave consumer presence:** Unchanged. C-1 → C-4 sequencing
  edge ensures W8/W9 consumers (per SK-V14 SYNTHESIS §3 C-4
  falsifiability gate) land in the same wave as the W8/W9
  scaffold-to-load-bearing wiring; F-V3-A5-1 + F-V3-SYNTHESIS-1
  gate-rejection invariant inside C-4 entry-gates carries through
  V5 unchanged.
- **C-1..C-5 envelope arithmetic:** Unchanged. The 2.8k–3.4k /
  600–1.08k / 1.2k–2.0k / 800–1.4k / 250–500 budget verifies
  byte-identically against `SYNTHESIS.md:271-275` at HEAD
  `037eec6b6` (no source touched; the SYNTHESIS file itself was
  not modified by V5).
- **§3.1 coverage arithmetic:** Unchanged. 41 + 7 + 11 + 4 + 11 =
  74 verified by direct read of `SYNTHESIS-AUDIT-OVERFIT.md:
  345-350`; F-V5-SYNTHESIS-1a + F-V5-SYNTHESIS-1b touch §2.4
  prose only, not the §3.1 count column (still 11 in the C-3 row).
- **Brace-expansion glob mechanism:** Unchanged. `{runtime/src/
  grammars,codegen/src}/**/*.rs` remains declared at §2.4:307;
  the verification-quote re-citation at §2.4:314 unchanged;
  F-V5-SYNTHESIS-1a's path-pattern-vs-structural-relation
  disclaimer reinforces the glob's path-pattern semantics
  explicitly but introduces no new glob, no new lint mechanism,
  and no new attribution path.
- **Workspace-crate framing precision:** This V5 micro-edit lands
  CLOSED-AT-SOURCE for the V4 CH4 §4 informational note; the
  workspace-crate sibling framing (`runtime/` + `codegen/` ARE
  direct siblings at depth 1) is now the prose-binding framing,
  matching the structural truth precisely.
- **CH1 style observation residue:** V3 CONSOLIDATED §2.1
  dispensation honoured per V5 commit message — the triple-nested
  parenthetical + double-em-dash style note at §2.4:307-314
  remains as a no-action item; CH4 records this as
  non-cost-axis-relevant.

**Zero fresh CH4-axis findings.**

## §5 — Summary

CH4 V5 verdict: **ACCEPT** at 100 % (19 / 19 sectioned dispositions).

Both V5 micro-fold packets (SYNTHESIS × 2 inside §2.4) are
envelope-neutral on the CH4 cost surface. F-V5-SYNTHESIS-1a closes
the V4 CH4 §4 sibling framing note precisely at the workspace-
crate level: the parenthetical now binds the sibling relation to
`skinny/crates/runtime/` and `skinny/crates/codegen/` (which ARE
direct siblings at depth 1, matching the structural truth) and
appends a path-pattern-vs-structural-relation disclaimer that
reconciles the brace-expansion glob's path-pattern semantics with
the workspace-crate sibling framing; the load-bearing 42 + 14 =
56 disjoint-set arithmetic is preserved verbatim. F-V5-SYNTHESIS-1b
collapses the redundant V2 → V3 precision-repair back-reference
made obsolete by F-V4-SYNTHESIS-1's inline `8 providers+templates
+ 6 ancillary` decomposition at §2.4:312; pure cohesion polish
without disturbing the brace-expansion glob's declared scope or
either attribution path.

The §3.1 coverage table arithmetic preserves 41 + 7 + 11 + 4 +
11 = 74; the SK-V14 SYNTHESIS C-1..C-5 LOC envelopes (2.8k–3.4k /
600–1.08k / 1.2k–2.0k / 800–1.4k / 250–500; total 5.65k–8.38k)
hold verbatim verified by direct read of `restart/skinny/tranches/
sk-v14/SYNTHESIS.md:271-275`; the PRUNE-4 sub-wave count remains
9 with the same 9 × 30 = 270-min cluster wall-clock; the V5
alpha-hardening F-V5-α-E-1 roster-count-agnostic phrasing
continues to absorb every documentary refinement (V2 per-row vs
per-category, V3 fixture-count 3→4 in §1.1 + codegen prose
precision 8→14, V4 inclusion-relation 42+14=56 + final C-3 cell
3→4, V5 sibling framing + closing-sentence deletion) without
re-arithmetic.

Two V4 sub-threshold notes close cleanly under V5:

- **CH4 §4 sibling framing** (V4 informational; "(the two path
  roots are siblings)" was loose because the lint-glob ROOTS
  themselves are at differing sub-depths while their workspace-
  crate PARENTS are direct siblings) — closed by F-V5-SYNTHESIS-1a
  amending the prose to "(the two path roots sit under sibling
  workspace crates `skinny/crates/runtime/` and `skinny/crates/
  codegen/`; the lint glob brace-expansion is path-pattern-based,
  not structural-relation-based)". The disjoint-set arithmetic
  is preserved verbatim and independently re-verified at HEAD
  (`git grep` returns 42 + 14 = 56 with zero overlap).

- **CH2 §4.2 cohesion polish** (V4 informational; V2 → V3
  precision-repair back-reference made redundant by F-V4-
  SYNTHESIS-1's inline `8 providers+templates + 6 ancillary`
  decomposition) — closed by F-V5-SYNTHESIS-1b deleting the
  closing sentence outright; brace-expansion glob remains
  declared at §2.4:307 + §2.4:314.

The V5 commit roster is **SYNTHESIS-only** (zero axis touches;
A1 + A2 + A3 + A4 + A5 + A6 STAND verbatim per `git diff
f8e279877..037eec6b6 --stat -- "…sk-v14-audit-overfit-*.md"`
returning empty). The six-version unbroken envelope-stability
chain (V5 alpha-hardening → V1 → V2 → V3 → V4 → V5) operates as
institutional evidence that the SK-V14 prune-list cost surface
is structurally stable; S-P3 plan work consuming this surface
can rely on the 5.65k–8.38k total LOC budget, the 9-sub-wave
PRUNE-4 size, the 30-min sub-wave wall-clock cap, and the C-3
OR LOCKS.md attribution split as binding-stable inputs.

No V6 cycle exists (V max=5 ceiling); this V5 confirming pass
lands at exactly the ceiling per V4 CONSOLIDATED §3.1 strict-
reading forecast. The V4 + V5 chain satisfies §3Z's "≥ 95 %
ACCEPT × 2 consecutive cycles, no orphan REVISEs" — V4 cleared
the floor at 100 % with zero NEW orphans; V5 here clears the
floor at 100 % with zero NEW findings (the V4 sub-threshold
notes closed at source under Option B). The strict-reading chain
closure stands at V4 + V5 → §3Z LOCK → G-S-P0-CONVERGED → S-P1
dispatch per the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.
