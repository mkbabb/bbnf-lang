# CH3 REGRESSION — SK-V18 T-P3 (cycle V5)

## Lens

No delta re-opens a `skinny/REDRESS.md` route; 3B revives no refuted wave; 3D
promotes no rejected route; 3C weakens no lock that REDRESS strengthened; no
delta revives one of the 13 T-P2-refuted assertions. Spot-verify the most
load-bearing deltas (a cited finding-id resolves; a cited LOCKS section exists;
the v+1 diff applies).

## Verdict Summary

Under the REGRESSION lens the V5 fold packet is disciplined and every conjunct
holds at HEAD. The V4 fold (`CH1-V4-001`, the out-of-range `V2/CH4.md` citation)
landed: 3A now cites `restart/audit/totality/p3/hardening/V2/CH4.md:36`, which is
in range (the file is 168 lines; :36 is the `3B-D04` cost-reconcile RESOLVED row
the V3 fold rests on). All other REGRESSION conjuncts re-verify clean.

ONE narrow REVISE survives — and it is a CH3-CHAIN evidence-honesty defect, not a
route regression. The V4 CH3 verdict and the V4 consolidated stale-pattern note
both record the evidence-output `grep -nE '67/67' 3E … returns ZERO`. At HEAD
that grep returns **2** (`3E:48`, `3E:154`). Both occurrences are inside the
RE-KEY/discharge clauses that DECLARE the absolute-67 invariant superseded by the
SK-V18 recensus (per-file `@generated` provenance over the live 71), so the
SEMANTIC conclusion of CH3-V3-R1's discharge is correct and the Pattern-H
regression remains closed. But the LITERAL "returns ZERO" output carried in the
CH3 chain is false at HEAD, and a converging lock must not crystallise on a stale
evidence literal. The V5 CH3 evidence line is re-keyed below to the TRUE output:
`67/67` returns 2, both inside discharge clauses, neither a live invariant.
This is the single REVISE (`CH3-V5-R1`); it does not reopen any route.

Every REDRESS span resolves to its asserted rejected shape (items 51/53/246/247
at `REDRESS:742/784/6184/6230`; the 96/97/98 host block at `:2795`-`2944` with the
scalar-cheaper-than-SIMD finding at `:2880`+). The un-fork negative-witness is
ZERO at HEAD (`rg -ic 'relocated.seam|RuntimeEmitterKind|un.fork' skinny/REDRESS.md
== 0`) — the un-fork owes no REDRESS id, it is SK-V18-NOVEL. The SK-V16/V17
reconcile stays RETIMED as a Pass-Omega-V6 / pre-W-PRUNE blocker that BLOCKS
G2/G4/G6 entry, mirrored consistently (3D=3, 3B=2, 3F=2). The RETIRED REDRESS
96/97/98 prior is carried FORWARD as a CollapsedStage promotion bar (3C-L10
`:92`/`:113`; 3A-D08 `:214`) — strengthening, not weakening. All 13
T-P2-refuted assertions (2C:307-315 + the SK-V18-2C inverse rows :222-224) are
carried as gates/fences; none revived. The mandatory `3C-locks-v+1-diff.md`
spot-check PASSES (`git apply --check` exit 0; header `@@ -622,6 +622,33 @@`
context-matches the LOCKS NEON-classifier clause at `:622` and the governance
heading at `:625`). 16 locks and 5 BackendShape variants intact.

The lone "that is REVISED" promotion in 3D (3D:128, Decision Engine) un-rejects a
PRIOR-CYCLE TOTALITY-DOC scaffold classification on cited T-P1 D-10/G-3 evidence;
the Decision Engine is a REDRESS-LANDED win (W7/W8/W9 lowerers, REDRESS:6326-6414),
NOT a REDRESS-rejected route — `rejected: []` in the 3D frontmatter, five-shape
canon preserved. It is the sanctioned monotonic skinny-WIN→V1-authoritative
direction, not a regression-lens violation.

The cycle-V1 `>=30% REVISE` expectation was authored for the divergent V1 cycle
and does not bind a converged V5 whose only defect is a stale evidence literal.
Manufacturing additional REVISEs against discharged evidence would itself be a
dishonesty this lens exists to catch.

## Evidence Commands And Outputs

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
awk '/^```diff$/{f=1;next} f&&/^```$/{exit} f{print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v5.diff
git apply --check /tmp/tp3-locks-v5.diff ; echo "apply exit=$?"
grep -nE '67/67' restart/audit/totality/p3/3E-grammar-generalisation.md
rg -ic 'relocated.seam|RuntimeEmitterKind|un.fork' skinny/REDRESS.md
sed -n '742p;784p;6184p;6230p;2795p' skinny/REDRESS.md
rg -nc 'G2/G4/G6 entry is BLOCKED until the SK-V16/V17' \
  restart/audit/totality/p3/{3B-master-plan-reconciliation,3D-skinny-fold,3F-migration-handoff}.md
```

```text
16                                   # 16 numbered locks (canon preserved)
71                                   # runtime Pattern-H census (NOT 67; +4 disclosed)
apply exit=0                         # mandatory v+1 diff applies cleanly
67/67 -> 2 hits (3E:48, 3E:154)      # BOTH inside RE-KEY/discharge clauses, NOT live invariants
                                     #   (V4 CH3 asserted "returns ZERO" — STALE; this is CH3-V5-R1)
un-fork negative-witness == 0        # SK-V18-NOVEL; owes no REDRESS id
742: 51. SK-V5 event-cursor redress: byte-class whitespace cursor is REJECTED.
784: 53. SK-V5 structural-mask parser-local cursor is REJECTED.
6184: ## SK-V14 W11T Parse-Only Structural Stream Reject   (item 246)
6230: ## SK-V14 W11V Parse-Only String64 Reject              (item 247)
2795: ## SK-V9 Wave 3 Union Event-Model Class-Column Redress (96/97/98 host block)
G2/G4/G6-BLOCKED mirror: 3D=3, 3B=2, 3F=2  (consistent across R2-mirror owners)
```

The v+1 diff header `@@ -622,6 +622,33 @@`: `LOCKS.md:622` = the Lock-16
NEON-classifier clause, `:625` = the governance heading — context matches; the
LOCKS-strengthening fence clauses CAN land. The 71-vs-67 runtime drift is
DISCLOSED and traced to `tape/{mod,cursor,arena,record}.rs` by 3C D-SKV18-L13
(:94/:115) + 3A-D12 (:218) + 1E LAC-1E-V5-07 (:153) — honest, not silent.

```sh
# 13 T-P2-refuted resolve and say what the fences claim
sed -n '307,315p' restart/audit/totality/p2/2C-grammar-neutrality.md
# cited finding-ids resolve
sed -n '133,134p' restart/audit/totality/p3/3D-skinny-fold.md   # 3D-D11/D12 tree-walk + md5-alone REJECT
sed -n '215p;219p'  restart/audit/totality/p3/3A-architecture-synthesis.md  # 3A-D09 wire-as-is REFUTED; 3A-D13 Nu8 refuted
sed -n '119,125p;199p' restart/audit/totality/p1/1D-skinny-lessons.md       # D-10/G-3 Decision Engine load-bearing
```

```text
2C:307-315 all resolve (neutral-name-on-one-grammar refuted; Nu8-litmus refuted ->
 precedence-tower; 9-ident table refuted; 4-name regex refuted; css_types.rs
 refuted; md5-distinct necessary-not-sufficient; IR tree-walk regresses 94.1%;
 enum-arm onboarding refuted; x86 closes no M5 row).
3D-D11 (3D:133) carries tree-walk REJECT; 3D-D12 (3D:134) carries md5-alone REJECT
 in favour of the 3-co-gate; 3A-D09 (3A:215) carries find_css_significant
 dead-at-admission + wire-as-is REFUTED; 3A-D13 (3A:219) carries the Nu8 litmus
 refutation + precedence-tower replacement. None revived.
1D D-10/G-3 (1D:119-125,:199): the 5-shape BackendShape + decision spine are
 LOAD-BEARING (SK-V15 W7/W8/W9, REDRESS:6326-6414), not a REDRESS-rejected route.
```

## Enumerated Dispositions Under The REGRESSION Lens

| # | delta / disposition | lens conjunct | finding | result |
|---|---|---|---|---|
| 1 | `3C-locks-v+1-diff.md` hunk | "the v+1 diff applies" (mandatory) | `git apply --check` exit 0 at HEAD; header `@@ -622,6 +622,33 @@`; LOCKS:622 = Lock-16 NEON-classifier clause, :625 = governance heading. Fence clauses CAN land. | ACCEPT |
| 2 | `CH1-V4-001` fold (3A `V2/CH4.md` citation) | "a cited section exists" | 3A:66 now cites `V2/CH4.md:36`; file is 168 lines, :36 is the `3B-D04` cost-reconcile RESOLVED row the V3 fold rests on. In-range; the out-of-range V4 defect is closed. | ACCEPT |
| 3 | `3E-D11` / `3E-L14-HC-05` Pattern-H recensus (CH3-V3-R1) | "no delta weakens a recensus the SK-V18 evidence strengthened" | Both cells (3E:48, 3E:154) carry the inline RE-KEY clause to per-file provenance over the live 71; the absolute-67 is DECLARED superseded, not re-asserted. Semantic discharge intact. **BUT** the V4 CH3 evidence literal `grep '67/67' == 0` is FALSE at HEAD (returns 2, both inside the discharge clauses). The V5 CH3 evidence line must be re-keyed to the true output. | REVISE (`CH3-V5-R1`) |
| 4 | SK-V16/V17 reconcile RETIME (CH3-V1-R2) | "no delta re-opens a REDRESS route" | Retimed to Pass-Omega-V6 / pre-W-PRUNE blocker; G2/G4/G6 entry BLOCKED until the reconcile is on the committed ledger; mirrored 3D=3 / 3B=2 / 3F=2 verbatim. A STRENGTHENING (adds a blocker), not a re-open. | ACCEPT |
| 5 | `3D-D06` Decision Engine "that is REVISED" un-rejection | "3D promotes no rejected route" | Un-rejects a PRIOR-CYCLE TOTALITY-DOC scaffold classification on cited T-P1 D-10/G-3 (1D:119-125,:199); the Decision Engine is a REDRESS-LANDED win (W7/W8/W9, REDRESS:6326-6414), NOT a REDRESS-rejected route. `rejected: []` in 3D frontmatter; five-shape canon preserved; selection-depth caveat carried honestly. Sanctioned skinny-WIN→V1 direction. | ACCEPT |
| 6 | `3D-D08-substrate-sidecar-lock` | items 51/53/246/247 four-item pre-block | NEON G6 = RETARGET-onto-the-live-recursive-shell, never a wire-as-is dead-flat kernel or parser-local second scanner; cites 1D:166-171 + `find_css_significant` dead-at-admission (2E:80). Fences. | ACCEPT |
| 7 | `3D-D11-one-generator-inflection-thesis` | 13-refuted "IR tree-walk preserves the 94.1% scan" (2C:313) | Carries the refutation as the (a)-(d) named-primitive gate (3D:133); "a tree-walk that inflates the flat scan into a combinator descent is REJECTED". | ACCEPT |
| 8 | `3D-D12-r16-relocated-seam-cogate` | 13-refuted "md5-distinct proves grammar-derived selection" (2C:312) | 3-co-gate conjunction {md5 ∧ branch==0 ∧ type==0 ∧ rows_collapsed} (3D:134); "md5-distinctness alone proves nothing about the un-fork (a REFUTED assertion)". | ACCEPT |
| 9 | `3A-D08` CollapsedStage diagnostic-only slot | abuts REDRESS 96/97/98 | Carries the RETIRED streamed-cursor prior; promotion past `diagnostic-only` must clear that retired prior (3A:214 cites REDRESS:2795-2944; 3C-L10:92/:113). Fences. | ACCEPT |
| 10 | `3A-D09` G6 retarget-not-author + single-movemask | abuts items 51/53/247 | RETARGETS the LIVE generated hot-leaf shell; REFUTES "wire `find_css_significant` as-is" (3A:215); REDRESS-fenced against 51/53/247. | ACCEPT |
| 11 | `3A-D13` Sheets precedence-tower negative control | 13-refuted "Nu8-tagged-alt is the Sheets litmus" (2C:308) | Carries the refutation (3A:219); precedence tower replaces the Nu8 litmus, lowers to EXISTING `SinkOnlyExpr` vocab, needing NO new IR primitive. | ACCEPT |
| 12 | `3B` §13.6 re-key SK-V18→SK-V19 | "3B revives no refuted wave" | Relabel of the tape-fold receivers; F1-F9 content preserved; A.W0..A.W4 stays pending; §13.5 SK-V15 preserved; `revives no refuted route` asserted at 3B:42 and verified. No refuted wave revived. | ACCEPT |
| 13 | `3B` removed deltas (frontmatter `removed: []`) | silent-drop of a refuted-route fence? | SK-V15-routing deltas CONSUMED by landed §13.5/§13.6; refuted-route FENCES carry forward as standing gates. No fence silently dropped. | ACCEPT |
| 14 | `3C` D-SKV18-L10-collapsed-slot | "3C weakens no lock REDRESS strengthened" | Carries the RETIRED REDRESS 96/97/98 scalar-cheaper-than-SIMD prior FORWARD as a binding promotion bar (3C:92/:113). Strengthens. | ACCEPT |
| 15 | `3C` D-SKV18-L08-aarch64-only | lock-sharpening vs weakening | SHARPENS SK-V17 aarch64-PRIMARY (LOCKS:622 NEON-classifier clause verified) to aarch64-ONLY; x86 = P1 deletion target. Strengthens. | ACCEPT |
| 16 | `3C` D-SKV18-L06-verbatim-blob | Lock-6 strengthening | Co-binds Lock-6 byte-equivalent-regen to the verbatim-blob prohibition; a `const CSS_GENERATED_RS` courier is REJECT as "grammar-driven". Strengthens. | ACCEPT |
| 17 | `3C` D-SKV18-L05-L10-unfork (negative-witness novelty) | route-novelty assertion | `rg -ic 'relocated.seam\|RuntimeEmitterKind\|un.fork' skinny/REDRESS.md == 0` VERIFIED at HEAD; "Ledger negative-witness confirms SK-V18-NOVEL" (3C:135). | ACCEPT |
| 18 | `3D-D02`/`3D-D03` CSS posture softened to directional-pending-re-lock | "no delta paper-closes a refuted CSS route" | CSS carried MEASUREMENT-VALID but DIRECTIONAL pending the H1 css_canon_bench re-lock (3D:43,:88,:110,:123); a SOFTENING toward honesty (no fake admit), not a paper-close. | ACCEPT |
| 19 | `3F-MH-003` PRUNE-before-GENERALIZE delete-before-provider gate + R2 mirror | dependency-precedes-deletion; route revival | "no GENERALIZE/PROVE wave deletes a hand-written ORACLE before its grammar-DERIVED replacement lands byte-equivalent" (3F:90); AND G2/G4/G6 BLOCKED until the SK-V16/V17 reconcile. Bars a rejected shape from re-entering with no committed fence. | ACCEPT |
| 20 | `3F-MH-008..012` migration deletes (x86/courier/replicas/phantom) | delete-before-provider; route revival | x86 = reach-matched crate-wide delete (MH-008); courier retires only with byte-equivalent oracle (MH-010); replicas collapse with `runtime_target_rows_collapsed` co-gate (MH-011); phantom `<G>` delete preserves K-axis (MH-012). All delete-before-provider-gated. | ACCEPT |
| 21 | `3F-MH-013` css_types.rs RELOCATE-or-DELETE | silent-drop check | DEFERRED to SK-V19 as an EXPLICIT migration decision (3F:100,:184); "discharges the no-silent-disposition rule"; mirrored 3D-D05 / 3C-L13 / 2C SK18-03. Not silently dropped. | ACCEPT |
| 22 | `3D-D05` Pattern-H provenance SK-V19 carry | delete-before-provider | Header-only or destructive delete without same-wave replacement rejects (3D:127); census preserved as the SK-V19 repair surface; md5-distinctness falsifier carried. Sound. | ACCEPT |

## What Holds (the ACCEPT spine)

1. **No REDRESS route reopened (within the captured ledger).** Items
   51/53/246/247 and the 96/97/98 host block carry verified line-spans with the
   explicit ADMISSIBLE-vs-REJECTED distinction. G4's `Cursor` is a VIEW over the
   existing Tape/ValueRef/PayloadArena; the G6 move RETARGETS the existing in-loop
   shell; neither is a second substrate, structural-stream driver, parser-local
   cursor, nor bespoke mask. The COMPLETENESS gap (U-5, SK-V16/V17) is fenced as a
   pre-G2/G4/G6 Pass-Omega-V6 blocker, mirrored 3D=3 / 3B=2 / 3F=2.

2. **3B revives no refuted wave.** §13.6 receivers re-keyed SK-V18→SK-V19 with
   content preserved; `removed: []`; the "revives no refuted route" assertion at
   3B:42 holds.

3. **3D promotes no rejected route.** G6 = RETARGET-not-wire-as-is (refuted "wire
   as-is" carried REFUTED); the (a)-(d) gate carries the refuted tree-walk as a
   REJECT (3D-D11); the relocated-seam md5-alone is REJECTED in favour of the
   3-co-gate (3D-D12). The lone "that is REVISED" (3D-D06 Decision Engine)
   un-rejects a TOTALITY-DOC scaffold classification on evidence — a
   REDRESS-LANDED win, `rejected: []` — not a REDRESS-route revival.

4. **3C weakens no lock REDRESS strengthened.** Every SK-V18 clause is additive
   or a sharpening (aarch64-PRIMARY→aarch64-ONLY; CollapsedStage admitted only as
   an inert slot that must clear the RETIRED REDRESS 96/97/98 prior; verbatim-blob
   courier REJECTED; relocated-seam firewall + named-primitive gate). 16 locks and
   5 BackendShape variants intact; un-fork is ledger-novel (negative-witness == 0).
   The v+1 diff applies cleanly.

5. **No delta revives one of the 13 T-P2-refuted assertions.** All carried as
   gates/fences (2C:307-315 + SK-V18-2C:222-224 resolve and say what the fences
   claim). The prior Pattern-H exception (the absolute-67 in 3E-D11 /
   3E-L14-HC-05) is RE-KEYED and DECLARED superseded; the residual REVISE is only
   that the CH3-chain evidence literal must reflect the true `67/67`=2 (both inside
   the discharge clauses), not the stale "returns ZERO".

## Required V6 Fold

| finding | required repair | owner |
|---|---|---|
| `CH3-V5-R1` | In `restart/audit/totality/p3/hardening/V6/CH3.md`, record the TRUE evidence output for the 3E Pattern-H discharge: `grep -nE '67/67' restart/audit/totality/p3/3E-grammar-generalisation.md` returns 2 hits (`3E:48` frontmatter V3-FOLD note; `3E:154` L14-HC-05 RE-KEY clause), BOTH inside the RE-KEY/discharge clauses that DECLARE the absolute-67 superseded — neither a live invariant assertion. Do NOT carry the V4 "returns ZERO" literal forward; the discharge of CH3-V3-R1 stands on the per-file recensus, not on the figure's absence from the text. | CH3 (V6 lens) |

## Residual Risk

1. CH3-V5-R1 is a CH3-chain evidence-honesty defect and may also touch CH1
   (citation/evidence consistency); if CH1 re-keys the same `67/67` literal in the
   V6 fold, the discharge is shared, not double-counted. The Pattern-H REGRESSION
   itself remains CLOSED — only the recorded evidence literal is stale.
2. CH3-V1-R2's reconcile is a committed-ledger blocker, but until the SK-V16/V17
   reconcile actually runs at Pass-Omega-V6, the possibility that an SK-V16/V17-era
   streamed-cursor or second-scanner reject is re-entered by G6 cannot be excluded,
   only bounded by the pre-G2/G4/G6 gate. The packet states this honestly across
   3D/3B/3F.
3. The CH3 open questions (3B/3D/3F R2-mirrors; 3A) are each well-formed
   (receiver/blocker/gate present); none is rejected.

TALLY accept=21 revise=1 reject=0
