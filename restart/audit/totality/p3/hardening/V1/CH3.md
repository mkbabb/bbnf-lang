# CH3 REGRESSION — SK-V18 T-P3 V1 (cycle V1)

## Lens

No delta re-opens a `skinny/REDRESS.md` route; 3B revives no refuted wave; 3D
promotes no rejected route; 3C weakens no lock that REDRESS strengthened; no
delta revives one of the 13 T-P2-refuted assertions. Spot-verify the most
load-bearing deltas (cited finding-id resolves; cited LOCKS section exists; the
v+1 diff applies).

## Verdict Summary

The SK-V18 generalization packet is, under the REGRESSION lens, overwhelmingly
disciplined: every adjoining REDRESS-rejected route (items 51/53/246/247,
96/97/98) is carried with its falsifying ADMISSIBLE-vs-REJECTED distinction, not
revived; every one of the 13 T-P2-refuted assertions is carried as a fence/gate,
not reopened; 3B's §13.6 re-key is a tranche relabel (SK-V18→SK-V19), not a wave
revival; 3D's G6 is RETARGET-not-wire-as-is, REDRESS-fenced; 3C strengthens (does
not weaken) the locks REDRESS strengthened. The cited finding-ids and LOCKS
sections that I spot-checked resolve.

But two regression-relevant defects are load-bearing and force REVISE, and the
mandatory v+1-diff spot-check FAILS. The packet's "no REDRESS route reopened"
guarantee has a SELF-DISCLOSED hole (U-5: the committed ledger ends at SK-V15 W11;
SK-V16/V17 rejected routes are uncaptured) that the packet routes to a SK-V19
ENTRY reconcile — i.e. AFTER the SK-V18 GENERALIZE waves (G2/G4/G6) that actually
abut the fenced shapes have already run. And the `3C-locks-v+1-diff.md` hunk does
not apply to the current `LOCKS.md`, which blocks the very LOCKS-strengthening
clauses that fence these routes from landing.

## Evidence Commands And Outputs

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

```text
16
      71
```

16 numbered locks intact (canon preserved). Runtime files = 71 (NOT the
LOCKS-asserted 67); 3A-V4-SK18-D12 and 3C D-SKV18-L13-pattern-h-recensus both
DISCLOSE the 71-vs-67 drift and attribute the +4 to `tape/{mod,cursor,arena,record}.rs`
— honest, traced, not a silent regression. (This is the expected-67 invariant in
the stale SK-V15 dispatch context; the packet itself reconciles it as a finding.)

```sh
awk '/^```diff$/{f=1;next} f&&/^```$/{exit} f{print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v18.diff
git apply --check /tmp/tp3-locks-v18.diff ; echo "exit=$?"
```

```text
error: corrupt patch at line 38
exit=128
```

The extracted hunk body has 5 context lines + 28 additions = 33 lines, but the
hunk header declares `@@ -622,6 +622,38 @@` (old=6, new=38). Both counts are
wrong (old should be ≤6 with the body present, new should be 33). Re-deriving a
correct header still fails: the live `LOCKS.md` has TWO blank lines between the
SK-V17 Lock-16 clause (`:622`) and `## v+1 Governance Boundary` (`:625`), while
the diff context carries only ONE. The diff DOES NOT APPLY. (Live anchor verified:
`LOCKS.md:622` = the SK-V17 NEON-classifier clause; `:623`-`:624` blank; `:625` =
`## v+1 Governance Boundary`.)

```sh
sed -n '156,173p' restart/audit/totality/p1/1D-skinny-lessons.md   # Rejected-Route Pre-Block
sed -n '6184,6186p;6230,6232p' skinny/REDRESS.md                   # items 246/247
sed -n '2795,2800p;2928,2933p' skinny/REDRESS.md                   # items 96/97/98 region
```

```text
1D:166-171 Rejected-Route Pre-Block table resolves: item 246 = REDRESS:6184-6219
(W11T parse-only structural stream, REJECT); item 247 = REDRESS:6230-6260 (W11V
parse-only string64 mask, REJECT); item 51 = REDRESS:742-768 (JsonEventCursor,
REJECT); item 53 = REDRESS:784-813 (JsonStructuralCursor, REJECT). REDRESS:2795+
= "SK-V9 Wave 3 Union Event-Model Class-Column Redress" / item 96 (the
scalar-cheaper-than-SIMD-cursor finding). All cited spans exist and say what the
synthesis claims.
```

```sh
rg -n 'refuted' restart/audit/totality/p2/2C-grammar-neutrality.md | rg '132|134|136|307|308|309|310'
```

```text
2C:134 "CSS_GENERATED_RS is grammar-derived" → refuted (fenced by 3A-D05/3D-D03/3F-MH-010)
2C:307 "neutral-name-on-one-grammar proves neutrality" → refuted (fenced by 3A-D10/3C-neutrality-proof/3E-D14)
2C:308 "Nu8-tagged-alt is the Sheets litmus" → refuted (replaced by precedence tower, 3A-D13/3E-D15)
2C:309/310 "9-ident table is neutral / 4-name regex sufficient" → refuted (fenced by 3A-D11/3C-L13/3F SK-V19)
2C:312 "md5-distinctness proves the un-fork" → refuted (fenced by 3D-D12/3A-D04/3E-D17 structural co-gate)
```

## Enumerated Dispositions Under The REGRESSION Lens

| # | delta / disposition | lens conjunct | finding | result |
|---|---|---|---|---|
| 1 | `ARCH-3A-V4-SK18-D08` CollapsedStage diagnostic-only slot | abuts REDRESS 96/97/98 (streamed-cursor) | Carries the REDRESS 96/97/98 retired prior; promotion past `diagnostic-only` "must clear that retired prior" (3A:182; 3C-L10:71; cites `REDRESS.md:2795-2944` finding `:2928-2933`). Fences, does not revive. | ACCEPT |
| 2 | `ARCH-3A-V4-SK18-D09` G6 retarget-not-author + single-movemask | abuts items 51/53/247 | RETARGETS the existing in-loop kernel (caller-data byte set, neutral inner); "REDRESS-fenced against item 51/53/247" (3A:183; 3C-L16-retarget:69). Falsifying distinction explicit. | ACCEPT |
| 3 | `3D-D08-substrate-sidecar-lock` | items 51/53/246/247 four-item pre-block | "the NEON G6 retarget is RETARGET-onto-the-live-recursive-shell, never a wire-as-is dead-flat kernel or a parser-local second scanner"; cites 1D:166-171, the verified pre-block (3D:125). | ACCEPT |
| 4 | `3D-D12-r16-relocated-seam-cogate` | 13-refuted "md5-distinctness proves un-fork" (2C:312) | Carries the refuted assertion as a 3-co-gate conjunction (md5 ∧ branch==0 ∧ type==0 ∧ rows_collapsed); does NOT credit md5 alone (3D:129). | ACCEPT |
| 5 | `3D-D11-one-generator-inflection-thesis` | 13-refuted "tree-walk preserves the 94.1% scan" | Carries the refutation as the (a)-(d) named-primitive gate; "a tree-walk that inflates the flat scan into a combinator descent is REJECTED" (3D:128). | ACCEPT |
| 6 | `3E-D14/D15/D17/D18` | refuted neutrality / Nu8-litmus / md5 / fleet-overclaim | All four carried as fences; 3E frontmatter `sk_v18_constraints_carried` lines 47-54 explicitly lists all 8 refutations incl. "bracket_depth_mask_64 replaces the scalar shell — REDRESS-96/97/98-fenced". | ACCEPT |
| 7 | `3B` §13.6 re-key SK-V18→SK-V19 (MP-3B-SKV18-D02) | "3B revives no refuted wave" | Relabel of the tape-fold block to SK-V19 + new §13.7 receiver map; F1-F9 fold-design content unchanged; A-J stays pending. No refuted wave revived (3B:123-130). | ACCEPT |
| 8 | `3F-MH-008..013` five migration deletes (x86/courier/replicas/phantom/css_types) | delete-before-provider; route revival | x86 = diagnostic-only plane delete (not a route); courier retires only with byte-equivalent oracle (3F-MH-010); replicas collapse with structural co-gate; phantom delete preserves K-axis; css_types DEFERRED to SK-V19 (not silently dropped). | ACCEPT |
| 9 | `3F-MH-003` PRUNE-before-GENERALIZE delete-before-provider gate | dependency-precedes-deletion | "no GENERALIZE/PROVE wave deletes a hand-written ORACLE before its grammar-DERIVED replacement lands byte-equivalent" (3F:79). Sound. | ACCEPT |
| 10 | `3C-locks-v+1-diff.md` hunk | "the v+1 diff applies" (mandatory spot-check) | **FAILS `git apply --check` (corrupt patch line 38).** Hunk header arithmetic wrong (old=6/new=38 vs body 5+28); and live `LOCKS.md:623-624` has two blank lines, diff context has one. The LOCKS-strengthening clauses that FENCE the REDRESS routes cannot land as written. | **REVISE** |
| 11 | U-5 / four-item pre-block timing | "no delta re-opens a REDRESS route" | The pre-block (1D:166-171) self-discloses (CH3-V4-006 caveat) that it covers ONLY the committed ledger ending SK-V15 W11; SK-V16/V17 rejected routes are STRUCTURALLY INVISIBLE. 3B-CH3-Q (3B:252), 3D-CH3-Q (3D:163), 3F-CH3-Q (3F:258) all route this to **SK-V19 entry** — AFTER SK-V18 G2/G4/G6 run. A SK-V16/V17-rejected shape can re-enter a SK-V18 GENERALIZE wave with no committed fence. | **REVISE** |
| 12 | `3C` LAC-2F-V3-03 DEFER (re-scope the "gap" frame) | silent-drop check | Folded as a one-line audit-scope note into D-SKV18-L16 with a named re-entry trigger; NOT silently dropped. Re-entry lands at SK-V19 — acceptable but, like #11, post-dates the SK-V18 waves it might bear on. | ACCEPT |
| 13 | `3B` removed deltas D03-D08/D11 | silent-drop of a refuted-route fence? | Removed = SK-V15-routing deltas CONSUMED by landed §13.5/§13.6 (3B frontmatter:19). The refuted-route FENCES carry forward as MP-3B-V1-D02/D09/D10 standing gates; no fence silently dropped. | ACCEPT |
| 14 | `3C` LAC-2D-V3-01 un-fork "ledger negative-witness" claim | route-novelty assertion | 3C asserts the un-fork is "SK-V18-NOVEL, not a REDRESS 96/97/98 revival" (3C:126). The un-fork (DELETE `RuntimeEmitterKind`, dispatch on `BackendShape`) is genuinely orthogonal to the streamed-cursor REDRESS class; claim holds. | ACCEPT |

## REVISE Repair Directives

**CH3-V1-R1 — `3C-locks-v+1-diff.md` does not apply (severity: HIGH).**
Owner: 3C. Target: `restart/audit/totality/p3/3C-locks-v+1-diff.md:47` hunk
header and the trailing-context block at `:76`-`:77`. Conflicting evidence:
live `restart/locks/LOCKS.md:622`-`625` has TWO blank lines (`:623`,`:624`)
before `## v+1 Governance Boundary`; the diff body emits 5 context + 28 added
lines while the header declares `@@ -622,6 +622,38 @@`. Correction: (a) emit the
missing second blank context line so the leading context matches `LOCKS.md:622`-
`624`; (b) set the hunk header to the actual line accounting (old = context-line
count, new = context + addition count) so `git apply --check` returns exit 0.
This is a REGRESSION-lens blocker because the corrupt hunk prevents the
verbatim-blob (D-SKV18-L06), CollapsedStage-slot (D-SKV18-L10), neutrality-proof
(D-SKV18-L14-neutrality), and retarget-not-author (D-SKV18-L16) clauses — the
clauses that FENCE the REDRESS 96/97/98 and item-51/53/247 routes — from being
applied to `LOCKS.md` by Pass Omega CRUD. The `3C-locks-v+1-diff.md` Invariant
Check (`:88`) asserts a clean `git apply --check`; that assertion is false at
HEAD and must be made true.

**CH3-V1-R2 — REDRESS coverage gap (U-5) is fenced only at SK-V19 entry, after
the SK-V18 GENERALIZE waves it should gate (severity: MEDIUM).**
Owner: 3D (primary; carries the four-item pre-block fold), with 3B/3F mirroring.
Target: `3D-D08`/`3D` CH3 open question (`3D-skinny-fold.md:163`); `3B`
CH3 open question (`3B-master-plan-reconciliation.md:252`); `3F` CH3 open
question (`3F-migration-handoff.md:258`). Conflicting evidence: the
Rejected-Route Pre-Block COMPLETENESS CAVEAT
(`restart/audit/totality/p1/1D-skinny-lessons.md:171`, CH3-V4-006) states the
four-item table "is complete for the captured ledger, NOT for the full skinny
history; any SK-V16/V17 rejected route is structurally invisible," and U-5
(`1D:244`-`248`) names the verify_action ("locate the SK-V16/V17 tranche
REDRESS/HANDOFF and reconcile against the SK-V18 S-P0 residual census BEFORE Pass
Omega ratification"). The packet routes the reconcile to SK-V19 entry, but the
SK-V18 G2 (`css_balanced_component_scan`), G4 (`Cursor`/`<G>` delete), and G6
(NEON retarget) waves — the exact moves that abut items 51/53/247 — run during
SK-V18, before SK-V19. Correction: move the SK-V16/V17 REDRESS reconcile from a
SK-V19-entry obligation to a **Pass-Omega-V6 / pre-W-PRUNE blocker** (per U-5's
own "before Pass Omega ratification" verify_action), so the four-item pre-block
is reconciled against the full skinny history BEFORE any SK-V18 GENERALIZE wave
that abuts a fenced shape dispatches. At minimum, 3D-D08 and 3F-MH-003 must state
that G2/G4/G6 entry is BLOCKED until the SK-V16/V17 pre-block reconcile is on the
committed ledger.

## What Holds (the ACCEPT spine)

1. **No REDRESS route reopened (within the captured ledger).** Items
   51/53/246/247 and 96/97/98 are each carried with a verified line-span and an
   explicit ADMISSIBLE-vs-REJECTED distinction (1D:166-171; 3A-D08/D09;
   3D-D08; 3E frontmatter:47-54; 3C-L10/L16; 3F-MH CH3-Q). The G4 `Cursor` is a
   VIEW over the existing `Tape`/`ValueRef`/`PayloadArena` (admissible); the G6
   move RETARGETS the existing in-loop shell (admissible); neither is a second
   substrate, a structural-stream driver, a parser-local cursor, nor a bespoke
   per-grammar mask (all rejected). The single COMPLETENESS gap is U-5, surfaced
   honestly and addressed by CH3-V1-R2.

2. **3B revives no refuted wave.** The §13.6 tape-fold receivers are re-keyed
   SK-V18→SK-V19 with F1-F9 content preserved verbatim; the A-J tranche set stays
   `pending`; the F.W5 un-fork claim is correctly marked UNREALISED-in-both-trees
   (3B:97,107), not paper-closed.

3. **3D promotes no rejected route.** The G6 decision is RETARGET-not-wire-as-is
   (the refuted "wire `find_css_significant` as-is" is carried as REFUTED, 3D:125,
   3E:48); the named-primitive (a)-(d) gate carries the refuted tree-walk as a
   REJECT condition (3D:128, 3D-D11).

4. **3C weakens no lock REDRESS strengthened.** Every SK-V18 clause is additive
   or a sharpening (aarch64-PRIMARY→aarch64-ONLY; CollapsedStage admitted only as
   an inert slot; verbatim-blob courier REJECTED). The CollapsedStage clause
   explicitly carries the REDRESS 96/97/98 retired prior forward as a promotion
   bar (3C-L10). 16 locks and 5 BackendShape variants intact.

5. **No delta revives one of the 13 T-P2-refuted assertions.** All carried as
   gates/fences (table above, conjunct 5/6).

## Residual Risk

1. CH3-V1-R1 is also a CH1 correctness finding (the diff-applies invariant). If
   CH1 repairs the hunk, this CH3 REVISE is discharged in the same fold; the two
   must not double-count.
2. CH3-V1-R2's reconcile may surface an SK-V16/V17 rejected route that a current
   SK-V18 delta DOES abut — until the reconcile runs, that possibility cannot be
   excluded, only bounded. The risk is that an SK-V16/V17-era streamed-cursor or
   second-scanner reject is re-entered by G6 with no committed fence.
3. The packet's CH3 open questions (3B:252, 3D:163, 3F:258) are each well-formed
   (receiver/blocker/gate present); CH3-V1-R2 retimes their gate, it does not
   reject them.

TALLY accept=12 revise=2 reject=0
