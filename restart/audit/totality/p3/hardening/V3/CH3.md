# CH3 REGRESSION — SK-V18 T-P3 V3 (cycle V3)

## Lens

No delta re-opens a `skinny/REDRESS.md` route; 3B revives no refuted wave; 3D
promotes no rejected route; 3C weakens no lock that REDRESS strengthened; no
delta revives one of the 13 T-P2-refuted assertions. Spot-verify the most
load-bearing deltas (cited finding-id resolves; cited LOCKS section exists; the
v+1 diff applies).

## Verdict Summary

Under the REGRESSION lens the V3-folded packet is disciplined and the two V1
REVISE items remain DISCHARGED at HEAD: the mandatory `3C-locks-v+1-diff.md`
spot-check PASSES (`git apply --check` exit 0), and the SK-V16/V17 REDRESS
reconcile is still retimed to a Pass-Omega-V6 / pre-W-PRUNE blocker that BLOCKS
G2/G4/G6 entry, mirrored verbatim across 3D-D08 / 3B-CH3 / 3F-MH-003. Every
adjoining REDRESS-rejected route (items 51/53/246/247; 96/97/98) is carried with
its falsifying ADMISSIBLE-vs-REJECTED distinction, not revived; every one of the
13 T-P2-refuted assertions is carried as a fence/gate; 3B's §13.6 re-key is a
tranche relabel (SK-V18→SK-V19), not a wave revival; 3D's G6 is
RETARGET-not-wire-as-is; 3C strengthens (does not weaken) the locks REDRESS
strengthened. The cited finding-ids and LOCKS anchors I spot-checked all resolve,
and the un-fork novelty claim's ledger negative-witness verifies at HEAD
(`rg -ic 'relocated.seam|RuntimeEmitterKind|un.fork' skinny/REDRESS.md == 0`).

One REGRESSION-lens defect persists and forces a single REVISE: it is the SAME
defect CH3-V2-R1 named, NOT folded by the V2→V3 cost-hardening pass. The carried
SK-V15 delta `3E-D11` (`:110`) and its sibling clause `3E-L14-HC-05` (`:151`)
still define the Pattern-H gate as the ABSOLUTE "67/67 line-1 generated
provenance" / "0/67 generated headers" invariant, which the SK-V18 recensus
(`3C` D-SKV18-L13 `:92`/`:113`; `3A` ARCH-3A-V4-SK18-D12 `:215`; `1E`
LAC-1E-V5-07 `:153`) explicitly re-keyed to per-file provenance over the LIVE
census (71 at HEAD). The V2→V3 fold commit (`e6c1c2a84 fold V2 cost hardening
into V3 synthesis`) touched 3E but did NOT carry CH3-V2-R1; the 3E frontmatter
`revised:` block lists no CH3-fold, and 3E still acknowledges the 67→71 drift
ONLY as a V4-section CH4 OPEN QUESTION (`:381`), never re-keying the carried
delta body. This is a soft regression (a carried delta weakly contradicts a
recensus the SK-V18 evidence strengthened), not a route revival — REVISE, not
REJECT — and it is an ORPHAN REVISE because the prior cycle directed it and it
went unfolded.

## Evidence Commands And Outputs

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
awk '/^```diff$/{f=1;next} f&&/^```$/{exit} f{print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v3.diff
git apply --check /tmp/tp3-locks-v3.diff ; echo "apply exit=$?"
awk 'NR>=620 && NR<=625 {printf "%d:%s\n", NR, $0}' restart/locks/LOCKS.md | cut -c1-60
```

```text
16
      71
apply exit=0
620:- Lock 14 ValueRef/classifier-generalisation clause: the lazy g
621:
622:- Lock 16 NEON-classifier-manifest clause: the shared NEON `selec
623:
624:
625:## v+1 Governance Boundary
```

16 numbered locks intact (canon preserved). Runtime files = 71 (NOT the
LOCKS-asserted 67); D-SKV18-L13-pattern-h-recensus + 3A-D12 + 1E LAC-1E-V5-07 all
DISCLOSE the 71-vs-67 drift and attribute the +4 to
`tape/{mod,cursor,arena,record}.rs` — honest, traced. The v+1 diff APPLIES
CLEANLY (header `@@ -622,6 +622,33 @@`; the second blank context line `:624` is
present; `LOCKS.md:622` = the NEON-classifier clause, `:623`/`:624` blank, `:625`
= the governance heading — diff context matches), so the LOCKS-strengthening
clauses that FENCE the REDRESS routes CAN land. CH3-V1-R1 stays folded.

```sh
sed -n '156,172p;244,248p' restart/audit/totality/p1/1D-skinny-lessons.md
sed -n '742p;784p;6184p;6230p;2795p;2928,2933p' skinny/REDRESS.md
rg -ic 'relocated.seam|RuntimeEmitterKind|un.fork' skinny/REDRESS.md
rg -nc 'G2/G4/G6 entry is BLOCKED until the SK-V16/V17' \
  restart/audit/totality/p3/{3B-master-plan-reconciliation,3D-skinny-fold,3F-migration-handoff}.md
```

```text
1D:166-171 Rejected-Route Pre-Block resolves: item 246 = REDRESS:6184 (W11T
parse-only structural stream, REJECT); item 247 = REDRESS:6230 (W11V parse-only
string64 mask, REJECT); item 51 = REDRESS:742 (SK-V5 event-cursor, REJECT); item
53 = REDRESS:784 (SK-V5 structural-mask parser-local cursor, REJECT). REDRESS:2795
= "SK-V9 Wave 3 Union Event-Model Class-Column Redress"; the load-bearing finding
at :2928-2933 = the M5-Max scalar-cheaper-than-SIMD-cursor result. U-5
verify_action 1D:244-248 = "reconcile SK-V16/V17 ... before Pass Omega
ratification". rg ledger negative-witness = 0 (un-fork owes no REDRESS id;
SK-V18-NOVEL). G2/G4/G6-BLOCKED statement present 3B=2, 3D=3, 3F=2 — consistent
across all three R2-mirror owners.
```

```sh
rg -n 'refuted' restart/audit/totality/p2/2C-grammar-neutrality.md \
  | rg '134|307|308|309|310|312|313'
grep -nE '67/67|0/67' restart/audit/totality/p3/3E-grammar-generalisation.md
rg -nE 'recensus|71 at HEAD|per-file|RE-KEYED|re-keyed|D-SKV18-L13' \
  restart/audit/totality/p3/3E-grammar-generalisation.md
```

```text
2C:134/307/308/309/310/312/313 all resolve and say what the synthesis fences
claim (CSS_GENERATED_RS not grammar-derived; neutral-name-on-one-grammar; Nu8
litmus; 9-ident table; 4-name regex; md5-distinctness necessary-not-sufficient;
tree-walk regresses the 94.1% scan). 3E:110 + 3E:151 STILL carry "67/67" / "0/67"
(absolute). 3E recensus cross-ref grep: ZERO recensus/per-file/re-keyed hit on
the D11 or HC-05 cells — the V2 repair was not applied. (3E:72 're-keyed' is the
unrelated W5→G2 wave-owner re-key.)
```

## Enumerated Dispositions Under The REGRESSION Lens

| # | delta / disposition | lens conjunct | finding | result |
|---|---|---|---|---|
| 1 | `3C-locks-v+1-diff.md` hunk (CH3-V1-R1 stays folded) | "the v+1 diff applies" (mandatory) | `git apply --check` exit 0 at HEAD; header `@@ -622,6 +622,33 @@`; second blank context line `:624` present; LOCKS:622 = NEON-classifier clause, :623/:624 blank, :625 = governance heading. Fence clauses CAN land. | ACCEPT |
| 2 | SK-V16/V17 reconcile RETIME (CH3-V1-R2 stays folded) | "no delta re-opens a REDRESS route" | Retimed to Pass-Omega-V6 / pre-W-PRUNE blocker; G2/G4/G6 entry BLOCKED until the SK-V16/V17 pre-block reconcile is on the committed ledger; mirrored 3D-D08(:128)/3D-CH3-Q, 3B-CH3(:24), 3F-MH-003(:85). Per U-5's own "before Pass Omega ratification". | ACCEPT |
| 3 | `3D-D08-substrate-sidecar-lock` | items 51/53/246/247 four-item pre-block | NEON G6 = RETARGET-onto-the-live-recursive-shell, never a wire-as-is dead-flat kernel or parser-local second scanner; cites 1D:166-171 (verified) + the dead `find_css_significant` REFUTED at 2E:80. Fences, does not revive. | ACCEPT |
| 4 | `3D-D11-one-generator-inflection-thesis` | 13-refuted "tree-walk preserves the 94.1% scan" (2C:313) | Carries the refutation as the (a)-(d) named-primitive gate; "a tree-walk that inflates the flat scan into a combinator descent ... is REJECTED" (3D:131); cites research R-B/R-C tree-walk REJECTED. | ACCEPT |
| 5 | `3D-D12-r16-relocated-seam-cogate` | 13-refuted "md5-distinctness proves un-fork" (2C:312) | 3-co-gate conjunction {md5 ∧ branch==0 ∧ type==0 ∧ rows_collapsed} (3D:132); "md5-distinctness alone proves nothing about the un-fork (a REFUTED assertion per the dispatch)". Does NOT credit md5 alone. | ACCEPT |
| 6 | `3A-D08` CollapsedStage diagnostic-only slot | abuts REDRESS 96/97/98 | Carries the RETIRED streamed-cursor prior; promotion past `diagnostic-only` "must clear that retired prior" (3C-L10:77 cites REDRESS:2795-2944 finding :2928-2933). Fences. | ACCEPT |
| 7 | `3A-D09` G6 retarget-not-author + single-movemask | abuts items 51/53/247 | RETARGETS the LIVE generated hot-leaf shell (caller-data byte set, neutral inner); REFUTES "wire `find_css_significant` as-is". REDRESS-fenced. | ACCEPT |
| 8 | `3A-D13` Sheets precedence-tower negative control | 13-refuted "Nu8-tagged-alt is the Sheets litmus" (2C:308) | Carries the refutation; precedence tower replaces the Nu8 litmus, lowers to EXISTING SinkOnlyExpr vocab, no new IR primitive. | ACCEPT |
| 9 | `3B` §13.6 re-key SK-V18→SK-V19 (MP-3B-SKV18-D01..D10) | "3B revives no refuted wave" | Relabel of the tape-fold receivers to a SK-V19 block; F1-F9 fold-design content preserved; A-J set stays pending; §13.5 SK-V15 preserved; F.W5 un-fork marked UNREALISED-in-both-trees (3B:95-101). No refuted wave revived. | ACCEPT |
| 10 | `3B` removed deltas D03-D08/D11 | silent-drop of a refuted-route fence? | Removed = SK-V15-routing deltas CONSUMED by landed §13.5/§13.6 (3B:19,52); refuted-route FENCES carry forward as standing gates (MP-3B-SKV18-D01..D10). No fence silently dropped. | ACCEPT |
| 11 | `3C` D-SKV18-L10-collapsed-slot | "3C weakens no lock REDRESS strengthened" | Carries the RETIRED REDRESS 96/97/98 scalar-cheaper-than-SIMD-cursor prior FORWARD as a promotion bar (additive, binding gate, not open question; 3C-L10:77). Strengthens. | ACCEPT |
| 12 | `3C` D-SKV18-L08-aarch64-only | lock-sharpening vs weakening | SHARPENS SK-V17 aarch64-PRIMARY (LOCKS:622, verified NEON-classifier clause) to aarch64-ONLY; x86 = deletion target. Strengthens. | ACCEPT |
| 13 | `3C` D-SKV18-L06-verbatim-blob | Lock-6 strengthening | Co-binds Lock-6 v+1 byte-equivalent-regen to the SK-V18 verbatim-blob prohibition; round-trip byte-equivalence is the binding proof. Strengthens. | ACCEPT |
| 14 | `3C` LAC-2D-V3-01 un-fork "ledger negative-witness" novelty | route-novelty assertion | `rg -ic 'relocated.seam\|RuntimeEmitterKind\|un.fork' skinny/REDRESS.md == 0` VERIFIED at HEAD; emitter discriminator ≠ retained structural cursor; SK-V18-NOVEL, not a REDRESS 96/97/98 revival. | ACCEPT |
| 15 | `3C` LAC-2F-V3-03 DEFER (re-scope the "gap" frame) | silent-drop check | Folded as a one-line audit-scope note into D-SKV18-L16 with a named SK-V19 re-entry trigger; NOT silently dropped. | ACCEPT |
| 16 | `3C` D-SKV18-L13-pattern-h-recensus | "3C weakens no lock" — Pattern-H invariant | Re-keys the ABSOLUTE-67 invariant to per-file provenance over the LIVE census (71 at HEAD); +4 traced to tape-fold roster; an unattributable +N opens the O(N) generated-size-budget scan. Strengthens. (See #20 for the 3E delta that did NOT absorb this recensus.) | ACCEPT |
| 17 | `3F-MH-003` PRUNE-before-GENERALIZE delete-before-provider gate + R2 mirror | dependency-precedes-deletion; route revival | "no GENERALIZE/PROVE wave deletes a hand-written ORACLE before its grammar-DERIVED replacement lands byte-equivalent"; AND G2/G4/G6 BLOCKED until SK-V16/V17 reconcile (3F:85). Sound; bars an SK-V16/V17-rejected shape from re-entering with no committed fence. | ACCEPT |
| 18 | `3F-MH-008..013` migration deletes (x86/courier/replicas/phantom/css_types) | delete-before-provider; route revival | x86 = diagnostic-only plane delete (reach-matched list, no RED-by-construction gate); courier retires only with byte-equivalent oracle (MH-010); replicas collapse with structural row-collapse co-gate `runtime_target_rows_collapsed` (MH-011); phantom `<G>` delete preserves K-axis (MH-012); css_types DEFERRED to SK-V19 (MH-013), not silently dropped. All delete-before-provider-gated. | ACCEPT |
| 19 | `3D-D05` / `3F` Pattern-H provenance SK-V19 carry | delete-before-provider | Header-only or destructive delete without same-wave replacement rejects; 67-file/6867-LOC census preserved as the SK-V19 repair surface; md5-distinctness falsifier carried. Sound. | ACCEPT |
| 20 | `3E-D11` / `3E-L14-HC-05` carried Pattern-H gate definition | "no delta weakens a recensus the SK-V18 evidence strengthened" | The carried delta TEXT (3E:110, :151) still defines the gate as the ABSOLUTE "67/67 line-1 generated provenance" / "0/67 generated headers", which the SK-V18 recensus (3C D-SKV18-L13:92 / 3A-D12:215 / 1E LAC-1E-V5-07:153) re-keyed to per-file provenance over the live 71-count. The V2→V3 fold (`e6c1c2a84`, cost hardening) did NOT carry CH3-V2-R1; 3E acknowledges the 67→71 drift only as a V4 CH4 OPEN QUESTION (3E:381). A carried invariant the cycle's own evidence refutes is a soft regression, and this is an ORPHAN (prior-cycle-directed, unfolded). | **REVISE** |

## REVISE Repair Directives

**CH3-V3-R1 — `3E-D11` / `3E-L14-HC-05` carry the SK-V18-refuted absolute-67
Pattern-H invariant; the prior-cycle CH3-V2-R1 repair went unfolded (severity:
MEDIUM; ORPHAN).** Owner: 3E. Target:
`restart/audit/totality/p3/3E-grammar-generalisation.md:110` (the
`3E-D11-pattern-h-provenance-before-deletion` delta cell, "Pattern H collapse
means 67/67 line-1 generated provenance ... and 0/67 generated headers") and the
sibling clause at `:151` (`3E-L14-HC-05 Pattern H provenance`, "67/67 generated
provenance plus regen/check proof"). Conflicting evidence: the SK-V18 recensus
re-keys the absolute-67 invariant to per-file provenance over the LIVE census
because `crates/core/src/runtime` is 71 at HEAD —
`3C` D-SKV18-L13-pattern-h-recensus
(`restart/audit/totality/p3/3C-locks-crystallisation.md:92`, `:113`),
`3A` ARCH-3A-V4-SK18-D12
(`restart/audit/totality/p3/3A-architecture-synthesis.md:215`, "the live
§7.4/§13.1 Pattern-H baseline command ... now returns 71, not the [67]"), and
`1E` LAC-1E-V5-07 (`restart/audit/totality/p1/1E-locks-evidence.md:153`,
"Re-key the Pattern H census invariant: the LAC-1E-15 67-file base ..."); the
live count is `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' |
wc -l == 71`. 3E itself acknowledges the drift only as a V4-extension OPEN
QUESTION (`restart/audit/totality/p3/3E-grammar-generalisation.md:381`), never
re-keying the carried delta body, and the V2→V3 fold commit
(`e6c1c2a84 docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`) carried
only the CH4 cost hardening — the 3E `revised:` frontmatter lists no CH3 fold.
Correction: add an inline recensus cross-ref to the `3E-D11` and `3E-L14-HC-05`
cells — "the absolute-67 figure is RE-KEYED by the SK-V18 recensus (3C
D-SKV18-L13 / 3A-D12 / 1E LAC-1E-V5-07) to per-file `@generated` provenance over
the LIVE census (71 at HEAD; the +4 traces to the tape-fold roster
`tape/{mod,cursor,arena,record}.rs`); an unattributable +N opens the
`[generated-size-budget]` O(N) scan" — so the carried delta no longer asserts an
absolute invariant the cycle's own evidence refutes. This is REGRESSION-lens (not
merely CH4-cost) because a carried delta that re-asserts a refuted absolute count
weakly contradicts the recensus the SK-V18 evidence strengthened. It does NOT
reopen a REDRESS route, revive a refuted-route, or weaken a hard lock — hence
REVISE, not REJECT. Because CH3-V2-R1 named this exact defect and the V3 fold did
not carry it, it is an ORPHAN REVISE and blocks a clean cycle.

## What Holds (the ACCEPT spine)

1. **No REDRESS route reopened (within the captured ledger).** Items
   51/53/246/247 and 96/97/98 are each carried with a verified line-span and an
   explicit ADMISSIBLE-vs-REJECTED distinction (1D:166-171; 3A-D08/D09/D01;
   3D-D08; 3E frontmatter; 3C-L10/L16; 3F-MH-003/008..013). The G4 `Cursor` is a
   VIEW over the existing Tape/ValueRef/PayloadArena (admissible); the G6 move
   RETARGETS the existing in-loop shell (admissible); neither is a second
   substrate, structural-stream driver, parser-local cursor, nor bespoke
   per-grammar mask. The COMPLETENESS gap (U-5, SK-V16/V17) is fenced as a
   pre-G2/G4/G6 Pass-Omega-V6 blocker, discharging CH3-V1-R2.

2. **3B revives no refuted wave.** The §13.6 tape-fold receivers are re-keyed
   SK-V18→SK-V19 with F1-F9 content preserved; the A-J set stays `pending`;
   §13.5 SK-V15 is preserved (CSS verdict UPGRADED to directional-pending-re-lock,
   not paper-closed); the F.W5 un-fork claim is correctly marked
   UNREALISED-in-both-trees (3B:95-101); the removed D03-D08/D11 are CONSUMED, not
   silently dropped.

3. **3D promotes no rejected route.** The G6 decision is RETARGET-not-wire-as-is
   (the refuted "wire `find_css_significant` as-is" carried REFUTED, 3D:128);
   the named-primitive (a)-(d) gate carries the refuted tree-walk as a REJECT
   condition (3D:131, 3D-D11); the relocated-seam md5-alone is REJECTED in favour
   of the 3-co-gate conjunction (3D:132, 3D-D12).

4. **3C weakens no lock REDRESS strengthened.** Every SK-V18 clause is additive
   or a sharpening (aarch64-PRIMARY→aarch64-ONLY; CollapsedStage admitted only as
   an inert slot that must clear the RETIRED REDRESS 96/97/98 prior; verbatim-blob
   courier REJECTED; the relocated-seam firewall and named-primitive gate REJECT a
   source-present/prose close). 16 locks and 5 BackendShape variants intact; the
   un-fork is ledger-novel (negative-witness verified == 0). The v+1 diff applies
   cleanly so these strengthening clauses CAN land.

5. **No delta revives one of the 13 T-P2-refuted assertions.** All carried as
   gates/fences (2C:134/307/308/309/310/312/313 resolve and say what the fences
   claim). The lone exception to "carried-as-fence" is the absolute-67 figure in
   3E-D11 / 3E-L14-HC-05 — a refuted count re-asserted in carried text (not a
   reopened route), which is CH3-V3-R1.

## Residual Risk

1. CH3-V3-R1 also touches CH4 (the cost/census lens) and CH1 (citation
   consistency). If CH1 or CH4 re-keys the `3E-D11` / `3E-L14-HC-05` figure in
   the same fold, this CH3 REVISE is discharged together; the three must not
   double-count. The defect has now survived TWO cycles (CH3-V2-R1 unfolded into
   V3); the V4 fold must scope the 3E Pattern-H re-key explicitly, not only CH4
   cost rows.
2. CH3-V1-R2's reconcile is a committed-ledger blocker, but until the SK-V16/V17
   reconcile actually runs at Pass-Omega-V6, the possibility that an SK-V16/V17-era
   streamed-cursor or second-scanner reject is re-entered by G6 cannot be
   excluded, only bounded by the pre-G2/G4/G6 gate. The packet states this
   honestly across 3D/3B/3F.
3. The packet's CH3 open questions (3B/3D/3F R2-mirrors; 3A) are each well-formed
   (receiver/blocker/gate present); none is rejected.

TALLY accept=19 revise=1 reject=0
