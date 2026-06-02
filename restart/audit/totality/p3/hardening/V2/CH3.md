# CH3 REGRESSION — SK-V18 T-P3 V2 (cycle V2)

## Lens

No delta re-opens a `skinny/REDRESS.md` route; 3B revives no refuted wave; 3D
promotes no rejected route; 3C weakens no lock that REDRESS strengthened; no
delta revives one of the 13 T-P2-refuted assertions. Spot-verify the most
load-bearing deltas (cited finding-id resolves; cited LOCKS section exists; the
v+1 diff applies).

## Verdict Summary

Under the REGRESSION lens the V2-folded packet is disciplined and the two V1
REVISE items are DISCHARGED. The mandatory `3C-locks-v+1-diff.md` spot-check now
PASSES (`git apply --check` exit 0; CH3-V1-R1 closed), and the SK-V16/V17 REDRESS
reconcile is RETIMED from an SK-V19-entry obligation to a Pass-Omega-V6 /
pre-W-PRUNE blocker that BLOCKS G2/G4/G6 entry, mirrored verbatim across the three
owners 3D-D08 / 3B-CH3 / 3F-MH-003 (CH3-V1-R2 closed). Every adjoining
REDRESS-rejected route (items 51/53/246/247; 96/97/98) is carried with its
falsifying ADMISSIBLE-vs-REJECTED distinction, not revived; every one of the 13
T-P2-refuted assertions is carried as a fence/gate; 3B's §13.6 re-key is a tranche
relabel (SK-V18→SK-V19), not a wave revival; 3C strengthens (does not weaken) the
locks REDRESS strengthened. The cited finding-ids and LOCKS anchors I spot-checked
all resolve, and the un-fork novelty claim's ledger negative-witness verifies at
HEAD (`rg -ic 'relocated.seam|RuntimeEmitterKind|un.fork' skinny/REDRESS.md == 0`).

One residual REGRESSION-lens defect forces a single REVISE: the carried SK-V15
delta `3E-D11` (and its sibling clause `3E-L14-HC-05`) still defines the Pattern-H
gate as the ABSOLUTE "67/67 generated provenance" invariant, which the SK-V18
recensus (`3C` D-SKV18-L13 / `3A` D11 / 1E LAC-1E-V5-07) explicitly re-keyed to
per-file-provenance over the LIVE census (71 at HEAD). 3E carries the 67→71 drift
only as a V4-extension OPEN QUESTION; the carried delta TEXT itself was never
re-keyed and now asserts an invariant the cycle's own evidence refutes. This is a
soft regression (a carried delta weakly contradicts a recensus the SK-V18 evidence
strengthened), not a route revival — REVISE, not REJECT.

## Evidence Commands And Outputs

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
awk '/^```diff$/{f=1;next} f&&/^```$/{exit} f{print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v2.diff
git apply --check /tmp/tp3-locks-v2.diff ; echo "exit=$?"
```

```text
16
      71
exit=0
```

16 numbered locks intact (canon preserved). Runtime files = 71 (NOT the
LOCKS-asserted 67); D-SKV18-L13-pattern-h-recensus + 3A-D11 + 1E LAC-1E-V5-07 all
DISCLOSE the 71-vs-67 drift and attribute the +4 to
`tape/{mod,cursor,arena,record}.rs` — honest, traced, not a silent regression. The
v+1 diff now APPLIES CLEANLY (header re-derived to `@@ -622,6 +622,33 @@`; the
missing second blank context line is present), so the LOCKS-strengthening clauses
that fence the REDRESS routes CAN land — CH3-V1-R1 is folded.

```sh
sed -n '156,171p;244,248p' restart/audit/totality/p1/1D-skinny-lessons.md
sed -n '742,745p;784,787p;6184,6186p;6230,6232p' skinny/REDRESS.md
sed -n '2795,2797p;2928,2933p' skinny/REDRESS.md
rg -ic 'relocated.seam|RuntimeEmitterKind|un.fork' skinny/REDRESS.md
```

```text
1D:166-171 Rejected-Route Pre-Block resolves: item 246 = REDRESS:6184 (W11T
parse-only structural stream, REJECT); item 247 = REDRESS:6230 (W11V parse-only
string64 mask, REJECT); item 51 = REDRESS:742 (JsonEventCursor, REJECT); item 53
= REDRESS:784 (JsonStructuralCursor, REJECT). REDRESS:2795 = "SK-V9 Wave 3 Union
Event-Model Class-Column Redress"; the load-bearing finding at :2928-2933 = the
M5-Max scalar-cheaper-than-SIMD-cursor result. U-5 verify_action at 1D:244-248 =
"reconcile SK-V16/V17 ... before Pass Omega ratification". rg ledger
negative-witness = 0 (un-fork owes no REDRESS id; SK-V18-NOVEL).
```

```sh
rg -n 'refuted' restart/audit/totality/p2/2C-grammar-neutrality.md | rg '134|307|308|309|310|312|313'
rg -c 'G2/G4/G6 entry is BLOCKED until the SK-V16/V17' \
  restart/audit/totality/p3/3B-master-plan-reconciliation.md \
  restart/audit/totality/p3/3D-skinny-fold.md \
  restart/audit/totality/p3/3F-migration-handoff.md
```

```text
2C:134/307/308/309/310/312/313 all resolve and say what the synthesis fences
claim (CSS_GENERATED_RS not grammar-derived; neutral-name-on-one-grammar; Nu8
litmus; 9-ident table; 4-name regex; md5-distinctness necessary-not-sufficient;
tree-walk regresses the 94.1% scan). The G2/G4/G6 blocker statement appears
3B=2, 3D=3, 3F=2 — present and consistent across all three R2-mirror owners.
```

## Enumerated Dispositions Under The REGRESSION Lens

| # | delta / disposition | lens conjunct | finding | result |
|---|---|---|---|---|
| 1 | `3C-locks-v+1-diff.md` hunk (CH3-V1-R1 fold) | "the v+1 diff applies" (mandatory) | `git apply --check` exit 0; header re-derived `@@ -622,6 +622,33 @@`; the second blank context line is present; 5 ctx + 28 add = 33. The LOCKS-strengthening fence clauses CAN now land. | ACCEPT |
| 2 | SK-V16/V17 reconcile RETIME (CH3-V1-R2 fold) | "no delta re-opens a REDRESS route" | Retimed from SK-V19-entry to a Pass-Omega-V6 / pre-W-PRUNE blocker; G2/G4/G6 entry BLOCKED until the SK-V16/V17 pre-block reconcile is on the committed ledger; mirrored 3D-D08(:128)/3D-CH3-Q(:166), 3B-CH3-Q(:256), 3F-MH-003(:85)/3F-CH3-Q(:265). Per U-5's own "before Pass Omega ratification". | ACCEPT |
| 3 | `3D-D08-substrate-sidecar-lock` | items 51/53/246/247 four-item pre-block | NEON G6 = RETARGET-onto-the-live-recursive-shell, never a wire-as-is dead-flat kernel or parser-local second scanner; cites 1D:166-171 verified pre-block. Fences, does not revive. | ACCEPT |
| 4 | `3D-D11-one-generator-inflection-thesis` | 13-refuted "tree-walk preserves the 94.1% scan" (2C:313) | Carries the refutation as the (a)-(d) named-primitive gate; "a tree-walk that inflates the flat scan into a combinator descent is REJECTED" (3D:131). | ACCEPT |
| 5 | `3D-D12-r16-relocated-seam-cogate` | 13-refuted "md5-distinctness proves un-fork" (2C:312) | 3-co-gate conjunction {md5 ∧ branch==0 ∧ type==0 ∧ rows_collapsed}; does NOT credit md5 alone (3D:132). | ACCEPT |
| 6 | `3A-D08` CollapsedStage diagnostic-only slot | abuts REDRESS 96/97/98 | Carries the RETIRED streamed-cursor prior; promotion past `diagnostic-only` "must clear that retired prior" (3A:211; 3C-L10:90); cites REDRESS:2795-2944 finding :2928-2933. Fences. | ACCEPT |
| 7 | `3A-D09` G6 retarget-not-author + single-movemask | abuts items 51/53/247 | RETARGETS the LIVE generated hot-leaf shell (caller-data byte set, neutral inner); "REDRESS-fenced against item 51/53/247" (3A:212); REFUTES "wire `find_css_significant` as-is". | ACCEPT |
| 8 | `3A-D01` phantom `<G>` generality-vehicle strike | second-substrate revival | The `Cursor` micro-trait is a VIEW over the existing Tape/ValueRef/PayloadArena, "REDRESS-fenced against item 51/53" (3A:204); K-axis preserved verbatim. | ACCEPT |
| 9 | `3A-D13` Sheets precedence-tower negative control | 13-refuted "Nu8-tagged-alt is the Sheets litmus" (2C:308) | Carries the refutation; the precedence tower replaces the Nu8 litmus; lowers to EXISTING SinkOnlyExpr vocab, no new IR primitive (3A:216). | ACCEPT |
| 10 | `3B` §13.6 re-key SK-V18→SK-V19 (MP-3B-SKV18-D02) | "3B revives no refuted wave" | Relabel of the tape-fold block + sequencing move; F1-F9 fold-design content preserved verbatim; A-J stays pending; §13.5 SK-V15 block preserved (not refuted). No refuted wave revived (3B:127-133, 191-192). | ACCEPT |
| 11 | `3B` §13.7 GENERALIZATION block + binding lattice | "no §13.7 wave reopens a fenced route" | Every wave carries a same-wave consumer + RED exit falsifier; G3 5-conjunct exit (`emit_shape_source==lowered_program`); the SK-V19 tee-up routes the carried totality-tree leaks FORWARD, not as SK-V18 revival (3B:135-185). | ACCEPT |
| 12 | `3B-MP-3B-V1-D02` carried (A-J stubs pending) | silent-drop / paper-close of a refuted fence | Scoped landings labelled scoped/partial/refuted, not V1/root close; the refuted-route fences carry forward as standing gates (3B:202). | ACCEPT |
| 13 | `3C` D-SKV18-L10-collapsed-slot | "3C weakens no lock REDRESS strengthened" | Carries the RETIRED REDRESS 96/97/98 scalar-cheaper-than-SIMD-cursor prior FORWARD as a promotion bar (additive); binding gate, not open question (3C:90, 136). Strengthens. | ACCEPT |
| 14 | `3C` D-SKV18-L08-aarch64-only | lock-sharpening vs weakening | SHARPENS the SK-V17 aarch64-PRIMARY clause (LOCKS:622, verified to be the NEON-classifier clause) to aarch64-ONLY; x86 = deletion target (3C:85, 125). Strengthens. | ACCEPT |
| 15 | `3C` D-SKV18-L06-verbatim-blob | Lock-6 strengthening | Co-binds the Lock-6 v+1 byte-equivalent-regen rule to the SK-V18 verbatim-blob prohibition; round-trip byte-equivalence is the binding proof (3C:86, 126). Strengthens. | ACCEPT |
| 16 | `3C` LAC-2D-V3-01 un-fork "ledger negative-witness" novelty | route-novelty assertion | `rg -ic 'relocated.seam\|RuntimeEmitterKind\|un.fork' skinny/REDRESS.md == 0` VERIFIED at HEAD; the emitter discriminator is a different object than a retained structural cursor; SK-V18-NOVEL, not a REDRESS 96/97/98 revival (3C:133; 2D:95). | ACCEPT |
| 17 | `3C` LAC-2F-V3-03 DEFER (re-scope the "gap" frame) | silent-drop check | Folded as a one-line audit-scope note into D-SKV18-L16 with a named SK-V19 re-entry trigger; NOT silently dropped (3C:142). | ACCEPT |
| 18 | `3C` D-SKV18-L13-pattern-h-recensus | "3C weakens no lock" — Pattern-H invariant | Re-keys the ABSOLUTE-67 invariant to per-file provenance over the LIVE census (71 at HEAD); +4 traced to tape-fold roster; an unattributable +N opens an O(N) generator-regression scan (3C:92, 113). Strengthens. (See #20 for the 3E delta that did NOT absorb this recensus.) | ACCEPT |
| 19 | `3F-MH-008..013` migration deletes (x86/courier/replicas/phantom/css_types) | delete-before-provider; route revival | x86 = diagnostic-only plane delete; courier retires only with byte-equivalent oracle (3F-MH-010); replicas collapse with structural row-collapse co-gate (3F-MH-011); phantom delete preserves K-axis (3F-MH-012); css_types DEFERRED to SK-V19 (3F-MH-013), not silently dropped. All delete-before-provider-gated. | ACCEPT |
| 20 | `3E-D11` / `3E-L14-HC-05` carried Pattern-H gate definition | "no delta weakens a recensus the SK-V18 evidence strengthened" | The carried delta TEXT (3E:110, 151) still defines the gate as the ABSOLUTE "67/67 line-1 generated provenance" / "0/67 generated headers", which the SK-V18 recensus (3C D-SKV18-L13 / 3A-D11 / 1E LAC-1E-V5-07) re-keyed to per-file-provenance over the live 71-count. 3E carries the 67→71 drift only as a V4 OPEN QUESTION (3E:381), never re-keying the delta body. A carried invariant the cycle's own evidence refutes is a soft regression. | **REVISE** |

## REVISE Repair Directives

**CH3-V2-R1 — `3E-D11` carries the SK-V18-refuted absolute-67 Pattern-H invariant
(severity: MEDIUM).** Owner: 3E. Target:
`restart/audit/totality/p3/3E-grammar-generalisation.md:110` (the
`3E-D11-pattern-h-provenance-before-deletion` delta cell, "Pattern H collapse
means 67/67 line-1 generated provenance ... and 0/67 generated headers") and the
sibling clause at `:151` (`3E-L14-HC-05 Pattern H provenance` "67/67 generated
provenance"). Conflicting evidence: the SK-V18 recensus re-keys the absolute-67
invariant to per-file provenance over the LIVE census because
`crates/core/src/runtime` is 71 at HEAD — `3C` D-SKV18-L13-pattern-h-recensus
(`restart/audit/totality/p3/3C-locks-crystallisation.md:92`, `:113`), `3A`
ARCH-3A-V4-SK18-D11 (`restart/audit/totality/p3/3A-architecture-synthesis.md:214`),
and 1E LAC-1E-V5-07 (`restart/audit/totality/p1/1E-locks-evidence.md:153`); the
live count is `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc
-l == 71`. 3E itself acknowledges the drift only as a V4-extension OPEN QUESTION
(`restart/audit/totality/p3/3E-grammar-generalisation.md:381`), never re-keying
the carried delta body. Correction: add an inline recensus cross-ref to the
`3E-D11` and `3E-L14-HC-05` cells — "the absolute-67 figure is RE-KEYED by the
SK-V18 recensus (3C D-SKV18-L13 / 3A-D11 / 1E LAC-1E-V5-07) to per-file
`@generated` provenance over the LIVE census (71 at HEAD; the +4 traces to the
tape-fold roster `tape/{mod,cursor,arena,record}.rs`); an unattributable +N opens
the `[generated-size-budget]` O(N) scan" — so the carried delta no longer asserts
an absolute invariant the cycle's own evidence refutes. This is REGRESSION-lens
(not merely CH4-cost) because a carried delta that re-asserts a refuted absolute
count weakly contradicts the recensus that the SK-V18 evidence strengthened. It
does NOT reopen a REDRESS route, revive a refuted-route, or weaken a hard lock —
hence REVISE, not REJECT.

## What Holds (the ACCEPT spine)

1. **No REDRESS route reopened (within the captured ledger).** Items
   51/53/246/247 and 96/97/98 are each carried with a verified line-span and an
   explicit ADMISSIBLE-vs-REJECTED distinction (1D:166-171; 3A-D08/D09/D01;
   3D-D08; 3E frontmatter:48-56; 3C-L10/L16; 3F-MH-003/008..013). The G4 `Cursor`
   is a VIEW over the existing Tape/ValueRef/PayloadArena (admissible); the G6
   move RETARGETS the existing in-loop shell (admissible); neither is a second
   substrate, structural-stream driver, parser-local cursor, nor bespoke
   per-grammar mask. The COMPLETENESS gap (U-5, SK-V16/V17) is now fenced as a
   pre-G2/G4/G6 Pass-Omega-V6 blocker, discharging CH3-V1-R2.

2. **3B revives no refuted wave.** The §13.6 tape-fold receivers are re-keyed
   SK-V18→SK-V19 with F1-F9 content preserved verbatim; the A-J set stays
   `pending`; §13.5 SK-V15 block is preserved (the CSS verdict is UPGRADED to
   directional-pending-re-lock, not paper-closed); the F.W5 un-fork claim is
   correctly marked UNREALISED-in-both-trees (3B:96-101).

3. **3D promotes no rejected route.** The G6 decision is RETARGET-not-wire-as-is
   (the refuted "wire `find_css_significant` as-is" carried REFUTED, 3D:128,
   3E:50); the named-primitive (a)-(d) gate carries the refuted tree-walk as a
   REJECT condition (3D:131, 3D-D11).

4. **3C weakens no lock REDRESS strengthened.** Every SK-V18 clause is additive
   or a sharpening (aarch64-PRIMARY→aarch64-ONLY; CollapsedStage admitted only as
   an inert slot that must clear the RETIRED REDRESS 96/97/98 prior; verbatim-blob
   courier REJECTED; the relocated-seam firewall and named-primitive gate REJECT a
   source-present/prose close, never confer one). 16 locks and 5 BackendShape
   variants intact; the un-fork is ledger-novel (negative-witness verified).

5. **No delta revives one of the 13 T-P2-refuted assertions.** All carried as
   gates/fences (2C:134/307/308/309/310/312/313 resolve; 3E frontmatter:48-56
   lists all 8 SK-V18 refutations incl. the bracket_depth_mask_64 /
   REDRESS-96/97/98 fence at :56).

## Residual Risk

1. CH3-V2-R1 also touches CH4 (the cost/census lens) and CH1 (citation
   consistency). If CH1 or CH4 re-keys the `3E-D11` figure in the same fold, this
   CH3 REVISE is discharged together; the three must not double-count.
2. CH3-V1-R2's reconcile is now a committed-ledger blocker, but until the
   SK-V16/V17 reconcile actually runs at Pass-Omega-V6, the possibility that an
   SK-V16/V17-era streamed-cursor or second-scanner reject is re-entered by G6
   cannot be excluded, only bounded by the pre-G2/G4/G6 gate. The packet now
   states this honestly across 3D/3B/3F.
3. The packet's CH3 open questions (3B:256, 3D:166, 3F:265, 3A:268) are each
   well-formed (receiver/blocker/gate present); none is rejected.

TALLY accept=19 revise=1 reject=0
