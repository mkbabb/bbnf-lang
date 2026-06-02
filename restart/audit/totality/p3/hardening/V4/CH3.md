# CH3 REGRESSION — SK-V18 T-P3 (cycle V4)

## Lens

No delta re-opens a `skinny/REDRESS.md` route; 3B revives no refuted wave; 3D
promotes no rejected route; 3C weakens no lock that REDRESS strengthened; no
delta revives one of the 13 T-P2-refuted assertions. Spot-verify the most
load-bearing deltas (a cited finding-id resolves; a cited LOCKS section exists;
the v+1 diff applies).

## Verdict Summary

Under the REGRESSION lens the V4 synthesis packet (Jun-1 regeneration, cycle
`V4-SKV18-totality`) is disciplined and the SINGLE open REVISE from the prior
cycle — `CH3-V3-R1` (the carried `3E-D11` / `3E-L14-HC-05` absolute-67
Pattern-H invariant) — is now DISCHARGED. Both cells carry an inline `RE-KEY
(CH3-V3-R1)` clause re-keying the absolute-67 figure to per-file `@generated`
provenance over the LIVE census (71 at HEAD; +4 = tape-fold roster
`tape/{mod,cursor,arena,record}.rs`), citing the SK-V18 recensus
(`3C` D-SKV18-L13 `:113` / `3A` ARCH-3A-V4-SK18-D12 `:218` / `1E`
LAC-1E-V5-07 `:153`); the 3E frontmatter `:48` records the V3-FOLD, and the only
residual `0/67` at `3E:113` is now a SOURCE citation of the PASS-IMPL finding
("0/67 generated headers"), NOT a live invariant assertion — `grep -nE '67/67'`
returns ZERO in 3E. The orphan that survived V2→V3 is closed.

Every REGRESSION conjunct holds. The four-item REDRESS pre-block (items
51/53/246/247) resolves at the cited line-spans, each carried with its falsifying
ADMISSIBLE-vs-REJECTED distinction (1D:156-173), not revived. The
SK-V16/V17 reconcile stays RETIMED to a Pass-Omega-V6 / pre-W-PRUNE blocker that
BLOCKS G2/G4/G6 entry, mirrored verbatim across 3D (×3) / 3B (×2) / 3F (×2). The
RETIRED REDRESS 96/97/98 scalar-cheaper-than-SIMD-cursor prior (the SK-V9
Wave-3 Union block, `REDRESS:2795-2944`, finding `:2928-2933`) is carried
FORWARD as a CollapsedStage promotion bar (3C-L10 `:92`/`:113`; 3A-D08 `:214`),
strengthening — not weakening — the lock. The un-fork ledger negative-witness
verifies at HEAD (`rg -ic 'relocated.seam|RuntimeEmitterKind|un.fork'
skinny/REDRESS.md == 0`): the un-fork owes no REDRESS id, it is SK-V18-NOVEL.
All 13 T-P2-refuted assertions (2C:132-138, :307-315, plus the SK-V18-2C inverse
rows :222-224) are carried as gates/fences. 3B's §13.6→SK-V19 re-key is a
tranche relabel, not a wave revival; 3D's G6 is RETARGET-not-wire-as-is; 3C's
SK-V18 clauses are additive sharpenings. The mandatory `3C-locks-v+1-diff.md`
spot-check PASSES (`git apply --check` exit 0).

This is a clean cycle under the REGRESSION lens. The cycle-V1 `>=30% REVISE`
expectation does not bind a converging V4 whose lone carried REVISE has been
folded; manufacturing a REVISE against discharged evidence would be dishonest.

## Evidence Commands And Outputs

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
awk '/^```diff$/{f=1;next} f&&/^```$/{exit} f{print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v4.diff
git apply --check /tmp/tp3-locks-v4.diff ; echo "apply exit=$?"
```

```text
16
71
apply exit=0
```

16 numbered locks intact (canon preserved). Runtime files = 71 (NOT the
LOCKS-asserted 67); D-SKV18-L13-pattern-h-recensus (3C:94/:115) + 3A-D12 (3A:218)
+ 1E LAC-1E-V5-07 (1E:153) all DISCLOSE the 71-vs-67 drift and attribute the +4
to `tape/{mod,cursor,arena,record}.rs` — honest, traced. The v+1 diff header is
`@@ -622,6 +622,33 @@`; `LOCKS.md:622` = the NEON-classifier clause, `:623`/`:624`
blank, `:625` = the governance heading — context matches; the LOCKS-strengthening
clauses that FENCE the REDRESS routes CAN land.

```sh
grep -nE '67/67' restart/audit/totality/p3/3E-grammar-generalisation.md   # absolute-67 live?
sed -n '742p;784p;6184p;6230p;2795p' skinny/REDRESS.md
rg -ic 'relocated.seam|RuntimeEmitterKind|un.fork' skinny/REDRESS.md
rg -nc 'G2/G4/G6 entry is BLOCKED until the SK-V16/V17' \
  restart/audit/totality/p3/{3B-master-plan-reconciliation,3D-skinny-fold,3F-migration-handoff}.md
```

```text
(67/67 in 3E = 0 — the absolute invariant is removed; only the V3-FOLD note + the
 PASS-IMPL "0/67" source citation remain)
742: 51. SK-V5 event-cursor redress: byte-class whitespace cursor is REJECTED.
784: 53. SK-V5 structural-mask parser-local cursor is REJECTED.
6184: ## SK-V14 W11T Parse-Only Structural Stream Reject   (item 246)
6230: ## SK-V14 W11V Parse-Only String64 Reject              (item 247)
2795: ## SK-V9 Wave 3 Union Event-Model Class-Column Redress (96/97/98 host block)
rg un-fork negative-witness == 0  (SK-V18-NOVEL; owes no REDRESS id)
G2/G4/G6-BLOCKED mirror: 3B=2, 3D=3, 3F=2  (consistent across all R2-mirror owners)
```

All REDRESS spans resolve to the asserted rejected shapes; the 96/97/98 host
block (the streaming-cursor / class-column substrate at `:2880`+, "REDRESS 96
landed the full class-column substrate", "Item 98 retires `G-W3-UNION-SUBSTRATE`")
matches the scalar-cheaper-than-SIMD finding 3C-L10/3A-D08 cite.

```sh
rg -n 'refuted' restart/audit/totality/p2/2C-grammar-neutrality.md | rg '307|308|309|310|311|312|313|314|315'
rg -nE 'tree-walk|md5-distinct|Nu8|find_css_significant|wire.*as.is' \
  restart/audit/totality/p3/3D-skinny-fold.md restart/audit/totality/p3/3A-architecture-synthesis.md
```

```text
2C:307-315 all resolve and say what the synthesis fences claim
 (neutral-name-on-one-grammar refuted; Nu8-litmus refuted -> precedence-tower;
  9-ident table refuted; 4-name regex refuted; css_types.rs refuted;
  md5-distinct necessary-not-sufficient; IR tree-walk regresses the 94.1% scan;
  enum-arm onboarding refuted; x86 closes no M5 row).
3D-D11 (3D:133) carries tree-walk as REJECT; 3D-D12 (3D:134) carries md5-alone as
REJECT in favour of the 3-co-gate; 3A-D09 (3A:215) carries "find_css_significant
is dead at admission" + "wire-as-is REFUTED"; 3A-D13 (3A:219) carries the Nu8
litmus refutation + precedence-tower replacement. None revived.
```

## Enumerated Dispositions Under The REGRESSION Lens

| # | delta / disposition | lens conjunct | finding | result |
|---|---|---|---|---|
| 1 | `3C-locks-v+1-diff.md` hunk | "the v+1 diff applies" (mandatory) | `git apply --check` exit 0 at HEAD; header `@@ -622,6 +622,33 @@`; LOCKS:622 = NEON-classifier clause, :623/:624 blank, :625 = governance heading. Fence clauses CAN land. | ACCEPT |
| 2 | `3E-D11` / `3E-L14-HC-05` Pattern-H recensus (CH3-V3-R1 fold) | "no delta weakens a recensus the SK-V18 evidence strengthened" | Both cells (`3E:113`, `3E:154`) now carry the inline RE-KEY clause to per-file provenance over the live 71; frontmatter `:48` records the V3-FOLD; `grep 67/67`=0; the residual `0/67` is a PASS-IMPL source citation, not a live invariant. ORPHAN DISCHARGED. | ACCEPT |
| 3 | SK-V16/V17 reconcile RETIME (CH3-V1-R2) | "no delta re-opens a REDRESS route" | Retimed to Pass-Omega-V6 / pre-W-PRUNE blocker; G2/G4/G6 entry BLOCKED until the SK-V16/V17 pre-block reconcile is on the committed ledger; mirrored 3D-D08(:130)/3D-CH3(:168), 3B(:30/:54), 3F-MH-003(:90)/CH3(:274). Per U-5's "before Pass Omega ratification". | ACCEPT |
| 4 | `3D-D08-substrate-sidecar-lock` | items 51/53/246/247 four-item pre-block | NEON G6 = RETARGET-onto-the-live-recursive-shell, never a wire-as-is dead-flat kernel or parser-local second scanner; cites 1D:166-171 (verified) + `find_css_significant` dead-at-admission (2E:80). Fences, does not revive. | ACCEPT |
| 5 | `3D-D11-one-generator-inflection-thesis` | 13-refuted "IR tree-walk preserves the 94.1% scan" (2C:313) | Carries the refutation as the (a)-(d) named-primitive gate (3D:133); "a tree-walk that inflates the flat scan into a combinator descent is REJECTED"; cites SYNTHESIS-RESEARCH "Candidate C (R-B/R-C full grammar-IR tree-walk) is REJECTED outright". | ACCEPT |
| 6 | `3D-D12-r16-relocated-seam-cogate` | 13-refuted "md5-distinct proves grammar-derived selection" (2C:312) | 3-co-gate conjunction {md5 ∧ branch==0 ∧ type==0 ∧ rows_collapsed} (3D:134); "md5-distinctness alone proves nothing about the un-fork (a REFUTED assertion)"; cites SYNTHESIS-AUDIT-OVERFIT:59-63 (verified). | ACCEPT |
| 7 | `3A-D08` CollapsedStage diagnostic-only slot | abuts REDRESS 96/97/98 | Carries the RETIRED streamed-cursor prior; promotion past `diagnostic-only` "must clear that retired prior" (3A:214 cites REDRESS:2795-2944; 3C-L10:92/:113). Fences. | ACCEPT |
| 8 | `3A-D09` G6 retarget-not-author + single-movemask | abuts items 51/53/247 | RETARGETS the LIVE generated hot-leaf shell (caller-data byte set, neutral inner); REFUTES "wire `find_css_significant` as-is" (3A:215); REDRESS-fenced against item 51/53/247. | ACCEPT |
| 9 | `3A-D13` Sheets precedence-tower negative control | 13-refuted "Nu8-tagged-alt is the Sheets litmus" (2C:308) | Carries the refutation (3A:219); precedence tower replaces the Nu8 litmus, lowers to EXISTING SinkOnlyExpr vocab, "needing NO new IR primitive". | ACCEPT |
| 10 | `3B` §13.6 re-key SK-V18→SK-V19 (MP-3B-SKV18-D01..D10) | "3B revives no refuted wave" | Relabel of the tape-fold receivers to a SK-V19 block (3B:37-38,:74); F1-F9 fold content preserved; A.W0..A.W4 stays pending (3B:105); §13.5 SK-V15 preserved; F.W5 marked the un-fork statement FED-by-SK-V18 (3B:96-100). No refuted wave revived. | ACCEPT |
| 11 | `3B` removed deltas D03-D08/D11 | silent-drop of a refuted-route fence? | Removed = SK-V15-routing deltas CONSUMED by landed §13.5/§13.6 via MP-3B-SKV17-D01..D08 (3B:19,:52); refuted-route FENCES carry forward as MP-3B-SKV18-D01..D10 standing gates. No fence silently dropped. | ACCEPT |
| 12 | `3C` D-SKV18-L10-collapsed-slot | "3C weakens no lock REDRESS strengthened" | Carries the RETIRED REDRESS 96/97/98 scalar-cheaper-than-SIMD-cursor prior FORWARD as a binding promotion bar (3C:92/:113; additive, not open question). Strengthens. | ACCEPT |
| 13 | `3C` D-SKV18-L08-aarch64-only | lock-sharpening vs weakening | SHARPENS SK-V17 aarch64-PRIMARY (LOCKS:622, verified NEON-classifier clause) to aarch64-ONLY; x86 = P1 deletion target. Strengthens. | ACCEPT |
| 14 | `3C` D-SKV18-L06-verbatim-blob | Lock-6 strengthening | Co-binds Lock-6 v+1 byte-equivalent-regen to the SK-V18 verbatim-blob prohibition; a `const CSS_GENERATED_RS: &str` courier is hand-written, REJECT as "grammar-driven". Strengthens. | ACCEPT |
| 15 | `3C` D-SKV18-L05-L10-unfork (negative-witness novelty) | route-novelty assertion | `rg -ic 'relocated.seam\|RuntimeEmitterKind\|un.fork' skinny/REDRESS.md == 0` VERIFIED at HEAD; "Ledger negative-witness confirms SK-V18-NOVEL, not a REDRESS 96/97/98 revival" (3C:135). | ACCEPT |
| 16 | `3D-D02`/`3D-D03` CSS posture softened to directional-pending-re-lock | "no delta paper-closes a refuted CSS route" | SK-V15 carried CSS as CONTRIVED/fake-admit; SK-V18 carries CSS MEASUREMENT-VALID but DIRECTIONAL pending the H1 css_canon_bench re-lock (3D:43,:88,:110,:123). A SOFTENING toward honesty (no fake admit), not a paper-close: CSS courier retires only with grammar-DERIVED body + same-run retime (3D-D03:125). | ACCEPT |
| 17 | `3F-MH-003` PRUNE-before-GENERALIZE delete-before-provider gate + R2 mirror | dependency-precedes-deletion; route revival | "no GENERALIZE/PROVE wave deletes a hand-written ORACLE before its grammar-DERIVED replacement lands byte-equivalent" (3F:90); AND G2/G4/G6 BLOCKED until SK-V16/V17 reconcile. Bars an SK-V16/V17-rejected shape from re-entering with no committed fence. | ACCEPT |
| 18 | `3F-MH-008..012` migration deletes (x86/courier/replicas/phantom) | delete-before-provider; route revival | x86 = reach-matched crate-wide delete, no RED-by-construction gate (MH-008:95); courier retires only with byte-equivalent oracle (MH-010:97); replicas collapse with structural `runtime_target_rows_collapsed` co-gate (MH-011:98); phantom `<G>` delete preserves K-axis (MH-012:99). All delete-before-provider-gated. | ACCEPT |
| 19 | `3F-MH-013` css_types.rs RELOCATE-or-DELETE | silent-drop check | DEFERRED to SK-V19 as an EXPLICIT migration decision (3F:100,:184), "discharges the no-silent-disposition rule"; mirrored 3D-D05/3C-L13/2C SK18-03. Not silently dropped. | ACCEPT |
| 20 | `3D-D05` Pattern-H provenance SK-V19 carry | delete-before-provider | Header-only or destructive delete without same-wave replacement rejects (3D:127); 67-file/6867-LOC census preserved as the SK-V19 repair surface; md5-distinctness falsifier carried. Sound. | ACCEPT |

## What Holds (the ACCEPT spine)

1. **No REDRESS route reopened (within the captured ledger).** Items
   51/53/246/247 and the 96/97/98 host block are each carried with a verified
   line-span and an explicit ADMISSIBLE-vs-REJECTED distinction (1D:156-173;
   3A-D08/D09; 3D-D08; 3C-L10/L16; 3F-MH-003/008..012). G4's `Cursor` is a VIEW
   over the existing Tape/ValueRef/PayloadArena (admissible); the G6 move
   RETARGETS the existing in-loop shell (admissible); neither is a second
   substrate, structural-stream driver, parser-local cursor, nor bespoke mask.
   The COMPLETENESS gap (U-5, SK-V16/V17) is fenced as a pre-G2/G4/G6
   Pass-Omega-V6 blocker.

2. **3B revives no refuted wave.** §13.6 tape-fold receivers re-keyed
   SK-V18→SK-V19 with F1-F9 content preserved; A.W0..A.W4 stays pending; §13.5
   SK-V15 preserved; F.W5 un-fork is FED-by-SK-V18; the removed D03-D08/D11 are
   CONSUMED, not silently dropped.

3. **3D promotes no rejected route.** G6 = RETARGET-not-wire-as-is (refuted
   "wire `find_css_significant` as-is" carried REFUTED); the (a)-(d) gate carries
   the refuted tree-walk as a REJECT condition (3D-D11); the relocated-seam
   md5-alone is REJECTED in favour of the 3-co-gate (3D-D12). The CSS posture
   softens from contrived to directional-pending — toward honesty, not closure.

4. **3C weakens no lock REDRESS strengthened.** Every SK-V18 clause is additive
   or a sharpening (aarch64-PRIMARY→aarch64-ONLY; CollapsedStage admitted only as
   an inert slot that must clear the RETIRED REDRESS 96/97/98 prior; verbatim-blob
   courier REJECTED; relocated-seam firewall + named-primitive gate REJECT a
   source-present/prose close). 16 locks and 5 BackendShape variants intact; the
   un-fork is ledger-novel (negative-witness == 0). The v+1 diff applies cleanly.

5. **No delta revives one of the 13 T-P2-refuted assertions.** All carried as
   gates/fences (2C:307-315 + SK-V18-2C:222-224 resolve and say what the fences
   claim). The lone prior exception — the absolute-67 figure in 3E-D11 /
   3E-L14-HC-05 — is now RE-KEYED to the live census; `grep 67/67` returns ZERO.

## Residual Risk

1. CH3-V1-R2's reconcile is a committed-ledger blocker, but until the SK-V16/V17
   reconcile actually runs at Pass-Omega-V6, the possibility that an SK-V16/V17-era
   streamed-cursor or second-scanner reject is re-entered by G6 cannot be
   excluded, only bounded by the pre-G2/G4/G6 gate. The packet states this
   honestly across 3D/3B/3F.
2. The CH3 open questions (3B/3D/3F R2-mirrors; 3A) are each well-formed
   (receiver/blocker/gate present); none is rejected.
3. The CH3-V3-R1 fold overlaps CH1 (citation) and CH4 (cost/census); if those
   lenses also re-key the 3E figure, the discharge is shared, not double-counted.
   At this V4 HEAD the 3E re-key is present and self-consistent.

TALLY accept=20 revise=0 reject=0
