---
lens: CH2
name: GENERALITY / LOCK 14
pass: T-P3-synthesis
cycle: V3
generated_at: 2026-05-24T00:30:00-04:00
disposition: ACCEPT
scope: "CH2 generality and Lock 14 carry-forward at V3 HEAD; verify F-V2-CH2+CH6+CH7-3F-A LAC-1E-14 4-site mirror discharge survives V3 micro-fold (V3 only touched 3F:123 bound command)"
v3_head_commit: b9b800e144be836785efdf238e5d58ea244372f2
artifacts_audited:
  - restart/audit/totality/p3/3A-architecture-synthesis.md
  - restart/audit/totality/p3/3B-master-plan-reconciliation.md
  - restart/audit/totality/p3/3C-locks-crystallisation.md
  - restart/audit/totality/p3/3C-locks-v+1-diff.md
  - restart/audit/totality/p3/3D-skinny-fold.md
  - restart/audit/totality/p3/3E-grammar-generalisation.md
  - restart/audit/totality/p3/3F-migration-handoff.md
  - restart/audit/totality/p3/hardening/V2/CH2.md
  - restart/audit/totality/p3/hardening/V3/CHALLENGE-CONTEXT.md
---

# T-P3 V3 CHALLENGE — CH2 GENERALITY Lens

Pass: T-P3 Synthesis. Cycle: V3. Lens: CH2 GENERALITY.
Date: 2026-05-24. HEAD: `b9b800e14` (V3 micro-fold commit; later commit `a4df15abc` is V3 context-seed with zero T-P3 artefact deltas). HARD CAP: 25 min.

## Scope

Per `restart/audit/totality/p3/hardening/V3/CHALLENGE-CONTEXT.md:27` §2 CH2 row:
verify F-V2-CH2+CH6+CH7-3F-A LAC-1E-14 4-site mirror discharge in 3F still
intact at V3 HEAD (V3 only touched `3F:123` bound command — verify LAC-1E-14
wording at `:104`, `:125`, `:311`, `:327` is unaffected). Verify Lock 14
preservation across all 7 T-P3 artefacts: 3E 5×15 CSS L4 sub-grammar matrix +
Sheets/BBNF-self/EBNF/BNF/CSV/math rows; 3C accepts no JSON-narrowing
amendment; 7-step future-grammar onboarding test survives; 12 L14-HC clauses
intact. CH2 V2 trajectory: 100% (11/11) → V3 target: 100% (LOCK extension;
3-cycle = V2+V3+predicted-V4).

## Findings

### F-V3-CH2-01 — 3F LAC-1E-14 4-site mirror UNTOUCHED by V3 (V3 only edited :123 bound command) — ACCEPT

V3 diff against 3F at `b9b800e14` shows a single hunk `@@ -120,7 +120,7 @@`
covering only line :123 (the 3F-MIG-003 bound command `find … -maxdepth 2 …`
→ `find … -mindepth 2 …` drop per F-V3-CH7-3F). The four LAC-1E-14 mirror
sites at `:104`, `:125`, `:311`, `:327` are outside the hunk and bit-identical
to V2 HEAD. Evidence: `git show b9b800e14 -- restart/audit/totality/p3/3F-migration-handoff.md`
shows the single-hunk +/- pair touches only line 123 (the bound command).

The 4-site canonical 5th-category wording grep at HEAD returns exactly 4 hits:

```
$ grep -cn "5th admitted-product category" restart/audit/totality/p3/3F-migration-handoff.md
4
```

Per-site verification at HEAD:

| Site | Path:line | Canonical elements present |
|------|-----------|----------------------------|
| 3F-MIG-004 table row | `restart/audit/totality/p3/3F-migration-handoff.md:104` | (a) "5th admitted-product category at the Lock 1 SUBSTRATE manifest" ✓; (b) "NOT a 6th \`BackendShape\` variant" ✓; (c) "5-shape \`BackendShape\` search domain at Lock 10 (\`{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}\`) holds" ✓; (d) "two axes … are ORTHOGONAL" ✓ |
| 3F-MIG-004 proposed-text §4 | `restart/audit/totality/p3/3F-migration-handoff.md:125` | (a) "**5th admitted-product category at the Lock 1 SUBSTRATE manifest**" (bolded) ✓; (b) "**NOT a 6th \`BackendShape\` variant**" (bolded) ✓; (c) "5-shape \`BackendShape\` search domain at Lock 10 — \`{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}\` — HOLDS" ✓; (d) "two axes … are ORTHOGONAL" ✓ |
| 3F CH2 open question (RESOLVED) | `restart/audit/totality/p3/3F-migration-handoff.md:311` | (a) "5th admitted-product category at the **Lock 1 SUBSTRATE manifest**" ✓; (b) "NOT a 6th \`BackendShape\` variant" ✓; (c) "5-shape \`BackendShape\` search domain at Lock 10 (\`{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}\`) HOLDS" ✓; (d) "two axes … are ORTHOGONAL" ✓ |
| 3F discipline citation (§8.2 binding) | `restart/audit/totality/p3/3F-migration-handoff.md:327` | (a) "5th admitted-product category at the Lock 1 SUBSTRATE manifest" ✓; (b) "NOT a 6th \`BackendShape\` variant" ✓; (c) "5-shape Lock 10 search domain HOLDS" ✓; (d) "two axes are orthogonal" ✓ |

All four sites preserve the V2-canonical wording verbatim. V2 over-discharge at
`:327` (the §8.2 5-shape coherence binding inscribed into the citation block)
survives V3 — F-V2-CH2-V2-04 carries forward unchanged.

### F-V3-CH2-02 — 3E 5×15 CSS L4 sub-grammar matrix intact (zero V3 edits on 3E) — ACCEPT

V3 diff confirms zero V3 edits on `3E-grammar-generalisation.md` (the V3 fold
packet at `b9b800e14` touches only 3 files: 3B + 3C-diff + 3F per `git show
b9b800e14 --stat`). Per `restart/audit/totality/p3/3E-grammar-generalisation.md:93`
the heading "BackendShape Matrix — 5 Shapes × 15 CSS L4 Sub-Grammars" remains;
the matrix body at `:114`-`:130` enumerates all 15 sub-grammars verbatim
(`tokens`, `stylesheet`, `selectors`, `properties`, `values`, `value-unit`,
`keywords`, `color`, `gradients`, `transforms`, `filters`, `easing`,
`func-body`, `keyframes`, `media`) with the 5 `BackendShape` columns
(`EagerTape`/`OffsetTape`/`EventTape`/`SinkOnly`/`CollapsedStage`).

Other-grammar matrix at `:139`-`:151` covers Sheets (4 rows for formula/cellRef,
function/LET/LAMBDA, infix, etc.), BBNF-self (3 rows for grammar dispatch,
expression/operator chain, directives + literal `\u`), EBNF/BNF (rule), CSV
(record/field), math (expression/operator); CollapsedStage row pinned x86-only
with admission gate. Primitive vocabulary transfer table at `:155`-`:165`
covers 8 primitive families across CSS L4 / Sheets / BBNF-self with explicit
hard-gate per primitive.

### F-V3-CH2-03 — 7-step future-grammar onboarding test survives at HEAD — ACCEPT

`restart/audit/totality/p3/3E-grammar-generalisation.md:167` heading
"Future-Grammar Onboarding Test (per 2C V4 7-step protocol)" intact; all 7
steps enumerated at `:173`-`:187` (1. grammar source + metadata only; 2.
regenerate rostered surfaces; 3. grammar-name leak scan; 4. grammar-shape leak
scan; 5. five-shape eligibility fixture; 6. primitive same-wave non-JSON
consumer; 7. telemetry/provenance consumed by gate). Fail-closed rule at
`:189`: "Fail closed if onboarding requires a new directive, BIR variant,
\`BackendShape\`, public substrate API, retained sidecar, or hand-coded
generic behavior." Zero V3 edits on this section.

### F-V3-CH2-04 — 3C accepts no JSON-narrowing amendment (zero V3 edits on 3C-locks-crystallisation.md) — ACCEPT

V3 diff confirms zero V3 edits on `3C-locks-crystallisation.md` (V3 touched
`3C-locks-v+1-diff.md:69` only — V4-1 hunk preface arithmetic correction per
F-V3-CH7-3C). 3C disposition matrix at HEAD: 38 ACCEPT + 13 MODIFY + 0 REJECT
+ 0 DEFER (verified via `grep -E "^\| ACCEPT|^\| MODIFY|^\| REJECT|^\| DEFER"
restart/audit/totality/p3/3C-locks-crystallisation.md`). LAC-1E-14 V4
disposition at `:120` carries the 5th *substrate* category framing verbatim:
"this is a 5th *substrate* category at the Lock 1 manifest level, NOT a 6th
\`BackendShape\` variant (the 5-shape canon at Lock 10 holds; \`FactStream\`
substrate_target = \`admitted_fact_output\`)". 3C V4 hunk V4-3 at
`restart/audit/totality/p3/3C-locks-v+1-diff.md:125,131` carries the canonical
diff body: "+ 5th admitted-product category at the Lock 1 substrate manifest…
+ variant. The 5-shape \`BackendShape\` search domain at Lock 10 holds:".

No JSON-narrowing language anywhere in the V2-amended 3C surface; the V3
:69 edit on `3C-locks-v+1-diff.md` (hunk preface `31:69` → `32:69`
arithmetic correction) is a CH7 numerator-only fix with zero CH2 surface
contact.

### F-V3-CH2-05 — 5-shape canon preserved cohort-wide (3A + 3B + 3D zero V3 edits) — ACCEPT

V3 fold touched only 3B at `:124,:217` (Pattern H bound command `-maxdepth 2`
drop per F-V3-CH7-3B). The 3B 5-shape coherence row at `:182` ("5-shape
\`BackendShape\` canon stays coherent | ARCH-3A-D03 + ARCH-3A-D04 … |
MP-3B-V1-D06 + MP-3B-V1-D08 (5-shape canon unchanged; FactStream is
substrate-target classification, not 6th BackendShape; W7 PRUNE-5 wires
CSP-selected shape to LOAD-BEARING) | 3E-D01 + 3E-D02 + 3E-D05 …") is outside
the V3 hunks and carries V2 wording verbatim. MP-3B-V1-D06 at `:127` preserves
"The 5-shape BackendShape canon stays unchanged; the FactStream category is a
substrate-target classification, not a 6th BackendShape" verbatim.

3A executive summary at `:23` preserves "the 5-shape canon, the substrate-union
fence, and the no-new-directive/no-new-BIR/no-new-substrate gate"; ARCH-3A-D07
at `:39` preserves "NOT a sixth BackendShape" wording. 3A `:27` preserves
"the 5-shape \`BackendShape\` canon at \`restart/ARCHITECTURE.md:1063-1087\`
stays intact per PASS-3-SYNTHESIS §8.2". 3D `:101` preserves the SK-V12
W1a row with explicit "NOT a sixth BackendShape" carrier note; 3D `:183`
preserves "CSS fact streams = output planes, not 6th shape (\`1B-codegen-evidence.md:36\`;
\`PASS-3-SYNTHESIS.md:211\`)".

### F-V3-CH2-06 — 12 L14-HC clauses intact at V3 HEAD — ACCEPT

`restart/audit/totality/p3/3E-grammar-generalisation.md:200`-`215` enumerates
all 12 Lock 14 hardening clauses (L14-HC-01..L14-HC-08 retained from V3
baseline; L14-HC-09..L14-HC-12 added in V4 per `:212`-`:215`). L14-HC-07
"fact streams are output planes" at `:210` preserves the canonical wording:
"They are not hidden retained sidecars and do not create a sixth
\`BackendShape\`." Zero V3 edits on this section.

### F-V3-CH2-07 — LAC-2F-V5-02 ELEVATED preserved (V2 CH2 F10 carry-forward) — ACCEPT

3C V4 elevation of LAC-2F-V5-02 to STRONGEST AMENDMENT SURFACE intact at
`restart/audit/totality/p3/3C-locks-crystallisation.md:32`: "ELEVATED amendment
— no cross-call retained classifier state, period (quote-mask, escape-mask,
structural-mask, class-stream, prev-state byte, prefix-XOR carry word). Carry
MUST stay within a single chunk-call boundary. … Generalises REDRESS 96/97/98
to ALL transient classifier-state primitives." Carried verbatim across all 4
carriers (3A ARCH-3A-D06 Part (a) + 3B MP-3B-V1-D10 + 3D FOLD-3D-012 + 3F
MIG-005 disambiguation) per V2 CH3 + CH5 cross-lens convergence. Zero V3
regression.

### F-V3-CH2-08 — V2 CH2 carry-forward constraints 1-8 hold at V3 HEAD — ACCEPT

V2 CH2 constraints 1-7 (V1-carried generated-output fence, negative-control,
provider-manifest, primitive policy, L14-HC-09 enum-drift, L14-HC-10
pass-layer leak) and constraint 8 (V2-NEW §327 §8.2 mirror clause) all hold
unchanged at V3 HEAD. The V2 mirror-gate structure at `:327` is intact (V3
edit at `:123` is structurally separate from the citation block at `:327`,
which sits at end-of-document and remains bit-identical to V2). Per V2 CH2
constraint 8: "modifying any one of `:104`/`:125`/`:311` without re-mirroring
`:327` (or vice versa) is a CH2 coherence-break that re-opens
REVISE-CH2-V1-01" — V3 modified NONE of the four, so the mirror-gate is
non-triggered and the discipline holds.

## Accept Rate

**8/8 = 100% — ACCEPT**

## Verdict

**`G-T-P3-V3-CH2`: ACCEPT.** V3 micro-fold (single-line `-maxdepth 2` drop at
`3F:123`) is structurally isolated from the LAC-1E-14 4-site mirror in 3F
(`:104`, `:125`, `:311`, `:327`); all four sites carry canonical V2-mirrored
wording verbatim at HEAD. Zero V3 edits on 3A/3C-cryst/3D/3E preserves the
5×15 CSS L4 matrix, 7-step onboarding test, 12 L14-HC clauses, no-JSON-
narrowing discipline, and LAC-2F-V5-02 ELEVATED substrate-union strengthening
cohort-wide. Cohort Lock 14 holds; 5-shape `BackendShape` canon at Lock 10
HOLDS; 16-lock count preserved.

## LOCK Trajectory

V1 87.5% (7/8) → V2 100% (11/11) → V3 100% (8/8) — **LOCK extension: 3-cycle**
(V2+V3 consecutive ≥95% + V4-predicted confirming = 3-cycle CH2 LOCK at
predicted cohort §3Z LOCK trigger). Per V2 CH2 §"Cycle Disposition": "V3
confirming pass must hold CH2 ACCEPT at ≥95% to close the cohort §3Z LOCK at
V2+V3" — V3 satisfies at 100%. CH2 2-cycle LOCK ELIGIBLE at V3 close;
3-cycle LOCK CONFIRMED at predicted V4 close per CHALLENGE-CONTEXT.md:27.

## Revise Queue (if any)

**Empty.** ACCEPT clean. The V3 micro-fold did not touch any CH2-binding
surface; all 8 verification checks pass at HEAD. V4 confirming cycle requires
no CH2 surgical edit (CH2 already at 2-cycle LOCK ELIGIBLE post-V3).

---

*End T-P3 V3 CH2 GENERALITY lens — ACCEPT clean; LOCK extension to 3-cycle at predicted V4 close.*
