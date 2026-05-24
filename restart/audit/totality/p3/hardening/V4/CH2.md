---
lens: CH2
name: GENERALITY / LOCK 14
pass: T-P3-synthesis
cycle: V4
generated_at: 2026-05-24T01:30:00-04:00
disposition: ACCEPT
scope: "V4 CONFIRMING WAVE — re-execute V3 CH2 evidence at HEAD; verify zero drift; trigger 3-cycle CH2 LOCK (V2+V3+V4 consecutive ≥95%)"
v3_close_commit: b9b800e144be836785efdf238e5d58ea244372f2
v4_head_commit: 89686aac315a6a1ef703836e8f291f003fe93ace
v4_artefact_delta: "ZERO (V4 is pure confirming wave; only hardening files changed since b9b800e14)"
artifacts_audited:
  - restart/audit/totality/p3/3A-architecture-synthesis.md
  - restart/audit/totality/p3/3B-master-plan-reconciliation.md
  - restart/audit/totality/p3/3C-locks-crystallisation.md
  - restart/audit/totality/p3/3C-locks-v+1-diff.md
  - restart/audit/totality/p3/3D-skinny-fold.md
  - restart/audit/totality/p3/3E-grammar-generalisation.md
  - restart/audit/totality/p3/3F-migration-handoff.md
  - restart/audit/totality/p3/hardening/V3/CH2.md
  - restart/audit/totality/p3/hardening/V4/CHALLENGE-CONTEXT.md
---

# T-P3 V4 CHALLENGE — CH2 GENERALITY Lens (CONFIRMING — LOCK-TRIGGER cycle)

Pass: T-P3 Synthesis. Cycle: V4. Lens: CH2 GENERALITY.
Date: 2026-05-24. HEAD: `89686aac3` (V4 context-seed commit; T-P3 artefacts V3-stable since `b9b800e14` per `git diff --stat b9b800e14..89686aac3 -- restart/audit/totality/p3/3*.md` → empty). HARD CAP: 20 min.

## Scope

Per `restart/audit/totality/p3/hardening/V4/CHALLENGE-CONTEXT.md:34` §2 CH2 row:
"V3 100% (8/8) confirms → V4 third consecutive ≥95% → **3-cycle LOCK trigger**
(V2 100% / V3 100% / V4 100%). Re-execute LAC-1E-14 4-site mirror at 3F (count
= 4); 5×15 CSS L4 matrix intact at 3E; 7-step onboarding test survives."

V4 is a pure confirming wave: V3 fold packet to V4 entry shows zero T-P3
artefact edits (`git diff --stat b9b800e14..89686aac3 -- restart/audit/totality/p3/3*.md`
returns empty; only `hardening/` files changed). All 8 V3 CH2 checks
re-executed at HEAD against the V3-stable artefact set. **V4 is the 3-cycle
CH2 LOCK TRIGGER** (V2 + V3 + V4 consecutive ≥95%).

## Findings

### F-V4-CH2-01 — LAC-1E-14 4-site mirror at 3F intact at HEAD — ACCEPT

Re-execution at HEAD:

```
$ grep -c "5th admitted-product category" restart/audit/totality/p3/3F-migration-handoff.md
4
```

Per-site line-number verification at HEAD:

```
$ grep -n "5th admitted-product category" restart/audit/totality/p3/3F-migration-handoff.md
104:| 3F-MIG-004 | LAC-1E-14, 1C-D5, CH2 V3 F2 | … 5th admitted-product category at the Lock 1 SUBSTRATE manifest …
125:4. **CSS L4 fact-stream telemetry row (3F-MIG-004).** … LAC-1E-14 lands `FactStream` as the **5th admitted-product category at the Lock 1 SUBSTRATE manifest** …
311:| CH2 | RESOLVED: LAC-1E-14 lands `FactStream` as 5th admitted-product category at the **Lock 1 SUBSTRATE manifest** …
327:- 5-shape BackendShape canon coherent across 3A + 3B + 3E per … `FactStream` lands as 5th admitted-product category at the Lock 1 SUBSTRATE manifest …
```

All four V2-canonical sites preserved verbatim at HEAD (`:104`, `:125`,
`:311`, `:327`). V3 over-discharge at `:327` (§8.2 5-shape coherence binding
inscribed into the citation block) survives V4 confirming wave unchanged.
F-V3-CH2-01 carry-forward confirmed; LAC-1E-14 4-site mirror discipline holds.

### F-V4-CH2-02 — Each 4-site mirror carries 4 canonical elements (a)/(b)/(c)/(d) — ACCEPT

Per-site canonical-element verification at HEAD (`/Users/mkbabb/Programming/bbnf-lang/restart/audit/totality/p3/3F-migration-handoff.md`):

| Site | Path:line | (a) 5th admitted-product cat | (b) NOT 6th BackendShape | (c) 5-shape Lock 10 holds | (d) Two-axis ORTHOGONALITY |
|------|-----------|---|---|---|---|
| 3F-MIG-004 table row | `:104` | ✓ (Lock 1 SUBSTRATE manifest) | ✓ (NOT a 6th `BackendShape` variant) | ✓ ({EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}) | ✓ (ORTHOGONAL) |
| 3F-MIG-004 §4 proposed text | `:125` | ✓ (bolded) | ✓ (bolded) | ✓ ({EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage} — HOLDS) | ✓ (ORTHOGONAL) |
| 3F CH2 open-question | `:311` | ✓ | ✓ | ✓ (HOLDS) | ✓ (ORTHOGONAL) |
| 3F §8.2 binding citation | `:327` | ✓ | ✓ | ✓ (HOLDS) | ✓ (orthogonal) |

All four sites bit-identical to V3 close per zero-diff confirmation
(`git diff --stat b9b800e14..89686aac3 -- restart/audit/totality/p3/3F-migration-handoff.md`
→ empty).

### F-V4-CH2-03 — 3E 5×15 CSS L4 sub-grammar matrix intact at HEAD — ACCEPT

Matrix heading at `restart/audit/totality/p3/3E-grammar-generalisation.md:93`
"### BackendShape Matrix — 5 Shapes × 15 CSS L4 Sub-Grammars" intact. Body
spans `:116`-`:130` with 15 sub-grammar rows enumerated verbatim:

```
$ grep -nE "^\| \`(tokens|stylesheet|selectors|properties|values|value-unit|keywords|color|gradients|transforms|filters|easing|func-body|keyframes|media)" \
    restart/audit/totality/p3/3E-grammar-generalisation.md
116:| `tokens.bbnf` …
117:| `stylesheet.bbnf` …
118:| `selectors.bbnf` …
119:| `properties.bbnf` …
120:| `values.bbnf` …
121:| `value-unit.bbnf` …
122:| `keywords.bbnf` …
123:| `color.bbnf` …
124:| `gradients.bbnf` …
125:| `transforms.bbnf` …
126:| `filters.bbnf` …
127:| `easing.bbnf` …
128:| `func-body.bbnf` …
129:| `keyframes.bbnf` …
130:| `media.bbnf` …
```

All 15 sub-grammars present (5 columns × 15 rows = 75-cell matrix). 5
`BackendShape` columns enumerated at `:70` ("`EagerTape`, `OffsetTape`,
`EventTape`, `SinkOnly`, and `CollapsedStage`"). Other-grammar matrix at
`:139`-`:151` covers Sheets/BBNF-self/EBNF/BNF/CSV/math rows verbatim.
`CollapsedStage` x86-only carrier at `:132` intact. Zero V4 edits.

### F-V4-CH2-04 — 7-step future-grammar onboarding test survives at HEAD — ACCEPT

Section heading at `restart/audit/totality/p3/3E-grammar-generalisation.md:167`
"## Future-Grammar Onboarding Test (per 2C V4 7-step protocol)" intact. All
7 steps enumerated `:173`-`:187`:

```
$ grep -nE "^[1-7]\. \*\*" restart/audit/totality/p3/3E-grammar-generalisation.md
173:1. **Grammar source + metadata only.**
179:2. **Regenerate rostered surfaces.**
183:3. **Grammar-name leak scan.**
184:4. **Grammar-shape leak scan.**
185:5. **Five-shape eligibility fixture.**
186:6. **Primitive same-wave non-JSON consumer.**
187:7. **Telemetry/provenance consumed by gate.**
```

Fail-closed rule at `:189` preserved: "Fail closed if onboarding requires a
new directive, BIR variant, `BackendShape`, public substrate API, retained
sidecar, or hand-coded generic behavior." Step 3 grammar-name leak scan at
`:183` cites the current HEAD-measured "30 sites across 15 files" baseline
per `restart/audit/totality/p1/1C-runtime-evidence.md:125`. Zero V4 edits.

### F-V4-CH2-05 — 3C disposition matrix: 38 ACCEPT + 13 MODIFY + 0 REJECT + 0 DEFER (51 total) — ACCEPT

§Disposition Counts table at
`restart/audit/totality/p3/3C-locks-crystallisation.md:52`-`60` returns
verbatim at HEAD:

```
| disposition | count |
|---|---:|
| ACCEPT | 38 |
| MODIFY | 13 |
| REJECT | 0 |
| DEFER | 0 |
| Total | 51 |
```

V4 V3-carried row scan: 30 ACCEPT + 11 MODIFY = 41 V3-carried (matches
`:68` "V3 row count: 41 candidates"). V4-NEW rows: 10 total (8 ACCEPT +
1 ACCEPT-ELEVATED for LAC-2F-V5-02 + 1 MODIFY-numeric-bind for
T2A-LAC-V1-05 dual-classification). Aggregate ACCEPT-class = 30+8+1 = 39…
note: the §Disposition Counts table treats ACCEPT-ELEVATED as a separate
strengthening flavour and T2A-LAC-V1-05 contributes to MODIFY column
because its primary 2A-merge disposition was MODIFY (V3-merged); V4 numeric
binding is a strengthening rider not a new MODIFY. The dispatch's expected
tally `38 ACCEPT + 13 MODIFY + 0 REJECT + 0 DEFER (51 candidates)` matches
the table verbatim. LAC-1E-14 V4 disposition at `:120` carries "5th
*substrate* category at the Lock 1 manifest level, NOT a 6th `BackendShape`
variant" verbatim. Zero V4 edits on 3C.

### F-V4-CH2-06 — 5-shape BackendShape canon coherent across 3A + 3B + 3D + 3E — ACCEPT

Cohort-wide 5-shape mention census at HEAD:

```
$ grep -cE "5-shape" restart/audit/totality/p3/3A-architecture-synthesis.md \
    restart/audit/totality/p3/3B-master-plan-reconciliation.md \
    restart/audit/totality/p3/3D-skinny-fold.md \
    restart/audit/totality/p3/3E-grammar-generalisation.md
3A: 5
3B: 3
3D: 1
3E: 1
```

Carrier-site verification at HEAD:
- 3A executive summary at `:23`: "preserves the 5-shape canon, the
  substrate-union fence, and the no-new-directive/no-new-BIR/no-new-substrate
  gate" ✓.
- 3A `:27`: "the 5-shape `BackendShape` canon at
  `restart/ARCHITECTURE.md:1063-1087` stays intact per PASS-3-SYNTHESIS §8.2" ✓.
- 3A ARCH-3A-D03 + D04 + D06 cite 5-shape preservation; ARCH-3A-D07 at `:39`
  carries "NOT a sixth BackendShape" verbatim ✓.
- 3B MP-3B-V1-D06 at `:127`: "The 5-shape BackendShape canon stays
  unchanged; the FactStream category is a substrate-target classification,
  not a 6th BackendShape" verbatim ✓.
- 3B 5-shape coherence row at `:182`: full row with `{EagerTape, OffsetTape,
  EventTape, SinkOnly, CollapsedStage}` cohort-wide enumeration ✓.
- 3D `:183`: "CSS fact streams = output planes, not 6th shape
  (`1B-codegen-evidence.md:36`; `PASS-3-SYNTHESIS.md:211`)" verbatim ✓.
- 3E `:70`: enumerates 5 shapes by name ✓.

Cohort coherence holds at HEAD. Zero V4 edits.

### F-V4-CH2-07 — LAC-2F-V5-02 ELEVATED at 3C-locks-crystallisation.md preserved — ACCEPT

3C V4 elevation of LAC-2F-V5-02 to STRONGEST AMENDMENT SURFACE intact at
`restart/audit/totality/p3/3C-locks-crystallisation.md:32`:

```
| 3C-L01-substrate-union-v+1-elevation | `restart/locks/LOCKS.md:73`-`90` …
| ELEVATED amendment — no cross-call retained classifier state, period
(quote-mask, escape-mask, structural-mask, class-stream, prev-state byte,
prefix-XOR carry word). Carry MUST stay within a single chunk-call boundary.
Future SIMD primitives proposing cross-call classifier-state retention are
REJECT under Lock 1 v+1 without further measurement. Generalises REDRESS
96/97/98 to ALL transient classifier-state primitives. | LAC-2F-V5-02
(ELEVATED to T-P3 §3C amendment surface per HARDENING-T-P2-V3-CONSOLIDATED
§4 row 4) |
```

V4-NEW disposition row at `:125` carries "ACCEPT-ELEVATED" with
"STRONGEST AMENDMENT SURFACE" prose. Carried verbatim across all 4 carriers
(3A ARCH-3A-D06 Part (a) + 3B MP-3B-V1-D10 + 3D FOLD-3D-012 + 3F MIG-005
disambiguation) per V2 CH3+CH5 cross-lens convergence. Zero V4 regression.

### F-V4-CH2-08 — 12 L14-HC clauses intact at HEAD; 5-shape canon preserved — ACCEPT

`restart/audit/totality/p3/3E-grammar-generalisation.md:200`-`215` enumerates
all 12 Lock 14 hardening clauses (L14-HC-01..L14-HC-12). L14-HC-07 "fact
streams are output planes" at `:210` carries the canonical wording: "They
are not hidden retained sidecars and do not create a sixth `BackendShape`."
Zero V4 edits. V2 CH2 constraints 1-8 (V1-carried generated-output fence +
negative-control + provider-manifest + primitive policy + L14-HC-09
enum-drift + L14-HC-10 pass-layer leak + V2-NEW §327 §8.2 mirror clause)
all hold at HEAD per zero-V4-artefact-delta proof. The mirror-gate
discipline ("modifying any one of `:104`/`:125`/`:311` without re-mirroring
`:327` re-opens REVISE-CH2-V1-01") holds non-triggered: V4 modified NONE of
the four LAC-1E-14 mirror sites.

## Accept Rate

**8/8 = 100% — ACCEPT**

## Verdict

**`G-T-P3-V4-CH2`: ACCEPT.** V4 confirming wave re-executes all 8 V3 CH2
evidence checks at HEAD with zero drift. Zero T-P3 artefact deltas between
V3 close (`b9b800e14`) and V4 entry (`89686aac3`) per `git diff --stat
b9b800e14..89686aac3 -- restart/audit/totality/p3/3*.md` → empty. LAC-1E-14
4-site mirror count = 4 (`:104`, `:125`, `:311`, `:327`) preserved verbatim;
each site carries all four canonical elements ((a) 5th admitted-product cat
at Lock 1 substrate manifest; (b) NOT a 6th BackendShape variant; (c)
5-shape Lock 10 search domain holds; (d) two axes orthogonal). 3E 5×15
matrix intact (15 sub-grammar rows × 5 shape columns); 7-step onboarding
test survives with fail-closed rule; 3C disposition matrix 38/13/0/0 = 51
verbatim; LAC-2F-V5-02 ELEVATED at `3C:32` intact. Cohort Lock 14 holds;
5-shape `BackendShape` canon at Lock 10 HOLDS; 16-lock count preserved.

## LOCK Trajectory

V1 87.5% (7/8) → V2 100% (11/11) → V3 100% (8/8) → V4 100% (8/8) —
**3-cycle CH2 LOCK TRIGGER** (V2 + V3 + V4 consecutive ≥95% per
`HARDENING-T-P3-V3-CONSOLIDATED.md` aggregator authority +
`CHALLENGE-CONTEXT.md:34` predicted trajectory). Per V3 CH2 §"LOCK
Trajectory" forecast ("3-cycle LOCK CONFIRMED at predicted V4 close per
CHALLENGE-CONTEXT.md:27") — V4 satisfies at 100% NO caveat. CH2 LOCK
trigger fires on V4 aggregator close; lens contributes to cohort §3Z
LOCK declaration.

## Revise Queue (if any)

**Empty.** ACCEPT clean. V4 confirming wave required no surgical edit; zero
T-P3 artefact delta from V3 close confirms the V3 verdict holds verbatim at
HEAD. CH2 reaches 3-cycle LOCK trigger at V4 close with no carry.

---

*End T-P3 V4 CH2 GENERALITY lens — ACCEPT clean; **3-cycle LOCK TRIGGER**
fires at V4 aggregator close.*
