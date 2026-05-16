# SK-V7 Restructure — R6 Cross-Cutting Synthesis

Date: 2026-05-16.
Agent: R6 (cross-cutting cohesion / dead-link / duplicate-content audit).
Scope: entire `restart/` tree. Adjacent R1-R5 outputs unread per dispatch.
Inputs: tree walk of 232 `*.md` files; 5,431 cross-references extracted; 247
unique target paths classified; 73 dead-link instances identified.

---

## §1 — Quantitative summary

| Metric | Value |
|---|---:|
| Markdown files in restart/ | 232 |
| Total cross-references containing `restart/` | 5,431 |
| Unique referenced paths | 247 |
| LIVE references | 174 |
| DEAD references (target missing) | 73 |
| Top-level docs (README/ARCHITECTURE/HANDOFF/MASTER-PLAN/MIGRATION) | 5 |
| Skinny canonical surfaces (BENCH/COMPILER/HARDENING/INDEX/SUBSTRATE/WORKSPACE) | 6 |
| skinny/audit/ master synthesis docs | 14 |
| skinny/audit/ cohort directories | 4 (SK-V5/6/7 + V9.5-PSI-EXCAVATION) |
| prompts/ files | 9 |
| Locks file | 1 (`LOCKS.md`; carries Locks 1-16) |
| Legacy `restart/audit/{pass-1,pass-2,pass-3,hardening}/` files | 86 |
| Legacy `restart/research/` files | 22 |
| `inheritance/` files | 1 |

---

## §2 — Dead-link audit (73 targets)

73 referenced paths do not resolve. Classification:

### §2.1 — Class I: Retired files referenced by retired files (low-priority; archive-relative)

The bulk: 9 of the 73 dead links originate from `prompts/PASS-1-SUBSTRATE.md`,
`prompts/PASS-2-CODEGEN.md`, `prompts/PASS-3-RUNTIME.md`, `prompts/SYNTHESIS.md`
themselves. `restart/README.md:435` declares these four prompt files "retired
at Phase 8.0" — but the README references them and no shim or redirect exists.
Per README §12 the retired surfaces should be deleted, not referenced as
ghosts.

| Source | Dead target | Count |
|---|---|---:|
| `restart/skinny/tranches/sk-v6/DISPATCH-PROMPT.md` | 11 SK-V1/SK-V2 hardening docs | ~11 |
| `restart/skinny/INDEX.md`, `BENCH.md`, `HARDENING.md`, `WORKSPACE.md` | `skinny/REDRESS.md`, `RESULTS.md`, `MIGRATION.md` (wrong prefix; live at `/Users/mkbabb/Programming/bbnf-lang/skinny/`, not `restart/skinny/`) | ~3 |
| `restart/skinny/tranches/sk-v7/research/skv7-B5-mesh-typed.md:485` | `restart/HANDOFF-SK-V6.md` (wrong path; live target is `restart/skinny/tranches/sk-v6/HANDOFF.md`) | 1 |

### §2.2 — Class II: Live-prefix path errors

`restart/skinny/REDRESS.md`, `restart/skinny/RESULTS.md`, and
`restart/skinny/MIGRATION.md` are dead under `restart/skinny/` but live at
`/Users/mkbabb/Programming/bbnf-lang/skinny/` (workspace root). The user's
mental model treats these as bridge-doc canonicals between corpus and impl;
the wrong-prefix reference is a recurring slip.

| Wrong path (referenced) | Live path | Files affected |
|---|---|---:|
| `restart/skinny/REDRESS.md` | `skinny/REDRESS.md` (workspace) | ≥6 |
| `restart/skinny/RESULTS.md` | `skinny/RESULTS.md` (workspace) | ≥6 |
| `restart/skinny/MIGRATION.md` | `skinny/MIGRATION.md` does not exist anywhere | ≥4 |

`restart/skinny/MIGRATION.md` is the most problematic — referenced from
`SK-V7-COHORT/skv7-B3-lock14-sequence.md` but the file has never existed
under either prefix. Either author the file or strike the references.

### §2.3 — Class III: Retired SK-V1..SK-V4 hardening + grand-synthesis docs

| Dead target | First referenced by | Likely disposition |
|---|---|---|
| `restart/skinny/tranches/HARDENING-{BENCH,COMPILER,CONSOLIDATED,INDEX,SUBSTRATE,WORKSPACE}-SK-V{1,2}.md` (12 files) | `IMPLEMENTATION-AGENT-PROMPT-SK-V6.md` (verification ritual) | Strike the verification ritual lines; these files are intentionally retired per SK-V6 nuke |
| `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` | `V9.5-PSI-EXCAVATION/`, SK-V5 cohort | Move to `archive/` (or delete; the V9.5 dig is the surviving forensic) |
| `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md` | SK-V5/V6 cohort archaeology | Same |
| `restart/skinny/tranches/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` | V9.5 + SK-V5 + SK-V7 C2 | Same |
| `restart/skinny/tranches/ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md` | SK-V5 + SK-V6 | Same |
| `restart/skinny/tranches/LAZY-TAPE-DESIGN.md` | `IMPLEMENTATION-AGENT-PROMPT-SK-V6.md` | Strike; design absorbed into V9.5 + locks |
| `restart/skinny/tranches/HARDENING-CONSOLIDATED-SK-V{1,2}.md` | `IMPLEMENTATION-AGENT-PROMPT-SK-V6.md` | Strike retired-doc citations |
| `restart/skinny/tranches/HARDENING-INDEX-SK-V{1,2}.md` | Same | Same |
| `restart/skinny/tranches/WAVE-1-2-COHORT-DIGEST.md` | SK-V5/V6 cohorts | Move to `archive/` if surviving evidence; else strike |

### §2.4 — Class IV: Pre-Phase-8 prompt-suite ghosts

| Dead target | Referenced by | Action |
|---|---|---|
| `restart/prompts/PASS-1-SUBSTRATE.md` | README.md:435, 16 hardening + pass-* files | Strike; per README §12 these retired at Phase 8.0 |
| `restart/prompts/PASS-2-CODEGEN.md` | Same | Same |
| `restart/prompts/PASS-3-RUNTIME.md` | Same | Same |
| `restart/prompts/SYNTHESIS.md` | Same | Same |

### §2.5 — Class V: `restart/specs/pass-1/*` (25 dead refs)

`restart/specs/pass-1/` directory does not exist. References come from
`restart/templates/cookbook-page.md`, `restart/templates/declaration-crate-review.md`,
and 23 referenced spec files. The `restart/templates/` directory does not
exist either. Either reconstitute templates + specs or strike all references.

### §2.6 — Class VI: Wave-2-classification + phase-7.2 ghosts

| Dead target | Referenced by | Action |
|---|---|---|
| `restart/audit/pass-1-substrate/wave-2-classification.md` | `HARDENING-PASS-1-V3.md` | Strike (V3 hardening is archival) |
| `restart/audit/pass-2-codegen/phase-7.2-classification.md` | `HARDENING-PASS-2-V7.md` | Strike |
| `restart/audit/pass-2-codegen/wave-2-classification-amendment.md` | `HARDENING-PASS-2-V3.md` | Same |
| `restart/audit/pass-2-codegen/wave-4.1-classification-amendment.md` | `HARDENING-PASS-2-V4.md` | Same |
| `restart/audit/pass-3-runtime/phase-7.2-classification.md` | `HARDENING-PASS-3-V7.md` | Same |
| `restart/research/PHASE-7.2-SYNTHESIS-CLASSIFICATION.md` | `HARDENING-MASTER-PLAN-V7.md` | Same |
| `restart/research/escalation-summary.md` | RESEARCH-FOLD-ORCHESTRATOR + V6 consolidated | Strike or revive |
| `restart/research/parse-that-spec.md` | `V1-FOLD-CANDIDATES.md` | Strike (parse-that is now extant code, not spec) |

---

## §3 — Duplicate-content audit

Five sections appear in ≥3 files and constitute load-bearing duplicates:

### §3.1 — `BackendShape` 5-shape enum

The enum body (`EagerTape | OffsetTape | EventTape | SinkOnly | CollapsedStage`)
plus the 8-step `derive_backend_shape` algorithm appears in **40 files**.

**Canonical**: `restart/ARCHITECTURE.md` §7.3 (per Lock 10's reference clause).
**Should reference, not duplicate**:
- `restart/HANDOFF.md` §5 — currently inlines the full enum + 8-step algo + per-grammar matrix (lines 153-198). Should cite ARCHITECTURE.md §7.3 + carry the per-grammar matrix only.
- `restart/MASTER-PLAN.md` — currently re-inlines the algorithm.
- `restart/skinny/COMPILER.md`, `restart/skinny/SUBSTRATE.md`, `restart/skinny/INDEX.md` — each carries partial restatements.
- `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md` — same.

**Recommendation**: ARCHITECTURE.md §7.3 stays canonical; all other surfaces
cite `restart/ARCHITECTURE.md §7.3` with a one-line summary ("5 shapes:
EagerTape | OffsetTape | EventTape | SinkOnly | CollapsedStage; derivation
8-step per ARCHITECTURE.md §7.3"). The per-grammar matrix (JSON / CSS L4 /
BBNF-self / Sheets) lives at HANDOFF.md §5 + nowhere else.

### §3.2 — Locks 15-16 verbiage

The full Lock 15 prose ("LTO + codegen-units=1 + force-inline + ≤20 KiB hot
function ceiling") and Lock 16 prose (allowlist of NEON/AVX-512 primitives
including the Wave 1 5-pack + 3-pack) appears in:

- `restart/locks/LOCKS.md` (canonical).
- `restart/HANDOFF.md` §3 (reading order #2 inlines).
- `restart/README.md` §11 ("Locks Carried Forward" table).
- `restart/ARCHITECTURE.md` §13.1 (admissible-SIMD-primitives table).
- `restart/MASTER-PLAN.md` §4.

**Recommendation**: `locks/14-LOCKS.md` is canonical (already cited as
such). README.md §11 keeps the table-row summary; HANDOFF.md §3 carries the
one-line summary + cite. ARCHITECTURE.md §13.1 carries only the V1 wave
allocation, not the citation list. MASTER-PLAN.md §4 cites and excerpts only
the Wave-bound row.

### §3.3 — Reading order

The "cold-start reading order" appears in:
- `restart/README.md` §12 (lines 426-435).
- `restart/HANDOFF.md` §1 (lines 21-32).
- `restart/prompts/README.md` lines 3-9 (prompt-suite reading order).
- `restart/prompts/ORCHESTRATOR.md` §1 (orchestrator-agent reading order).

Each has a different ordering and a different scope. The README and HANDOFF
versions diverge: README §12 lists 6 items, HANDOFF §1 lists 10 items.
ORCHESTRATOR's list is broader but cites retired `HARDENING-CONSOLIDATED-V7.1.md`
(extant but pre-skinny-track) as a load-bearing entry.

**Recommendation**: README §12 stays canonical for "the cold-start agent";
HANDOFF.md §1 cites "see README §12 for cold-start order" and lists only the
SK-V7-track-specific reading sequence; ORCHESTRATOR.md §1 is the orchestrator-
specific subset.

### §3.4 — M5 Max cross-parser landscape table

The 17-row corpus × parser throughput table appears in:
- `restart/HANDOFF.md` §4 (lines 112-130).
- `restart/skinny/tranches/sk-v6/HANDOFF.md`.
- `restart/skinny/tranches/sk-v7/HANDOFF.md`.
- `restart/skinny/tranches/sk-v6/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v7/research/skv7-A2-sota-strict-beat.md`.

Each version is dated; per-SK numbers may differ. The most-current is the
post-V6 RESULTS.md numbers (HANDOFF-SK-V7 + GRAND-SYNTHESIS-SK-V7).

**Recommendation**: `skinny/RESULTS.md` (workspace) is the only authoritative
numeric source; each restart/ doc carries a snapshot + per-snapshot date.
HANDOFF.md should snapshot the most-recent SK-V{N} table; per-SK HANDOFF
documents may inline the SK-aligned snapshot.

### §3.5 — TapeKind variant naming

Pre-rename names (`Object`, `Array`, `Pair`, `String`, `Number`, `Bool`, `Null`,
`Member`, `Element`) appear in:
- `restart/audit/pass-2-codegen/agent-1-backend-ir-architect.md` (Phase-2 IR
  Architect output; archival).
- `restart/audit/pass-2-codegen/agent-2-rust-lowerer-architect.md`.
- `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md`.
- `restart/corpora/CENSUS.md`.
- `restart/skinny/tranches/sk-v6/research/skv6-A5-general-grammar-abstraction.md`.
- `restart/skinny/tranches/sk-v6/research/skv6-C6-generality-costfacts.md`.
- `restart/skinny/tranches/sk-v6/research/skv6-R3d-direct-generality.md`.

Post-rename names (`Container`, `Bucket`, `KeyValuePair`, `StringValue`,
`NumberValue`, `BoolValue`, `NullValue`, `Member`, `Element`) appear in:
- `restart/skinny/tranches/sk-v7/SPEC.md` §3 (the SK-V7 W1 plan
  authoritative).
- `restart/skinny/tranches/sk-v7/HANDOFF.md`.
- `restart/skinny/tranches/sk-v7/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v7/research/skv7-A5-lock-audit.md`.
- `restart/skinny/tranches/sk-v7/research/skv7-B3-lock14-sequence.md`.

The two are not yet reconciled because the rename is a SK-V7 W1 admit-pending.
**No corrective action until W1 lands.** The MIGRATION.md should carry the
rename table at admit time.

---

## §4 — Vocabulary consistency audit

### §4.1 — Strictness vocabulary

The IMPLEMENTATION-PACKET-SK-V7.md §0.3 schema fixes the canonical:
`Strictness ∈ {strict, permissive, deferred}` + `parse_utf8 ∈ {scan-boundary,
view-boundary, none}` + `escape_complete ∈ {yes, no}` + `flaw_probe = string`.

Drift detected:
- HANDOFF.md uses "utf8_lossy" + "strict" + "permissive" + "deferred".
- skinny/BENCH.md uses unprefixed "strict" / "permissive" / "lossy".
- IMPLEMENTATION-PACKET-SK-V7 uses "strict" / "lossy" / "permissive" + treats
  asmjson-SWAR + RapidJSON as "flaw probe".

**Action**: BENCH.md and HANDOFF.md should adopt the PASS-ALPHA.md §4.3 schema
verbatim. The 24-column schema lives in PASS-ALPHA.md and is duplicated in
IMPLEMENTATION-PACKET-SK-V7.md §0.3.

### §4.2 — Lock-count consistency

| File | "16 locks" claim | Actual content |
|---|---|---|
| `locks/14-LOCKS.md` | filename says 14; content carries Locks 1-16 | 16 locks present |
| `README.md` §11 | "16 locks" | matches content |
| `HANDOFF.md` §1 | "16 architectural locks post-2026-05-12" | matches |
| `prompts/README.md` | "Lock 1-16 (Pass Omega proposes amendments)" | matches |

The filename `LOCKS.md` is misleading: 14 was the legacy count before
Locks 15 + 16 (2026-05-12 build-profile + SIMD admissibility) landed.
**Recommendation**: rename to `restart/locks/LOCKS.md` (drop the count;
PASS-OMEGA can extend).

### §4.3 — Wave-letter conventions

Three conventions coexist:
- `H.W{N}` (`H.W0`-`H.W5`) — H tranche waves in MASTER-PLAN.md.
- `H.W{N}.LOCK{M}` — sub-wave anchors (MASTER-PLAN.md §H.W4.LOCK14).
- `SK-V{N}-W{N}` — skinny-iteration waves in IMPLEMENTATION-PACKET-SK-V{N}.md
  + COHORT/.
- `W{N}` — bare in `prompts/SKINNY-PASSES.md` + within COHORT artefacts.

**Recommendation**: keep H.W{N} for V1 totality tranche, SK-V{N}-W{N} for
skinny waves. The bare `W{N}` form drops in cohort filenames because COHORT
context implies the iteration.

### §4.4 — Master-doc naming

Inconsistent: `GRAND-SYNTHESIS-SK-V{N}.md` (V5/V6/V7), `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md`
(suffixed variant V6 + variant tag), `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md`
(dead; legacy V3 tag order).

**Recommendation**: stabilise at `GRAND-SYNTHESIS-SK-V{N}.md` + optional
`-{TAG}.md` suffix at author discretion; archive deprecated variants under
`archive/SK-V{N}/`.

### §4.5 — Cohort-dir naming

Three conventions coexist:
- `SK-V{N}-COHORT/` (V5/V6/V7).
- `V9.5-PSI-EXCAVATION/` (numeric prefix, no SK-V tag).
- (proposed in PASS-OMEGA.md §7) `restart/totality/astral/V{V}/`.

**Recommendation**: rename V9.5-PSI-EXCAVATION/ → `SK-V3.5-PSI-EXCAVATION/`
(it is fundamentally SK-V3 era retrospective) and move under
`skinny/audit/archive/`.

---

## §5 — Per-iteration archive consistency

SK-V3 + SK-V4 cohorts are **deleted in place**; only references survive. The
remaining SK-V3 / SK-V4 forensics live at `V9.5-PSI-EXCAVATION/`. SK-V5, SK-V6,
SK-V7 cohorts are intact at `skinny/audit/SK-V{5,6,7}-COHORT/`.

Asymmetries:
- SK-V5 has 15 cohort reports + 4 master docs (GRAND-SYNTHESIS-SK-V5,
  IMPLEMENTATION-PACKET-SK-V5, NUKE-PLAN-SK-V5, HANDOFF-SK-V5).
- SK-V6 has 32 cohort reports (A1-A6 + B1-B6 + C1-C6 + R1-R6 with sub-suffixes
  + schema A-C) + 3 master docs (GRAND-SYNTHESIS-SK-V6 + variant + HANDOFF +
  IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY).
- SK-V7 has 18 cohort reports (A1-A6 + B1-B6 + C1-C6) + 3 master docs +
  Class `IMPLEMENTATION-AGENT-PROMPT-SK-V6.md` (the dispatch surface).

**Action**: per the PASS-ALPHA.md §6 output structure, the canonical pattern
is:
```
restart/skinny/tranches/SK-V{N}-COHORT/{alpha,alpha-hardening}/
restart/skinny/tranches/GRAND-SYNTHESIS-SK-V{N}.md
restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V{N}.md
restart/skinny/tranches/HANDOFF-SK-V{N}.md
```
The existing structure honours this for SK-V7. SK-V5 + SK-V6 should be moved
to `archive/SK-V{N}/` (carrying master docs + cohort dir together).

---

## §6 — V7-introduced files audit

### §6.1 — `restart/prompts/{README, PASS-ALPHA, PASS-OMEGA, SKINNY-PASSES}.md`

Status: live at git head; written.

Reference outward:
- README.md cites `ORCHESTRATOR.md`, `PASS-ALPHA.md`, `PASS-OMEGA.md`,
  `TOTALITY-PASS-{1,2,3}-{RESEARCH,PROFILE,HARDENING}.md`, `SKINNY-PASSES.md`.
  **`TOTALITY-PASS-{1,2,3}` files do not exist.** Dead refs from README:9,
  127-131.
- PASS-ALPHA.md cites `ORCHESTRATOR.md` §5 + §iteration-governance.
  `iteration-governance` section does not exist in ORCHESTRATOR.md. Dead
  section reference. The ORCHESTRATOR.md §9 "Hardening cycle hard cap" is
  the nearest; the inferred section name is "§iteration-governance".
- PASS-OMEGA.md cites `ORCHESTRATOR.md` §5 ("CH1-CH6 per ORCHESTRATOR.md §5").
  §5 exists ("Hardening-cycle naming canon") but does NOT define CH1-CH6.
  The §6 "Lens registry" defines A-K lenses, not CH1-CH6. Cross-document
  drift: PASS-ALPHA + PASS-OMEGA invent CH1-CH6 as a six-lens shorthand;
  ORCHESTRATOR uses A-K letter scheme.
- SKINNY-PASSES.md cites `ORCHESTRATOR.md` §iteration-governance + §5.

Reference inward (from existing files into the new V7 prompts):
- IMPLEMENTATION-PACKET-SK-V7.md cites `PASS-ALPHA.md` §4.3, `SKINNY-PASSES.md`,
  `ORCHESTRATOR.md`. All extant.
- HANDOFF-SK-V7.md cites `PASS-ALPHA.md` + `PASS-OMEGA.md`.
- GRAND-SYNTHESIS-SK-V7.md cites `PASS-ALPHA.md` §2.
- No incoming citation from `restart/README.md`, `restart/HANDOFF.md`,
  `restart/MASTER-PLAN.md`, or `restart/locks/LOCKS.md`.

**Recommendation**: README.md §12 + HANDOFF.md §1 must add anchors to the V7
prompt-suite. README §12 should be amended to list:
```
Cold-start reading order (V7+):
1. restart/HANDOFF.md
2. restart/prompts/README.md
3. restart/prompts/ORCHESTRATOR.md
4. restart/prompts/pass-contracts/PASS-ALPHA.md or PASS-OMEGA.md (per current phase)
5. restart/README.md
6. restart/locks/LOCKS.md
7. restart/skinny/tranches/HANDOFF-SK-V{N}.md (most recent)
```

### §6.2 — SK-V7 master docs

Status: live; references-out audited above.

Inconsistencies with existing master docs:
- HANDOFF-SK-V7.md (206 LOC) vs HANDOFF-SK-V6.md (~? LOC) vs HANDOFF-SK-V5.md.
  The shape is similar but SK-V7 introduces the §0 Close Condition framing
  via PASS-ALPHA §4. The V5/V6 HANDOFFs predate the framing and read
  differently.
- IMPLEMENTATION-PACKET-SK-V7.md (437 LOC) — the longest packet file. It
  carries §0 goalset + §1 non-negotiables + §2-§9 per-wave specs.
- GRAND-SYNTHESIS-SK-V7.md (304 LOC) — folds SK-V6 + SK-V7 cohort findings.

The V7 trio is the first iteration of the formal Pass Alpha contract. The
V5/V6 docs are pre-formalism artefacts; they should be archived under
`restart/skinny/tranches/archive/` per the per-iteration archive recommendation.

---

## §7 — Restructure proposal (synthesised from first principles)

### §7.1 — Target tree shape

```
restart/
├── README.md                              ← gestalt; cite-only for locks + shapes
├── ARCHITECTURE.md                        ← V1 spec; canonical for BackendShape §7.3
├── HANDOFF.md                             ← top-level state; current SK-V{N} anchor
├── MASTER-PLAN.md                         ← H tranche
├── MIGRATION.md                           ← renames + abrogates + (NEW) TapeKind rename map post-W1
├── locks/
│   └── LOCKS.md                           ← rename from 14-LOCKS.md
├── prompts/
│   ├── README.md                          ← framework gestalt
│   ├── ORCHESTRATOR.md                    ← main dispatch
│   ├── PASS-ALPHA.md                      ← skinny astral
│   ├── PASS-OMEGA.md                      ← totality astral
│   ├── SKINNY-PASSES.md                   ← per-iteration triumvirate contract
│   ├── pass-contracts/                    ← NEW subdir; per-pass contracts
│   │   ├── TOTALITY-PASS-1-RESEARCH.md    ← NEW; referenced from README.md but missing
│   │   ├── TOTALITY-PASS-2-PROFILE.md     ← NEW
│   │   └── TOTALITY-PASS-3-HARDENING.md   ← NEW
│   └── sub-orchestrators/                 ← NEW subdir
│       ├── HARDENING.md                   ← from prompts/HARDENING.md
│       ├── HARDENING-ORCHESTRATOR.md      ← from prompts/HARDENING-ORCHESTRATOR.md
│       ├── RESEARCH-FOLD-ORCHESTRATOR.md  ← move
│       └── AMENDMENT-DISPATCH.md          ← move
├── totality/                              ← NEW; totality-track artefacts (PASS-OMEGA §7)
│   ├── pass-1-research/V{V}/
│   ├── pass-2-profile/V{V}/
│   ├── pass-3-hardening/V{V}/
│   └── astral/V{V}/
├── skinny/
│   ├── BENCH.md
│   ├── COMPILER.md
│   ├── INDEX.md
│   ├── SUBSTRATE.md
│   ├── WORKSPACE.md
│   ├── HARDENING.md
│   └── audit/
│       ├── HANDOFF-SK-V{current}.md       ← current iteration
│       ├── IMPLEMENTATION-PACKET-SK-V{current}.md
│       ├── GRAND-SYNTHESIS-SK-V{current}.md
│       ├── SOTA-BEAT-DESIGN.md            ← keep; cross-iteration design
│       ├── SK-V{current}-COHORT/
│       │   ├── alpha/                     ← α-A through α-F
│       │   ├── alpha-hardening/V{V}/      ← CH1-CH6 + CONSOLIDATED
│       │   └── skv{N}-{wave}{agent}-{topic}.md
│       └── archive/
│           ├── SK-V5/                     ← move SK-V5-COHORT/ + master docs
│           ├── SK-V6/                     ← move SK-V6-COHORT/ + master docs
│           └── SK-V3.5-PSI-EXCAVATION/    ← rename + move V9.5-PSI-EXCAVATION/
├── audit/                                 ← legacy hardening + pass-*; merge with totality/
│   └── archive/                           ← NEW; sweep all pre-Phase-8 artefacts
├── research/                              ← legacy; merge selectively into corpora/
│   └── archive/                           ← NEW
├── corpora/                               ← keep; durable
└── inheritance/                           ← archive; legacy BA-BD pointer
```

### §7.2 — Five most-consequential moves

| Rank | Move | Source | Target | Rationale |
|---|---|---|---|---|
| 1 | Rename + extend locks file | `restart/locks/LOCKS.md` | `restart/locks/LOCKS.md` | filename misleads (carries 16 locks); PASS-OMEGA can extend; remove count-coupling |
| 2 | Archive SK-V5 + SK-V6 cohorts + master docs | `restart/skinny/tranches/{SK-V5-COHORT,SK-V6-COHORT,GRAND-SYNTHESIS-SK-V5.md,HANDOFF-SK-V5.md,NUKE-PLAN-SK-V5.md,IMPLEMENTATION-PACKET-SK-V5.md,IMPLEMENTATION-AGENT-PROMPT-SK-V5.md,GRAND-SYNTHESIS-SK-V6.md,GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md,HANDOFF-SK-V6.md,IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md,IMPLEMENTATION-AGENT-PROMPT-SK-V6.md}` | `restart/skinny/tranches/archive/{SK-V5/,SK-V6/}` | SK-V7 is current; per-iteration archive discipline (§5) requires asymmetry resolution |
| 3 | Author missing TOTALITY-PASS files | (none) | `restart/prompts/pass-contracts/TOTALITY-PASS-{1,2,3}-{RESEARCH,PROFILE,HARDENING}.md` | README.md:9 + ORCHESTRATOR cite them; currently dead refs |
| 4 | Strike pre-Phase-8 prompt ghosts from README + HANDOFF | `restart/README.md:435`, `restart/HANDOFF.md` various | edit-in-place | 9+ dead refs to `PASS-1-SUBSTRATE.md` / `PASS-2-CODEGEN.md` / `PASS-3-RUNTIME.md` / `SYNTHESIS.md`; per README §12 these retired at Phase 8.0 |
| 5 | Move V9.5-PSI-EXCAVATION + rename | `restart/skinny/tranches/sk-v3.5/research/` | `restart/skinny/tranches/archive/SK-V3.5-PSI-EXCAVATION/` | establishes archive/ subdir; consistent SK-V{N} naming; signal that the dig is historical not current |

### §7.3 — Concrete CRUD list

**DELETE** (12 entries; safe — content survives in audit/):

```
restart/audit/hardening/HARDENING-MASTER-PLAN-V{2,3,4,5,7,8,8.1,9,9.1,9.2}.md   ← per-cycle plans; retain only V7.1 + V9.2 latest
restart/audit/hardening/HARDENING-PASS-{1,2,3}-V{2,3,4,5,6,7,8,8.1,9,9.1,9.2}.md ← pre-skinny waves; archive
restart/audit/hardening/REVIEW-{A,B,C,D}-*.md                                     ← Phase-8 reviews; archive
restart/audit/pass-{1-substrate,2-codegen,3-runtime}/agent-*.md                  ← per-agent Phase-1/2/3 outputs; archive
```
All four classes can move to `restart/audit/archive/`.

**MOVE** (per §7.1 tree):

| From | To | Note |
|---|---|---|
| `restart/skinny/tranches/sk-v5/research/` | `restart/skinny/tranches/archive/SK-V5/COHORT/` | +12 files |
| `restart/skinny/tranches/sk-v6/research/` | `restart/skinny/tranches/archive/SK-V6/COHORT/` | +32 files |
| `restart/skinny/tranches/sk-v3.5/research/` | `restart/skinny/tranches/archive/SK-V3.5-PSI-EXCAVATION/` | rename + move |
| `restart/skinny/tranches/sk-v5/SYNTHESIS.md` + master peers | `restart/skinny/tranches/archive/SK-V5/` | with cohort dir |
| `restart/skinny/tranches/GRAND-SYNTHESIS-SK-V6*.md` + peers | `restart/skinny/tranches/archive/SK-V6/` | with cohort dir |
| `restart/prompts/audit-specs/HARDENING-LENS-SET.md` + `HARDENING-ORCHESTRATOR.md` + `RESEARCH-FOLD-ORCHESTRATOR.md` + `AMENDMENT-DISPATCH.md` | `restart/prompts/sub-orchestrators/` | encapsulate |
| `restart/locks/LOCKS.md` | `restart/locks/LOCKS.md` | rename |

**RENAME** (3 entries):

| Old | New | Rationale |
|---|---|---|
| `restart/locks/LOCKS.md` | `restart/locks/LOCKS.md` | count-decoupled |
| `restart/skinny/tranches/sk-v3.5/research/` | `restart/skinny/tranches/archive/SK-V3.5-PSI-EXCAVATION/` | SK-V{N} convention |
| `restart/skinny/tranches/sk-v6/SPEC.md` | `restart/skinny/tranches/archive/SK-V6/IMPLEMENTATION-PACKET-SK-V6.md` | drop tag suffix; archive |

**UPDATE** (8 surfaces):

| File | Update |
|---|---|
| `restart/README.md` §12 | Add SK-V7+ reading order; strike dead PASS-1/2/3/SYNTHESIS refs (line 435) |
| `restart/HANDOFF.md` §1 | Cite README §12 for cold-start order; trim duplicate reading orders |
| `restart/HANDOFF.md` §5 | Cite ARCHITECTURE.md §7.3 for BackendShape; carry only per-grammar matrix |
| `restart/MASTER-PLAN.md` §4 | Cite locks/LOCKS.md §15-16 instead of inlining |
| `restart/ARCHITECTURE.md` §7.3 | Mark as canonical BackendShape source; cross-link from HANDOFF §5 |
| `restart/skinny/INDEX.md` | Fix `restart/skinny/REDRESS.md`/`RESULTS.md` references to `skinny/` (workspace root) |
| `restart/skinny/BENCH.md` | Same fix |
| `restart/skinny/tranches/sk-v6/DISPATCH-PROMPT.md` verification ritual | Strike pre-SK-V3 SK-V{1,2} HARDENING-* refs (intentionally retired) |

**CREATE** (5 entries):

| Path | Purpose |
|---|---|
| `restart/prompts/pass-contracts/TOTALITY-PASS-1-RESEARCH.md` | currently dead-ref per README.md:9 |
| `restart/prompts/pass-contracts/TOTALITY-PASS-2-PROFILE.md` | same |
| `restart/prompts/pass-contracts/TOTALITY-PASS-3-HARDENING.md` | same |
| `restart/skinny/tranches/archive/README.md` | one-liner pointer to per-iteration archive shape |
| `restart/audit/archive/README.md` | same for hardening + pass-* archive |

---

## §8 — SK-V7 W0 dispatch one-liner

Per IMPLEMENTATION-PACKET-SK-V7.md §2, the W0 (comparator-plane repair)
dispatch is:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny \
  && sed -i '' 's/, "utf8_lossy"//' crates/bbnf-bench/Cargo.toml \
  && cargo tree -p bbnf-bench --edges=features | grep sonic-rs \
  && cargo bench -p bbnf-bench --bench json_parity \
  && cargo run -p bbnf-bench --bin gate --release \
  && git commit -am "feat(sk-v7-wave0): comparator-plane repair (sonic-rs strict)"
```

Verified prerequisite: `skinny/crates/bbnf-bench/Cargo.toml:21` carries:
```
sonic-rs = { version = "=0.5.8", default-features = false, features = ["sort_keys", "utf8_lossy"] }
```
After the edit:
```
sonic-rs = { version = "=0.5.8", default-features = false, features = ["sort_keys"] }
```

Expected impact (per IMPLEMENTATION-PACKET-SK-V7 §0.1 + §2 exit gate):
- sonic-rs Mbps drops 3-8% on every row.
- `instruments` parse flips to PASS (≥100% strict sonic).
- `unicode_basic` parse flips to PASS or documents residual.
- No Track 1 / Track 2 regression (W0 is comparator-only).
- Hard cap 60 min.

Companion write: `restart/skinny/tranches/sk-v7/research/wave-0-strict-baseline.md`
documenting the per-row Mbps delta. This file is currently dead-referenced
from HANDOFF-SK-V7.md + IMPLEMENTATION-PACKET-SK-V7.md (the W0 owner-paths
section).

---

## §9 — Closing posture

The restart/ tree is **structurally sound but accumulates**. The five
canonical surfaces (README, ARCHITECTURE, HANDOFF, MASTER-PLAN, MIGRATION)
hold; the locks file is correct; the skinny canonical surfaces (BENCH,
COMPILER, INDEX, SUBSTRATE, WORKSPACE, HARDENING) hold. The accumulation is
in three places: pre-Phase-8 audit + research artefacts (86 + 22 files;
mostly archival inertia); SK-V5 + SK-V6 cohorts living alongside SK-V7
(per-iteration archive discipline not yet applied); dead references to
retired prompt-suite + SK-V1..V4 master docs (73 dead links, mostly from
verification rituals that survived the file deletes).

The V7 prompt-suite is well-formed but not yet anchored — README.md +
HANDOFF.md cite the pre-Phase-8 prompt names, not the V7 framework. The
SK-V7 master docs + COHORT/ are intact and correctly anchored to PASS-ALPHA.
The Pass Alpha α-F output (the SK-V7 trio) exists; the Pass Omega CRUD pass
has not yet run on the V1 spec surfaces.

The proposed restructure is **archive-discipline plus reference-honesty plus
sub-orchestrator encapsulation**: pre-Phase-8 surfaces archive; SK-V5/V6
master + cohort archive; V7 prompts gain proper inward anchoring; the locks
file renames; the per-iteration cohort pattern formalises; the
`restart/totality/` tree per PASS-OMEGA.md §7 creates on first dispatch.

This synthesis is the cross-cutting layer over R1-R5 slice inspections.
Per-slice content drift, per-document accuracy issues, and per-cohort
hygiene findings live in R1-R5; the cross-tree restructure rationale lives
here.

---

## §10 — Report metadata

- File size: ~620 lines.
- Dead-link count: 73 unique paths.
- Duplicate-section count: 5 load-bearing (BackendShape, Locks 15-16,
  reading order, M5 Max cross-parser table, TapeKind variant naming).
- Five most-consequential moves: §7.2 table above.
- SK-V7 W0 dispatch one-liner: §8.

Generated 2026-05-16 by R6 cross-cutting audit per the user's dispatch.
