# SK-V7 Restructure R2 — `restart/skinny/` Canonical Surfaces Audit

Scope: inventory + per-file inspection of the six durable skinny-spec
surfaces under `restart/skinny/` plus the cross-iteration
`restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md`. Produces a pruning +
restructuring recommendation, with explicit attention to (1) the
`HARDENING.md` duplication question against `restart/prompts/audit-specs/HARDENING-LENS-SET.md`,
(2) the SK-V6 → SK-V7 anchor drift in `INDEX.md`, and (3) the dead
SK-V3 references that persist in `SUBSTRATE.md` + `SOTA-BEAT-DESIGN.md`
after the SK-V3/SK-V4 packets were deleted.

Boundary: this file is the only output. No tracked file is modified.

---

## §1. Full inventory

```
restart/skinny/BENCH.md                          2208 LOC (126 710 B)
restart/skinny/COMPILER.md                        995 LOC ( 67 507 B)
restart/skinny/HARDENING.md                       202 LOC ( 17 396 B)
restart/skinny/INDEX.md                           176 LOC ( 34 651 B)
restart/skinny/SUBSTRATE.md                       748 LOC ( 57 942 B)
restart/skinny/WORKSPACE.md                       695 LOC ( 53 538 B)
restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md          550 LOC ( 66 315 B)
                                                ─────
Total                                            5574 LOC (424 059 B)
```

Sibling `restart/prompts/audit-specs/HARDENING-LENS-SET.md` for the duplication audit:

```
restart/prompts/audit-specs/HARDENING-LENS-SET.md                      268 LOC ( ~21 KB)
restart/prompts/sub-orchestrators/HARDENING.md         151 LOC
```

Last-modified commits:

| File | Commit | Date band |
|---|---|---|
| `restart/skinny/BENCH.md` | `2631a834` SK-V6 asmjson DAV1D fold | 2026-05-15 |
| `restart/skinny/COMPILER.md` | `2631a834` SK-V6 asmjson DAV1D fold | 2026-05-15 |
| `restart/skinny/INDEX.md` | `2631a834` SK-V6 asmjson DAV1D fold | 2026-05-15 |
| `restart/skinny/SUBSTRATE.md` | `2631a834` SK-V6 asmjson DAV1D fold | 2026-05-15 |
| `restart/skinny/WORKSPACE.md` | `2631a834` SK-V6 asmjson DAV1D fold | 2026-05-15 |
| `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md` | `2631a834` SK-V6 fold | 2026-05-15 |
| `restart/skinny/HARDENING.md` | `1519cf16` SK-V4 redress | older (pre-SK-V5) |
| `restart/prompts/audit-specs/HARDENING-LENS-SET.md` | `bc31560c` phase-8.1 restructure | concurrent |

The lag is load-bearing. Five of the six durable spec files folded the
SK-V6 asmjson/DAV1D pass on 2026-05-15. `HARDENING.md` was last touched
under SK-V4 redress and therefore still names the SK-V1 cycle as
"first-pass after `restart/skinny/` lands the five quadrants"
(`HARDENING.md:139`) — even though SK-V6 has landed and SK-V7 is the
active cycle per `audit/GRAND-SYNTHESIS-SK-V7.md:1-18`.

---

## §2. Per-file inspection

### §2.1 `INDEX.md` (176 LOC)

Top-level structure (heading anchors at `INDEX.md:19, 29, 88, 102, 114, 144, 162`):

```
# Skinny Implementation Spec — Index
## Four quadrants
## What the skinny is testing
## What the skinny is NOT testing
## Cross-quadrant invariants
## Open contradictions and skinny-specific deviations from V1
## Decision protocol
## Authority cross-references
```

**Cross-references to other restart/ docs**:
`../ARCHITECTURE.md` §7.3 + §11; `../MASTER-PLAN.md`; `../MIGRATION.md`;
`../locks/LOCKS.md` Locks 1/14/15/16; `../prompts/HARDENING.md`
implicit via `HARDENING.md`'s reading list.

**Cross-references to SK-V{n} master docs**
(`INDEX.md:5-15, 57-74, 164-176`):

- `audit/GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md` (line 6) — named as
  "current synthesis layer."
- `audit/IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md` (line 7).
- `audit/HANDOFF-SK-V6.md` (line 8).
- `audit/SK-V6-COHORT/skv6-A*.md` + `skv6-B*.md` (line 9).
- `audit/IMPLEMENTATION-AGENT-PROMPT-SK-V6.md` (lines 58, 166).
- `audit/SOTA-BEAT-DESIGN.md` §6 (line 167).
- `audit/GRAND-SYNTHESIS-SK-V5.md` (lines 168, 174).
- `audit/IMPLEMENTATION-PACKET-SK-V5.md` (line 169).
- `audit/NUKE-PLAN-SK-V5.md` (line 170).
- `audit/HANDOFF-SK-V5.md` (line 171).
- `audit/SK-V5-COHORT/` (line 172).

**Stale content / dead anchors**:

1. **No SK-V7 reference exists in `INDEX.md`.** `audit/GRAND-SYNTHESIS-SK-V7.md`,
   `audit/IMPLEMENTATION-PACKET-SK-V7.md`, `audit/HANDOFF-SK-V7.md`, and
   `audit/SK-V7-COHORT/` (18 reports) are landed on disk
   (`ls restart/skinny/tranches/`) but the anchor doc still names SK-V6 as
   the active dispatch authority (`INDEX.md:5-9, 57-58`).
2. **The SK-V6 fold-back paragraph is now historical** (`INDEX.md:5-15`).
   Per `audit/GRAND-SYNTHESIS-SK-V7.md:42-67`, V6 is settled state
   (wins listed, open gates listed); V7 is the active cycle. The "active
   dispatch anchor" sentence at `INDEX.md:57-58` is incorrect.
3. **SK-V3/SK-V4 deletion is acknowledged** (`INDEX.md:58-59`: "It
   supersedes the deleted SK-V3/SK-V4 implementation packets and SK-V1/SK-V2
   hardening reports") — that part is honest.

**Lock 14 grammar-neutrality status**: clean. The vocabulary in `INDEX.md`
uses grammar-neutral names (`BackendShape`, `LayoutFacts`, `OffsetTape`,
`EventTape`, `SinkOnly`, `CollapsedStage`) and the per-grammar facts
flow through metadata + grammar source per `INDEX.md:14` ("grammar-specific
behavior enters only through grammar source, metadata, generated `.data`,
recognizer/cost facts, host/API schema facts, and generated runtime
modules").

**Close-condition state**: the close condition cited at `INDEX.md:174`
is the **SK-V5** close (`audit/GRAND-SYNTHESIS-SK-V5.md` §12), not the
SK-V7 close. SK-V7's per-row close at `audit/IMPLEMENTATION-PACKET-SK-V7.md`
§0.1 + §0.2 is not represented.

**Recommendation: EDIT-INLINE-{SK-V7 anchor refresh}**.
Specifically: replace the "SK-V6 authority update (2026-05-15)"
paragraph (lines 5-15) with an SK-V7 anchor; promote the SK-V7
references (grand-synthesis, packet, handoff, cohort directory); demote
the SK-V6 references to historical lineage; replace the close-condition
restatement at lines 174-175 with the SK-V7 close per
`IMPLEMENTATION-PACKET-SK-V7.md` §0. The four-quadrant table and the
cross-quadrant invariants survive intact.

---

### §2.2 `SUBSTRATE.md` (748 LOC)

Top-level structure (`SUBSTRATE.md:1, 3, 48, 297, 368, 466, 575, 594, 650, 671, 706, 737`):

```
# Skinny Spec — Substrate Slice
## 0. Scope and stance
## 1. Tape, TapeToken, ValueRef, DocumentView — concrete layouts
  §1.1–§1.6 covering EagerTape token, lazy Vec sealing, ValueRef,
  DocumentView, Tape ≡ structural projection (OffsetTape), typed event cursor.
## 2. Payload arena policy
## 3. SIMD scan integration contract
## 4. Direct-to-struct overlay
## 5. Snapshot / identity invariant
## 6. Visitor entry
## 7. What this skinny substrate omits, and why each omission is safe
## 8. Hand-coded JSON parity contract (delivered to BENCH)
## 9. Module layout for the skinny
## 10. Open questions surfaced for the orchestrator
```

**Cross-references to other restart/ docs** (sampled):
`../ARCHITECTURE.md` §9 (lines 1373-1426), §3.1 (lines 191-244), §7.2
(BIR rows lines 920-963), §7.3, §11 row 1519; `../audit/pass-3-runtime/PASS-3.md`
§4; `../audit/pass-2-codegen/PASS-2.md` §2; `../locks/LOCKS.md` Lock 1
+ Lock 8 (verbatim citations at `SUBSTRATE.md:35-43`).

**Cross-references to SK-V{n} master docs**:

- `restart/skinny/tranches/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` (line 14) —
  **DEAD LINK**. This file no longer exists (no `V3` or `V4` files in
  `audit/`).
- `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` (line 285) —
  **DEAD LINK**.
- SK-V5 redress items 50-56 and SK-V6 redress items 66-72 are referenced
  by *number* but the docs themselves are not cited as anchors.

**Stale content** (V3/V4/V5/V6 references that are superseded):

| Line | Content | Status |
|---|---|---|
| 9 | "split verdict" against original triad + expanded corpus | Pre-V7 framing; V7 packet collapses this to per-row close conditions |
| 14 | Cites `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` | Dead link |
| 23-31 | "SK-V6 substrate fold-back (2026-05-15)" | Outdated header; V7 is active |
| 244-256 | SK-V5 redress items 51 + 53 (rejected JsonEventCursor + JsonStructuralCursor) | Still binding negative evidence — KEEP |
| 285 | "exact wave contract lives in `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md`" | Dead link; receiver is now SK-V7 packet |

**Lock 14 grammar-neutrality status**: The doc explicitly claims
grammar neutrality at `SUBSTRATE.md:7` ("The substrate is **grammar-neutral**:
every type, layout decision, and arena policy here is the same shape the
full V1 will ship for CSS, BBNF, Sheets, and the rest"). The five
`BackendShape` variants are correctly named (`SUBSTRATE.md:54-57, 225,
260, 274, 281`). JSON-prefixed identifiers (`JsonRoot`, `JsonObject`,
`JsonString`, `JsonNumber`, etc.) appear, but they are presented
as **typed views generated per grammar** (§4.1; `SUBSTRATE.md:472-550`),
which is the correct Lock 14 disposition — the kind markers
(`JsonRootKind` etc., `SUBSTRATE.md:168-178`) are codegen-emitted, not
substrate-baked.

**5-shape `BackendShape`**: documented at `SUBSTRATE.md:54-57` and
`SUBSTRATE.md:225, 260, 274, 281`. JSON skinny derives `OffsetTape` per
ARCH §7.3 (correctly noted at `SUBSTRATE.md:225`). The V6 substrate
union (Lock 1 verified) is reflected in the §1.5 + §1.6 surfaces.

**V5/V6 admits**: The `ContainerNext` admit (`V6 admit 2b3bef79`) is
*not* explicitly named in `SUBSTRATE.md` — it lives only in the SK-V7
synthesis's V6-state recap. The tiny-string cap (V6 admit `1e213001`)
is referenced obliquely via REDRESS-72 in `INDEX.md:127` but does not
surface in `SUBSTRATE.md`.

**Recommendation: EDIT-INLINE-{V3 deadlink purge + V7 fold-back}**.
Specifically: (a) replace the SK-V3 link at line 14 with a citation to
`audit/GRAND-SYNTHESIS-SK-V7.md` §2 (the V6 wins recap is precisely
the V3/V5/V6 lineage summary the line is gesturing at); (b) replace
line 285's dead packet link with `audit/IMPLEMENTATION-PACKET-SK-V7.md`;
(c) refresh the "SK-V6 substrate fold-back (2026-05-15)" paragraph at
lines 23-31 to "SK-V7 substrate state (2026-05-16)" naming the four
V6 wins (SinkOnly from BIR, Eisel-Lemire vendored, Canada SIMD floor
restored, ContainerNext dispatch carry, tiny-string cap, host-output-schema
DirectBuild, BackendShape Rust state) per `audit/GRAND-SYNTHESIS-SK-V7.md`
§2.

---

### §2.3 `COMPILER.md` (995 LOC)

Top-level structure (`COMPILER.md:1, 43, 158, 218, 416, 554, 670, 856, 895, 928, 961`):

```
# Skinny Spec — Compiler Slice
## 1. json.bbnf Source Sketch
## 2. Grammar IR Subset
## 3. BIR Subset
  §3.3 Lowering matrix per LayoutFacts.backend_shape (normative)
## 4. HM-Only Type Checker
## 5. Pipeline Subset
## 6. codegen::rust Path
## 7. What's Stubbed In The Skinny
## 8. The Compile-And-Test Loop
## 9. Open Questions And Source-Authority Conflicts
## 10. Summary
```

**Cross-references**: `SUBSTRATE.md`, `BENCH.md`, `WORKSPACE.md`,
`../ARCHITECTURE.md` §7.3, `../audit/pass-2-codegen/PASS-2.md:432`,
`../locks/LOCKS.md` Lock 14/15/16.

**Cross-references to SK-V{n} master docs**:
SK-V5 redress 52/54/57 (lines 383-389); SK-V6 redress 66-72
(lines 355, 364, 370, 394); SK-V5 cohort B1 + IMPLEMENTATION-PACKET-SK-V5
+ IMPLEMENTATION-AGENT-PROMPT-SK-V6 (lines 789, 851-852).

**Stale content**:

| Line | Content | Status |
|---|---|---|
| 29-36 | "SK-V6 compiler fold-back (2026-05-15)" | Header should rebase to SK-V7 |
| 36-39 | C6 cleanup directive on `parse-that-regex` JSON-named helpers | Still binding (Lock 14 cleanup target per SK-V7 A5+B3) |
| 781 | "`parse-attribution` feature flag (SK-V5 Wave 0)" | Landed; still binding |
| 845-848 | SK-V6 fold-back of `parse-attribution` mandate | Still binding |
| 851-852 | Authority pointers to SK-V5 packet + SK-V6 agent prompt | V7 packet supersedes |

**Lock 14 grammar-neutrality status**: §3.3 lowering matrix
(`COMPILER.md:253-265`) names the five `BackendShape` values
correctly. The JSON-specific concretion is in `parse_value` / `parse_object`
example bodies (`COMPILER.md:267-301`), which is appropriate for a
skinny example. The `grammars/json/json.bbnf` source sketch
(`COMPILER.md:53-93`) is fine — it is a grammar source file, not generic
crate code.

**Recommendation: EDIT-INLINE-{SK-V7 anchor refresh + authority pointer
rebase}**. Replace "SK-V6 compiler fold-back" header at lines 29-39
with "SK-V7 compiler state (2026-05-16)" and rebase the authority
pointers at lines 851-852 from SK-V5 packet + SK-V6 prompt to SK-V7
packet. The §3.3 lowering matrix, §4 HM-only checker, and §6 codegen
path are durable spec content; do not touch them.

---

### §2.4 `BENCH.md` (2208 LOC)

This is the largest canonical doc by a 3× margin (next is COMPILER.md
at 995). Top-level structure (10 §-headers; from `BENCH.md:1, 35, 168,
310, 487, 595, 760, 974, 1459, 1590, 1716`):

```
# Skinny Spec — Bench and Parity Harness
## §1 The dual-track contract
## §2 Comparator baselines and workload planes
## §3 Corpus tiers (17-corpus expansion at §3.1)
## §4 The structural-scan microbenchmark
## §5 Reproducibility schema (+ §5.1.1 strictness columns + schema v3)
## §6 Go/no-go threshold matrix
## §7 Criterion harness layout (+ §7.8 masking probes, §7.9 conformance, §7.10 comparative profile)
## §8 CI integration
## §9 What this skinny bench omits
## §10 Verdict-writing template — skinny/RESULTS.md
```

**Cross-references to other restart/ docs**: extensive — `../ARCHITECTURE.md`
§7.3 + §11; `../MASTER-PLAN.md` §4 lines 108-169 + 140-154 + 160-168;
`../locks/LOCKS.md` Lock 8 + Lock 9 + Lock 16; `../corpora/SOTA.md`
lines 50-89 + 130-136.

**Cross-references to SK-V{n} master docs**:
SK-V6 fold-back at lines 19, 26, 183, 309, 320, 330, 448, 455, 639,
641-642, 651, 673, 678, 799, 801, 880, 896-897, 1303-1304, 1306, 2185.
SK-V5 redress items 52-57 + cohort B3 native sidecars
(lines 641-642). Most-recent commit cite at line 2185.

**Stale content**:

| Line | Content | Status |
|---|---|---|
| 19-26 | "SK-V6 fold-back (2026-05-15)" | Header → SK-V7 |
| 183 | "SK-V6 caveat: bench dependency must not enable `utf8_lossy`" | SK-V7 A1 confirms this is **one-line Cargo diff** Wave 0; the caveat is correct |
| 1303-1306 | Alternate-plan probes still reference SK-V5/SK-V6 numbering | Authority pointers stale |
| 2185 | "Canada structural_scan = 69075 Mbps" | Was the V5/V6 number; SK-V7 has Canada 41,833 Mbps per V7 synthesis §2 — **CONFLICT** |

**Lock 14 grammar-neutrality**: §2.5 (`competitor configuration table`)
+ §3.1 (corpus inventory) name JSON corpora explicitly, which is correct
— this is a JSON skinny bench. The probe vocabulary in §7.8.2
(`alternate_event_cursor_plan`, `alternate_fused_string_sink_plan`,
`alternate_primitive_kernel_plan`) is grammar-neutral.

**Recommendation: EDIT-INLINE-{SK-V7 header + Canada number reconciliation}**.
The doc is canonical and well-structured at 2208 LOC; do not split. The
narrowest edits are: (a) rebase the SK-V6 fold-back header at lines 19-26
to SK-V7, (b) reconcile the Canada structural_scan number at line 2185
against `audit/GRAND-SYNTHESIS-SK-V7.md` §2 ("Canada SIMD scan floor
restored: 22,136 → 41,833 Mbps"), (c) rebase the alternate-plan
authority pointers at lines 1303-1306 to the SK-V7 packet §3 + §5.

There is a case for splitting `BENCH.md` (§7 alone is 480 LOC; §6 is
210 LOC) but the cross-references inside the doc are dense and a split
would scatter the citations. **Leave as a single file** until a later
restructure round.

---

### §2.5 `WORKSPACE.md` (695 LOC)

Top-level structure (`WORKSPACE.md:1, 3, 55, 92, 122, 296, 456, 521,
... §7 §8 §9 §10 §11`). 10 main sections covering boundary, crate set,
LOC budget, Cargo.toml skeleton, directory layout, build commands, xtask
runner, migration parity, deviation ledger, closure conditions.

**Cross-references**: `SUBSTRATE.md`, `COMPILER.md`, `BENCH.md`,
`../audit/pass-2-codegen/PASS-2.md:432`, `../locks/LOCKS.md` Lock
13/14/16, `skinny/REDRESS.md` items 15/16/17/18/19/20/25.

**Cross-references to SK-V{n} master docs**: §0.1 "Post-Iteration State
(SK-V2)" heading at line 31 — predates SK-V3+ but the body has been
updated through SK-V6. SK-V5 redress item 56 (line 42). SK-V3 bench
redress (line 37, 94, 106). SK-V4 direct-into-codegen (line 69, 106).
SK-V6 workspace fold-back paragraph (lines 20-27).

**Stale content**:

| Line | Content | Status |
|---|---|---|
| 20-27 | "SK-V6 workspace fold-back (2026-05-15)" | Header → SK-V7 |
| 31 | "Post-Iteration State (SK-V2)" heading | Misleading — body has SK-V3/V4/V5/V6 content |
| 37 | "redressed to 3,300 LOC after final auditability gates" | SK-V3-vintage justification; still correct quantum |
| 69 | "SK-V4 moves Track 1 direct into generated runtime/codegen" | Landed in SK-V6 per `audit/GRAND-SYNTHESIS-SK-V7.md:48-49` |
| 106 | bbnf-bench crate description still references SK-V3/V4 sequencing | Landed; rebase |
| 562 | "and pointing implementers at the SK-V2 surgery options" | Stale |
| 614, 666 | REDRESS §-citations | Persist correctly |

**Lock 14 grammar-neutrality status**: §1 (Skinny Crate Set) names
crates that are grammar-neutral. The `grammars/json/` directory under
`runtime/src/` is the per-grammar instance; the rest of the workspace
is grammar-neutral. The `bbnf-simd` crate per `WORKSPACE.md:68, 105`
honors Lock 16 (NEON + scalar mandatory; AVX2 available; AVX-512 +
handwritten ASM Lock-16-allowlisted). SK-V6 W4 already split the
`bbnf-simd` JSON god-module residue (716 → 273 LOC per
`audit/GRAND-SYNTHESIS-SK-V7.md:153`). Lock 14 status: clean modulo
the V7 leak count (46 HIGH leaks per A5+B3) which is a codegen/passes
issue, not a `WORKSPACE.md` issue.

**Recommendation: EDIT-INLINE-{SK-V7 anchor refresh + heading rebase}**.
(a) Rebase the §0.1 heading from "Post-Iteration State (SK-V2)" to
"Post-Iteration State (SK-V7)"; (b) refresh the SK-V6 fold-back
paragraph at lines 20-27 to SK-V7; (c) edit line 562 reference from
SK-V2 to current (SK-V7); (d) leave the per-crate LOC budget intact —
the 32,000 handwritten + 4,000 generated envelope survives. The
Cargo.toml + directory layout + xtask sections are durable.

---

### §2.6 `HARDENING.md` (202 LOC) — THE DUPLICATION QUESTION

Top-level structure (`HARDENING.md:1, 7, 21, 36, 52, 75, 133, 145, 163,
180, 190, 196`):

```
# HARDENING-SKINNY — Per-Target Audit Specification
## §1 — Purpose: why skinny hardening differs from V1 hardening
## §2 — Target selection
## §3 — Required reading (mandatory; in order)
## §4 — Lens registry
## §5 — Skinny-specific lenses
  ### Lens L — Premise fidelity
  ### Lens M — Falsifiability
  ### Lens N — Graduation mechanicality
## §6 — Cycle naming
## §7 — Per-item discipline
## §8 — Output contract
## §9 — Hard cap
## §10 — Cross-tranche scope boundary
## §11 — Closing posture
```

**Cross-references**: Composes with `restart/prompts/audit-specs/HARDENING-LENS-SET.md`
(lines 19, 36, 42, 54, 56-67); cycle dispatch via
`restart/prompts/sub-orchestrators/HARDENING.md` (line 198).

**Comparison vs `restart/prompts/audit-specs/HARDENING-LENS-SET.md` (268 LOC)**:

| Surface | `restart/prompts/audit-specs/HARDENING-LENS-SET.md` (V1) | `restart/skinny/HARDENING.md` (skinny) |
|---|---|---|
| Targets | PASS-1/2/3, MASTER-PLAN, SUITE (V1 master plan + ARCH + MIGRATION) | SUBSTRATE, COMPILER, BENCH, WORKSPACE, INDEX, SKINNY-SUITE (skinny quadrants) |
| Lenses A-K | Defined verbatim (Lanes 1-9 + Lens F + G + H + I + J + K) | **Referenced only** — "Lenses A-K live at `restart/prompts/audit-specs/HARDENING-LENS-SET.md`" (line 198) |
| Lenses L-M-N | Absent | **Defined verbatim** (skinny-specific premise fidelity, falsifiability, graduation mechanicality) |
| Verdict classes | KEEP/REINVENT/DISCARD + V8+ SIMPLIFY/CONSOLIDATE/LEVERAGE/HYBRID/LOAD-BEARING/ASPIRATIONAL/SPECULATIVE | Inherits V1 verdicts + adds FAITHFUL/MASKING (Lens L) + MECHANICAL/ANTI-MECHANICAL (Lens N) |
| Cycle namespace | V1, V2, … (V1 corpus) | SK-V1, SK-V2, … (skinny corpus) |
| Output path | `restart/audit/hardening/HARDENING-{TARGET}.md` | `restart/skinny/tranches/HARDENING-{TARGET}-SK-V{N}.md` |

**Verdict**: this is **NOT a duplicate**. `restart/skinny/HARDENING.md`
is a complementary skinny-scoped audit specification that explicitly
composes by reference with the V1 spec at `restart/prompts/audit-specs/HARDENING-LENS-SET.md`
(line 198: "Lenses A-K live at `restart/prompts/audit-specs/HARDENING-LENS-SET.md`; Lenses
L-N live here. Cycle dispatch lives at
`restart/prompts/sub-orchestrators/HARDENING.md`; skinny target table +
cycle namespace live here").

The two files are **canonical for different audit scopes**:

- `restart/prompts/audit-specs/HARDENING-LENS-SET.md` audits V1 architecture surfaces
  (MASTER-PLAN + ARCHITECTURE + MIGRATION + passes 1/2/3).
- `restart/skinny/HARDENING.md` audits skinny implementation surfaces
  (BENCH + COMPILER + SUBSTRATE + WORKSPACE + INDEX, plus the SUITE
  combiner) and inherits A-K from the V1 prompt.

**Stale content in `restart/skinny/HARDENING.md`**:

| Line | Content | Status |
|---|---|---|
| 139 | "SK-V1 \| (initial) \| First-pass after `restart/skinny/` lands the five quadrants" | Stale; SK-V6 has landed, SK-V7 is the active cycle |
| 141 | "SK-V3+ \| SK-V2 \| Subsequent measurement-driven amendments" | Stale; cycle is now at SK-V7 |
| 143 | "Cycle outputs land at `restart/skinny/tranches/HARDENING-CONSOLIDATED-SK-V{N}.md`" | **No `HARDENING-CONSOLIDATED-SK-V*.md` files exist in `audit/`** — the convention was abandoned in favor of the `GRAND-SYNTHESIS-SK-V{n}.md` + cohort directory pattern |

This last item is the most acute: the `HARDENING.md` cycle output path
contract is unenforced. The actual cycle output for SK-V5/V6/V7 lives
at `GRAND-SYNTHESIS-SK-V{n}.md` + `SK-V{n}-COHORT/`, not at
`HARDENING-CONSOLIDATED-SK-V{n}.md`.

**Lock 14 grammar-neutrality status**: not applicable — this is a
process spec, not a code/grammar surface. The lens definitions are
grammar-neutral.

**Recommendation: KEEP-with-EDIT-INLINE-{cycle table + output-path rebase}**.
Specifically:

1. **Do not delete** `restart/skinny/HARDENING.md` — it is canonical
   for skinny-scoped Lenses L/M/N + cycle naming + per-quadrant target
   table.
2. **Do not move** `restart/prompts/audit-specs/HARDENING-LENS-SET.md` — it is canonical for
   V1 Lenses A-K and is referenced by the skinny doc.
3. **Rename clarification optional**: if disambiguation is desired,
   rename `restart/skinny/HARDENING.md` → `restart/skinny/HARDENING-SKINNY.md`
   to match its top-line title (`# HARDENING-SKINNY — Per-Target Audit
   Specification`). The convention is already in the title; only the
   filename does not reflect it. **Recommendation: do not rename**
   (cost: every consumer that cites `HARDENING.md` would need a rebase
   and the disambiguation is already carried by the directory parent;
   `restart/skinny/HARDENING.md` is unambiguous in context).
4. **Edit the cycle naming table** (lines 137-141) to add SK-V3 through
   SK-V7 rows or to generalize the table to SK-V{N} progression.
5. **Edit the output path** at line 143 from
   `HARDENING-CONSOLIDATED-SK-V{N}.md` to
   `GRAND-SYNTHESIS-SK-V{N}.md` plus the cohort-dir pattern, matching
   the actual practice.

---

### §2.7 `audit/SOTA-BEAT-DESIGN.md` (550 LOC)

Top-level structure (`SOTA-BEAT-DESIGN.md:1, 18, 32, 65, 231, 316, 435,
504, 522, 532, 544`):

```
# SOTA-BEAT-DESIGN — Structural-Index-Driven Codegen with SIMD Primitive Layer
## §1. Empirical premise
## §2. The architectural shape — structural-index-driven typed parse
## §3. SIMD primitive layer — bbnf-simd crate
## §4. Codegen template contract (lowering pattern; no new BIR variant)
## §5. Phase 3 — Collapsed-stage AVX-512 backend (asmjson-class)
## §6. Falsifiability + gates
## §7. Implementation sequence
## §8. Locks affected
## §9. Open residues
## §10. Verdict
```

**Cross-references**: `skinny/RESULTS.md`,
`skinny/profile/{native-sidecars,sonic-rs-v2,sonic-rs-expanded,simdjson-v2,
yyjson,wave2-asm,wave2-pmu,wave2-capacity,reprofile-2026-05-12,skinny-expanded}/PROFILE-REPORT.md`,
`SUBSTRATE.md`, `BENCH.md`, `COMPILER.md`, `INDEX.md`, `WORKSPACE.md`,
`../locks/LOCKS.md` Lock 1/10/14/15/16, dav1d FFmpeg references.

**Cross-references to SK-V{n} master docs**:

- Line 4: "superseded for dispatch by
  `IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md` (2026-05-15)" —
  explicit status header that this doc is **historical design input**,
  not active dispatch. Honest.
- Line 13: "current SK-V3 Wave 0/1 run" — stale (SK-V3 superseded).
- Line 30: cites `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` §3 — **DEAD LINK**.
- Line 165: cites a "SK-V3/SK-V4 M5 Max close condition" — superseded.
- Line 309: "Post-SK-V3 correction" — accurate as historical lineage.
- Line 429: cites "SK-V4 close claim" — superseded by SK-V7.
- Line 506: cites `IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md` as
  current receiver — **DEAD LINK**.
- Line 507: "the older SK-V3 packet remains historical context" — honest.
- Line 550: "execute `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md`" —
  **DEAD LINK** in the final "Hereupon" line.

**Stale content**: Lines 30, 165, 429, 506, 550 all carry dead V3/V4
packet links. The doc's status header (lines 3-11) is honest about
historical-input-only status but the body still names SK-V3/V4 as if
they were active.

**Lock 14 grammar-neutrality**: STRONG — §3 (`bbnf-simd` crate) is
explicitly grammar-neutral; §5.2 ("Two-layer reusable vocabulary
(dav1d / asmjson pattern)") at lines 342-362 makes the Layer 0
(`ext/x86/x86inc.asm`) + Layer 1 (`ext/x86/bbnf.asm`) generality
contract explicit; line 369 anchors the Lock 14 invariant ("zero
overfitting is preserved unconditionally: no grammar-specific
instruction lives in any generic crate"). The JSON-prefixed identifiers
that appear (`MAX_JSON_DEPTH`, `parse_json_zmm_dom.S`) are
**comparator references** (asmjson source) not bbnf primitives —
correct citation usage.

**SOTA-beat targets currency**: §6.4 (lines 477-503) names sonic-rs
Value-DOM 2438 MiB/s, simdjson DOM 2923 MiB/s, yyjson 3687 MiB/s,
asmjson 10.93 GiB/s territory. These are the M5 Max comparator anchors
established in SK-V6 and still binding in SK-V7 (per
`audit/GRAND-SYNTHESIS-SK-V7.md:79-86` — "yyjson at 3,687 MiB/s
twitter (0.91 c/B) is the actual M5 Max DOM-class leader, not simdjson
(1.142 c/B). bbnf at 15,597 Mbps = ~1,950 MiB/s. The 1.98x gap is the
primary SOTA-beat target"). The targets are current.

**Should it move to `restart/skinny/tranches/design/`?**: arguable but
not required. The doc is cross-iteration design context with explicit
"historical design input" status. Moving it under `audit/design/` would
isolate it from the per-iteration audit cohort (which currently
co-locates it). Per the user's `feedback_no_god_modules` discipline, a
nested `design/` directory inside `audit/` for a single doc is
contrivance.

**Recommendation: KEEP-AT-AUDIT-with-EDIT-INLINE-{dead-link purge +
status-header refresh}**. Specifically:

1. **Keep at `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md`** (do not move).
2. **Refresh status header** (lines 3-11): change "superseded for
   dispatch by `IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md`
   (2026-05-15)" to "superseded for dispatch by
   `IMPLEMENTATION-PACKET-SK-V7.md` (2026-05-16)".
3. **Purge dead V3/V4 packet links** at lines 30, 506, 550 —
   replace with citations to the SK-V7 packet's §3-§5 wave plan and
   the V7 synthesis's V6-state recap.
4. **Refresh line 13** ("SK-V3 Wave 0/1 run") to "SK-V6 post-fold
   state" with the V7 synthesis as the active anchor.
5. The §3 SIMD primitive crate spec + §5 collapsed-stage AVX-512 backend
   description + §6 phase gates are durable design content; do not edit
   the body.

---

## §3. Cross-document cohesion matrix

| Concept | INDEX | SUBSTRATE | COMPILER | BENCH | WORKSPACE | HARDENING | SOTA-BEAT-DESIGN | Cohesion |
|---|---|---|---|---|---|---|---|---|
| 5-shape BackendShape names | `:137` | `:54-57, 225` | `:255, 257-263` | `:1303` | n/a | n/a | `:188, 246-272, 359, 406` | **Coherent** — `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` everywhere |
| TapeKind variants | `:108, 134` | `:60-96` | n/a | n/a | n/a | n/a | n/a | Coherent — `TapeToken`/`NodeKindId`/`TokenFlags` only |
| Lock 1 (tape substrate) | `:108, 134, 176` | `:40` | `:255` | n/a | n/a | n/a | `:63, 526` | Coherent |
| Lock 14 (grammar-neutrality) | `:112, 130` | `:677` | `:36, 39` | n/a | `:25, 185, 196, 653` | `:32` | `:369, 528` | Coherent |
| Lock 15 (build profile) | `:131, 142` | `:288` | `:784, 834` | n/a | n/a | n/a | `:529` | Coherent |
| Lock 16 (SIMD admissibility) | `:130, 139, 176` | `:30, 286-292` | n/a | n/a | `:68, 105` | n/a | `:530` | Coherent |
| `SK-V6` active dispatch claim | `:5, 57` | `:23-31` | `:29-39` | `:19-26` | `:20-27` | (silent) | `:4` | **STALE** — SK-V7 is active, all 5 quadrants need rebase |
| `SK-V7` reference | absent | absent | absent | absent | absent | absent | absent | **MISSING** — the active cycle is not anchored anywhere in the 6 canonical files |
| SK-V3/V4 dead links | absent | `:14, 285` | absent | absent | absent | absent | `:30, 506, 550` | **STALE** — 5 dead links to deleted V3/V4 docs |
| Wave letter convention | "wave 0/1 V5 cohort" implicit at `:172` | `:242` | (silent) | "Wave 2 Agent N" `:803, 814` | n/a | "skinny single-wave; Lane 2 N/A" `:58` | "Wave 1/2/3" `:20, 22, 30, 70` | Mixed — `Wave 2 Agent N` (V5 cohort) collides with `Wave 0/1/2/3` (SK-V3 vintage); both still appear |
| Canada SIMD scan number | `:130, 142` (69,075 Mbps) | `:225` (no number) | (silent) | `:2185` (69,075 Mbps) | `:42-49` (no number) | n/a | `:489` (10.93 GiB/s asmjson) | **CONFLICT** — INDEX + BENCH cite 69,075 Mbps as V5-redress-56 number; SK-V7 synthesis cites 41,833 Mbps. Both real, different bench shapes; current doc surface gives 69,075 |
| Open contradictions ledger | `:114-142` (24 rows) | `:737-748` (4 rows) | `:928-960` (2 rows) | `:1716` "verdict template" | `:614-666` (deviation ledger) | "open contradictions in target" | n/a | Coherent design — each quadrant carries its own ledger |
| Outcome classification | `:79-84` (5-row outcome table) | n/a | n/a | `:782` (full matrix), `:799-801` (L + N-direct) | n/a | n/a | n/a | Coherent — INDEX has summary, BENCH has full |
| Lens L/M/N (skinny lenses) | n/a | n/a | n/a | n/a | n/a | `:75-132` | n/a | Canonical at HARDENING only — correct |

**Net cohesion verdict**: terminology and lock-anchoring are coherent.
The two acute drifts are (a) every quadrant still names SK-V6 as the
active cycle; (b) `SUBSTRATE.md` + `SOTA-BEAT-DESIGN.md` carry five
dead links to deleted SK-V3/SK-V4 packets.

---

## §4. Proposed restructure

The structural shape is already correct:

```
restart/skinny/
├── BENCH.md            ← keep canonical (2208 LOC)
├── COMPILER.md         ← keep canonical (995 LOC)
├── HARDENING.md        ← keep canonical (202 LOC); NOT a duplicate of restart/prompts/audit-specs/HARDENING-LENS-SET.md
├── INDEX.md            ← keep + rebase SK-V6 → SK-V7 anchors
├── SUBSTRATE.md        ← keep canonical (748 LOC); purge SK-V3 dead links
├── WORKSPACE.md        ← keep canonical (695 LOC); rebase §0.1 heading
└── audit/              ← per-iteration audit + cross-iteration design
    ├── SOTA-BEAT-DESIGN.md            ← keep at top of audit/; refresh status header
    ├── GRAND-SYNTHESIS-SK-V{5,6,7}.md ← per-iteration synthesis
    ├── IMPLEMENTATION-PACKET-SK-V{5,6,7}.md
    ├── HANDOFF-SK-V{5,6,7}.md
    ├── SK-V{5,6,7}-COHORT/
    └── V9.5-PSI-EXCAVATION/           ← pre-skinny excavation
```

**No file moves recommended**. The directory shape is correct. Per
`feedback_no_god_modules` and `feedback_directory_modules`, isolating
`SOTA-BEAT-DESIGN.md` into a `design/` sub-directory would create a
single-child mount-point (contrivance). The current sibling placement
under `audit/` correctly signals that it is design context for the
audit cohort.

**Pruning targets** — strict edits only, no deletes:

| File | Edit | Lines touched | Rationale |
|---|---|---|---|
| `INDEX.md` | SK-V6 anchor block → SK-V7 anchor block | 5-15 |  Active dispatch authority is SK-V7 |
| `INDEX.md` | Authority cross-refs rebase | 164-176 | Promote SK-V7 docs; demote SK-V5 to lineage |
| `SUBSTRATE.md` | Purge SK-V3 dead link | 14 | File no longer exists |
| `SUBSTRATE.md` | Purge SK-V3 dead link | 285 | Receiver is now SK-V7 packet |
| `SUBSTRATE.md` | SK-V6 fold-back header → SK-V7 | 23-31 | Active state |
| `COMPILER.md` | SK-V6 compiler fold-back → SK-V7 | 29-39 | Active state |
| `COMPILER.md` | Authority pointer rebase | 851-852 | SK-V7 packet is canonical receiver |
| `BENCH.md` | SK-V6 fold-back header → SK-V7 | 19-26 | Active state |
| `BENCH.md` | Reconcile Canada Mbps | 2185 vs SK-V7 §2 | Per-iteration measurement update |
| `BENCH.md` | Alternate-plan authority refs | 1303-1306 | Rebase to SK-V7 |
| `WORKSPACE.md` | §0.1 heading SK-V2 → SK-V7 | 31 | Heading mislabel |
| `WORKSPACE.md` | SK-V6 fold-back → SK-V7 | 20-27 | Active state |
| `WORKSPACE.md` | "SK-V2 surgery options" → current | 562 | Stale |
| `HARDENING.md` | Cycle naming table SK-V3+ rows | 137-141 | Need SK-V3..SK-V7 enumeration |
| `HARDENING.md` | Output path convention | 143 | `HARDENING-CONSOLIDATED-SK-V{N}.md` was never adopted; rebase to `GRAND-SYNTHESIS-SK-V{N}.md` + cohort-dir convention |
| `SOTA-BEAT-DESIGN.md` | Status header refresh | 3-11 | Supersession authority is SK-V7 packet, not SK-V6 |
| `SOTA-BEAT-DESIGN.md` | Purge SK-V3 dead link | 30 | `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` deleted |
| `SOTA-BEAT-DESIGN.md` | Purge SK-V4 dead link | 506 | `IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md` deleted |
| `SOTA-BEAT-DESIGN.md` | Purge SK-V3 dead link | 550 | "Hereupon: execute …-SK-V3-…" — receiver is SK-V7 |

---

## §5. Pruning summary

**LOC before**: 5574 (six canonical + SOTA-BEAT-DESIGN).

**LOC after edits (estimate)**: 5510-5560 LOC. The edits are
in-place rewrites of header paragraphs and authority blocks, not
deletions. Net change is ≈ -1% LOC.

**Files moved**: 0.

**Files renamed**: 0 recommended. (Optional disambiguation rename
`HARDENING.md` → `HARDENING-SKINNY.md` is explicitly declined per §2.6
recommendation — current name is unambiguous in context and the cost
of rebasing all consumers is not justified.)

**Files deleted**: 0.

**Files added**: 0 (the task is explicitly write-only of
`/tmp/skv7-restructure-R2-skinny-canonical.md`; no canonical surface
is changed).

**Cross-iteration design isolation**: `SOTA-BEAT-DESIGN.md` is the
only cross-iteration design doc in `audit/`. It correctly self-declares
historical-input status at `SOTA-BEAT-DESIGN.md:3-11`. Keeping it at
the top level of `audit/` (siblings of per-iteration cohorts) preserves
the design-as-context anchor for any future cycle that revisits the
SOTA-beat route.

---

## §6. Report

**File size**: 5574 LOC across 7 files (6 canonical + SOTA-BEAT-DESIGN);
26 LOC of header + the edits in §4 above totaling ≤2% touched.

**Top 3 stale-content items**:

1. **SK-V6 is named as active dispatch in all five non-HARDENING
   canonical surfaces** (`INDEX.md:5-15, 57-58`; `SUBSTRATE.md:23-31`;
   `COMPILER.md:29-39`; `BENCH.md:19-26`; `WORKSPACE.md:20-27`). SK-V7
   is the active cycle per `audit/GRAND-SYNTHESIS-SK-V7.md` (2026-05-16),
   `IMPLEMENTATION-PACKET-SK-V7.md`, `HANDOFF-SK-V7.md`, and the
   `SK-V7-COHORT/` directory of 18 reports. Zero references to SK-V7
   exist anywhere in the six canonical surfaces.
2. **Five dead links to deleted SK-V3/SK-V4 packets persist**:
   `SUBSTRATE.md:14` → `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md`;
   `SUBSTRATE.md:285` → `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md`;
   `SOTA-BEAT-DESIGN.md:30` → `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md`;
   `SOTA-BEAT-DESIGN.md:506` → `IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md`;
   `SOTA-BEAT-DESIGN.md:550` → `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md`.
   Per `INDEX.md:58-59` the SK-V3/SK-V4 packets were explicitly deleted;
   the in-body authority pointers were not rebased.
3. **`HARDENING.md` cycle-naming table is frozen at SK-V1/SK-V2/SK-V3+**
   (`HARDENING.md:137-141`) and the output-path convention
   (`HARDENING.md:143` `HARDENING-CONSOLIDATED-SK-V{N}.md`) was never
   adopted in practice — cycle outputs land at
   `GRAND-SYNTHESIS-SK-V{N}.md` + `SK-V{N}-COHORT/` directory.

**Recommendation on HARDENING.md duplication**: **NOT A DUPLICATE — KEEP
BOTH.** `restart/skinny/HARDENING.md` (202 LOC, skinny-scope) is
complementary to `restart/prompts/audit-specs/HARDENING-LENS-SET.md` (268 LOC, V1 scope).
They compose by reference at `restart/skinny/HARDENING.md:198`. Lenses
A-K are canonical in the V1 doc; Lenses L/M/N (premise fidelity,
falsifiability, graduation mechanicality) are canonical in the skinny
doc. The two cover different audit surfaces with non-overlapping
target tables. Deleting either is data-loss. No rename is required
(the in-doc title already disambiguates as "HARDENING-SKINNY"). The
only edits needed are the cycle-naming and output-path refresh at
`restart/skinny/HARDENING.md:137-143`.
