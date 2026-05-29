---
lens: CH2-GENERALITY
pass: T-P1-SKV17-excavation
cycle: V2
reviewed_at: 2026-05-29T22:40:00Z
subject_artefacts:
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
mandate: >
  Lock 14 grammar-neutrality firewall. No divergence catalogued as JSON-only
  when it is a grammar-neutral substrate fact. Excavation covers ALL grammars
  (JSON / CSS / Sheets / BBNF-self) for the value-API + NEON + tape surfaces
  (Lock 14). Not CSS-only. Every grammar-name leak in a generic crate flagged.
  JSON-empirical vs grammar-neutral separated.
live_truth_method: >
  grep/sed over crates/core/src/grammar/generated/*.rs, crates/simd-scan/src/{lib,alphabet}.rs,
  crates/core/src/runtime/{json,css_l4}/, crates/ir/src/registry/struct.rs,
  skinny/crates/{runtime,bbnf-simd}/src/, restart/ARCHITECTURE.md, restart/locks/LOCKS.md,
  restart/skinny/tranches/sk-v17/SPEC.md; wc -l for LOC; no build mutation
sections_dispositioned: 11
accept: 8
revise: 2
reject: 1
note: >
  Only 1f-* inventories exist for sk-v17 cycle V2; 1a–1e are absent from the
  pass root (see Open Orphan §6). CH2 dispositions the eleven catalogued
  sections that exist across the three 1F outputs. The two missing-inventory
  gaps for grammar-generality coverage (1C runtime census, 1D skinny-lessons
  JSON-vs-neutral split) are surfaced as orphan REVISE on the pass, not on a
  section, per §6.
---

## Executive Summary

CH2 GENERALITY firewall over the three SK-V17 T-P1 1F inventories. The
tape/value-API/NEON excavation is, on the whole, *grammar-correct*: the
lazy-`ValueRef<G>` generalization (COH17-002), the `select_classifier`/
`StructuralAlphabet` grammar-as-data classifier (COH17-008), the AoS-vs-SoA
tape mismatch (COH17-001), and the x86-pinned `CollapsedStage` canon
(COH17-004) are all catalogued as grammar-NEUTRAL substrate facts, not as
CSS-only or JSON-only quirks. That is the right altitude and most rows hold.

ONE material CH2 defect dominates: **the core structural-scan generality is
under-stated and mis-catalogued as JSON-restricted in three places**
(1f-coherence Exec Summary lines 55/57, Cross-Tree map line 84, and the Gaps
row line 107). The live truth is that `scan_structural` is wired into **all
eight** generated grammars in crates/core — including `css_l4.rs:15982`,
`css_pretty.rs`, `google_sheets.rs:3559` (Sheets), and `bbnf.rs:4843`
(BBNF-self) — not "json/ebnf/bnf/csv only." The Gaps row's assertion that
"generated css_l4 in crates/core [is] not in the scan-wired set … css_l4 grep
absent there" is **flatly contradicted by the live tree**. This is precisely
the CH2 failure mode: a grammar-neutral substrate fact narrowed to a
JSON/CSV-class subset, asserting a false CSS-absent gap, understating the very
generality Lock 14 protects. → REJECT the Gaps row (line 107); REVISE the
Cross-Tree map row (line 84) and the Exec Summary scan claim (lines 55/57).

A second REVISE: the Cross-Tree map (line 84) and Exec Summary (line 56)
characterise the SKINNY scan as "JSON-only today" without carrying the
SPEC §9.9 grammar-general-by-design mandate alongside the today-state — the
"JSON-only" is a wiring-state fact, not a design property, and the bare
phrasing invites a JSON-only mis-read of a grammar-neutral classifier.

All grammar-name references in generic crates are correctly dispositioned as
Lock-14-ALLOWED per-grammar runtime surfaces (1f-anti-pattern AP17-002/004);
no uncited grammar-name leak passes. The past-corpora ledger correctly
separates skinny-benched-surface findings from totality-tree fold-target
artefacts (1f-past-corpora PC17-005). 1f-anti-pattern and 1f-past-corpora are
clean on the generality axis (8 ACCEPT).

## Section Dispositions

### 1f-coherence-scan.md

| # | Section / row | Disposition | CH2 finding + concrete fix |
|---|---|---|---|
| 1 | COH17-001 tape AoS-vs-SoA (table row + Cross-Tree row) | **ACCEPT** | Catalogued grammar-neutrally: the tape encoding mismatch is a substrate fact across all grammars, not a CSS quirk. Cross-tree shape drift, not a per-grammar leak. Verified `crates/core/src/runtime/tape/record.rs:103` AoS vs `skinny/crates/runtime/src/tape/mod.rs:94` SoA. Correct altitude. |
| 2 | COH17-002 value-API per-grammar vs `ValueRef<G>` | **ACCEPT** | Correctly grammar-neutral. Verified: `grep value_from_ref\|ValueRef` over `crates/core/src/runtime/{json,css_l4}/` returns EMPTY (per-grammar eager enums); skinny `ValueRef<'doc,'input,K,G: EventGrammar>` at `tape/mod.rs:175` is grammar-parametric, and skinny runtime carries json + sheets_witness + 7 CSS modules — the lazy projection is genuinely grammar-general (JSON+CSS+Sheets). Not catalogued as CSS-only. Correct. |
| 3 | COH17-003 tape-unwired + scan-wired split | **ACCEPT** | Grammar-neutral asymmetry, correctly stated. Verified `TapeStructBuilder` grep-zero outside `tape/` (one doc-comment `TapeRec` ref in number.rs:17 only). The "scan wired" half is correct in fact but UNDER-scoped — see row 8 below; the COH17-003 row itself only cites json.rs:732 as an existence-proof of wiring, which is true, so the row stands. |
| 4 | COH17-004 CollapsedStage x86 vs aarch64-NEON | **ACCEPT** | Grammar-neutral canon fact. Verified `ARCHITECTURE.md:1206` "x86-only; aarch64 mechanically refused"; SPEC §9 `:854` D6. The 5-shape canon and its x86-pin are grammar-independent; correctly not catalogued per-grammar. The aarch64-NEON-absorbs-into-4-LLVM-shapes framing is grammar-general. Correct. |
| 5 | COH17-005 two SIMD crates multi-arch vs aarch64 | **ACCEPT** | Grammar-neutral scope fact. The doc-comment in `crates/simd-scan/src/alphabet.rs:17` ("no per-grammar code branch — the Shape value picks the lowering, the data drives it") confirms the multi-arch scan is grammar-as-data. Catalogued as impl-exceeds-spec scope, not a per-grammar defect. Correct. |
| 6 | COH17-006 StructLayout retired-but-live | **ACCEPT** | Grammar-neutral naming fact. Verified `LOCKS.md:160` retires StructLayout→Layout; `crates/ir/src/registry/struct.rs:202` `pub struct StructLayout` live. Naming is grammar-independent; correctly not scoped to a grammar. Correct. |
| 7 | COH17-007 FactStream 5th category vs SK-V17 retire | **ACCEPT** | Correctly carried as UNKNOWN with a verify_action (U-COH17-001). FactStream-as-CSS-admission is the SK-V17-specific case; the row correctly distinguishes the grammar-neutral Lock-1 FactStream output-plane category from the CSS-specific admission-plane retirement. The JSON-empirical-vs-neutral separation is honoured. Correct. |
| 8 | **Cross-Tree map line 84 — "Structural scan … WIRED into generated json/ebnf/bnf/csv"** | **REVISE** | CH2 DEFECT. The core column restricts scan-wiring to four grammars. LIVE: `scan_structural` is wired into ALL EIGHT generated grammars — `css_l4.rs:15982`, `css_pretty.rs`, `google_sheets.rs:3559`, `bbnf.rs:4843`, plus json/ebnf/bnf/csv. FIX: change the core cell to "WIRED into ALL generated grammars (json/ebnf/bnf/csv/**css_l4/css_pretty/google_sheets/bbnf**); the scan layer is grammar-general across JSON/CSS/Sheets/BBNF-self (`crates/core/src/grammar/generated/css_l4.rs:15982`, `google_sheets.rs:3559`, `bbnf.rs:4843`)." The skinny cell's "UNWIRED on CSS, JSON-only today" is a wiring-state fact (verified: only `skinny/crates/runtime/src/grammars/json/scan.rs` exists; CSS scan is W3 work) — append "by-design grammar-general per SPEC §9.9 (`SPEC.md:103-106`); CSS wiring is W3" so the today-state is not mis-read as a design property. |
| 9 | **Gaps row line 107 — "crates/core structural scan has NO css_l4 alphabet wired (json/ebnf/bnf/csv only) … css_l4 grep absent there"** | **REJECT** | CH2 DEFECT, FLATLY FALSE. Live counter-evidence: `crates/core/src/grammar/generated/css_l4.rs:15976` builds a `StructuralAlphabet` and `:15982` calls `scan_structural(input, &alphabet)`; `css_pretty.rs`, `google_sheets.rs:3549-3559`, and `bbnf.rs:4843` likewise. The css_l4 grep is NOT absent. This is the canonical CH2 failure: a grammar-neutral substrate generality narrowed to a JSON/CSV subset, asserting a phantom CSS-absent gap. FIX: DELETE this gap row entirely. Replace with the true gap (which is generality-positive): "The core structural scan is ALREADY grammar-general (all 8 grammars wired, including CSS/Sheets/BBNF-self); the gap is that NO tape consumes the index — the `OnceCell<StructuralIndex>` feeds eager builders, not a tape (`crates/core/src/grammar/generated/css_l4.rs:15952` OnceCell; no TapeStructBuilder consumer)." This reframes the gap from a false JSON-only restriction to the true Lock-1 unification gap. |
| 10 | COH17-008 alphabet richness | **ACCEPT** | Correctly grammar-neutral. Verified `crates/simd-scan/src/alphabet.rs:19-37` (`singletons`/`digraph_mask`/`digraph_pairs`/`quote_classes`) and skinny `select_classifier(&[u8;64])` `dispatch.rs:42`. Both are grammar-as-data; the row says so ("Both are grammar-as-data (Lock 14 honoured)"). The doc itself disclaims per-grammar branching. Correct. |

### 1f-anti-pattern.md

| # | Section / row | Disposition | CH2 finding |
|---|---|---|---|
| 11 | AP17-002/004 grammar-name leaks + sidecar (whole inventory, generality axis) | **ACCEPT** | CH2-clean. Every grammar-named construct in a generic crate is correctly dispositioned as Lock-14-ALLOWED per-grammar runtime surface, NOT a generic-crate leak: `CssStructBuilder`/`CssTypedValue` are per-grammar runtime (Lock 14 §SPEC:334), and the generic `TapeStructBuilder` is verified to dispatch on `StructLayout`, "never on per-grammar route strings" (`crates/core/src/runtime/tape/mod.rs:54-56`). The emitter `substrate.rs:43,55` consumes `builder_path` as DATA, not a grammar-name branch. The sidecar `OnceCell<StructuralIndex>` is grammar-general (all 8 grammars, consistent with the row-9 correction). No uncited leak. Correct. |

### 1f-past-corpora.md

The whole inventory is CH2-clean and folds into the AP17-002/004 ACCEPT above
on the generality axis. PC17-005 correctly separates skinny-benched-surface
artefacts (grammar-empirical) from totality-tree fold-target artefacts
(`StructLayout`/`OpenFrame`/`CssArena` are totality-only, alphaC:20-25),
and the Direction-Monotonicity note honours the JSON-empirical-vs-neutral
discipline (§8.5). No JSON-only mis-catalogue; no grammar-name leak. ACCEPT
(counted within the AP17 row above; no separate section defect).

## §6 — Orphan REVISE on the pass (missing inventories)

CH2's mandate is grammar-coverage across the value-API + NEON + tape surfaces
for ALL grammars. Two coverage surfaces are STRUCTURALLY ABSENT from cycle V2
and cannot be CH2-dispositioned because the inventories do not exist:

- **1c-runtime-evidence (absent).** §8.3 assigns the per-grammar runtime
  census + Lock-14 grammar-name leak audit to 1C. CH2 cannot confirm the
  Sheets (`google_sheets`) and BBNF-self (`bbnf`) runtime surfaces are
  generality-clean without it. The 1F anti-pattern scan partially covers this
  but is scoped to the tape/sidecar firewall, not the full per-grammar module
  census. **Orphan REVISE: dispatch 1C before V3 close** so the
  JSON/CSS/Sheets/BBNF-self runtime generality is fully catalogued, not
  inferred from 1F's narrower scan.
- **1d-skinny-lessons (absent).** §8.5 requires 1D to separate JSON-empirical
  from grammar-neutral skinny lessons so T-P3 folds only durable findings.
  The grammar-general NEON `select_classifier` (SPEC §9.9, exercises ≥1
  non-JSON grammar) and the sheets_witness §0.4 caveat (no `BackendRule`
  shape) are JSON-vs-neutral discriminations that belong in 1D; 1F's
  past-corpora ledger touches them but does not own the split. **Orphan
  REVISE: dispatch 1D before V3 close.**

These are pass-level orphans, not section defects; they do not count against
the eleven catalogued sections but block the §4 convergence ("zero orphan
unresolved REVISE") until the inventories land and CH2 re-scans them.

## CH2 Verdict

The 1F triad is grammar-correct at the substrate/value-API/canon altitude
(8 ACCEPT) but carries ONE three-fold scan-generality defect that
under-states the core scan's grammar-generality and asserts a phantom
CSS-absent gap — the exact Lock-14 failure mode CH2 firewalls. The Gaps row
(line 107) is factually false and REJECTed; the Cross-Tree map (line 84) and
Exec Summary (lines 55/57) are REVISEd to restore all-eight-grammar coverage
and to mark the skinny "JSON-only today" as a wiring-state, not a design
property. Two missing inventories (1C, 1D) block convergence as pass-level
orphan REVISE.

Counts: 11 sections — 8 ACCEPT, 2 REVISE, 1 REJECT (≈73% ACCEPT). Below the
≥95% gate; V3 fold required.
