---
lens: CH1-CORRECTNESS
pass: T-P1-excavation
cycle: V2
subject: SK-V17 T-P1 EXCAVATION
generated_at: 2026-05-29T22:40:00Z
master_head: 445925167154de73540e3ea3283d0170371de790
reviewer_method: "Read PASS-1-EXCAVATION.md §3 CH1; Read all three live inventories (1f-coherence-scan.md, 1f-anti-pattern.md, 1f-past-corpora.md); resolved every cited file:line via sed/grep over crates/core, crates/ir, crates/simd-scan, skinny/crates, restart/{ARCHITECTURE,MASTER-PLAN}.md, restart/locks/LOCKS.md, restart/skinny/tranches/sk-v17/{SPEC.md,research/}; no recall, every claim re-anchored at source"
inventories_reviewed:
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
inventories_absent:
  - 1a-substrate-evidence.md (NOT PRODUCED)
  - 1b-codegen-evidence.md (NOT PRODUCED)
  - 1c-runtime-evidence.md (NOT PRODUCED)
  - 1d-skinny-lessons.md (NOT PRODUCED)
  - 1e-locks-evidence.md (NOT PRODUCED)
disposition_counts:
  accept: 3
  revise: 3
  reject: 2
sections_dispositioned: 8
---

## CH1 CORRECTNESS — Scope + Method

CH1 verifies: every spec-claim ↔ implementation row resolves at the cited
file:line/SHA; the tape-substrate / eager-OpenFrame divergence is accurately
mapped; SKINNY-proven facts are cited correctly. No recalled LOC, no recalled
symbol path. The V2 cycle for SK-V17 T-P1 produced only the three 1F outputs;
the 1A–1E inventories are absent (see §0 Census Defect — this is the single
REJECT, because the spec-claim↔impl correctness floor for the substrate (1A),
codegen/BackendShape (1B), runtime census (1C), skinny-lessons (1D), and locks
(1E) rows is structurally unmet — those rows do not exist to verify). Within
the three 1F outputs that DO exist, the citation discipline is strong: of ~70
distinct file:line citations I re-anchored, the overwhelming majority resolve
exactly. Three classes of defect remain (one factual REJECT in COH17-008's
Gaps row; two REVISE for path-precision and a frontmatter count overstatement).

Master HEAD confirmed `445925167154de73540e3ea3283d0170371de790`.

---

## §0 — INVENTORY CENSUS (cross-cutting) — REJECT

**Finding CH1-V2-001 — Five of six required inventories absent.**

PASS-1-EXCAVATION §2 mandates six parallel inventories 1A–1F; §3 CH1 scans
"every spec-claim ↔ implementation row." Only 1F exists
(`restart/audit/totality/sk-v17/p1/{1f-coherence-scan,1f-anti-pattern,1f-past-corpora}.md`).
The substrate (1A), codegen/BackendShape (1B), runtime-census (1C),
skinny-lessons (1D), and locks-candidates (1E) inventories were not produced
(`find restart/audit/totality/sk-v17/p1 -type f` returns exactly the three 1F
files; all untracked).

Why this is a CH1 REJECT and not merely an orchestration gap: CH1's mandate is
to confirm the **spec-claim↔impl table rows resolve**. The §8 bbnf-lang axes
are explicit that 1A is "the Lock 1 substrate-union audit … spine", 1B must
"inventory whole … all five shapes, the 8-step derive_backend_shape algorithm",
1C is the runtime census, 1D the skinny proved/disproved table. Those rows do
not exist, so the bulk of the correctness surface CH1 is chartered to verify is
structurally absent. 1F's coherence scan is a cross-document overlay that
*presumes* the per-layer inventories beneath it; it cannot substitute for them.

- **file:line**: `restart/audit/totality/sk-v17/p1/` (directory contains only 1f-*.md)
- **Fix**: Dispatch 1A–1E for SK-V17 T-P1 V2 per §2 scope matrix before the
  CHALLENGE wave can converge. CH1 cannot ACCEPT a cycle whose spec-claim↔impl
  tables for substrate/codegen/runtime/skinny/locks were never written. The
  three 1F outputs are individually sound (see §1–§3 below) but do not meet the
  §3Z convergence floor alone (zero orphan REVISE requires the underlying rows
  to exist).

DISPOSITION: **REJECT** (cycle-level: missing required artefacts).

---

## §1 — 1f-coherence-scan.md spec-claim↔impl table — REVISE

All eight COH17 rows were re-anchored. The substantive divergence mapping —
the tape-substrate vs eager-OpenFrame split, the core-AoS vs skinny-SoA shape,
the per-grammar value-API vs `ValueRef<G>` — is **accurate**:

- COH17-001 AoS-vs-SoA: `crates/core/src/runtime/tape/record.rs:103`
  (`#[repr(C, align(4))] pub struct TapeRec`), 16-byte assert at line 120/121
  (`assert!(core::mem::size_of::<TapeRec>() == 16)`), and the mod doc
  `tape/mod.rs:6-9` ("kept AoS first … the same TapeCursor API rides a later
  SoA split") — **all verbatim-confirmed**. Skinny SoA `Tape<'input>` with
  `offsets: Vec<u32>` at `skinny/crates/runtime/src/tape/mod.rs:94-96` —
  **confirmed**. ACCURATE.
- COH17-002 value-API: `value_from_ref`/`ValueRef` grep over
  `crates/core/src/runtime/json/value.rs` returns **empty** — **confirmed**
  (re-ran). `CssTypedValue` at `css_l4/value.rs:414` — **confirmed**. The
  eager `OpenFrame` builder at `json/builder.rs:9` and `css_l4/builder.rs:16`
  — **confirmed** verbatim. ACCURATE mapping.
- COH17-003 unwired tape / wired scan: `JsonStructBuilder::new()` at
  `json/parse_with.rs:34,44` — **confirmed**; `scan_structural` in
  `generated/json.rs:732` — **confirmed**. ACCURATE.
- COH17-004 CollapsedStage x86: `ARCHITECTURE.md:1088,1186,1206` — **confirmed**
  (1206 carries "aarch64 mechanically refused", `target.arch == x86` +
  `target.avx512bw`). SPEC §9 aarch64-only at `:258,:806,:854` — **confirmed**.
  Mapping accurate.
- COH17-005 / COH17-006 / COH17-007 / COH17-008 base citations — **confirmed**:
  `crates/simd-scan/src/{neon,avx2,avx512,wasm,scalar}.rs` all present;
  `StructLayout` live at `crates/ir/src/registry/struct.rs:202` and consumed at
  `crates/core/src/runtime/tape/mod.rs:185` (`begin_compound(&mut self, layout:
  &StructLayout)`); Lock 2 retirement at `LOCKS.md:160`; FactStream 5th-category
  at `LOCKS.md:100` and `ARCHITECTURE.md:1803`; `StructuralAlphabet` fields at
  `crates/simd-scan/src/alphabet.rs` (`singletons`, `digraph_mask: [u64;4]`,
  `digraph_pairs`, `quote_classes`). ALL ACCURATE.

**Two defects requiring REVISE within this table/file:**

**CH1-V2-002 — `value_from_ref` path missing the `grammars/` segment.**
COH17-002 and the Cross-Tree map (row "Value API") cite
`json/value.rs:143` as the live skinny `value_from_ref` site. The actual path
is `skinny/crates/runtime/src/grammars/json/value.rs:143` (verified: line 143 is
`pub(crate) fn value_from_ref<'doc, 'input: 'doc>(`). The cited short form
`json/value.rs` resolves to **no file** at `skinny/crates/runtime/src/json/`.
The shortform originates in SK-V17 SPEC `:58-60` and p2f `:34`, but CH1's bar is
"no recalled symbol path" — an inventory presenting a path as resolvable live
truth must carry the full `grammars/` segment. The skinny SoA tape file
(`skinny/crates/runtime/src/tape/mod.rs`) IS at the un-`grammars/` path, so the
inconsistency is real and reader-misleading.
- **Fix**: amend COH17-002 + Cross-Tree "Value API" row to
  `skinny/crates/runtime/src/grammars/json/value.rs:143`.

DISPOSITION (§1, file-level): **REVISE** — table content substantively correct;
fix the `grammars/json/value.rs` path and (see §4) the frontmatter count.

---

## §2 — 1f-coherence-scan.md Gaps table css_l4-scan claim — REJECT

**CH1-V2-003 — "css_l4 has NO scan wired" is factually false.**

The Gaps/Missing-Primitives table row states: *"crates/core structural scan has
NO css_l4 alphabet wired (json/ebnf/bnf/csv only) … css_l4 grep absent there."*
This is **wrong**. `crates/core/src/grammar/generated/css_l4.rs` (108,406 LOC)
DOES wire the structural scan:
- `::simd_scan::scan_structural(input, &alphabet)` at **`css_l4.rs:15982`**;
- a full `StructuralAlphabet { singletons, digraph_mask, digraph_pairs,
  quote_classes }` constructed at `css_l4.rs:15976-15981`;
- the `OnceCell<StructuralIndex>` discipline at `css_l4.rs:15936,15952,15972`.

`grep -c scan_structural` over css_l4.rs returns **1** (same as json/ebnf/bnf/csv).
So css_l4 is in the scan-wired set, not absent from it.

This is not a path-precision nit; it inverts the divergence. The inventory's
own COH17-008 frames the core alphabet as "richer (digraphs, quote classes)"
than the proven `[u8;64]` — yet the Gaps row simultaneously asserts CSS isn't
wired at all. Both cannot hold: the css_l4 generated parser uses exactly the
richer `StructuralAlphabet` it claims is unused on CSS. The CORRECT divergence
is: *core's css_l4 scan IS wired (with the rich alphabet), whereas SK-V17 wires
CSS scan in skinny only at W3* — a far stronger fold finding than "core CSS scan
absent."
- **file:line**: 1f-coherence-scan.md Gaps table ("No css_l4 alphabet wired")
  contradicted by `crates/core/src/grammar/generated/css_l4.rs:15982`.
- **Fix**: replace the Gaps row with the corrected fact — core css_l4 IS
  scan-wired via the rich `StructuralAlphabet` (`css_l4.rs:15976-15982`); the
  real gap is the skinny-side CSS scan (W3) + the absent tape consumer. This
  also forces an amendment in §3 below (AP17-002 evidence list omits css_l4).

DISPOSITION (§2): **REJECT** — false impl claim; the row must be rewritten, not
merely re-cited.

---

## §3 — 1f-anti-pattern.md — REVISE

Base citations re-anchored and **confirmed**:
- AP17-001 single tape: `TapeStructBuilder` at `crates/core/src/runtime/tape/mod.rs:58`,
  `TapeRec` AoS at `record.rs:103`, eager builders at `json/builder.rs:9` +
  `css_l4/builder.rs:16` — confirmed. The "UNWIRED, no parallel substrate"
  verdict is sound: `TapeStructBuilder` is consumed only inside `tape/` and the
  live parse paths use `JsonStructBuilder`/`CssStructBuilder` (parse_with.rs:34).
  ACCURATE — the central Lock-1 firewall verdict holds.
- AP17-002 OnceCell sidecar: cited `json.rs:686,702,732` (confirmed verbatim),
  and `ebnf.rs:1335, bnf.rs:802, csv.rs:520` — **all three confirmed** (each is
  the OnceCell-doc line; the field decl follows at ebnf:1350/bnf:817/csv:535).
- AP17-003 god module: `css_l4/builder.rs` = **817 LOC** (confirmed `wc -l`);
  `json/builder.rs` = **231 LOC** (confirmed).
- AP17-004 emitter refs: `substrate.rs:43,55,60,73` — confirmed
  (`builder_path`/`document_path` consumed as DATA, Lock 14 honoured). ACCURATE.
- Renamed-scanner row: `scan_structural(input, &alphabet)` at
  `crates/simd-scan/src/lib.rs:80` — confirmed (signature is
  `pub fn scan_structural(input: &[u8], alphabet: &StructuralAlphabet)`).

**Two defects requiring REVISE:**

**CH1-V2-004 — AP17-002 / AP17-003 omit css_l4 from the OnceCell+builder census.**
AP17-002's evidence enumerates "same shape in ebnf.rs:1335, bnf.rs:802,
csv.rs:520" but omits css_l4, which carries the identical
`OnceCell<StructuralIndex>` at `css_l4.rs:15936`. Consequent to CH1-V2-003, the
anti-pattern scan undercounts the retained-index surface by one grammar (the
largest one). The god-module row (AP17-003) correctly flags the 817-LOC CSS
builder but the OnceCell row's grammar list is incomplete.
- **Fix**: add `css_l4.rs:15936` to AP17-002's evidence; note css_l4 carries
  both the retained index AND the eager `OpenFrame` builder.

**CH1-V2-005 — `pending_*` count: doc/inventory say "nine"/"seven", source has
six Vec + one Option.** AP17-003 states "nine `pending_*` Vecs (:74-79)"; the
core mod doc (`tape/mod.rs:8`) says "seven `pending_*` `Vec<Vec>`". Source truth
at `css_l4/builder.rs`: lines 74-79 carry **six** `pending_* : Vec<…>`
(`pending_rules`, `pending_decls`, `pending_selectors`, `pending_values`,
`pending_blocks`, `pending_components`) plus one `pending_value: Option<…>` at
line 71 — i.e. **6 Vec + 1 Option = 7 `pending_` fields**, none of them a
`Vec<Vec>`. "Nine" overcounts; "Vec<Vec>" mischaracterises (they are flat
`Vec`s, not nested). The substantive point (eager `pending_` slabs SK-V17
PRUNEs) is correct; the number and the `Vec<Vec>` shape are not.
- **Fix**: amend AP17-003 to "seven `pending_` fields (six `Vec`, one `Option`)
  at `css_l4/builder.rs:71,74-79`"; drop the `Vec<Vec>` characterisation or
  attribute it to the mod-doc's own (also imprecise) wording.

DISPOSITION (§3): **REVISE** — firewall verdicts sound; census completeness +
the pending-count must be corrected.

---

## §4 — 1f-coherence-scan.md frontmatter divergence_count — REVISE

**CH1-V2-006 — `spec_claims_implemented: 2` overstates by one.**
Frontmatter declares implemented=2, unimplemented=4, impl-exceeds-spec=2,
unknown=1 (8 rows). Re-tallying the table verdict column: only **COH17-003** is
verdicted `spec-claims-implemented`. The eight rows tally to
implemented=1 (003), unimplemented=4 (001,002,004,006), impl-exceeds-spec=2
(005,008), unknown=1 (007) — total 8. The frontmatter's implemented=2 has no
second supporting row.
- **file:line**: 1f-coherence-scan.md frontmatter `divergence_count:
  spec_claims_implemented: 2`.
- **Fix**: set `spec_claims_implemented: 1`. (Note: this count omits the two
  impl-exceeds-spec rows from any "implemented" framing — confirm the schema
  intends `spec_claims_implemented` to exclude impl-exceeds-spec, which it does
  per §2.1.)

DISPOSITION (§4): **REVISE**.

---

## §5 — 1f-past-corpora.md — ACCEPT

All PC17 ledger citations re-anchored and **confirmed**:
- PC17-001 REDRESS-53: SPEC `:578` ("retained cursor / sidecar event vector; a
  second substrate") and `:837-840` ("L1/L4 index == tape-offsets identity … A
  retained parallel index collapses into REDRESS-53") — **verbatim-confirmed**.
- PC17-002 REDRESS 96/97/98 class-column: `LOCKS.md:129-135,142-144` — confirmed
  (the substrate-ceiling history + the v+1 generalization to all transient
  classifier-state primitives). p2d `:34-49` "six members, one position-keyed
  vector" — confirmed against `skinny/.../tape/mod.rs:94-100`.
- PC17-003 AZ-IV: SPEC `:791-793` ("AZ-IV eager value tree (the 118×
  regression)") — **verbatim-confirmed**; correctly mapped to core's eager
  `CssTypedValue` + `pending_*` (`css_l4/builder.rs:74-79`, `value.rs:414`).
- PC17-004 x86 not target: alphaC `:307-316` + SPEC `:806,:826,:854` — confirmed.
- PC17-005 totality-only names: alphaC `:20-25,:29` (grep-zero for
  `StructLayout`/`OpenFrame`/`CssArena`/`begin_compound`/`TapeStructBuilder` on
  skinny benched surface; Lock 2 retirement) — confirmed.
- PC17-006 D6 second substrate: SPEC `:807-811,:854` — confirmed (the §9 block
  names skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor` as
  forbidden-in-skinny, and the direction-monotonicity note correctly states the
  fold flows skinny→core, never the reverse).

The direction-monotonicity guard and the SK-V14 continuity note (re-anchoring
prior COH-014/COH-008 at current line positions) are accurate. No recalled
citation; every ledger entry resolves.

DISPOSITION (§5): **ACCEPT**.

---

## §6 — Tape-substrate ↔ eager-OpenFrame divergence map accuracy — ACCEPT

CH1's named focus: "the tape-substrate / eager-OpenFrame divergence is
accurately mapped." Verified end-to-end and **the map is correct**:
- The proven SoA substrate (`skinny/.../tape/mod.rs:94` `Tape<'input>` with
  `offsets: Vec<u32>` + sparse `flag_cursors`/`flag_values` + `PayloadArena`)
  vs the core AoS `TapeRec` (16-byte, `record.rs:103/120`) — both surfaces exist
  exactly as catalogued; COH17-001 + Cross-Tree map are accurate.
- The UNWIRED-tape / live-eager-OpenFrame split: `TapeStructBuilder` consumed
  only inside `tape/`; live JSON parse uses `JsonStructBuilder::new()`
  (`parse_with.rs:34`); live CSS uses `CssStructBuilder` with `OpenFrame` +
  `pending_*` (`css_l4/builder.rs:16,71-79`) — confirmed. COH17-003 + the CH5
  firewall verdict (1f-anti-pattern §"CH5 Firewall Verdict") are accurate: no
  Track-1≡Track-2 dishonesty inside core; the tape is dormant, not a sidecar.
- The lazy-`ValueRef<G>` vs per-grammar-eager-enum value-API divergence is
  correct (grep-empty on core `value_from_ref` re-confirmed).

The map's substance is sound; the only corrections (CH1-V2-002 path, CH1-V2-003
css_l4-scan, CH1-V2-006 count) do not touch the tape/OpenFrame divergence
itself, which is the load-bearing fold finding and is accurately rendered.

DISPOSITION (§6): **ACCEPT**.

---

## §7 — SKINNY-proven facts cited correctly — ACCEPT

CH1's named focus: "SKINNY-proven facts cited correctly." Re-anchored the
SKINNY-side citations underpinning the fold map:
- `Tape<'input>` SoA + `offsets: Vec<u32>` — `skinny/crates/runtime/src/tape/mod.rs:94-96`. ✓
- `ValueRef<'doc,'input,K,G>` grammar-parametric — `skinny/.../tape/mod.rs:175`. ✓
- `select_classifier(alphabet: &'static [u8; 64])` — `skinny/crates/bbnf-simd/src/dispatch.rs:42`. ✓
- `value_from_ref` — `skinny/crates/runtime/src/grammars/json/value.rs:143`
  (cited short-path corrected in CH1-V2-002). ✓ (symbol correct; path segment fix)
- The 6-wave LOCK + the SPEC §9 pre-block ledger (REDRESS-53, AZ-IV, D6,
  x86/AVX/SVE) — all resolve (see §5). ✓
- `FieldSource`/`StructLayout` in `crates/ir/src/registry/struct.rs:84,202` —
  the fold-target layout recipe — confirmed (this is the totality-tree analogue
  the map ties to skinny `BackendRule`). ✓

Every SKINNY-proven symbol the inventories lean on resolves at source. The lone
path-segment defect is dispositioned under §1/CH1-V2-002.

DISPOSITION (§7): **ACCEPT**.

---

## Disposition Summary

| § | Subject | Disposition | Finding |
|---|---|---|---|
| §0 | Inventory census (1A–1E absent) | **REJECT** | CH1-V2-001 |
| §1 | 1f-coherence spec↔impl table | **REVISE** | CH1-V2-002 (value_from_ref path) |
| §2 | 1f-coherence Gaps css_l4-scan | **REJECT** | CH1-V2-003 (false "no css_l4 scan") |
| §3 | 1f-anti-pattern | **REVISE** | CH1-V2-004 (css_l4 omitted), CH1-V2-005 (pending count) |
| §4 | 1f-coherence frontmatter count | **REVISE** | CH1-V2-006 (implemented=2→1) |
| §5 | 1f-past-corpora | **ACCEPT** | — |
| §6 | Tape↔OpenFrame divergence map | **ACCEPT** | — |
| §7 | SKINNY-proven citations | **ACCEPT** | — |

**Counts**: ACCEPT 3 · REVISE 3 · REJECT 2 (CH1-V2-001 census, CH1-V2-003
css_l4-scan).

**Verdict**: The three 1F inventories that exist are citation-disciplined and
the central tape-substrate/eager-OpenFrame fold divergence is accurately
mapped (§6, §7 ACCEPT). However CH1 cannot pass the cycle: (1) the spec↔impl
correctness floor for substrate/codegen/runtime/skinny/locks is structurally
absent — 1A–1E were never written (REJECT); (2) one impl claim is factually
inverted — core's css_l4 IS scan-wired (REJECT). The two REJECTs plus three
REVISE dispositions block §3Z convergence for V2.
