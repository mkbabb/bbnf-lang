---
lens: CH1-CORRECTNESS
pass: T-P1-excavation
cycle: V3
subject: SK-V17 T-P1 EXCAVATION
generated_at: 2026-05-30T00:30:00Z
master_head: 445925167154de73540e3ea3283d0170371de790
reviewer_method: "Read PASS-1-EXCAVATION.md §3 CH1 + ORCHESTRATOR §3W; Read prior V2/CH1.md to carry continuity; Read the three CH1-scope V3 inventories (1a-substrate, 1c-runtime, 1d-skinny-lessons) end-to-end + cross-checked 1f triad continuity; re-anchored every load-bearing file:line via sed/grep over crates/core, crates/ir, crates/simd-scan, skinny/crates, restart/{ARCHITECTURE,MASTER-PLAN}.md, restart/locks/LOCKS.md, restart/skinny/tranches/sk-v17/{SPEC.md,research/alpha/*}; no recalled LOC, every claim re-anchored at source"
inventories_reviewed:
  - restart/audit/totality/sk-v17/p1/1a-substrate-evidence.md
  - restart/audit/totality/sk-v17/p1/1c-runtime-evidence.md
  - restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md (continuity, V2-dispositioned)
prior_cycle_dispositions_carried:
  - CH1-V2-001 (five inventories absent) — RESOLVED: 1A-1E now produced (V3)
  - CH1-V2-002 (value_from_ref grammars/ path) — RESOLVED: all V3 inventories carry full grammars/ segment
  - CH1-V2-003 (css_l4 scan FALSE-unwired) — RESOLVED: V3 inventories correctly assert css_l4 IS scan-wired
  - CH1-V2-004 (OnceCell census omits css_l4) — RESOLVED: 1C/1A/1D enumerate all 8
  - CH1-V2-005 (pending count) — RESOLVED: corrected to six Vec + one Option
  - CH1-V2-006 (divergence_count overstated) — see §4
disposition_counts:
  accept: 4
  revise: 2
  reject: 0
sections_dispositioned: 6
---

## CH1 CORRECTNESS — Scope + Method

CH1 verifies, for the V3 cycle: every spec-claim ↔ implementation row resolves
at the cited file:line/SHA; the tape-substrate / eager-OpenFrame divergence is
accurately mapped; SKINNY-proven facts are cited correctly. No recalled LOC,
no recalled symbol path. V3 closes the V2 structural REJECT (CH1-V2-001:
1A–1E were never produced); all six 1X inventories now exist. CH1's V3 scope
focuses on the three correctness-floor inventories the lens is chartered for:
**1A substrate**, **1C runtime census**, **1D skinny-lessons** (RESULTS/REDRESS
citation resolution). The 1F triad was V2-dispositioned and its V3 carry is
checked for continuity only.

Master HEAD confirmed `445925167154de73540e3ea3283d0170371de790`.

**Verdict in brief.** The V3 inventories are strongly citation-disciplined.
Of ~95 distinct file:line/SHA citations I re-anchored across 1A/1C/1D, the
overwhelming majority resolve verbatim — including every SKINNY-proven symbol,
every core fold-target line, the 8-grammar scan census (each at its exact line),
the StructLayout=960 count, the Lock 1/2/10/14 anchors, the RESULTS Track-1>sonic
rows, the 24-row broadcast count, and the alphaC/alphaD/SPEC corpus refs. The
tape-substrate ↔ eager-OpenFrame divergence map and the SKINNY-proven citations
are accurate (§5, §6 ACCEPT). Two REVISE defects remain, both in 1C (one shared
with 1D): a fabricated `*ChildrenIter` symbol path, and a load-bearing
"HAND-WRITTEN" mischaracterisation of files that are `@generated` — which also
silently inverts a Lock-14 verdict. No REJECT: no inverted impl claim, no
unresolved-recalled load-bearing fact.

---

## §1 — 1A substrate-evidence.md spec-claim↔impl table — ACCEPT

All eight SUB17 rows + the Cross-Tree map + the Firewall table + the four
Open-Question rows were re-anchored. Every load-bearing citation resolves
verbatim:

- **SUB17-001** SoA `Tape<'input>` six members — `skinny/crates/runtime/src/tape/mod.rs:94-100`
  (`source`, `offsets: Vec<u32>`, `flag_cursors: Vec<u32>`, `flag_values: Vec<u8>`,
  `payloads: PayloadArena`, `id`) + `from_offsets` `debug_assert_eq!` at `:104-117`
  — **verbatim-confirmed**. Lock 1 `LOCKS.md:75`, ARCH `:1088` — confirmed.
- **SUB17-002** core AoS `TapeRec` — `record.rs:103` (`pub struct TapeRec`),
  16-byte+align-4 const-asserts at `:120-121`, mod-doc "kept AoS first … later
  SoA split" at `mod.rs:6-9` — **all verbatim-confirmed**. `TapeStructBuilder`
  at `tape/mod.rs:58` — confirmed.
- **SUB17-003** UNWIRED: `TapeStructBuilder` grep-zero outside `tape/`
  (`grep -rln TapeStructBuilder crates/core/src/ | grep -v /tape/` = empty,
  re-ran) — **confirmed**. The lone non-`tape/` hit is the doc-comment mention
  at `backend/rust/emitter/shapes/number.rs:17` (`//! leaf carries
  [TapeRec::PAYLOAD_F64_DIRECT_BIT]`), correctly flagged as a mention not a
  usage — **confirmed**. Live JSON `JsonStructBuilder::new()` at
  `json/parse_with.rs:34` — confirmed.
- **SUB17-004** core has no `ValueRef`/`value_from_ref` in `json/value.rs`
  (grep empty); `CssTypedValue<'p>` at `css_l4/value.rs:414` — **confirmed**.
- **SUB17-005** CollapsedStage x86-pinned + aarch64 UNKNOWN-2D-05 at
  `ARCHITECTURE.md:1206` — **verbatim-confirmed** (line carries
  `target.arch == x86 + target.avx512bw`, "aarch64 mechanically refused",
  "aarch64 candidate is UNKNOWN-2D-05"). SPEC :854 D6 REJECTed,
  :807-811 forbidden-in-skinny names — confirmed.
- **SUB17-006** `StructLayout` Lock-2-retired (`LOCKS.md:160`) yet live at
  `crates/ir/src/registry/struct.rs:202`; `grep -rn StructLayout crates/`
  = **960** (re-ran, exact) — **confirmed**. `begin_compound(&StructLayout)`
  at `crates/core/src/runtime/tape/mod.rs:185` — confirmed.
- **SUB17-007** richer `StructuralAlphabet` (`singletons`/`digraph_mask:[u64;4]`/
  `digraph_pairs`/`quote_classes`) at `crates/simd-scan/src/alphabet.rs:19-37`;
  quote_classes JSON/CSS-motivated doc `:33-37` — **verbatim-confirmed**. The
  "config-breadth not proof-breadth" framing is the correct CH2-aligned reading.
- **SUB17-008 / SUB17-009** two simd crates + the StructRegistry per-leaf fence
  (`struct.rs:84,313,331`; `begin_compound` reads `layout.rule_id & 0x1F` at
  `tape/mod.rs:186`) — **confirmed**.

The frontmatter `divergence_count` (implemented=2, unimplemented=4,
impl-exceeds-spec=2, unknown=2) tallies correctly against the verdict column:
SUB17-001 + SUB17-003 (implemented, the latter UNWIRED-confirmed); SUB17-002/004/005/006
(unimplemented); SUB17-007/008 (impl-exceeds-spec); U-SUB17-001/002 (unknown).
The V2 overstatement (CH1-V2-006) does not recur here.

The prior-cycle fold ledger (CH1-V2-001/002/003, CH5-S0/S4/S8, CH3-R1/R2) is
correctly cited as resolved. The SK17 per-leaf-lookup fence (SUB17-009) is the
correct carry of CH3-R1.

DISPOSITION (§1): **ACCEPT** — every row resolves; counts tally; the prior
dispositions are folded with accurate source anchors.

---

## §2 — 1A SKINNY-proven citations + tape↔OpenFrame map — ACCEPT

CH1's named focus, against 1A. The SKINNY-proven inputs all resolve at source:

- `Tape<'input>` SoA + `offsets: Vec<u32>` — `skinny/.../tape/mod.rs:94-96`. ✓
- `ValueRef<'doc, 'input: 'doc, K = AnyKind, G: EventGrammar = AnyGrammar>` —
  `skinny/.../tape/mod.rs:175` (1A's `ValueRef<'doc,'input>` shorthand is a
  faithful elision of the full generic list; the symbol + line are exact). ✓
- `value_from_ref` — `skinny/crates/runtime/src/grammars/json/value.rs:143`
  (`pub(crate) fn value_from_ref<'doc, 'input: 'doc>(`) — full `grammars/`
  segment carried throughout 1A (CH1-V2-002 RESOLVED). ✓
- `select_classifier(alphabet: &'static [u8; 64])` —
  `skinny/crates/bbnf-simd/src/dispatch.rs:42`. ✓

The tape-substrate ↔ eager-OpenFrame divergence map (Cross-Tree table + the
Substrate-Union Firewall) is accurate end-to-end: the dead AoS `TapeStructBuilder`
vs the live eager `OpenFrame` builders (`json/builder.rs:9`, `css_l4/builder.rs:16`),
the `OnceCell<StructuralIndex>` scan-cache scoped to all 8 carriers, the no-
cross-call-classifier-state Lock-1-v+1 check, and the per-leaf-registry fence
are all rendered correctly. No Track-1≡Track-2 dishonesty asserted; the tape is
correctly catalogued dormant, not a sidecar.

DISPOSITION (§2): **ACCEPT**.

---

## §3 — 1C runtime-evidence.md — REVISE

Base citations re-anchored and **confirmed**:

- **8-grammar scan census** — every line number resolves exactly: json:732,
  css_l4:15982, bbnf:4843, bnf:848, csv:566, ebnf:1381, css_pretty:1905,
  google_sheets:3559 (each is `::simd_scan::scan_structural(input, &alphabet)`,
  grep-verified one per file). The `math.rs`/`mod.rs` exclusion (0 scan) is
  correct — they are not generated grammar parsers. **ACCURATE.**
- `css_l4/parse_with.rs:34` = `CssStructBuilder::new()` — confirmed.
- `json/value.rs` symbols `JsonValue:4 / JsonNumber:13 / JsonPair:29 /
  JsonArray:34 / JsonObject:38` — **all verbatim-confirmed**.
- `substrate.rs:43,55` data-bind `rust.builder_path`/`rust.document_path` from
  `EmitStrategy::StructDirect` — confirmed; Lock-14 data-bind verdict sound.
- builder LOC 231 (json) / 817 (css_l4) — confirmed `wc -l`.
- `tape/mod.rs:55` "dispatches on the [StructLayout] handed to begin_compound,
  never on per-grammar route strings" — confirmed verbatim.
- RT17-005 OnceCell doc lines (bbnf:4797, bnf:802, css_l4:15936, css_pretty:1859,
  csv:520, ebnf:1335, google_sheets:3513, json:686) + json field :701 — confirmed.
- RT17-007 BackendShape canon `ARCHITECTURE.md:31` (`5-shape canon {EagerTape,
  OffsetTape, EventTape, SinkOnly, CollapsedStage}`) + :1114 (`CollapsedStage,`
  enum variant) + :1206 UNKNOWN-2D-05 — confirmed; the runtime-carries-no-shape
  finding (grep-empty) is correct.

**Two defects requiring REVISE:**

**CH1-V3-001 — RT17-004 cites a fabricated `JsonChildrenIter view.rs:39` /
`CssChildrenIter view.rs:44`.** RT17-004's evidence column lists
"`JsonChildrenIter` view.rs:39, `CssChildrenIter` view.rs:44" among the
grammar-named symbols. Neither symbol exists:
`grep -n 'ChildrenIter' crates/core/src/runtime/view.rs` returns **empty**;
`view.rs` is only **76 lines**; lines 39 and 44 are doc-comments
(`/// focuses whose span is a union of disjoint child spans.` and a bare `///`).
This is precisely the recalled-symbol-path failure CH1 is chartered to catch.
The substantive Lock-14 verdict (grammar-named symbols live only under
`runtime/<g>/` dirs) is independently supported by the real symbols
(`JsonStructBuilder`, `CssStructBuilder`, `CssTypedValue`), so the verdict
survives — but the fabricated citation must be struck.
- **file:line**: `1c-runtime-evidence.md` RT17-004 evidence column
  ("JsonChildrenIter view.rs:39 / CssChildrenIter view.rs:44") contradicted by
  `crates/core/src/runtime/view.rs` (76 LOC, grep-empty for `ChildrenIter`).
- **Fix**: strike the `*ChildrenIter view.rs:39/44` pair; if a view-side
  grammar-named iterator is wanted as evidence, cite a real symbol at its real
  line (`runtime/<g>/view.rs` if one exists) or drop the row's view example and
  rely on the confirmed builder/value symbols. Also correct the `substrate.rs:62`
  doc-example citation: the `::bbnf::runtime::css_l4::CssStructBuilder` example
  string is at `substrate.rs:60`, not :62 (line 62 is the `[TokenStream]`
  continuation) — off-by-two; the symbol is real, the line is wrong.

**CH1-V3-002 — "HAND-WRITTEN per-grammar value/view modules" is factually wrong
and silently inverts the Lock-14 verdict.** RT17-002, RT17-004, and the Exec
Summary repeatedly describe the core per-grammar value/view modules as
"per-grammar HAND-WRITTEN eager value enums" / "per-grammar HAND-WRITTEN eager
value/view modules." The files carry the opposite truth:
`crates/core/src/runtime/json/value.rs:1` = `// @generated by xtask regen-json;
do not edit by hand.`; `css_l4/value.rs:1` = `// @generated by xtask regen-css;
do not edit by hand.`; both builders likewise (`json/builder.rs:1`,
`css_l4/builder.rs:1` = `@generated by xtask regen-*`). They are
**template-emitted, not hand-written.** This is load-bearing for Lock 14:
LOCKS.md:349 states "Per-grammar runtime modules (value, document, view, kind)
are emitted from a single grammar-agnostic generator template … hand-written
per-grammar runtime files are forbidden." A genuinely hand-written per-grammar
runtime file would be a **Lock-14 VIOLATION**, not "Lock 14 honoured" — so the
RT17-004 verdict ("Lock 14 honoured / implemented") is correct ONLY because the
files are generated, which is exactly what 1C denies. The inventory asserts the
honoured-verdict while simultaneously labelling the cause as the forbidden
hand-written shape; the two cannot both stand. The substantive divergence 1C
means to capture (eager-enum value-API vs lazy `ValueRef`) is correct and
unaffected.
- **file:line**: `1c-runtime-evidence.md` RT17-002 / RT17-004 / Exec-Summary
  "HAND-WRITTEN" contradicted by `crates/core/src/runtime/json/value.rs:1`
  (`@generated by xtask regen-json`), `css_l4/value.rs:1`, `*/builder.rs:1`,
  and reconciled against `LOCKS.md:349`.
- **Fix**: replace "HAND-WRITTEN" with "template-emitted / `@generated`
  (per-grammar value/view/builder, `@generated by xtask regen-*`)" throughout
  1C; restate the divergence as "the GENERATED per-grammar value API is an eager
  typed enum, not the lazy `ValueRef<G>` projection" — the fold target is the
  GENERATOR (it must emit a `ValueRef` projection), not a hand-edit of emitted
  files. The Lock-14-honoured verdict then reads consistently (generated ⇒
  honoured).

The Lock-14 firewall verdict itself (RT17-004) is otherwise sound; only the
"hand-written" cause must be corrected to "generated."

DISPOSITION (§3): **REVISE** — census + line citations overwhelmingly sound;
strike the fabricated `*ChildrenIter` symbol (CH1-V3-001), fix the
substrate.rs:60-not-62 line, and correct the "hand-written"→"generated"
mischaracterisation that inverts the Lock-14 cause (CH1-V3-002).

---

## §4 — 1D skinny-lessons.md RESULTS/REDRESS + corpus citations — REVISE

CH1's named focus per §3 ("RESULTS-row and REDRESS-entry citations in 1D
resolve to real entries"). Re-anchored:

- **RESULTS Track-1>sonic proof** `skinny/RESULTS.md:5-55` — confirmed: twitter
  parse_only 8349.290 > sonic 4913.095, citm/canada/direct/typed rows all
  carry `Track 1 > sonic … per-iter equality PASS`. The standing-proof framing
  is accurate. ✓
- **24-row broadcast** `skinny/RESULTS.md:112-135` — `grep -c` over the slice =
  **24** (exact). The PERMANENT-PRE-BLOCK (L-SK17-04) is correctly anchored. ✓
- `TapeBuilder` at `assembler.rs:42`, `push_plain_offset` at `assembler.rs:71`
  — **confirmed**. `PayloadArena` at `tape/mod.rs:38` — confirmed (1D's `:38`
  cite is `pub struct PayloadArena`; the SoA fields it lists at `:96-99` are
  exact: offsets:96, flag_cursors:97, flag_values:98, payloads:99). ✓
- skinny `BackendShape` 5-shape match at `cost.rs:119-121`
  (`for_backend_shape` → `EagerTape | OffsetTape | EventTape`) — confirmed. ✓
- `W5C_REQUEST_FACT_PROFILES` at `skinny/crates/codegen/src/lib.rs:336` — confirmed. ✓
- **Corpus refs**: alphaC §1 AZ-IV-118x (`:50` header), §2 registry-indirection
  (`:98` header, "28-65x; 983x; 10583x WATCHDOG"), §6 x86/AVX (`:307` header);
  alphaD I5 (`:104`, AZ-IV pre-block); SPEC :794-795 (registry pre-block +
  "No registry lookup in the per-leaf hot path"), :110-114 (foldable-into-totality,
  "sheets_witness has no BackendRule shape"), :807-811, :854, :825 (REDRESS-53),
  :578 — **all resolve verbatim**. ✓
- Lock 14 at `LOCKS.md:349` (full grammar-generalisation) — confirmed; SK17L-009's
  `:349` anchor is correct. FactStream 5th category `LOCKS.md:100` — confirmed.

The JSON-empirical vs grammar-neutral split (CH2-aligned) is accurate; the
do-not-redrive ledger (L-SK17-01…07) maps each pre-block to its measured
refutation with a real corpus anchor. The frontmatter count (implemented=1,
unimplemented=4, impl-exceeds-spec=2, unknown=2) tallies against the verdict
column.

**One defect requiring REVISE (shared root with CH1-V3-002):**

**CH1-V3-003 — SK17L-002 + Exec-Summary "per-grammar hand-written eager value
enums".** 1D inherits the same mischaracterisation as 1C: SK17L-002's impl
column says "crates/core has per-grammar hand-written eager value enums," and
SK17L-003 references "`CssTypedValue` eager enum." The value/builder files are
`@generated by xtask regen-*` (`json/value.rs:1`, `css_l4/value.rs:1`,
`css_l4/builder.rs:1`), not hand-written. The eager-vs-lazy divergence is
correct; the "hand-written" attribute is not.
- **file:line**: `1d-skinny-lessons.md` SK17L-002 impl column + Exec Summary
  contradicted by `crates/core/src/runtime/json/value.rs:1` /
  `css_l4/value.rs:1` (`@generated by xtask regen-*`).
- **Fix**: change "hand-written" → "GENERATED (template-emitted, `@generated by
  xtask regen-*`)"; the SK-V18 fold target is the GENERATOR emitting a lazy
  `ValueRef<G>` projection in place of the eager enum, not a hand-edit of the
  emitted files. Keep the divergence verdict (unimplemented / lazy-ValueRef fold).

Note: this is a single shared root defect surfaced in two inventories (1C +
1D); folding it requires both authors to correct in lockstep (the eager-enum
value API is `@generated`, the fold acts on the generator). Not an orphan —
both carriers are named.

DISPOSITION (§4): **REVISE** — RESULTS/REDRESS/corpus citations all resolve;
correct the shared "hand-written"→"generated" attribute in SK17L-002/Exec.

---

## §5 — Tape-substrate ↔ eager-OpenFrame divergence map accuracy (cross-inventory) — ACCEPT

CH1's named focus, audited across 1A + 1C + 1D as a single coherent map:

- The proven SoA substrate (`skinny/.../tape/mod.rs:94` `Tape<'input>` +
  `offsets: Vec<u32>` + sparse flags + `PayloadArena`) vs the core AoS `TapeRec`
  (16-byte const-asserted, `record.rs:103,120`) — rendered identically and
  correctly in 1A SUB17-001/002, 1C RT17-001, 1D SK17L-001.
- The UNWIRED-tape / live-eager-OpenFrame split: `TapeStructBuilder` grep-zero
  outside `tape/`; live JSON `JsonStructBuilder` (`json/parse_with.rs:34`); live
  CSS `CssStructBuilder` + `OpenFrame` + six `pending_*` Vecs + one
  `pending_value: Option` (`css_l4/builder.rs:16,71,74-79`) — confirmed across
  all three inventories with the corrected six-Vec+one-Option count (CH1-V2-005
  RESOLVED; the core mod-doc's own "seven pending_* Vec<Vec>" at `tape/mod.rs:8`
  is correctly flagged as the doc's own imprecision, not the source truth).
- The scan-wired-all-8 / tape-unwired asymmetry — confirmed (8 scan sites at
  exact lines; CH1-V2-003 css_l4-FALSE-unwired RESOLVED — all three V3
  inventories correctly assert css_l4 IS scan-wired with the rich alphabet).

The load-bearing fold finding is accurately rendered everywhere it appears.

DISPOSITION (§5): **ACCEPT**.

---

## §6 — Prior-cycle disposition fold integrity — ACCEPT

V3 closes the V2 structural REJECT (CH1-V2-001): all six 1X inventories now
exist (`ls restart/audit/totality/sk-v17/p1/` = 1a..1f). The five prior REVISE/
REJECT dispositions are folded with accurate source anchors:

- CH1-V2-002 (value_from_ref `grammars/` segment) — RESOLVED: 1A/1C/1D all carry
  `skinny/crates/runtime/src/grammars/json/value.rs:143`; 1C even calls out "the
  `grammars/` segment is load-bearing; the short `json/value.rs:143` form
  resolves to no file." ✓
- CH1-V2-003 (css_l4 scan FALSE-unwired) — RESOLVED: 1A SUB17-008/Gaps, 1C
  RT17-003/Gaps, 1D SK17L-008 all correctly assert css_l4 IS scan-wired
  (`css_l4.rs:15976-15982`) and rewrite the gap to "missing TAPE CONSUMER, not
  the scan." ✓
- CH1-V2-004 (OnceCell census omits css_l4) — RESOLVED: all three enumerate 8
  carriers. ✓
- CH1-V2-005 (pending count) — RESOLVED: six Vec + one Option. ✓
- CH1-V2-006 (divergence_count overstated) — does not recur; 1A/1C/1D counts
  tally against their verdict columns. ✓

No prior disposition is mis-folded or left orphan. The fold is complete and
source-anchored, not paper-folded.

DISPOSITION (§6): **ACCEPT**.

---

## Disposition Summary

| § | Subject | Disposition | Finding |
|---|---|---|---|
| §1 | 1A spec↔impl table + counts | **ACCEPT** | — |
| §2 | 1A SKINNY citations + tape↔OpenFrame map | **ACCEPT** | — |
| §3 | 1C runtime census | **REVISE** | CH1-V3-001 (fabricated `*ChildrenIter` view.rs:39/44 + substrate.rs:60-not-62); CH1-V3-002 (HAND-WRITTEN→generated, inverts Lock-14 cause) |
| §4 | 1D skinny-lessons RESULTS/REDRESS/corpus | **REVISE** | CH1-V3-003 (shared HAND-WRITTEN→generated in SK17L-002/Exec) |
| §5 | Tape↔OpenFrame divergence map (cross-inventory) | **ACCEPT** | — |
| §6 | Prior-cycle disposition fold integrity | **ACCEPT** | — |

**Counts**: ACCEPT 4 · REVISE 2 · REJECT 0.

**Verdict**: V3 is a decisive improvement over V2. The structural REJECT
(1A–1E absent) is closed; all six prior dispositions are folded with accurate
source anchors; the tape-substrate ↔ eager-OpenFrame divergence map and every
SKINNY-proven citation resolve verbatim (§1, §2, §5, §6 ACCEPT). Two REVISE
defects remain, sharing one root: (CH1-V3-001) 1C cites a fabricated
`Json/CssChildrenIter view.rs:39/44` that does not exist in the 76-line
`view.rs` (plus a substrate.rs:60-not-62 off-by-two); (CH1-V3-002 / CH1-V3-003)
both 1C and 1D label the core per-grammar value/builder modules "HAND-WRITTEN"
when they are `@generated by xtask regen-*` — which silently inverts the Lock-14
cause (a hand-written per-grammar runtime file is a Lock-14 *violation*, not the
"honoured" verdict 1C records). The eager-enum-vs-lazy-`ValueRef` divergence the
two inventories capture is itself correct and unaffected; only the "hand-written"
attribute and the one fabricated symbol need correction. No REJECT: no inverted
impl claim, no unresolved load-bearing recall. With CH1-V3-001/002/003 folded,
CH1's correctness floor is met for SK-V17 T-P1 V3.
