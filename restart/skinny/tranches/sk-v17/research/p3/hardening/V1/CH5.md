# SK-V17 S-P3 CHALLENGE — CH5 HIDDEN-COUPLING (V1)

Lens: CH5 HIDDEN-COUPLING. Cycle: V1. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/SPEC.md` + `research/p3/{p3a..p3f}.md`.
Focus (per PASS-3 §3 CH5 + ORCHESTRATOR §3W): no wave introduces a parallel
substrate / sidecar producer / renamed scanner (Lock 1) / retained cursor / aux
density table / sidecar event vector / Track 1 ≡ Track 2 dishonesty. Tape +
projection ONE substrate. L8 flag = `BackendRule` branch-tag, NOT an aux table.
Master HEAD `f87ee713a`.

Disposition vocabulary: ACCEPT / REVISE / REJECT. Counts at §3.

---

## §0 — Method + source-of-truth verification (greps at HEAD)

CH5 is a substrate-cardinality lens. I verified every load-bearing substrate claim
the SPEC keys its Lock-1 gates on, against the benched skinny tree:

- **`emit_fact_stream(input:&str) -> Result<String, CssFactError>`** present at
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:5` —
  the String admission plane the SPEC §0.1 row 2 / §9 retires. CITATION HONEST.
- **`track1_facts(input:&str) -> Result<String,String>`** at
  `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:596` — the Track 1 String the SPEC
  retires. CITATION HONEST (SPEC §0.1 row 2 `nonjson_css_l4.rs:596`).
- **Zero tape symbols in CSS path:** `grep -rl 'TapeBuilder|crate::tape|ValueRef'`
  over `grammars/css_l4_*` returns EMPTY at HEAD. The W1 tape-activation gate (grep
  non-zero) is therefore a REAL falsifiable transition, not a no-op. CONFIRMED.
- **One substrate exists:** `TapeBuilder` (single, non-generic) `assembler.rs:42`;
  `push_plain_offset` `:71`; `reserve_offsets_cold` `:89`; sparse `flag_cursors`/
  `flag_values` `:45-46`, `mod.rs:97-98`; `flags_at` binary-search `mod.rs:144-150`;
  `ValueRef<'doc,'input,K,G:EventGrammar=AnyGrammar>` `mod.rs:175`; `PayloadArena`
  `mod.rs:38`; `CapacityPlan::OneShotSimd` `assembler.rs:14-17`. Every L2/L3/L7/L8
  owner-path symbol resolves; none is a new type. CONFIRMED.
- **L8 flag is a branch-tag, not a catalogue:** `OffsetFlags(pub u8)` `mod.rs:18`
  with `GRAMMAR_BIT0=0x01`/`GRAMMAR_BIT1=0x02` `mod.rs:22-23` — generic grammar
  branch-tag bits over the EXISTING sparse pair, NOT a per-rule table. The S-P2 §6.2
  condition (flag = `BackendRule` branch-tag projection) is satisfiable by the actual
  mechanism. CONFIRMED.
- **`W5C_REQUEST_FACT_PROFILES`** at `codegen/src/lib.rs:299/336/567/611` — exactly
  the cited lines. CITATION HONEST.
- **Track 2 is structurally distinct:** `oracle_facts` / `OracleParser`
  `nonjson_css_l4.rs:624-633` is an independent reference parser, NOT a Track-1
  sidecar. The §0.2 comparator table's "Track 2 / oracle ... structurally distinct
  from Track 1 (Lock 1, CH5)" is HONEST — no Track 1 ≡ Track 2 dishonesty.
- **No real second substrate in skinny:** `grep 'StructLayout|TapeStructBuilder|
  TapeCursor|UnionTape'` hits ONLY guard/lint machinery
  (`lock14_baseline.rs:618/2403-2404/2767/5081/5093`, `codegen/src/lib.rs:1355`
  forbidden-name list, `report.rs:2275` "public UnionTape" pre-block). These are the
  ANTI-second-substrate enforcement, not constructs. The substrate cardinality is one.

The SPEC is, on its central CH5 axis, the most substrate-honest S-P3 packet I can
construct a counter-case against. The Lock-1 union is asserted at §1 (No-1), §0.1
row 2, §0.2 comparator table, §0.4 telemetry (`tape_activated`), §9 global blocks +
per-wave + binding conditions 1-2-3, and every wave's pre-blocked-routes list. The
REDRESS-140 differential (substrate cardinality one; the index IS the tape `offsets`)
is the explicit admissibility seam over the RETIRED REDRESS 96/97/98 union-substrate
thesis (P3-E §1, §2 W1/W3). I could not find a parallel-substrate, retained-cursor,
sidecar-event-vector, or aux-density-table escape in any wave of the SPEC.

---

## §1 — Per-section disposition (SPEC.md — the load-bearing contract)

### §0.1 close condition — ACCEPT
Row 2 names ONE substrate explicitly ("NO new cursor/builder type — the EXISTING
`Tape`/`ValueRef`/`TapeBuilder` is the only substrate (Lock 1)"). Row 3 routes
projection over "the SAME `BackendRule` shape ... over the existing `Tape`/`ValueRef`".
Row 9 forbids a retained classifier and constrains the NEON leaf to a transient
`Vec<u32>` the tape consumes. No coupling escape. SPEC.md:53, :58-60, :96-101.

### §0.2 comparator classes — ACCEPT
The table marks "Track 2 / oracle ... structurally distinct from Track 1 (Lock 1,
CH5)" (SPEC.md:120) — verified against `oracle_facts`/`OracleParser`. The strict
admission predicate (`css_comparator_plane=full-cssom`, equality-before-speed) gives
no room for a fact-stream-as-comparator Track1≡Track2 collapse.

### §0.3 / §0.4 outcome + telemetry — ACCEPT
`tape_activated` keyed to `PayloadArena` write/alloc counters, NOT a `crates/core`
grep (SPEC.md:169) — the wrong-tree dishonesty is foreclosed. `lazy_view_generated`
keyed to "accessor generator emits ... over BackendRule" — projection rides the
substrate, no sidecar producer.

### §1 non-negotiables (Lock 1) — ACCEPT
SPEC.md:224-230 is the canonical CH5 clause: "no second tape, no `StructLayout`/
`TapeStructBuilder`/`TapeCursor`, no `UnionTape`, no public substrate API, no
parser-owned structural cursor/facts, no sidecar event vector, no aux density/
projection table, no retained cursor/list, no parallel source pass, no cross-call
classifier-state retention. A SIMD mask stream is a transient producer, not a
retained sidecar; if structural offsets are retained, the structural projection IS
the tape." This is exhaustive over the CH5 escape set. ACCEPT.

### §3 W0 (baseline) — ACCEPT
No behavior LOC; no substrate touched. Pre-blocks the 24-row broadcast (one tuple →
N rows, the Track1-fabrication coupling) and the fact-stream comparator. No
hidden-coupling surface.

### §4 W1 (tape activation) — ACCEPT
"it does not add a parallel tape path beside the String" (SPEC.md:386). Entry gate
requires CHALLENGE accept that W1 "introduces no second substrate (Lock 1)"
(:418). Exit gate: "No second substrate; no parser-owned cursor; exactly one
retained tape survives" (:448). `push_plain_offset` = "one branchless u32 write into
the EXISTING `offsets`" (:429). The `offsets.len()` checkpoint / truncate rollback
forbids `split_off`/`Vec<Vec>` (:457). The L8 sparse pair is "USED for L8, not
widened" (:479). No coupling escape.

### §5 W2 (projection generator) — ACCEPT
Entry gate (SPEC.md:489-491) makes CHALLENGE accept the two CH5 binding conditions
verbatim: "L8 flag bits are `BackendRule` branch-tag projections, NOT a hand-curated
per-rule catalogue"; "the L1/L4 index, when consumed, IS the tape's `offsets`, never
a parallel retained vector". Exit gate task 2: "node kind recovered from the source
byte at the offset (no stored tag)" — no aux tag table. Pre-blocked routes (:522-525)
name "retained cursor / aux density / sidecar event vector; a second substrate;
relocating per-rule branching into projection DATA". The "no new cursor/builder type"
constraint (:497) is restated. ACCEPT.

### §6 W3 (NEON structural index) — ACCEPT
Entry gate (SPEC.md:569-570): CHALLENGE accept "the `Vec<u32>` index IS the tape's
`offsets`". L6 carry is "i32 `depth_carry` threaded WITHIN a single
`scan_components_to_index` call, init-0-per-parse, never retained" (:580) — a
transient producer, not a retained sidecar. L5 mask "1-bit carry threads WITHIN one
block sequence" (P3-A S5). Pre-blocks "cross-call classifier-state retention; a
retained index vector parallel to the tape (REDRESS-53)" (:602-603). The exit gate
restates "no second substrate; the index IS the tape" (:594). ACCEPT.

### §7 W4 (L9 conditional) — ACCEPT
Rides the SK-V16-banked O(1) `offsets.len()` checkpoint / truncate on the ONE offset
vector; "byte-identical tape" exit gate forecloses a behavior-change-as-control-flow.
Pre-blocks `split_off`/`Vec<Vec>` arena (SPEC.md:660-662). No new substrate.

### §8 W5 (close) — ACCEPT
No source by default; reconciliation only. Pre-blocks "deleting legacy CSS generated/
runtime shims before replacement proof landed" — i.e. forbids the close from masking
a still-live second plane.

### §9 pre-blocked ledger — ACCEPT
The "Second substrate" global block (SPEC.md:746-750) enumerates the full CH5 escape
set; the binding conditions 1-2-3 (index==offsets, flag=branch-tag, derived-from-
grammar) carry the S-P2 §6 conditions verbatim (verified against
`HARDENING-S-P2-V3-CONSOLIDATED.md:318-326`). ACCEPT.

---

## §2 — Cross-artefact coupling defect (the one REVISE)

**CH5-1 — REVISE — wave-numbering divergence across the cohort de-couples the
per-wave hidden-coupling attributions from the wave that enforces them.**

Three distinct wave-number→candidate maps exist across the six P3 artefacts + SPEC:

| Artefact | tape | projection | NEON | L9 | close |
|---|---|---|---|---|---|
| **SPEC.md** (load-bearing) | W1 | W2 | **W3** | **W4** | **W5** |
| **p3a** §3 / **p3c** / **p3f** §1-§2 | W1 | W2 | **W3** | **W4** | **W5** |
| **p3b** §2 manifest (5-wave) | W1 (tape+proj merged) | W1 | **W2** | **W3** | W4 |
| **p3e** §1 wave-map (5-wave) | W1 | W2 | **W3** | **W4** | (none) |

- `p3b-wave-sequencing.md:77-83` lands NEON in **W2** and L9 in **W3** (5 waves, no
  separate projection wave — tape+projection collapsed into W1), with the topological
  summary diagram (`:257-261`) drawing `W2 (NEON) → W3? (L9) → W4 (close)`.
- `p3e-preblocked-ledger.md:67-73` lands projection in **W2**, NEON in **W3**, L9 in
  **W4**, and has NO close wave (5-row map W0-W4).
- The SPEC + p3a + p3c + p3f agree on the 6-wave map (W3=NEON, W4=L9, W5=close).

Why this is a CH5 (not merely CH4) concern: the per-wave hidden-coupling BINDING
ATTRIBUTIONS are wave-keyed. P3-C §4.1 binds "L1/L4 index == tape-offsets identity →
**W3** exit gate (c)" and "L8 flag = branch-tag → **W1** exit gate (g)". P3-B §2.1
binds the SAME index==offsets condition to its **W2** ("§6.1 ... → CH5 REJECT-at-wave"
at `:201-205`) and L8=branch-tag to its **W1** (`:150-152`). P3-E binds index==offsets
to its **W3** (`:132`). A wave triumvirate dispatched by reference to "W2" or "W3"
will read a DIFFERENT coupling-condition checklist depending on which artefact it
opens — the same Lock-1 condition is attributed to W2 in p3b, W3 in SPEC/p3c/p3e.
The substrate-union enforcement is intact in EVERY artefact; what diverges is WHICH
wave is named the enforcer. That is a reference-coupling hazard: the CH5 gate could be
checked at the wrong wave, or double-checked / skipped, under a wave label collision.

Concrete fix (REVISE, not REJECT — the SPEC itself is correct and self-consistent):
1. Adopt the SPEC's 6-wave map (W0 baseline, W1 tape, W2 projection, **W3 NEON**,
   **W4 L9-conditional**, **W5 close**) as the single canonical numbering. It is the
   load-bearing contract and p3a/p3c/p3f already conform.
2. `p3b-wave-sequencing.md:77-261` — re-number to the 6-wave map: split the
   tape+projection W1 into W1 (tape) + W2 (projection), shift NEON W2→W3, L9 W3→W4,
   close W4→W5. Re-attribute the §2.1 binding-condition enforcement lines (`:150-152`
   L8→W1/W2, `:201-209` index==offsets→W3) to the canonical waves.
3. `p3e-preblocked-ledger.md:67-73` + per-wave tables (`:93,:114,:128,:144`) — add the
   missing W5 close-wave row and align NEON→W3 / L9→W4 to the canonical map (p3e
   already uses W3=NEON/W4=L9, so only the close-wave row and the §1 map header
   need the 6-wave alignment; the substrate attributions are already at W3).

This is a coupling-of-references defect, not a substrate-honesty defect. No artefact
introduces a parallel substrate; the REVISE is to make the per-wave CH5 gate point at
ONE wave so the triumvirate checks it exactly once, at the right wave.

---

## §3 — Counts + dispositions

Reviewed surface: SPEC.md §0.1-§0.5, §1, §3-§9 (the contract); p3a §2-§4; p3b §2-§4;
p3c §2-§4.1; p3e §1-§4; p3f §1-§4. Dispositioned as wave/section units.

Disposition units (15):

| # | Unit | Disposition |
|---|---|---|
| 1 | SPEC §0.1 close condition (one-substrate rows) | ACCEPT |
| 2 | SPEC §0.2 comparator classes (Track2 distinctness) | ACCEPT |
| 3 | SPEC §0.4 telemetry (tape_activated/lazy_view honesty) | ACCEPT |
| 4 | SPEC §1 Lock-1 non-negotiable | ACCEPT |
| 5 | SPEC §3 W0 baseline | ACCEPT |
| 6 | SPEC §4 W1 tape activation | ACCEPT |
| 7 | SPEC §5 W2 projection generator | ACCEPT |
| 8 | SPEC §6 W3 NEON index | ACCEPT |
| 9 | SPEC §7 W4 L9 conditional | ACCEPT |
| 10 | SPEC §8 W5 close | ACCEPT |
| 11 | SPEC §9 pre-blocked ledger + binding conditions | ACCEPT |
| 12 | p3a shortlist (S1-S9 substrate honesty) | ACCEPT |
| 13 | p3c falsifiability gates (CH5 binding §4.1) | ACCEPT |
| 14 | p3e pre-blocked ledger (REDRESS 96/97/98 + 140 seam) | ACCEPT |
| 15 | Cross-artefact wave-numbering divergence (p3b/p3e vs SPEC) | **REVISE (CH5-1)** |

- ACCEPT: 14
- REVISE: 1 (CH5-1)
- REJECT: 0

ACCEPT rate: 14/15 = 93.3%.

Critical defects: 0 (no substrate-cardinality violation, no sidecar, no retained
cursor, no aux density table, no Track1≡Track2 dishonesty; L8=branch-tag confirmed
against `OffsetFlags(u8)`).

The single REVISE (CH5-1) is a fold-on-next-cycle reference-alignment, orphan-free:
its fix is bounded to re-numbering p3b + adding the p3e close-wave row to the SPEC's
canonical 6-wave map. The load-bearing SPEC.md is CH5-clean as written.

---

## §4 — Sources

- `restart/skinny/tranches/sk-v17/SPEC.md` (§0-§10).
- `restart/skinny/tranches/sk-v17/research/p3/{p3a,p3b,p3c,p3e,p3f}.md`.
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  §6 binding conditions (:318-326), §4 REJECTed (:344).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH5.
- HEAD `f87ee713a` greps: `runtime/src/grammars/css_l4_declaration_values/generated.rs:5`
  (`emit_fact_stream`); `runtime/src/tape/mod.rs:18,38,97-98,144-175`
  (`OffsetFlags(u8)`/`GRAMMAR_BIT0,1`, `PayloadArena`, sparse pair, `flags_at`,
  `ValueRef<...,G>`); `runtime/src/tape/assembler.rs:14-17,42,71,89`
  (`CapacityPlan::OneShotSimd`, `TapeBuilder`, `push_plain_offset`,
  `reserve_offsets_cold`); `codegen/src/lib.rs:299,336,567,611`
  (`W5C_REQUEST_FACT_PROFILES`); `bbnf-bench/src/nonjson_css_l4.rs:596,624-633`
  (`track1_facts -> String`, `oracle_facts`/`OracleParser`); zero-tape-in-css grep
  EMPTY over `grammars/css_l4_*`; second-substrate symbols hit only guard machinery
  (`lock14_baseline.rs`, `report.rs`, `codegen/src/lib.rs:1355` forbidden-name list).
