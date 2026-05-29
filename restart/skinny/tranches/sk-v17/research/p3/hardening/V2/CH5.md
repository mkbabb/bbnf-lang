# SK-V17 S-P3 CHALLENGE — CH5 HIDDEN-COUPLING (V2)

Lens: CH5 HIDDEN-COUPLING. Cycle: V2. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/SPEC.md` + `research/p3/{p3a..p3f}.md`.
Focus (per PASS-3 §3 CH5 + ORCHESTRATOR §3W): no wave introduces a parallel
substrate / sidecar producer / renamed scanner (Lock 1) / retained cursor / aux
density table / sidecar event vector / Track 1 ≡ Track 2 dishonesty. Tape +
projection ONE substrate (Lock 1). L8 flag = `BackendRule` branch-tag, NOT an aux
table. Master HEAD `f87ee713a`.

Disposition vocabulary: ACCEPT / REVISE / REJECT. Counts at §3.

---

## §0 — V1 carry-forward + method (greps at HEAD)

V1 CH5 returned ACCEPT 14 / REVISE 1 / REJECT 0 (93.3%) with one defect, **CH5-1**:
the wave-numbering divergence across the cohort de-coupled the per-wave
hidden-coupling attributions from the wave that enforces them (p3b ran a 5-wave map
with NEON@W2/L9@W3/close@W4; p3e lacked a close-wave row; the SPEC/p3a/p3c/p3f ran
the 6-wave map). The prescribed fix: adopt the SPEC's 6-wave map as canonical and
re-number p3b + add the p3e close row.

**V1 CH5-1 fold verified — RESOLVED.** At V2:

- `p3b-wave-sequencing.md:10-31` §0 explicitly re-authors to the SPEC six-wave
  manifest verbatim (W0 baseline, W1 PRUNE/tape, W2 projection, **W3 NEON**, **W4
  L9-conditional**, **W5 close**); `:121-136` the manifest table now reads W0…W5 with
  NEON@W3 / L9@W4 / close@W5; `:332-341` re-attributes §6.1 index==offsets to **W3**
  and §6.4 L6 scalar-balance to **W3**; `:272-280` re-attributes §6.2 L8=branch-tag to
  **W2**. The 5-wave numbering is RETIRED (`:24,:111`).
- `p3e-preblocked-ledger.md:64-80` carries the **canonical 6-wave W0-W5 map** with the
  W5 close row present (`:80`: "W5 | close, clean regen … | none"); NEON@W3 (`:78`),
  L9@W4 (`:79`); `:122-123` explicitly states "**L8 is NOT a W1 candidate** — it lands
  in W2 (`SPEC.md:469-470`)". The missing close-wave row is added; the NEON/L9
  attributions align to W3/W4.

The substrate-cardinality facts the SPEC keys its Lock-1 gates on, re-verified at HEAD
this cycle:

- **Zero tape symbols in the CSS path:** `grep -rlE 'TapeBuilder|crate::tape|ValueRef'`
  over `crates/runtime/src/grammars/css_l4_*` returns EMPTY at HEAD. The W1
  tape-activation gate (grep non-zero) is a REAL falsifiable transition. CONFIRMED.
- **`emit_fact_stream(input:&str)->Result<String,CssFactError>`** at
  `crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:5` — the String
  admission plane the SPEC §0.1 row 2 / §9 retires. HONEST.
- **`track1_facts(input:&str)->Result<String,String>`** at
  `crates/bbnf-bench/src/nonjson_css_l4.rs:596`; **`oracle_facts`/`OracleParser`** at
  `:624-697` — an INDEPENDENT reference parser, NOT a Track-1 sidecar. The §0.2
  comparator table's "Track 2 / oracle … structurally distinct from Track 1 (Lock 1,
  CH5)" is HONEST — no Track 1 ≡ Track 2 dishonesty.
- **`W5C_REQUEST_FACT_PROFILES`** at `crates/codegen/src/lib.rs:299/336/567/611` —
  exactly the cited lines. HONEST.
- **L8 flag = branch-tag, not catalogue:** `OffsetFlags(pub u8)` `tape/mod.rs:18` with
  `GRAMMAR_BIT0=0x01`/`GRAMMAR_BIT1=0x02` `:22-23`, `flags_at` binary-search `:144-150`
  over the EXISTING sparse `flag_cursors`/`flag_values` pair (`assembler.rs:93-113`) —
  a generic branch-tag bit over an existing sparse pair, NOT a per-rule table, NOT a
  dense parallel column. The S-P2 §6.2 condition is satisfiable by the actual
  mechanism. CONFIRMED.
- **No real second substrate in skinny:** `grep 'StructLayout|TapeStructBuilder|
  TapeCursor|UnionTape'` over `crates/` hits ONLY guard/lint machinery
  (`bbnf-bench/src/lock14_baseline.rs`, `bbnf-bench/src/report.rs`,
  `codegen/src/lib.rs` forbidden-name list) — the ANTI-second-substrate enforcement,
  not constructs. Substrate cardinality is ONE. CONFIRMED.

The SPEC remains, on its central CH5 axis, substrate-honest: Lock 1 is asserted at §1
(line 229-235), §0.1 row 2, §0.2 comparator table (line 125), §0.4 telemetry
(`tape_activated` keyed to `PayloadArena` counters, NOT a `crates/core` grep, line
174), §9 global "Second substrate" block (line 802-806) + per-wave + binding
conditions 1-2-3-4-5 (line 832-845), and every wave's pre-blocked-routes list. The
REDRESS-140 differential (substrate cardinality one; the index IS the tape `offsets`)
is the explicit admissibility seam over the RETIRED REDRESS 96/97/98 union-substrate
thesis (`p3e:30-62`). I could find NO parallel-substrate, retained-cursor,
sidecar-event-vector, or aux-density-table escape in any wave of the load-bearing SPEC.

---

## §1 — Per-section disposition (SPEC.md — the load-bearing contract)

### §0.1 close condition (one-substrate rows) — ACCEPT
Row 2 (SPEC.md:50-53): "NO new cursor/builder type — the EXISTING
`Tape`/`ValueRef`/`TapeBuilder` is the only substrate (Lock 1)." Row 3 (:58-72) routes
projection over "the SAME `BackendRule` shape the parser emits … over the existing
`Tape`/`ValueRef`". Row 9 (:100-106) forbids a retained classifier, constrains the NEON
leaf to a transient `Vec<u32>` the tape consumes. No coupling escape.

### §0.2 comparator classes (Track2 distinctness) — ACCEPT
Line 125 marks "Track 2 / oracle … structurally distinct from Track 1 (Lock 1, CH5)" —
verified against `oracle_facts`/`OracleParser` `nonjson_css_l4.rs:624-697`. The strict
admission predicate (`css_comparator_plane=full-cssom`, equality-before-speed, line
128-131) forecloses a fact-stream-as-comparator Track1≡Track2 collapse.

### §0.3 / §0.4 outcome + telemetry — ACCEPT
`tape_activated` keyed to `PayloadArena` write/alloc counters, NOT a `crates/core` grep
(line 174) — wrong-tree dishonesty foreclosed. `lazy_view_generated` keyed to "accessor
generator emits document/value/view/visitor over BackendRule" (line 175) — projection
rides the substrate, no sidecar producer. `projection_generality_exercise ∈ {json,
css_l4}` with `sheets_witness INVALID` (line 176).

### §1 non-negotiables (Lock 1) — ACCEPT
Line 229-235 is the canonical CH5 clause: "no second tape, no `StructLayout`/
`TapeStructBuilder`/`TapeCursor`, no `UnionTape`, no public substrate API, no
parser-owned structural cursor/facts, no sidecar event vector, no aux density/
projection table, no retained cursor/list, no parallel source pass, no cross-call
classifier-state retention. A SIMD mask stream is a transient producer, not a retained
sidecar; if structural offsets are retained, the structural projection IS the tape."
Exhaustive over the CH5 escape set.

### §3 W0 (baseline) — ACCEPT
0 behavior LOC; no substrate touched. Pre-blocks the 24-row broadcast (one tuple → N
rows — the Track1-fabrication coupling) and the fact-stream comparator. No
hidden-coupling surface.

### §4 W1 (tape activation) — ACCEPT
"it does not add a parallel tape path beside the String" (line 391). Entry gate (line
436): CHALLENGE accept that W1 "introduces no second substrate (Lock 1)". Exit gate
(line 476): "No second substrate; no parser-owned cursor; exactly one retained tape
survives." `push_plain_offset` = "one branchless u32 write into the EXISTING `offsets`"
(line 446). `offsets.len()` checkpoint / truncate rollback forbids `split_off`/`Vec<Vec>`
(line 434, 484-485). The L8 sparse pair is "used, not modified to add a second tape"
(assembler owner-path line 414-415). W1's candidate set is **L2+L7+L3** (line 396) — L8
is correctly NOT a W1 candidate.

### §5 W2 (projection generator) — ACCEPT
Entry gate (line 526-528) makes CHALLENGE accept the two CH5 binding conditions
verbatim: "L8 flag bits are `BackendRule` branch-tag projections, NOT a hand-curated
per-rule catalogue"; "the L1/L4 index, when consumed, IS the tape's `offsets`, never a
parallel retained vector". Task 2 (line 538-541): "node kind recovered from the source
byte at the offset (no stored tag)" — no aux tag table. Task 3 (line 542-544): L8 flag
bits stored in the EXISTING sparse pair, each a `BackendRule` branch-tag projection.
Pre-blocked routes (line 571-574) name "L8 flag as a hand-curated per-rule catalogue
(the relocated-W5C overfit); the L1/L4 index retained as a parallel vector (REDRESS-53);
retained cursor / aux density / sidecar event vector; a second substrate; relocating
per-rule branching into projection DATA." "NO new cursor/builder type" (line 537).
L8 lands HERE (W2), and the SPEC's branch-tag guard is HERE (W2). Internally consistent.

### §6 W3 (NEON structural index) — ACCEPT
Entry gate (line 617-619): CHALLENGE accept "the `Vec<u32>` index IS the tape's
`offsets`". L6 carry is "i32 `depth_carry` threaded WITHIN a single
`scan_components_to_index` call, init-0-per-parse, never retained" (line 628-629) — a
transient producer, not a retained sidecar. L5 mask "1-bit carry threads WITHIN one
block sequence" (`p3a:114`). Pre-blocks "cross-call classifier-state retention; a
retained index vector parallel to the tape (REDRESS-53)" (line 651-652). Exit gate
restates "no second substrate; the index IS the tape" (line 643).

### §7 W4 (L9 conditional) — ACCEPT
Rides the SK-V16-banked O(1) `offsets.len()` checkpoint / truncate on the ONE offset
vector; "byte-identical tape" exit gate (line 705) forecloses behavior-change-as-control-
flow. Pre-blocks `split_off`/`Vec<Vec>` arena (line 716-718). No new substrate.

### §8 W5 (close) — ACCEPT
No source by default; reconciliation only. Pre-blocks "deleting legacy CSS generated/
runtime shims before replacement proof landed" (line 766-768) — forbids the close from
masking a still-live second plane.

### §9 pre-blocked ledger + binding conditions — ACCEPT
The "Second substrate" global block (line 802-806) enumerates the full CH5 escape set;
binding conditions 1-2-3-4-5 (line 832-845) carry the S-P2 §6 conditions verbatim
(verified against `HARDENING-S-P2-V3-CONSOLIDATED.md` §6). The per-wave attribution
table (line 818-823) correctly keys L8=catalogue to W2, index==offsets/CTZ to W3.

---

## §2 — The residual coupling defect (the one REVISE)

**CH5-2 — REVISE — p3c keys the L8 flag=`BackendRule` branch-tag binding condition to
W1, where L8 does not land; the SPEC, p3b, and p3e all land L8 (and its anti-aux-table
guard) in W2. The L8 anti-sidecar/anti-relocated-W5C guard is attributed to the wrong
wave in p3c.**

This is the V1 CH5-1 wave-numbering defect, RESIDUAL in p3c after the fold. p3b and p3e
were re-numbered and re-attributed; p3c was NOT fully reconciled on L8.

The L8 placement disagreement:

| Artefact | L8 wave | L8 branch-tag (anti-aux-table) guard at |
|---|---|---|
| **SPEC.md** (load-bearing) | **W2** (§5 task 3, line 542-544; candidate list line 499) | **W2** entry gate (line 526) + W2 pre-block (line 571-572) |
| **p3a** S8 | W2 (same-wave consumer = S3 full rider, `p3a:146`) | via S3's W2 equality gate (`p3a:147`) |
| **p3b** §2 | **W2** (`:127`, `:241-243`); binding §6.2 → W2 (`:272-274`) | **W2** (`:251-253`, "CH2 REJECT-at-wave") |
| **p3e** §1 | **W2** (`:122-123`: "L8 is NOT a W1 candidate — it lands in W2") | W2 table |
| **p3c** (THE divergence) | **W1** (`:48` map; `:76` §2.1 header "L8"; `:83-85` W1 gate) | **W1** exit gate (g) (`:85`); §3 binding (`:190`: "L8 flag = `BackendRule` branch-tag → **W1** exit gate (g)") |

Concrete evidence of the gap:

1. `p3c:48` wave-map row: "**W1** tape activation | L2 … · **L8** sparse-flag side-table
   · L7 …". `p3c:85` W1 exit gate (g): "L8 flag bits are `BackendRule` branch-tag
   projections, NOT a hand-curated per-rule catalogue (§6.2 condition; grep for a
   relocated `W5C_REQUEST_FACT_PROFILES`-shaped array → REJECT)."
2. `p3c:189-190` §3 binding-condition table: "2. **L8 flag = `BackendRule` branch-tag**
   → **W1 exit gate (g)**: no hand-curated per-rule catalogue (relocated
   `W5C_REQUEST_FACT_PROFILES` → CH2 REVISE)."
3. `p3c:96-99` W2 gate (§2.2) carries the W5C-retirement check (b) and the JSON
   byte-equal check (c) — but does NOT carry the L8 flag=branch-tag check. So in p3c
   the L8 anti-aux-table guard exists ONLY at W1.

Why this is a CH5 (not merely CH4) concern: the L8 flag=branch-tag condition is the
load-bearing guard against an **aux density / projection table** — the
relocated-`W5C_REQUEST_FACT_PROFILES`-into-flag-form re-entry, which is precisely a
sidecar/aux-table coupling escape (§1 line 232 "no aux density/projection table"; §9
W2 pre-block line 571-572). The SPEC binds this guard to W2 because that is where L8
lands (the kind-disambiguation flag bits, SPEC §5 task 3) and where its reader (L3-full)
is co-resident. If a W1 wave-triumvirate dispatches by reference to p3c, it will check
the L8 anti-aux-table guard at W1 — a wave whose SPEC candidate set (L2+L7+L3) does not
include L8 — and the W2 triumvirate reading p3c's §2.2 gate will find NO L8 branch-tag
check at the wave where L8 actually lands. The same Lock-1 anti-sidecar condition is
attributed to W1 in p3c and W2 in the SPEC/p3b/p3e: the CH5 gate could be checked at the
wrong wave, or skipped at W2 entirely. This is a coupling-of-references defect of the
exact CH5-1 family, residual in the one artefact the V1 fold did not reach on L8.

(Note on the W1 assembler owner-path: the SPEC and p3b DO touch the assembler's
`flag_cursors`/`flag_values` pair in the W1 owner-paths as the existing substrate that
is "USED, not widened" — but the L8 *candidate* (storing kind-disambiguation flag bits
+ its branch-tag guard) is a W2 task with its reader in W2. p3c conflates "the sparse
pair exists and may be touched in W1" with "L8 the candidate lands in W1". The SPEC is
unambiguous: L8 lands in W2.)

Concrete fix (REVISE, not REJECT — the SPEC is correct and self-consistent; only p3c
diverges; bounded to p3c):

1. `p3c:48` — move **L8** from the W1 wave-map row to the W2 wave-map row (W2 currently
   reads "L3 generalization · W5C retire"; add "· L8 sparse-flag side-table
   (`BackendRule` branch-tag)"). W1 row becomes "L2 · L3 `ValueRef` projection · L7" (no
   L8), matching SPEC §4 candidate list (line 396) and p3b/p3e.
2. `p3c:76` §2.1 header "W1: tape activation (L2 + L3 + L8 + L7)" → "(L2 + L3 + L7)".
3. `p3c:85` W1 exit gate — remove gate part (g) (the L8 branch-tag check) from W1.
4. `p3c:99` W2 exit gate (§2.2) — ADD the L8 flag=`BackendRule` branch-tag check (the
   anti-relocated-W5C-into-flag-form / anti-aux-table guard) as a new W2 exit-gate part:
   "L8 flag bits are `BackendRule` branch-tag projections stored in the EXISTING sparse
   `flag_cursors`/`flag_values` pair, NOT a hand-curated per-rule catalogue and NOT a
   dense parallel column (grep for a relocated `W5C_REQUEST_FACT_PROFILES`-shaped array
   or a widened per-position record → REJECT)." This places the anti-aux-table guard at
   the wave where L8 lands.
5. `p3c:189-190` §3 binding-condition table item 2 — re-attribute "L8 flag =
   `BackendRule` branch-tag → **W2** exit gate" (matching SPEC §5 entry gate line 526
   and the V2 p3b `:272-274` / p3e `:122-123`).

This is a coupling-of-references defect, NOT a substrate-honesty defect. No artefact
introduces a parallel substrate, a sidecar producer, a retained cursor, an aux density
table, or a Track1≡Track2 dishonesty. The REVISE makes the per-wave L8 anti-aux-table
CH5 gate point at the ONE wave (W2) the triumvirate enforces it at, so it is checked
exactly once, where L8 lands.

---

## §3 — Counts + dispositions

Reviewed surface: SPEC.md §0.1-§0.5, §1, §3-§9 (the contract); p3a §2-§4; p3b §0-§2; p3c
§1-§3; p3e §1-§4; p3f. Dispositioned as wave/section units.

Disposition units (16):

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
| 13 | p3b wave-sequencing (V1 CH5-1 fold; canonical 6-wave; per-wave CH5 attributions) | ACCEPT |
| 14 | p3e pre-blocked ledger (REDRESS 96/97/98 retired + 140 seam; W5 row added) | ACCEPT |
| 15 | p3c falsifiability gates (index==offsets W3; W5C-retire W2) | ACCEPT |
| 16 | p3c L8 flag=branch-tag wave-attribution (W1 vs SPEC/p3b/p3e W2) | **REVISE (CH5-2)** |

- ACCEPT: 15
- REVISE: 1 (CH5-2)
- REJECT: 0

ACCEPT rate: 15/16 = 93.75%.

Critical defects: 0 (no substrate-cardinality violation, no sidecar, no retained cursor,
no aux density table, no Track1≡Track2 dishonesty; L8=branch-tag confirmed against
`OffsetFlags(u8)`/`GRAMMAR_BIT0,1`; second-substrate symbols hit only guard machinery).

V1 CH5-1 (wave-numbering divergence) is RESOLVED in p3b + p3e. The single residual REVISE
(CH5-2) is the same defect family surviving in p3c on L8's wave-placement: a
fold-on-next-cycle reference-alignment, orphan-free, bounded to five p3c edits (move L8
W1→W2 in the map, header, W1 gate, W2 gate, and the §3 binding table). The load-bearing
SPEC.md is CH5-clean as written — it lands L8 in W2 and binds the anti-aux-table guard
at W2.

---

## §4 — Sources

- `restart/skinny/tranches/sk-v17/SPEC.md` (§0-§10; W1 candidate list line 396; W2 L8
  task line 542-544; W2 entry gate line 526; §9 binding conditions line 832-845).
- `restart/skinny/tranches/sk-v17/research/p3/{p3a,p3b,p3c,p3e,p3f}.md` — p3b §0
  re-sequence (`:10-31`), manifest (`:121-136`), per-wave CH5 attributions
  (`:251-253,:272-280,:332-341`); p3e canonical map (`:64-80`), L8-not-W1 (`:122-123`),
  REDRESS-96/97/98-retired + 140 seam (`:30-62`); p3c wave-map (`:48`), W1/W2 gates
  (`:83-85,:96-99`), §3 binding (`:189-190`).
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V1/CH5.md` (V1 CH5-1).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  §6 binding conditions, §4 REJECTed set.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH5.
- HEAD `f87ee713a` greps: `runtime/src/grammars/css_l4_declaration_values/generated.rs:5`
  (`emit_fact_stream`); `runtime/src/tape/mod.rs:18,22-23,144-150` (`OffsetFlags(u8)`/
  `GRAMMAR_BIT0,1`, `flags_at`); `runtime/src/tape/assembler.rs:42,71,89,93-113`
  (`TapeBuilder`, `push_plain_offset`, `reserve_offsets_cold`, sparse pair);
  `codegen/src/lib.rs:299,336,567,611` (`W5C_REQUEST_FACT_PROFILES`);
  `bbnf-bench/src/nonjson_css_l4.rs:596,624-697` (`track1_facts -> String`,
  `oracle_facts`/`OracleParser`); zero-tape-in-css grep EMPTY over `grammars/css_l4_*`;
  second-substrate symbols hit only guard machinery (`bbnf-bench/src/lock14_baseline.rs`,
  `bbnf-bench/src/report.rs`, `codegen/src/lib.rs` forbidden-name list).
