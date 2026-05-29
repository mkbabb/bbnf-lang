# SK-V17 S-P3 CHALLENGE — CH5 HIDDEN-COUPLING (V3)

Lens: CH5 HIDDEN-COUPLING. Cycle: V3. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/SPEC.md` + `research/p3/{p3a..p3f}.md`.
Focus (PASS-3 §3 CH5 + ORCHESTRATOR §3W): no wave introduces a parallel substrate /
sidecar producer / renamed scanner (Lock 1) / retained cursor / aux density table /
sidecar event vector / Track 1 ≡ Track 2 dishonesty. Tape + projection ONE substrate
(Lock 1). L8 flag = `BackendRule` branch-tag, NOT an aux table. Master HEAD `f87ee713a`.

Disposition vocabulary: ACCEPT / REVISE / REJECT. Counts at §3.

---

## §0 — V2 carry-forward + method (greps at HEAD)

V2 CH5 returned ACCEPT 15 / REVISE 1 / REJECT 0 (93.75%) with one residual defect,
**CH5-2**: p3c keyed the L8 flag=`BackendRule` branch-tag binding condition to **W1**,
where L8 does not land; the SPEC, p3a, p3b, and p3e all land L8 (and its anti-aux-table
guard) in **W2**. The prescribed fix (five p3c edits): move L8 W1→W2 in the wave-map,
the §2.1 header, the W1 exit gate, the W2 exit gate, and the §3 binding table.

**V2 CH5-2 fold verified — RESOLVED.** At V3, every L8 reference in p3c is re-keyed to W2:

- `p3c:48` wave-map: W1 row reads "L2 `push_plain_offset` · L3 minimal `ValueRef` cursor
  read · L7 one-shot SIMD reserve" (NO L8); W2 row reads "L3 generalization … · **L8
  sparse-flag side-table** · L4 tokenize-once reuse · W5C_REQUEST_FACT_PROFILES retire".
- `p3c:76` §2.1 header: "W1: tape activation (**L2 + L3-minimal + L7**)" — L8 gone.
- `p3c:85` W1 exit gate (e): "**L8 does NOT land in W1** — it is single-valued to W2
  (`SPEC.md:497`); its `BackendRule` branch-tag guard is the W2 exit gate, §2.2."
- `p3c:90,:99` §2.2 W2: title "(L3 generalization + **L8** + L4 + W5C retire)"; exit
  gate (f) carries the L8 branch-tag check verbatim: "L8 flag bits are `BackendRule`
  branch-tag projections, NOT a hand-curated per-rule catalogue (§6.2 / S-P2 §6 condition;
  `SPEC.md:526-527,571-572`) … L8 lands in W2, read by L3 the same wave, `SPEC.md:574`".
- `p3c:190` §3 binding table item 2: "**L8 flag = `BackendRule` branch-tag** → **W2 exit
  gate (f)**: L8 is single-valued to W2 (`SPEC.md:497,574`); no hand-curated per-rule
  catalogue (relocated `W5C_REQUEST_FACT_PROFILES` → CH2 REVISE)."

The single open REVISE from V2 is folded with no orphan. The whole cohort now agrees on
the L8 wave-placement: SPEC §5 (`:497,:542-543`), p3a S8 (`:139,:146`), p3b §2
(`:145,:266,:538-540`), p3e (`:77,:122-123,:129,:135`), p3c (above) all land L8 in W2 with
its branch-tag / anti-aux-table guard bound to the W2 exit gate. The CH5-1 (V1) /
CH5-2 (V2) wave-numbering-divergence family is now fully closed across the cohort.

The substrate-cardinality facts the SPEC keys its Lock-1 gates on, re-verified at HEAD:

- **Zero tape symbols in the CSS path:** `grep -rlE 'TapeBuilder|crate::tape|ValueRef|
  PayloadArena'` over `crates/runtime/src/grammars/css_l4_*` returns EMPTY at HEAD. The
  W1 tape-activation gate (grep non-zero, `SPEC.md:463`) is a REAL falsifiable transition.
  CONFIRMED.
- **`emit_fact_stream(input:&str)->Result<String,CssFactError>`** at
  `crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:5` — the String
  admission plane the SPEC §0.1 / §1 / §9 retires. HONEST.
- **`track1_facts(input:&str)->Result<String,String>`** `nonjson_css_l4.rs:596`;
  **`oracle_facts(input)->Result<String,CssOracleError>`** `:624`; **`struct
  OracleParser<'i>`** `:2725` — an INDEPENDENT reference parser at DISTINCT lines from the
  Track-1 emitter, NOT a Track-1 sidecar. The SPEC §0.2 comparator table's "Track 2 /
  oracle … structurally distinct from Track 1 (Lock 1, CH5)" is HONEST — no Track 1 ≡
  Track 2 dishonesty.
- **L8 flag = branch-tag, not catalogue:** `OffsetFlags(pub u8)` `tape/mod.rs:18` with
  `GRAMMAR_BIT0=0x01`/`GRAMMAR_BIT1=0x02` `:22-23`, `flags_at` binary-search `:144-150`
  over the EXISTING sparse `flag_cursors:Vec<u32>`/`flag_values:Vec<u8>` pair
  (`assembler.rs:45-46,98-111`) — a generic branch-tag bit over an existing sparse pair,
  paid only where non-zero, NOT a per-rule table, NOT a dense parallel column. The S-P2
  §6.2 condition is satisfiable by the actual mechanism. CONFIRMED.
- **No real second substrate in skinny:** `grep -rln 'StructLayout|TapeStructBuilder|
  TapeCursor|UnionTape'` over `crates/` hits ONLY guard/lint machinery
  (`bbnf-bench/src/lock14_baseline.rs`, `bbnf-bench/src/report.rs`, `codegen/src/lib.rs`
  forbidden-name list) — the ANTI-second-substrate enforcement, not constructs. Substrate
  cardinality is ONE. CONFIRMED.
- **`W5C_REQUEST_FACT_PROFILES`** at `codegen/src/lib.rs:299/336/567/611` — exactly the
  cited lines; the Lock-14-phrase-#1 construct W2 must RETIRE (not relocate). HONEST.

The SPEC remains, on its central CH5 axis, substrate-honest: Lock 1 is asserted at §1
(`:229-235`, the exhaustive escape-set clause), §0.1 close rows, §0.2 comparator table,
§0.4 telemetry (`tape_activated` keyed to `PayloadArena` write/alloc counters not a
`crates/core` grep, `:463-465`; `lazy_view_generated` keyed to "accessor generator emits
document/value/view/visitor over BackendRule" `:175,:549`), §9 "Second substrate" global
block (`:807-811`) + the per-wave attribution table (`:822-828`) + binding conditions
1-2-3-4-5 (`:837-850`), and every wave's pre-blocked-routes list. I could find NO
parallel-substrate, retained-cursor, sidecar-event-vector, aux-density-table, renamed-
scanner, or Track1≡Track2 escape in any wave of the load-bearing SPEC or any P3 artefact.

---

## §1 — Per-section disposition (SPEC.md — the load-bearing contract)

### §0.1 close condition (one-substrate rows) — ACCEPT
Row 2 forbids a new cursor/builder type — the EXISTING `Tape`/`ValueRef`/`TapeBuilder` is
the only substrate (Lock 1). Row 3 (`:54-62`) routes projection over the SAME
`BackendRule` shape the parser emits, "NOT a CSS-pinned emitter; a generator that leaves
JSON's hand-written path untouched" is the forbidden failure mode. No coupling escape.

### §0.2 comparator classes (Track2 distinctness) — ACCEPT
The strict admission predicate (`css_comparator_plane=full-cssom`, equality-before-speed,
§1 `:253-255`) forecloses a fact-stream-as-comparator Track1≡Track2 collapse. Verified
against the independent `oracle_facts`/`OracleParser` `nonjson_css_l4.rs:624,2725`.

### §0.4 telemetry (tape_activated / lazy_view honesty) — ACCEPT
`tape_activated` keyed to `PayloadArena` write/alloc counters, NOT a `crates/core` grep
(`:463-465`) — wrong-tree dishonesty foreclosed. `lazy_view_generated` keyed to "accessor
generator emits document/value/view/visitor over BackendRule" (`:175,:549`) — projection
rides the substrate, no sidecar producer. `simd_non_json_exercise=css_l4` (`:328,:647`).

### §1 non-negotiables (Lock 1) — ACCEPT
`:229-235` is the canonical CH5 clause: "no second tape, no `StructLayout`/
`TapeStructBuilder`/`TapeCursor`, no `UnionTape`, no public substrate API, no parser-owned
structural cursor/facts, no sidecar event vector, no aux density/projection table, no
retained cursor/list, no parallel source pass, no cross-call classifier-state retention. A
SIMD mask stream is a transient producer, not a retained sidecar; if structural offsets
are retained, the structural projection IS the tape." Exhaustive over the CH5 escape set.

### §3 W0 (baseline) — ACCEPT
0 behavior LOC; no substrate touched. Pre-blocks the 24-row broadcast (one tuple → N
rows — the Track1-fabrication coupling, `:801-802`) and the fact-stream comparator. No
hidden-coupling surface.

### §4 W1 (tape activation) — ACCEPT
Candidate set **L2+L3-minimal+L7** (`:497` single-values L8 OUT of W1; W1 same-wave
consumer is L3's `ValueRef` cursor read of L2's tape, `:478`). Exit gate (`:475-476`): "No
second substrate; no parser-owned cursor; exactly one retained tape survives." Pre-blocks
the W5C array (RETIRED not relocated), StructRegistry indirection, a second substrate,
`split_off`/`Vec<Vec>` arena (`:481-485`). L8 is correctly NOT a W1 candidate.

### §5 W2 (projection generator) — ACCEPT
Entry gate (`:526-528`) makes CHALLENGE accept the two CH5 binding conditions verbatim:
"L8 flag bits are `BackendRule` branch-tag projections, NOT a hand-curated per-rule
catalogue"; "the L1/L4 index, when consumed, IS the tape's `offsets`, never a parallel
retained vector". Task 2 (`:538-541`): "node kind recovered from the source byte at the
offset (no stored tag)" — no aux tag table. Task 3 (`:542-543`): L8 flag bits stored in
the EXISTING sparse pair, each a `BackendRule` branch-tag projection. Pre-blocked routes
(`:576-579`) name "L8 flag as a hand-curated per-rule catalogue (the relocated-W5C
overfit); the L1/L4 index retained as a parallel vector (REDRESS-53); retained cursor /
aux density / sidecar event vector; a second substrate; relocating per-rule branching into
projection DATA." "NO new cursor/builder type" (`:537`). L8 lands HERE and its branch-tag
guard is HERE. Internally consistent and now cohort-consistent (CH5-2 folded).

### §6 W3 (NEON structural index) — ACCEPT
Entry gate (`:621`): CHALLENGE accept "the `Vec<u32>` index IS the tape's `offsets`". L6
carry is "i32 `depth_carry` threaded WITHIN a single `scan_components_to_index` call,
init-0-per-parse, never retained" (`:634`) — a transient producer, not a retained sidecar.
Pre-blocks "cross-call classifier-state retention; a retained index vector parallel to the
tape (REDRESS-53)" (`:656-657`). Exit gate restates "no second substrate; the index IS the
tape" (`:648`). lo6-on-CSS / udot digit / FNV-primitive barred (`:653-656`).

### §7 W4 (L9 conditional) — ACCEPT
Rides the SK-V16-banked O(1) `offsets.len()` checkpoint / truncate on the ONE offset
vector; "byte-identical tape" exit forecloses behavior-change-as-control-flow. Pre-blocks
`split_off`/`Vec<Vec>` arena (`:827`). No new substrate.

### §8 W5 (close) — ACCEPT
No source by default; reconciliation only. Pre-blocks deleting legacy CSS shims before
replacement proof landed and full-codegen close while dirty generated files remain
(`:816-818`) — forbids the close from masking a still-live second plane.

### §9 pre-blocked ledger + binding conditions — ACCEPT
The "Second substrate" global block (`:807-811`) enumerates the full CH5 escape set; the
per-wave attribution table (`:822-828`) correctly keys L8=catalogue/aux-table to **W2**
(`:825`), REDRESS-53 retained-index to W2/W3, classifier-state retention to W3. Binding
conditions 1-2-3-4-5 (`:837-850`) carry the S-P2 §6 conditions verbatim (re-verified
against `HARDENING-S-P2-V3-CONSOLIDATED.md` §6).

---

## §2 — Per-artefact disposition (the research cohort)

### p3a candidate shortlist (S1-S9 substrate honesty) — ACCEPT
S1/S3/S4 bind index==tape-offsets identity ("the index IS the tape (no parser-local
second cursor)", `:98`; cond-1 `:204-206`). S5/S6 mark the carry/mask as transient
producers, not retained sidecars (`:114,:120`). S8 (`:139-148`): flag bit = `BackendRule`
branch-tag over the EXISTING sparse pair, "NOT a new vector, NOT a widened per-position
record, NOT a dense parallel column" (`:142`), "the side-table adds no substrate
(CH5-clean, Lock 1)" (`:148`). The REJECTed-set bar (D6 second substrate, `:34,:200`) is
carried. No coupling escape.

### p3b wave-sequencing (per-wave CH5 attributions) — ACCEPT
§0 (`:16-18`) records the CH5-2 fold: "L8→W2 with its branch-tag guard route to P3-A +
P3-C to reconcile … L8 in W2, so no P3-B placement edit is required. This V3 re-keys".
Manifest (`:145`) lands L8 in W2; §6 (`:538-540`) keys the L8 sparse-flag → sidecar /
hand-curated-catalogue pre-block to W2 per the SPEC L8-in-W2 placement. Canonical 6-wave
map preserved. CH5-clean.

### p3c falsifiability gates (CH5-2 fold; index==offsets W3; W5C-retire W2; L8 W2) — ACCEPT
The V2 CH5-2 defect is fully folded (§0 above): L8 moved W1→W2 in the wave-map (`:48`),
the §2.1 header (`:76`), the W1 exit gate (`:85`, now "L8 does NOT land in W1"), the W2
exit gate (`:99` part (f) carries the branch-tag/anti-aux-table check), and the §3 binding
table (`:190` → W2). §3 binding item 1 keys index==offsets to W3 exit (c) + W2 exit (g)
(`:189`); item 2 L8=branch-tag to W2 exit (f) (`:190`); item 4 L6 scalar-balance to W3 (d)
(`:192`). Pre-blocked routes (`:179-183`) name REDRESS-53, second substrate, retained
sidecar/aux-density/event-vector per wave. CH5-clean.

### p3e pre-blocked ledger (REDRESS 96/97/98 retired; W5 row; L8-not-W1) — ACCEPT
Canonical W0-W5 map with the W5 close row (`:77` W2 = L3+L8+L4). `:122-123`: "**L8 is NOT
a W1 candidate** — it lands in W2 (`SPEC.md:542-543`), where its reader (L3's full rider)
is co-resident; this ledger keys the L8 anti-sidecar pre-block to the W2 table". §W2
(`:135`): the sparse-flag → sidecar/dense-column route is "re-keyed from W1" to W2, the
flag MUST be a branch-tag projection NOT a hand-curated per-rule catalogue. The
class-column-substrate (REDRESS-96) and EventTape anti-sidecar gate are carried as the
L8-widening pre-block. CH5-clean.

### p3f spec-draft (Lock1 substrate-union; Track1≡Track2 pre-block) — ACCEPT
§1 carries Lock-1 substrate-union + Lock-14 grammar-neutrality (`:127`); §2.1 the
generality + Lock-14 gate (`:132`); the pre-blocked-routes section carries "retained
cursor / aux density / sidecar event vector" (`:221`) and "**All waves:** Track 1 ≡ Track
2 dishonesty (CH5); wrong-plane comparator admission" (`:230`) + a second substrate
(`:216`). CH5-clean.

---

## §3 — Counts + dispositions

Reviewed surface: SPEC.md §0.1-§0.5, §1, §3-§9 (the contract); p3a §2-§4; p3b §0-§6; p3c
§0-§4; p3e §1-§4; p3f §1-§4. Dispositioned as wave/section + per-artefact units.

| # | Unit | Disposition |
|---|---|---|
| 1 | SPEC §0.1 close condition (one-substrate rows) | ACCEPT |
| 2 | SPEC §0.2 comparator classes (Track2 distinctness) | ACCEPT |
| 3 | SPEC §0.4 telemetry (tape_activated/lazy_view honesty) | ACCEPT |
| 4 | SPEC §1 Lock-1 non-negotiable | ACCEPT |
| 5 | SPEC §3 W0 baseline | ACCEPT |
| 6 | SPEC §4 W1 tape activation (L2+L3-min+L7; L8 not here) | ACCEPT |
| 7 | SPEC §5 W2 projection generator (L8+branch-tag guard here) | ACCEPT |
| 8 | SPEC §6 W3 NEON index (Vec<u32> IS the tape offsets) | ACCEPT |
| 9 | SPEC §7 W4 L9 conditional | ACCEPT |
| 10 | SPEC §8 W5 close | ACCEPT |
| 11 | SPEC §9 pre-blocked ledger + binding conditions | ACCEPT |
| 12 | p3a shortlist (S1-S9 substrate honesty) | ACCEPT |
| 13 | p3b wave-sequencing (CH5-2 fold; per-wave CH5 attributions) | ACCEPT |
| 14 | p3c falsifiability gates (CH5-2 RESOLVED; L8→W2) | ACCEPT |
| 15 | p3e pre-blocked ledger (REDRESS 96/97/98; L8-not-W1) | ACCEPT |
| 16 | p3f spec-draft (Lock1; Track1≡Track2 pre-block) | ACCEPT |

- ACCEPT: 16
- REVISE: 0
- REJECT: 0

ACCEPT rate: 16/16 = 100%.

Critical defects: 0 (no substrate-cardinality violation, no sidecar producer, no retained
cursor, no aux density/projection table, no renamed scanner, no Track1≡Track2 dishonesty;
L8=branch-tag confirmed against `OffsetFlags(u8)`/`GRAMMAR_BIT0,1`/sparse pair; second-
substrate symbols hit only guard machinery; zero tape symbols in css_l4_* at HEAD).

The V2 REVISE (CH5-2 — p3c's L8 W1-vs-W2 wave divergence) is RESOLVED across all five p3c
edit sites; the whole cohort now lands L8 in W2 with its `BackendRule` branch-tag /
anti-aux-table guard bound to the W2 exit gate. The CH5 wave-numbering-divergence family
(CH5-1 V1, CH5-2 V2) is fully closed. The load-bearing SPEC.md is CH5-clean: tape +
projection are ONE substrate (Lock 1), L8 is a branch-tag bit over the existing sparse
pair (not an aux table), and the exit-gate language forbids a parser-owned structural
projection / retained cursor / aux density table / sidecar event vector at every wave. No
open or orphan REVISE for CH5.

---

## §4 — Sources

- `restart/skinny/tranches/sk-v17/SPEC.md` — §1 Lock-1 clause (`:229-235`); W1 candidate
  set (`:497` L8-single-valued-out, `:478` consumer); W2 §5 (L8 task `:542-543`, entry-gate
  branch-tag/identity conditions `:526-528`, pre-blocks `:576-579`); W3 §6 (index IS the
  tape `:621,:648`, L6 transient carry `:634`, pre-blocks `:656-657`); §9 second-substrate
  block (`:807-811`), per-wave table (`:822-828`, L8→W2 `:825`), binding conditions
  (`:837-850`); telemetry (`:175,:463-465`).
- `restart/skinny/tranches/sk-v17/research/p3/{p3a,p3b,p3c,p3e,p3f}.md` — p3a S8 (`:139-148`),
  cond-1/2 (`:204-207`); p3b CH5-2 fold (`:16-18`), manifest L8→W2 (`:145`), §6 (`:538-540`);
  p3c CH5-2 fold sites (`:48,:76,:85,:90,:99,:189-192`); p3e canonical map (`:77`), L8-not-W1
  (`:122-123`), W2 anti-sidecar (`:135`); p3f Lock1 (`:127,:216,:221`), Track1≡Track2 (`:230`).
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V2/CH5.md` (V2 CH5-2).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  §6 binding conditions, §4 REJECTed set.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH5.
- HEAD `f87ee713a` greps: `runtime/src/grammars/css_l4_declaration_values/generated.rs:5`
  (`emit_fact_stream`); `runtime/src/tape/mod.rs:18,22-23,144-150` (`OffsetFlags(u8)`/
  `GRAMMAR_BIT0,1`/`flags_at`); `runtime/src/tape/assembler.rs:45-46,71,98-111`
  (sparse pair, `push_plain_offset`); `codegen/src/lib.rs:299,336,567,611`
  (`W5C_REQUEST_FACT_PROFILES`); `bbnf-bench/src/nonjson_css_l4.rs:596,624,2725`
  (`track1_facts->String`, `oracle_facts`, `OracleParser`); zero-tape-in-css grep EMPTY
  over `grammars/css_l4_*`; second-substrate symbols hit only guard machinery
  (`bbnf-bench/src/lock14_baseline.rs`, `bbnf-bench/src/report.rs`, `codegen/src/lib.rs`).
