# SK-V14 S-P2 V1 CHALLENGE — Lens CH5 — HIDDEN COUPLING

Author: CH5 lens agent, S-P2 CHALLENGE V1.
Date: 2026-05-23.
Scope: lens CH5 — HIDDEN COUPLING — across the six committed P2 artefacts
(`p2a-sota-teardown.md` … `p2f-grammar-neutral.md`).
Binding: `PASS-2-RESEARCH.md §3 CH5` ("no parallel substrate / sidecar producer /
renamed scanner / Track 1 ≡ Track 2 dishonesty") + `ORCHESTRATOR.md §3W` CH5 +
`PASS-2-RESEARCH.md §8.6` substrate union closing pin ("P2-D concludes whether
the tape + structural projection are one substrate; a candidate that splits
them, or adds a sidecar event vector, violates Lock 1 and S-P3 may not
shortlist it") + `LOCKS.md:48-90` (Lock 1 + v+1 substrate-ceiling fold +
REDRESS 96/97/98 binding-history clause) + CHALLENGE-CONTEXT.md §2 CH5 binding.
Discipline: write-only, `path:line` per claim, executable-verification mandate,
HARD CAP 30 min.

## §1 — CH5 disposition summary

CH5 asks whether any P2 candidate primitive proposes (a) a **parallel substrate**
(a second classifier walking the same source), (b) a **sidecar producer** (a
parser-owned vector / cursor / map outside the offset tape), (c) a **renamed
scanner** (a second source scan with a cosmetic alias), or (d) a **Track 1 ≡
Track 2 dishonesty** (collapsing the structurally independent direct-vs-typed
cursors into one symbol-path-sharing primitive). The CHALLENGE-CONTEXT V1
focus narrows this to two load-bearing checks:

- P2-D concluded **substrate-union YES** at HEAD (3 active + 1 pre-blocked
  candidate). CH5 must verify P2-A/C/E/F candidates respect that conclusion.
- **C-P2C-2** (`pmull_cssc_structural_union_emit64`) carries an explicit
  Lock 1 dependency on P2-D's substrate-union YES verdict
  (`p2c-arch-esoterica.md:139,57`). CH5 must verify the dependency is
  honoured (the emitted projection IS the tape, not a sidecar) and that
  C-P2C-2's REDRESS 88/89/96-98 pre-block remains intact at V1.

### Headline verdict

**ACCEPT (6 of 6 axes).** Every active P2 candidate across P2-A/B/C/D/E/F
respects Lock 1 substrate-union at the symbol-path level: every candidate
either (i) consumes the existing single substrate (`Tape<'input>` +
`StructuralIndex` folded into `TapeBuilder.offsets` per
`p2d-substrate-tape.md:27`), (ii) returns transient mask/offset/value with
no retained state (`local_temp_only` substrate target per Lock 1 v+1
manifest at `LOCKS.md:73-82`), (iii) explicitly elides retention via
`BackendShape::SinkOnly` (the *removal* direction of substrate state per
`p2d-substrate-tape.md:102,116`), or (iv) is listed only as
REJECT-by-REDRESS-history paper-trail (C-P2D-4 `EventTape`,
`p2d-substrate-tape.md:134-141`). Zero candidates propose a parallel
classifier, sidecar event vector, second source scan, or Track 1 ≡ Track 2
collapse.

C-P2C-2's Lock 1 dependency on P2-D is **satisfied**: P2-D concludes
substrate-union YES per `p2d-substrate-tape.md:92,204`; C-P2C-2 stays
PRE-BLOCKED at V1 per its own §2 disposition (`p2c-arch-esoterica.md:44`)
pending an S-P3 Union-C wave with SIMD-first direct tuple writeback that
**deletes** the scalar consume path. The "fresh material differential =
consumer-shape change, not host-pin reasoning" clause at
`p2c-arch-esoterica.md:67` is the binding REDRESS 88/89/96-98 escape
valve, and C-P2C-2's V1 disposition correctly defers admission to S-P3.

ACCEPT-rate per axis (6 axes): 6/6 = **100 %** ACCEPT, 0/6 = 0 % REVISE,
0/6 = 0 % REJECT.

## §2 — Per-artefact CH5 disposition table

The CH5 audit per artefact: does any candidate primitive imply (a) parallel
substrate walking the same bytes via a second classifier, (b) a sidecar event
vector the parser writes outside the offset-tape, (c) a renamed scanner / a
retained cursor whose lifetime spans parse iterations, (d) a second source
scan over the input, or (e) a Track 1 ≡ Track 2 symbol-path collapse?

| Artefact | (a) parallel substrate? | (b) sidecar producer? | (c) renamed scanner / retained cursor? | (d) second source scan? | (e) Track 1 ≡ Track 2 collapse? | CH5 verdict |
|---|---|---|---|---|---|---|
| `p2a-sota-teardown.md` (367 lines; 7 candidates C1-C7) | No — C1 (`lazy_field_skip_with_index`) and C5 (`structural_index_singular_substrate_consumer`) explicitly **consume the existing `StructuralIndex` produced by `scan_structurals`** rather than introducing a parallel scan (`p2a-sota-teardown.md:120,155-156`). C5's substrate-target is `existing_tape` (not a new substrate); per §4 risks (line 234) C5 is "the **opposite** of REDRESS 96/97/98 — it removes parallel-substrate consumers by re-routing existing consumers to the existing producer". C2/C3/C7 return transient SIMD masks consumed inside the chunk loop (`local_temp_only` per §4 substrate-union risks, line 249). | No — C1/C5 do not retain new state; C2/C3/C7 SIMD masks are transient producers per §4 substrate-union risks (line 249) "the SIMD masks are transient producers, not retained sidecars". The Lock 1 declaration triple per candidate (line 249) names `local_temp_only`/`local_loop`/`generated_grammar` for C2/C3/C7 and `existing_tape`/`output_row`/`generated_grammar` for C5. C4 (force-inline) and C6 (parse-attribution) are build invariants with no substrate footprint. | No — `lazy_field_skip_with_index` skips by *advancing the cursor* against the existing positions Vec, not by spawning a second cursor (`p2a-sota-teardown.md:115-117`); the scalar reference walks the positions Vec, not the source bytes. C5 unifies the existing direct/typed cursors as consumers of the *same* `StructuralIndex` — a cursor-collapse-toward-substrate-union, not a cursor-fork. | No — C1/C5 are explicitly the substrate-union shape: "the structural projection IS the tape; the projection cannot be a retained sidecar … but it must be the **single** substrate the envelope consults" (`p2a-sota-teardown.md:33` simdjson row). The single classifier pass is preserved; only consumer wiring changes. | No — `bbnf_bench::generated_real_typed::DirectParser` (Track 2 typed) and `runtime::generated_json::generated::parse_object_value_at_direct::<JsonDigestSink>` (Track 1 direct) remain structurally independent. C5 unifies *Track 1*'s direct + typed envelopes onto Track 1's structural index; Track 2 is out of scope of C5 by construction (Track 2 has its own `DirectParser::cursor` per `p2d-substrate-tape.md:70`). | **ACCEPT** |
| `p2b-dav1d-process.md` (217 lines; 5-stage admission process) | No — the admission process itself is structural infrastructure (Stage A scalar-reference, Stage B checkasm, Stage C corpus parity, Stage D wave-close, Stage E manifest+substrate). Stage E's `substrate_target` column **rejects-by-construction** any candidate naming `parallel_substrate` or any value outside `{local_temp_only, existing_tape, direct_sink, admitted_fact_output}` per `p2b-dav1d-process.md:122,160`. | No — Stage E's `retention_lifetime` column rejects any value outside `{local_loop, generated_function, output_row}`; a parser-owned cursor would require `retention_lifetime = parser_owned`, which is not in the allowlist and Stage E rejects it (`p2b-dav1d-process.md:170`). Same-wave consumer rule (line 20) explicitly forbids landing a primitive in wave N to be wired in wave N+3 — closes the sidecar-producer-without-consumer gap. | No — Stage E manifests do not name renamed scanners; the manifest-row schema (`LOCKS.md:309-318`) is sixteen-column attribution including stable primitive id + abstract primitive name + ISA citation + scalar reference + same-wave production consumer (`p2b-dav1d-process.md:114-122`). A renamed scanner would fail Stage E's `policy_owner` column check. | No — the process is admission infrastructure, not a parse path. | No — process is plane-blind; per §2 (line 160) "Generic, with a grammar-neutrality column. The manifest's `grammar policy source` column names 'generated grammar config' or 'caller data' or 'none'; never names a specific grammar". Track 1 / Track 2 separation is enforced via the `track2_entry_point` schema column (R2 wave) which Stage E references as the per-row cohesion check. | **ACCEPT** |
| `p2c-arch-esoterica.md` (143 lines; 8 instruction-route candidates C-P2C-1..8) | **Conditional ACCEPT — C-P2C-2 is the load-bearing dependency.** C-P2C-2 (`pmull_cssc_structural_union_emit64`) explicitly stays PRE-BLOCKED at V1 per `p2c-arch-esoterica.md:44` "PRE-BLOCKED at SK-V14 V1 by REDRESS 88 + 89 + 96-98 unless S-P3 dispatches a Union-C wave with: (a) SIMD-first direct tuple writeback that DELETES the current scalar consume path (not adds-alongside); … (c) Lock 1 substrate union held per P2-D". P2-D's `p2d-substrate-tape.md:92,204` concludes substrate-union YES — so condition (c) is satisfied — but conditions (a)/(b)/(d) defer admission to S-P3. The V1 disposition correctly stays PRE-BLOCKED. C-P2C-7 (`byte_context_orphan_resolution`) is `NOT-S-P3-ELIGIBLE` standalone; it folds into C-P2C-5 consumer or hygiene deletion — no parallel substrate. C-P2C-1/4/5/8 are scalar/measurement/instruction primitives with no parallel-substrate footprint. C-P2C-3 (UDOT) returns digit-lane decode values to the consumer — no substrate add. C-P2C-6 (EOR3) returns mask algebra — no substrate add. | No — per §4 risks (lines 70-72) "REDRESS 96/97/98 … Any Union-C route must cite these and name the material differential: SIMD-first direct tuple writeback, not class-column, streaming cursor, or class-lane-only substrate"; and "REDRESS 60-72 (SK-V6 retained-parse + sidecar producers): NO sidecar event vector, parser-owned cursor, or decoded-string stats sink. C-P2C-2 and C-P2C-5 must preserve Lock 1 and generated-grammar ownership". C-P2C-2's emitted class+position tuple IS the tape per §2 line 44 ("the emitted tuple is the tape, not a sidecar"). | No — C-P2C-2 explicitly forbids "Local body-fill of the scalar delegates remains REJECTED" (line 44) — the route is not a renamed `bitmap_prefix_xor_64_neon` body fill; it is a consumer-shape change. The Item 88 / Item 89 falsification mode is the structural test: a candidate that only renames the scalar delegate fails the same regressions Item 88 measured (instruments -4.62 %, numbers -10.04 %, unicode_escapes -12.66 % / -15.52 %). C-P2C-2's V1 PRE-BLOCK posture honours this. | No — C-P2C-2 builds the structural-position matrix from the *existing* 64-byte mask emitted by the *existing* NEON scan (`scan.rs:200-275`); the SIMD-first union consumer **deletes** the subsequent scalar consume step rather than adding a second scan. The "no second source scan" property is exactly the Item 88/89 mitigation. | No — C-P2C-2 lives inside `bbnf-simd::aarch64::*` (Layer 1) with consumer in `runtime::generated_json::*` (Track 1 substrate); does not touch `bbnf_bench::generated_real_typed::*` (Track 2). The §2.1 candidate-list discipline footnote at line 144 forbids parallel substrate proposals; the active C-P2C-1..8 set respects this. | **ACCEPT** (V1 PRE-BLOCK posture honours all CH5 invariants; admission deferred to S-P3 Union-C wave) |
| `p2d-substrate-tape.md` (257 lines; 3 active + 1 pre-blocked) | No — the entire artefact is the CH5 disposition primary source for V1. §1.1 (line 27) verifies "There is no sibling substrate crate, no parallel offset stream, no second tape, no retained `Vec<JsonEvent>`. … `grep -rn 'struct.*Tape\b' skinny/crates/runtime/src/` returns three hits — `Tape<'input>`, `TapeBuilder<'input>`, `TapeId` … one substrate." §1.5 (line 86-92) explicitly architecturally-blocks any new union variant per Pass Omega V1.1 / SK-V13 receiver: "the substrate-walk-with-shape-validation framing IS that architectural block: there is no new union variant to admit". §4.7 (line 204) explicitly carries the CH5 cross-check: "CH5 dispose: ACCEPT for all 3 active candidates". | No — C-P2D-1 (`SinkOnly` activation) *removes* a retained substrate (elides `TapeBuilder` construction; `p2d-substrate-tape.md:116`); C-P2D-2 (`OffsetTapeStats` column) re-uses existing fields; C-P2D-3 (sparse-flag gating) gates existing fields' construction. None add a sidecar. C-P2D-4 is explicitly listed as REJECT-by-history REDRESS 96/97/98 anti-pattern reference (line 134-141), not as a candidate. | No — C-P2D-1 does the *opposite* of cursor retention: it elides the `TapeBuilder` cursor that the Track 1 `JsonDigestSink` consumer never asks for. §1.3 (lines 67-74) re-verifies the two-cursor independence at HEAD: Track 1 cursor lives entirely inside `runtime::generated_json::*` (`parser.rs:10` + 12 inline `cursor: &mut usize` signatures); Track 2 cursor lives entirely inside `bbnf_bench::generated_real_typed::*` (`generated_real_typed.rs:2742-2746`). Neither cursor is renamed in any P2-D candidate. | No — single substrate, single scan. The `attach_structural_index` call (`scan.rs` plus the generated wire) folds the scan output into `TapeBuilder.offsets` (line 29); no second walk. | No — §1.3 (lines 67-74) re-verifies the two cursors "do not share a module path beyond the crate-graph root. Neither cursor calls into the other's parse path. Both cursors index the same source slice (`&'i [u8]`) — they share the substrate, not the producer." The substrate-union framing is correctly named per Lock 1's 2026-05-04 reframe (`LOCKS.md:48`). C-P2D-1's SinkOnly activation is per-rule cost-model selection, not a Track-collapse. | **ACCEPT** |
| `p2e-parse-that-gaps.md` (342 lines; 8 Layer-1 primitive gaps) | No — §1.3 (line 76) makes the substrate-union constraint explicit: "Every gap below MUST emit, if it carries a position-returning interface, into the SAME tape that `bulk_emit_positions_64` feeds (per `skinny/crates/bbnf-simd/src/lib.rs:227` `compact_mask`). A new primitive that proposes a separate position-table, a parser-local cursor, or a sidecar event vector violates Lock 1 (substrate-union); per P2-D and CH5, such a candidate is REJECTed." Every gap (1-8) either (a) returns offset/value/width with no position emit, or (b) returns a bitmask the existing `compact_mask` consumer folds into the shared tape (`p2e-parse-that-gaps.md:76-77`). | No — §4.2 (line 268): "every gap above either returns a mask … folded into the existing `compact_mask` consumer or returns an offset / width / value … with no position emit. Zero gaps introduce a second position vector, a parallel cursor, or a sidecar event ring. **CH5-compliant by construction.**" Per-gap substrate-union annotations at lines 109, 123, 139, 153, 169, 185, 201, 215, 229. | No — every gap is Layer-1 SIMD primitive (vectorisation of existing SWAR-8 inner loops at `parse-that-regex/src/lib.rs:510-530, 565-572, 814-820` per gap 1, line 99). No renamed scanner; consumer wiring is the gap, not a scanner rename. | No — gap 1's `_sweep` driver processes 64 bytes at a time across 4 NEON registers, replacing the existing 8-byte SWAR scalar walk (line 99). One walk, wider window. Gap 6 explicitly composes with `classify_tbl4::classify_block_from_table` (the existing in-substrate-union classifier per line 264) — no new column substrate. | No — gaps live in `parse-that-regex` Layer-1 (used by both Track 1 and Track 2); but the consumer-wiring per gap names the Track-1 callsite (`scan.rs`, `lib.rs`) without touching Track 2's `generated_real_typed.rs`. The CH5 plane is "no symbol-path overlap between Track 1 generated runtime and Track 2 oracle"; gap-level primitives shared at Layer 1 are explicitly allowed by Lock 14 (cross-grammar reusable primitives) — they are not Track 1 ≡ Track 2 collapse because the consumer-wiring stays separate. | **ACCEPT** |
| `p2f-grammar-neutral.md` (333 lines; 14 candidates ALL clear Lock 14) | No — §1.3 (line 54) makes the holding assumption explicit: "P2-F assumes the conclusion is YES (the SK-V14 dispatch context §1 binds this … under that assumption, any candidate primitive that touches the tape touches the *single* substrate; any candidate that proposes a second source scan, retained cursor, aux density table, or parser-owned structural projection violates Lock 1 and is REJECTed per CH5." Per-candidate substrate-target labels at §2.* are `local_temp_only` (C2/C3/C5/C7/C8/C10/C12/C13), `existing_tape` (C1/C9/C11), `direct_sink` (C4/C5/C6) — all in the Lock 1 v+1 allowlist. | No — Stage E manifest enforcement at `p2b-dav1d-process.md:170` is the binding gate; P2-F's per-candidate substrate-target enumeration honours the four enumerated values. C11 (`substrate-walk-with-shape-validation`) explicitly carries `existing_tape` substrate-target per line 183; "S-P2 must not split it into two separate primitives per P1-E §4.4 — it is a single substrate-union primitive" (line 182). | No — C11 is the typed-skip primitive (the `DirectParser::skip_value` substrate-walk per `generated_real_typed.rs:2949`); the candidate explicitly forbids splitting it. No primitive in §2 is a renamed scanner. | No — per-candidate scan/dispatch primitives compose with the existing classifier; §4.1 risk table (lines 256-272) verifies REDRESS 60-72 (retained-parse + sidecar) and REDRESS 96-98 (production-union substrate) are not re-opened. | No — §1.3 (line 54) explicit foreclose. C11 admits as a single primitive precisely to *prevent* the Track-1 ≡ Track-2 collapse a split would create (P1-E §4.4 Lock 14 disallows JSON-specific naming of the unified primitive; the unified primitive *is* the substrate-union honest framing). §4 CH5 risk on C11 (line 279): "substrate-union assumption. P2-F's verdicts depend on P2-D concluding YES on the substrate union. … The V2 CHALLENGE fold must re-verify §2.11 against P2-D output once committed." — P2-D HAS concluded YES (`p2d-substrate-tape.md:92,204`); C11's substrate-target slip-risk is closed. | **ACCEPT** |

Per-axis ACCEPT-rate: 6/6 ACCEPT, 0/6 REVISE, 0/6 REJECT → **100 % ACCEPT** on
the CH5 lens.

## §3 — Critical findings

### Finding CH5-A — C-P2C-2's Lock 1 dependency on P2-D is satisfied; pre-block posture is correct

C-P2C-2 (`pmull_cssc_structural_union_emit64`) at
`p2c-arch-esoterica.md:44` carries four S-P3 admission gates:

> (a) SIMD-first direct tuple writeback that DELETES the current scalar
> consume path (not adds-alongside); (b) strict same-row non-regression on
> the 11 row set Item 88/89 falsified (`instruments`, `numbers`,
> `unicode_escapes` parse-only + direct; `canada`, `citm_catalog`,
> `marine_ik`, `mesh` parse-only Track 1 + 2); (c) Lock 1 substrate union
> held per P2-D; (d) emitted-asm proof of `pmull.1q` and `ctz` per SK-V7
> W10b template.

P2-D concluded **substrate-union YES** at HEAD per
`p2d-substrate-tape.md:92` ("S-P2-D's substrate-union verdict … the
substrate union holds at HEAD; tape + structural projection ARE one
substrate") and reaffirmed at `:204` ("§1.1 + §1.3 + §1.5 jointly conclude
**YES, the substrate union holds at HEAD**"). Condition (c) is therefore
satisfied at the dispatch level.

Conditions (a) and (b) and (d) remain S-P3 wave deliverables. C-P2C-2's V1
disposition correctly stays PRE-BLOCKED at V1; the wave plan must dispatch
a Union-C wave with the SIMD-first direct tuple writeback that **deletes**
the scalar consume step. The "fresh material differential = consumer-shape
change, not host-pin reasoning" clause at `p2c-arch-esoterica.md:67`
forbids the SK-V14 wave from re-opening REDRESS 88/89 simply by citing
"SK-V14 host is M5 Max, not the SK-V7 host" — the differential MUST be the
consumer-shape change.

The §5.6 fold target at `p2c-arch-esoterica.md:139` is correctly closed:
"depends on P2-D substrate-union conclusion. If P2-D concludes the substrate
union does NOT hold at SK-V14 HEAD … C-P2C-2 cannot ship under Lock 1 and
must be re-framed or REJECTed. Fold target: read P2-D output and adjust §2
disposition." — P2-D's YES verdict matches the C-P2C-2 disposition; no
re-framing needed at V1. Severity: NONE (admission posture correct).

### Finding CH5-B — substrate-target taxonomy is consistently applied across P2-D / P2-F / P2-B

Lock 1 v+1 manifest at `LOCKS.md:73-82` defines the substrate-target
enumeration `{local_temp_only, existing_tape, direct_sink,
admitted_fact_output}`, retention-lifetime `{local_loop, generated_function,
output_row}`, and policy-owner `{generated_grammar, caller_data, none}`.

P2-F per-candidate annotations (§2.1-§2.13) consistently use these values:
C1 (`existing_tape` / structural bitmap → tape positions); C2/C3/C5/C7/C8/C10
(`local_temp_only`); C4 (`direct_sink`); C9 (`existing_tape`); C11
(`existing_tape`); C12/C13 (`local_temp_only`); C14 N/A. Zero candidates use
out-of-allowlist values.

P2-D per-candidate (§2.* C-P2D-1/2/3/4) names the substrate primitive as the
existing `Tape<'input>` / `TapeBuilder<'input>` / `OffsetTapeStats` / `flag_*`
fields (`p2d-substrate-tape.md:112-130`). C-P2D-4 is explicitly out-of-scope
(REJECT-by-history).

P2-B's Stage E (`p2b-dav1d-process.md:114-124`) enforces the manifest schema
per `LOCKS.md:309-318` (sixteen named columns including `substrate target`,
`retention lifetime`, `policy owner`); rejects any value outside the
allowlists by construction.

The taxonomy is uniformly applied. No candidate slips. Severity: NONE
(audit pass-through; taxonomy honoured).

### Finding CH5-C — C5 (`structural_index_singular_substrate_consumer`) is the substrate-union completion candidate; CH5 explicitly recommends it

P2-A's C5 (`p2a-sota-teardown.md:153-161`) is the **substrate-union
honouring direction**: it removes the parallel-substrate-via-cursor pattern
that REDRESS 96/97/98 added, by re-routing the direct + typed envelopes to
consume the **existing** `StructuralIndex` produced by `scan_structurals`
instead of re-scanning bytes.

Per §4 (line 234): "C5 (substrate-union completion) is the **opposite** of
these routes: it removes parallel-substrate consumers by re-routing existing
consumers to the existing producer. Substrate-target = `existing_tape`; no
new retention; no class column. The Lock 1 v+1 contract (`LOCKS.md:73-82`)
admits C5 because retention_lifetime = `output_row` and policy_owner =
`generated_grammar`."

The P1-E §4.4 substrate-union framing (`p1e-hot-leaf-attribution.md:246`
"`skip_value` is `substrate` + `dispatch` in equal parts") is the load-bearing
antecedent. C5 operationalises the framing: the typed-plane skip and the
direct-plane skip consume the same singular substrate the parse-time scan
populates.

P2-F's C11 (`p2f-grammar-neutral.md:174-183`) is the same primitive at the
grammar-neutral abstraction layer: "the typed-plane rank-1 is 'a substrate-
union observation: it is neither a pure substrate primitive (the offset
tape) nor a pure producer primitive (typed-value construction), but a
hybrid that walks the substrate while validating type-shape.' Under Lock 1
v+1 the candidate is `existing_tape` substrate-target with `output_row`
retention lifetime and `generated_grammar` policy_owner. **S-P2 must not
split it into two separate primitives** per P1-E §4.4 — it is a single
substrate-union primitive."

The CH5 reading is: C5 / C11 are **load-bearing for substrate-union
honesty**. The S-P3 wave plan should sequence C5 / C11 admission ahead of
or simultaneously with C-P2C-2 (Union-C) so the Union-C consumer-shape
change has a consumer to wire into. Severity: NONE for V1 (research
identifies; admission is S-P3); MED for S-P3 wave sequencing (without C5 /
C11 landing, C-P2C-2's "SIMD-first direct tuple writeback that DELETES the
scalar consume path" has no per-rule consumer to write into).

### Finding CH5-D — P2-E gap 1 (`scan_string_special_64_sweep`) explicitly preserves the tape emit at unchanged callsite

P2-E gap 1 (`p2e-parse-that-gaps.md:99-109`) replaces the SWAR-8 inner
loops at `parse-that-regex/src/lib.rs:510-530, 565-572, 814-820` with a
64-byte NEON sweep. The substrate-union annotation at line 109:

> Returns offset + flags; emits no positions; no parallel substrate.
> The caller's tape emit at `runtime/src/grammars/json/scan.rs` is
> unchanged.

This is the right shape: the SIMD primitive returns transient values to the
caller; the caller's tape emit (the position write into the shared offset
tape) is unchanged. Every P2-E gap (1-8) follows this pattern per §4.2
(line 268).

The CH5 reading is: P2-E gaps are **CH5-compliant by construction**. The
"returns mask or offset; never emits positions outside the existing tape"
contract is the structural answer to the "parallel substrate / sidecar
producer / renamed scanner" risk. Severity: NONE (audit pass-through).

### Finding CH5-E — no new union variant proposed; P2-D's architectural block is the binding closure

Per Pass Omega V1.1 / SK-V13 substrate receiver
(`SUBSTRATE.md:33-41` cited at `p2d-substrate-tape.md:86`): "one substrate
remains binding, but SK-V13 must admit or architecturally block a new union
variant distinct from REDRESS 96/97/98."

P2-D explicitly architecturally-blocks any new union variant
(`p2d-substrate-tape.md:92`): "S-P2-D's substrate-union verdict is therefore
not 'find a new union variant'; it is 'the substrate union holds at HEAD;
tape + structural projection ARE one substrate; the producer asymmetry
between Track 1 (writes tape) and Track 2 (skips, no tape write) is the
projection-plane discriminant Lock 1 admits, not a substrate split.'"

C-P2D-4 (`EventTape`) is listed *only* as the pre-blocked anti-pattern
reference (lines 134-141): "**NOT a candidate at S-P2-D**: REDRESS 96 and
REDRESS 97 are the two faithful implementations … Both regressed uniformly
across W3 and W10b. REDRESS 98 retires the union-substrate thesis on this
host."

The CH5 reading is: no P2 candidate proposes a new union variant. The
substrate-union shape is closed; the active candidate set (P2-A C1-C7,
P2-C C-P2C-1..8, P2-D C-P2D-1/2/3, P2-E gaps 1-8, P2-F C1-C14) all respect
the closure. The Lock 1 v+1 substrate-ceiling fold (`LOCKS.md:84-90`) is
honoured. Severity: NONE (closure verified).

### Finding CH5-F — track2_entry_point column gap carries forward from P1 V1; not a P2 CH5 fault

S-P1 V1 CHALLENGE CH5 (`research/p1/hardening/V1/CH5.md:46-55`) noted the
`track2_entry_point` schema column is absent from `RESULTS.md` and from
every per-row table; the population was deferred to the C-2 (R2) wave.

P2 candidate primitives respect Track 1 / Track 2 separation by inspection
at the symbol-path level: every P2-A/C/D/E/F candidate either lives in
Track 1's substrate (`runtime::generated_json::*`,
`runtime::tape::*`, `bbnf-simd::*` Layer-1 consumed by Track 1) or
explicitly names the typed-plane skip primitive (C11 in P2-F, C5 in P2-A)
which is the *same* primitive at the grammar-neutral layer — not a
Track-collapse but a substrate-union honouring unification.

The CH5 reading is: P2 V1 inherits the P1 V1 column gap; mechanical
enforcement remains a downstream wave deliverable. P2's narrative coverage
correctly maintains Track 1 / Track 2 separation. Severity: LOW (column
gap is C-2 wave; P2 does not regress on it).

## §4 — V2 fold recommendations

The V1 → V2 fold for CH5 requires three narrowly-scoped actions:

1. **CH5-A C-P2C-2 wave sequencing.** S-P3 wave plan must sequence the
   Union-C wave AFTER (or simultaneously with) C5 / C11 substrate-union
   completion landings, so C-P2C-2's "SIMD-first direct tuple writeback
   that DELETES the scalar consume path" has a per-rule consumer to write
   into. If C5 / C11 do not land first, C-P2C-2 has no consumer to wire
   the deleted scalar consume step into. (MED; wave-sequencing question
   for S-P3.)

2. **CH5-C substrate-target manifest schema mechanisation.** P2-B Stage E
   manifest enforcement is described prose-level; the V2 fold should
   surface whether the sixteen-column schema is materialised as a CSV /
   TOML / YAML artefact that `xtask` validates per-wave, or whether the
   schema remains prose-only at S-P2 V1. If prose-only, the V2 CHALLENGE
   CH5 cannot mechanically reject a candidate that names an
   out-of-allowlist substrate-target — only structurally reject. (LOW;
   tooling deliverable for S-P3.)

3. **CH5-F track2_entry_point column population.** When the C-2 (R2) wave
   lands the `track2_entry_point` column on `RESULTS.md`, the V_C-2_+1
   CHALLENGE CH5 mechanically enforces the no-cross-ancestor guard per
   row. Until then, P2 V1's Track 1 / Track 2 separation discipline is
   narrative-enforced only. (MED; downstream C-2 deliverable; P2 cannot
   deliver unilaterally.)

## §5 — Sources cited (executable-verification)

Verified per CHALLENGE-CONTEXT §3 "Executable-verification mandate":

- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CHALLENGE-CONTEXT.md` — read end-to-end (33 lines).
- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md` (367 lines) — §1.1-§1.5 + §2 C1-C7 + §3 grammar-neutrality table + §4 risks + §5 sources read.
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md` (217 lines) — §2.A-§2.E stages + §4 risks (Stage E substrate-target / retention-lifetime / policy-owner enforcement) read.
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md` (143 lines) — §1 findings 1-10 + §2 C-P2C-1..8 table + §4 REDRESS 88/89/96-98 risks + §5.6 V1→V2 fold targets (C-P2C-2 P2-D dependency) read end-to-end.
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md` (257 lines) — §1.1-§1.6 substrate-union findings + §2 C-P2D-1/2/3/4 + §3 grammar-neutrality + §4.1-§4.7 risks (CH5 cross-check at §4.7) read end-to-end.
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md` (342 lines) — §1.3 Lock-1 substrate-union constraint + §2 gaps 1-8 substrate-union annotations + §4.2 CH5 binding read.
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md` (333 lines) — §1.3 substrate-union holding assumption + §2 C1-C14 per-candidate substrate-target labels + §4 REDRESS risks + §4 CH5 risk on C11 read.
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH5.md` (222 lines) — P1 V1 CH5 verdict carry-forward for `track2_entry_point` column gap.
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH5.md:78-83` (cited via P2-D §1.3) — two-cursor independence verification.
- `restart/locks/LOCKS.md:48-90` — Lock 1 substrate-union + v+1 substrate-ceiling fold + manifest triple (substrate_target / retention_lifetime / policy_owner) + REDRESS 96/97/98 binding-history clause.
- `restart/locks/LOCKS.md:309-318` — sixteen-column primitive manifest schema (cited via P2-B §2.E).
- `restart/prompts/skinny/PASS-2-RESEARCH.md §3 CH5` (lines 125-131; cited via P2-D §5.3) — CH5 lens binding.
- `restart/prompts/skinny/PASS-2-RESEARCH.md §8.6` (lines 236-240; cited via P2-D §5.3) — substrate union closing pin.

Source-code verification (CH5-A, CH5-D, CH5-E framing — verified via
P2-D's executable-verification at HEAD; P2-D §1.1 line 27 cites
`grep -rn "struct.*Tape\b" skinny/crates/runtime/src/` → three hits;
P2-D §1.3 lines 67-72 verifies two-cursor independence at
`parser.rs:10` + `generated_real_typed.rs:2742-2746`):

- `skinny/crates/runtime/src/tape/mod.rs:94-101` — `Tape<'input>` canonical retained substrate (5 backing fields; verified single).
- `skinny/crates/runtime/src/tape/assembler.rs:42-122` — `TapeBuilder<'input>` parse-time builder facade + `from_offsets` ownership transfer (verified single substrate, no shadow).
- `skinny/crates/runtime/src/grammars/json/parser.rs:7-12` — `ParserState<'i>` carries `tape: TapeBuilder<'i>` (single substrate; no parallel `structural_offsets` field).
- `skinny/crates/runtime/src/grammars/json/scan.rs:22` — `scan_structurals` (one classifier pass).
- `skinny/crates/runtime/src/grammars/json/generated.rs:466-502` — `parse_object_value_at_direct::<S>` Track 1 direct envelope; threads `cursor: &mut usize`.
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2742-2746` — `DirectParser { input, bytes, cursor: usize }` Track 2 typed substrate (structurally independent of Track 1).
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2949-3003` — `DirectParser::skip_value` / `skip_object` / `skip_array` walks `self.bytes` via `self.cursor` only; no Track 1 substrate touch.
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1-5` — scalar delegate (REDRESS 88 pre-block surface; C-P2C-2 must NOT replace this body locally).
- `skinny/REDRESS.md:2508-2540` (Item 88), `:2542-2585` (Item 89), `:2587-2618` (Item 90), `:2797-2906` (REDRESS 96/97), `:2910-2950` (REDRESS 98) — substrate-ceiling history pre-block surface honoured by C-P2C-2 V1 PRE-BLOCK posture.
