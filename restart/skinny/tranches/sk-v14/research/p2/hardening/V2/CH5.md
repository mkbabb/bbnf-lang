# SK-V14 S-P2 V2 CHALLENGE — Lens CH5 — HIDDEN COUPLING

Author: CH5 lens agent, S-P2 CHALLENGE V2.
Date: 2026-05-23.
Scope: lens CH5 — HIDDEN COUPLING — across the six P2 artefacts at V2 cycle
(`p2a-sota-teardown.md` V1-LOCKED, `p2b-dav1d-process.md` V2 amended,
`p2c-arch-esoterica.md` V2 amended, `p2d-substrate-tape.md` V2 amended,
`p2e-parse-that-gaps.md` V1-LOCKED, `p2f-grammar-neutral.md` V2 amended).
Binding: `PASS-2-RESEARCH.md §3 CH5` ("no parallel substrate / sidecar
producer / renamed scanner / Track 1 ≡ Track 2 dishonesty") +
`ORCHESTRATOR.md §3W` CH5 + `PASS-2-RESEARCH.md §8.6` substrate-union
closing pin + `LOCKS.md:48-90` (Lock 1 + v+1 substrate-ceiling fold +
REDRESS 96/97/98 binding-history clause) + V2 CHALLENGE-CONTEXT §2 CH5
focus (substrate-union YES still holds; P2-D §1.6(d) demotion did not
break union claim; verify §4.7 V2-demoted clean-by-construction note;
no parallel substrate introduced by demotions; P2-F §2.Y cross-axis
tracking note surfaces hidden coupling for S-P3 consolidation;
F-V2-P1ABC-RERECORD dependency tracking does not introduce
envelope-masking renamed-scanner risk). Discipline: write-only,
`path:line` per claim, executable-verification mandate, HARD CAP 30 min.
Inheritance: V1 CH5 ACCEPT 6/6 (100 %) at commit `e1a8bb8b7e`;
this V2 cycle audits whether the four V2-amended axes preserve V1
CH5 invariants and whether new cross-axis surfaces (P2-F §2.Y, P2-D
§1.6(d), F-V2-P1ABC-RERECORD) introduce new hidden-coupling risk.

## §1 — V2 CH5 disposition summary

CH5 asks whether any P2 candidate primitive at V2 proposes (a) a
**parallel substrate** (a second classifier walking the same source), (b)
a **sidecar producer** (a parser-owned vector / cursor / map outside the
offset tape), (c) a **renamed scanner** (a second source scan with a
cosmetic alias), or (d) a **Track 1 ≡ Track 2 dishonesty** (collapsing
the structurally independent direct-vs-typed cursors into one
symbol-path-sharing primitive). The V2 CHALLENGE-CONTEXT focus narrows
the V1 surface to four load-bearing V2 checks:

1. **Substrate-union YES still holds** at HEAD after the P2-D §1.6(d)
   demotion (C-P2D-3 moved from active candidate to substrate-side
   observation). Specifically: does the demotion break the §1.1 + §1.3
   + §1.5 union conclusion at `p2d-substrate-tape.md:92,204`?
2. **No parallel substrate introduced by demotions** (P2-C C-P2C-1/6/7
   to §2.X non-candidate inventory; P2-D C-P2D-3 to §1.6(d); P2-F C8 to
   §2.X.1). Verify §4.7 V2-demoted clean-by-construction note holds.
3. **P2-F §2.Y cross-axis tracking note** (NF-CH6-4 long-string-body SIMD
   scan consolidation across P2-A C2, P2-E Gap 1, P2-F C1+C2) surfaces
   hidden coupling for S-P3 consolidation — this is the *anti-paper-close
   exemplar*, not a CH5 violation; verify the note correctly flags
   three-name-one-primitive for S-P3 binding, not three orthogonal SIMD
   bodies.
4. **F-V2-P1ABC-RERECORD dependency tracking** (the parse-attribution
   profile-rebuild gate per P2-C C-P2C-8 + P2-F C6 cargo feature gating)
   does not introduce envelope-masking renamed-scanner risk.

### Headline V2 verdict

**ACCEPT (6 of 6 axes).** Every active V2 candidate across P2-A/B/C/D/E/F
respects Lock 1 substrate-union at the symbol-path level; the V2
demotions strengthen rather than weaken CH5 posture (fewer candidates =
fewer surfaces to audit, and the four demotions all carry
substrate-clean disposition stamps). The P2-F §2.Y cross-axis tracking
note is exemplary anti-paper-close discipline (CH6 win) AND simultaneously
a CH5 hidden-coupling discovery surfaced for S-P3 binding (CH5 win) —
it does not violate CH5; it is the artefact of CH5 discipline working
as designed. F-V2-P1ABC-RERECORD is a process-gate / measurement
deliverable that toggles the existing `cfg_attr(feature =
"parse-attribution", inline(never))` plumbing at
`runtime/src/grammars/json/generated.rs:43-44` (verified at HEAD); it
does not introduce a renamed scanner or any substrate change. V2
ACCEPT-rate: 6/6 = **100 %**, 0/6 REVISE, 0/6 REJECT. V2 inherits
V1's 100 % ACCEPT cleanly; V2 → V3 → LOCK trajectory predicted at the
first ≥95 % gate per §3Z (this is the V2 100 % cycle).

V1 → V2 CH5 delta: zero new violations introduced; four V2 demotions
preserve substrate-clean technical content verbatim (verified §3
below); one V2-NEW cross-axis tracking note (P2-F §2.Y) discovers
hidden coupling across three axes and binds S-P3 to ONE canonical
primitive name — the discovery IS the CH5 lens working correctly, not a
violation.

## §2 — Per-artefact V2 CH5 disposition table

The V2 audit per artefact: does any candidate primitive imply (a)
parallel substrate walking the same bytes via a second classifier, (b)
a sidecar event vector the parser writes outside the offset-tape, (c) a
renamed scanner / a retained cursor whose lifetime spans parse
iterations, (d) a second source scan over the input, or (e) a Track 1 ≡
Track 2 symbol-path collapse?

| Artefact (V2 status) | (a) parallel substrate? | (b) sidecar producer? | (c) renamed scanner / retained cursor? | (d) second source scan? | (e) Track 1 ≡ Track 2 collapse? | V2 CH5 verdict |
|---|---|---|---|---|---|---|
| `p2a-sota-teardown.md` (V1-LOCKED at commit `e1a8bb8b7`; 367 lines; 7 candidates C1-C7; zero V2 drift confirmed via `git diff e1a8bb8b7 447a26b07 -- p2a-sota-teardown.md` returning empty) | No — V1 verdict holds unchanged: C1 (`lazy_field_skip_with_index`) and C5 (`structural_index_singular_substrate_consumer`) explicitly **consume the existing `StructuralIndex` produced by `scan_structurals`** (`p2a-sota-teardown.md:120,155-156`). C5's substrate-target is `existing_tape` (not a new substrate); per §4 (line 234) C5 is "the **opposite** of REDRESS 96/97/98 — it removes parallel-substrate consumers by re-routing existing consumers to the existing producer". C2/C3/C7 return transient SIMD masks (`local_temp_only` per §4 line 249). | No — V1 verdict holds: C1/C5 do not retain new state; C2/C3/C7 SIMD masks are transient producers (`local_temp_only` / `local_loop` / `generated_grammar` per §4 line 249). C4/C6 are build invariants. | No — V1 verdict holds: `lazy_field_skip_with_index` skips by *advancing the cursor* against the existing positions Vec, not by spawning a second cursor (`p2a-sota-teardown.md:115-117`). C5 unifies existing direct/typed cursors as consumers of the *same* `StructuralIndex`. | No — V1 verdict holds: C1/C5 are explicitly the substrate-union shape ("the structural projection IS the tape; the projection cannot be a retained sidecar … but it must be the **single** substrate the envelope consults" at `p2a-sota-teardown.md:33`). | No — V1 verdict holds: `bbnf_bench::generated_real_typed::DirectParser` (Track 2 typed) and `runtime::generated_json::generated::parse_object_value_at_direct::<JsonDigestSink>` (Track 1 direct) remain structurally independent. C5 unifies *Track 1*'s direct + typed envelopes onto Track 1's structural index; Track 2 is out of scope of C5 by construction. | **ACCEPT** (V1-LOCKED; zero drift confirmed) |
| `p2b-dav1d-process.md` (V2 amended at commit `447a26b07`; 217 lines; FFmpeg `08571418...` + dav1d `1718ff9a...` SHAs pinned at §5.1 lines 183-185 per V2 hardening context §1; V1-consolidator §5.4→§5.3 register correction noted) | No — V2 SHA pinning does not change the §2.E rejection-by-construction posture: Stage E's `substrate_target` column rejects any candidate naming `parallel_substrate` or any value outside `{local_temp_only, existing_tape, direct_sink, admitted_fact_output}` per `p2b-dav1d-process.md:122,170`. SHA pinning is bibliographic verification, not a primitive admission lever. | No — V2 SHA pinning does not affect Stage E's `retention_lifetime` column rejection of `parser_owned` (`p2b-dav1d-process.md:170`); same-wave consumer rule (line 20) unchanged. | No — V2 SHA pinning does not introduce a renamed scanner; the FFmpeg + dav1d SHAs are upstream-source citations pinning the canonical implementation of the differential harness pattern (`tests/checkasm/`). The bbnf-simd port at `crates/bbnf-simd/tests/checkasm_parity.rs:1-21` is unchanged. | No — process is admission infrastructure, not a parse path; V1 verdict holds. | No — process is plane-blind; `track2_entry_point` schema column R2 wave reference unchanged. | **ACCEPT** (V2 SHA pinning is bibliographic verification only; zero CH5 surface change) |
| `p2c-arch-esoterica.md` (V2 amended at commit `447a26b07`; 143→164 lines; 8→5 active candidates; C-P2C-1/6/7 demoted to `§2.X — Non-candidate inventory (zero P1-antecedent at SK-V14)` lines 48-72 with full technical content verbatim + disposition stamp per V2 hardening context §1) | **Conditional ACCEPT — V2 demotions strengthen CH5 posture.** V1 verdict on C-P2C-2 (`pmull_cssc_structural_union_emit64`) holds: PRE-BLOCKED at SK-V14 V2 per `p2c-arch-esoterica.md:42` (line content unchanged from V1) unless S-P3 dispatches a Union-C wave with (a) SIMD-first direct tuple writeback that DELETES the current scalar consume path; (b) strict same-row non-regression on Item 88/89 rows; (c) Lock 1 substrate union held per P2-D; (d) emitted-asm proof of `pmull.1q` and `ctz`. **V2 verification of condition (c):** P2-D V2 conclusion at `p2d-substrate-tape.md:92,204` still YES; §4.7 cross-check at line 199-201 explicitly re-affirms (see P2-D row below). C-P2C-1/6/7 demoted to §2.X non-candidate inventory at lines 48-72: technical content preserved verbatim (line counts indicate full content moved); each carries the disposition stamp "Demoted V2: zero S-P1 hot-leaf antecedent at SK-V14; re-evaluate if F-V2-P1ABC-RERECORD surfaces antecedent" (lines 69, 70, 71). **None of the demoted candidates propose parallel substrate**: C-P2C-1 is byte-set CSS delimiter primitive (`local_temp_only` mask + first-set extraction, no substrate add); C-P2C-6 is three-mask XOR fusion (mask algebra, no substrate add); C-P2C-7 is the orphan `byte_context` to be folded into C-P2C-5 consumer or deleted (no parallel substrate). The §2.X demotion explicitly preserves the CH5-clean posture; demotion is *removal from active enumeration*, not introduction of a new surface. | No — V2 demotions do not retain new state; the demoted candidates were already substrate-clean at V1 (per V1 CH5 §2 row for P2-C). Active C-P2C-2 maintains the V1 disposition: "the emitted tuple is the tape, not a sidecar" (line 42); explicit forbid of "Local body-fill of the scalar delegates remains REJECTED" (line 42). | No — V2 active set (C-P2C-2/3/4/5/8) carries no renamed scanner; C-P2C-2 builds the structural-position matrix from the *existing* 64-byte mask emitted by the *existing* NEON scan; C-P2C-8 is a process-gate (parse-attribution cargo feature toggle), not a scanner. | No — V2 active set does not propose a second source scan; C-P2C-2's V2 disposition preserves the "deletes the subsequent scalar consume step rather than adding a second scan" property (line 42). | No — V2 active set lives inside `bbnf-simd::aarch64::*` (Layer 1) + `runtime::generated_json::*` (Track 1 substrate); does not touch `bbnf_bench::generated_real_typed::*` (Track 2). The §2.X demotion explicitly removes 3 candidates from the active set without introducing any new Track-coupling surface. | **ACCEPT** (V2 demotions strengthen rather than weaken CH5 posture; C-P2C-2 PRE-BLOCK posture preserved verbatim; C-P2D-3 demotion in P2-D §1.6(d) does not break the C-P2C-2 → P2-D substrate-union dependency since the YES verdict at `p2d-substrate-tape.md:92,204` is unchanged) |
| `p2d-substrate-tape.md` (V2 amended at commit `447a26b07`; 257→254 lines; 3→2 active candidates; C-P2D-3 demoted to `§1.6(d)` substrate-side observation lines 104 + 110 + 128-130, with identifier stub gap-note in §2 per V2 hardening context §1) | No — V2 demotion of C-P2D-3 is the artefact's primary CH5 verification at V2. §1.1 (line 27) unchanged: "There is no sibling substrate crate, no parallel offset stream, no second tape, no retained `Vec<JsonEvent>`. … `grep -rn 'struct.*Tape\b' skinny/crates/runtime/src/` returns three hits — `Tape<'input>`, `TapeBuilder<'input>`, `TapeId` … one substrate." **Executable re-verification at V2 HEAD**: `grep -rn "struct.*Tape\b" skinny/crates/runtime/src/` returns three hits — `Tape<'input>` at `tape/mod.rs:94`, `TapeBuilder<'input>` at `tape/assembler.rs:42`, `TapeId(pub u64)` at `tape/mod.rs:92`. **Substrate-union YES still holds at HEAD post-V2-demotion.** §1.5 (line 86-92) unchanged: architectural-block of any new union variant preserved. §4.7 (line 199-201) V2-amended CH5 cross-check explicitly: "**ACCEPT for both active candidates (C-P2D-1, C-P2D-2); the V2-demoted §1.6(d) sparse-flag observation (formerly C-P2D-3) is CH5-clean by construction (re-uses existing substrate field, no new sidecar); the pre-blocked C-P2D-4 documents the anti-pattern for cross-checking.**" The V2-demoted §1.6(d) note (line 104) preserves C-P2D-3's technical content verbatim AND explicitly verifies CH5-cleanness: "**The substrate primitive is `Option<(Vec<u32>, Vec<u8>)>` or a `SmallVec`-style inline-2 store; structurally a one-liner change at `TapeBuilder::new`** … The `binary_search` consumer at `tape/mod.rs:144-150` is unchanged" — re-uses existing substrate field `flag_cursors`/`flag_values`, does NOT introduce a new substrate. | No — V2 active set: C-P2D-1 (`SinkOnly` activation) *removes* a retained substrate (elides `TapeBuilder` construction; line 116); C-P2D-2 (`OffsetTapeStats` column) re-uses existing fields. The demoted §1.6(d) sparse-flag observation does not retain a new sidecar even if re-elevated by a future same-wave consumer (per `p2d-substrate-tape.md:197` V2-amended footnote). C-P2D-4 remains REJECT-by-history paper-trail. | No — V2 active set's C-P2D-1 does the *opposite* of cursor retention; §1.3 (lines 67-74) V2-unchanged re-verifies the two-cursor independence at HEAD: Track 1 cursor at `parser.rs:10` (verified at HEAD: `pub cursor: usize` in `ParserState<'i>`); Track 2 cursor at `generated_real_typed.rs:2745` (verified at HEAD: `cursor: usize` inside `struct DirectParser<'i>`). Neither cursor is renamed in any V2 active candidate or the V2-demoted observation. | No — V2-unchanged: single substrate, single scan; the `attach_structural_index` call folds the scan output into `TapeBuilder.offsets` (line 29). | No — V2-unchanged §1.3 (lines 67-74) re-verifies the two cursors "do not share a module path beyond the crate-graph root. … Both cursors index the same source slice — they share the substrate, not the producer." C-P2D-1's SinkOnly activation is per-rule cost-model selection, not a Track-collapse. | **ACCEPT** (V2 demotion of C-P2D-3 to §1.6(d) preserves substrate-union YES; §4.7 V2-amended note explicitly verifies V2-demoted CH5-cleanness-by-construction; executable re-verification at HEAD confirms three-hit `struct.*Tape` set + two-cursor independence) |
| `p2e-parse-that-gaps.md` (V1-LOCKED at commit `e1a8bb8b7`; 342 lines; 8 Layer-1 primitive gaps; zero V2 drift confirmed via `git diff e1a8bb8b7 447a26b07 -- p2e-parse-that-gaps.md` returning empty) | No — V1 verdict holds unchanged: §1.3 (line 76) substrate-union constraint preserved ("A new primitive that proposes a separate position-table, a parser-local cursor, or a sidecar event vector violates Lock 1 (substrate-union); per P2-D and CH5, such a candidate is REJECTed"). Every gap (1-8) either returns offset/value/width with no position emit, or returns a bitmask the existing `compact_mask` consumer folds into the shared tape. | No — V1 verdict holds: §4.2 (line 268) "Zero gaps introduce a second position vector, a parallel cursor, or a sidecar event ring. **CH5-compliant by construction.**" | No — V1 verdict holds: every gap is Layer-1 SIMD primitive (vectorisation of existing SWAR-8 inner loops). | No — V1 verdict holds: gap 1's `_sweep` driver processes 64 bytes at a time across 4 NEON registers, replacing the existing 8-byte SWAR scalar walk. Gap 6 explicitly composes with `classify_tbl4::classify_block_from_table` (the existing in-substrate-union classifier) — no new column substrate. | No — V1 verdict holds: gaps live in `parse-that-regex` Layer-1 (used by both Track 1 and Track 2); consumer-wiring per gap names the Track-1 callsite without touching Track 2's `generated_real_typed.rs`. | **ACCEPT** (V1-LOCKED; zero drift confirmed) |
| `p2f-grammar-neutral.md` (V2 amended at commit `447a26b07`; 333→360 lines; 6 sub-folds per V2 hardening context §1: Fold-2 C8 DEMOTED + Fold-6 SKIPPED per `[no-deferrals]`; Fold-3 C6/C7/C10/C12/C13 disposition stamps with C12 reframed CH4-ACCEPT per CF-1; Fold-4 C10 scalar-ref `crates/bbnf-simd/src/scalar/byte_context_64.rs`; Fold-5 C13 scalar-ref `crates/bbnf-simd/src/scalar/bcax_64.rs`; NF-CH6-3 C2 upgrade with P2-E Gap 6 three-way composition; NF-CH6-4 cross-axis tracking §2.Y for long-string-body SIMD scan consolidation at lines 231-239) | No — V2 active set (13 of 14 V1 candidates; C8 demoted to §2.X.1) preserves Lock 1 substrate-union: §1.3 (line 54) unchanged "any candidate primitive that touches the tape touches the *single* substrate; any candidate that proposes a second source scan, retained cursor, aux density table, or parser-owned structural projection violates Lock 1 and is REJECTed per CH5." V2-amended §3 substrate-target table (line 247-260) holds: C1/C9/C11 `existing_tape`; C2/C3/C5/C7/C10/C12/C13 `local_temp_only`; C4/C5/C6 `direct_sink`; C14 N/A. The V2-NEW §2.Y cross-axis tracking note (lines 231-239) does NOT introduce a parallel substrate — it identifies that three axes name the SAME underlying primitive (long-string-body SIMD scan) under three names, and binds S-P3 to ONE canonical primitive. This is the *anti-paper-close* discipline working: discovering that three near-duplicates exist BEFORE they ship as three orthogonal SIMD bodies. The cross-axis note is the CH5 lens *enforcing* substrate-union honesty at the design layer (one primitive, one substrate, one canonical name) BEFORE S-P3 admission. | No — V2 active set's per-candidate substrate-target enumeration holds; C8 demotion removes one candidate from the active surface (no sidecar would have shipped at V2 anyway since C8 was NEUTRAL-PENDING-CONSUMER at V1). The Fold-4 + Fold-5 scalar-ref authoring at `crates/bbnf-simd/src/scalar/byte_context_64.rs` (C10) and `crates/bbnf-simd/src/scalar/bcax_64.rs` (C13) lands same-commit with the SIMD body per Lock 16 same-commit discipline — no new sidecar; scalar references for existing primitive shapes. | No — V2 active set carries no renamed scanner; C11 (`substrate-walk-with-shape-validation`) preserves the V1 single-primitive framing per P1-E §4.4 ("**S-P2 must not split it into two separate primitives**" at line 179, V2-unchanged). The C6 dispatch primitive's F-V2-P1ABC-RERECORD inheritance (lines 130, 300) is a cargo feature toggle for the existing `cfg_attr(feature = "parse-attribution", inline(never))` plumbing at `runtime/src/grammars/json/generated.rs:43-44` — does NOT rename or duplicate any scanner; toggles whether `dispatch_value` inlines its inner primitives for attribution measurement. | No — V2-unchanged: per-candidate scan/dispatch primitives compose with the existing classifier. The NF-CH6-3 C2 upgrade with P2-E Gap 6 three-way composition (per V2 hardening context §1) is composition, not a second scan. | No — V2-unchanged §1.3 (line 54) explicit foreclose; C11 admits as a single primitive precisely to *prevent* the Track-1 ≡ Track-2 collapse a split would create. §4 CH5 risk on C11 (line 306) V2-unchanged: "The V2 CHALLENGE fold must re-verify §2.11 against P2-D output once committed." — P2-D V2 has concluded YES (line 199-201 §4.7); C11's substrate-target slip-risk is closed at V2 cycle. | **ACCEPT** (V2 NF-CH6-4 §2.Y cross-axis tracking note is the CH5 lens working as designed; V2 C8 demotion removes a candidate with no substrate footprint; V2 scalar-ref Fold-4/Fold-5 landings carry no substrate change; F-V2-P1ABC-RERECORD cargo feature gating is verified non-renaming) |

Per-axis ACCEPT-rate: 6/6 ACCEPT, 0/6 REVISE, 0/6 REJECT → **100 % ACCEPT**
on the V2 CH5 lens. V2 inherits V1's 100 % ACCEPT and adds zero new
CH5 violations.

## §3 — Critical V2 findings

### Finding CH5-V2-A — Substrate-union YES still holds at HEAD after P2-D §1.6(d) demotion

The V2 demotion of C-P2D-3 (sparse-flag-band gating on
`Tape::flag_cursors`/`flag_values` construction) from active candidate
to §1.6(d) substrate-side observation does NOT break the substrate-union
verdict at `p2d-substrate-tape.md:92` ("**YES, the substrate union
holds at HEAD; tape + structural projection ARE one substrate**") or
the §4 closing pin at line 204 (identical YES re-affirmation). The
demotion is per-candidate eligibility movement; the substrate-union
conclusion is per-artefact §1.1 + §1.3 + §1.5 evidence convergence,
which is unaffected by the C-P2D-3 candidate's eligibility status.

Executable re-verification at V2 HEAD:
- `grep -rn "struct.*Tape\b" /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/` returns three hits — `Tape<'input>` at `tape/mod.rs:94`, `TapeBuilder<'input>` at `tape/assembler.rs:42`, `TapeId(pub u64)` at `tape/mod.rs:92`. The triple is one substrate (V1 verified; V2 unchanged).
- Two-cursor independence: `ParserState` at `runtime/src/grammars/json/parser.rs:7-12` carries `pub cursor: usize` at line 10; `DirectParser` at `bbnf-bench/src/generated_real_typed.rs:2742-2746` carries `cursor: usize` at line 2745. The two structurally independent cursors V1-verified by CH5 V3 `research/p1/hardening/V3/CH5.md:78-83` remain V2-verified at HEAD.

§4.7 V2-amended note explicitly confirms: "the V2-demoted §1.6(d)
sparse-flag observation (formerly C-P2D-3) is **CH5-clean by
construction (re-uses existing substrate field, no new sidecar)**."
The note's CH5-clean-by-construction phrasing is correct: the §1.6(d)
content describes wrapping the EXISTING `flag_cursors`/`flag_values`
field access in an `Option::get_or_insert_with(Default::default)`; the
`binary_search` consumer at `tape/mod.rs:144-150` is unchanged. No new
substrate field, no new sidecar, no new cursor. Severity: NONE
(substrate-union YES preserved; V2 §4.7 cross-check correct).

### Finding CH5-V2-B — V2 demotions across P2-C and P2-F do not introduce parallel-substrate surfaces

P2-C V2 §2.X non-candidate inventory (lines 48-72) demotes
C-P2C-1 (`ascii_set_member64_css_delimiter`),
C-P2C-6 (`eor3_string_mask_fusion`),
C-P2C-7 (`byte_context_orphan_resolution`)
to non-candidate inventory with disposition stamp "Demoted V2: zero
S-P1 hot-leaf antecedent at SK-V14; re-evaluate if F-V2-P1ABC-RERECORD
surfaces antecedent." Each demoted candidate carries CH5-clean
technical content:

- **C-P2C-1** is a 64-byte byte-set CSS delimiter primitive (`byte_class_from_eq_set_64` mask + first-set extraction) — returns transient mask, no substrate add. The W4 microbench evidence (4.72x speedup on `{,},;` delimiter set; SK-V13 W4 archival) is preserved as inventory; the substrate footprint is `local_temp_only` regardless of eligibility.
- **C-P2C-6** is three-mask XOR fusion (`quote ^ escape ^ control` via EOR3) — mask algebra returning a single mask, no substrate add.
- **C-P2C-7** is the orphan `byte_context` (EXT helper) folded into C-P2C-5 consumer or deleted with REDRESS evidence — no parallel substrate; the only admissible disposition is *consumer folding* (which preserves substrate-union) or *deletion* (which is the opposite of substrate addition).

P2-F V2 §2.X.1 demotes C8 (comment-skip primitive) to non-candidate
inventory per `[no-deferrals]` (no same-wave consumer commit in V2 wave
plan). The demoted C8 is a `local_temp_only` substrate-target candidate
(per `p2f-grammar-neutral.md:228` V2-unchanged) — even if re-elevated by
a future same-wave consumer, it does not introduce a new substrate.
The V2 demotion is removal from active enumeration, not introduction
of a new surface.

Aggregate V2 demotion CH5 footprint: ZERO new parallel substrate
surfaces introduced; FOUR candidates removed from active enumeration;
all four preserve substrate-clean technical content verbatim per the
disposition stamp + technical preservation discipline. Severity: NONE
(V2 demotions strengthen CH5 posture by reducing the active candidate
surface area while preserving cross-tranche identifier stability).

### Finding CH5-V2-C — P2-F §2.Y cross-axis tracking note is the CH5 lens working as designed (anti-paper-close + substrate-union enforcement convergence)

The V2-NEW §2.Y cross-axis tracking note at
`p2f-grammar-neutral.md:231-239` is the load-bearing V2 CH5 win.
The note identifies that three artefacts surface the SAME underlying
long-string-body SIMD scan primitive under THREE distinct names:

- **P2-A C2** `long_string_body_simd_scan` — names existing scalar refs `match_tiny_plain_string_with_cap` (`runtime/src/grammars/json/generated.rs:169`) + `unescape_string` (`crates/parse-that-regex/src/lib.rs:718`).
- **P2-E Gap 1** `scan_string_special_block_sweep_64` — names `scan_string_special_block_scalar`-as-bitwise-OR-fold (`crates/bbnf-simd/src/aarch64/string_block.rs:31`).
- **P2-F C1 + C2** (this artefact, quote-aware classifier composition) — names `scan_structurals_scalar` (`runtime/src/grammars/json/scan.rs:32`) and the P2-E Gap 6 composition.

The CH5 reading: this is **not** a CH5 violation. CH5 enforces "no
parallel substrate / sidecar producer / renamed scanner / Track 1 ≡
Track 2 dishonesty" at the SHIPPING surface (S-P3 admitted primitives).
The §2.Y note operates at the DESIGN-DOC surface (S-P2 research
artefacts) and SURFACES the would-be-violation BEFORE it ships. The
S-P3 consolidator binding ("S-P3 must produce ONE canonical primitive
name + ONE canonical scalar reference function rather than admitting
three near-duplicates") is the CH5 lens working at exactly the right
abstraction level: the discovery prevents three orthogonal SIMD bodies
from shipping as three substrate-coupled primitives.

This is simultaneously a CH6 anti-paper-close win (per V2 hardening
context §2 CH6 binding "P2-F NF-CH6-4 cross-axis tracking note is
exemplary anti-paper-close pattern") AND a CH5 hidden-coupling
discovery (per the V2 CHALLENGE-CONTEXT CH5 focus "P2-F §2.Y cross-axis
tracking note surfaces hidden coupling for S-P3 consolidation"). The
convergence of CH5 + CH6 on the same §2.Y note is the load-bearing
artefact of the V2 cycle: it demonstrates that the CHALLENGE lenses are
compositional (multiple lenses catch the same hidden surface) and
self-reinforcing (the lens that catches the discovery also binds the
remediation to S-P3). Severity: NONE for V2 (the note IS the CH5 win);
MED for S-P3 wave-plan (the consolidator binding is load-bearing —
without it, three near-duplicate SIMD bodies could ship).

### Finding CH5-V2-D — F-V2-P1ABC-RERECORD dependency tracking does not introduce envelope-masking renamed-scanner risk

F-V2-P1ABC-RERECORD appears in V2 as:
- P2-C C-P2C-8 active candidate at line 46 (process-gate: parse-attribution profile rebuild).
- P2-F C6 dispatch primitive (cargo feature gating) at lines 123, 130, 300 (inherited V2 carry-forward per dispatch context §1).
- P2-C V2 §2.X demoted candidates re-evaluation hook at lines 65, 69, 70, 71 ("re-evaluate if F-V2-P1ABC-RERECORD surfaces antecedent").
- P2-F §2.Y cross-axis note (does not depend on F-V2-P1ABC-RERECORD; substrate-coupling is independent).

The CH5 question: does F-V2-P1ABC-RERECORD introduce a renamed scanner
or parallel substrate? **No.** F-V2-P1ABC-RERECORD is a process-gate /
measurement deliverable that:

1. Builds with `--features runtime/parse-attribution` per P2-C C-P2C-8 spec (line 46).
2. Toggles the EXISTING `cfg_attr(feature = "parse-attribution", inline(never))` plumbing at `runtime/src/grammars/json/generated.rs:43-44` from OFF (current bench-build default per V2 P2-C finding 1, line 12) to ON.
3. Re-records P1-A/B/C on the full corpus set with interactive `samply record`.
4. Names the inner primitives behind every `dispatch_value` / `parse_object_value_at_direct` / `parse_array_element_at_direct` envelope.

The cargo feature toggle changes the inlining decision for 14 envelope
functions per `generated.rs:33-237` (per P2-C finding 1, line 12); it
does NOT add, rename, duplicate, or fork any scanner, any cursor, any
substrate, or any source-walk. The same `scan_structurals` at
`runtime/src/grammars/json/scan.rs:22` is consumed; the same
`ParserState` / `TapeBuilder` / `Tape` triple is constructed; the same
Track 1 + Track 2 cursors are independent. The toggle only changes
WHICH function frames samply attributes self-time to — it cracks the
envelope-inlining opacity that the CH2 mis-attribution census
identifies, allowing inner-primitive measurement.

Per P2-C C-P2C-8 disposition (line 46): "S-P3 process-gate prerequisite
at SK-V14 V1. Not an instruction candidate but a measurement
deliverable that must complete before any SK-V14 wave can shortlist
instruction routes whose admission depends on an envelope-cracked
attribution." This is correct: F-V2-P1ABC-RERECORD is a measurement
prerequisite, not a substrate or scanner addition.

Per P2-F C6 disposition (line 300): "S-P3 must ensure the wave that
admits C6 carries the parse-attribution rerun in the same wave (per
dispatch context §1: 'S-P2 designs with the existing P1 profile; the
re-record refines later'); no admit gate may close on C6 evidence
without the rerun." This is also correct: the rerun is same-wave with
C6 admission, but C6 itself is the existing `dispatch_value` envelope
primitive — no renaming.

Severity: NONE (F-V2-P1ABC-RERECORD is a measurement deliverable that
toggles existing cargo feature plumbing; zero scanner/substrate
addition).

### Finding CH5-V2-E — V1 CH5 findings A-F all carry forward at V2 without amendment

V1 CH5 §3 findings A through F (V1 commit `e1a8bb8b7`,
`research/p2/hardening/V1/CH5.md:85-249`) are all preserved at V2:

- **V1-A (C-P2C-2 Lock 1 dependency on P2-D satisfied; pre-block posture correct):** preserved at V2. P2-D V2 conclusion still YES (`p2d-substrate-tape.md:92,204` unchanged); C-P2C-2 V2 disposition at `p2c-arch-esoterica.md:42` PRE-BLOCKED unchanged.
- **V1-B (substrate-target taxonomy consistently applied across P2-D / P2-F / P2-B):** preserved at V2 with one V2 addition — P2-F V2 §2.X.1 demoted C8 carries `local_temp_only` substrate-target (unchanged from V1; demotion does not alter the taxonomy slot).
- **V1-C (C5 / C11 substrate-union completion candidates; S-P3 wave sequencing recommendation):** preserved at V2. P2-A C5 unchanged (V1-LOCKED); P2-F C11 unchanged at line 179 ("S-P2 must not split it into two separate primitives" V2-preserved). S-P3 wave sequencing recommendation still load-bearing.
- **V1-D (P2-E gap 1 explicitly preserves the tape emit at unchanged callsite):** preserved at V2 (P2-E V1-LOCKED; zero drift confirmed).
- **V1-E (no new union variant proposed; P2-D's architectural block is the binding closure):** preserved at V2. P2-D §1.5 line 86-92 V2-unchanged; C-P2D-4 remains REJECT-by-history paper-trail; C-P2D-3 V2-demotion to §1.6(d) does not introduce a new union variant.
- **V1-F (track2_entry_point column gap carries forward from P1 V1; not a P2 CH5 fault):** preserved at V2 (no V2 axis amendment claims to populate the column; the deferral to C-2 wave remains).

V1 → V2 finding delta: 0 V1 findings withdrawn; 0 V1 findings amended;
4 V2-NEW findings (A through D above) added covering the V2 amendment
surfaces (P2-D demotion, P2-C demotions, P2-F §2.Y, F-V2-P1ABC-RERECORD).
Severity: NONE (V1 carry-forward clean).

## §4 — V2 → V3 fold recommendations

The V2 → V3 fold for CH5 carries forward the V1 → V2 recommendations
that remain open (S-P3 wave-plan deliverables) and adds two V2-specific
items:

1. **V1 CH5-A C-P2C-2 wave sequencing (CARRY-FORWARD):** unchanged from V1 §4.1. S-P3 must sequence the Union-C wave AFTER (or simultaneously with) C5 / C11 substrate-union completion landings. (MED; wave-sequencing question for S-P3.)

2. **V1 CH5-C substrate-target manifest schema mechanisation (CARRY-FORWARD):** unchanged from V1 §4.2. P2-B Stage E manifest enforcement remains prose-level at V2; xtask validation is an S-P3 tooling deliverable. (LOW; tooling deliverable for S-P3.)

3. **V1 CH5-F track2_entry_point column population (CARRY-FORWARD):** unchanged from V1 §4.3. C-2 (R2) wave landing remains the mechanical-enforcement gate. (MED; downstream C-2 deliverable; P2 cannot deliver unilaterally.)

4. **V2-NEW CH5-V2-C P2-F §2.Y cross-axis consolidator binding (V2-NEW):** the §2.Y note binds S-P3 to ONE canonical primitive name + ONE canonical scalar reference function across P2-A C2, P2-E Gap 1, P2-F C1+C2 long-string-body SIMD scan consolidation. S-P3 must produce the canonical name selection + canonical scalar-ref function in the wave-program shortlist; failure to consolidate ships three near-duplicate SIMD bodies as three substrate-coupled primitives (CH5 violation at admission). (MED; wave-program deliverable for S-P3.)

5. **V2-NEW CH5-V2-D F-V2-P1ABC-RERECORD same-wave landing (V2-NEW):** P2-C C-P2C-8 process-gate and P2-F C6 cargo feature gating both require the parse-attribution profile rebuild as a same-wave deliverable in any S-P3 wave that admits envelope-cracked attribution-dependent primitives (C-P2C-2/-3/-4 per their respective dispositions). The orchestrator must schedule F-V2-P1ABC-RERECORD as Stage-0 of the first SK-V14 implementation wave per P2-C §5.6 line 164. (MED; wave-plan orchestration deliverable.)

The V2 → V3 LOCK trajectory per §3Z: V2 is the first ≥95 % CH5 cycle
(100 % ACCEPT), inheriting from V1 (also 100 % ACCEPT). Predicted V3
cycle is a zero-amendment confirmation pass; LOCK candidate at the V3
ACCEPT confirmation per §3Z two-cycle-chain convergence discipline.

## §5 — Sources cited (executable-verification at V2 HEAD)

Verified per V2 CHALLENGE-CONTEXT §3 "Executable-verification mandate":

### §5.1 — V2 hardening authority

- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CHALLENGE-CONTEXT.md` (39 lines) — read end-to-end; CH5 binding at §2 line 26; V1-LOCKED axis drift audit at line 30; V1 consolidator §5.4→§5.3 register correction at line 32.
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH5.md` (312 lines) — V1 CH5 100 % ACCEPT verdict; V1 §3 findings A-F carry-forward; V1 §4 fold recommendations 1-3 carry-forward.
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CHALLENGE-CONTEXT.md` (33 lines) — V1 CH5 binding reference.
- `restart/skinny/tranches/sk-v14/research/p2/S-P2-DISPATCH-CONTEXT.md` (P2-D scope binding + the no-parallel-substrate constraint; referenced via P2-D §5).

### §5.2 — V1-LOCKED axes (zero V2 drift confirmed)

- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md` (367 lines; V1 commit `e1a8bb8b7e`; `git diff e1a8bb8b7 447a26b07 -- p2a-sota-teardown.md` returns empty — zero drift confirmed).
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md` (342 lines; V1 commit `e1a8bb8b7e`; same diff confirms zero drift).

### §5.3 — V2 amended axes (V2 commit `447a26b07c`)

- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md` (217 lines; V2 amendment: SHA pinning at §5.1 lines 183-185 + V1-consolidator §5.4→§5.3 register correction surface).
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md` (164 lines; V2 amendment: 8→5 active candidates; C-P2C-1/6/7 demoted to §2.X lines 48-72 with disposition stamps at lines 69-71).
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md` (254 lines; V2 amendment: C-P2D-3 demoted to §1.6(d) line 104 + identifier stub gap-note at lines 110, 128-130; §4.7 V2-amended CH5 cross-check at lines 199-201).
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md` (360 lines; V2 amendment: 6 sub-folds per V2 hardening context §1; NF-CH6-4 §2.Y cross-axis tracking note at lines 231-239; Fold-4 C10 + Fold-5 C13 scalar-ref paths).

### §5.4 — Authority bindings

- `restart/prompts/skinny/PASS-2-RESEARCH.md §3 CH5` (lines 125-131; cited via P2-D §5.3) — CH5 lens binding.
- `restart/prompts/skinny/PASS-2-RESEARCH.md §8.6` (lines 236-240; cited via P2-D §5.3) — substrate union closing pin.
- `restart/prompts/ORCHESTRATOR.md §3W + §3Z` — lens registry + convergence cadence.
- `restart/locks/LOCKS.md:48-90` — Lock 1 substrate-union + v+1 substrate-ceiling fold + manifest triple + REDRESS 96/97/98 binding-history clause.
- `restart/locks/LOCKS.md:309-318` — sixteen-column primitive manifest schema (cited via P2-B §2.E).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH5.md:78-83` (cited via P2-D §1.3) — two-cursor independence verification carry-forward.

### §5.5 — Source-code executable re-verification at V2 HEAD

- `skinny/crates/runtime/src/tape/mod.rs:94` — `pub struct Tape<'input>` (canonical retained substrate; verified single).
- `skinny/crates/runtime/src/tape/mod.rs:92` — `pub struct TapeId(pub u64)` (identity newtype).
- `skinny/crates/runtime/src/tape/assembler.rs:42` — `pub struct TapeBuilder<'input>` (parse-time builder facade).
- `grep -rn "struct.*Tape\b" /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/` returns exactly 3 hits (the three above) — substrate-union single-substrate property V2-verified.
- `skinny/crates/runtime/src/grammars/json/parser.rs:7-12` — `ParserState<'i>` carries `pub cursor: usize` at line 10 (Track 1 cursor; V2-verified).
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2742-2746` — `struct DirectParser<'i> { input, bytes, cursor: usize }` at line 2745 (Track 2 cursor; V2-verified structurally independent of Track 1).
- `skinny/crates/runtime/src/grammars/json/generated.rs:43-44` — `cfg_attr(feature = "parse-attribution", inline(never))` plumbing (the F-V2-P1ABC-RERECORD toggle target; existing infrastructure, no new scanner introduced).
- `skinny/crates/runtime/src/grammars/json/scan.rs:22` — `scan_structurals` SIMD entry (one classifier pass; V2-unchanged).
- `skinny/REDRESS.md:2508-2540` (Item 88), `:2542-2585` (Item 89), `:2587-2618` (Item 90), `:2797-2906` (REDRESS 96/97), `:2910-2950` (REDRESS 98) — substrate-ceiling history pre-block surface honoured by V2 C-P2C-2 PRE-BLOCK posture (V2-unchanged from V1).

### §5.6 — Git history (V1 → V2 cycle commits)

- V1 axis commit: `e1a8bb8b7e4cf48aadd0bdfbad2806a4dc89fc0b` — "docs(sk-v14-p2+t-p1): dual-track six+eight axis pass — atomic write-only commit".
- V2 axis commit: `447a26b07c353b217905c15a3d61c907a8e78410` — "docs(sk-v14-p2-V2): atomic micro-fold (4 axes amended) + V2 dispatch context".
- V1-LOCKED axis drift verification: `git diff e1a8bb8b7 447a26b07 -- restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md` returns empty (zero drift confirmed for both V1-LOCKED axes).
