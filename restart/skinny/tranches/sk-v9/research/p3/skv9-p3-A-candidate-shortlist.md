# SK-V9 P3-A: Candidate-Intervention Shortlist

Pass: S-P3 Synthesis-Plan. Cycle: V4.
Date: 2026-05-18.
Scope: Distil the six S-P2 interventions into a ranked shortlist of ≤8
candidate interventions for SK-V9 waves — each carrying owner paths,
scalar-reference + checkasm-parity status, same-wave consumer,
falsifiability gate (named corpus rows + Mbps thresholds), preliminary
LOC envelope + risk class, dependency, and REDRESS pre-block citations.
Output: this file.
Pass Alpha goalset: SK-V9 §0 close-condition — W0 telemetry-lock (closed);
fresh S-P1 rerun convergence (achieved); Apache/CITM measured typed rows,
retained class/event grammar + `ValueRef` proof, and the structural-heavy
parse route either admit by named gates or remain explicitly blocked;
strict admission stays strict-vs-strict on matching planes; RESULTS /
REDRESS / SPEC / DISPATCH-PROMPT / HANDOFF agree at close.
Candidate pool: `research/p2/` post-CHALLENGE survivors (S-P2 converged
6/6 ≥95% per `HARDENING-S-P2-CONVERGED.md`).

## §1 — Method

### §1.1 — Source mapping

The six S-P2 reports decompose unevenly: P2-A, P2-B, P2-C, and P2-E each
carry one coherent intervention; P2-D carries four named ASM
sub-candidates plus a fifth (dead-SIMD-scanner wiring) that is the
implementation body of P2-A's structural producer; P2-F is a teardown
that names *no new intervention* — it grounds the SOTA picture and
authors the inter-report dependency graph (`I ← P2-A ← P2-B`;
`II ← P2-E`; `III ← P2-D ← P2-A`). P2-F therefore contributes the
dependency spine (§3) but no shortlist row of its own.

The raw intervention set distils to **eight** candidates:

| # | Raw intervention | Source report |
|---|---|---|
| C1 | Apache/CITM measured typed row-table admission | P2-C |
| C2 | Retained class/event grammar + `ValueRef` proof | P2-B |
| C3 | Union event-model (cursor/class split; structural index consumed) | P2-A |
| C4 | `\uXXXX` codec broadening + per-quartet NEON fall-through | P2-D §3 + P2-E |
| C5 | 32-byte string-block scanner widening (`scan_string_special_block_32`) | P2-D §4 |
| C6 | SHA3 EOR3 vector prefix-XOR ladder | P2-D §5.3.1 |
| C7 | CSSC CTZ at the string-mask first-set extract | P2-D §4.4 |
| C8 | Checkasm-parity backfill (missing differential tests) | P2-D §6.2 / §6.2.1 |

P2-D's "dead-SIMD-scanner wiring" (§5) is **not a separate candidate**:
it *is* the structural-bitmap producer body that C3's union event-model
consumes — the same kernel, the same wave. It is folded into C3's owner
paths. P2-E and P2-D §3 are merged into a single candidate (C4) because
both name the identical kernel (`unescape_uxxxx_neon` /
`unescape_uxxxx_x4_neon` at `bbnf-simd/src/aarch64/unescape_uxxxx.rs`)
and the identical production consumer (the already-wired x4 path at
`parse-that-regex/src/lib.rs:402`); shipping them as two candidates
would split one kernel across two waves and orphan it.

### §1.2 — Ranking criteria (applied in order)

1. **GO-count lift per LOC.** A candidate that moves rows from `NO-GO`
   to `A / GO` (or admits new measured `A / GO` rows) at low LOC ranks
   above a candidate that moves no row. Proof-only candidates carry zero
   row lift but may be a hard dependency for row-movers.
2. **Dependency depth.** Independent candidates rank above dependent
   ones; a candidate that must wait on another landing first is
   demoted by its depth in the §3 graph.
3. **Risk class.** Within a depth tier, LOW risk ranks above MEDIUM
   above HIGH. REDRESS-adjacent candidates (C6, C7) carry HIGH/MEDIUM
   risk because the structurally-adjacent shape was already rejected.

### §1.3 — Proof-only vs row-moving (preview; full table §4)

C1 is row-moving (admits two measured typed rows). C3 is row-moving
(the structural fix). C4/C5 are row-moving (the four uncloseable rows).
C2 is **proof-only** — P2-B §1.1 is explicit: "no row in
`skinny/RESULTS.md` moves; `cargo bench` is *not* a verification
surface." C8 is **infrastructure-only** — checkasm tests move no row;
they are a same-wave *admission precondition* of C4/C5/C6. C6 and C7 are
row-contributing but not row-closing alone — they are sub-slices of
C5's wave and admit only under C3 (P2-A) landing in the same wave.

### §1.4 — Drops

No S-P2 candidate was REJECTed by the S-P2 CHALLENGE — the consolidation
records 6/6 ≥95% ACCEPT. Nothing is dropped for rejection. Two raw items
are *folded* rather than dropped: P2-D §5 dead-scanner wiring folds into
C3; P2-D §5.3.2 (AESE byte-class shuffle) is dropped because P2-D §5.3.2
itself dispositions it "not useful for structural classification …
Rejected for §5.3 use" — it never reached candidate status.

## §2 — The ranked shortlist

### §2.1 — Summary table

| Rank | ID | Candidate | Source | Class | Risk | Prelim LOC | Depends on | GO-count lift |
|---:|---|---|---|---|---|---:|---|---|
| 1 | C1 | Apache/CITM measured typed row-table admission | P2-C | row-moving | LOW | ~300 | — (independent) | +2 measured `real_typed_struct A / GO` |
| 2 | C2 | Retained class/event grammar + `ValueRef` proof | P2-B | proof-only | LOW | ~395–425 | — (independent) | 0 (unlocks C3) |
| 3 | C3 | Union event-model (cursor/class split) | P2-A (+P2-D §5) | row-moving | MEDIUM | ~265 hand + ~120 regen | C2 | ~5–6 string/structural-dense rows toward `≥ sonic` |
| 4 | C8 | Checkasm-parity backfill | P2-D §6.2 | infra-only | LOW | ~360 (tests) | — (precondition of C4/C5/C6) | 0 (admission gate) |
| 5 | C4 | `\uXXXX` codec broadening + per-quartet NEON | P2-D §3 + P2-E | row-moving (conditional) | MEDIUM-HIGH | ~1,045 net (P2-E §7.4) | C3 (same-wave) + C8 | up to 2 unicode rows (conditional admission) |
| 6 | C5 | 32-byte string-block scanner widening | P2-D §4 | row-moving (conditional) | MEDIUM | ~145–270 | C3 (same-wave) + C8 | contributes `unicode_mixed` + `gsoc-2018` |
| 7 | C6 | SHA3 EOR3 vector prefix-XOR ladder | P2-D §5.3.1 | row-contributing | MEDIUM | ~60–120 | C3 (same-wave) + C8 | 0 alone (producer accel) |
| 8 | C7 | CSSC CTZ at string-mask first-set extract | P2-D §4.4 | row-contributing | HIGH | ~15–35 | C3 (same-wave) + C5 + C8 | 0 alone (consumer accel) |

### §2.2 — Per-candidate detail

---

#### C1 — Apache/CITM measured typed row-table admission

- **Source.** P2-C (`skv9-p2-C-apache-citm-admission.md`).
- **Owner file paths (seven, five disjoint slices, P2-C §4.1):**
  - `skinny/crates/bbnf-bench/src/report.rs:709` — `SK_V8_OPEN_BASELINE`
    (add two rows; possible rename to `SK_V9_OPEN_BASELINE`).
  - `skinny/crates/bbnf-bench/src/bin/gate.rs:1820-1831` — regression
    test `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures`
    (flip Apache/CITM assertions).
  - `skinny/RESULTS.md` — promoted row block + run-id refresh.
  - `skinny/crates/bbnf-bench/target/skv9-w{n}/criterion/` — fresh
    same-run capture (out-of-band; non-LOC artefact).
  - `skinny/REDRESS.md` — new admission entry.
  - `restart/skinny/tranches/sk-v9/HANDOFF.md` §3 row 1 — state update.
  - `restart/locks/LOCKS.md` (Lock 14) — `sk-v9-real-typed-w{n}`
    parent-diff allowance scoped to the seven paths.
- **Scalar-reference status.** N/A (not a kernel). The Track 1
  generated DirectBuild typed parsers (`parse_apache_builds`,
  `parse_citm_catalog`) already exist (REDRESS 71-admitted, W2-frozen at
  schema identity `sk-v8-real-typed-w2`); the Track 2/oracle is
  `serde_json::from_slice` (structurally independent at the
  implementation level, P2-C §2.7).
- **Checkasm-parity status.** N/A (no SIMD kernel). The correctness
  surface is `assert_real_typed_parity` full-fixture checksum equality
  across {generated, serde, sonic} — **exists** at
  `real_typed_struct.rs:310-323`.
- **Same-wave consumer.** The `gate-json` baseline-fixture contract +
  the regression test `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures`
  (the test IS the consumer for the baseline flip); `same_wave_consumer_class = gate_only`.
- **Falsifiability gate (P2-C §4.3).** Apache typed Track 1 `≥
  ceil(sonic_rs_real_typed_struct_Mbps / 1.10)`; CITM typed Track 1 same
  (`DIRECT_PROJECTION_SONIC_SLACK = 1.10`, `gate.rs:56`). Full-fixture
  parity passes for both rows. The four existing typed GO rows
  (`twitter`, `update_center`, `mesh`, `marine_ik`) hold their `A / GO`
  outcome with no regression below `sonic × 1.10⁻¹`. Direct rows
  (Apache `N-direct/NO-GO`, CITM `A/GO`) unchanged at SK-V9-open
  verdicts. Per-row anchors: existing typed GO slack runs −4.5%
  (`update_center`) to +25.2% (`marine_ik`); Apache/CITM must clear the
  same ±9.1% strict slack band.
- **Preliminary LOC envelope.** ~255 hand + run-id refresh ≈ ~300 total
  (P2-C §2.0); five slices ~85 min, ≤90 min HANDOFF cap.
- **Risk class.** **LOW.** No new parser, codegen template, substrate,
  or SIMD primitive; the typed parsers are W2-frozen and unchanged. The
  binding risk is a measurement miss against the 1.10 slack, handled by
  the §4.3 revert-and-route-back protocol.
- **Dependency.** **Independent.** P2-C §6: no retained-parse surface,
  no structural-heavy parse — REDRESS 92 not reopened; does not depend
  on C2 or C3.
- **REDRESS pre-blocks.** REDRESS 91 (Canada stays rejected; the wave
  admits Apache + CITM only and does not weaken the canada
  checksum-mismatch route-out). REDRESS 71 is the *admitted* route
  (generated host/API typed DirectBuild), not a pre-block. REDRESS 60-72
  retained-parse routes not reopened (P2-C §6: no retained surface, no
  semantic string facts, no parser-owned scratch, no cap-16 extension).
  REDRESS 92, 93 not reopened. HANDOFF §5 item 1 ("Apache/CITM
  measured-row overclaim from REDRESS 91") is *closed* by satisfying the
  admission criterion, not reopened.

---

#### C2 — Retained class/event grammar + `ValueRef` proof

- **Source.** P2-B (`skv9-p2-B-retained-grammar-proof.md`).
- **Owner file paths (five slices, P2-B §1.2 / §6.1):**
  - `skinny/crates/runtime/src/tape/event_grammar.rs` (NEW) —
    `EventGrammar` trait + `AnyGrammar` default instance.
  - `skinny/crates/runtime/src/tape/event_grammar_tests.rs` (NEW, in
    `tests/` per `feedback_no_inline_tests`).
  - `skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs` (NEW).
  - `skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs`
    + `mod.rs` (NEW) — the Lock-14 non-JSON witness (CSS L4 is the §6.1 S3
    fallback).
  - `skinny/crates/runtime/src/tape/mod.rs` (TOUCHED, ~20 lines) —
    `ValueRef` `K = AnyKind` → `G: EventGrammar = AnyGrammar`.
  - `skinny/crates/runtime/src/lib.rs` (TOUCHED, ~5 lines) — re-exports
    behind a single `#[cfg(any(test, feature = "proof"))]` at the parent
    `pub mod` site.
- **Scalar-reference status.** N/A — proof carries no kernel. The
  "reference" is the compiler itself: the `const _: fn() =
  _proof_compiles::<…>` triple (JSON, Sheets, AnyGrammar) compiling is
  the proof; refusal is the failure mode.
- **Checkasm-parity status.** N/A — no SIMD. The verification surface is
  `cargo check -p runtime` + `cargo test -p runtime event_grammar` +
  the `rg` Lock-14 audits (P2-B §3.3).
- **Same-wave consumer.** **None — and the rule is silent here.** P2-B
  §5.1 gives the formal disposition: the same-wave-consumer rule binds
  *substrates*, not *contracts*; the proof is a trait declaration plus
  three witness `impl`s, gated out of any release/bench surface. No
  orphan kernel ships because no kernel ships.
- **Falsifiability gate.** Compile-only — `cargo check -p runtime`
  passes; the three `const _` proof lines compile; the negative
  `ValueRef<'static, 'static, …>` test fails to compile (the third leg,
  P2-B §2.3); the Lock-14 `rg` audits return matches only inside
  `event_grammar.rs` / witness files / proof test. **No bench row, no
  Mbps threshold** — P2-B is explicit that `cargo bench` is not a
  verification surface. This is the one shortlist candidate whose gate
  is non-Mbps; it is measurable (binary compile pass/fail) and therefore
  CH1-admissible as a falsifiability gate, but it is **proof-only**.
- **Preliminary LOC envelope.** ~395 (P2-B §1.2) / ~425 aggregate
  per-slice cap (P2-B §6.1) — inside the HANDOFF 450 LOC envelope;
  ≤90 min.
- **Risk class.** **LOW.** Five small slices, all NEW files or
  rename-only edits; the `ValueRef` rename preserves the 12-byte stack
  footprint and existing call sites compile via the `AnyGrammar`
  default (zero call-site edits expected). Residual risk is
  borrow-checker variance friction (P2-B §6.3 buffers ≤15 min for it).
- **Dependency.** **Independent** to land — but it is the *unlock
  dependency* for C3. P2-B §5: the proof removes exactly one HANDOFF §5
  pre-block ("W3 structural implementation without retained class/event
  grammar plus retained `ValueRef` cursor proof"), making C3 *eligible
  to dispatch*. C3 must not land before C2.
- **REDRESS pre-blocks.** REDRESS 92 (the direct antecedent — the proof
  is the routed "define + prove" precursor; the "reopen" is explicitly
  out of scope, HANDOFF "no row movement at Alpha depth" enforces it).
  REDRESS 60-72 (the rejected SK-V6 retained-parse class — P2-B §4
  differential: no production consumer, no measurement surface, no edit
  to `generated.rs`/`scan.rs`/`parser.rs`/`view.rs`/templates, no new
  BIR/directive/`BackendShape`, `ValueRef` field layout unchanged).
  REDRESS 71 (orthogonal — typed-output `DirectBuild` codegen path, not
  the retained-tape contract; P2-B §6.2 R5).

---

#### C3 — Union event-model (cursor/class split)

- **Source.** P2-A (`skv9-p2-A-union-event-model.md`); P2-D §5
  dead-SIMD-scanner wiring is the structural-bitmap producer body folded
  into this candidate.
- **Owner file paths (eight slices, P2-A §5):**
  - `skinny/crates/runtime/src/tape/{mod,assembler}.rs` — `classes:
    Vec<u8>` column, `class_at(cursor)`, `push_offset_with_class`.
  - `skinny/crates/runtime/src/grammars/json/parser.rs` — `emit_plain_offset`
    → `emit_event_offset(offset, class)`; structural-index walker cursor.
  - `skinny/crates/runtime/src/grammars/json/generated.rs:292-306`
    (regen) — `consume_structural` deleted; structural-index walk.
  - `skinny/crates/runtime/src/grammars/json/value.rs:29-47` (regen) —
    `JsonNodeKind::at_cursor` byte-rediscovery → class-column read.
  - `skinny/crates/codegen/src/json_templates/{generated,parser,view,value}.rs`
    — class-column emission + structural-walk lowering.
  - `skinny/crates/bbnf-simd/src/lib.rs` — surface the structural index
    by move-consume (the `class_table` at `lib.rs:41` already exists);
    plus the `bbnf-simd/src/aarch64/` structural-bitmap chain (P2-D §5:
    4-register TBL classify + quote/escape/backslash mask + VEXT
    cross-chunk carry).
  - `skinny/crates/runtime/src/grammars/json/scan.rs` (regen) — stop
    discarding the index.
  - `skinny/crates/bbnf-bench/src/parity.rs` — class-column parity +
    structural-index move-consumed asserts.
- **Scalar-reference status.** **Exists.** The structural-bitmap chain's
  scalar references are in tree: `bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs`,
  `scalar/bitmap_next_set_bit.rs`, `scalar/byte_class_from_eq_set_64.rs`,
  `scalar/byte_class_from_table_64.rs` (P2-D §5.1 inventory). The
  cursor/class split itself is not a kernel — its "reference" is the
  current `consume_structural` shape, preserved row-for-row in cursor
  count.
- **Checkasm-parity status.** **Partially exists, must-extend.** Existing:
  `checkasm_bitmap_prefix_xor_64.rs`, `checkasm_bitmap_next_set_bit.rs`,
  `checkasm_bulk_emit_positions_64.rs`, `checkasm_byte_class_from_eq_set_64.rs`,
  `checkasm_byte_class_from_table_64.rs`, `checkasm_structural_terminator_64.rs`.
  **Must-author:** `scan_structurals` end-to-end checkasm + corpus-parity
  gate (P2-D §5.4 slice, ~50-90 LOC) — the structural-bitmap chain as a
  whole has no differential test today.
- **Same-wave consumer.** The retained `JsonRoot` view's
  `JsonNodeKind::at_cursor` (P2-A §2.4 #2) — reads `tape.class_at(cursor)`
  instead of `tape.source()[offset_at(cursor)]`. The byte-rediscovery
  line is *deleted* in the same wave. The structural-bitmap producer's
  consumer is the parser walker (`walk_container_at_class` /
  `parse_object` etc.) consuming the index by move.
- **Falsifiability gate (P2-A §4).** Must-improve (P2-A §4.1):
  `twitter` Track 1 `≥ 17685` (sonic-strict/1.10; today 13188);
  `apache_builds ≥ 14124` (today 11917); `distinct_values
  ≥ 15731` (today 8972); `update_center ≥ 14370` (today 9857,
  `ceil(15806/1.10)`). `gsoc-2018` does **not** bind the W3 exit gate
  (F-spec §6, P3-C §2): its throughput gap exceeds the per-delimiter
  budget, so the union substrate alone cannot lift it to
  sonic-strict/1.10 — gsoc-2018 carries a no-regression-only clause at
  W3 and its partial improvement is recorded, not gated (the W3 gate
  binds only the four named must-improve rows above).
  Hot-leaf: `consume_structural` ≤ 5% self-time,
  `JsonNodeKind::at_cursor` ≤ 1%. Must-not-regress — the W10b six-row
  block (P2-A §4.2, binding), each row at `floor(today × 0.98)` or
  `ceil(sonic_strict / 1.10)`, whichever higher (the `today × 0.98` leg
  floored uniformly across all six): `canada ≥ 15866`,
  `citm_catalog ≥ 28630`, `instruments ≥ 15865`, `marine_ik ≥ 11831`,
  `mesh ≥ 12186`, `numbers ≥ 17596`. Falsified if
  `consume_structural > 5%` on twitter/apache_builds, or any W10b row
  drops below floor, or Track 2 / direct / SinkOnly rows move beyond
  noise, or a JSON symbol leaks into a generic crate.
- **Preliminary LOC envelope.** ~265 hand + ~120 regen net (P2-A §5.9);
  inside the W3 default 450 LOC budget. Plus the P2-D §5 structural-chain
  body (~120-220 LOC `bbnf-simd` + ~30-60 VEXT + ~60-120 cursor wire +
  ~50-90 checkasm) — S-P3 P3-B/P3-C reconcile whether the §5 body lands
  in C3's wave or a co-sequenced slice; the chain is C3-internal.
- **Risk class.** **MEDIUM.** P2-A §5.9: "the mechanism is a
  *contracting* one (it *deletes* `consume_structural` and shrinks
  `at_cursor`)"; the novel surface is the codegen template's
  structural-walk lowering (slice A.5). The P2-D §5 structural-bitmap
  chain raises the wave's aggregate risk to HIGH if folded in whole
  (P2-D §5.4: "wave-class substrate replacement") — S-P3 must decide the
  split. The cursor scheme is preserved row-for-row, which bounds the
  blast radius.
- **Dependency.** **Depends on C2.** P2-B §5: C3 (SC-3 Tier A migration)
  is eligible to dispatch only after the C2 proof removes the HANDOFF §5
  pre-block. C3 must land *after* C2. C3 is itself the same-wave
  dependency for C4/C5/C6/C7 (P2-D §0 cascade constraint).
- **REDRESS pre-blocks.** REDRESS 92 (the routed precursor — C3
  implements it by splitting the conflated cursor/class question, not
  merging; satisfies one substrate / same-wave consumer / preserved
  `ValueRef` contract). REDRESS 50 (parser-written aux side tables — the
  class column is co-emitted at the existing `emit_plain_offset` site,
  no separate aux pass). REDRESS 51 (parser-local byte-class cursor — no
  parser-owned cursor beside the tape's). REDRESS 53 (parser-local mask
  cursor / second scanner — the structural index is consumed by move, no
  second mask). REDRESS 60-72 (retained-parse sidecar producers — the
  class column is the tape's own column, written by the only producer).
  REDRESS 28+33 (16-byte tiny-string dispatch into parser hot loop — P2-D
  §5.5: C3's consumer is the union substrate, a different code path, not
  the parser hot loop). REDRESS 82, 83, 84, 88, 89 orthogonal. The
  blanket pre-blocks (no new `BackendShape`/BIR/directive, no public
  substrate API, no parser-owned structural cursor, no `tape_vs_tape`
  production consumer, no `UnionTape`) all honoured per P2-A §6.

---

#### C8 — Checkasm-parity backfill

- **Source.** P2-D §6.2 / §6.2.1.
- **Owner file paths (P2-D §6.2.1 dispatch table):**
  - `bbnf-simd/tests/checkasm_unescape_uxxxx.rs` (NEW) — covers
    `unescape_uxxxx_neon` + `_scalar` + `unescape_uxxxx_x4_neon` +
    `join_surrogate_pair_neon`; the alignment sweep REDRESS 82's
    rejected patch never landed.
  - `bbnf-simd/tests/checkasm_string_block.rs` (NEW) — covers
    `scan_string_special_block` + `_scalar` + the new `_32` variant.
  - `bbnf-simd/tests/checkasm_match_tiny_plain_string.rs` (NEW) — the
    standalone differential (today only indirectly covered by
    `classifier_parity.rs`).
  - `bbnf-simd/tests/checkasm_movemask.rs` (NEW) — standalone
    `movemask_u8x16` differential.
  - `checkasm_digit_mac.rs` — **deferred ownership**: assigned to the
    first SK-V9+ wave that wires `digit_mac` into a numeric-token
    consumer (no §3-§5 consumer this iteration; P2-D §6.2.1 carries
    the ownership forward rather than dropping it).
- **Scalar-reference status.** **Exists** for every primitive — the
  scalar oracle at `bbnf-simd/src/scalar/*.rs` is present for all
  aarch64 primitives (P2-D §6.3 invariant 1). This candidate authors the
  *differential test*, not the reference.
- **Checkasm-parity status.** This candidate **IS** the checkasm-parity
  authoring. P2-D §6.2: a wired primitive without a checkasm
  differential is a standing DAV1D-discipline violation
  (`unescape_uxxxx_x4_neon` is wired-but-untested today).
- **Same-wave consumer.** The test IS the consumer for the kernel bodies
  (P2-E S6 framing). Per P2-D §6.2.1 the admission rule is: **the wave
  that broadens / widens / wires the primitive authors that primitive's
  checkasm test in the same wave** — so C8 is not a standalone wave but
  a *bundled precondition* distributed across the C4 wave
  (`checkasm_unescape_uxxxx.rs`, `checkasm_match_tiny_plain_string.rs`)
  and the C5 wave (`checkasm_string_block.rs`, `checkasm_movemask.rs`).
  It is ranked as a candidate because it carries its own owner paths and
  admission gate, but P3-B should sequence it as same-wave preconditions
  of C4/C5/C6, not as W{n} of its own.
- **Falsifiability gate.** Each `checkasm_<name>.rs` asserts
  byte-identical output between the NEON body and the scalar oracle
  across the randomised input alphabet (digit-count × alignment ×
  terminator pattern × validity; P2-E §7.3 names ~6,000 cases per
  binding). `cargo test -p bbnf-simd --release --test checkasm_*` is
  green. **No Mbps threshold** — this is a correctness gate; failure
  *blocks* the broadening wave commit.
- **Preliminary LOC envelope.** P2-E S6 sizes `checkasm_escape_codec.rs`
  at ~250 LOC; P2-D §4.3 sizes `checkasm_string_block.rs` at ~40-70 LOC;
  `checkasm_movemask.rs` + `checkasm_match_tiny_plain_string.rs` ~40-70
  combined. Aggregate ~330-390 test LOC.
- **Risk class.** **LOW** — test harness only; no codepath, no row
  movement. The risk it *retires* is high: it is the gate that prevents
  C4/C5/C6 from landing on an untested kernel.
- **Dependency.** **Independent to author** but **bound by sequence** —
  it must land *before* the consumer wiring of the wave it precedes
  (P2-E S6: "lands BEFORE any consumer wiring … revert blocks the
  wave"). It is a precondition, not a successor.
- **REDRESS pre-blocks.** None reopened — test harness only. It
  *closes* the standing DAV1D-discipline gap that REDRESS 82's and
  REDRESS 83's rejected patches left (each carried a checkasm test that
  went out with the rejected patch).

---

#### C4 — `\uXXXX` codec broadening + per-quartet NEON fall-through

- **Source.** P2-D §3 + P2-E (`skv9-p2-E-unicode-escape-codec.md`) —
  merged: same kernel, same production consumer.
- **Owner file paths (P2-E §7.1 eleven slices):**
  - `bbnf-simd/src/aarch64/escape_codec/` (NEW directory module):
    `mod.rs`, `scalar.rs`, `hex_x4_neon.rs`, `hex_x8_neon.rs`,
    `hex_variable_neon.rs`, `surrogate_join.rs`.
  - `parse-that-regex/src/lib.rs:402` / `:718-810` — re-body the
    already-wired x4 path + the `Some(b'u')` arm onto the kernel.
  - `runtime/src/grammars/json/sink.rs` — call-site swap.
  - `codegen/src/escape_codec/` (NEW sub-module) — const-generic
    emission for the five bindings (JSON-4, TOML-4, TOML-8, CSS L4
    `Range(1,6)`, JS `\u{}`).
  - `bbnf-css/tests/` — CSS L4 scaffold (`#[cfg(test)]`, compile-only).
  - `bbnf-simd/tests/checkasm_escape_codec.rs` — the parity gate
    (overlaps C8; P2-D §6.2.1 assigns `checkasm_unescape_uxxxx.rs` to
    this wave).
  - `bbnf-simd/src/aarch64/unescape_uxxxx.rs` — superseded kernel
    removed (−215 LOC, lands LAST after the consumer is green).
- **Scalar-reference status.** **Exists, refined.** `read_hex_unit_scalar`
  + `hex_nibble` at `parse-that-regex/src/lib.rs:945-966` are the current
  scalar path; P2-E S1 re-homes the reference into
  `escape_codec/scalar.rs` (~120 LOC) as the parity oracle for S2-S5.
- **Checkasm-parity status.** **Must-author.** No `checkasm_unescape_uxxxx.rs`
  exists today (REDRESS 82's wave added one and it was rejected with the
  patch). P2-E S6 authors `checkasm_escape_codec.rs` (~250 LOC) covering
  all five bindings; it **lands first** (CH6-E-1 prerequisite) and a
  revert blocks the wave.
- **Same-wave consumer.** The **production consumer** is the JSON
  materialiser `unescape_string` — specifically the already-wired
  `unescape_four_unicode_escapes` x4 path at
  `parse-that-regex/src/lib.rs:402` (verified in-tree; P2-D §0 corrects
  the V1 "not wired" error). The wave *re-bodies* an existing production
  path; it does not introduce a new consumer. CSS L4 + TOML ship as
  **scaffolds** (compile-validated, no production parse loop).
- **Falsifiability gate (P2-E §6, PMU-rederived — honest verdict).**
  `unicode_escapes` Track 1 `≥ 16,319 Mbps` (sonic-strict 18,132 ×
  0.90); projected 15,423 — **NEAR-FAIL 94.5%**. `y_string_unicode`
  Track 1 `≥ 8,270 Mbps` (sonic-strict 11,814 × 0.70 W4-precedent
  slack); projected 7,837 — **NEAR-FAIL 94.8%**. `unicode_mixed` Track 1
  `≥ 12,338 Mbps` (sonic-strict 14,515 × 0.85); projected 7,864 —
  **FAIL 63.7%** (needs C5 paired). `gsoc-2018` `≥ 21,430 Mbps`
  (`baseline 21,646 − 1%`, no-regression basis; codec share ≈ 0%).
  **Honest posture: zero of the four rows admit on the codec alone.**
  Admission is the §6.4 same-wave conditional rule — the codec admits
  iff the *measured* post-wave Mbps clears the gate, with the projection
  flagged as expected-best-case. Plus the §3.5 binding direct-route
  no-regression gate on `unicode_escapes/direct`, `y_string_unicode/direct`,
  `unicode_mixed/direct` (REDRESS 82's blocking rows become C4's
  admission rows).
- **Preliminary LOC envelope.** ~890 hand + ~120 regen = ~1,010 new;
  ~1,260 with tests; −215 deletion; **~1,045 net** (P2-E §7.4). Eleven
  slices, ~6.0 h aggregate per-slice cap — the largest candidate.
- **Risk class.** **MEDIUM-HIGH.** P2-E §7.2: LOW on JSON-4 correctness,
  MEDIUM on variable-width CSS/JS, **MEDIUM-HIGH on `unicode_escapes` +
  `y_string_unicode` performance** (both NEAR-FAIL — real µop count
  could fall short), HIGH on `unicode_mixed` (does not close alone).
- **Dependency.** **Cascade-locked to C3; lands as the W4b sub-waves.**
  P2-D §3.5 / §0: the codec broadening blocks on the P2-A (C3) union
  substrate existing OR fails CH5 — absent the substrate, the
  broadening only reduces fall-through in the *parser-owned* helper,
  the REDRESS-82-rejected shape. Per the P3-F SPEC §2.2 binding reading
  of P2-D §0, the cascade-lock is satisfied by W3 (the C3 substrate)
  preceding the W4 sub-waves — it does NOT mean one monolithic wave.
  C4 is itself ~1,045 net LOC and lands as three sub-waves
  W4b-1/W4b-2/W4b-3 (P3-F SPEC §7.2). C8's `checkasm_escape_codec.rs`
  is the W4b-1 admission precondition.
- **REDRESS pre-blocks.** REDRESS 82 (W4 single-quartet classifier — P2-E
  §5 five-axis differential: primitive class not classifier, const-generic
  template not JSON-instance, one production consumer + two scaffolds,
  explicit surrogate/terminator policy params, P1-V3 evidence not the
  SK-V6 profile; P2-D §3.5: the differential is *broadening* the
  already-wired kernel, not "wire the kernel"). REDRESS 64 (retained
  Unicode-escape run validator — no validator state; the kernel is pure
  functional). REDRESS 66-69 (direct source-hook / parser-owned scratch /
  byte-output unescape / DirectBuild semantic strings — C4 does NOT
  extend to a DirectBuild field-fact emit site; P2-F §7.2 / §0: that
  expansion is REDRESS 66-69 territory and is not opened). REDRESS 88, 89
  orthogonal (codec is not the prefix-XOR / CTZ path).

---

#### C5 — 32-byte string-block scanner widening

- **Source.** P2-D §4 (`scan_string_special_block_32`).
- **Owner file paths (P2-D §4.3 slices):**
  - `bbnf-simd/src/aarch64/string_block.rs` — `scan_string_special_block_32`
    32-byte NEON body + the `interesting`-mask producer-side OR-fold.
  - `bbnf-simd/src/scalar/string_block.rs` — 32-byte scalar oracle.
  - `parse-that-regex/src/lib.rs:162` — `match_string_at_quote_trusted_utf8`
    producer-site rewire to the 32-byte block + scalar tail.
  - `bbnf-simd/tests/checkasm_string_block.rs` (NEW; overlaps C8) —
    differential gate covering the 16-byte and 32-byte bodies.
- **Scalar-reference status.** **Exists, must-extend.** The 16-byte
  `scan_string_special_block` has a scalar reference at
  `scalar/string_block.rs`; the 32-byte variant's scalar oracle is a
  new sub-slice (P2-D §4.3, in the ~60-110 LOC body slice).
- **Checkasm-parity status.** **Must-author.** No `checkasm_string_block.rs`
  exists today (REDRESS 83's wave added one and it was rejected with the
  patch). The C5 wave authors it (P2-D §6.2.1 / §4.3, ~40-70 LOC) as a
  same-wave precondition.
- **Same-wave consumer.** `match_string_at_quote_trusted_utf8` at
  `parse-that-regex/src/lib.rs:162` — the *full*-scan path (not the tiny
  path REDRESS 83 wired into); the 32-byte block replaces the producer
  at the existing call site, no new wrapper.
- **Falsifiability gate (P2-D §4).** The win is consumer-side
  mask-handling halving, **not** producer throughput (P2-D §4.2: the
  widening is roughly µop-neutral per byte) — so the gate must measure
  the *combined* producer + consumer path, not the block-scan
  microbench. Target rows: `unicode_mixed` (Track 1 today 6803, the
  `string_escape` + `string_full_scan` + `trailing_zeros` pipeline)
  and `gsoc-2018` (Track 1 today 22184, the `simd_movemask` +
  `trailing_zeros` + `string_block_scan` pipeline at 46.2% combined
  self-time). C5 *contributes* to `unicode_mixed`'s §6.4 closure — P2-E
  §6.3 binds `unicode_mixed`'s admission (`≥ 12,338 Mbps`) to "a
  same-wave per-string-span scanner intervention," which is C5. W10b
  six-row no-regression block applies (`canada`, `citm_catalog`,
  `instruments`, `marine_ik`, `mesh`, `numbers`).
- **Preliminary LOC envelope.** ~60-110 (`_32` body + scalar oracle) +
  ~30-60 (producer rewire) + ~15-30 (OR-fold) + ~40-70 (checkasm) ≈
  **~145-270 LOC** (P2-D §4.3).
- **Risk class.** **MEDIUM.** P2-D §4.3: the binding risk is the
  µop-neutral-per-byte finding — the win is consumer-side, so the gate
  must measure the combined path. Different call site than REDRESS 83
  (full-scan, not tiny-probe); the widening replaces the producer, not
  a wrapper layered on the consumer.
- **Dependency.** **Depends on C3 (same-wave) + C8.** P2-D §4.4 /
  §0: the CSSC CTZ sub-slice (C7) blocks on P2-A; the 32-byte body
  itself can wire into `match_string_at_quote_trusted_utf8` without C3,
  but to *close* `unicode_mixed` it is paired with C4 (the codec) in the
  same wave per P2-E §6.3. Practically C5 co-sequences with C4 in the
  C3-gated unicode-row wave. C8's `checkasm_string_block.rs` is a
  same-wave precondition.
- **REDRESS pre-blocks.** REDRESS 83 (W5 StringBlock16 tiny probe — P2-D
  §4.3 differential: different call site, full-scan not tiny; the
  widening replaces the producer, no wrapper layered on the consumer).
  REDRESS 60-62 (boundary collapse / always-or-delayed-wide retained
  trusted scan — P2-D §7: the widening is producer-side per-block, not a
  retained scanner). REDRESS 84 orthogonal.

---

#### C6 — SHA3 EOR3 vector prefix-XOR ladder

- **Source.** P2-D §5.3.1.
- **Owner file paths (P2-D §5.3.1 slices):**
  - `bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs` — vector
    `uint8x16_t` shift-XOR ladder with `veor3q_u8` 3-stage fold +
    Lock-16 `FEAT_SHA3` admissibility gate.
  - `bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs` (EXISTS) —
    extend with the EOR3 path under a forced `FEAT_SHA3` mask.
- **Scalar-reference status.** **Exists.** `bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs`
  is the production default (the body REDRESS 88 *kept*). The EOR3 ladder
  is a capability-conditional specialisation; the scalar shift-XOR ladder
  remains the unconditional fallback.
- **Checkasm-parity status.** **Exists, must-extend.**
  `checkasm_bitmap_prefix_xor_64.rs` is in tree; P2-D §5.3.1 adds a
  ~20-40 LOC extension covering the EOR3 path — the parity oracle must
  cover the vector-vs-scalar-vs-PMULL three-way differential.
- **Same-wave consumer.** The §5 union-substrate structural-bitmap
  producer (C3 scope) — the prefix-XOR turns the quote mask into an
  inside-string mask inside the structural classifier.
- **Falsifiability gate.** No row-closing gate of its own — C6 is a
  *producer accelerator* (P2-D §5.3.1 monotonic-µop argument: 3 EOR3
  ops vs 6-stage XOR, ~12 µops → ~6 µops). The binding gate is the W10b
  six-row no-regression maintain block (`canada`, `citm_catalog`,
  `instruments`, `marine_ik`, `mesh`, `numbers`) — the prefix-XOR hot
  body is the surface W10b proved regresses the WIN block even when
  correctness-green, so **no EOR3 body ships unless those six rows
  hold**. C6's contribution surfaces in C3's must-improve rows
  (faster structural producer → faster `twitter`/`apache_builds`/etc.),
  not as a row of its own.
- **Preliminary LOC envelope.** ~40-80 (EOR3 ladder + Lock-16 gate) +
  ~20-40 (checkasm extension) ≈ **~60-120 LOC** (P2-D §5.3.1).
- **Risk class.** **MEDIUM.** P2-D §5.3.1: MEDIUM despite the
  monotonic-µop argument because the vector-ladder representation
  differs from the u64-word scalar representation — the parity oracle
  must cover the three-way differential. Capability-gated
  (`FEAT_SHA3`), scalar fallback unconditional — the same admissibility
  shape as `digit_mac` (DotProd-gated).
- **Dependency.** **Cascade-locked to C3; lands as sub-wave W4c.**
  P2-D §5.3.1 / §0: the EOR3 ladder blocks on the P2-A (C3) union
  substrate existing — its only consumer is the §5 structural-bitmap
  producer (C3 / W3 scope); absent the substrate, C6 ships orphaned.
  Per the P3-F SPEC §2.2 reading, the cascade-lock is satisfied by W3
  preceding W4c — W4c wires the EOR3 ladder into the already-landed W3
  structural-bitmap producer same-commit.
- **Wave disposition.** C6 lands as **sub-wave W4c** — a fresh
  triumvirate that wires the SHA3 EOR3 prefix-XOR ladder into the
  already-landed W3 union structural-bitmap producer. It is neither
  dropped nor deferred: the W4 sub-wave structure (W4a string-block,
  W4b codec, W4c EOR3, W4d CTZ) gives each cascade-locked consumer its
  own ~75-min redress cap while preserving the same-wave-consumer rule
  against the W3 substrate.
- **REDRESS pre-blocks.** REDRESS 88 (W10 PMULL prefix-XOR default body
  — P2-D §5.3.1 three-axis differential: different intrinsic [EOR3
  3-way XOR, no carryless multiply], different latency profile
  [1-cycle vs 4-cycle], different primitive shape [vector ladder fold
  vs polynomial substitution]; EOR3 *accelerates* the scalar ladder
  REDRESS 88 kept, it does not re-admit PMULL). HANDOFF §5 ("PMULL
  prefix-XOR … as default hot paths" pre-block) applies to PMULL
  re-admission, explicitly not to the SHA3-gated wave-internal EOR3
  fold (P2-D §3.5 caveat).

---

#### C7 — CSSC CTZ at the string-mask first-set extract

- **Source.** P2-D §4.4.
- **Owner file paths.** `bbnf-simd/src/aarch64/` mask consumer — the
  CSSC CTZ body at the string-mask first-set extract (`ctz` under
  `-C target-cpu=native`) + a `cargo asm` proof that the intended CTZ
  sequence appears.
- **Scalar-reference status.** **Exists.** The consumer-side mask
  extract is `<u16>::trailing_zeros`, which the production rustc
  baseline lowers to `rbit + clz`; the CSSC `ctz` is a host-conditional
  specialisation, with `rbit + clz` as the unconditional fallback.
- **Checkasm-parity status.** **Covered by C5's `checkasm_string_block.rs`** —
  the CTZ extract is a sub-step of the 32-byte block scanner's mask
  consumer; its correctness is exercised by the string-block differential.
  No separate checkasm file; the `cargo asm` proof is the
  instruction-selection gate.
- **Same-wave consumer.** The union-substrate string-mask consumer (C3
  scope) — the per-mask first-set extract inside the C5 32-byte block
  scanner's consumer.
- **Falsifiability gate.** No row-closing gate of its own — C7 is a
  *consumer accelerator* (the `<u16>::trailing_zeros` 10.5% on
  `gsoc-2018/t1` is scalar RBIT+CLZ; native CTZ saves one µop per
  extract). The binding gate is the W10b six-row no-regression block —
  REDRESS 89 already rejected the structurally-adjacent CSSC CTZ body
  for `bitmap_next_set_bit` on a 3-8% WIN-block regression. C7's
  contribution surfaces inside C5's combined-path gate on `gsoc-2018`
  and `unicode_mixed`.
- **Preliminary LOC envelope.** ~15-35 LOC (P2-D §4.4) — the smallest
  candidate.
- **Risk class.** **HIGH.** P2-D §4.4: HIGH because REDRESS 89 already
  rejected the structurally-adjacent CSSC CTZ body; the differential
  (different call site — string-mask first-set extract, not
  `bulk_emit_positions_64`; different failure profile — LOSS rows under
  guard, not WIN rows) is plausible but unproven. The W10b six-row
  WIN-block no-regression gate is a hard blocking precondition.
- **Dependency.** **Depends on C3 (same-wave) + C5 + C8.** P2-D §4.4 /
  §0: the slice blocks on P2-A (C3) landing — the string-mask consumer
  that makes the CTZ extract non-orphan is C3 scope; and it operates
  inside C5's 32-byte block scanner consumer. Deepest dependency of any
  candidate.
- **Wave disposition.** C7 lands as **sub-wave W4d** — a fresh
  triumvirate that wires the CSSC CTZ string-mask first-set extract into
  the W4a 32-byte block scanner consumer over the already-landed W3
  union string-mask substrate. It is neither dropped nor deferred: as
  the leaf optimisation it is the last W4 sub-wave, admitted under the
  W10b six-row maintain gate, with W4a (string-block) as its strict
  predecessor.
- **REDRESS pre-blocks.** REDRESS 89 (W10b CSSC CTZ bulk consumer — P2-D
  §4.4 differential: different call site, different failure profile).
  HANDOFF §5 ("CTZ/bulk production rewires as default hot paths"
  pre-block) applies to default rewires; C7 is a host-capability-gated
  specialisation at a non-default call site under the W10b six-row
  maintain gate.

## §3 — Dependency graph

The dependency spine is fixed by P2-F §7.4, the S-P2 consolidation
("the dependency order is firm: P2-B proof → P2-A union → P2-D
consumers; P2-E codec is independent but conditional; P2-C is fully
independent"), and the P2-D §0 cascade-sequencing constraint.

```
  INDEPENDENT (depth 0 — may dispatch first)
  ────────────────────────────────────────────
   C1  Apache/CITM typed admission ........... no dependency
   C2  Retained class/event grammar proof .... no dependency
   C8  Checkasm-parity backfill .............. no dependency to author;
                                               bound as a precondition

  DEPTH 1 — gated by the C2 proof
  ────────────────────────────────────────────
   C2 ──unlocks──► C3  Union event-model
                       (P2-B §5: the proof removes the HANDOFF §5
                        pre-block; C3 becomes eligible to dispatch)

  DEPTH 2 — cascade-locked to C3 (W3 precedes the W4 sub-waves)
  ────────────────────────────────────────────
                       C3 (W3) ──precedes──► C4  codec   (W4b-1/2/3)
                          │                  C5  32-byte (W4a)
                          │                  C6  EOR3    (W4c)
                          └──────────────► (P2-D §0, binding reading
                            per P3-F SPEC §2.2: a P2-D kernel must not
                            land WITHOUT the union substrate existing —
                            satisfied by W3 preceding the W4 sub-waves;
                            it does NOT mean one monolithic wave. Each
                            W4 sub-wave wires its kernel into the
                            already-landed W3 union same-commit.)

  DEPTH 3 — inside the C5 consumer (W4a)
  ────────────────────────────────────────────
   C3 + C5 ──────► C7  CSSC CTZ string-mask extract (W4d)

  PRECONDITION EDGES (must land BEFORE consumer wiring of their wave)
  ────────────────────────────────────────────
   C8.checkasm_unescape_uxxxx ──► C4
   C8.checkasm_match_tiny     ──► C4
   C8.checkasm_string_block   ──► C5
   C8.checkasm_movemask       ──► C5
   C6 extends checkasm_bitmap_prefix_xor_64 (in its own wave)
```

Reading the graph for P3-B wave sequencing:

- **C1 and C2 are dispatchable in parallel** after W0 / S-P1-rerun
  convergence — neither blocks the other, neither blocks on a kernel.
- **C3 cannot precede C2.** The C2 proof is the *necessary* (not
  sufficient) gate; landing C3 first reopens the REDRESS 92 fit-gate
  failure mode that the proof exists to discharge.
- **C4, C5, C6, C7 are cascade-locked to C3.** P2-D §0 is explicit —
  no P2-D kernel may land *without the union substrate existing*.
  P3-F SPEC §2.2 gives the binding reading: the cascade-lock is
  satisfied by **W3 (the C3 union substrate) preceding the W4
  sub-waves** — it does NOT mean one monolithic redress wave. The C3
  union event-model is W3; the consumers are the W4 sub-waves W4a (C5
  string-block), W4b-1/W4b-2/W4b-3 (C4 codec, itself three sub-waves
  for LOC reasons — §2.2), W4c (C6 EOR3 ladder), W4d (C7 CSSC CTZ).
  Each W4 sub-wave is a fresh triumvirate that wires its kernel into
  the already-landed W3 union in the same commit — the consumer
  exists, no orphan ships. C4's codec moves rows only at the W4b-2
  sub-wave, strictly paired with W4a.
- **C8 is not a wave.** Its four test files distribute as same-wave
  preconditions of C4 and C5; `checkasm_digit_mac.rs` ownership is
  carried forward to a future numeric-row wave (P2-D §6.2.1 — no
  paper-close to a no-consumer wave).
- **C7 is the deepest** — it needs C3 (the union string-mask consumer)
  *and* C5 (the 32-byte block scanner it extracts from). It is a leaf
  optimisation, admissible last, under the W10b maintain gate.

## §4 — Proof-only vs row-moving classification

| ID | Class | Moves a `RESULTS.md` row? | What it produces |
|---|---|---|---|
| C1 | **Row-moving** | Yes — admits 2 new measured `real_typed_struct A / GO` rows (`apache_builds`, `citm_catalog`); maintains the 4 existing typed GO guards. | Measured typed row-table admission. |
| C2 | **Proof-only** | **No** — P2-B §1.1 explicit: "no row in `skinny/RESULTS.md` moves." | A compile-time contract (`EventGrammar` trait + `ValueRef` proof) that removes one HANDOFF §5 pre-block and makes C3 eligible. |
| C3 | **Row-moving** | Yes — must-improve `twitter`, `apache_builds`, `distinct_values`, `update_center` from `S / NO-GO` toward `≥ sonic`; `gsoc-2018` no-regression-only (gap exceeds the per-delimiter budget — does not bind the W3 gate); maintains the W10b six-row WIN block. | The structural fix: union event-model, `consume_structural` deleted. |
| C4 | **Row-moving (conditional)** | Conditionally — `unicode_escapes` + `y_string_unicode` NEAR-FAIL (94.5% / 94.8%); admission is the §6.4 same-wave conditional rule (admits iff *measured* Mbps clears the gate). `gsoc-2018` no-regression-only. | The `\uXXXX` codec; closes (conditionally) up to 2 unicode rows. |
| C5 | **Row-moving (conditional)** | Conditionally — *contributes* to `unicode_mixed`'s closure (P2-E §6.3 binds its `≥ 12,338` gate to a same-wave scanner intervention = C5) and to `gsoc-2018`. Does not close a row alone. | The 32-byte string-block scanner widening. |
| C6 | **Row-contributing** | **No row of its own** — a producer accelerator; its speed-up surfaces inside C3's must-improve rows. Carries a W10b six-row no-regression gate. | SHA3 EOR3 prefix-XOR ladder (faster structural producer). |
| C7 | **Row-contributing** | **No row of its own** — a consumer accelerator; surfaces inside C5's combined-path gate on `gsoc-2018`. Carries a W10b six-row no-regression gate. | CSSC CTZ string-mask first-set extract. |
| C8 | **Infrastructure-only** | **No** — checkasm differential tests; correctness gate, not throughput. | Same-wave admission preconditions for C4/C5/C6 (closes the standing DAV1D-discipline gap). |

Summary: **2 unconditional row-movers** (C1, C3); **2 conditional
row-movers** (C4, C5 — admit only if measured Mbps clears the gate);
**1 proof-only** (C2); **2 row-contributing accelerators** (C6, C7 —
no row of their own, gated by the W10b maintain block);
**1 infrastructure-only** (C8). The honest posture P2-E carries: zero
of the four uncloseable unicode rows admit on the codec alone — C4 is a
strong contributor that approaches but does not reliably cross the gate,
and `unicode_mixed` needs C4 + C5 paired. S-P3 P3-B/P3-C must sequence
the conditional row-movers behind a measured falsifiability gate and a
revert protocol, never a paper-close.

## §5 — Sources

- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md`
  — C3; §1 W3 fit-gate diagnosis, §2 alternate event-model, §4
  falsifiability gate + W10b six-row block, §5 per-slice cost, §6
  REDRESS pre-blocks.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md`
  — C2; §1 proof shape + owner files, §4 differential vs REDRESS 60-72,
  §5 what the proof unlocks + same-wave-consumer disposition, §6 LOC +
  risk.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md`
  — C1; §1 REDRESS 91 differential, §2 admission methodology + per-slice
  budgets, §3 per-row specifics, §4 owner files + falsifiability gates,
  §6 pre-block citations.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md`
  — C4 (§3), C5 (§4), C7 (§4.4), C6 (§5.3.1), C8 (§6.2 / §6.2.1); §0
  cascade-sequencing constraint, §7 REDRESS material differentials.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-E-unicode-escape-codec.md`
  — C4; §1 scalar diagnosis, §2 cross-grammar parameterisation, §3 SIMD
  design, §4 same-wave consumer plan, §5 REDRESS 82 differential, §6
  PMU-rederived falsifiability gate, §7 per-slice LOC + checkasm.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-F-sota-teardown-m5max.md`
  — §3 dependency spine; §7.4 inter-report dependency graph
  (`I ← P2-A ← P2-B`; `II ← P2-E`; `III ← P2-D ← P2-A`).
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
  — load-bearing diagnoses (dead SIMD scanner, string-scanner pair,
  unicode-escape codec, the four uncloseable rows); the OLS regression.
- `restart/skinny/tranches/sk-v9/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
  — S-P2 6/6 ≥95% convergence; the firm dependency order;
  "S-P3 owns the wave manifest, per-wave falsifiability gates, …".
- `restart/skinny/tranches/sk-v9/SPEC.md` — §0 close-condition + goalset;
  §1 non-negotiables; §2 wave manifest (W0 closed, behaviour waves
  conditional).
- `restart/skinny/tranches/sk-v9/HANDOFF.md` — §3 candidate boundaries +
  Alpha cost binding (Apache/CITM 300 LOC, retained proof 450 LOC); §5
  pre-blocked routes.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` — §2 P3-A scope
  (≤8 candidates, owner path / scalar-reference / checkasm-parity /
  same-wave consumer / falsifiability gate per candidate); §8 bbnf-lang
  axes (W0-first, same-wave consumer, no hypothesis transfer).
- `skinny/RESULTS.md` — the `SK-V9-open` 38-row baseline
  (`sk-v9-open:criterion-fnv64-cd1673844eeea12f`); per-corpus Track 1 /
  sonic-strict Mbps.
- `skinny/REDRESS.md` — entries 28, 33, 50-55, 60-72, 82-84, 88-93 (the
  pre-blocked routes each candidate must not re-open).

## §0 — V2 fold

V2 fold: F-AUX surgical touch-up per S-P3 V1 CHALLENGE.

## §0 V3 fold footer

V3 comprehensive integration. P3-A is reconciled to the unified P3-F
SPEC §2 manifest. Changes: (1) the §3 dependency-graph reading text and
the DEPTH-2 graph block are re-bound to the W4 sub-wave structure — C4
codec → W4b-1/W4b-2/W4b-3, C5 → W4a, C6 → W4c, C7 → W4d — and the
P2-D §0 cascade-lock is stated in its disambiguated P3-F SPEC §2.2
reading ("W3 precedes the W4 sub-waves", not "one monolithic wave");
the stale "one cascade-locked behaviour wave" / bare "the wave may not
be split" prose is corrected. (2) Arithmetic: the C3 falsifiability
gate's `update_center` floor `14369 → 14370` (`ceil(15806/1.10)`); the
W10b six-row block is floored uniformly (`floor(today × 0.98)`) —
`citm_catalog` `28631 → 28630`, `numbers` `17597 → 17596`,
`instruments` stated as `15865`. The C1..C8 shortlist and the per-
candidate detail are otherwise unchanged.

## §0 V4 fold footer

V4 fold per S-P3 V3 CHALLENGE CH1 (the lone surviving defect). The C3
falsifiability gate (§2.2) and the candidate-classification table (§4)
removed `gsoc-2018` from the W3 must-improve list — gsoc-2018's
throughput gap exceeds the per-delimiter budget, so the union substrate
alone cannot lift it; it carries a no-regression-only clause at W3,
consistent with F-spec §6 and P3-C §2. The dangling `§4.3`
cross-reference was replaced with a non-dangling clause. Cycle stamp
bumped to V4.
