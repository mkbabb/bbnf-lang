# SK-V12 Alpha-E Candidate Shortlist - User Pin Re-Bracket

Pass: Alpha SK-V11 -> SK-V12, lane alpha-E.
Date: 2026-05-20.
Scope: pin-aware shortlist only. This replaces the pre-pin Alpha-E shortlist
that treated Sheets as an equivalent fallback and used the old
`ceil(baseline_mbps * 1.01)` non-JSON bar.

## Authority Read

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md` Section 6.5 and Section 7
- `restart/skinny/tranches/sk-v12/SPEC.md` as pre-pin context only where it
  does not conflict with the user pin
- Six 2026-05-20 audits:
  `skv12-W1-A7-sheets-execution-scout.md`,
  `skv12-aarch64-simd-coverage-audit.md`,
  `skv12-profile-truth-audit.md`,
  `skv12-value-api-audit.md`,
  `skv12-decision-engine-audit.md`,
  `skv12-totality-fold-scout.md`
- Pre-pin S-P1/S-P2/S-P3 converged artifacts, as context only after measured
  revalidation under the user pin:
  `research/p1/hardening/HARDENING-S-P1-CONVERGED.md`,
  `research/p2/hardening/HARDENING-S-P2-CONVERGED.md`,
  `research/p3/hardening/HARDENING-S-P3-CONVERGED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 120

## Pin Rebinding

The user pin changes the Alpha-E routing surface:

- CSS L4 declaration values are authoritative. Sheets and BBNF-self are
  fallbacks only after a CSS L4 redress attempt fails, not after a preflight
  skip.
- The CSS admission floor is strict `generated_track1_mbps >
  lightningcss_mbps + 1` on the same corpus, same output plane, strict
  equality, same-host run. The integer Mbps values emitted by the gate/report
  row are compared directly; equality at `+1` is a miss.
- The seven Lock 14 leaks in `json_templates/generated.rs` must be extracted
  through a `GrammarConfig`/generated-metadata surface before CSS emission is
  legal.
- `escape_mask_64` correctness is a campaign blocker before any new SIMD
  admission.
- The union-substrate and ASM-gen categories are unblocked at category level.
  REDRESS 88/89/90 and 96/97/98 remain historical implementations; new
  attempts must cite them, name the material differential, pass CHALLENGE, and
  satisfy scalar reference, checkasm/parity, microbench, and same-wave consumer
  gates.
- JSON direct residuals remain guard-only under REDRESS 119 unless a future
  route names fresh material evidence beyond REDRESS 114-119. `parse_only` is
  diagnostic-only.

## Shortlist

| ID | Candidate | Primary role | Wave seed | Hand LOC cap | Generated LOC ceiling | Risk |
|---|---|---|---|---:|---:|---|
| E2 | `GrammarConfig` / Lock 14 leak extraction | legalizes CSS emission | W1a | <=360 | <=1200 smoke/regenerated LOC | high |
| E1 | CSS L4 generated baseline plus lightningcss comparator | mandatory first admission target | W1b | <=620 | <=12000 CSS declaration-value generated LOC unless S-P3 proves a tighter/full-stylesheet ceiling | high |
| E3 | `escape_mask_64` correctness closure | required before SIMD admission | W2 | <=180 | 0 | high |
| E4 | CSS-local same-tape event union | union-substrate category attempt | W3 | <=420 | <=4000 generated delta LOC over E1 baseline | high |
| E5 | ARMv9.2 TBL/TBX CSS byte-class consumer | ASM-gen/SIMD category attempt | W4 | <=430 | <=2500 generated delta LOC over current CSS runtime | high |
| Close | G-Alpha close/fixpoint docs | reconcile campaign close state | W5 | docs-only | 0 | medium |

E2 and E1 are sequential, not one redress slice. E2 is the generic-crate
prerequisite without which E1 is not legal; E1 is the first row-moving CSS
admission attempt. E3 must close before E5 or any other new SIMD admission.
E4 and E5 are eligible later-wave candidates after the CSS row and lightningcss
comparator lane are measurable, unless S-P3 records a measured reason to
attempt the required union/ASM-gen categories as rejected FIXPOINT evidence.
All wave seeds inherit the campaign caps: 20 min research, 15 min plan, and
30 min redress. At S-P3, each generated output slice must report pre/post
generated LOC and return REVISE on an unexplained O(N) increase.

### E1 - CSS L4 Generated Baseline Plus Lightningcss Comparator

- Purpose: admit `css_l4/declaration_values/direct_to_struct/main` as a
  generated CSS L4 row and compare it against lightningcss on the same fixture,
  output plane, host, flags, and strict equality path.
  The shared output representation is a canonical CSS declaration-value fact
  stream used symmetrically by generated Track 1, independent Track 2/oracle,
  and lightningcss. S-P3 may select a full-stylesheet row only if the row id,
  fixture, equality adapter, and lightningcss comparator all move to the same
  full-stylesheet output plane.
- Owner paths:
  - `grammar/css/l4/values.bbnf`
  - `grammar/css/l4/tokens.bbnf`
  - `grammar/css/l4/value-unit.bbnf`
  - `grammar/css/l4/color.bbnf`
  - `skinny/crates/codegen/src/lib.rs`
  - `skinny/crates/codegen/src/json_provider.rs` only to remove/bypass the
    JSON-only guard without turning it into a polymorphic grammar policy table
  - selected grammar-neutral codegen provider/template files named by E2
  - `skinny/crates/runtime/src/lib.rs`
  - `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
  - `skinny/crates/bbnf-bench/Cargo.toml`
  - `skinny/crates/bbnf-bench/benches/nonjson_baseline.rs`
  - `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
  - `skinny/crates/bbnf-bench/src/report.rs`
  - `skinny/crates/bbnf-bench/src/bin/gate.rs`
  - `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
  - `restart/skinny/tranches/sk-v12/research/w1/css-l4/`
- Scalar reference status: open. The generated Track 1 parser is the candidate
  under test; the independent scalar/oracle side is lightningcss plus a
  same-plane equality adapter over CSS declaration-value facts. The oracle must
  not call generated Track 1, generated CSS sink helpers, or a report fixture.
- Checkasm/parity status: checkasm N/A for the baseline if scalar-only.
  Required parity is strict equality against the lightningcss-derived oracle
  and independent Track 2/source provenance. Any SIMD helper used by the
  baseline inherits E3/E5 gates.
- Same-wave consumer: Criterion row
  `nonjson/css_l4/declaration_values/direct_to_struct`, equality artifact, and
  non-JSON gate report. Producer-only telemetry rejects.
- Gate: `G-W1b-CSS-L4-LIGHTNINGCSS-BASELINE`.
  - generated Track 1 Mbps > `lightningcss_mbps + 1`
  - lightningcss comparator Mbps measured same-run and same corpus
  - strict equality PASS for every fixture item
  - sample count >= 30
  - output plane is the selected CSS declaration-value fact stream over the
    generated direct sink, with symmetric lightningcss fact extraction
  - report schema `sk-v12-nonjson-generated-v1` consumed by gate
  - Lock 14 Section 2.1 clean
  - if any generic runtime, codegen, generated-output, benchmark, report, or
    gate path that can produce JSON moves, JSON direct/typed guards are
    refreshed; the no-refresh shortcut is legal only when no JSON-producing
    path moved and `skinny/RESULTS.md` is proven unchanged
- LOC budget: <=620 handwritten source/test/gate LOC plus generated CSS output
  outside the hand LOC budget. This deliberately widens the pre-pin 520 CSS
  estimate because the pin makes lightningcss parity and comparator wiring
  mandatory.
- Risk: high. This crosses REDRESS 112/113 and establishes the campaign close
  row.
- Revert: revert codegen/runtime/bench/report/gate/RESULTS and generated CSS
  files as one slice; save `/tmp/skv12-waveW1b-rejected.patch`; REDRESS records
  exact blocker or measured miss. E1 failure blocks CSS emission until S-P3
  replans the row, comparator, and equality adapter.

### E2 - `GrammarConfig` And Lock 14 Leak Extraction

- Purpose: remove the seven JSON policy leaks called out by
  `skv12-value-api-audit.md` so E1 can emit CSS L4 without routing through JSON
  value/container/string/number/key/sink policy.
- Owner paths:
  - `skinny/crates/runtime/src/tape/grammar_config.rs`
  - `skinny/crates/runtime/src/tape/mod.rs`
  - `skinny/crates/runtime/src/tape/assembler.rs`
  - `skinny/crates/runtime/src/lib.rs`
  - `skinny/crates/codegen/src/lib.rs`
  - `skinny/crates/codegen/src/json_templates/generated.rs` only to preserve
    or extract existing JSON parity; it may not become the polymorphic CSS
    provider and may not branch on CSS/JSON grammar names
  - new grammar-neutral codegen template/provider files, for example
    `skinny/crates/codegen/src/nonjson_profile.rs` and
    `skinny/crates/codegen/src/generated_config.rs`
  - generated CSS config/output under
    `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
  - `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
  - `skinny/crates/bbnf-bench/src/report.rs`
- Scalar reference status: scalar-only API extraction. The reference is
  byte-for-byte equivalence for existing JSON generated output and a compiling
  CSS generated module whose grammar policies come from generated metadata.
- Checkasm/parity status: checkasm N/A unless E2 changes SIMD contact points.
  Required parity is JSON regen parity plus a CSS generated-config compile
  smoke consumed by the W1a gate.
- Same-wave consumer: a generated CSS declaration-value config smoke module
  plus JSON regen parity consume the new `GrammarConfig` surface in W1a. E1
  then consumes the same surface for the row-moving baseline in W1b. A generic
  API addition with no generated CSS consumer is a paper close.
- Gate: `G-W1a-GRAMMARCONFIG-LOCK14`.
  - no new public JSON-named API in generic crates
  - no generic branch on grammar name, corpus name, object/array role, field
    name, string role, or layout role
  - structural alphabet, dispatch primary set, escape policy, number policy,
    key/member policy, flag interpretation, and sink trait are supplied by
    generated grammar metadata or per-grammar generated modules
  - JSON generated output remains parity-green
  - because W1a owns generic runtime, codegen, generated-output, benchmark,
    report, and Lock 14 paths, direct/typed JSON guards refresh or record
    measured REDRESS demotion unless no JSON-producing path moved and
    `skinny/RESULTS.md` is proven unchanged
  - CSS generated-config smoke module compiles and is gate-consumed; W1b must
    later consume the same surface in the row-moving CSS baseline
- LOC budget: <=360 handwritten LOC.
- Risk: high. This touches generic codegen/runtime substrate and is the main
  Lock 14 failure surface.
- Revert: revert generic API/template/runtime changes and smoke generated CSS
  output together; save `/tmp/skv12-waveW1a-rejected.patch`. If E1 later fails
  for a Lock 14 reason, W1b records the dependency and blocks further CSS
  emission until W1a is replanned.

### E3 - `escape_mask_64` Correctness Closure Before SIMD

- Purpose: verify and resolve the NEON/string escape-mask correctness blocker
  (`0xCAFEF00DBAADF00D`) before any new SIMD/ASM admission. This candidate is
  a correctness gate, not a throughput admission.
- Owner paths:
  - `skinny/crates/bbnf-simd/src/lib.rs`
  - `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
  - `skinny/crates/bbnf-simd/src/aarch64/byte_context.rs` only if boundary
    handoff is needed
  - `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
  - `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
  - `skinny/crates/bbnf-simd/tests/corpus_parity.rs`
  - `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
  - `skinny/REDRESS.md`
- Scalar reference status: existing scalar `escape_mask_64` in
  `skinny/crates/bbnf-simd/src/lib.rs` is the reference, subject to explicit
  boundary-state semantics (`bs_carry_in` to `new_carry`) being documented by
  tests.
- Checkasm/parity status: incomplete for the pin. Add an adversarial xorshift
  reproducer using seed `0xCAFEF00DBAADF00D`, boundary backslash runs,
  carry-in true/false, every legal alignment, and corpus parity. A PASS must
  run before E5 or any other SIMD candidate can claim admission.
- Same-wave consumer: Lock 16/checkasm gate and corpus parity harness. If a
  source fix is needed, the harness is the same-wave consumer; no performance
  row moves from E3 alone.
- Gate: `G-W2-ESCAPE-MASK64-CORRECTNESS`.
  - scalar differential PASS for adversarial seed and boundary cases
  - corpus parity PASS on expanded skinny corpus
  - CHECKASM report updated with the resolved failure signature
  - no throughput/SOTA admission claimed
- LOC budget: <=180 handwritten LOC.
- Risk: high because it gates all later SIMD claims. Scope is intentionally
  narrow and correctness-only.
- Revert: revert SIMD source/test/report edits; record REDRESS if the bug
  cannot be resolved inside cap; save `/tmp/skv12-waveW2-rejected.patch`.
  Later SIMD waves stay blocked.

### E4 - CSS-Local Same-Tape Event Union

- Purpose: exercise the user-pin-unblocked union-substrate category with a new
  implementation that targets CSS L4 declaration-value alternatives rather than
  JSON parse-plane structural rediscovery.
- Material differential vs REDRESS 96/97/98:
  - REDRESS 96 wrote a JSON class-column substrate plus move-consumed
    structural index and regressed every row.
  - REDRESS 97 removed allocation but retained a JSON streaming cursor route
    and still regressed.
  - REDRESS 98 retired that specific SK-V9 gate.
  - This candidate is generated, CSS-local, output-plane-owned, and consumed
    inside the CSS declaration-value direct parser. It does not retain a second
    structural vector, public substrate API, parser-owned sidecar, or
    parse_only scanner. The union tag is an in-row generated Rust enum or
    same-tape event projection used immediately by the CSS direct sink.
- Owner paths:
  - `skinny/crates/codegen/src/lower/`
  - `skinny/crates/codegen/src/direct_schema.rs`
  - selected generated CSS runtime under
    `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
  - `skinny/crates/runtime/src/tape/event_grammar.rs` only to consume existing
    sealed/internal bounds; no exported public substrate item may be added
  - `skinny/crates/runtime/src/tape/mod.rs` only if an existing generic
    `EventGrammar` bound must be consumed without public API expansion
  - `skinny/crates/bbnf-bench/benches/nonjson_baseline.rs`
  - `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
  - `skinny/crates/bbnf-bench/src/report.rs`
- Scalar reference status: required. The scalar reference is the pre-union CSS
  generated baseline from E1 with identical fact output; E4 adds an isolated
  microbench comparing baseline dispatch vs generated union dispatch.
- Checkasm/parity status: checkasm N/A unless the union producer consumes
  SIMD. Product parity against E1/lightningcss is mandatory.
- Same-wave consumer: CSS L4 generated direct parser consumes the union route
  in the row measured by the non-JSON gate. No helper-only event model lands.
- Gate: `G-W3-CSS-SAME-TAPE-UNION`.
  - E1 baseline admitted first
  - microbench shows positive same-host movement on CSS declaration-value
    dispatch before source redress continues
  - generated CSS Track 1 > `lightningcss_mbps + 1`
  - generated CSS Track 1 is faster than E1 baseline or records a measured
    REDRESS reject
  - strict equality PASS against lightningcss oracle
  - no public substrate API, second retained substrate, sidecar class column,
    retained structural vector, or parse_only admission
  - public API diff proves no directive, BIR variant, BackendShape variant,
    `UnionTape`, generic event side vector, retained cursor/list, or
    parser-owned fact slot was added
- LOC budget: <=420 handwritten LOC plus regenerated CSS output.
- Risk: high. This is the required new union category attempt and must pass
  mandatory CHALLENGE before redress.
- Revert: revert generated union source, CSS generated output, bench/report,
  and RESULTS/REDRESS changes as one slice; save
  `/tmp/skv12-waveW3-rejected.patch` if measured negative.

### E5 - ARMv9.2 TBL/TBX CSS Byte-Class Consumer

- Purpose: exercise the ASM-gen/ARMv9.2 category with a real CSS consumer and
  zero orphan kernel posture. Preferred primitive is a NEON TBL/TBX byte-class
  classifier for CSS declaration-value layout/delimiter/string-interesting byte
  sets; fallback within this candidate is UDOT digit span only if CSS numeric
  token profile names digit-run as the hot leaf.
- Material differential vs REDRESS 88/89/90:
  - REDRESS 88 rejected PMULL prefix-XOR as a JSON default body.
  - REDRESS 89 rejected CSSC CTZ bulk consumer/canary fold.
  - REDRESS 90 admitted only canary hardening Stage 1, not row movement.
  - This candidate uses CSS row-local byte-class or digit-run work, not PMULL
    prefix-XOR default, not CTZ bulk emit, and not canary hardening as a row
    movement claim.
- Owner paths:
  - `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs`
  - `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs`
  - `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs`
  - `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs`
  - `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs`
  - `skinny/crates/bbnf-simd/src/dispatch.rs`
  - `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
  - `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs`
  - selected generated CSS runtime under
    `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
  - `skinny/crates/parse-that-regex/src/lib.rs` only for a grammar-neutral
    byte-set span wrapper
  - `skinny/crates/bbnf-bench/benches/nonjson_baseline.rs`
  - `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- Scalar reference status: existing eq-set/table scalar references are usable;
  the selected CSS caller still needs a scalar byte-set span reference with
  generated CSS byte classes.
- Checkasm/parity status: existing byte-class checkasm is a starting point.
  Required additions: CSS delimiter/layout/string-interesting byte sets, tails,
  alignment, high-bit bytes, duplicate-set cases, and corpus windows. E3 must
  pass first if the consumer touches string/escape scanning.
- Microbench status: required before wave-scoping. It must isolate the CSS
  byte-class caller and prove positive movement over scalar on this host.
- Same-wave consumer: generated CSS declaration-value parser consumes the
  classifier in layout skip, delimiter dispatch, or string-interesting scan.
  Dispatch-table-only or checkasm-only work rejects as orphan.
- Gate: `G-W4-CSS-ARMV9-TBL-CONSUMER`.
  - E3 correctness PASS if string/escape path is touched
  - scalar differential/checkasm PASS
  - same-host microbench positive on selected CSS caller
  - generated CSS Track 1 > `lightningcss_mbps + 1`
  - strict equality PASS against lightningcss oracle
  - carried orphan set zero or explicitly inventory-demoted with evidence:
    `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
    `byte_context`, and `cache_hints`
  - if any JSON-producing path moves, refreshed JSON guards hold or record
    measured REDRESS demotion; otherwise `skinny/RESULTS.md` is proven
    unchanged
- LOC budget: <=430 handwritten LOC plus regenerated CSS output.
- Risk: high. It touches native SIMD dispatch and generated consumer wiring.
- Revert: revert SIMD/source/generated/bench/report changes as one slice; if
  parity passes but row movement fails, record measured REDRESS, save
  `/tmp/skv12-waveW4-rejected.patch`, and leave no orphan native body. A
  parity pass with row miss must either remove the native body, demote it as
  inventory-only with evidence, or admit a non-orphan consumer.

## Non-Shortlisted Fallbacks

Sheets and BBNF-self remain legal only after a CSS L4 redress attempt fails and
is recorded. They are not Alpha-E candidates under the user pin because D1
makes CSS L4 authoritative for the current campaign.

JSON direct residuals are also not shortlisted here. REDRESS 119 remains the
guard authority unless a later pass supplies fresh profile, microbench, and
material differential evidence beyond REDRESS 114-119.
