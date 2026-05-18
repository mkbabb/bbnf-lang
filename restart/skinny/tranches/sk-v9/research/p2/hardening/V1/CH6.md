# CH6 — ANTI-PAPER-CLOSE — S-P2 Research V1

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-18.
Lens: CH6 per `restart/prompts/ORCHESTRATOR.md` §3W + §8 non-negotiable
"No deferrals — a wave closes on measurement, not a future-phase
promise."
Cohort: P2-A, P2-B, P2-C, P2-D, P2-E, P2-F (S-P2 V1).
Authority: `restart/prompts/skinny/PASS-2-RESEARCH.md` §CHALLENGE; the
S-P1 hand-off `restart/skinny/tranches/sk-v9/research/p1/hardening/
HARDENING-S-P1-CONVERGED.md`; PMU rows at `/tmp/skv9-xctrace-v3/
pmu_rows.tsv`; the in-tree code referenced in each report's §sources.
Note on cited per-symbol exports: P2-A and P2-E cite the directory
`/tmp/skv9-xctrace-v3/p1b-tp/exports/` (per S-P1 CONVERGED §1.2). At
audit time the directory is absent on the runner (the parent
`/tmp/skv9-xctrace-v3/pmu_rows.tsv` survives); spot-checks of cited
self-time percentages therefore route through P1-V3-B / P1-V3-C
report tables, which are themselves derivative artefacts of the
missing exports.

## §1 Method — live-evidence audit protocol

For each report I sampled ≥5 load-bearing self-reports and ran each
against the same three-part predicate:

1. **Citation resolves.** The `path:line` or `commit:SHA` named in the
   report points at content that materialises the claim in-tree at
   audit time.
2. **Derivation is grounded.** Quantitative claims (LOC, %self-time,
   c/B, µop count, Mbps projection) are sourced from a primary
   artefact (PMU TSV row, P1-V3-B/C table, file-counted LOC) and the
   derivation is reproducible from that source.
3. **Convergence test is measurable.** The §falsifiability §gate
   block names a row + threshold + verifier that an S-P3 redress can
   *fail* or *pass* in a single commit. A "near-fail at 94.5% of the
   threshold" without a binding halt-or-proceed clause counts as
   paper-close.

Cross-checks ran in two passes. Pass A (file-existence): every cited
`path:line` was hit with `wc -l` + `grep` + `head` against the
current working tree. Pass B (semantic): the quantitative claim was
recomputed against the primary source where possible (PMU TSV →
c/B; file-counted LOC → table cell; in-tree symbol search → wired
vs unwired).

The disposition table below records one row per probed claim:
**ACCEPT** = predicate holds across all three parts; **REVISE** =
citation resolves but derivation or measurability slips; **REJECT** =
the claim is materially false at audit time.

## §2 Disposition table — per report

### §2.1 — P2-A union event-model

| # | Claim | Citation | Audit verdict | Notes |
|---:|---|---|---|---|
| A-1 | `JsonNodeKind::at_cursor` at `value.rs:29-47` carries the per-cursor byte-rediscovery: `match tape.source()[offset]` over `{}[],:"` + digit/`t`/`f`/`n`. | `skinny/crates/runtime/src/grammars/json/value.rs:29-47` | **ACCEPT** | File lines 29-47 hold exactly the `match tape.source()[offset]` block over the claimed byte set. The "second hidden redundancy" framing (§7 source comment) is well-grounded — the SIMD class is rediscovered per cursor at view time. |
| A-2 | "~265 hand + 120 regen LOC" total budget (`§5`). | §5 row breakdown | **REVISE** | The §5 table lists per-slice estimates (`+60/-20` tape, `+15` parser, `+80/-50` regen, `+120` templates, `+20` simd, `+10/-5` scan, `+30` parity) which sum to ~265 hand + ~120 regen as stated. Derivation arithmetic checks. But the figures are *prospective* (no commit exists), so they are forecasts, not counted source. CH6 admits forecasts only with a binding S-P3 LOC ceiling — the report does not name one. |
| A-3 | `consume_structural` at `generated.rs:280-306` is the per-byte scalar rediscovery to delete. | `generated.rs:280-306` | **ACCEPT** (file-resolve) | File path exists; line range matches S-P1 V3 reports referenced by P1 §1 dead-SIMD-scanner finding. Symbol existence verified by S-P1 antecedent. |
| A-4 | Falsifiability gates per §4.1 are concrete (`consume_structural ≤5% self-time on twitter`, `at_cursor ≤1% self-time`, plus Mbps floors per row). | §4.1, §4.4 | **ACCEPT** | The six falsifying observations at §4.4 are individually executable: each names a hot leaf, a measurement (self-time or Mbps), and a threshold. CH6 passes — failures terminate, successes admit. |
| A-5 | The alternate model does *not* re-open REDRESS 50–55, 60–72, 82–84, 88–89, 92. | §6 | **ACCEPT** (logic) | Each REDRESS entry is named with its falsifier; the proposal binds itself to those falsifiers ("if a pass other than the parser writes `classes`, this fails REDRESS 50"). Predicate-level differential is sound. |
| A-6 | The model does NOT close `gsoc-2018` to its 41198 floor on the structural delta alone (§4.3). | §4.3 | **ACCEPT** | Explicit acknowledgement that gsoc-2018 lands partially-closed and the residual routes to P2-D / P2-E; this is the *opposite* of paper-close — the report carries its own unworkability. |

P2-A self-reports survive CH6: 5 ACCEPT, 1 REVISE on LOC-forecast
discipline. No paper-close; no deferral; the report self-admits its
gsoc-2018 ceiling rather than overclaim.

### §2.2 — P2-B retained grammar proof

| # | Claim | Citation | Audit verdict | Notes |
|---:|---|---|---|---|
| B-1 | "395 LOC source" (§1.2 + §6.1 totals): `~110 + 80 + 120 + 80 + 5 = 395`. | §1.2, §6.1 | **ACCEPT** (arithmetic) | The 5 numbers in the LOC column add to 395. The figures are *prospective* (the files don't exist yet — the report explicitly says "NEW" on each). The §6.1 table is therefore a budget, not a count. CH6 accepts because §1.2 names the file paths and the trait body is sketched at §1.3, making the estimate testable at S-P3. |
| B-2 | "Sheets recommended over CSS L4 because Sheets exercises the StructuralClassTable edge case SC-6 §4.4 names (escape-equals-delimiter: `""` denotes a literal quote)." (§1.4, §3.1) | `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md` §4.4 | **REVISE** | SC-6 §4.4 is cited by section number but no `path:line` resolves the doubled-quote claim back to SC-6's text. The rationale is internally plausible (Sheets does use `""` escape) but the SC-6 reference is opaque without a line cite. CH6 REVISE on citation precision — the structural reason "Sheets has no production parser, so witness cannot accidentally land a consumer" (§3.1 third bullet) is the *load-bearing* reason and that one is self-evident. |
| B-3 | The proof has no production consumer; gated behind `#[cfg(any(test, feature = "proof"))]`; `cargo bench -p bbnf-bench` cannot reach it (§4.2, §5.1). | §4.2, §5.1 | **ACCEPT** | The same-wave-consumer-rule disposition at §5.1 is formally argued: the rule binds substrates, the proof is a contract. The cfg-gate strategy is concretely named (parent `pub mod` gate in `lib.rs`); R4 in §6.2 names a verification `rg` against `bbnf-bench/`. No paper-close. |
| B-4 | "The compiler refusing to emit either `const _: fn() = _proof_compiles::<JsonEventGrammar>` line is the proof's failure mode" (§1.4). | §1.4 | **ACCEPT** | Concrete compile-time falsification. `cargo check -p runtime` is the verifier; result is binary (compiles or does not). CH6 passes. |
| B-5 | §5 explicit non-unlock list: the proof does NOT unlock SC-3 Tier A migration, REDRESS 91 admission, REDRESS 93 direct contract, SC-6-L1-R1 refinement. | §5 | **ACCEPT** | The "necessary but not sufficient" disposition is the *anti-paper-close* posture. The report carries explicit non-unlock fences rather than overclaiming what the proof closes. |
| B-6 | The `EventGrammar` trait method surface is "deliberately minimal" — adding `step_into`, `event_kind`, `class_at` would invite either generic-event-role Lock 14 violation or runtime callback consumer creep (§1.3 closing paragraph). | §1.3 | **ACCEPT** | Specific anti-creep argument; the report names what it refuses to add and why. Method addition is *deferred to whichever future wave reopens the union substrate; this is recorded under §5* — this deferral is legitimate scope-shedding (the proof is contract-only), not S-P2 paper-close. |

P2-B self-reports survive CH6: 5 ACCEPT, 1 REVISE on SC-6 §4.4
citation depth. No paper-close; the proof *is* the falsifier and the
report explicitly carries its non-unlocks.

### §2.3 — P2-C apache + CITM admission

| # | Claim | Citation | Audit verdict | Notes |
|---:|---|---|---|---|
| C-1 | "REDRESS 91 gap is the `SK_V8_OPEN_BASELINE` whitelist at `report.rs:709`" (§2.1) | `skinny/crates/bbnf-bench/src/report.rs:709` | **ACCEPT** | `pub const SK_V8_OPEN_BASELINE: &[SkV8OpenBaseline] = &[` is at `report.rs:709`. Verified by `grep -n SK_V8_OPEN_BASELINE` (line 709 hit). |
| C-2 | `w0_real_typed_metadata_expected("apache_builds") == false` and `== false` for `citm_catalog`; `gate.rs:1199-1201` defines the helper and `gate.rs:1826-1831` asserts the regression test. | `gate.rs:1199-1201`, `:1826-1831` | **ACCEPT** | `grep -n` confirms `w0_real_typed_metadata_expected` at line 1199; the four assertions at 1827-1830 match the report (twitter+update_center expected, apache+citm not). The regression test name `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures` is canonical. |
| C-3 | Apache PMU c/B Track 1 = 2.910, Track 2 = 2.862; CITM Track 1 = 1.180, Track 2 = 1.703; rows 8-9 / 4-5 in `/tmp/skv9-xctrace-v3/pmu_rows.tsv`. | PMU TSV | **ACCEPT** | TSV column `cycles_per_byte`: apache_builds/track1 = 2.909724, track2 = 2.862380; citm_catalog/track1 = 1.179831, track2 = 1.703392. Reproduced to three places; rounding matches. Row order in TSV: citm at lines 4-5, apache at lines 8-9; report citation is correct. |
| C-4 | "the typed parsers exist; strict parity tests pass; schema identity is `sk-v8-real-typed-w2`" (§3 closing paragraph). | `real_typed_struct.rs:595-618`, `real_typed_schema.rs:57-99` | **ACCEPT** (file path resolves) | Cited line ranges fall inside the named files. The parity claim is provenance-bound to existing W2 state, not to S-P2 work. |
| C-5 | "PMU c/B for `real_typed_struct` rows does not exist in the SK-V9-open evidence" (§2.8). | `/tmp/skv9-xctrace-v3/pmu_rows.tsv` column space; `xctrace_probe.rs:28-46` | **ACCEPT** | TSV has only parse-only rows; no typed-track column. The report self-discloses the absence rather than fabricating a typed-track c/B. CH6 ACCEPT — explicit absence acknowledgment is the opposite of paper-close. |
| C-6 | Falsifiability gates per §4.3 (Apache + CITM typed `≥ sonic × 1.10⁻¹`; existing 4 typed GO rows hold; Lock 14 `cargo test -p bbnf-bench lock14_baseline` green). | §4.3 | **ACCEPT** | Each gate row names a verifier, a threshold, and a halt-on-fail clause ("A wave that misses any gate halts at the redress phase, records the falsified gate in REDRESS, and routes back into S-P2/S-P3 without promoting the row"). Strong CH6 posture. |
| C-7 | "Typed track not measured" — the probe binary at `xctrace_probe.rs` is hard-wired to parse_only. (§2.8) | `xctrace_probe.rs:28-46` (cited from S-P1 V3-A) | **ACCEPT** | The report does not paper-close around this absence; it routes the typed-PMU extension as *optional* and explicitly NOT a prerequisite for `A/GO` admission. Honest scope-shedding. |

P2-C self-reports survive CH6: 7 ACCEPT, 0 REVISE. The strongest
adherence to CH6 in the cohort — every threshold names a verifier, every
absent measurement is self-disclosed.

### §2.4 — P2-D aarch64 ASM opportunities

| # | Claim | Citation | Audit verdict | Notes |
|---:|---|---|---|---|
| D-1 | "`unescape_uxxxx_x4_neon` at `:125` exists. **Neither is wired into the parse-that-regex hot path.**" (§2.1, bold in original) | `bbnf-simd/src/aarch64/unescape_uxxxx.rs:125`; `parse-that-regex/src/lib.rs` | **REJECT** | The kernel exists at line 125 (verified). But it IS wired — `parse-that-regex/src/lib.rs:402` in `unescape_four_unicode_escapes` calls `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon(&packed)`. P2-E's §1.2 correctly identifies this consumer at lines 384-459. P2-D's claim contradicts the in-tree state and is the most load-bearing factual error in the cohort. |
| D-2 | SHA3 EOR3 collapses 12 µops → 6 µops on the vector prefix-XOR ladder (§5.3.1). | §5.3.1 derivation; ARM ARM cited at §8 sources of *other* reports | **REVISE** | The µop-arithmetic derivation is sketched: 6-stage shift-XOR ladder → 3 EOR3 instructions, each 3-way XOR. Arithmetic is internally consistent (3 EOR3 × 2 µops/instr ≈ 6 µops, vs 6 XOR pairs × 2 ≈ 12). But the report cites no `ARM ARM §C7.x` for EOR3 latency/throughput, and asserts "1-cycle latency" without a §reference. The Apple-M5-EOR3 retire latency is plausible but not differentially proven against PMULL.1Q's "4-cycle latency" claim (also unsourced). CH6 REVISE — the µop derivation needs a primary-source latency anchor. |
| D-3 | Per-quartet hex_nibble cost "~28 µops/quartet" via "6-7 µops/digit × 4 digits" (§1.4). | §1.4 derivation | **REVISE** | 6-7 µops/digit × 4 digits = 24-28 µops, then *unspecified* fold/guard work; the report writes "~28 µops" without itemising the assumed branch costs. The number is plausible (P2-E §1.5 gives ~30 µops/quartet by a related but distinct decomposition: 4×5 ops/byte + 5 + 4 = 29). Two reports agreeing inside ±10% suggests the order-of-magnitude is correct, but neither sources a primary measurement. |
| D-4 | Falsifiability gate "the §3 and §4 primitives can be admitted on the existing checkasm infrastructure IF they ship their per-primitive checkasm test (gap §6.2)" (§6.3). | §6.2, §6.3 | **REVISE** | §6.2 names the missing checkasm tests as a *gap*; §6.3 lists invariants 2-5 as "Insufficient" and concludes "fuller invariant 2-5 closure is SK-V10+ work per the SK-V7 A3 §2 menu and skv6-B2; deferring those does **not** block §3/§4 admission." This is a textbook CH6 paper-close pattern — naming a gap as a gap, then deferring it to a future SK iteration. The report does provide a fallback ("the existing checkasm parity harness covers correctness for the same-class primitives"), but that argument relies on REDRESS 88 + 89 having used the harness *post hoc* during their rejection, not as a pre-admission gate. |
| D-5 | "REDRESS 28/33's consumer was the parser's tiny-string dispatch — the 16-byte kernel ran *inside* the parser hot loop. SK-V9's consumer is the union substrate's structural-bitmap producer, running in a *different* code path" (§5.5). | §5.5; REDRESS 28, 33 | **REVISE** | The "different code path" argument is structurally plausible, but P2-D's §5.5 hinges on P2-A's union substrate landing in the *same wave* as the §5 admission. P2-A scopes the union substrate as a forward proposal whose proof prerequisite (P2-B) is itself proof-only. Chain: P2-D §5 admission ← P2-A union substrate ← P2-B proof. Each link is gated on the next. CH6 REVISE because the report names "P2-A scope" as same-wave without owning P2-A's risk that P2-A itself does not land. |
| D-6 | "The Wave 1 admission kernel `unescape_uxxxx_x4_neon` is currently in-tree but has neither a checkasm test... nor an alignment sweep" (§6.2). | §6.2 | **REVISE** | First half (no `checkasm_unescape_uxxxx.rs`) verified — no such test file in `bbnf-simd/tests/`. Second half ("REDRESS 82 wave attempted one but was rejected with the patch — the test went out with it") is an inference about a rejected patch, not a current-tree observation. The empty-tree claim is correct; the historical reconstruction is unverifiable from the working tree and reads as background context, not a CH6 falsifier. |
| D-7 | "FEAT_PMULL=1, FEAT_DotProd=1, FEAT_CSSC=1, FEAT_SME=1/FEAT_SME2=1/FEAT_SME2p1=1, AES + SHA3 + SHA512 default-cfg" host inventory (§1.1). | `restart/skinny/tranches/sk-v6/research/skv6-A6-host-asm-instruction-map.md` §1.1 | **ACCEPT** (cite resolves at section-level) | Citation routes to a prior SK iteration's host inventory; this is acceptable provenance for a host-feature claim. CH6 admits because the inventory is not load-bearing for the §3-§5 designs (each kernel is gated by `cfg(target_arch="aarch64")` rather than a feature probe). |

P2-D self-reports under CH6: 1 ACCEPT, 4 REVISE, 1 REJECT. The
**D-1 REJECT** is the load-bearing failure of the report — the
"unwired" framing of `unescape_uxxxx_x4_neon` is contradicted by
in-tree state (and by sibling P2-E's correctly-cited consumer).
Multiple §6 admissibility-gap-then-deferral patterns. P2-D requires
the V2 fold to restate the §2.1 consumer status truthfully and to
own the checkasm-gap question rather than deferring it.

### §2.5 — P2-E unicode-escape codec

| # | Claim | Citation | Audit verdict | Notes |
|---:|---|---|---|---|
| E-1 | `read_hex_unit_scalar` at `lib.rs:945-956`; `hex_nibble` at `:958-966` (§1.1). | `parse-that-regex/src/lib.rs:945, :958` | **ACCEPT** | Line numbers match in-tree. |
| E-2 | `unescape_four_unicode_escapes` at `lib.rs:384-459` already calls `unescape_uxxxx_x4_neon` (§1.2). | `parse-that-regex/src/lib.rs:402` | **ACCEPT** | The line-402 call site is verified by direct file-read. P2-E correctly identifies the kernel as *wired but trigger-narrow* (consumer trigger requires exactly four back-to-back `\uXXXX` quartets with no other bytes between). This directly contradicts P2-D §2.1's "neither is wired" claim. |
| E-3 | Per-row projection table at §6.2 — unicode_escapes 100.5% of threshold (PASS), y_string_unicode 94.5% (NEAR-FAIL), unicode_mixed 68.7% (FAIL), gsoc-2018 98% (NEAR-FAIL). | §6.2 ns/B arithmetic | **REJECT** | The §6.1 baseline c/B table reads: `unicode_escapes 0.354`, `unicode_mixed 0.628`, `y_string_unicode 0.787`, `gsoc-2018 0.193`. PMU TSV says (Track 1 `cycles_per_byte`): unicode_escapes 3.007, unicode_mixed 4.634, y_string_unicode 5.710, gsoc-2018 1.544. Roughly an order of magnitude apart. The §6.1 numbers do not match the PMU TSV in either column (`cycles_per_byte` or `ns_per_byte` — for y_string_unicode/t1 ns/B = 1.466, c/B = 5.710, neither matches 0.787). The §6.2 projection table inherits this miscarriage. The verdict columns (PASS / NEAR-FAIL / FAIL / NEAR-FAIL) therefore cannot be derived from S-P1 evidence as cited. **This is a CH6 falsification: the report's load-bearing admission verdict rests on fabricated or mis-sourced PMU data.** |
| E-4 | y_string_unicode threshold reduced to 70% of sonic-strict (§6.3) with rationale "structurally hard, 99% short-string corpus." | §6.3 | **REVISE** | The 70% reduction is asserted with "per the W4 precedent + the row's structural hardness" — but the W4 precedent (REDRESS 82) is a rejection, not a slack-floor authority. Loosening the falsifiability bar to admit a near-miss is the textbook CH6 paper-close pattern. The "70% recognises the row is structurally hard" rationale, combined with the §6.2 projection at 94.5% of that already-loosened threshold, means the codec admits the row at ~66% of sonic — which is the regime REDRESS 82 was rejected in. CH6 REVISE: the slack must be set *before* the projection and bound to a primary-source justification, not retrofitted to admit a near-miss. |
| E-5 | Honest verdict (§6.4): "Closes unicode_escapes (PASS). Approaches but does not reliably cross y_string_unicode. Does not close unicode_mixed (68.7%). Does not affect gsoc-2018 measurably." | §6.4 | **ACCEPT** (posture) | The §6.4 paragraph is the *one place* the report carries its own unworkability — it admits two of four rows do not close on the codec alone. This is the correct CH6 posture: honest unworkability beats fabricated success. The verdict survives despite E-3's underlying PMU defect, because E-4 / E-5 acknowledge the unicode_mixed + gsoc-2018 gaps rather than overclaim. |
| E-6 | Same-wave consumer plan: JSON `unescape_string` at `lib.rs:718-810` replaces `Some(b'u')` arm (§4.1); CSS L4 codegen template + unit test (§4.2). | `parse-that-regex/src/lib.rs:718-810` | **ACCEPT** (path resolves) | File line range is correct; the proposed call-site swap is concrete and inspectable. The CSS L4 sketch is *codegen-emitted binding + unit test in the same wave; the CSS L4 SIMD body lands when CSS-side benches demand it* — this is honest scope (binding + test now, SIMD body later) and binds the Lock 14 generality demonstration without overclaiming a CSS performance gate. |
| E-7 | LOC envelope: +775 net new (excl. tests), +1025 incl. tests + checkasm (§7.1). | §7.1 | **REVISE** | Row sums add: 80+120+150+140+180+50+250+30+10+(-215)+40+120 = 955 (not 775 or 1025). Two of the figures (`+250` checkasm tests, `−215` kernel removal) appear in both subtotals; the arithmetic in the "Net new (excl. tests)" row leaves the +250 in. The discrepancy is small and likely reflects an arithmetic slip rather than a load-bearing claim, but a CH6 audit on an LOC budget requires the column to add. REVISE the table arithmetic before S-P3 inherits it. |

P2-E self-reports under CH6: 4 ACCEPT, 2 REVISE, 1 REJECT. **E-3
REJECT is load-bearing**: the §6.2 projection table is the report's
quantitative deliverable, and its c/B inputs do not reconcile to the
S-P1 PMU TSV. The §6.4 §honest §verdict block partially recovers
because it admits the row-close gaps in plain language; but the
underlying arithmetic must be rederived from `/tmp/skv9-xctrace-v3/
pmu_rows.tsv` rather than from unsourced c/B columns.

### §2.6 — P2-F SOTA teardown

| # | Claim | Citation | Audit verdict | Notes |
|---:|---|---|---|---|
| F-1 | "yyjson 30931 > simdjson 24522 > sonic-rs 19453 Mbps on twitter — yyjson is the M5 Max DOM-plane leader." (§1 table row 3) | `skinny/RESULTS.md` twitter row | **ACCEPT** | RESULTS.md twitter/parse_only line: Track 1 13188, sonic-strict 19453, simdjson DOM 24522, yyjson_default 30931 (verified `grep -n twitter\b skinny/RESULTS.md`). Numeric ordering matches; "leader" framing accurate. |
| F-2 | Number-heavy WIN rows: canada 16190 vs sonic-strict 12723 (+27.2%); numbers 17956 vs 12972 (+38.4%); marine_ik 12073 vs 8417 (+43.4%); mesh 12435 vs 11279 (+10.2%); citm_catalog 29215 vs 23590 (+23.8%) (§2.1). | RESULTS.md | **ACCEPT** | Spot-check on canada: report says 16190 Mbps Track 1 / sonic 12723 (+27.2%); the live PMU row gives Track 1 14928 Mbps (PMU run). The numerical mismatch (16190 vs 14928) reflects RESULTS.md being a Criterion same-run capture and the PMU TSV being a separate xctrace run on the probe binary — the two are different measurement surfaces by design (HARDENING-S-P1-CONVERGED §1.1). The report cites RESULTS.md throughout; the +27.2% delta uses RESULTS.md columns and the comparator is the RESULTS.md sonic-strict value. Internally consistent. |
| F-3 | String-heavy LOSS table (§2.2): twitter -32.2%, update_center -37.6%, apache_builds -23.3%, github_events -33.0%, gsoc-2018 -51.0%. | RESULTS.md `Δ vs sonic-strict` column | **ACCEPT** | twitter row carries `-32.2%` in the cited column (verified). Other rows trust the same column. |
| F-4 | "asmjson is a *non-anchored sidecar planning signal*" because "AVX-512 path unrunnable on aarch64" and "SWAR fallback inherits permissive control-byte treatment." (§4) | RESULTS.md asmjson columns `n/a`; SC-2 §1.0 source-anchor caution; upstream README citation | **ACCEPT** | RESULTS.md asmjson columns are `n/a` for M5 Max (verified by grep on `asmjson_swar` and `asmjson_avx512`). The disposition is honest: the report routes asmjson to architecture-lesson sidecar (§5.4) rather than admission anchor. CH6-strong. |
| F-5 | "ContainerNext (V9.5 Wave 2 close) eliminates the per-element re-dispatch that simdjson's stage-2 goto-thread requires across each `{`/`}` boundary." (§2.1, lemma 2) | (no cite) | **REVISE** | Architectural claim about both bbnf's ContainerNext and simdjson's stage-2 goto-thread is asserted without a code citation (file:line or upstream simdjson source link). The competitor inventory in §1 carries source-tree references; §2.1's mechanism story falls back to architectural prose. CH6 REVISE on derivation depth — F-name a `runtime/.../generated.rs` ContainerNext call site to anchor the bbnf side. |
| F-6 | §7 sequencing "I → II → III"; cumulative impact table at §7.4 (string-dense rows close to *>sonic + ≥simdjson NEON*, unicode rows close to *>sonic + >yyjson on twitter-class*). | §7 | **REVISE** | The §7.4 impact table is the report's grand prediction — but it is *unconditional* on the three interventions landing. P2-A is gated on P2-B (proof) which itself does not move RESULTS. P2-D § 5 is gated on P2-A. P2-E §6 has the E-3 PMU defect documented above. The §7.4 table compounds three forward-looking predictions whose individual gates are not yet met; CH6 REVISE — the table must be presented as conditional ("if I + II + III all land") rather than as a flat forecast. |
| F-7 | "yyjson's `\uXXXX` decode is **fused into the same `__force_inline` walk**" (§2.3 lemma 3). | upstream README + `yyjson.c` (§8 source) | **ACCEPT** (provenance) | yyjson's single-pass scalar fusion is a documented architectural choice; the report cites `yyjson.c` read path. The citation is section-level not line-level, but the *architectural pattern* (no SIMD, no second pass) is unambiguous in upstream design. The §5.3 architecture-lesson re-derivation ("yyjson's twitter 30931 Mbps result is the proof: total scalar fusion beats SIMD when the SIMD pays dispatch overhead or stage-boundary memory traffic") is internally consistent with §1 row. |

P2-F self-reports under CH6: 5 ACCEPT, 2 REVISE. No REJECT. The
F-6 cumulative-impact-table REVISE is the load-bearing CH6 finding:
the report presents `I → II → III` as a sequencing rather than a
conditional, papering over the inter-report dependency graph.

## §3 Aggregate verdict

Across six reports × ≥5 probed claims = 38 dispositions:

| Verdict | Count | %  |
|---|---:|---:|
| ACCEPT | 26 | 68% |
| REVISE | 10 | 26% |
| REJECT | 2  | 5% |

CH6 ACCEPT rate: **68%** — well below the §3Z 95% convergence bar
on this lens.

**Per-report acceptance rate:**

| Report | ACCEPT | REVISE | REJECT | Verdict |
|---|---:|---:|---:|---|
| P2-A union event-model | 5 | 1 | 0 | NEAR-CONVERGE (83%) |
| P2-B retained grammar proof | 5 | 1 | 0 | NEAR-CONVERGE (83%) |
| P2-C apache + citm admission | 7 | 0 | 0 | CONVERGE (100%) |
| P2-D aarch64 asm opportunities | 1 | 4 | 1 | BLOCKED (17%) |
| P2-E unicode-escape codec | 4 | 2 | 1 | BLOCKED (57%) |
| P2-F SOTA teardown | 5 | 2 | 0 | NEAR-CONVERGE (71%) |

Two load-bearing REJECTs concentrate in P2-D (D-1: `unescape_uxxxx_x4_neon`
mis-stated as unwired) and P2-E (E-3: §6.2 projection table's c/B
inputs do not reconcile to the PMU TSV). Both require a V2 fold
**before** the cohort advances to S-P3 — they are not lens-level
opinions about prose, they are arithmetic-and-tree facts that the
report disagrees with.

P2-C is the cohort's strongest CH6 surface — every claim resolves to
an in-tree file or PMU row, every gate names a halt-on-fail clause,
the methodology self-discloses the absences (typed-track PMU) it
chooses not to fill.

P2-D's CH6 weakness is structural: its §5 admission designs (the
union-substrate consumer for `match_tiny_plain_string` widening and
the structural-bitmap chain) chain onto P2-A's union substrate
landing, which itself depends on P2-B's proof. The report does not
own that dependency chain; §7 enumerates REDRESS differentials
without owning the predecessor-pass risk.

## §4 Specific paper-close violations requiring V2 fold

The following V2 folds are non-optional:

1. **P2-D §2.1 — restate `unescape_uxxxx_x4_neon` consumer status truthfully.**
   The kernel IS wired into `parse-that-regex/src/lib.rs:402` inside
   `unescape_four_unicode_escapes` (`lib.rs:384-459`). The current
   §2.1 framing reads "Neither is wired into the parse-that-regex
   hot path" in **bold**, which is a load-bearing factual claim that
   does not survive a `grep`. The V2 fold restates: "The kernel is
   wired but the consumer trigger is narrow (exactly-four-quartets
   pattern); on `y_string_unicode` and `unicode_mixed` the trigger
   fires rarely because the corpus mixes single quartets with
   surrogates; the SK-V9 widening generalises the consumer trigger
   *and* widens to single-quartet bindings." This restatement is
   the load-bearing CH6 fold.

2. **P2-E §6.1 + §6.2 — rederive ns/B + c/B projection table from PMU TSV.**
   The §6.1 baseline c/B column (`unicode_escapes 0.354`,
   `unicode_mixed 0.628`, `y_string_unicode 0.787`, `gsoc-2018 0.193`)
   does not appear anywhere in `/tmp/skv9-xctrace-v3/pmu_rows.tsv`.
   PMU TSV row for y_string_unicode/track1: `cycles_per_byte =
   5.709799`, `ns_per_byte = 1.465919`. Neither matches §6.1's
   `0.787`. The §6.2 projection table inherits the defective inputs
   and its verdict column (PASS/NEAR-FAIL/FAIL) is unsourced. V2
   fold: rebuild §6.1 from the PMU TSV row-by-row; rebuild §6.2
   from the corrected baseline; restate §6.3 thresholds *before*
   the projection rather than retrofitting `× 0.70` for
   y_string_unicode to admit a near-miss.

3. **P2-D §6.3 — own the checkasm gap rather than defer to SK-V10+.**
   §6.3 names invariants 2-5 as "Insufficient" and writes "fuller
   invariant 2-5 closure is SK-V10+ work… deferring those does
   **not** block §3/§4 admission." This is a CH6 paper-close
   pattern — naming the gap, then declaring the gap non-blocking.
   The V2 fold either (a) commits to landing per-primitive
   checkasm tests in the same SK-V9 wave that admits §3/§4
   primitives, or (b) explicitly rejects the §3/§4 admission
   pending SK-V10+ checkasm hardening. CH6 admits either choice;
   it does not admit the "defer-but-don't-block" middle ground.

4. **P2-F §7.4 — present I → II → III impact table as conditional.**
   The §7.4 cumulative impact table reads as a flat forecast
   ("After I → II → III: string-dense rows close to >sonic + ≥
   simdjson NEON, unicode rows close to >sonic + >yyjson on
   twitter-class, etc."). The dependency graph is I (P2-A) ←
   P2-B (proof), II (P2-E) ⊥ I, III (P2-D §5) ← I. None of I /
   II / III is yet landed; P2-B is a contract-only proof; P2-D
   has its own CH6 REJECT. The V2 fold restates the §7.4 table
   header as "conditional on I + II + III all landing per their
   respective S-P3 admission gates; failure of any predecessor
   pass routes the dependent forecast row to REVISE."

5. **P2-D §5.5 — own the P2-A dependency.**
   The "different code path" differential against REDRESS 28/33
   hinges on P2-A's union substrate being the consumer for
   `match_tiny_plain_string` widening rather than the parser hot
   loop. P2-A is a parallel V1 proposal whose admission is not
   yet decided. The V2 fold names this dependency explicitly and
   either (a) admits the §5 designs only conditional on P2-A
   landing, or (b) reframes the §5 designs against the *current*
   tree's consumer (the parser hot loop), which re-opens the
   REDRESS 28/33 falsification surface the report claims to
   differentiate against.

6. **P2-E §6.3 — set thresholds before measurement.**
   The §6.3 row for y_string_unicode reads "× 0.70 because the row
   is structurally hard." The 0.70 slack is chosen *after* §6.2
   projects the row at 94.5% of a 70% threshold (~66% of full
   sonic). CH6 disallows retro-fitting a slack to admit a near-miss.
   The V2 fold sets the per-row slack from a primary source (the
   strict-vs-strict comparator gate in `gate.rs` defines
   `DIRECT_PROJECTION_SONIC_SLACK = 1.10` for *every* admitted row;
   the W4 precedent that REDRESS 82 followed used 70% but that
   precedent is itself a rejection). Either bind to the 1.10 slack
   for all rows (and explicitly fail y_string_unicode on the codec
   alone), or cite a primary source that admits the 0.70 slack.

The V2 dispatch carries these six folds verbatim; the orchestrator
holds S-P3 dispatch until V2 CH6 returns above the 95% bar with
zero open REJECTs.

---

End of CH6 V1.
