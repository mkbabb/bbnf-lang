# Implementation Packet SK-V7

Date: 2026-05-16.

Workspace: `/Users/mkbabb/Programming/bbnf-lang/skinny`.

Authority:

- `restart/skinny/tranches/sk-v7/SYNTHESIS.md` (companion synthesis).
- `restart/skinny/tranches/sk-v7/research/` (18 cohort reports).
- `restart/skinny/tranches/sk-v7/HANDOFF.md` (packet handoff).
- `restart/prompts/pass-contracts/PASS-ALPHA.md` (this packet's goalset format spec).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (per-wave triumvirate contract).
- `restart/prompts/ORCHESTRATOR.md` (dispatch + sign-off gates).
- `skinny/RESULTS.md` (current gate authority).

## §0 — Close Condition (the goalset)

SK-V7 closes when ALL of the following hold simultaneously on
`skinny/RESULTS.md` against the **strict-rebuilt sonic-rs** baseline
(post-Wave 0):

### §0.1 — Per-row close conditions

| Corpus | Workload | Current | Target | Wave | Falsifiability |
|---|---|---:|---|---|---|
| sonic-rs comparator | n/a | utf8_lossy | strict | W0 | sonic Mbps drops 3-8% per row vs current |
| instruments | parse | 92.0% sonic | ≥100% | W0 | strict rebuild flips |
| unicode_basic | parse | 91.7% sonic | ≥100% | W0 | strict rebuild flips |
| canada | direct | 83.6% sonic | ≥100% | W2 | B5b mantissa widen + EL fallback elim |
| numbers | direct | 100.4% sonic (PASS) | maintain | W2 | no regression after B5b |
| mesh | real_typed_struct | (none today) | ≥100% sonic | W3 | DirectTypeRef::Vec specialisation + mesh schema |
| marine_ik | real_typed_struct | (none today) | ≥100% sonic | W3 | same |
| unicode_escapes | parse | 80.4% sonic | ≥95% | W4 | B1 per-`\uXXXX` TBL (78% \uXXXX content) |
| y_string_unicode | parse | 46.0% sonic | ≥70% | W4 | B1 per-`\uXXXX` TBL (74% \uXXXX content) |
| twitter | parse | 73.6% sonic | ≥90% | W5 | B2 NEON 16-byte plain-string scan |
| update_center | parse | 59.6% sonic | ≥90% | W5 | B2 |
| unicode_basic | parse | 91.7% sonic | ≥100% | W5 | B2 supplements W0 |
| random | parse | 65.5% sonic | ≥85% | W5 | B2 |
| unicode_mixed | parse | 56.1% sonic | ≥85% | W5 | B2 (NOT B1 — has 0% `\uXXXX`) |
| distinct_values | parse | 60.2% sonic | ≥85% | W5 | B2 (NOT B1 — has 0% `\uXXXX`) |
| citm_catalog | parse | 130.3% sonic (G via Track 2) | maintain | W6 | B6 control compaction; Track 2 closes |
| instruments | direct | 93.5% sonic | ≥100% | W6 | B6 |
| Hard residual: twitter parse | parse | 73.6% sonic | (V8 scope) | W5 partial + V8 fusion refactor | yyjson 1.98x gap requires Lock 15 fusion-quality work |

### §0.2 — Strict comparator gate

Every row must show strict-vs-strict comparison against:
- sonic-rs strict (post-W0 rebuild)
- simdjson C++ DOM + On Demand
- yyjson default
- asmjson SWAR (flaw probe only on M5; recorded but does not count toward PASS)
- RapidJSON default (flaw probe)
- serde_json (strict reference)

### §0.3 — Telemetry schema (RESULTS.md columns)

Per `PASS-ALPHA.md` §4.3 verbatim:

```
| Corpus | Workload | Outcome | Verdict | Strictness | parse_utf8 |
escape_complete | flaw_probe | Output plane | Track 1 Mbps |
Track 2 Mbps | sonic-rs strict | sonic-rs lossy | simdjson DOM |
simdjson OD | yyjson | asmjson SWAR | asmjson AVX-512 | RapidJSON |
serde_json | Δ vs SK-V6 | Δ vs sonic-strict | Δ vs simdjson DOM |
Δ vs yyjson | Hot leaf | Signal |
```

24 columns. The xtask `gate-json` command rejects any row missing
required columns. The bench harness emits this schema verbatim.

### §0.4 — Run protocol

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- gate-json
```

All must pass before closing each wave's exit gate.

## §1 — Non-Negotiables (per `restart/prompts/README.md` non-negotiables)

| Rule | Enforcement |
|---|---|
| No new BBNF directives | grep grammars/ pre-/post-V7 |
| No new BIR variant | grep ir/src/ pre-/post-V7 |
| No new substrate | Lock 1 audit per CH5 per wave |
| No JSON code in generic crates | Lock 14 audit; CI grep-gate on bbnf-simd, parse-that-regex, codegen/lower, runtime/tape, passes |
| Scalar reference per primitive | every NEON/ASM kernel ships with scalar Rust ref + checkasm parity BEFORE wiring |
| Same-wave consumer | each redress commit MUST land hot-path caller; verified via samply symbol path |
| Profile-first prescription | no kernel intervention without fresh PC-level profile on NEW Track 1 baseline (hypothesis transfer from SK-V6 forbidden) |
| Strict-vs-strict comparisons | every comparator row matches strictness plane; permissive rows flaw-probe only |
| Triumvirate discipline | research → plan → redress in distinct commits |
| Hard cap per dispatch | every dispatch carries minute cap; at 0.9× commit, at cap halt |
| Same-row falsification gate | kernel that fails its named row falsifiability is rejected; REDRESS + revert |
| No deferrals | wave closes on measurement, not "future phase will fix it" |

## §2 — Wave 0: Comparator-plane repair (one-line diff + bench rerun)

### Owner paths
- `skinny/crates/bbnf-bench/Cargo.toml` (line 21: remove `"utf8_lossy"`)
- `skinny/crates/bbnf-bench/src/lib.rs` (verify no API depends on lossy mode)
- `skinny/RESULTS.md` (re-baseline; populate schema v3 columns)
- `restart/skinny/tranches/sk-v7/research/wave-0-strict-baseline.md` (the rerun report)

### Tasks
1. Edit `bbnf-bench/Cargo.toml:21`: remove `, "utf8_lossy"` from sonic-rs features.
2. Run `cargo tree -p bbnf-bench --edges=features | grep sonic-rs` to confirm lossy is OFF.
3. Run `cargo bench -p bbnf-bench --bench json_parity` (fresh; ~30 min wall).
4. Run `cargo run -p bbnf-bench --bin gate --release` to emit refreshed RESULTS.md.
5. Manually add the missing schema v3 columns if the bench harness doesn't yet emit them (this is per PASS-ALPHA §4.3; if the bench harness is incomplete, mark as a Wave 0 follow-up).
6. Write a re-baseline report at `restart/skinny/tranches/sk-v7/research/wave-0-strict-baseline.md` documenting per-row Mbps delta.

### Exit gate
- sonic-rs Mbps drops 3-8% on every row vs current RESULTS.md.
- `instruments` parse classifies PASS (≥100% strict sonic).
- `unicode_basic` parse classifies PASS (≥100% strict sonic) OR documents the residual gap.
- No row regresses Track 1 or Track 2 (the bench is comparator-only at W0; bbnf code unchanged).

### Hard cap
- 60 min total (1 min Cargo edit + 30 min bench + 15 min RESULTS rewrite + 14 min report).

## §3 — Wave 1: Class D TapeKind rename (Lock 14 lowest-risk)

### Owner paths
- `skinny/crates/ir/src/lib.rs` (TapeKind enum + DirectBuildDecode)
- `skinny/crates/passes/src/lib.rs:744-750` (the 7 consumer match arms)
- `skinny/crates/codegen/src/json_templates/generated.rs` (re-render if regen affects)
- Any consumer file post-rename (mechanical)

### Tasks (per B3 Class D sequencing)
1. Rename `TapeKind::{Object,Array,Pair,String,Number,Bool,Null,Member,Element}` to grammar-neutral: `TapeKind::{Container,Bucket,KeyValuePair,StringValue,NumberValue,BoolValue,NullValue,Member,Element}` (or similar — the rename is the load-bearing thing; the new names need to be grammar-neutral).
2. Rename `DirectBuildDecode::{JsonString,JsonNumber}` to `DirectBuildDecode::{StringValue,NumberValue}`.
3. Delete `passes::materialization_for_rule` (Class A; absorbs the 7 consumers).
4. Re-run codegen + verify byte-identical generated.rs (excluding rename diff).
5. `cargo test --workspace` green.

### Exit gate
- Renames applied; no JSON-prefixed TapeKind variants remain.
- `cargo test --workspace` green.
- RESULTS.md unchanged (no behavior delta).
- Lock 14 HIGH leak count: -3.

### Hard cap
- 90 min total.

## §4 — Wave 2: B5b Eisel-Lemire mantissa widen + canada fallback elimination

### Owner paths
- `parse-that-regex/src/number/eisel_lemire/algorithm.rs` (or wherever EL is vendored)
- `parse-that-regex/src/number/integer.rs` (verify integer fast path coverage)
- `bbnf-bench/src/direct_struct.rs` (verify the `text.parse::<f64>()` fallback path; eliminate or document)

### Tasks (per C2 + A4 findings)
1. Profile canada with --features parse-attribution (per V5 W0 admit) to confirm ~25% of f64 hits fallback.
2. Investigate widening the EL mantissa range (per the upstream parse-that crate's EL code; the algorithm tolerates wider mantissas with additional powers-of-10 table entries).
3. Land the wider mantissa table entries; verify bit-parity tests stay green.
4. Bench canada direct + numbers direct + mesh direct + marine_ik direct.

### Falsifiability gate
- canada direct ≥100% sonic-strict (currently 83.6% lossy → predicted 89% strict; B5b must add ≥11% from mantissa widen).
- numbers direct stays PASS (≥100%).
- No row regresses.

### Exit gate (commit on success)
- canada direct PASS.
- REDRESS entry numbering the admit + measurement table.
- `cargo run -p xtask --release -- primitive-checkasm` for EL parity tests green.

### Revert protocol (on failure)
- Roll back the mantissa widen patch.
- REDRESS entry naming the failure mode + saved patch at `/tmp/skv7-wave-2-rejected.patch`.

### Hard cap
- 105 min total (45 min research + 60 min implement + bench).

## §5 — Wave 3: B5 mesh DirectBuild + DirectTypeRef::Vec specialisation

### Owner paths
- `codegen/src/json_typed_direct.rs:306-315` (the shape-blind Vec helper)
- `codegen/src/lower/sink_only.rs` (verify Vec emission path)
- `codegen/src/direct_schema.rs:64` (DirectTypeRef::Vec enum variant — add capacity_hint field)
- `xtask/src/real_typed_schema.rs` (add Mesh struct definition)
- `bbnf-bench/src/generated_real_typed.rs` (add mesh consumer + bench wiring)

### Tasks (per B5 design + C3 critical correction)
1. Add `capacity_hint: Option<usize>` field to `DirectTypeRef::Vec` (mirrors `MapEntriesVec` arm at lines 326-342).
2. Update `type_key()` at `:362` to include `capacity_hint` to prevent helper-function collisions.
3. Specialise the Vec helper to emit `Vec::with_capacity(hint.unwrap_or(0))` + SWAR-across-commas digit scan when inner type is DirectScalar::F64/U32.
4. Add mesh `Mesh { vertices: Vec<f32>, normals: Vec<f32>, indices: Vec<u32>, ... }` schema fixture.
5. Wire mesh real_typed_struct workload through bbnf-bench Track 1 (generated) + Track 2 (independent typed oracle).
6. Bench mesh + marine_ik real_typed_struct.

### Falsifiability gate
- mesh real_typed_struct ≥100% sonic-strict.
- marine_ik real_typed_struct ≥100% sonic-strict.
- mesh direct_to_struct stays ≥91.8% (no regression on existing workload).
- twitter real_typed_struct stays ≥151.5% (no regression).

### Exit gate
- Both mesh + marine_ik real_typed_struct PASS.
- REDRESS entry numbering the admit.

### Revert protocol
- Roll back the DirectTypeRef::Vec specialisation.
- REDRESS entry; note this is the third attempt at the mesh DirectBuild
  shape (V5+V6 rejected; V7 with codegen specialisation).

### Hard cap
- 165 min total (60 min research + 60 min implement + 30 min bench + 15 min REDRESS).

## §6 — Wave 4: B1 per-`\uXXXX` TBL classifier

### Owner paths
- `parse-that-regex/src/lib.rs:911-922` (the unescape_json_string call site)
- `parse-that-regex/src/unicode/escape_decode.rs` (NEW; per B1 design)
- `bbnf-simd/src/aarch64/unescape_uxxxx.rs` (reuse existing kernel; no new intrinsic body)
- `bbnf-simd/tests/checkasm_unicode_escape.rs` (NEW; differential parity)

### Tasks (per B1 design + C1 correction)
1. **NOTE C1 correction**: this wave applies to ONLY 2 of the 4 originally-named rows (unicode_escapes 78% \uXXXX + y_string_unicode 74% \uXXXX). The other 2 (unicode_mixed, distinct_values) have 0% \uXXXX and are addressed by W5 B2 instead.
2. Author `parse-that-regex/src/unicode/escape_decode.rs` per-quartet TBL classifier reusing `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon` (already TBL-based + checkasm-tested).
3. Wire into `unescape_json_string` at lib.rs:911-922 (~30 LOC change).
4. checkasm test for every BMP codepoint + surrogate pairs + invalid hex (~150 LOC).
5. Bench unicode_escapes + y_string_unicode parse + direct.

### Falsifiability gate
- At least 2 of 4 named must-lift rows cross thresholds on both parse-G AND direct:
  - unicode_escapes parse 80.4% → ≥95%
  - y_string_unicode parse 46.0% → ≥70%
- AND no row regresses ≥3%.

### Exit gate
- 2 named rows cross threshold.
- REDRESS entry numbering the admit + B1 falsifiability eval.
- checkasm parity green; `primitive-checkasm` passes.

### Revert protocol
- Roll back per B1 design § rejected-route protocol.
- REDRESS entry distinct from REDRESS 64 (per the per-quartet vs 4-batch distinction).

### Hard cap
- 125 min total.

## §7 — Wave 5: B2 NEON 16-byte plain-string scan widening

### Owner paths
- `runtime/src/grammars/json/generated.rs:173` (the match_tiny_plain_string_with_cap::<16> hot leaf)
- `bbnf-simd/src/aarch64/string_block.rs` (existing scan_string_special_block)
- `parse-that-regex/src/lib.rs:295-347` (the dispatcher with the 0x80 early-exit at line 331 — V5 W3 family REFUTED; B2 is structurally different: it targets the tiny-plain layer specifically, not the validation fold)

### Tasks (per C1 + C4 findings)
1. The current `match_tiny_plain_string_with_cap::<16>` is scalar 8-byte loop; replace with NEON 16-byte block compare against alphabet `(b'"', b'\\', 0x20)`.
2. Verify the NEON kernel doesn't re-enter the REFUTED V5 W3 family by:
   - NOT touching `parse-that-regex/src/lib.rs:331` 0x80 early-exit (separate code path).
   - NOT widening to 4-quartet contiguous validation (REDRESS 64).
   - Targeting the tiny-string per-quote-pair layer specifically (8-byte → 16-byte stride only).
3. Scalar reference + checkasm parity.
4. Wire same-wave through generated.rs:173.
5. Bench 6 string-bound rows.

### Falsifiability gate
- At least 4 of 6 named rows cross threshold:
  - twitter parse 73.6% → ≥90%
  - update_center parse 59.6% → ≥90%
  - unicode_basic parse 91.7% → ≥100%
  - random parse 65.5% → ≥85%
  - unicode_mixed parse 56.1% → ≥85% (this is where the W4 B1 deficit shows up — unicode_mixed lifts via plain-body, not via escape decode)
  - distinct_values parse 60.2% → ≥85% (same; plain-body)
- AND no row regresses ≥3%.

### Exit gate
- 4+ named rows cross threshold.
- REDRESS entry + measurement table.
- checkasm parity green.

### Revert protocol
- Roll back; REDRESS entry; document the structural distinction from V5 W3 family.

### Hard cap
- 165 min (60 research + 75 implement + 30 bench).

## §8 — Wave 6: B6 control / key compaction

### Owner paths
- `runtime/src/grammars/json/generated.rs` (the citm + instruments hot leaves; per C1: container/key bookkeeping is the dominant cost on these rows)
- `bbnf-simd/src/aarch64/` (potential new primitive: key-byte run scan)

### Tasks
1. Profile citm + instruments specifically; identify the container/key bookkeeping leaf at PC level.
2. Optimize the per-key dispatch path (current likely has redundant scans or per-key alloca).
3. Bench citm + instruments.

### Falsifiability gate
- citm parse Track 2 closes (current Track 2 83.5%; needs ≥90%).
- instruments parse ≥100% sonic-strict.

### Exit gate / revert
- Per gate.

### Hard cap
- 165 min.

## §9 — Wave 7: Lock 14 cleanup Phase A + B (parse-that-regex + passes)

### Owner paths
- `parse-that-regex/src/lib.rs` (rename JsonStringMatch → StringMatch alias + 9 HIGH leaks)
- `passes/src/lib.rs:28-29` (delete shapes_for_json + nominate_json + materialization_for_rule literal-name-match)

### Tasks (per B3 Phase A + B sequencing)
1. Phase A: parse-that-regex 9 HIGH renames. Collapse JsonStringMatch/JsonNumberMatch into StringMatch/NumberSpan with SpecialByteSet parameter (~250 LOC).
2. Phase B: passes/src/lib.rs grammar-neutral derive_recognizers refactor (~300 LOC).
3. `cargo test --workspace` green at each phase.

### Exit gate
- No JSON-prefixed types in parse-that-regex public API.
- passes::compile() consumes Grammar parameter without literal-name-match.
- Lock 14 HIGH count: -20 (≥-44% reduction).

### Hard cap
- 240 min (split across 2 sub-waves if needed).

## §10 — Wave 8: Lock 14 Phase C + D (codegen + ir)

### Owner paths
- `codegen/src/lib.rs` + `json_sink_direct.rs` + `json_typed_direct.rs` + `lower/schema_direct.rs` (~18 HIGH; Phase C, riskiest)
- `ir/src/lib.rs` (Class E ir leak residue post-Phase D Class D)

### Tasks (per B3 Phase C + D, riskiest phase)
1. Phase C: codegen rebrand (~470 LOC across 4 files). Replace emit_json_* with emit_grammar_*; collapse hardcoded JSON shape rosters into grammar-derived facts.
2. Sub-split into 3a/3b/3c if any sub-commit >200 LOC.
3. byte-identical generated.rs is the gating invariant; `xtask gen --check` must succeed.

### Exit gate
- Lock 14 HIGH count: -38 (≥-83% reduction).
- byte-identical generated.rs verified.

### Hard cap
- 360 min.

## §11 — Wave 9: CostFacts substrate (per B2 design)

### Owner paths
- `ir/src/cost.rs` (NEW)
- `passes/src/lib.rs` (extend with cost_facts: HashMap<RuleId, CostFacts>)
- `codegen/src/lower/mod.rs` (ShapeLowering trait consuming CostFacts)
- `xtask/src/main.rs` (gate-json --with-cost-facts flag)

### Tasks (per B2 design)
1. Define CostFacts + ShapeRationale + RejectedAlternative + Measurement + PriorityStep types.
2. Refactor `derive_backend_shape_with_diagnostics` to emit CostFacts + populate the priority table as `&'static [PriorityStep]` (per B2 design risk mitigation).
3. Add LayoutFacts.cost_facts field.
4. Add xtask gate-json --with-cost-facts surface.
5. Backfill REDRESS 72 evidence.

### Exit gate
- 7 JSON rules have populated CostFacts.
- xtask gate-json --with-cost-facts outputs CostFacts per rule.
- Two new diagnostics: BBNF-DOMINATED-ALTERNATIVE + BBNF-COSTFACTS-MISSING-EVIDENCE.

### Hard cap
- 360 min.

## §12 — Wave 10: bbnf.asm body fills (PMULL + CSSC CTZ)

### Owner paths
- `bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs` (PMULL body per A3)
- `bbnf-simd/src/aarch64/bitmap_next_set_bit.rs` (CSSC CTZ body)
- `bbnf-simd/tests/` (checkasm tests per B6 hardening)
- Runtime consumer wiring (same-wave)

### Tasks (per A3 + B6 designs)
1. PMULL body for BITMAP_PREFIX_XOR_64: `vmull_p64(mask, u64::MAX)` collapses 6-stage shift-XOR to 1 µop.
2. CSSC CTZ body for BITMAP_NEXT_SET_BIT: emit CTZ under `-C target-cpu=native`.
3. Scalar references for both.
4. checkasm parity tests + same-wave OffsetTape consumer in scan path.
5. B6 hardening Stage 1: stack canary XOR-fold compare (10 LOC; smallest delta with widest impact per B6 design).

### Exit gate
- Both primitives admitted; checkasm green; same-wave consumer wired.
- B6 Stage 1 hardening landed.
- No row regresses.

### Hard cap
- 240 min.

## §13 — Pass Alpha dispatch (post-W10 convergence)

After W10 commits and bench is rerun, dispatch Pass Alpha for the
SK-V7 → SK-V8 cycle:

```
dispatch alpha SK-V7→SK-V8
```

Pass Alpha consumes the SK-V7 cycle's full state and produces:
- GRAND-SYNTHESIS-SK-V8.md
- IMPLEMENTATION-PACKET-SK-V8.md
- HANDOFF-SK-V8.md

The likely SK-V8 framing: address the hard residual (twitter parse +
yyjson 1.98x gap) via Lock 15 fusion-quality refactor; address any
remaining Lock 14 residue (codegen template residue); address bbnf.asm
remaining primitive body fills.

## §14 — Pass Omega trigger checkpoint

After SK-V7 close, Pass Omega is triggered per `PASS-OMEGA.md` §1:
- T-P3 hardening returned READY for the current totality cycle: N (no
  totality cycle has run V7-paired; this needs to happen).
- A major skinny iteration has closed: YES (SK-V7 close).
- Corpus drift ≥10%: TO BE ASSESSED.

The user may dispatch `dispatch omega` after SK-V7 closes to fold
skinny lessons into V1 spec amendments + locks proposals (per V7
GRAND-SYNTHESIS §9).

## §15 — Final SK-V7 handoff

Final report lands at:
```
restart/skinny/tranches/sk-v7/HANDOFF.md  (THIS PACKET's companion; lands now)
```

Required sections per SK-V6 HANDOFF convention plus:
- Per-wave commit chain + per-wave Mbps delta on each row.
- The strict-vs-strict comparator table on the post-W0 baseline.
- The 5 corrected diagnoses from SK-V7 cohort (C1 unicode_mixed 0%
  \uXXXX; C3 twitter skip-work; C3 mesh DirectBuild blocked by
  codegen; C2 Eisel-Lemire 5.2% on mesh; A6 CostFacts absent).
- The Pass Omega trigger candidates per §14.

No wave closes on "future phase will fix it". Every miss becomes a
named blocker, a rejected route with REDRESS evidence, or the next
concrete wave input.
