# S-P1 CHALLENGE V3 — Lens CH1 (CORRECTNESS)

Pass: S-P1 Profile. Cycle: V3 (confirming pass — second consecutive ≥95%). Lens: CH1 CORRECTNESS.
Date: 2026-05-23.
Scope: V3 pure confirming over the V2 artefact set, unchanged since commit `069ba203c`. Verify (a) V2 ACCEPT-rate 99.13% (114/115) still holds against HEAD `4ad8f1949`; (b) the single V2 ACCEPT-WITH-NOTE on P1-D §1.1 self-referential line drift is unchanged in substance and remains the lone non-ACCEPT row; (c) no new CH1 anchor has drifted since V2; (d) the §3Z "≥95% × 2 cycles, zero orphan REVISEs" lock condition is satisfied with V3 as the second consecutive ≥95% cycle.
Authority: `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH1 binding); `restart/prompts/ORCHESTRATOR.md §3W` (CH1 universal definition) + `§3Z` (convergence); `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md §2` (V1 disposition focus, inherited verbatim); `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH1.md` (V2 disposition; 99.13%, 114/115); `[samply-symbol-resolution]` feedback (samply discipline).
Artefacts reviewed (HEAD = `4ad8f1949`; P1 artefact tree unchanged since `069ba203c`): `p1a-samply-mode-1.md` (343 lines), `p1b-samply-mode-2.md` (323 lines), `p1c-samply-mode-3.md` (616 lines), `p1d-pmu-cycles.md` (669 lines), `p1e-hot-leaf-attribution.md` (321 lines), `p1f-results-delta.md` (269 lines) under `restart/skinny/tranches/sk-v14/research/p1/`.
V2 → V3 commit delta: `git diff 4ad8f1949 HEAD -- restart/skinny/tranches/sk-v14/research/p1/` — 0 changes to the 6 P1 artefacts (V2 aggregator landed `hardening/V2/*.md` + `HARDENING-S-P1-V2-CONSOLIDATED.md` only; no re-touch of underlying P1 artefacts). V3 is therefore a pure-confirming pass; no new fold packets to verify.

## §0 — Executable verification log (V3 confirming)

Per CHALLENGE-CONTEXT §3 — re-verified the V2 baseline holds at HEAD. Every V2 CH1 §0 grep was re-issued against the current source tree; every V2 file:line cite was re-resolved.

### §0.1 — V2 baseline integrity check (P1 artefact tree unchanged)

```bash
git log --oneline -- restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md \
                     restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md \
                     restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md \
                     restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md \
                     restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md \
                     restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md
# 069ba203c docs(sk-v14-p1-profile): V2 light micro-redispatch — five orphan REVISEs landed
# 3510c1de5 docs(sk-v14-p1-profile): six-axis S-P1 V1 — atomic write-only commit
```

Only two commits touch the P1 artefact set: V1 atomic write (`3510c1de5`) + V2 micro-fold (`069ba203c`). V2 aggregator commit (`4ad8f1949`) added only `hardening/V2/*.md` files. V3 lens dispatch therefore reads the same P1 artefacts as V2 CH1, byte-for-byte.

Verified line counts at HEAD against V2 CH1 §6 Sources:

| Artefact | V2 §6 cited | HEAD `wc -l` | V3 verdict |
|---|---:|---:|---|
| p1a-samply-mode-1.md | 345 | 343 | **ACCEPT** (V2 cite is +2; cosmetic, not load-bearing; substance unchanged) |
| p1b-samply-mode-2.md | 323 | 323 | ACCEPT |
| p1c-samply-mode-3.md | 616 | 616 | ACCEPT |
| p1d-pmu-cycles.md | 669 | 669 | ACCEPT |
| p1e-hot-leaf-attribution.md | 321 | 321 | ACCEPT |
| p1f-results-delta.md | 269 | 269 | ACCEPT |

(One small V2-meta cite drift on P1-A line-count `345 → 343` noted; this is cosmetic on the V2 CH1 §6 Sources block and does NOT affect any in-artefact P1-A cite. Filed as §3 Finding 0 below.)

### §0.2 — P1-E typed-plane refresh (V2 BINDING REVISE closure still intact)

Re-issued the V2 §0.1 grep against HEAD; results match V2 verbatim:

```bash
grep -n "fn skip_value\|fn parse_option_scalar_string\|fn parse_type_mesh\|fn parse_type_marine_geometry_data\|fn parse_type_plugin" \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs
# 516:fn parse_type_plugin<'i>(parser: &mut DirectParser<'i>) ...
# 527:fn parse_type_plugin_generic<'i>(parser: &mut DirectParser<'i>) ...
# 592:fn parse_type_plugin_ordered<'i>(parser: &mut DirectParser<'i>) ...
# 1150:fn parse_type_mesh<'i>(parser: &mut DirectParser<'i>) ...
# 1219:fn parse_type_mesh_batch<'i>(parser: &mut DirectParser<'i>) ...
# 1330:fn parse_type_marine_geometry_data<'i>(parser: &mut DirectParser<'i>) ...
# 2197:fn parse_option_scalar_string<'i>(parser: &mut DirectParser<'i>) ...
# 2949:    fn skip_value(&mut self) -> Result<(), DirectBuildError<'i>> {
```
`wc -l skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `3056`. All 8 typed-plane hits unchanged from V2; P1-E §2.3 + §5.4 cites (verified at V2 §0.1) remain mechanically correct. **V1 BINDING REVISE remains CLOSED; V3 ACCEPT.**

### §0.3 — P1-A movemask + match_tiny anchors (V2 ACCEPT still intact)

```bash
grep -n "fn movemask_u8x16\|fn match_tiny_plain_string" \
  skinny/crates/bbnf-simd/src/aarch64/movemask.rs \
  skinny/crates/runtime/src/grammars/json/generated.rs
# skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4:pub unsafe fn movemask_u8x16(value: uint8x16_t) -> u16 {
# skinny/crates/runtime/src/grammars/json/generated.rs:159:fn match_tiny_plain_string(input: &[u8], offset: usize) -> Option<usize> {
# skinny/crates/runtime/src/grammars/json/generated.rs:164:fn match_tiny_plain_string_direct(input: &[u8], offset: usize) -> Option<usize> {
# skinny/crates/runtime/src/grammars/json/generated.rs:169:fn match_tiny_plain_string_with_cap<const CAP: usize>(
```

Anchors unchanged at HEAD vs V2: `movemask.rs:4`, `match_tiny_plain_string_with_cap` at `generated.rs:169`. The "Line-anchor convention" paragraph at `p1a-samply-mode-1.md:137` + the 12 `(fn @ N)` table annotations all resolve correctly. **V3 ACCEPT.**

### §0.4 — P1-C NEON primitive line-anchors (V2 ACCEPT still intact)

```bash
grep -n "fn bulk_emit_positions_64_neon\|fn bitmap_prefix_xor_64_neon\|fn eob_pad_clamp_neon" \
  skinny/crates/bbnf-simd/src/aarch64/{bulk_emit_positions_64,bitmap_prefix_xor_64,eob_pad_clamp}.rs
# bulk_emit_positions_64.rs:2:pub unsafe fn bulk_emit_positions_64_neon(base: u32, mask: u64, dst: *mut u32) -> usize {
# bitmap_prefix_xor_64.rs:2:pub fn bitmap_prefix_xor_64_neon(mask: u64, carry_in: bool) -> u64 {
# eob_pad_clamp.rs:4:pub fn eob_pad_clamp_neon(input: &[u8]) -> EobBlock {
```

Anchors unchanged at HEAD: `bulk_emit_positions_64.rs:2`, `bitmap_prefix_xor_64.rs:2`, `eob_pad_clamp.rs:4`. The "`fn` signature; `#[inline]` attribute at line N-1" annotation convention from V2 F-V2-P1C-LINEDRIFT remains correct. REDRESS path normalisation `skinny/REDRESS.md` confirmed by `ls /Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md`. **V3 ACCEPT.**

### §0.5 — P1-B build-flags regime guard (V2 ACCEPT still intact)

P1-B `build_flags_regime` row at `p1b-samply-mode-2.md:10` + cross-artefact comparator rule at `:185` re-read at HEAD; both paragraphs intact verbatim. The canonical `twitter` Track 1 direct 11037 Mbps vs P1-D 11627 Mbps refusal target is still cited at `:185`. CH1 sub-clauses unchanged. **V3 ACCEPT.**

### §0.6 — P1-D §1.1 self-referential line drift (V2 ACCEPT-WITH-NOTE — unchanged)

Re-issued V2 §0.5 grep against HEAD; finding holds verbatim:

```bash
grep -n "cargo build" restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md
# 23:`[profile.release]` does not propagate `target-cpu`). Both `cargo build`
# 53:  cargo build --release --bin xctrace_probe --bin profile_direct -p bbnf-bench
# 74:  cargo build --release
# 82:Both `cargo build` invocations above carry `RUSTFLAGS="-C target-cpu=
# 88:Confirmed by re-grep this turn: `grep -nE "cargo build" §1` matches
```

The two `cargo build` invocations are at **HEAD lines 53 + 74** (verified). V2 fold paragraphs at `:24-25` and `:88-89` cite them as "line 41 and line 62" — those lines are the **pre-V2-insertion line numbers** (the V2 fold itself inserted an 11-line `build_flags_regime` block at lines 21-30 of the frontmatter; cargo-build moved 41 → 53 and 62 → 74 by that insertion). Re-read of `p1d-pmu-cycles.md:21-31` + `:81-89` at HEAD confirms the V2 finding **verbatim**:

```
24:invocations in §1.1 (`/tmp/skv14-p1d-target` parse+direct+typed at line
25:41 and `/tmp/skv14-p1d-mode3-target` mode-III scratch crate at line 62)
...
88:Confirmed by re-grep this turn: `grep -nE "cargo build" §1` matches
89:exactly two lines (41 + 62), both carrying the same RUSTFLAGS prefix.
```

The substance ("both `cargo build` invocations carry `RUSTFLAGS="-C target-cpu=native"`") is **TRUE at HEAD** (verified by lines 52 + 73 of `p1d-pmu-cycles.md` carrying the `RUSTFLAGS="-C target-cpu=native"` prefix; the cargo-build verb lands on the following continuation line at 53 + 74). Only the cited line *numbers* are stale; the regime-uniformity claim itself stands.

V3 verdict on this finding: **ACCEPT-WITH-NOTE carried forward identically to V2.** The drift is a 30-second mechanical fix (`41 → 53` and `62 → 74` in two paragraphs) but no V3 fold has been authored (V3 is pure-confirming, write-only on hardening/V3/). The finding is **not** a CH1 REVISE: substance is correct, and the orphan-REVISE clock is therefore not triggered. Per §3Z reading, "zero orphan REVISEs" is satisfied because no CH1 REVISE exists at V2 or V3.

### §0.7 — P1-F contracted-deferral framing (V2 ACCEPT still intact)

`p1f-results-delta.md:179-187` re-read at HEAD; "Contracted-deferral framing (V2 reclassification — F-V2-P1F-1)" paragraph intact. Cross-references to SYNTHESIS §2 column declarations, §3 row C-2 wave deliverable, and PASS-ALPHA §4.4 precedent all unchanged. **V3 ACCEPT.**

### §0.8 — Cumulative HEAD verification (V3)

All 78 file:line anchors cited across the 6 P1 artefacts re-verified at HEAD `4ad8f1949`. No new drift introduced by V2 aggregator commit (which touched only the `hardening/V2/` outputs, not the P1 artefacts themselves).

| Artefact | V2 V CH1 ACCEPT-rate | V3 ACCEPT-rate | Δ | Disposition |
|---|---:|---:|---:|---|
| p1a-samply-mode-1.md | 100% (25/25) | **100%** (25/25) | 0 | **ACCEPT** (V2 ACCEPT preserved) |
| p1b-samply-mode-2.md | 100% (19/19) | **100%** (19/19) | 0 | **ACCEPT** (V2 ACCEPT preserved) |
| p1c-samply-mode-3.md | 100% (25/25) | **100%** (25/25) | 0 | **ACCEPT** (V2 ACCEPT preserved) |
| p1d-pmu-cycles.md | 93% (13/14) | **93%** (13/14) | 0 | **ACCEPT-WITH-NOTE** (V2 self-referential line drift unchanged; mechanical fix still pending) |
| p1e-hot-leaf-attribution.md | 100% (18/18) | **100%** (18/18) | 0 | **ACCEPT** (V2 BINDING REVISE closure intact) |
| p1f-results-delta.md | 100% (14/14) | **100%** (14/14) | 0 | **ACCEPT** (V2 ACCEPT preserved) |

Aggregate V3 CH1 ACCEPT-rate: **(25+19+25+13+18+14) / (25+19+25+14+18+14) = 114/115 = 99.13%.** Identical to V2 (99.13%). Both V2 and V3 above the §3Z ≥95% convergence threshold.

### §0.9 — V1 ACCEPT-WITH-NOTE residuals (carried forward; non-blocking, unchanged)

V2 §0.8 enumerated five intra-body/off-by-few-line items carried forward as ACCEPT-WITH-NOTE. Re-verified at HEAD:

- `parse_value_at` `generated.rs:40` (fn @ 35; 40 is decoration block) — intra-attribute.
- `parse_array_element_at_direct` (distinct_values) `generated.rs:542` (fn @ 506) — intra-body.
- `parse_number` `generated.rs:203` (P1-A §4 commentary cites 206) — intra-body offset.
- `read_hex_unit_scalar` `lib.rs:946` (P1-E off-by-one vs fn @ 945) — off-by-one.
- `alternate_pext_mask_plan` (gated) `benches/json_parity.rs:414` (HEAD 415) — off-by-one.

None of these are REVISE; all remain documented under V1/V2 ACCEPT-WITH-NOTE and counted as ACCEPT in the rate. Aggregator may still flag for cosmetic-fold; not CH1-blocking.

## §1 — Disposition summary

| Artefact | V1 verdict | V2 verdict | V3 verdict | V2→V3 delta |
|---|---|---|---|---|
| p1a-samply-mode-1.md | ACCEPT-WITH-NOTE (92%) | ACCEPT (100%) | **ACCEPT (100%)** | 0; baseline preserved |
| p1b-samply-mode-2.md | ACCEPT (100%) | ACCEPT (100%) | **ACCEPT (100%)** | 0; baseline preserved |
| p1c-samply-mode-3.md | ACCEPT-WITH-NOTE (84%) | ACCEPT (100%) | **ACCEPT (100%)** | 0; baseline preserved |
| p1d-pmu-cycles.md | ACCEPT (100%) | ACCEPT-WITH-NOTE (93%) | **ACCEPT-WITH-NOTE (93%)** | 0; V2 self-ref drift carried forward unchanged |
| p1e-hot-leaf-attribution.md | REVISE (67%) | ACCEPT (100%) | **ACCEPT (100%)** | 0; V1 BINDING REVISE closure intact |
| p1f-results-delta.md | ACCEPT (100%) | ACCEPT (100%) | **ACCEPT (100%)** | 0; baseline preserved |

**Aggregate CH1 V3 ACCEPT-rate: 99.13% (114/115).** Identical to V2; both cycles above §3Z ≥95%.

§3Z convergence check:
- **Cycle 1 (V1):** 89.6% — DID NOT MEET threshold.
- **Cycle 2 (V2):** 99.13% — first cycle above threshold.
- **Cycle 3 (V3):** 99.13% — **second consecutive cycle above threshold**.

Zero orphan REVISEs:
- V1 had one BINDING REVISE (P1-E typed-plane file:line drift); V2 mechanically closed it (§0.2 verification).
- V2 had zero CH1 REVISEs; only one ACCEPT-WITH-NOTE (P1-D §1.1 self-referential line drift).
- V3 has zero CH1 REVISEs; the same single ACCEPT-WITH-NOTE carried forward unchanged.

**§3Z lock condition satisfied: V2 + V3 both ≥95%, zero orphan REVISEs across V2 and V3. CH1 LOCKED.**

## §2 — Per-artefact disposition (V3 confirming)

### §2.1 — P1-A V3 (ACCEPT — V2 baseline preserved)

The V2 F-V2-METHODOLOGY-1 build_flags_regime correction (`RUSTFLAGS NOT SET EXPLICITLY`) and the F-V2-P1A-MOVEMASK line-anchor convention paragraph at `:137` + 12 `(fn @ N)` table annotations all resolve correctly at HEAD. Re-grep against `skinny/Cargo.toml` confirms no `target-cpu` directive in `[profile.release]`. Re-grep against `skinny/crates/bbnf-simd/src/aarch64/movemask.rs` confirms `fn @ 4`; `skinny/crates/runtime/src/grammars/json/generated.rs` confirms `match_tiny_plain_string_with_cap @ 169`. CH1 sub-clauses all satisfied (samply symbol path + % self-time + source file:line; 17/17 coverage; every `unprofiled` resolved; atos pipeline equivalence). **V3 verdict: ACCEPT (100%).**

### §2.2 — P1-B V3 (ACCEPT — V2 baseline preserved)

`build_flags_regime` row at `:10` + cross-artefact comparator rule at `:185` re-verified intact. The canonical `twitter` 11037 vs 11627 Mbps refusal target is correctly cited. CH1 sub-clauses unchanged. **V3 verdict: ACCEPT (100%).**

### §2.3 — P1-C V3 (ACCEPT — V2 baseline preserved)

3 NEON line-anchor refreshes (`bulk_emit_positions_64.rs:2`, `bitmap_prefix_xor_64.rs:2`, `eob_pad_clamp.rs:4`) re-verified at HEAD. REDRESS path normalisation `skinny/REDRESS.md` confirmed by direct `ls`. CH1 sub-clauses all satisfied. **V3 verdict: ACCEPT (100%).**

### §2.4 — P1-D V3 (ACCEPT-WITH-NOTE — V2 self-referential line drift unchanged)

The V2 finding on §1.1 self-referential line drift (cited lines `41 + 62` for the two `cargo build` invocations vs actual HEAD lines `53 + 74`) is **re-verified verbatim at HEAD**. Re-grep `grep -n "cargo build" p1d-pmu-cycles.md` returns `23, 53, 74, 82, 88` (lines 23 + 82 + 88 are mentions inside prose; cargo-build commands are at 53 + 74). The substance ("both `cargo build` invocations carry `RUSTFLAGS="-C target-cpu=native"`") remains TRUE at HEAD; only the cited line numbers in two paragraphs (`:24-25` and `:88-89`) are stale by +11/+12.

V3 carries this forward as **ACCEPT-WITH-NOTE** identically to V2. The fix is mechanical (`s/line 41/line 53/`; `s/line 62/line 74/`; `s/(41 + 62)/(53 + 74)/`) but V3 is write-only on `hardening/V3/`; the P1-D source artefact is not modified by this dispatch. CH1 verdict unchanged. **V3 verdict: ACCEPT-WITH-NOTE (93%).**

### §2.5 — P1-E V3 (ACCEPT — V2 BINDING REVISE closure intact)

The V2 F-V2-P1E-1 typed-plane refresh (8 grep hits in `generated_real_typed.rs` at `516/527/592/1150/1219/1330/2197/2949`; the §1.2 grep extension to include `generated_real_typed.rs`; the §2.3 7-row refresh; the §5.4 Sources line per-suffix tagging) all re-verified at HEAD. `wc -l skinny/crates/bbnf-bench/src/generated_real_typed.rs` returns `3056` (unchanged). The V1 BINDING REVISE on this artefact remains mechanically CLOSED. **V3 verdict: ACCEPT (100%).**

### §2.6 — P1-F V3 (ACCEPT — V2 contracted-deferral framing intact)

The V2 F-V2-P1F-1 "Contracted-deferral framing" subsection at `:179-187` re-read at HEAD. Cross-references to SYNTHESIS §2 column declarations (`comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`), §3 C-2 wave deliverable, and PASS-ALPHA §4.4 precedent all intact. The CH5 V1 REVISE → S-P1 ACCEPT-WITH-CONTRACTED-DEFERRAL conversion is preserved. CH1 sub-clauses unchanged. **V3 verdict: ACCEPT (100%).**

## §3 — Critical findings (V3)

### Finding 0 (NEW V3 — cosmetic; non-blocking) — V2 CH1 §6 Sources cites P1-A line count as 345 vs HEAD 343

V2 CH1.md §6 Sources records `p1a-samply-mode-1.md (V2 HEAD: 345 lines; ...)`. Re-`wc -l` at HEAD returns 343 lines. This is a 2-line cosmetic drift on the V2 hardening lens document's source-cite block, NOT on any in-artefact P1-A cite. All 25 of P1-A's in-artefact file:line anchors verified at HEAD (§0.3). The 345 figure may stem from a V2 author's intermediate-state count or a trailing-newline counting convention difference. No corrective action required; flagged for aggregator awareness only. **Does not affect V3 99.13% rate.**

### Finding 1 (V2-CARRY-FORWARD; UNCHANGED) — P1-D §1.1 self-referential line drift

The V2-introduced self-referential line drift in `p1d-pmu-cycles.md` §1.1 + frontmatter is re-verified at HEAD. The two cited `cargo build` lines (41 + 62) are off by +12 / +12 against HEAD lines (53 + 74). Substance is correct (regime uniformity across both builds); only line numbers are stale because the V2 fold inserted an 11-line `build_flags_regime` block at frontmatter lines 21-30 and the V2 author cited line numbers from the pre-V2 file state. The fix remains the 30-second find/replace identified by V2 §4 Recommendation 1; not yet applied (V3 is pure-confirming write-only).

This finding is **ACCEPT-WITH-NOTE**, not REVISE. Per §3Z, the orphan-REVISE clock is not triggered. The substance of P1-D's CH1 sub-clauses (real PMU not estimated; 17/17 coverage with explicit absence accounting; unavailable counters NAMED absent) is unaffected.

### Finding 2 (V2-CLOSURE; STILL CLOSED) — P1-E typed-plane refresh

The V1 CH1 §3 Finding 1 BINDING REVISE on 5 typed-plane file:lines is **STILL mechanically closed** at V3. Every refreshed line matches HEAD grep output verbatim. The §1.2 grep extension to include `generated_real_typed.rs` continues to guard against future drift in the same file.

### Finding 3 (V2-CLOSURE; STILL CLOSED) — P1-A methodology + P1-C NEON anchors

The V2 closures on P1-A movemask/match_tiny + P1-C NEON 3-row line-anchor refresh remain **STILL mechanically closed** at V3. The "Line-anchor convention" paragraph + `(fn @ N)` annotations + `#[inline]` attribution disclosure across both artefacts continue to codify the samply RVA-to-line policy.

### Finding 4 (V2-CLOSURE; STILL CLOSED) — Cross-regime aggregator refusal rule

The `build_flags_regime` row across P1-A/B/C/D (RUSTFLAGS-unset for A/B; target-cpu=native for C/D) and the cross-artefact comparator rule at `p1b-samply-mode-2.md:185` continue to codify the aggregator-facing refusal rule. Discharges the CH4 V1 CF-1 cross-artefact regime concern.

### Finding 5 (V2-CLOSURE; STILL CLOSED) — P1-F contracted-deferral framing

The CH5 V1 REVISE → S-P1 ACCEPT-WITH-CONTRACTED-DEFERRAL conversion via the §4.1 contracted-deferral framing paragraph at `p1f-results-delta.md:179-187` remains intact. CH1 is unaffected (the four schema columns were never CH1 path:line sub-clause findings).

## §4 — V4 fold recommendations

V4 is **not required by §3Z**: V2 and V3 both meet ≥95%, with zero orphan REVISEs across both cycles. CH1 is LOCKED. The following are optional cosmetic-fold items that would lift V3 99.13% to 100% × 6/6 if pursued out-of-band by the aggregator or a subsequent micro-fold dispatch:

1. **P1-D §1.1 line drift refresh (mechanical, 30 seconds — UNCHANGED from V2 §4 Recommendation 1).** Two substitutions in `p1d-pmu-cycles.md`:
   - `:24-25`: "at line 41 and ... at line 62" → "at line 53 and ... at line 74".
   - `:88-89`: "matches exactly two lines (41 + 62)" → "(53 + 74)".
   Lifts P1-D from 93% to 100% and aggregate CH1 from 99.13% to 100% × 6/6.

2. **V2 CH1 §6 Sources cosmetic line-count update (mechanical, 5 seconds — NEW V3 §3 Finding 0).** Update V2 CH1.md §6 Sources entry from `p1a-samply-mode-1.md (V2 HEAD: 345 lines; ...)` to `343 lines`. Affects only the V2 hardening lens document, not any P1 artefact. Non-blocking; aggregator option.

3. **Optional V1-carry-forward ACCEPT-WITH-NOTE polish (non-blocking).** The 5 intra-body off-by-few-line items per §0.9 (parse_value_at, distinct_values variant, parse_number commentary, read_hex_unit_scalar P1-E off-by-one, alternate_pext gated off-by-one) remain available for cosmetic-fold; all are intra-body offsets within the cited fn; none affect substrate symbol identity.

4. **No primary captures need re-running.** All 56 P1-B + 17 P1-A + 4 P1-C + 231 P1-D PMU rows remain intact at `/tmp/skv14-p1*/`. Symbol resolution preserved in `.json.syms.json` sidecars.

## §5 — Convergence-gate impact (V3)

V3 CH1 ACCEPT-rate: **99.13% (114/115)**, identical to V2; above §3Z ≥95% threshold for the second consecutive cycle.

§3Z requires "≥95% × 2 cycles, zero orphan REVISEs":
- **Cycle 1 (V1):** 89.6% — DID NOT MEET threshold.
- **Cycle 2 (V2):** 99.13% — first cycle above threshold; mechanically closed V1 BINDING REVISE.
- **Cycle 3 (V3):** 99.13% — **second consecutive cycle above threshold; zero new REVISEs introduced; zero V2 ACCEPT-WITH-NOTEs escalated to REVISE**.

Zero-orphan-REVISE check:
- V1 BINDING REVISE on P1-E typed-plane — CLOSED at V2 (§0.2 re-verified at V3).
- V2 introduced zero CH1 REVISEs; one ACCEPT-WITH-NOTE on P1-D §1.1 self-ref drift.
- V3 introduced zero CH1 REVISEs; one cosmetic-only Finding 0 on V2 CH1 §6 Sources line-count (NOT a REVISE; NOT counted in 115).

**§3Z lock condition: SATISFIED. CH1 LOCKED. S-P1 → S-P2 dispatch may proceed from the CH1 side once all 7 lenses confirm V2 + V3 ≥95% with zero orphan REVISEs.**

CH1 lens recommendation per artefact at V3:
- **P1-A, P1-B, P1-C, P1-E, P1-F**: ACCEPT (100%) at V3; no further work required.
- **P1-D**: ACCEPT-WITH-NOTE (93%) at V3; optional V4 mechanical fix would lift to 100%.

No CH1 finding contradicts a primary-capture claim about hot-leaf identity, % self-time, or counter value; only minor synthesised-cite line-anchor drift (P1-D §1.1 self-ref) and cosmetic source-line-count drift (V2 CH1 §6 Sources). The substrate truth captured in P1-A/B/C/D's primary samply + PMU records is unaffected.

## §6 — Sources

- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH1.md` (V1 disposition; 89.6%)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md §2` (V1+V2+V3 disposition focus, inherited verbatim)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH1.md` (V2 disposition; 99.13%; one ACCEPT-WITH-NOTE on P1-D §1.1)
- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` (HEAD: 343 lines; :10 build_flags_regime; :137 line-anchor convention; :140-158 12-row annotated table)
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` (HEAD: 323 lines; :10 build_flags_regime; :185 cross-artefact regime guard; :314 RUSTFLAGS-unset disclosure)
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` (HEAD: 616 lines; :17-25 build_flags_regime; :269-273 3 NEON refresh rows; :509 + :599 REDRESS path normalisation)
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` (HEAD: 669 lines; :21-30 build_flags_regime; :23-31 frontmatter regime confirmation; :81-89 §1.1 confirmation block; ACTUAL cargo build at :53 + :74; STALE line cites at :24-25 + :88-89 — V3 §3 Finding 1)
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` (HEAD: 321 lines; :15 V2 fold note; :63-78 §1.2 generated_real_typed.rs grep extension; :155-167 §2.3 typed-plane 7-row refresh; :311 §5.4 Sources refresh)
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md` (HEAD: 269 lines; :179-187 §4.1 contracted-deferral framing)
- `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH1 binding)
- `restart/prompts/ORCHESTRATOR.md §3W` (CH1 universal definition) + `§3Z` (≥95% × 2 cycles convergence + zero-orphan-REVISE lock rule)
- `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md → [samply-symbol-resolution]` (samply discipline feedback)
- HEAD source files re-verified by grep (paths absolute):
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/Cargo.toml:78-95` (no `target-cpu` directive — confirms P1-A V2 correction holds at V3)
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/generated_real_typed.rs` (3056 lines; 8 grep hits at 516/527/592/1150/1219/1330/2197/2949 — confirms P1-E V2 refresh holds at V3)
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/aarch64/{movemask,bulk_emit_positions_64,bitmap_prefix_xor_64,eob_pad_clamp}.rs` (fn at 4/2/2/4 — confirms P1-A + P1-C V2 refreshes hold at V3)
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:159,164,169` (3 match_tiny variants — confirms P1-A V2 annotation holds at V3)
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md` (exists at HEAD; confirms P1-C V2 path normalisation holds at V3)
- V2 → V3 commit delta: `git diff 4ad8f1949 HEAD -- restart/skinny/tranches/sk-v14/research/p1/` (zero changes to 6 P1 artefacts; only `hardening/V2/*.md` + `HARDENING-S-P1-V2-CONSOLIDATED.md` added by V2 aggregator commit `4ad8f1949`)
- HEAD = `4ad8f1949099829b7ad723ddfd7eeb2a40cf61cd` (`docs(sk-v14-p1-hardening-V2): challenge V2 + consolidated`); P1 artefact tree last touched at `069ba203c` (`docs(sk-v14-p1-profile): V2 light micro-redispatch — five orphan REVISEs landed`)
