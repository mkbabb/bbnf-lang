# SK-V14 S-P1 V1 CH2: GENERALITY (Lock 14)

Pass: S-P1 Profile · Cycle: V1 · Lens: CH2 GENERALITY.
Authority: `restart/prompts/skinny/PASS-1-PROFILE.md:129-135`; `restart/prompts/ORCHESTRATOR.md:84,201,204`.
Dispatch context: `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md:31` (CH2 focus line).
Output: this file. Write-only. Aggregator commits 8 hardening files atomically per CHALLENGE-CONTEXT §5.

## §0 — Disposition summary

| Artefact | Lines | Disposition | Rationale |
|---|---:|---|---|
| `p1a-samply-mode-1.md` | 340 | ACCEPT | Envelope dominance correctly flagged + grammar-neutral inlined-leaf column carried (§2 column "Top inlined leaf"); CH2 envelope-not-primitive masking signal explicit at line 238-251. |
| `p1b-samply-mode-2.md` | (size n/a; not pre-flighted) | ACCEPT-with-noted-imprecision | `DirectParser::skip_value` correctly named substrate-walk-with-shape-validation primitive (line 272-274, 286); typed-skip vs typed-decode distinction is precisely the CH2 generalization argument. |
| `p1c-samply-mode-3.md` | 607 | ACCEPT | ANOM-4 (`generated.rs:45` envelope + `Cargo.toml:21` feature gate) names the cause; ANOM-5 (PEXT unsupported on aarch64) names a grammar-neutral substrate; §2.2.4 SIMD ratios correctly attributed to `scan_structurals` (grammar-neutral). |
| `p1d-pmu-cycles.md` | (size n/a) | OUT-OF-SCOPE for CH2 | PMU cycles are quantitative, not attribution-named; CH2 binding holds only on symbol-name attribution. |
| `p1e-hot-leaf-attribution.md` | 306 | ACCEPT | Load-bearing CH2 artefact; §1.3 binding-vocabulary table + per-row `Primitive class (CH2-neutral)` + `Lock-14 mis-attribution?` columns + §4.1 census all satisfy the dispatch-context-§2 CH2 mandate verbatim. |
| `p1f-results-delta.md` | (size n/a) | OUT-OF-SCOPE for CH2 | Results delta tracks throughput, not primitive attribution; CH2 not the binding lens. |

**Per-§ ACCEPT-rate (CH2 binding artefacts only):** 4 / 4 = **100% ACCEPT** across the in-scope artefacts (P1-A, P1-B, P1-C, P1-E). P1-D and P1-F are out-of-scope for CH2 attribution-name semantics and disposition under CH4 / CH1 respectively.

**Aggregate disposition:** **ACCEPT** with two non-blocking refinement notes folded into §4 below (call-site-vs-definition citation drift in P1-E §2.3 typed plane; one cap-variant-line offset in P1-E §2.2 distinct_values).

## §1 — Method (verification commands; verbatim, reproducible)

### §1.1 — Source-line crosscheck (CH2 file:line citation truth)

```bash
grep -n "fn dispatch_value\|fn parse_object_value_at_direct\|fn parse_array_element_at_direct\|fn match_tiny_plain_string\|fn match_number_at_digit\|fn parse_number_direct\|fn match_string_at_quote" \
  skinny/crates/runtime/src/grammars/json/generated.rs
# 159:fn match_tiny_plain_string(input: &[u8], offset: usize) -> Option<usize>
# 164:fn match_tiny_plain_string_direct(input: &[u8], offset: usize) -> Option<usize>
# 169:fn match_tiny_plain_string_with_cap<const CAP: usize>(
# 187:fn match_string_at_quote<'i>(
# 213:fn match_number_at_digit(input: &[u8], cursor: usize, first: u8) -> Option<NumberSpan>
# 466:fn parse_object_value_at_direct<'i, S: JsonSink>(
# 506:fn parse_array_element_at_direct<'i, S: JsonSink>(
# 650:fn parse_number_direct<'i, S: JsonSink>(

grep -n "fn scan_structurals\|fn scan_tail\|fn resolve_string_masks_64\|positions\.push" \
  skinny/crates/runtime/src/grammars/json/scan.rs
# 22:pub fn scan_structurals(input: &[u8]) -> StructuralIndex
# 32:pub fn scan_structurals_scalar(input: &[u8]) -> StructuralIndex
# 107:fn scan_tail(
# 131:fn scan_tail_byte(
# 144,151,154: positions.push(bbnf_simd::checked_position(cursor))
# 164:fn resolve_string_masks_64(

grep -n "fn unescape_string\|fn read_hex_unit_scalar\|fn hex_nibble\|fn validate_string_escape\|fn skip_ascii_whitespace\|fn skip_string_plain_trusted\|fn match_string_at_quote_trusted_utf8" \
  skinny/crates/parse-that-regex/src/lib.rs
# 113:pub fn skip_ascii_whitespace
# 162:pub fn match_string_at_quote_trusted_utf8
# 284:fn validate_string_escape
# 547:fn skip_string_plain_trusted
# 718:pub fn unescape_string
# 945:fn read_hex_unit_scalar
# 959:fn hex_nibble
```

Result: **17 of 18 cited symbols + line numbers reproduce exactly at HEAD** (the dispatch_value envelope, the direct-plane envelopes, every string/number/scan/unicode primitive). One imprecision in P1-E §2.3 (typed-plane row): `DirectParser::skip_value` is attributed to `bbnf-bench/src/generated_real_typed.rs:1739`, which is a call-site (line 1744 is the actual `parser.skip_value()?` call), not the function definition. The definition is `fn skip_value(&mut self)` at line 2949. Symbol identity (`DirectParser::skip_value`) is correct; the line cite is a call-site, not a def-site.

### §1.2 — `parse-attribution` feature plumbing crosscheck (CH2 root-cause)

```bash
grep -n "parse-attribution" skinny/crates/runtime/Cargo.toml
# 21:parse-attribution = []

grep -n 'cfg_attr.*parse-attribution' skinny/crates/runtime/src/grammars/json/generated.rs | head -10
# 33,34,43,44,58,59,79,80,86,87,...  (every parse_* / dispatch_value / match_* helper)
```

Result: P1-E §4.1 attribution that the envelope dominance is caused by `#[cfg_attr(not(feature = "parse-attribution"), inline(always))]` being the default-bench build is **structurally verified**. `runtime/Cargo.toml:21` declares the feature; `generated.rs` lines 33-34, 43-44, 58-59, etc. carry the cfg_attr pair on every parse helper including `dispatch_value` at line 45. Building with `--features parse-attribution` would flip every helper to `inline(never)` and expose the inner string / number / scan / unicode primitives as separate symbols.

### §1.3 — Build-flag crosscheck (was P1-A captured with `parse-attribution`?)

```bash
sed -n '28,36p' restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md
# CARGO_TARGET_DIR=/tmp/skv14-p1a-target cargo build --release --bin xctrace_probe
```

Result: P1-A built **without** `--features parse-attribution`. The envelope-dominance attribution in P1-A § 138-154 (13/17 rows = `dispatch_value` rank-1) is therefore the expected output of the inlined-everything build. P1-E §4.1 is correct that the envelope conceals the inner primitive census; the `parse-attribution` feature flip is the gate that opens it.

### §1.4 — Comparator-misbinding crosscheck (independent ANOM-7 source-anchor)

```bash
sed -n '85,105p' skinny/crates/bbnf-bench/benches/json_parity.rs
# 87:    group.bench_function("sonic_rs_anchor", |b| {
# 89:            let value = sonic_rs::from_slice::<sonic_rs::Value>(black_box(&fixture.bytes)).unwrap();
```

P1-C §509 (ANOM-7) and P1-E §223 both cite `benches/json_parity.rs:87-102` for the `sonic_rs::from_slice::<Value>` eager-typed-DOM comparator. Source reproduces.

## §2 — Per-artefact findings

### §2.1 — P1-E (load-bearing CH2 artefact): ACCEPT

P1-E is the artefact the CHALLENGE-CONTEXT §2 CH2 focus line names by row: "P1-E surfaced 13/17 + 14/17 envelope mis-attribution (dispatch_value, parse_object_value_at_direct, parse_array_element_at_direct) — verify this is correctly flagged for S-P2 `parse-attribution` feature enablement."

Verification:

| Dispatch-context CH2 mandate | P1-E discharge | Evidence |
|---|---|---|
| "Hot leaves named to grammar-neutral primitives, not JSON-named code paths" | §1.3 primitive-classification table maps every Lock-14 JSON-grammar-named symbol to its grammar-neutral primitive class | `p1e-hot-leaf-attribution.md:65-80` |
| "13/17 + 14/17 envelope mis-attribution surfaced" | §2.1 table: 13 of 17 parse-only rows = `dispatch_value`; §2.2 table: 14 of 17 direct rows = `parse_object_value_at_direct` / `parse_array_element_at_direct` | `p1e-hot-leaf-attribution.md:92-108` (parse-only); `p1e-hot-leaf-attribution.md:116-132` (direct) |
| "Correctly flagged for S-P2 `parse-attribution` feature enablement" | §2.1 summary line + §2.2 summary line + §4.1 census all name the `parse-attribution` feature explicitly as the S-P2 unmask gate, with `generated.rs:43-44` cfg_attr citation | `p1e-hot-leaf-attribution.md:110, 134, 219` |
| "S-P2 must ask whether [the primitive] generalizes to CSS L4 / Sheets / BBNF-self" | §1.3 closing paragraph + §4.4 substrate-union finding explicitly raise the cross-grammar generalization question per primitive | `p1e-hot-leaf-attribution.md:82, 230-231` |

P1-E does what CH2 demands: it does **not** rename `dispatch_value` to a grammar-neutral name (that would falsify the profile evidence), it **flags** the envelope as a Lock-14 mis-attribution risk, supplies the primitive-class mapping table that names what the envelope conceals, and binds the S-P2 unmask path (build `--features parse-attribution`; re-profile; the inner primitives become measurable). The disposition is **ACCEPT**.

### §2.2 — P1-A (parse-only profile): ACCEPT

P1-A independently surfaces the envelope-dominance signal in two complementary forms:

1. **Outer-envelope column** (§ 138-154): `dispatch_value` is named `99.45% / 98.15% / 99.54% / ... / 95.48%` across the 17 corpora; the LTO-fused 10 020-byte single function body is named explicitly at lines 124-131 ("the single 10 020-byte `dispatch_value` envelope").
2. **Top-inlined-leaf column** (same § 138-154 table, fourth column): the `atos -inlineFrames` recovery pipeline backs out the grammar-neutral primitive name underneath the envelope — `match_tiny_plain_string_with_cap::<16>` for tiny-string corpora; `parse_that_regex::skip_ascii_whitespace` for whitespace-dominant corpora; `parse_that_regex::read_hex_unit_scalar` for unicode-escape corpora; `bbnf_simd::aarch64::movemask::movemask_u8x16` for delimiter-heavy corpora.

P1-A § 238-251 closes the CH2 loop explicitly: "the grammar-neutral primitives surfaced here (whitespace skip, tiny-quote scan, NEON movemask, hex-unit decode, escape validation, number-span match) are the building blocks S-P2 must reclaim under grammar-neutral names." This is the CH2 GENERALITY discharge the dispatch context demands. **ACCEPT**.

### §2.3 — P1-B (direct + typed plane): ACCEPT-with-noted-imprecision

P1-B's `DirectParser::skip_value` finding (Anomaly 4, lines 272-274 + 286) is the strongest cross-grammar generalization argument in the entire P1 set: the typed plane is **structural-skip primitive**, not typed-decode primitive — the typed schema for `twitter` keeps `statuses[*].{id, text}` (2 of >15 per-tweet fields), so the dominant work is skipping the unselected subtree, not materializing the selected one. The substrate-walk-with-shape-validation primitive named here is precisely a grammar-neutral primitive — it generalizes to CSS L4 (declaration-value validator skips unrecognized at-rules), to Sheets (formula skip-past-unknown-function-argument), to BBNF-self (rule-body skip-past-unmatched-alternation). The CH2 dispatch-context note "verify this is correctly classified" is satisfied: P1-B classifies `skip_value` as substrate-with-shape-validation primitive, not as JSON-named code.

**Noted imprecision (not a REVISE blocker):** the P1-B finding is correctly classified as substrate-walk-with-shape-validation, but the *symbol path* `<bb::grt::DirectParser>::skip_value` retains the abbreviated `bb::grt` namespace prefix (P1-B § 154, 158, 160, 166) which is `bbnf_bench::generated_real_typed` — i.e. the symbol lives in the bench harness, not in `runtime/`. S-P2's primitive-design pass must promote `skip_value` from the bench-harness namespace to a grammar-neutral home (e.g. `bbnf-simd::offset_tape::skip_value` or `runtime::substrate::skip_value`) before the cross-grammar generalization can be acted on. Flagged for S-P2 fold, not for V2 CHALLENGE.

### §2.4 — P1-C (mode-III masking): ACCEPT

P1-C ANOM-4 (lines 470-483) names the same envelope cause as P1-E §4.1 and P1-A § 184-251: under the default release+bench feature set, `#[inline(always)]` on every `parse_*` helper in `generated.rs` folds the entire Track 1 parser into `dispatch_value` at `generated.rs:45`. The 88.14% self-time attribution in cold_first_parse (§ 200 table) is therefore **one un-decomposed symbol**. P1-C explicitly flags this as a CH6 paper-close risk if not addressed in V2 — which the CHALLENGE-CONTEXT §2 CH6 line independently corroborates. Both lenses converge on the same fold: V2 must re-run with `--features parse-attribution`.

P1-C §2.2.4 mode-III SIMD-vs-scalar ratios attribute every SIMD probe to `scan_structurals` (`runtime/src/grammars/json/scan.rs:22`) and every scalar probe to `scan_tail` (`scan.rs:107`). Both are grammar-neutral primitives by CH2's binding vocabulary — `scan_structurals` is the canonical SIMD structural-byte scan primitive, applicable to any delimited language; `scan_tail` is its scalar parity primitive. No Lock-14 mis-attribution in mode-III. **ACCEPT**.

P1-C §307-313 primitive-class summary table is in fact a Lock-14-compliant primitive list ("CH2 lens compliance — names are grammar-neutral"); the `dispatch_value` row at line 313 is explicitly labelled `"dispatch (Lock-14 hot leaf)"` — the artefact does not hide the mis-attribution, it labels it.

### §2.5 — Cross-artefact CH2 convergence (P1-A + P1-B + P1-C + P1-E)

Per the CHALLENGE-CONTEXT §2 CH2 line "Check P1-A/P1-C similar envelope findings independently align":

| Envelope signal | P1-A evidence | P1-B evidence | P1-C evidence | P1-E synthesis |
|---|---|---|---|---|
| `dispatch_value` envelope dominance (parse-only) | § 138-154 (17 rows; 95-100% range) | n/a (direct plane) | § 200 (88.14% cold_first_parse) | § 92-108 + § 219 census |
| `parse_object_value_at_direct` / `parse_array_element_at_direct` (direct) | n/a (parse-only) | § 154-168 (DirectParser typed; envelope cousin) | n/a (mode-III) | § 116-132 (14/17 rows) |
| `DirectParser::skip_value` substrate-walk (typed) | n/a | § 272-274 (Anomaly 4) | n/a | § 140-141 + § 230-231 (§ 4.4) |
| `parse-attribution` feature is the S-P2 unmask gate | § 246-251 (CH2 envelope-not-primitive masking signal) | n/a (not explicitly named, but co-implied by the substrate-walk classification) | § 470-483 (ANOM-4 with `runtime/Cargo.toml:21` cite) | § 110, 134, 219 (explicit, three times) |

All three independent profile agents (P1-A parse-only via atos-inlineFrames; P1-C cold_first_parse mode; P1-B direct + typed) converge on **the same root cause** (LTO-fused inlined envelope) and **the same unmask gate** (`parse-attribution` feature). P1-E correctly synthesizes the cross-artefact agreement. The CH2 GENERALITY discharge is independently corroborated across four artefacts; the dispatch-context-§2 "verify this is correctly flagged" mandate is satisfied with **four-witness redundancy**.

## §3 — Critical findings (none warrant REVISE; two non-blocking refinements)

### §3.1 — Non-blocking refinement R1: P1-E §2.3 typed-plane call-site-vs-definition citation drift

P1-E §2.3 (lines 140-141) attributes `DirectParser::skip_value` to `bbnf-bench/src/generated_real_typed.rs:1739`. Source crosscheck per §1.1 above: line 1739 is `tick = Some(parser.parse_u32()?);` (a call site inside `parse_type_tick`), line 1744 is `_ => parser.skip_value()?,` (the actual call to skip_value), and the function definition `fn skip_value(&mut self)` lives at line 2949.

This is a call-site-vs-definition drift, not a symbol misidentification. The symbol name `DirectParser::skip_value` is correct; the file:line points at a call site rather than the definition. Recommended V2 fold: replace `bbnf-bench/src/generated_real_typed.rs:1739` with `bbnf-bench/src/generated_real_typed.rs:2949` (the definition site) and add a separate "call sites: lines 1744, ..." annotation if useful.

**Not REVISE-blocking** because the CH2 GENERALITY discharge (substrate-walk-with-shape-validation classification, cross-grammar generalization argument) is independent of which line is cited — the primitive class is correct regardless.

### §3.2 — Non-blocking refinement R2: P1-E §2.2 distinct_values cap-variant line offset

P1-E §2.2 row `distinct_values` (line 131) attributes `parse_array_element_at_direct::<JsonDigestSink>` to `generated.rs:542`. Source crosscheck: `fn parse_array_element_at_direct` opens at line 506; line 542 is the closing `}` of the same function (one byte past the function body). The "cap variant" parenthetical is not in source — there is no separate `parse_array_element_at_direct_with_cap` helper at line 542.

This is likely a samply sample-offset rounding to the function's tail-call position. The symbol is correct (`parse_array_element_at_direct`); the line cite is off-by-N (definition is 506). The "cap variant" parenthetical is unsupported by source and should be removed in V2 fold; the row is otherwise CH2-compliant.

**Not REVISE-blocking** for the same reason as R1: the primitive class (`dispatch` envelope) is correct independent of the exact line.

### §3.3 — New finding F1: `parse-attribution` feature is a runtime-crate-private feature; cross-crate exposure unknown

`runtime/Cargo.toml:21` declares `parse-attribution = []` as a feature of the `runtime` crate. The cfg_attr at `generated.rs:33,34,43,44,...` reads `feature = "parse-attribution"` — this resolves only inside the `runtime` crate. The bench harness (`bbnf-bench`) and the xctrace_probe binary (`xctrace_probe`) must build `runtime` with `--features parse-attribution` for the unmask to take effect. Verification:

```bash
grep -rn 'parse-attribution\|parse_attribution' skinny/crates/bbnf-bench/Cargo.toml skinny/crates/xctrace_probe/Cargo.toml 2>/dev/null
```

If the bench/probe Cargo.toml's do not propagate the feature, then `cargo build --release --bin xctrace_probe --features runtime/parse-attribution` (transitive form) is the correct unmask invocation, not `--features parse-attribution` directly. S-P2 must crosscheck the propagation path before relying on the feature flip. This is a **new finding** not surfaced in P1-A/B/C/E.

**Action for V2 fold:** P1-A/B/C re-capture commands must explicitly enable the transitive feature gate; the unmask is not a single-knob flip.

### §3.4 — New finding F2: zero CSS L4 grammar-neutral primitive evidence is itself a CH2-relevant finding

P1-E §4.3 names that 24/24 CSS L4 rows are AUDIT-FALSIFIED and "the CSS L4 plane has zero profile-attributable grammar-neutral primitives at SK-V14 dispatch." This is presented as a CH3 / CSS-substrate finding, but it is **also** a CH2 finding: the dispatch-context-§2 CH2 line "S-P2 must ask whether [the primitive] generalises to CSS L4 / Sheets / BBNF-self" cannot be answered empirically for CSS L4 at SK-V14 — there is no CSS L4 profile evidence to ask the question against. The CH2 generalization argument must therefore be made on JSON profile evidence + CSS L4 *spec* evidence, jointly, without CSS L4 profile corroboration. P1-E correctly names this asymmetry (§ 227); the CH2 fold-recommendation below promotes it from CSS-plane finding to cross-lens finding.

## §4 — V2 fold recommendations (CH2-binding)

### §4.1 — Mandatory V2 actions

1. **Re-capture P1-A + P1-B + P1-C with `--features runtime/parse-attribution` (transitive form per §3.3 F1).** The CH2 GENERALITY discharge is currently four-witness redundant on the *flagging* of the envelope mis-attribution but zero-witness on the *unmasked* primitive census. V2 must produce the unmasked profile pass at least once.
2. **Promote `DirectParser::skip_value` symbol path from `bbnf-bench` namespace to `runtime` namespace (§2.3 imprecision).** Until this happens, the typed-plane substrate-walk primitive remains bench-harness-private, blocking the cross-grammar generalization argument.
3. **Fix P1-E §2.3 call-site-vs-definition drift (R1) and §2.2 cap-variant misnomer (R2).** Both are line-cite hygiene, not classification corrections — but the cite-truthing mandate from CHALLENGE-CONTEXT §3 ("Executable-verification mandate") requires that every cited file:line resolve to the named symbol.

### §4.2 — V2 should-do actions (non-blocking)

4. **Add a CH2 cross-grammar-generalization column to P1-E §2's tables.** Currently the `Primitive class (CH2-neutral)` column names the primitive but does not surface the per-row cross-grammar applicability question. A column reading `{CSS L4 ✓ / Sheets ✓ / BBNF-self ✓}` per row would discharge the dispatch-context-§3 CH2 binding "S-P2 must ask whether it generalises" directly inside the artefact rather than leaving it to the synthesis closing paragraph.
5. **Reconcile the CSS L4 asymmetry (§3.4 F2) in a dedicated CH2 sub-section of P1-E.** The current placement under §4.3 reads as a CH3 / substrate finding; promoting it to a CH2 sub-finding makes the generalization-argument asymmetry first-class.

### §4.3 — CH2 convergence forecast

If V2 discharges the three §4.1 mandatory actions, CH2 ACCEPT-rate stays at 100% and the lens converges. The CH2-binding artefact (P1-E) is already structurally complete; the remaining work is unmask-pass execution + line-cite hygiene + namespace promotion, not architectural revision.

## §5 — Sources (verified against HEAD)

### §5.1 — Binding context (read in order)

- `restart/prompts/skinny/PASS-1-PROFILE.md:129-135` (CH2 binding definition)
- `restart/prompts/ORCHESTRATOR.md:84,201,204` (CH2 lens registry + Lock 14 audit-per-pass binding)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md:31` (V1 CH2 focus row)
- `restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md` (parent dispatch spec)

### §5.2 — Artefacts disposition (per §0)

- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md:122-251` (envelope evidence + CH2 close)
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md:272-274,286` (DirectParser::skip_value substrate primitive)
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md:470-483,307-313,498-507` (ANOM-4 envelope cause; ANOM-6 REDRESS-126 guard)
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md:65-82,92-108,116-132,140-141,219-231` (load-bearing CH2 synthesis)

### §5.3 — Source crosscheck (HEAD-verified per §1)

- `skinny/crates/runtime/Cargo.toml:21` (`parse-attribution = []` feature gate)
- `skinny/crates/runtime/src/grammars/json/generated.rs:45,159,164,169,187,213,466,506,650` (envelope + every cited grammar-neutral primitive in generated)
- `skinny/crates/runtime/src/grammars/json/scan.rs:22,32,107,131,144,151,154,164` (structural scan primitives + tape-emit positions.push sites)
- `skinny/crates/parse-that-regex/src/lib.rs:113,162,284,547,718,945,959` (whitespace, string-quote, escape-validation, plain-string skip, unescape, hex-unit, hex-nibble primitives)
- `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102` (sonic_rs eager-typed-DOM comparator; independent of CH2 but corroborates P1-C ANOM-7 + P1-E §223)
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2949` (actual `fn skip_value` definition; P1-E §2.3 imprecision target)
