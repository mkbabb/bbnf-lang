# S-P1 CHALLENGE V3 — Lens CH2 (GENERALITY)

Pass: S-P1 Profile. Cycle: V3 (pure confirming pass). Lens: CH2 GENERALITY.
Date: 2026-05-23.
Scope: V3 confirming pass over unchanged V2 artefacts at HEAD `4ad8f1949` (V2 hardening commit — adds `V2/CH*.md` + V2 consolidated; **does not touch any P1 axis artefact**; P1 files remain at `069ba203c` V2 micro-fold geometry). Verify (a) V2's 100% ACCEPT verdict (4/4 in-scope artefacts) carries forward unchanged for the second consecutive cycle; (b) V2's mechanical closure of V1 R1 via `F-V2-P1E-1` remains intact at HEAD (skip_value `:2949`, parse_option_scalar_string `:2197`, parse_type_mesh `:1150`, parse_type_marine_geometry_data `:1330`, parse_type_plugin `:516`); (c) V1 R2 (P1-E §2.2 `distinct_values` row `:542` → `:506` cite drift) remains in its V2 status as non-blocking, deferred to S-P2; (d) V1 F1 (`parse-attribution` transitive feature) and F2 (CSS L4 zero-evidence asymmetry) carry forward to S-P2 unchanged; (e) V2-introduced fresh findings (F-V2-CH2-1 apache_builds string primitive; F-V2-CH2-2 `(fn @ N)` cite hygiene; F-V2-CH2-3 cohort discovery is CH4-binding) hold as non-blocking V3 hand-offs to S-P2; (f) no new CH2 GENERALITY REVISE is uncovered by V3 lens application.
Authority: `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH2 binding); `restart/prompts/ORCHESTRATOR.md:84,201,204` (CH2 lens registry + Lock 14 audit-per-pass binding); `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH2.md` (V2 disposition; 100% ACCEPT, R1 closed); `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md §2` (CH2 V1 focus row carries to V3 verbatim); `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md §2.1` (V3 Option A pure-confirming dispatch directive).
Artefacts reviewed (HEAD = `4ad8f1949`; P1 files unchanged since `069ba203c`): `p1a-samply-mode-1.md`, `p1b-samply-mode-2.md`, `p1c-samply-mode-3.md`, `p1d-pmu-cycles.md`, `p1e-hot-leaf-attribution.md`, `p1f-results-delta.md` under `restart/skinny/tranches/sk-v14/research/p1/`.
V2 → V3 commit delta: `4ad8f1949` is `docs(sk-v14-p1-hardening-V2): challenge V2 + consolidated` — adds `V2/CH*.md` + `HARDENING-S-P1-V2-CONSOLIDATED.md`; **zero P1 axis file modifications**. Therefore V3 has a vanishing diff slice on the binding artefacts — the confirming pass discharges the §3Z "× 2 cycles" sub-clause by re-verifying that the V2 verdict still maps to HEAD source.

## §0 — Disposition summary (V3 confirming)

V2 disposition was **100% ACCEPT (4/4 CH2-binding artefacts)** with V1 R1 mechanically closed by `F-V2-P1E-1`; R2/F1/F2 carried forward to S-P2; three V2-lens fresh findings (F-V2-CH2-1/2/3) all non-blocking. V3 dispatch is Option A pure-confirming (per `HARDENING-S-P1-V2-CONSOLIDATED §2.3`); no V2 → V3 P1 axis diff. The V3 confirming verdict therefore tracks V2: **100% ACCEPT**, with all V2 closures (R1) re-verified intact at HEAD, all V2 carry-forwards (R2/F1/F2) unchanged, and the four-witness CH2 redundancy (P1-A + P1-B + P1-C + P1-E) holding for the third consecutive cycle.

| Artefact | V1 disposition | V2 disposition | V3 confirming disposition | V3 delta basis |
|---|---|---|---|---|
| `p1a-samply-mode-1.md` | ACCEPT | ACCEPT | **ACCEPT** | Zero P1-A diff since `069ba203c`. V2 `(fn @ 4)` movemask annotation + envelope-not-primitive masking signal at §137-141, §149, §238-251 re-verified at HEAD; primitive class (`scan`/`dispatch`) unchanged. |
| `p1b-samply-mode-2.md` | ACCEPT-with-noted-imprecision | ACCEPT (imprecision unchanged) | **ACCEPT** (imprecision unchanged) | Zero P1-B diff. `DirectParser::skip_value` substrate-walk-with-shape-validation classification at §275-277 + §289 re-verified at HEAD; bench-harness namespace imprecision still flagged for S-P2 promotion (not V3 escalation). |
| `p1c-samply-mode-3.md` | ACCEPT | ACCEPT | **ACCEPT** | Zero P1-C diff. ANOM-4 envelope cause + `parse-attribution` unmask gate (§337-345, §479-486) re-verified at HEAD; §307-313 Lock-14-compliant primitive table intact; §322 dispatch-primitive row intact. |
| `p1d-pmu-cycles.md` | OUT-OF-SCOPE (CH4 binding) | OUT-OF-SCOPE | **OUT-OF-SCOPE** | Zero P1-D diff. PMU quantitative, not attribution-named. |
| `p1e-hot-leaf-attribution.md` | ACCEPT | ACCEPT (R1 closed) | **ACCEPT** (R1 closure intact) | Zero P1-E diff. §2.3 typed-plane refresh anchors (`:2949`, `:2197`, `:1150`, `:1330`, `:516`) re-grepped at HEAD — every one resolves to the canonical `fn` definition line. R2 (§2.2 `distinct_values` `:542`) carry-forward unchanged. §227 CSS L4 zero-evidence framing intact. |
| `p1f-results-delta.md` | OUT-OF-SCOPE (CH1/CH4 binding) | OUT-OF-SCOPE | **OUT-OF-SCOPE** | Zero P1-F diff. Results-delta tracks throughput, not primitive attribution. |

**Per-§ ACCEPT-rate (CH2 binding artefacts only):** 4 / 4 = **100% ACCEPT** unchanged from V1 + V2 across the four in-scope artefacts (P1-A, P1-B, P1-C, P1-E).

**Aggregate disposition: ACCEPT.** Zero orphan REVISEs. R1 closure re-verified intact. R2/F1/F2 + F-V2-CH2-1/2/3 carry-forward to S-P2 (none re-opened by V3 lens). CH2 V3 → §3Z LOCK: **100% × 3 cycles**, zero orphan REVISEs across V1/V2/V3 — exceeds the §3Z "≥95% × 2 cycles" gate by ≥4 pp and one extra cycle. CH2 GENERALITY lens **CONVERGES** at V3.

## §1 — Method (V3 verification commands; verbatim, reproducible at HEAD `4ad8f1949`)

### §1.1 — V2 R1 closure re-verification (the load-bearing V2 packet F-V2-P1E-1)

V2 §1.1 verified that V1 R1 (`DirectParser::skip_value` cite `:1739` → `:2949`) closed mechanically via `F-V2-P1E-1`. Re-grep against HEAD to confirm the closure has not regressed:

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

`wc -l skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `3056`. All 8 grep hits cited in `P1-E §5.4` reproduce exactly at HEAD; the 5 V2-refreshed §2.3 typed-plane anchors (`:2949` skip_value, `:2197` parse_option_scalar_string, `:1150` parse_type_mesh, `:1330` parse_type_marine_geometry_data, `:516` parse_type_plugin) all hit their canonical `fn` definition lines. **V1 R1 closure intact at V3 HEAD.**

### §1.2 — V1 R2 carry-forward re-verification (P1-E §2.2 `distinct_values` `:542` row)

V2 §1.2 noted that R2 (`distinct_values` row attributes `parse_array_element_at_direct::<JsonDigestSink>` to `generated.rs:542`; definition opens at line 506; line 542 is the closing `}` of the function) was **not** refreshed by `F-V2-P1E-1` because R2 was V1-classified as non-blocking. Re-verify the underlying source state at HEAD:

```bash
grep -n "fn parse_array_element_at_direct\|fn parse_object_value_at_direct" \
  skinny/crates/runtime/src/grammars/json/generated.rs
# 466:fn parse_object_value_at_direct<'i, S: JsonSink>(
# 506:fn parse_array_element_at_direct<'i, S: JsonSink>(
```

The definition still opens at line 506. P1-E §2.2 row 146 still cites `generated.rs:542` (verified by `grep -n "generated.rs:542" restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` → `146:| distinct_values | parse_array_element_at_direct::<JsonDigestSink> (generated.rs:542) | 49.5 | dispatch (array-element with cap variant) | yes | AUDIT-SUSTAINED (N-direct/NO-GO) |`). **R2 status unchanged from V2:** non-blocking; the CH2 primitive class `dispatch` is correct regardless of the exact line cite; route stays the same — recommend collapse to `:506` (or the V2-introduced `(fn @ 506)` cite hygiene form per F-V2-CH2-2) at S-P2 first-wave grooming. **No V3 escalation warranted.**

### §1.3 — V1 F1 carry-forward re-verification (`parse-attribution` transitive feature gate)

V2 §1.3 verified F1's prescription (build with `--features runtime/parse-attribution` transitive form because `parse-attribution` is a `runtime`-crate-private feature). Re-verify feature plumbing at HEAD:

```bash
grep -n 'parse-attribution\|parse_attribution' skinny/crates/runtime/Cargo.toml \
  skinny/crates/bbnf-bench/Cargo.toml
# skinny/crates/runtime/Cargo.toml:21:parse-attribution = []
# (bbnf-bench Cargo.toml: no match — the bench harness depends on runtime with features = ["bench-counters"] only; parse-attribution does not propagate as a default)
```

Confirmed: `runtime/Cargo.toml:21` declares the feature; `bbnf-bench/Cargo.toml` line 18 (`runtime = { workspace = true, features = ["bench-counters"] }`) does **not** add `parse-attribution` to the default feature set. F1's prescription — use `--features runtime/parse-attribution` (transitive form) at the bench-build command line for any S-P2 re-capture — remains the correct unmask invocation. **F1 carry-forward intact at V3.**

**Fresh V3 observation (non-escalation):** V2 §1.3 cited `skinny/crates/xctrace_probe/Cargo.toml` as a sibling crate path to check propagation. At HEAD, `xctrace_probe` does not exist as a separate crate — it is a binary inside `bbnf-bench` (`skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`; verified `find skinny/crates -name "*.rs" -path "*xctrace*"` → only this single hit; `find skinny/crates -maxdepth 2 -name "Cargo.toml" | xargs grep -l "name = \"xctrace_probe\""` → no hits). The substantive F1 finding is unaffected (the xctrace_probe bin builds against the same bbnf-bench feature set, so the transitive form `--features runtime/parse-attribution` still applies); only the V2 cite path is a vestigial pre-restructure reference. **Routed to S-P2 first-wave grooming as cite-hygiene only (F-V3-CH2-1, §3.1).**

### §1.4 — V1 F2 carry-forward re-verification (CSS L4 zero-evidence asymmetry)

V2 §1.4 verified that P1-E §227 CSS L4 zero-profile-evidence framing carries forward; the V2 fold did not alter §4.3. Re-verify at HEAD:

```bash
grep -n "CSS L4\|24/24\|css.*AUDIT-FALSIFIED" \
  restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md | head -10
# 19: ... 5 parse_only + 4 direct + 7 typed + 24 CSS = 40 rows AUDIT-FALSIFIED ...
# 227 (per V2 §1.4 — the asymmetry framing paragraph)
```

P1-E §227 carry-forward intact (V3 commit `4ad8f1949` did not touch any P1 file). F2's prescriptive guidance — promote CSS L4 zero-evidence from CH3/CSS-substrate to a CH2 sub-finding for S-P2 — remains S-P2 fold work, not V3 escalation. **F2 carries forward unchanged.**

### §1.5 — Cross-artefact CH2 substrate-walk classification re-verification (P1-B `skip_value`)

Re-verify at HEAD:

```bash
grep -n "skip_value\|substrate-walk\|structural-skip primitive" \
  restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md | head -10
# 90:  | <bbnf_bench::generated_real_typed::DirectParser>::skip_value | ...
# 155: twitter Track1 ... <bb::grt::DirectParser>::skip_value 72.50% ...
# 159: citm_catalog Track1 ... skip_value 76.12% ...
# 161: github_events Track1 ... skip_value 39.51% ...
# 167: marine_ik Track1 ... skip_value 41.70% ...
# 275: Anomaly 4 — generated_real_typed::DirectParser::skip_value dominates ...
# 277: "...the strongest cold-leaf evidence ... structural-skip primitive, not a typed-decode primitive..."
```

P1-B §90, §155, §159, §161, §167 (all 4 typed corpora with skip_value rank-1) + §275-277 (Anomaly 4 substrate-walk framing) + §289 (S-P2 hand-off) all carry the substrate-walk classification verbatim. **CH2 cross-grammar generalization argument unchanged at V3.**

### §1.6 — Cross-artefact CH2 envelope dominance re-verification (P1-A + P1-C)

Re-verify the parallel-witness envelope dominance signal at HEAD:

```bash
grep -n "parse-attribution\|generated.rs:45\|envelope-not-primitive\|ANOM-4\|Anomaly 4" \
  restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md \
  restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md | head -20
# p1a: §137, 141, 149, 246, 251 (dispatch envelope + (fn @ 4) annotation + CH2 close)
# p1c: §209, 224, 322, 337-345, 479-486, 605 (ANOM-4 + parse-attribution gate + Cargo.toml:21 cite)
```

P1-A §141 (twitter row: `dispatch_value 99.45% (generated.rs:45)` + `match_tiny_plain_string_with_cap::<16> 39.27% (generated.rs:160,176 (fn @ 169))`), P1-A §149 (gsoc-2018: `dispatch_value 99.50%` + `movemask_u8x16 24.58% (movemask.rs:22 (fn @ 4))`), P1-A §246, §251 (CH2 close paragraph naming the building blocks S-P2 must reclaim under grammar-neutral names) all carry forward. P1-C §322 dispatch-primitive row (`dispatch (Lock-14 hot leaf)`), §337-345 ANOM-4 paragraph (cfg_attr quote + `runtime/Cargo.toml:21` cite), §479-486 ANOM-4 explicit re-statement, §605 sources cross-reference all carry forward. **Four-witness CH2 redundancy (P1-A + P1-B + P1-C + P1-E) holds at V3 for the third consecutive cycle.**

### §1.7 — V2-introduced fresh findings re-verification (F-V2-CH2-1, F-V2-CH2-2, F-V2-CH2-3)

V2 §3 introduced three lens-fresh findings, all non-blocking. Re-verify at HEAD:

**(i) F-V2-CH2-1 — `apache_builds` `parse_option_scalar_string` row as the strongest cross-grammar string-primitive candidate.**

```bash
sed -n '2197,2200p' skinny/crates/bbnf-bench/src/generated_real_typed.rs
# 2197:fn parse_option_scalar_string<'i>(parser: &mut DirectParser<'i>) -> Result<Option<Cow<'i, str>>, DirectBuildError<'i>> {
```

Cite resolves; the optional-scalar-string primitive is the only non-`dispatch` typed-plane row in P1-E §2.3. **Carry forward to S-P2 unchanged.**

**(ii) F-V2-CH2-2 — V2 P1-A `(fn @ N)` cite-hygiene convention precedent.**

Re-verify the convention's anchor in P1-A:

```bash
grep -n "fn @ 4\|(fn @ " restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md | head -10
# 137: Line-anchor convention (V2 fold F-V2-P1A-MOVEMASK) ... bbnf_simd::aarch64::movemask::movemask_u8x16 cites carry movemask.rs:22 (fn @ 4) ...
# 141: ... movemask.rs:22 (fn @ 4) ...
# 149: ... movemask.rs:22 (fn @ 4) ...
# 177: ... movemask.rs:22 (fn @ 4) ...
```

P1-A §137 establishes the convention; §141, §149, §177 are exemplars. Convention not yet standardized across P1-B, P1-C, P1-E (V2 §3.2 observation). **V3 does not standardize the convention (out of pure-confirming scope); routed to S-P2 first-wave grooming alongside R2 collapse (§3.1 below).**

**(iii) F-V2-CH2-3 — V2 cohort discovery is CH4-binding, not CH2.**

Re-verify: cohort regime (`{P1-A, P1-B}` RUSTFLAGS-unset vs `{P1-C, P1-D}` RUSTFLAGS-native) does not re-classify any CH2 primitive (`scan`, `string`, `number`, `unicode`, `structural`, `tape`, `dispatch` are grammar-neutral by definition; the cohort affects absolute cycle counts and Mbps numbers — CH4 binding — but not primitive class assignment). **CH2 V3 disposition: no escalation; cohort regime remains CH4-binding only.**

## §2 — Per-artefact V3 confirming findings

### §2.1 — P1-E (load-bearing CH2 artefact): ACCEPT; V2 R1 closure re-verified intact

P1-E V2 §2.3 typed-plane refresh anchors (`:2949` skip_value, `:2197` parse_option_scalar_string, `:1150` parse_type_mesh, `:1330` parse_type_marine_geometry_data, `:516` parse_type_plugin) all resolve at HEAD to the canonical `fn` definition lines per §1.1. The V2 mechanical closure of V1 R1 has not regressed; the typed-plane substrate-walk classification (which was V1 ACCEPT independent of the cite drift) is intact. P1-E §2.2 `distinct_values` row 146 still carries the `:542` cite — V1 R2 status carry-forward — explicitly non-blocking per V1 §3.2 ("the primitive class `dispatch` envelope is correct independent of the exact line"). P1-E §227 CSS L4 zero-evidence framing intact for F2 carry-forward. P1-E §65-82 binding-vocabulary table + §92-108 parse-only census + §116-132 direct census + §219-231 substrate-union finding all intact.

**V3 disposition: ACCEPT.** R1 closure intact; substrate-walk classification intact; cross-grammar generalization argument intact; R2/F1/F2 carry-forward to S-P2.

### §2.2 — P1-A (parse-only profile): ACCEPT; V2 movemask + cite-hygiene precedent intact

P1-A V2 changes (`F-V2-P1A-MOVEMASK` + `F-V2-METHODOLOGY-1`) re-verified at HEAD per §1.6: the `(fn @ 4)` movemask cite hygiene convention is present at §137, §141, §149, §177; envelope dominance + inlined-leaf columns at §138-154 intact; CH2 closing paragraph at §238-251 intact. The `fn movemask_u8x16` source anchor (`skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`) re-verified at HEAD via `grep -n "fn movemask_u8x16" skinny/crates/bbnf-simd/src/aarch64/movemask.rs → 4:pub unsafe fn movemask_u8x16(value: uint8x16_t) -> u16 {`.

Four CH2 grammar-neutral primitives still independently surfaced (`match_tiny_plain_string_with_cap`, `skip_ascii_whitespace`, `movemask_u8x16`, `read_hex_unit_scalar`). Envelope-not-primitive masking signal still correctly flagged.

**V3 disposition: ACCEPT.** Envelope-dominance + inlined-leaf columns unchanged; cite-hygiene precedent intact.

### §2.3 — P1-B (direct + typed plane): ACCEPT; imprecision unchanged (still flagged for S-P2)

P1-B's load-bearing CH2 finding (`DirectParser::skip_value` substrate-walk-with-shape-validation primitive; §275-277 + §289) intact at HEAD per §1.5. The V1 noted imprecision (bench-harness namespace `<bbnf_bench::generated_real_typed::DirectParser>` rather than `runtime::`) unchanged from V2 — V3 does not re-classify it; it remains non-blocking pending S-P2 primitive-design promotion (move `skip_value` to a grammar-neutral home such as `runtime::substrate::skip_value` or `bbnf-simd::offset_tape::skip_value`).

**V3 disposition: ACCEPT** (with noted imprecision unchanged; still flagged for S-P2 promotion, not V3 escalation).

### §2.4 — P1-C (mode-III masking): ACCEPT; NEON line-anchors + ANOM-4 intact

P1-C V2 changes (`F-V2-METHODOLOGY-1` + `F-V2-P1C-LINEDRIFT` — 3 NEON primitive line-anchors + `#[inline]` attribute annotation) re-verified intact at HEAD per §1.6. ANOM-4 (envelope cause + `parse-attribution` unmask gate, §337-345 + §479-486) intact. §307-313 Lock-14-compliant primitive table intact. §322 dispatch-primitive row labelled `dispatch (Lock-14 hot leaf)` intact — the artefact does not hide the mis-attribution, it explicitly labels it per the CH2 dispatch-context mandate.

**V3 disposition: ACCEPT.** Four-witness CH2 redundancy intact.

### §2.5 — Cross-artefact V3 CH2 convergence (per V2 §2.5 carry-forward)

| Envelope signal | P1-A V3 evidence | P1-B V3 evidence | P1-C V3 evidence | P1-E V3 synthesis |
|---|---|---|---|---|
| `dispatch_value` envelope dominance (parse-only) | §141, §149 (intact at HEAD) | n/a (direct plane) | §209, §224, §322 (intact) | §92-108 + §219 census (intact) |
| `parse_object_value_at_direct` / `parse_array_element_at_direct` (direct) | n/a | §154-168 (DirectParser envelope intact) | n/a | §116-132 14/17 rows (intact; R2 row 146 `:542` carry-forward) |
| `DirectParser::skip_value` substrate-walk (typed) | n/a | §90, §155-167, §275-277, §289 (intact) | n/a | **§2.3 row `:2949` (V2 R1 closure intact at V3 HEAD)** |
| `parse-attribution` feature is the S-P2 unmask gate | §246-251 (intact) | n/a | §337-345, §479-486, §605 (intact) | §110, §134, §219 (intact) |
| `(fn @ N)` cite-hygiene convention precedent (V2-introduced) | §137, §141, §149, §177 (intact) | not adopted | not adopted | not adopted (V3 routes to S-P2) |
| Cohort regime (RUSTFLAGS-unset vs RUSTFLAGS-native) | CH4-binding only | CH4-binding only | CH4-binding only | CH4-binding only (no CH2 re-classification) |

All four CH2-binding artefacts independently agree at V3 on root cause + unmask gate. The V3 confirming pass confirms the V1+V2 four-witness CH2 redundancy is intact for the third consecutive cycle; the V1 + V2 + V3 100% ACCEPT rate sustains.

## §3 — Fresh-finding scan (V3 lens)

V3 has a vanishing diff slice on the binding artefacts (V2 → V3 commit `4ad8f1949` touches only `V2/CH*.md` + V2 consolidated; zero P1 axis modifications). The surface area for fresh CH2 findings is therefore limited to (i) drift in HEAD source files cited by V2 (re-verified intact in §1), and (ii) cross-cycle observations only visible from the V3 vantage. One non-escalation finding surfaces:

### §3.1 — F-V3-CH2-1 — V2 §1.3 cited `skinny/crates/xctrace_probe/Cargo.toml` is a vestigial path; xctrace_probe is now a `bbnf-bench` bin

V2 CH2 §1.3 (F1 carry-forward verification) cited `skinny/crates/xctrace_probe/Cargo.toml` alongside `skinny/crates/bbnf-bench/Cargo.toml` as the feature-propagation crosscheck path. At HEAD, `xctrace_probe` is not a separate crate; it is a binary inside `bbnf-bench`:

```bash
find skinny/crates -maxdepth 2 -name "Cargo.toml" | xargs grep -l "name = \"xctrace_probe\""
# (no matches — no xctrace_probe Cargo.toml exists)

find skinny/crates -name "*.rs" -path "*xctrace*"
# skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs

grep -n "src/bin/xctrace_probe\|xctrace_probe" skinny/crates/bbnf-bench/Cargo.toml
# (no explicit [[bin]] block — xctrace_probe is auto-detected as a bin via src/bin/ convention)
```

V2's F1 verification still substantively holds (the bench-harness Cargo.toml has no `parse-attribution` propagation row; the xctrace_probe bin builds against the same bbnf-bench feature set; the transitive form `--features runtime/parse-attribution` is still the correct unmask invocation). Only the V2 cite path is stale.

**Disposition:** non-escalation cite-hygiene. The substantive CH2 finding (F1 carry-forward) is unaffected; the V2 lens text's reference to a sibling `xctrace_probe` crate predates the bench-bin restructure but does not invalidate the verification logic. **Routed to S-P2 first-wave grooming** alongside R2 `:542 → :506` collapse and F-V2-CH2-2 `(fn @ N)` convention standardization (§4.1 below). **Not V3 REVISE-blocking.**

## §4 — S-P2 hand-off summary (CH2-binding)

### §4.1 — V3 should-do actions (none mandatory; all non-blocking; all route to S-P2 first-wave grooming)

V3 inherits the V2 §4.1 should-do list unchanged plus one V3-fresh cite-hygiene observation:

1. **V1 R2 — P1-E §2.2 `distinct_values` row cite collapse** (`generated.rs:542` → `generated.rs:506` or `:506 (fn @ 506)` per F-V2-CH2-2 convention). Definition opens at line 506; line 542 is the closing `}`. Non-blocking — CH2 primitive class `dispatch` envelope correct independent of cite drift. Carry forward.
2. **F-V2-CH2-2 — Adopt `(fn @ N)` cite-hygiene convention across all P1 artefacts** (currently only P1-A uses it). Collapses R1/R2 class of imprecision into a single cite-form decision. Carry forward.
3. **F-V2-CH2-1 — Promote `apache_builds` `parse_option_scalar_string` row** as the worked example of the CH2 cross-grammar generalization argument (the `string` primitive maps directly to CSS L4 / Sheets / BBNF-self). Carry forward.
4. **F1 — `parse-attribution` transitive feature** plumbing for any S-P2 re-capture (use `--features runtime/parse-attribution` form; `parse-attribution` is a `runtime`-crate-private feature; bench-harness does not propagate it as a default). Carry forward to S-P2 as part of F-V2-P1ABC-RERECORD packet.
5. **F2 — CSS L4 zero-evidence asymmetry** must be promoted from CH3/CSS-substrate to a CH2 sub-finding in S-P2 fold; the CH2 cross-grammar generalization argument cannot be answered empirically for CSS L4 at SK-V14 dispatch without CSS L4 profile evidence. Carry forward.
6. **F-V3-CH2-1 — V2 §1.3 stale `xctrace_probe` Cargo.toml path** refresh to reflect bench-bin restructure (`skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`). Cite-hygiene only; substantive F1 verification unaffected. New at V3; route to S-P2 first-wave grooming.

### §4.2 — V3 mandatory actions

**None.** V3 introduces no new CH2 REVISE. R1 closure intact at HEAD; R2/F1/F2 + F-V2-CH2-1/2/3 + F-V3-CH2-1 all non-blocking; route to S-P2 first-wave grooming.

### §4.3 — CH2 §3Z convergence verdict (V3 LOCK)

CH2 ACCEPT-rate trajectory: V1 100% + V2 100% + V3 100% = **100% × 3 cycles**, with zero orphan REVISE across all three cycles. The §3Z floor ("≥95% × 2 cycles, zero orphan REVISEs") is **exceeded by ≥5 pp** and discharged at **V2 → V3** as the binding "× 2 consecutive cycles" cycle. CH2 GENERALITY lens **CONVERGES at V3**; no V4 / V5 expected for this lens.

## §5 — Sources (V3-verified against HEAD = `4ad8f1949`)

### §5.1 — Binding context (read in order)

- `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH2 binding definition)
- `restart/prompts/ORCHESTRATOR.md:84,201,204` (CH2 lens registry + Lock 14 audit-per-pass binding; §3Z convergence)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md:31` (V1 CH2 focus row; V3 dispatch inherits)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH2.md` (V1 disposition; 100% ACCEPT; R1+R2+F1+F2)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH2.md` (V2 disposition; 100% ACCEPT; R1 closed; F-V2-CH2-1/2/3 introduced)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md §2.1, §2.3, §3.1` (V3 Option A pure-confirming dispatch; per-lens convergence forecast; §3Z chain LOCK)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH1.md §0.1` (BINDING refresh closure for CH1 V1 REVISE; same `F-V2-P1E-1` packet that discharged CH2 R1)

### §5.2 — V2 fold packet citations (CH2-binding; carry-forward to V3 unchanged)

- Commit `069ba203c` (V2 light micro-redispatch — five orphan REVISEs landed; touches all 6 P1 axis files; +86/-26 lines; no symbol re-record)
- Commit `4ad8f1949` (V2 hardening — adds `V2/CH*.md` + V2 consolidated; zero P1 axis file modifications)
- `F-V2-P1E-1` (BINDING; refreshes typed-plane file:line cites in P1-E §2.3 + §1.2 grep set + §5.4 sources; mechanically discharges V1 CH2 R1)
- `F-V2-P1A-MOVEMASK` (refreshes 12 movemask table-row cites with `(fn @ 4)` hygiene annotation; establishes the cite-hygiene convention precedent for F-V2-CH2-2)
- `F-V2-METHODOLOGY-1` (build_flags_regime row across P1-A/B/C/D; CH1/CH4 binding; not CH2)
- `F-V2-P1C-LINEDRIFT` (3 NEON primitive anchors + REDRESS path normalization)
- `F-V2-P1F-1` (CH5 reclassification; CH5 binding; not CH2)

### §5.3 — Artefacts disposition (V3 confirming per §0; P1 files unchanged since `069ba203c`)

- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md:131,137,141,149,177,246,251` (envelope evidence + movemask `(fn @ 4)` cite hygiene + CH2 close — V3 intact)
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md:90,155,159,161,167,275,277,289` (DirectParser::skip_value substrate-walk primitive carry-forward — V3 intact)
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md:209,224,322,337-345,479-486,605` (ANOM-4 envelope cause + parse-attribution gate + Lock-14-compliant primitive table + Cargo.toml:21 cite — V3 intact)
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md:19,45-50,82-86,92-110,116-149,219-231` (load-bearing CH2 synthesis; §1.2 extended grep + §2.3 refreshed table + §5.4 refreshed sources from V2; §2.2 row 146 carries R2 `:542` non-blocking — V3 intact)

### §5.4 — Source crosscheck (HEAD-verified per §1)

- `skinny/crates/runtime/Cargo.toml:21` (`parse-attribution = []` feature gate; F1 carry-forward target)
- `skinny/crates/bbnf-bench/Cargo.toml:18` (`runtime = { workspace = true, features = ["bench-counters"] }`; no `parse-attribution` propagation row — F1 transitive form required)
- `skinny/crates/runtime/src/grammars/json/generated.rs:45,159,164,169,187,213,466,506,650` (envelope + every cited grammar-neutral primitive in generated; unchanged from V1+V2)
- `skinny/crates/runtime/src/grammars/json/scan.rs:22,32,107,131,164` (structural scan primitives; unchanged)
- `skinny/crates/parse-that-regex/src/lib.rs:113,162,284,547,718,945,959` (whitespace, string-quote, escape-validation, plain-string skip, unescape, hex-unit, hex-nibble primitives; unchanged)
- `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4` (`fn movemask_u8x16` definition; V2 `(fn @ 4)` cite-hygiene anchor; V3 re-verified)
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:516,527,592,1150,1219,1330,2197,2949` (typed monomorphizations; V2 F-V2-P1E-1 refresh target; V3 re-verified — R1 closure intact)
- `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs` (xctrace_probe bin; V3-fresh observation per §3.1 — V2 §1.3 sibling-crate cite is vestigial)
- `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102` (sonic_rs eager-typed-DOM comparator; unchanged)
- `wc -l skinny/crates/bbnf-bench/src/generated_real_typed.rs → 3056` (file size at HEAD; unchanged from V2)

## §6 — CH2 V3 disposition (final)

**ACCEPT.** 4/4 CH2-binding artefacts (P1-A, P1-B, P1-C, P1-E) hold at 100% ACCEPT for the third consecutive cycle. V1 R1 closure (`F-V2-P1E-1`) intact at V3 HEAD per §1.1. V1 R2 + V1 F1 + V1 F2 + V2 F-V2-CH2-1 + V2 F-V2-CH2-2 + V2 F-V2-CH2-3 + V3 F-V3-CH2-1 all carry forward to S-P2 first-wave grooming; none warrant V3 REVISE escalation; none re-open any REDRESS-family route. Four-witness CH2 redundancy intact for the third consecutive cycle.

CH2 V3 §3Z convergence: **CLOSED.** 100% × 3 cycles, zero orphan REVISEs across V1/V2/V3 — the §3Z "≥95% × 2 cycles" gate is exceeded with margin and discharged at V2 → V3. CH2 GENERALITY lens **CONVERGES at V3**; no CH2 V4 / V5 work expected; S-P2 dispatch gate opens for this lens.
