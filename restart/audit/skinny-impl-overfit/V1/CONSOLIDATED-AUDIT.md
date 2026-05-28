# SK-V14 Skinny Implementation Overfit Audit — Consolidated

Date: 2026-05-26. HEAD: `8e7378025`. Cycle: V1.
Audit dispatch: 6 parallel agents across distinct overfitting axes.

## Headline

**JSON is honest. CSS L4 is contrived. Generic infrastructure is mixed-with-creep.**

| axis | verdict | weight |
|---|---|---|
| JSON-specific hardcoding (AUDIT-1) | ACCEPT-AS-PROOF-OF-CONCEPT | strong proof |
| CSS-L4-specific hardcoding (AUDIT-2) | PRUNE-REQUIRED | dispositive |
| Pattern H runtime collapse (AUDIT-3) | MIXED — 5/9 grammars are true template, 4/9 bespoke; 0/67 carry `@generated` | partial |
| Codegen/xtask Lock 14 leaks (AUDIT-4) | LOCK-14-LEAKS-PRESENT; one big axis retired, four new spawned | mixed |
| Bench/test contrivances (AUDIT-5) | MIXED — JSON measurement-valid; CSS dispositively contrived | mixed |
| Substrate/value-API (AUDIT-6) | GRAMMAR-CREEP-PRESENT at codegen+runtime; primitives clean | mixed |

## Dispositive finding (BLOCKER)

**All 24 CSS L4 "row admits" are one measurement broadcast 24 times.** Per AUDIT-5 F-1 + AUDIT-2 F-3:

- `skv14-redress-215-css-full-parse-profile.tsv` lines 2-25 show identical `track1=2319.041, lightningcss=929.281, cssparser=2362.037` repeated for 24 different conceptual feature row-ids.
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs:206-228 measure_full_parse_profiles` runs ONE aggregate loop over the combined corpus.
- `W8_SELECTED_CSS_ROWS = 24` (line 17) is a hardcoded broadcast constant.
- The `SKV13_CSS_FEATURES` list at `skinny/xtask/src/main.rs:1333-1358` projects that one number across 24 row-ids in `skinny/RESULTS.md:112-135`.
- The current gate (`xtask/main.rs:1004-1023`) enforces structural stamping but not measurement diversity, so this passes existing checks.

**Further: the CSS "full_parse" comparison is workload-mismatched.** Per AUDIT-2 F-4: `generated.rs:53-59` defines `CssFullParseSummary` with only `{rules, at_rules, qualified_rules, declarations}` counters — i.e. a brace counter. lightningcss builds a full CSSOM. cssparser in the same measurement actually beats Track 1 by ~43 Mbps and that isn't surfaced.

**Further: the "grammar-agnostic generator" for CSS is a hand-written tokeniser embedded as a 646-line string literal.** Per AUDIT-2 F-2: `skinny/crates/codegen/src/runtime_generator.rs:713-1359` `const CSS_GENERATED_RS: &str = r#"..."#`. It is NOT derived from `grammar/css/l4/*.bbnf`. The 7 CSS L4 "generated" runtime modules are byte-identical copies of this string (md5sum `8675e262...`).

**The W5C-GEN / W5D-DELETE deletion of the 7 CSS provider modules was cosmetic.** Provider files removed; their content relocated into one string literal in `runtime_generator.rs`; no grammar-source consultation.

## Other significant findings

### Pattern H NOT collapsed (AUDIT-3 F-1/F-2)

- Census = 67 (identical to pre-W6; not a single file deleted).
- 0/67 core Pattern H files carry `@generated` headers. The W2 bypass-header detector is a no-op against the core surface.
- `xtask/src/regen_simple_runtime.rs:32-38` declares FOUR runtime styles (`Simple, TypedFormula, TypedBbnf, TypedJson`) — not one template. Three are bespoke per-grammar cohorts.
- 5/9 grammars (math, csv, bnf, ebnf, css_pretty = 35/67 files) ARE true single-template instantiations — the success case.
- CSS L4 still ships a 14-variant `OpenFrame` enum (`crates/core/src/runtime/css_l4/builder.rs:14-80`) — the canonical Pattern H violation cited in LOCKS.md:349.

### Lock 14 grep gate has explicit allowlist holes (AUDIT-4 F-8)

`skinny/xtask/src/lock14_baseline.rs:2370-2379` `GENERIC_SCAN_ROOTS` deliberately excludes `runtime_generator.rs`, `grammar_provider.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, `json_templates/`. The Lock 14 gate cannot see four of AUDIT-4's six HIGH findings. This is a gate that lies by omission.

### Decision Engine is SCAFFOLD, not LOAD-BEARING (AUDIT-6 F-5/F-6)

- `DecisionCspFacts` carries grammar-named fields `static_css_provider_status` + `json_sink_only_status` (`ir/src/cost.rs:242-243`).
- `decision_csp.rs:166` literally sets `block_id = "JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT"` — a self-confession that the Decision Engine is wired but does not drive emission.
- `backend_egraph.rs:66` runs the e-graph with ZERO rewrite rules. The CSP substrate constraint is tautological.
- 4 of 5 `BackendShape` lower-impls (`eager_tape.rs`, `offset_tape.rs`, `event_tape.rs`, `collapsed_stage.rs`) are 17-LOC stubs returning `format!("rule {} -> shape_name", ...)`. Only `sink_only.rs` (270 LOC) is real, and it routes downstream to JSON-named renderers.

### No CSS value API (AUDIT-6 F-7)

`JsonValue` enum + view/visitor exists for JSON. The seven CSS L4 grammars have NO value API — `parse()` returns `Result<String, CssFactError>` of a tab-separated fact-stream. The user's gate ("perfect our parsing + value API for both CSS and JSON") is **materially unmet on CSS**.

### Bench-only JSON contrivance — quarantined but flag (AUDIT-1 F-2)

- W11L y_string_unicode admit hashes the decoded string with FNV-64 and matches a closed 11-entry table; sonic-rs and serde sidecars also deserialize into the same closed enum (`real_typed_struct.rs:942-957`), so the strict-product comparator cannot catch hash collisions.
- Same pattern in W11N (unicode_mixed) and W11O (gsoc) closed enums.
- These are isolated to `skinny/xtask/src/real_typed_schema.rs` + `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (bench-only; NOT linked into production runtime).
- Flag before this technique migrates to runtime.

### W11 TSV honesty sample (AUDIT-5)

5 of 6 sampled W11 TSVs are honest: distinct per-corpus×mode numbers with `warmup_iters=0`. W11F admirably reports 6 REJECTs with negative margins. Only the CSS L4 W8R TSV (the 24-broadcast) is contrived.

### `target-cpu=native` is mandatory for admission (AUDIT-5 F-2)

`report.rs:6553` plus 9 more sites require `RUSTFLAGS="-C target-cpu=native"`. Headline Mbps is host-tuned (Apple M5 Max NEON). Not portable to release distribution.

## Inflection-point assessment

The user's brief: *"once we perfect our parsing + value API for both CSS and JSON and >SOTA for each, we can backtrack and then generalize to be fully grammar driven. This should be done at that exactly inflection point."*

**We are not at the inflection point.**

| axis | gate | status |
|---|---|---|
| JSON parse perfected | yes — 51/51 admit; >sonic-rs strict 5-77%; honest scanner improvements | ✅ |
| JSON value API perfected | yes — `JsonValue` + view/visitor exists | ✅ |
| JSON >SOTA | yes — measurement-valid per AUDIT-5 | ✅ |
| CSS parse perfected | NO — 7 byte-identical hand-written 646-LOC tokenisers; not grammar-derived | ❌ |
| CSS value API exists | NO — `parse()` returns fact-stream string; no typed Value | ❌ |
| CSS >SOTA | DISPUTED — single measurement vs brace-counter workload; cssparser actually wins in the same row | ❌ |
| Substrate primitives grammar-neutral | yes — bbnf-simd primitive layer clean | ✅ |
| BackendShape lower-impls implemented | NO — 4/5 are 17-LOC stubs | ❌ |
| Decision Engine load-bearing | NO — block_id self-confesses scaffold | ❌ |
| Pattern H collapsed | PARTIAL — 5/9 true template; 4/9 bespoke; 0/67 carry @generated | ⚠️ |

JSON is at the inflection point. CSS is two cycles away. Substrate is one cycle away. Grammar-driven generalization is blocked on the CSS leg + Decision Engine + Pattern H discipline.

## SK-V15 entry constraints (binding input)

The SK-V15 Pass Alpha + skinny passes MUST consume the following as binding entry constraints:

### PRUNE-WAVE-A (CSS contrivance retirement)

- Collapse the 24-row CSS L4 broadcast to ONE honest row (or partition the corpus and time each feature independently).
- Replace the brace-counter `CssFullParseSummary` with a real CSS value type (start with CSSOM-equivalent rule/declaration/at-rule typed nodes).
- Restate CSS measurement against cssparser (the actual same-workload comparator); lightningcss requires a fuller CSSOM build to be a same-workload comparator.
- Retire the 646-LOC `CSS_GENERATED_RS` string literal; replace with grammar-derived emission from `grammar/css/l4/*.bbnf` (this is the W2/W5 promise unfulfilled).
- Delete the topology-pinning tests at `skinny/xtask/src/regen_css.rs:148, :164`.

### PRUNE-WAVE-B (Lock 14 gate restoration)

- Remove the `GENERIC_SCAN_ROOTS` exclusion at `skinny/xtask/src/lock14_baseline.rs:2370-2379` for `runtime_generator.rs`, `grammar_provider.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, `json_templates/`.
- Re-run the Lock 14 baseline against the full surface; expect a wave of newly-visible leaks; PRUNE each.

### PRUNE-WAVE-C (codegen leak abrogation)

- Collapse the 9 `xtask::Cmd::Regen<Grammar>` variants + 9 hardcoded match arms to the single generic `Cmd::Regen { grammar: Option<String>, ... }` already present at `skinny/xtask/src/main.rs:62-89`.
- Collapse the 4-variant `RuntimeStyle` enum to one template at `skinny/xtask/src/regen_simple_runtime.rs:32-37`.
- Collapse the 2-variant `RuntimeGenerationMode` (which is the grammar-family branch renamed: `PassCompiled=JSON`, `FrontendFacts=CSS`) at `skinny/crates/codegen/src/runtime_generator.rs`.
- Retire the 7-arm CSS L4 profile-id match at `runtime_generator.rs:114-153`.

### PRUNE-WAVE-D (Pattern H discipline)

- Every Pattern H runtime file MUST carry `// @generated by skinny bbnf-codegen; do not edit by hand.` header at line 1 (parity with the skinny twin runtime, where 43/48 already carry it).
- The 4/9 bespoke grammars (CSS L4 OpenFrame, JSON sink/scan, the other two) collapse into the single template.
- Census stays 67 (no count change) but `grep -l "@generated by skinny bbnf-codegen" crates/core/src/runtime/**/*.rs | wc -l` should return 67.

### REBUILD-WAVE-E (CSS value API)

- Author the CSS Value type (mirror `JsonValue` shape).
- Author the CSS view/visitor (mirror JSON shape).
- Re-time CSS >SOTA against a typed CSSOM workload; report honest deltas vs cssparser AND lightningcss.

### REBUILD-WAVE-F (Decision Engine activation)

- Populate the e-graph with at least one real rewrite rule (currently zero).
- Make `DecisionCspFacts` drive emission selection (not just scaffold).
- Remove `static_css_provider_status` + `json_sink_only_status` grammar-named fields; replace with grammar-neutral `provider_status` indexed by grammar-id.
- Implement the 4 stub BackendShape lower-impls (eager/offset/event/collapsed); each ≥50 LOC of actual lowering.

### REBUILD-WAVE-G (bench-only contrivance quarantine)

- The W11L/W11N/W11O FNV-64 closed-enum scheme stays bench-only. Add explicit guard: no FNV-keyed arbiter may migrate into `crates/core/src/runtime/`.
- Strengthen the strict-product comparator to detect closed-enum-aware sidecars in the differential.

## Discipline forward-lens (for the new PASS-IMPL-OVERFIT-AUDIT pass)

CH3 + CH5 + CH7 in T-P3 CHALLENGE need strengthening. Three procedural addenda:

- **NEW-CH3-V5-01: wave-graph cycle detection.** Per V3 W2R + V4 W4R precedent: every spec amendment that mandates deletion of artefact X in wave N MUST verify that the rebuild capability for X is delivered no later than wave N. Grep pattern: "delete X" + "regen X exists" / "X is generated by wave M ≤ N".
- **NEW-CH5-V5-02: broadcast-admission detection.** Per AUDIT-5 F-1: when N rows admit on a measurement, verify the TSV produces N distinct rows of measurement data, not 1 row broadcast N times.
- **NEW-CH7-V5-03: gate exclusion detection.** Per AUDIT-4 F-8: every Lock 14 / Lock 16 grep gate must scan its own exclusion list and surface anything in the exclusion list as a finding (not silently passed).

## Trajectory summary

| cycle | JSON | CSS L4 | Pattern H | Lock 14 | Decision Engine |
|---|---|---|---|---|---|
| pre-restart | overfit | overfit | 67 hand | leaks | nonexistent |
| SK-V14 (current) | honest >SOTA | contrived 24-broadcast | 67 hand, partial template | mixed, gate excluded | scaffold |
| SK-V15 (PRUNE) | minor bench-only flag | retire contrivance + value API + grammar-derived | @generated headers + collapse 4 bespoke | restore full scan | populate e-graph + impl 4 stubs |
| SK-V16 (REBUILD) | maintain | re-time honest >SOTA vs CSSOM | (collapsed) | (clean) | (load-bearing) |

Inflection point falls at SK-V16 close if SK-V15 PRUNE-WAVEs A-G all admit.
