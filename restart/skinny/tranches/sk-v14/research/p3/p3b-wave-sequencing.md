# SK-V14 P3-B: Wave Sequencing

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-23.
Scope: order the S-P2 LOCKED candidate pool (V3 §3Z COHORT LOCK; HEAD `ebe84954b1a6c31bb6183ca8f5e68d88647d9df7`) + the SYNTHESIS §3 C-1..C-5 PRUNE candidates into waves W0..W{n≤12}, honoring `[build-infra-first]` W0-baseline + the three binding S-P0 architectural sequencing constraints + the three S-P2 §6 carry-forward packets.
Output: this file (`restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md`).
Pass Alpha goalset: SYNTHESIS §0.1 R10 close-condition — every JSON cell (51 = 17 × 3) + every CSS L4 feature (24 non-OUT_OF_SCOPE) ADMITs > strict-vs-strict OR carries architectural-block proof; R1-R10 acceptance criteria at SYNTHESIS §0.3; current baseline AUDIT-ZERO at SYNTHESIS §0.2 (0/17 parse_only, 0/17 direct, 0/17 typed, 0/24 CSS L4).
Candidate pool: SK-V14 SYNTHESIS §3 C-1..C-5 (the Pass-Alpha-bound load-bearing candidates) + S-P2 V3-LOCKED axis cohort (P2-A 7 SOTA, P2-C 5 active + 3 demoted, P2-D 2 active + 1 demoted + 1 pre-blocked, P2-E 9 Layer-1 gaps, P2-F 13 active + 1 demoted) consumed as SUBSTRATE for the re-admit waves W9-W11, never as standalone SK-V14 implementation waves.

---

## §1 — Synthesis

### §1.1 — Binding inputs converged at this fold

The wave-sequencing problem has four binding inputs that jointly determine the topology:

1. **SYNTHESIS §3 candidate slate (C-1..C-5)** — the five Pass-Alpha-authorised load-bearing candidates (`restart/skinny/tranches/sk-v14/SYNTHESIS.md:264-280`):
   - C-1 = R3 PRUNE-3 + PRUNE-4 (Lock-14 refactor cluster; 2.8k-3.4k LOC; VERY HIGH risk; architectural / multi-wave).
   - C-2 = R1 + R2 (three plane-correct strict comparators + per-iter equality oracle; 600-1.08k LOC; HIGH risk; harness + comparator surface).
   - C-3 = R4 + R5 (`cargo xtask regen-css` + `skinny/corpora/css-l4-sk-v14/` ≥800 KB; 1.2k-2.0k LOC; HIGH risk; xtask + corpora + dual-tree round-trip).
   - C-4 = R3 PRUNE-5 (W8 + W9 SCAFFOLD → LOAD-BEARING; CSP-chosen shape produces measurable runtime divergence; 800-1.4k LOC; VERY HIGH risk; Lock-1 substrate-ceiling surface).
   - C-5 = R3 PRUNE-1 + PRUNE-2 (clean revert of fake admits; 250-500 LOC; MED-LOW risk; revert + REDRESS scribe).

2. **Three binding architectural sequencing constraints** (S-P0 SYNTHESIS-AUDIT-OVERFIT §2.1-§2.3; binding on the S-P3 wave manifest verbatim):
   - **§2.1 R4 BEFORE PRUNE-2** — without R4, PRUNE-2's deletion of 7 hand-written CSS template directories + 7 provider modules + 7 runtime twins leaves the 24 CSS L4 ADMITTED rows permanently unrecoverable (no `cargo xtask regen-css` subcommand exists at HEAD to regenerate from `grammar/css/l4/*.bbnf`).
   - **§2.2 C-1 (PRUNE-3 + PRUNE-4) BEFORE C-4 (PRUNE-5)** — wiring W8 / W9 into the current 8 per-grammar `RuntimeProvider::*` match-arm mesh at `skinny/crates/codegen/src/lib.rs:167-209` would re-deepen the Lock-14 violation PRUNE-3 is dispatched to remediate. C-1 → C-4 is structural, not nominal.
   - **§2.3 PRUNE-4 = 9 sub-waves, NOT 8** — per A3/A5/A6 cross-confirmed `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l = 9` (`bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math`); the `css_pretty` directory is the +1 over the SK-V13 baseline.

3. **Three S-P2 §6 carry-forward packets** (HARDENING-S-P2-V3-CONSOLIDATED.md:466-529; binding on S-P3 wave-program admission discipline):
   - **CF-3 / 3-gate CH4 admission cell** — every shortlisted candidate's admission manifest carries (scalar-ref status / checkasm-parity expectation / same-wave-consumer NAMED). Promotes from S-P2 §4 documentation-discipline to S-P3 wave-program admission-gate.
   - **§2.Y NF-CH6-4 canonical-name binding** — ONE canonical primitive name + ONE canonical scalar-ref function across the three convergent identifiers (P2-A C2 `long_string_body_simd_scan` / P2-E Gap 1 `scan_string_special_block_sweep_64` / P2-F C1+C2 quote-aware classifier composition), all grounded on the `unescape_string` direct rank-1 46.7 % `unicode_escapes` hot-leaf (P1-E §2.2). Binds R6/R7/R8 admission, never three orthogonal SIMD bodies.
   - **F-V2-P1ABC-RERECORD Stage-0 wave commitment** — cargo build `--features runtime/parse-attribution` + interactive samply record (NOT `--save-only` per `[samply-symbol-resolution]`) + cfg_attr flip verification at 8 sites in `generated.rs:33-237` (lines `33-34, 43-44, 58-59, 79-80, 86-87, 117-118, 138-139, 157-158`). MUST ship as Stage 0 of any wave admitting any of the 12 consumer-dependency primitives (P2-A C6 + P2-C C-P2C-3/-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13). Per `[no-deferrals]`.

4. **CH4 wave-count + same-wave-consumer ceilings** per PASS-3-SYNTHESIS-PLAN §3 CH4 + ORCHESTRATOR §3Z + `[no-deferrals]`:
   - ≤ 12 waves total (skinny-bracket ceiling).
   - Every primitive lands WITH its hot-path consumer in the same commit (`[execute-planned-architecture]` + SYNTHESIS §4 "support-only landings are invalid").
   - W0 always = baseline + telemetry lock (per `[build-infra-first]`).

### §1.2 — Sequencing derivation (substrate before consumer; guard before risk)

Per PASS-3-SYNTHESIS-PLAN §2 P3-B row ("Topological — substrate before consumer; guard rows before risk rows") and SK-V8 SPEC §2 (the SPEC shape verbatim — W0 baseline, W1 cost/comparator, behavior waves conditional, single-substrate enforcement), the topological order resolves:

**Phase 1 — TELEMETRY + COMPARATOR (W0-W1).** No behavior wave admits before the telemetry gate is the consumer of every emitted row column and the comparator is rebound to strict-vs-strict per plane. W0 = SK-V14-open baseline capture + telemetry-schema lock (the SK-V8 SPEC §3 shape, extended with the 4 SK-V14-NEW columns `comparator_plane` / `per_iter_equality` / `audit_overlay_verdict` / `track2_entry_point` per SYNTHESIS §2). W1 = C-2 R1+R2 (three plane-correct strict comparators wired + per-iter equality oracle inside the timing region + `gate-json` rejection of empty equality column).

**Phase 2 — PRUNE-1 (W2).** C-5 PRUNE-1 reverts the 22 AUDIT-FALSIFIED JSON ADMITTED rows in `ROLLING-SOTA-DELTA.md` + `RESULTS.md` (5 parse_only W14.1-5 + 6 direct + 11 typed under the broader ledger per SYNTHESIS §0.2 reconciliation block). PRUNE-1 is BEFORE PRUNE-2 because it has no upstream substrate dependency (it is a documentation revert + REDRESS scribe per row citing v2 §1-4); landing it under the rebound W1 comparator allows the post-revert `audit_overlay_verdict` column to be authoritatively populated by `gate-json` per SYNTHESIS §2 audit overlay binding.

**Phase 3 — R4 SUBSTRATE (W3).** C-3 R4 lands `cargo xtask regen-css` consuming the 15 `.bbnf` files at `/grammar/css/l4/` and emitting CSS L4 runtime modules with round-trip-clean discipline (`rm -rf generated && cargo xtask regen-css && git diff` empty). R4 BEFORE PRUNE-2 is constraint §2.1 verbatim.

**Phase 4 — PRUNE-2 (W4).** C-5 PRUNE-2 deletes the 7 hand-written CSS template directories + 7 provider modules + 7 runtime twins + reverts the 24 CSS L4 ADMITTED rows; the deletion is recoverable because R4's emission path is live. PRUNE-2's exit gate verifies that every post-revert CSS L4 row resolves through the R4 emission path (no `include_str!()` of hand-written template), and that the fake `@generated` header detector returns empty (`git grep -l '@generated by skinny bbnf-codegen' -- skinny/crates/runtime crates/core/src/runtime` traces every match to a registered xtask emission per `[clean-regen-discipline]`).

**Phase 5 — R5 CORPORA (W5).** C-3 R5 lands `skinny/corpora/css-l4-sk-v14/` with Bootstrap + Tailwind + Material + Animate (~960 KB), gated by `du -sh skinny/corpora/css-l4-sk-v14 > 800 KB` per SYNTHESIS §3 C-3 falsifiability + `[no-deferrals]` (no embedded tiny fixtures). R5 BEFORE the re-admit waves R6/R7/R8 because R6's per-feature parity row requires ≥ 800 KB working-set per pre-block P-3.

**Phase 6 — PRUNE-3 (W6).** C-1 PRUNE-3 replaces `RuntimeProvider` enum at `skinny/crates/codegen/src/lib.rs:167-209` with trait-based dispatch, collapses 8 per-grammar provider modules under `codegen/src/` into ONE grammar-agnostic generator template consuming grammar source + workspace metadata, and migrates bbnf/grammar/passes JSON-rooted symbols to per-grammar facades. The forward invariant per SYNTHESIS §3 C-1 — `find skinny/crates -name '*.rs' | xargs grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar'` returns ZERO post-redress — is gate-enforced.

**Phase 7 — PRUNE-4 (W7).** C-1 PRUNE-4 refactors 67 hand-written per-grammar runtime files in `crates/core/src/runtime/{bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math}/` into emitted output. PRUNE-4 ships as ONE top-level wave manifesting 9 sub-passes (one per grammar directory); the ≤12 wave ceiling does not permit 9 separate top-level waves. The per-grammar discipline per `[clean-regen-discipline]` requires the wave's exit gate to verify ZERO per-grammar dirs at `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` plus the forward invariant (any new grammar produces ZERO new `.rs` files in `skinny/crates/{codegen, runtime, passes, bbnf, grammar}/src/`; Lock 14 baseline gate rejects any commit that violates this).

**Phase 8 — PRUNE-5 (W8).** C-4 PRUNE-5 wires W8 (per-grammar policy) + W9 (same-substrate union) from gate-layer-only (3 files / 20 references confirmed by audit A5 NEW-MED — `bbnf-bench/src/{bin/gate.rs, lock14_baseline.rs, report.rs}` only; zero matches in `passes/`, `codegen/`, `runtime/`, `ir/`) to LOAD-BEARING in compile + lower + runtime. Same-wave consumer per SYNTHESIS §3 C-4: named pre-wave row `json/numbers/direct_to_struct/main` shows pre-wave hot-leaf `parse_value_at`, post-wave hot-leaf names the W11.1 number-specialised symbol explicitly in samply trace; per-shape Lock-1 triad (`substrate_target`, `retention_lifetime`, `policy_owner`) declared in REDRESS per `LOCKS.md:73-82`. PRUNE-5 AFTER PRUNE-3 + PRUNE-4 is constraint §2.2 verbatim.

**Phase 9 — RE-ADMIT WAVES (W9-W11).** R6/R7/R8 are the consumer waves for C-1..C-5; per SYNTHESIS §3 closing pin "R6 / R7 / R8 (re-admit waves) are downstream CONSUMERS of C-1 through C-5". W9 = R6 CSS L4 24-feature re-admit through R4 emission + R5 corpora + lightningcss/cssparser strict equality. W10 = R7 JSON direct + typed re-admit through rebound C-2 comparators + post-PRUNE substrate. W11 = R8 JSON parse_only distinct path (stand up distinct parse_only code path in `generated_json` with no full-tape build per SYNTHESIS R8) wired to sonic-rs Skipper-class comparator. **F-V2-P1ABC-RERECORD Stage 0 ships in W9** (the first re-admit wave admitting any envelope-internal primitive consumer); per CH6 V3 §1.6 the dual-gate (CH2 measurability + CH4 cost-discriminator) is the binding inheritance.

The 12-wave total (W0..W11) is at the §3Z ceiling. The re-admit waves W9-W11 admit candidates from the S-P2 LOCKED axis pool (P2-A C1-C7, P2-C C-P2C-2/-3/-4/-5/-8, P2-D C-P2D-1/-2, P2-E Gaps 1-9, P2-F C1-C7/C9-C14 minus C8) under the §2.Y canonical-name binding, the CF-3 3-gate admission cell, and the F-V2-P1ABC-RERECORD Stage 0 commitment — never as standalone implementation waves outside the R6/R7/R8 envelope.

---

## §2 — Deliverable (wave manifest)

Per the mandatory per-wave output schema (Wave ID; Entry gate; Owner-path family; Conditional-dispatch status; Hard cap LOC + minute; Primary candidate(s) from S-P2 pool; Same-wave consumer NAMED; Topological dependencies). All 12 waves total. The SPEC shape mirrors SK-V8 SPEC §2 verbatim per PASS-3-SYNTHESIS-PLAN §8.1.

### §2.1 — Wave manifest (W0..W11)

| Wave | Section | Name | Initial dispatch status | Source/edit LOC budget | Implementation/redress cap | Same-wave consumer (NAMED) |
|---|---|---|---|---|---:|---|
| W0 | §2.3 | Baseline Profile + Telemetry Lock | Dispatchable only after G-Omega + S-P0 convergence (per SYNTHESIS §0.4 P-blocks + ORCHESTRATOR-PROMPT.md) | 0 production behavior LOC; ≤120 telemetry-gate/report/Lock-14-baseline LOC | ≤90 min | `xtask gate-json` rejects rows missing required columns per SYNTHESIS §2 |
| W1 | §2.4 | C-2 Comparator Rebind + Per-Iter Equality Oracle (R1 + R2) | Conditional on W0 close | 600-1.08k source/test LOC per SYNTHESIS §3 C-2 envelope | ≤90 min | `xtask gate-json` rejects rows whose `per_iter_equality` column is empty + rows whose `comparator_plane` does work asymmetric to Track 1 |
| W2 | §2.5 | C-5 PRUNE-1 (Revert 22 AUDIT-FALSIFIED JSON ADMIT rows) | Conditional on W1 close | ≤250 revert + REDRESS scribe LOC (subset of C-5 250-500 envelope) | ≤90 min | `audit_overlay_verdict` column populated per row by `gate-json` against `audit-overfit/validation/v2 §1-4` cite; ROLLING-SOTA-DELTA rebases to audit-zero in same commit |
| W3 | §2.6 | C-3 R4 (`cargo xtask regen-css` pipeline) | Conditional on W1 close (independent of W2; concurrent-eligible per SYNTHESIS §4 agent-orchestration if file domains disjoint) | 1.0k-1.5k xtask + emission scaffolding LOC (subset of C-3 1.2k-2.0k envelope) | ≤90 min | Round-trip xtask check: `rm -rf skinny/crates/runtime/src/grammars/css_l4_*  crates/core/src/runtime/css_l4/ && cargo xtask regen-css && git diff` empty on both runtime trees |
| W4 | §2.7 | C-5 PRUNE-2 (Delete 7 CSS templates + revert 24 CSS rows) | Conditional on W3 close (constraint §2.1 R4-BEFORE-PRUNE-2) + W2 close | ≤250 delete + revert + REDRESS scribe LOC (balance of C-5 envelope) | ≤90 min | Fake-`@generated` detector returns empty post-revert (`git grep -l '@generated by skinny bbnf-codegen' -- skinny/crates/runtime crates/core/src/runtime` traces every match to a registered xtask emission); `audit_overlay_verdict` populated per row against v1 §1-6 |
| W5 | §2.8 | C-3 R5 (`skinny/corpora/css-l4-sk-v14/` production corpora ~960 KB) | Conditional on W3 close (concurrent-eligible with W4 per file-domain disjointness — W4 touches `runtime/src/grammars/css_l4_*` + `codegen/src/css_l4_*_templates`; W5 touches `corpora/`) | 200-500 corpus-curation + bench-wiring LOC (subset of C-3 1.2k-2.0k envelope; corpora bytes do NOT consume LOC budget per SK-V8 SPEC §2 "generated outputs do not consume the source LOC budget") | ≤90 min | `du -sh skinny/corpora/css-l4-sk-v14` > 800 KB; bench rows wired to new corpora consume them in same wave |
| W6 | §2.9 | C-1 PRUNE-3 (Trait-dispatch + 8-provider collapse) | Conditional on W0-W5 close | 700-900 source LOC (subset of C-1 2.8k-3.4k envelope) | ≤90 min | `find skinny/crates -name '*.rs' \| xargs grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar'` returns ZERO post-redress; Lock-14 baseline gate rejects any commit that re-introduces a `RuntimeProvider::*` match arm |
| W7 | §2.10 | C-1 PRUNE-4 (9 per-grammar runtime sub-passes) | Conditional on W6 close (constraint §2.2 C-1 sequencing) | 2.1k-2.5k source LOC (balance of C-1 2.8k-3.4k envelope; 9 sub-passes folded under single wave manifest per §1.2 Phase 7 + ≤12 ceiling) | ≤90 min per sub-pass × 9 = ≤810 min cumulative cap; sub-pass split-before-dispatch if any single sub-pass overflows the 90-min cap per SK-V8 SPEC §2 | `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns ZERO post-redress; forward invariant (any new grammar produces ZERO new `.rs` files in `skinny/crates/{codegen, runtime, passes, bbnf, grammar}/src/`) is gate-enforced by Lock-14 baseline |
| W8 | §2.11 | C-4 PRUNE-5 (W8/W9 SCAFFOLD → LOAD-BEARING) | Conditional on W6 + W7 close (constraint §2.2 C-1-BEFORE-C-4) | 800-1.4k source LOC per SYNTHESIS §3 C-4 envelope | ≤90 min | Named pre-wave row `json/numbers/direct_to_struct/main`: post-wave hot-leaf names W11.1 number-specialised symbol explicitly in samply trace; per-shape Lock-1 triad (`substrate_target`, `retention_lifetime`, `policy_owner`) declared in REDRESS; CSP-emitted `BackendShape` dispatch (no `match grammar { Json => ..., CssL4 => ... }` arm in dispatch path) |
| W9 | §2.12 | R6 CSS L4 24-feature re-admit (LOAD-BEARING; first re-admit wave) | Conditional on W0-W8 close | ≤900 source/test/REDRESS LOC (consumer wiring only; primitives drawn from S-P2 LOCKED pool) | ≤90 min | Per-feature parity row gates on `lightningcss Mbps` + `cssparser_oracle Mbps` strict equality + `Δ vs SOTA ≥ +1 Mbps` per SYNTHESIS §0.1; F-V2-P1ABC-RERECORD Stage 0 ships in this wave's Stage 0 |
| W10 | §2.13 | R7 JSON direct + typed re-admit (LOAD-BEARING) | Conditional on W0-W9 close | ≤900 source/test/REDRESS LOC (consumer wiring; primitives drawn from S-P2 LOCKED pool, never re-authored) | ≤90 min | Per-corpus `direct_to_struct` + `real_typed_struct` row gates on rebound C-2 comparators (`sonic-rs strict struct deser per corpus` direct; `per-corpus typed struct deser` typed) + `Δ vs comparator-strict ≥ +1 Mbps` |
| W11 | §2.14 | R8 JSON parse_only distinct path + admit (LOAD-BEARING) | Conditional on W0-W10 close | ≤700 source/test/REDRESS LOC (parse_only path stand-up + Skipper-class consumer; primitives drawn from S-P2 LOCKED pool) | ≤90 min | Per-corpus `parse_only` row gates on `sonic-rs Skipper-class` strict + `Δ vs Skipper-strict ≥ +1 Mbps`; distinct parse_only path in `generated_json` with no full-tape build (audit-checkable via tape-allocation telemetry) |

LOC budgets are conjunctive with the 90-minute cap and rerun ceilings per SK-V8 SPEC §2. Generated outputs (regen-css output; xtask-emitted CSS L4 runtime modules; PRUNE-4 emitted per-grammar runtimes) do NOT consume source LOC budget. A wave plan that exceeds either its LOC budget or the 90-min cap must split before dispatch or return REVISE per SK-V8 SPEC §2.

### §2.2 — Owner-path families per wave (binding for triumvirate dispatch envelope)

| Wave | Owner-path family |
|---|---|
| W0 | `skinny/crates/bbnf-bench/src/{bin/gate.rs, gate.rs, report.rs, lock14_baseline.rs}`, `skinny/xtask/src/main.rs`, `skinny/RESULTS.md`, `skinny/ROLLING-SOTA-DELTA.md` (re-baseline), `restart/skinny/tranches/sk-v14/research/wave-0-*.md` |
| W1 | `skinny/crates/bbnf-bench/{benches/json_parity.rs, src/}`, per-corpus typed-binding stubs at `…/real_typed_struct.rs:695-727`, comparator-plane wiring in `xtask gate-json`, `skinny/RESULTS.md` (telemetry-column populate) |
| W2 | `skinny/RESULTS.md` (revert W14.1-5 + W13.* + W15.1), `skinny/ROLLING-SOTA-DELTA.md` (revert + rebase to audit-zero), `skinny/REDRESS.md` (per-row entries citing `audit-overfit/validation/v2 §1-4`) |
| W3 | `skinny/xtask/src/{main.rs, regen_css/*}` (NEW subcommand), `skinny/crates/codegen/src/css_l4_*_templates/` (consumed by xtask), `grammar/css/l4/*.bbnf` (15 files; xtask input), emission targets at `skinny/crates/runtime/src/grammars/css_l4_*` + `crates/core/src/runtime/css_l4/` (dual-tree), `xtask check-css-l4-*` companion subcommand per S-P0 §2.4 round-trip-pairing extension |
| W4 | `skinny/crates/codegen/src/css_l4_*_templates/` (DELETE 7 dirs), `skinny/crates/runtime/src/grammars/css_l4_*/` (DELETE 7 hand-written twins), `skinny/RESULTS.md` (revert 24 CSS L4 rows), `skinny/ROLLING-SOTA-DELTA.md` (revert CSS L4 ledger), `skinny/REDRESS.md` (per-row entries citing `audit-overfit/validation/v1 §1-6`) |
| W5 | `skinny/corpora/css-l4-sk-v14/` (NEW; Bootstrap + Tailwind + Material + Animate; ≥800 KB working set), `skinny/crates/bbnf-bench/benches/css_l4_parity.rs` (corpus wiring) |
| W6 | `skinny/crates/codegen/src/lib.rs:167-209` (`RuntimeProvider` enum → trait dispatch), `skinny/crates/codegen/src/{json_provider.rs, css_l4_*_providers/}` (8 provider modules → 1 generic generator template), `skinny/crates/bbnf/src/`, `skinny/crates/grammar/src/`, `skinny/crates/passes/src/lib.rs:476-478` (JSON-rooted symbol facade migration), Lock-14 baseline gate update at `skinny/crates/bbnf-bench/src/lock14_baseline.rs` |
| W7 | `crates/core/src/runtime/{bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math}/*.rs` (67 hand-written files → emitted output; 9 sub-passes one-per-directory), `skinny/crates/runtime/src/grammars/` mirror, `skinny/xtask/src/regen_*/` (per-grammar subcommands; companion `check-*` per S-P0 §2.4) |
| W8 | `skinny/crates/ir/src/cost.rs` (W8 per-grammar policy → CostFacts consumer), `skinny/crates/passes/src/` (W9 same-substrate union → resolver consumer), `skinny/crates/codegen/src/lib.rs` (CSP-emitted `BackendShape` dispatch; no grammar-name arm in dispatch path), `skinny/crates/runtime/src/grammars/json/generated.rs` (W11.1 number-specialised symbol emission), `skinny/RESULTS.md` (`json/numbers/direct_to_struct/main` hot-leaf attribution flip), `skinny/REDRESS.md` (per-shape Lock-1 triad declaration) |
| W9 | F-V2-P1ABC-RERECORD Stage 0: `skinny/crates/runtime/src/grammars/json/generated.rs:33-237` cfg_attr flip verification at 8 sites; `samply record` interactive (NOT `--save-only`); `cargo build --release -p bbnf-bench --features runtime/parse-attribution`. Then: `skinny/crates/runtime/src/grammars/css_l4_*/` (post-W3 R4 emission targets), per-feature consumer wiring drawing from S-P2 LOCKED pool (P2-F C1 structural classify; P2-C C-P2C-1 ascii_set_member64_css_delimiter post-PRUNE-2 successor wiring; P2-F C5 string-block 64-byte oracle), `skinny/RESULTS.md` (24 CSS L4 row admits), `lightningcss` + `cssparser` strict comparator integration |
| W10 | `skinny/crates/runtime/src/grammars/json/generated.rs` (P2-A C1 `lazy_field_skip_with_index` consumer wiring at `parse_object_value_at_direct` + `DirectParser::skip_value`; P2-C C-P2C-4 `tbl_tbx_escape_decode_batch` JSON `\uXXXX` wiring; P2-D C-P2D-1 `BackendShape::SinkOnly` activation), `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (typed admit rows; per-corpus typed-struct binding), `skinny/RESULTS.md` (per-corpus direct + typed admits) |
| W11 | `skinny/crates/runtime/src/grammars/json/generated.rs` (NEW parse_only distinct path; no full-tape build; consumer for `scan_structurals` positions Vec), `skinny/crates/bbnf-bench/benches/json_parity.rs` (Skipper-class comparator integration; either wrap `LazyObject`/`LazyValue` traversal to skip-all-fields or measure `serde_json::Deserializer::from_slice::<IgnoredAny>` strict baseline per P2-A §1.4), `skinny/RESULTS.md` (17 parse_only row admits) |

### §2.3 — W0 details (Baseline Profile + Telemetry Lock)

**Entry gate.** G-Omega closed by user per SK-V14 ORCHESTRATOR-PROMPT.md; S-P0 OVERFIT-AUDIT converged per SYNTHESIS §0.5; S-P1 + S-P2 cohort LOCKs achieved per HARDENING-S-P{1,2}-V*-CONSOLIDATED.md §3Z LOCKs; `skinny/RESULTS.md` is the SK-V13 close baseline; W0 plan names the `SK-V14-open` capture method and no-behavior-change proof.

**Owner-path family.** `skinny/crates/bbnf-bench/`, `skinny/xtask/src/`, `skinny/RESULTS.md`, `skinny/ROLLING-SOTA-DELTA.md` (re-baseline to §1.3 honest delta per SYNTHESIS §2), `restart/skinny/tranches/sk-v14/research/wave-0-*.md`.

**Conditional-dispatch status.** LOAD-BEARING (no SCAFFOLD-ONLY rows; per `[no-deferrals]` every emitted telemetry field MUST be consumed by `gate-json` in the same wave; producer-only telemetry rejects the wave per SK-V8 SPEC §0.4).

**Hard cap.** ≤120 telemetry-gate/report/Lock-14-baseline LOC; ≤90 min implementation cap.

**Primary candidate.** SYNTHESIS §2 telemetry-schema binding + audit-overlay column (`audit_overlay_verdict` ∈ {AUDIT-FALSIFIED, AUDIT-SUSTAINED, AUDIT-PENDING}); SK-V8 SPEC §0.4 24-column schema carry-forward + the 4 SK-V14-NEW columns (`comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`).

**Same-wave consumer NAMED.** `xtask gate-json` consumes every emitted telemetry field and rejects malformed/missing evidence in the same W0 slice per SYNTHESIS §2.

**Topological dependencies.** None upstream (W0 is the bracket entry wave); blocks every downstream wave.

### §2.4 — W1 details (C-2 Comparator Rebind + Per-Iter Equality Oracle)

**Entry gate.** W0 admitted; `SK-V14-open` telemetry exists for every current main row.

**Owner-path family.** `skinny/crates/bbnf-bench/benches/json_parity.rs`, `skinny/crates/bbnf-bench/src/`, per-corpus typed-binding stubs.

**Conditional-dispatch status.** LOAD-BEARING (`gate-json` enforces strict-vs-strict per plane; rejects empty `per_iter_equality` column; rejects asymmetric `comparator_plane` work).

**Hard cap.** 600-1.08k LOC; ≤90 min.

**Primary candidate.** C-2 (SYNTHESIS §3) = R1 + R2: parse_only → sonic-rs Skipper-class (structural-skip iteration); direct → sonic-rs strict struct deser per corpus; typed → per-corpus typed struct deser. Per-iter equality on EACH bench iter inside the timing region (not startup-only); equality-pass column per iter.

**Same-wave consumer NAMED.** `xtask gate-json` rejects any row whose equality column is empty + any row whose comparator does work asymmetric to Track 1 per SYNTHESIS §2.

**Topological dependencies.** W0 (telemetry schema). Blocks W2 (PRUNE-1 revert audit-overlay binding) + all downstream.

### §2.5 — W2 details (C-5 PRUNE-1)

**Entry gate.** W1 admitted; rebound comparators + per-iter equality oracle live.

**Owner-path family.** `skinny/RESULTS.md`, `skinny/ROLLING-SOTA-DELTA.md`, `skinny/REDRESS.md`.

**Conditional-dispatch status.** LOAD-BEARING (REDRESS scribe is the consumer; ROLLING-SOTA-DELTA rebases to audit-zero in same commit set).

**Hard cap.** ≤250 LOC (revert + REDRESS scribe; subset of C-5 250-500 envelope); ≤90 min.

**Primary candidate.** C-5 PRUNE-1 (SYNTHESIS §3) = revert W14.1-W14.5 + the W13.* + W15.1 admit rows (broader 22-row population per SYNTHESIS §0.2 reconciliation: 5 parse_only + 6 direct + 11 typed); REDRESS per row cites `audit-overfit/validation/v2 §1-4`.

**Same-wave consumer NAMED.** `audit_overlay_verdict` column populated per row by `gate-json` (post-revert reads AUDIT-FALSIFIED with the v2 §reference cite); ROLLING-SOTA-DELTA shows JSON `parse_only` 0/17, `direct_to_struct` 0/17, `real_typed_struct` 0/17 per SYNTHESIS §1.3.

**Topological dependencies.** W1 (comparator/equality binding for audit-overlay authority). Concurrent-eligible with W3 (disjoint file domains). Blocks none (PRUNE-1 is documentation revert; downstream consumers need W3+W4 for CSS L4 closure).

### §2.6 — W3 details (C-3 R4 regen-css xtask)

**Entry gate.** W1 admitted; W3 plan names exact xtask subcommand, .bbnf input set (15 files at `/grammar/css/l4/`), emission targets (dual-tree: `skinny/crates/runtime/src/grammars/css_l4_*` + `crates/core/src/runtime/css_l4/`), round-trip discipline per `[clean-regen-discipline]`.

**Owner-path family.** `skinny/xtask/src/main.rs` (NEW `regen-css` + `check-css-l4-*` subcommands per S-P0 §2.4 round-trip pairing extension), `skinny/xtask/src/regen_css/`, `skinny/crates/codegen/src/css_l4_*_templates/`.

**Conditional-dispatch status.** LOAD-BEARING (xtask emission becomes the only path; no `include_str!` of hand-written template post-W4).

**Hard cap.** 1.0k-1.5k LOC; ≤90 min.

**Primary candidate.** C-3 R4 (SYNTHESIS §3) = `cargo xtask regen-css` consuming the 15 `.bbnf` files at `/grammar/css/l4/`; emits CSS L4 runtime modules; round-trip clean (`rm -rf generated → run xtask regen-css → diff empty`).

**Same-wave consumer NAMED.** Round-trip xtask check on both runtime trees + `check-css-l4-*` companion subcommand wired to CI invocation reading emitted bytes, re-running `regen-css`, and diffing.

**Topological dependencies.** W1 (telemetry/comparator). Constraint §2.1 R4-BEFORE-PRUNE-2 makes this BLOCKING for W4. Concurrent-eligible with W2 (disjoint file domains).

### §2.7 — W4 details (C-5 PRUNE-2)

**Entry gate.** W3 admitted (R4 emission path live); W2 admitted (JSON ledger reverted); W4 plan names exact deletion set (7 hand-written CSS template directories + 7 provider modules + 7 runtime twins) + 24 CSS L4 admit-row revert manifest.

**Owner-path family.** `skinny/crates/codegen/src/css_l4_*_templates/` (DELETE), `skinny/crates/runtime/src/grammars/css_l4_*/` (DELETE), `skinny/RESULTS.md` (revert 24 rows), `skinny/ROLLING-SOTA-DELTA.md` (revert CSS ledger), `skinny/REDRESS.md` (per-row entries).

**Conditional-dispatch status.** LOAD-BEARING (deletion is recoverable through W3 R4 emission; fake-`@generated` header detector enforced post-deletion).

**Hard cap.** ≤250 LOC (delete + revert + REDRESS scribe; balance of C-5 250-500 envelope); ≤90 min.

**Primary candidate.** C-5 PRUNE-2 (SYNTHESIS §3) = delete 7 CSS hand-written template files + their `include_str!`'d `generated.rs` + revert 24 CSS L4 admitted rows; REDRESS per row cites `audit-overfit/validation/v1 §1-6`.

**Same-wave consumer NAMED.** Fake-`@generated` header detector empty per `[clean-regen-discipline]`; `audit_overlay_verdict` populated per row against v1 §1-6; `ROLLING-SOTA-DELTA.md` shows CSS L4 0/24 per SYNTHESIS §1.3.

**Topological dependencies.** W3 (R4) BLOCKING per constraint §2.1; W2 (ledger consistency). Concurrent-eligible with W5 (disjoint file domains — W4 = `runtime/src/grammars/css_l4_*` + `codegen/src/css_l4_*_templates`; W5 = `corpora/`).

### §2.8 — W5 details (C-3 R5 production corpora)

**Entry gate.** W3 admitted (regen-css emission live so corpus parsing is exercisable); W5 plan names corpus sources (Bootstrap + Tailwind + Material + Animate; ≥800 KB working set per pre-block P-3 ≥800 KB threshold) + bench wiring.

**Owner-path family.** `skinny/corpora/css-l4-sk-v14/`, `skinny/crates/bbnf-bench/benches/css_l4_parity.rs`.

**Conditional-dispatch status.** LOAD-BEARING (corpora consumed by bench in same wave; `du -sh` gate enforced).

**Hard cap.** 200-500 LOC (curation + bench wiring; corpora bytes don't consume LOC budget per SK-V8 SPEC §2); ≤90 min.

**Primary candidate.** C-3 R5 (SYNTHESIS §3) = `skinny/corpora/css-l4-sk-v14/` with Bootstrap + Tailwind + Material + Animate (~960 KB).

**Same-wave consumer NAMED.** Bench rows in `css_l4_parity.rs` measure parse_only + direct + typed on the new corpora; `du -sh skinny/corpora/css-l4-sk-v14 > 800 KB` is gate-asserted.

**Topological dependencies.** W3 (R4 emission). Concurrent-eligible with W4 (disjoint file domains). Blocks W9 (R6 CSS L4 admit) per pre-block P-3.

### §2.9 — W6 details (C-1 PRUNE-3 — Trait-Dispatch + Provider Collapse)

**Entry gate.** W0-W5 admitted; W6 plan names trait API surface, 8-provider-collapse target (1 generic generator template), JSON-rooted symbol migration path, Lock-14 baseline gate update.

**Owner-path family.** `skinny/crates/codegen/src/lib.rs:167-209`, `skinny/crates/codegen/src/{json_provider.rs, css_l4_*_providers/}`, `skinny/crates/bbnf/src/`, `skinny/crates/grammar/src/`, `skinny/crates/passes/src/lib.rs:476-478`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.

**Conditional-dispatch status.** LOAD-BEARING (forward invariant gate-enforced; new `RuntimeProvider::*` match arm rejected by Lock-14 baseline gate).

**Hard cap.** 700-900 source LOC (subset of C-1 2.8k-3.4k envelope split across W6 + W7); ≤90 min.

**Primary candidate.** C-1 PRUNE-3 (SYNTHESIS §3) = replace `RuntimeProvider` enum with trait-based dispatch; collapse 8 per-grammar provider modules under `codegen/` into ONE grammar-agnostic generator template.

**Same-wave consumer NAMED.** Lock-14 baseline gate at `skinny/crates/bbnf-bench/src/lock14_baseline.rs` rejects any commit re-introducing `RuntimeProvider::Json | RuntimeProvider::CssL4 | …` match arm; `find skinny/crates -name '*.rs' | xargs grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar'` returns ZERO.

**Topological dependencies.** W0-W5 (substrate). Constraint §2.2 C-1-BEFORE-C-4 makes W6 BLOCKING for W8. Blocks W7 (PRUNE-4 9 sub-passes need trait dispatch to attach to).

### §2.10 — W7 details (C-1 PRUNE-4 — 9 Per-Grammar Runtime Sub-Passes)

**Entry gate.** W6 admitted (trait dispatch live); W7 plan names 9 sub-pass order (`bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math` per S-P0 §2.3); per-sub-pass LOC + min budget; per-sub-pass revert protocol.

**Owner-path family.** `crates/core/src/runtime/{bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math}/*.rs` (67 files → emitted), `skinny/crates/runtime/src/grammars/` mirror, `skinny/xtask/src/regen_*/` per-grammar subcommands + `check-*` companions.

**Conditional-dispatch status.** LOAD-BEARING (forward invariant: any new grammar produces ZERO new `.rs` files in `skinny/crates/{codegen, runtime, passes, bbnf, grammar}/src/` and ZERO new directories in `crates/core/src/runtime/`; Lock-14 baseline gate-enforced).

**Hard cap.** 2.1k-2.5k source LOC (balance of C-1 2.8k-3.4k envelope); ≤90 min per sub-pass × 9 sub-passes = ≤810 min cumulative; any sub-pass overflow splits before dispatch or returns REVISE per SK-V8 SPEC §2.

**Primary candidate.** C-1 PRUNE-4 (SYNTHESIS §3 + S-P0 §3.3) = refactor 67 hand-written per-grammar runtime files into emitted output via per-grammar `regen-*` xtask subcommands; 9 sub-passes (one per grammar dir; `css_pretty` is +1 over SK-V13 baseline per S-P0 §2.3 + §1.3).

**Same-wave consumer NAMED.** Per-sub-pass: `cargo xtask check-<grammar>` round-trip-clean (read emitted bytes, re-run `regen-<grammar>`, diff empty); cumulative: `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns 0 hand-written entries; Lock-14 baseline gate update at `skinny/crates/bbnf-bench/src/lock14_baseline.rs` rejects any commit violating the forward invariant.

**Topological dependencies.** W6 (PRUNE-3 trait dispatch). Constraint §2.2 C-1-BEFORE-C-4 makes W7 BLOCKING for W8. Blocks W8 (C-4 PRUNE-5 needs generic generator template + per-grammar emitted runtime).

### §2.11 — W8 details (C-4 PRUNE-5 — W8/W9 SCAFFOLD → LOAD-BEARING)

**Entry gate.** W6 + W7 admitted (generic dispatcher + per-grammar emitted runtime live; constraint §2.2); W8 plan names CSP-emitted `BackendShape` dispatch site at `skinny/crates/codegen/src/lib.rs` (no `match grammar { Json => ..., CssL4 => ... }` arm in dispatch path per SYNTHESIS §4); per-shape Lock-1 triad (`substrate_target`, `retention_lifetime`, `policy_owner`); same-wave consumer row `json/numbers/direct_to_struct/main`.

**Owner-path family.** `skinny/crates/ir/src/cost.rs`, `skinny/crates/passes/src/`, `skinny/crates/codegen/src/lib.rs`, `skinny/crates/runtime/src/grammars/json/generated.rs`, `skinny/RESULTS.md`, `skinny/REDRESS.md`.

**Conditional-dispatch status.** LOAD-BEARING (W8 + W9 promoted from gate-layer-only — 3 files / 20 references per S-P0 A5 NEW-MED — into compile + lower + runtime; fail-closed cascade for JSON / CSS / Sheets / BBNF-self rows per SYNTHESIS §4).

**Hard cap.** 800-1.4k source LOC per SYNTHESIS §3 C-4 envelope; ≤90 min.

**Primary candidate.** C-4 PRUNE-5 (SYNTHESIS §3) = CSP-chosen shape produces measurable runtime divergence on a named pre-wave row.

**Same-wave consumer NAMED.** Named pre-wave row `json/numbers/direct_to_struct/main`: pre-wave hot leaf `parse_value_at`, post-wave hot leaf names the W11.1 number-specialised symbol explicitly in the samply trace; row hot-leaf attribution changes in `RESULTS.md`; per-shape Lock-1 triad declared in REDRESS; no row admit cites W8/W9 without measured runtime consumption (per SYNTHESIS §0.4 P-5).

**Topological dependencies.** W6 + W7 (constraint §2.2). Blocks W9-W11 (re-admit waves need CSP-driven shape selection live).

### §2.12 — W9 details (R6 CSS L4 24-feature re-admit + F-V2-P1ABC-RERECORD Stage 0)

**Entry gate.** W0-W8 admitted; W9 plan names per-feature candidate primitive (drawn from S-P2 LOCKED pool under §2.Y canonical-name binding), Track 1 generated path, Track 2/oracle path (independent), strict comparator (lightningcss / cssparser per SYNTHESIS R6), F-V2-P1ABC-RERECORD Stage 0 pre-wave commitment.

**Owner-path family.** **Stage 0 (F-V2-P1ABC-RERECORD):** `skinny/crates/runtime/src/grammars/json/generated.rs:33-237` (cfg_attr flip verification at 8 sites: lines 33-34, 43-44, 58-59, 79-80, 86-87, 117-118, 138-139, 157-158); cargo build `--release -p bbnf-bench --features runtime/parse-attribution`; `samply record` interactive (NOT `--save-only` per `[samply-symbol-resolution]`). **Stage 1+:** `skinny/crates/runtime/src/grammars/css_l4_*/` (W3 R4 emission targets), S-P2 LOCKED primitive consumers (per-feature wiring), `skinny/RESULTS.md` (24 CSS L4 row admit attempts).

**Conditional-dispatch status.** LOAD-BEARING. F-V2-P1ABC-RERECORD is **Stage 0 of W9** per S-P2 §6.3 binding ("Stage 0 of the first SK-V14 implementation wave admitting any dispatch-envelope-internal primitive"); W9 is the first such wave because R6 admits CSS L4 candidates including P2-F C1 structural classify + P2-C C-P2C-1 ascii_set_member64_css_delimiter + P2-F C5 string-block 64-byte oracle (envelope-internal primitives per CH2 V3 dual-gate inheritance).

**Hard cap.** ≤900 source/test/REDRESS LOC (consumer wiring only; primitives drawn from S-P2 LOCKED pool — never re-authored at W9); ≤90 min implementation cap (90 min binding per SK-V8 SPEC §2; F-V2-P1ABC-RERECORD Stage 0 included).

**Primary candidate.** R6 (SYNTHESIS §0.3) = CSS L4 24-feature re-admit through R4 grammar-derived pipeline + R5 production corpora + work-equivalent lightningcss/cssparser strict comparator. S-P2 LOCKED primitives admissible per CF-3 3-gate (scalar-ref / checkasm-parity / same-wave-consumer): P2-F C1 (structural-byte SIMD classify; `byte_class_from_eq_set_64` + `classify_tbl4`), P2-C C-P2C-1 (CSS delimiter member-find — now S-P3-eligible per W4 PRUNE-2 successor wave per p2c §2.X disposition), P2-F C5 (string-block 64-byte oracle), P2-E Gaps as relevant.

**Same-wave consumer NAMED.** Per-feature parity row in `skinny/RESULTS.md`; `lightningcss Mbps` + `cssparser_oracle Mbps` strict equality + `Δ vs SOTA ≥ +1 Mbps` per SYNTHESIS §0.1 per-cell bar; F-V2-P1ABC-RERECORD Stage 0 verifies cfg_attr flip + samply re-record produces envelope-cracked attribution (`dispatch_value` inlined-out; inner primitives named).

**Topological dependencies.** W0-W8 (substrate + comparator + corpora + emission + CSP shape). Blocks W10 + W11 (re-admit cascade).

### §2.13 — W10 details (R7 JSON direct + typed re-admit)

**Entry gate.** W0-W9 admitted (F-V2-P1ABC-RERECORD already shipped in W9 Stage 0 — does NOT re-ship in W10); W10 plan names per-corpus typed-struct bindings, S-P2 LOCKED primitive consumers (P2-A C1 lazy-field-skip-with-index; P2-C C-P2C-4 TBL/TBX hex escape decode; P2-D C-P2D-1 `BackendShape::SinkOnly` activation), Track 2/oracle structural independence proof.

**Owner-path family.** `skinny/crates/runtime/src/grammars/json/generated.rs:466,506` (parse_object_value_at_direct + parse_array_element_at_direct consumer), `skinny/crates/runtime/src/grammars/json/generated.rs:2949` (DirectParser::skip_value consumer per P1-B 72.5-76.1 % self-time signal), `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (typed admit rows + per-corpus typed-struct bindings), `skinny/RESULTS.md`.

**Conditional-dispatch status.** LOAD-BEARING (per SYNTHESIS R7: every JSON direct + typed row re-baselined against rebound strict comparators; cells previously HOLDING under misbound comparator hold again under right comparator OR are reverted).

**Hard cap.** ≤900 source/test/REDRESS LOC (consumer wiring; primitives from S-P2 LOCKED pool — §2.Y canonical-name binding enforced); ≤90 min.

**Primary candidate.** R7 (SYNTHESIS §0.3). Consumer-wiring candidates from S-P2 LOCKED pool: P2-A C1 lazy_field_skip_with_index (typed-plane DirectParser::skip_value at 39.5-76.1 % self-time on 5/7 rows per P1-E §2.3); P2-A C2 long_string_body_simd_scan (under §2.Y canonical-name binding with P2-E Gap 1 + P2-F C1+C2); P2-C C-P2C-4 TBL hex decode batch (S-P3-eligible per p2c §2 disposition); P2-D C-P2D-1 BackendShape::SinkOnly activation (`parse_object_value_at_direct::<JsonDigestSink>` 81.13 % twitter direct Track 1).

**Same-wave consumer NAMED.** Per-corpus `direct_to_struct` + `real_typed_struct` row in `skinny/RESULTS.md`; rebound C-2 comparators (sonic-rs strict struct deser per corpus / per-corpus typed struct deser) + `Δ vs comparator-strict ≥ +1 Mbps`; Track 2/oracle structurally independent of Track 1 per SK-V8 SPEC §2.1.

**Topological dependencies.** W0-W9 (substrate + comparator + W9 F-V2-P1ABC-RERECORD attribution). Blocks W11.

### §2.14 — W11 details (R8 JSON parse_only distinct path + admit)

**Entry gate.** W0-W10 admitted; W11 plan names distinct parse_only code path in `generated_json` (no full-tape build per SYNTHESIS R8); Skipper-class comparator integration (sonic-rs `LazyObject`/`LazyValue` traversal skip-all-fields wrap OR `serde_json::Deserializer::from_slice::<IgnoredAny>` strict baseline per P2-A §1.4); per-corpus parse_only row threshold.

**Owner-path family.** `skinny/crates/runtime/src/grammars/json/generated.rs` (NEW parse_only distinct path), `skinny/crates/bbnf-bench/benches/json_parity.rs` (Skipper-class comparator integration), `skinny/RESULTS.md` (17 parse_only row admits).

**Conditional-dispatch status.** LOAD-BEARING (per SYNTHESIS R8: distinct parse_only code path + Skipper-class comparator; no full-tape build is audit-checkable via tape-allocation telemetry from W0 schema).

**Hard cap.** ≤700 source/test/REDRESS LOC; ≤90 min.

**Primary candidate.** R8 (SYNTHESIS §0.3). Consumer-wiring candidates from S-P2 LOCKED pool drawn under §2.Y canonical-name binding (P2-A C2 / P2-E Gap 1 / P2-F C1+C2 = ONE canonical long-string-body SIMD scan primitive per S-P2 §6.2); P2-F C4 (tiny-keyword-set match cap-16 `match_tiny_plain_string_with_cap::<16>` 96.3 % distinct_values parse-only); P2-F C6/C7 (envelope-internal primitives per S-P2 §6.3 F-V2-P1ABC-RERECORD dependency — already discharged at W9).

**Same-wave consumer NAMED.** Per-corpus `parse_only` row in `skinny/RESULTS.md`; sonic-rs Skipper-class strict comparator + `Δ vs Skipper-strict ≥ +1 Mbps`; tape-allocation telemetry confirms no full-tape build (W0 schema column `substrate_target` ∈ `local_temp_only` for parse_only path per Lock-1 v+1 manifest).

**Topological dependencies.** W0-W10. Bracket close: when W11 admits successfully (or routes residuals with architectural-block proof), SK-V14 closes per SYNTHESIS §0.1 R10; otherwise Pass Alpha brackets SK-V15 per SYNTHESIS §0.1 addendum A4.

---

## §3 — Falsifiability binding

Per PASS-3-SYNTHESIS-PLAN §2.1 + `[no-orphan-redress]`, every wave's exit gate names corpus rows + Mbps thresholds. Full-table maintain budget per SK-V8 SPEC §5 W2 floor pattern: every non-target row no worse than -2.0 % Track 1 + Track 2 vs `SK-V14-open`. (P3-C authors the per-wave gate matrix; this artefact names the falsifiability bindings load-bearing on the wave sequencing.)

| Wave | Named row gate | Mbps threshold | Full-table maintain | Revert protocol |
|---|---|---|---:|---|
| W0 | All 51 JSON + 24 CSS L4 main rows | ±1.0 % of `SK-V14-open` per SK-V8 SPEC §3 | N/A (baseline-establishing) | Revert W0 implementation commit set; restore opening RESULTS schema; W0 REDRESS rejection naming missing profile / gate / row |
| W1 | All JSON main rows (51) | per_iter_equality = PASS on every iter; comparator-plane Mbps reported | ±1.0 % of `SK-V14-open` | Revert C-2 harness changes; W1 REDRESS naming missing equality column or asymmetric comparator |
| W2 | W14.1-5 + W13.* + W15.1 admit-row revert (22 rows) | post-revert: parse_only 0/17, direct 0/17, typed 0/17 per SYNTHESIS §1.3 honest delta | ±1.0 % vs W1-close (revert is documentation; no production behavior delta) | Revert PRUNE-1 ledger reverts; restore W14.1-5 + W13.* + W15.1 row entries to pre-W2 state |
| W3 | round-trip check on dual runtime trees | xtask-emitted bytes `git diff` empty post `rm -rf … && cargo xtask regen-css` | ±1.0 % JSON rows vs W2-close (R4 is CSS-only) | Revert xtask regen-css subcommand + emission scaffolding; restore pre-W3 css_l4_*_templates state |
| W4 | 24 CSS L4 admit-row revert + 7-twin deletion | post-revert: CSS L4 0/24 per SYNTHESIS §1.3; fake-`@generated` detector empty | ±1.0 % JSON rows vs W3-close | Revert PRUNE-2 deletions + ledger reverts; W4 REDRESS naming any twin-deletion that failed round-trip-clean check |
| W5 | `du -sh skinny/corpora/css-l4-sk-v14` | > 800 KB working set per pre-block P-3 | ±1.0 % JSON rows vs W4-close | Revert corpora directory + bench wiring; W5 REDRESS naming missing corpus source or bench-integration gap |
| W6 | Lock-14 baseline gate | `find skinny/crates -name '*.rs' \| xargs grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar'` returns ZERO | ±1.0 % JSON + CSS rows vs W5-close | Revert PRUNE-3 trait-dispatch + provider collapse; W6 REDRESS naming any JSON-rooted symbol surviving in generic crate |
| W7 | 9-grammar per-pass round-trip + Lock-14 baseline | `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns ZERO post-redress; per-grammar `check-*` xtask `git diff` empty | ±1.0 % JSON + CSS rows vs W6-close | Per-sub-pass revert; sub-pass overflow splits before dispatch; W7 REDRESS naming the offending sub-pass + LOC overflow |
| W8 | `json/numbers/direct_to_struct/main` hot-leaf attribution flip + per-shape Lock-1 triad | post-wave hot-leaf names W11.1 number-specialised symbol in samply trace; per-shape `substrate_target / retention_lifetime / policy_owner` declared in REDRESS | ±1.0 % JSON + CSS rows vs W7-close (no admit attempt yet) | Revert C-4 W8/W9 wire-up; W8 REDRESS naming missing CSP runtime divergence or absent Lock-1 triad |
| W9 | 24 CSS L4 feature parity rows | `lightningcss Mbps` + `cssparser_oracle Mbps` strict equality; per-cell `Δ vs SOTA ≥ +1 Mbps` per SYNTHESIS §0.1 R10 bar | ±2.0 % every non-target JSON row vs `SK-V14-open` per SK-V8 SPEC §5 pattern; existing real-typed GO maintains GO | Revert per-feature consumer wiring; F-V2-P1ABC-RERECORD Stage 0 artefacts preserved as evidence; W9 REDRESS per failed feature naming intrinsic-block proof OR rejection |
| W10 | 17 direct + 17 typed admit rows | rebound C-2 comparator-strict + `Δ ≥ +1 Mbps`; Track 2 structural independence per SK-V8 SPEC §5 | ±2.0 % parse_only + CSS L4 rows vs `SK-V14-open` | Revert per-row consumer wiring + generated outputs + bench wiring; W10 REDRESS per failed row |
| W11 | 17 parse_only admit rows | sonic-rs Skipper-class strict + `Δ ≥ +1 Mbps`; tape-allocation telemetry confirms no full-tape build | ±2.0 % direct + typed + CSS L4 rows vs `SK-V14-open` | Revert parse_only distinct path + Skipper-class integration; W11 REDRESS per failed row |

Per `[no-warm-benches]` every threshold above is on cold-per-parse Mbps; warm/cached benches are inadmissible. Per `[bench-sequential-regression]` benchmarks run sequentially with regression check.

---

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)

Per SYNTHESIS §0.4 P-1..P-7 + PASS-3-SYNTHESIS-PLAN §3 CH3 + S-P2 V3-LOCKED REDRESS surface, the per-wave pre-block ledger (binding on every wave's redress phase; P3-E authors the full ledger; this artefact names the load-bearing pre-blocks on the sequencing):

**Global pre-blocks (every wave inherits per SYNTHESIS §5):**

- **P-1 — Fake `@generated` header on hand-written templates** (`audit-overfit/validation/v1 §1` Claim 1). W3 + W4 + W7 enforce; ESPECIALLY W4 post-deletion + W7 per-grammar emission round-trip.
- **P-2 — `sonic_rs::from_slice::<Value>` mislabelled as strict comparator** (`v6-comparator-integrity §1 + §3`). W1 + W10 + W11 enforce.
- **P-3 — Tiny-fixture Criterion-overhead Mbps inflation** (`v1 §1` Claim 4). W5 R5 corpora pin (≥800 KB) is the structural pre-block; W9 + W10 + W11 cannot admit on <1 KB fixtures.
- **P-4 — Gate-relabel as admit** (`v2 §1`). W9 + W10 + W11 require parser/codegen source delta cited per row + measurement evidence per REDRESS.
- **P-5 — Scaffold-research counted as load-bearing** (`v4 §4 + §5`). W8 PRUNE-5 is the structural pre-block (wires W8/W9 from SCAFFOLD to LOAD-BEARING); no W9/W10/W11 row admit may cite W8/W9 until W8 is the runtime consumer measured.
- **P-6 — Per-grammar provider modules in generic codegen** (`v3 §1`). W6 PRUNE-3 is the structural pre-block; W7 PRUNE-4 carries the per-grammar runtime collapse.
- **P-7 — Track 1 ≡ Track 2 dishonesty.** W0 + W9 + W10 + W11 enforce `track2_entry_point` column; gate-json rejects any row where Track 1 + Track 2 share common ancestor in `runtime::tape::` beyond public `Tape` / `OffsetFlags` types per SYNTHESIS §2.

**Per-wave additional pre-blocks (REDRESS entries):**

- **W0:** no parser, scanner, SIMD, asm, codegen behavior, product-plane behavior, or generated parser output change lands (SK-V8 SPEC §3 exit gate verbatim); REDRESS 36/37/38 (Lock-14 residue) carry forward as audit-only.
- **W1:** no behavior changes; comparator rebind + per-iter equality oracle only; REDRESS 87 (CostFacts evidence boundaries) inherited as audit-only.
- **W2:** documentation revert only; no parser/codegen/runtime delta; REDRESS 50-55 (SK-V5 dispatch-table alternates) audit-only.
- **W3:** xtask emission only; no behavior delta on existing JSON rows; REDRESS 119/120 (direct-row fixpoints) audit-only.
- **W4:** deletion + revert only; REDRESS 60-72 (retained-parse + sidecar producers) inherited as P-7 enforcer.
- **W5:** corpora curation only; no source delta.
- **W6:** refactor only; preserves 4 existing real-typed GO rows + 3 existing direct GO rows per SYNTHESIS §1.1 W2 floor pattern; REDRESS 28+33 (Class A NEON tiny-string wiring) audit-only.
- **W7:** per-grammar emission only; preserves all 4 existing real-typed GO rows; REDRESS 80 (canada mantissa-widen) audit-only.
- **W8:** CSP-driven shape selection; REDRESS 96/97/98 (Union-substrate / class-column / streaming-cursor variants) PERMANENTLY pre-blocked per P2-D §4.1 + LOCKS.md:84-90; W8 plan MUST cite REDRESS 96/97/98 as forbidden routes; no parallel substrate / retained cursor / aux density table / sidecar event vector per SK-V8 SPEC §6 W3 pre-blocks.
- **W9:** REDRESS 28+33 (tiny-string NEON wiring as parse close) pre-blocked; REDRESS 82-84 (single-quartet unicode classifier + StringBlock16 tiny probe + object-pair compaction) pre-blocked per P2-C §4 + P2-E §1.4; REDRESS 88 (PMULL prefix-XOR as default hot body) pre-blocked unless C-P2C-2 ships as SIMD-first union consumer DELETING the scalar consume step (per p2c §2 disposition).
- **W10:** REDRESS 50-55 (no-allocation visitor, parse-time aux side tables, EventCursor, parser-local structural-mask cursor, decoded stats sink, quote-source fused string materializer) pre-blocked per SK-V8 SPEC §10; REDRESS 66-72 (direct source-hook families, parser-owned scratch, byte-output unescape, semantic string facts, hand typed sinks as proof, stale mantissa widening, raw f64 shortcut) pre-blocked.
- **W11:** REDRESS 89 (CSSC CTZ next-bit bulk consumer) pre-blocked per P2-C §4 (only re-opens as SIMD-first union consumer per p2c §2 disposition); REDRESS 90 (B6 canary hardening as performance evidence) pre-blocked.

**Cross-wave (load-bearing) pre-blocks per S-P2 carry-forward:**

- **No three orthogonal SIMD bodies for the long-string-body SIMD scan primitive** per S-P2 §6.2 §2.Y canonical-name binding. The three convergent identifiers (P2-A C2 / P2-E Gap 1 / P2-F C1+C2) MUST consolidate to ONE canonical primitive name + ONE canonical scalar-ref function at W9/W10/W11 admission. A wave admitting any of the three under three names fails CH2/CH7.
- **F-V2-P1ABC-RERECORD Stage 0 inheritance** per S-P2 §6.3. The 12 consumer-dependency candidates (P2-A C6 + P2-C C-P2C-3/-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13) cannot ship without Stage 0 (cargo build `--features runtime/parse-attribution` + interactive samply record + cfg_attr flip verification at 8 sites). Stage 0 lands in W9; if W9's primitive consumers do NOT include the 12-list (i.e., W9 only admits CSS L4 primitives not in the 12-list), Stage 0 STILL ships in W9 because W9 is the first implementation wave per S-P2 §6.3 binding.
- **3-gate CH4 admission cell on every shortlisted candidate** per S-P2 §6.1 CF-3. Every W9/W10/W11 candidate's admission manifest cell carries (scalar-ref status / checkasm-parity expectation / same-wave-consumer NAMED); the cell rejects any candidate missing one of three.

---

## §5 — Sources

### §5.1 — Binding authorities (read end-to-end before sequencing)

- `restart/skinny/tranches/sk-v14/research/p3/S-P3-DISPATCH-CONTEXT.md` (88 lines; §0-§5 in full).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` (276 lines; §2 scope matrix + §2.1 frontmatter + §3 CH1-CH6 lens overlay + §7 hard caps + §8 bbnf-lang specifics + §8.3 W0 binding).
- `restart/skinny/tranches/sk-v8/SPEC.md` (812 lines; §2 wave manifest shape verbatim — the SPEC shape P3-F mirrors).
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (407 lines; §0 close-condition + R1-R10 goalset + P-1..P-7 pre-blocks + §3 C-1..C-5 candidate slate + §4 S-P3 constraints).
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (534 lines; §2.1-§2.3 three binding sequencing constraints + §3 prune list + §3.3 sub-wave count).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` (659 lines; §3Z COHORT LOCK + §6 carry-forward packets binding on S-P3 wave program).
- `restart/locks/LOCKS.md` (Lock 1 substrate-union + Lock 14 grammar-neutrality v+1 + Lock 16 SIMD/ASM allowlist).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (wave-execution contract every wave conforms to).

### §5.2 — S-P2 LOCKED candidate pool (V3 §3Z COHORT LOCK; HEAD `ebe84954b`)

- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md` (367 lines; 7 SOTA candidates 7/7 grammar-neutral): C1 lazy_field_skip_with_index; C2 long_string_body_simd_scan (under §2.Y canonical-name binding); C3 quote-aware-classifier inside parse envelope; C4 number-fast-path SIMD; C5 force-inline + LTO discipline; C6 envelope-internal primitive census (F-V2-P1ABC-RERECORD-gated); C7 stage1/stage2 separation.
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md` (217 lines; 5-stage admission process — dav1d-style discipline binding on every primitive admission).
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md` (164 lines; 5 active candidates + 3 demoted): C-P2C-2 pmull_cssc_structural_union_emit64 (pre-blocked at V1; re-opens only as SIMD-first union consumer); C-P2C-3 udot_digit_span_x4 (NOT-S-P3-ELIGIBLE at V1; F-V2-P1ABC-RERECORD-gated); C-P2C-4 tbl_tbx_escape_decode_batch (S-P3-ELIGIBLE for JSON fixed-width `\uXXXX`); C-P2C-5 string_special_64_context (conditional support primitive); C-P2C-8 parse_attribution_profile_rebuild_gate (= F-V2-P1ABC-RERECORD; Stage 0 of W9); demoted: C-P2C-1 (CSS delimiter; re-evaluates after PRUNE-2 successor wave = W9), C-P2C-6 (eor3 mask fusion; no antecedent), C-P2C-7 (byte_context orphan; hygiene only).
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md` (254 lines; 2 active + 1 demoted + 1 pre-blocked): C-P2D-1 BackendShape::SinkOnly activation; C-P2D-2 OffsetTapeStats column extension; C-P2D-3 demoted to §1.6(d) substrate-side observation; C-P2D-4 EventTape pre-blocked by REDRESS 96/97/98.
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md` (342 lines; 9 Layer-1 primitive gaps; zero Layer-0 needed; Gap 1 `scan_string_special_block_sweep_64` under §2.Y canonical-name binding).
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md` (360 lines; 13 active + 1 demoted): C1-C14 with C8 demoted §2.X.1 default per `[no-deferrals]`; C10/C13 Stage-A authoring targets at `crates/bbnf-simd/src/scalar/byte_context_64.rs` + `bcax_64.rs` (NOT-PRESENT at HEAD — queued for same-commit S-P3 admission per Lock 16); C12 reframed CH4-ACCEPT (scalar-ref EXISTS via `scan_structurals_scalar` at `scan.rs:32`); C2 upgraded with P2-E Gap 6 three-way composition.

### §5.3 — S-P1 hot-leaf antecedents (post-V3 amendments; binding on candidate justification)

- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` (V2 §1.3 CH2 primitive classification; §2.1-§2.5 per-corpus tables; §4.1 envelope mis-attribution census; §4.4 substrate-union framing; §4.7 REDRESS reconciliation).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md` (V3-LOCKED consolidator).
- `restart/skinny/tranches/sk-v14/research/p1/p1{a..f}-*.md` (six axis files).

### §5.4 — Empirical floor + REDRESS surface

- `skinny/RESULTS.md` (185 lines; SK-V13 close baseline; W0 captures `SK-V14-open` from this).
- `skinny/REDRESS.md` (~5041 lines; per-wave pre-block surface; P3-E authors the full per-wave ledger; this artefact cites the load-bearing entries).
- `skinny/ROLLING-SOTA-DELTA.md` (rolling delta; W2 + W4 re-baseline to SYNTHESIS §1.3 honest delta).

### §5.5 — Architectural sequencing-constraint origins (S-P0 V1; gate-binding on this wave plan)

- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:240-291` (§2.1 R4-before-PRUNE-2 verbatim; §2.2 C-1-before-C-4 verbatim; §2.3 PRUNE-4 = 9 sub-waves verbatim with `find` evidence).
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-{css-measurement, admit-mechanism, lock14-scan, generator-truth, decision-engine, pre-restart-pattern}.md` (six per-axis files; 74 findings).

### §5.6 — Sources S-P3 V1 → V{N+1} fold targets

- P3-A candidate-shortlist artefact: W9/W10/W11 candidate slate refined per CF-3 3-gate admission cell on every shortlisted candidate (this artefact assumes the S-P2 LOCKED pool is the substrate; P3-A's ≤8-candidate shortlist further filters for the SPEC's load-bearing slate).
- P3-C falsifiability-gate artefact: per-wave gate matrix (named corpus rows + Mbps thresholds + full-table maintain budget + revert protocol) — this artefact names the falsifiability bindings load-bearing on the wave sequencing; P3-C authors the per-wave matrix in full.
- P3-D telemetry-schema artefact: SK-V14 column schema + `gate-json` rejection rules — W0's same-wave-consumer claim depends on P3-D's column enumeration; this artefact's W0 binding consumes the SYNTHESIS §2 telemetry binding.
- P3-E pre-blocked-ledger artefact: per-wave REDRESS pre-block list — this artefact's §4 names the load-bearing pre-blocks; P3-E enumerates the full per-wave ledger.
- P3-F SPEC drafting: `restart/skinny/tranches/sk-v14/SPEC.md` consumes this wave manifest verbatim under the SK-V8 SPEC §2 wave-manifest shape; the 12-wave count + topology + LOC budgets + same-wave-consumer namings + sequencing-constraint discharges flow into the SPEC's wave-by-wave sections (SPEC §3 = W0, §4 = W1, …, §14 = W11; total 12 wave sections matching SK-V8 SPEC §3-§9 shape).
