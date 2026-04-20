# AX Planning — Session Recap + Audit-Staleness Map

Deep recap of the W1 + planning sessions, inventory of prior audit
artefacts, explicit staleness verdict per document, and the fresh-audit
wave structure this document grounds. Written at master HEAD
`4f99b8c5`, pre-fresh-wave.

## 1. W1 cascade — what landed

Eight sub-waves commissioned, eight landed. Master trail:

| # | Commit | Sub-wave | Deliverable |
|---|--------|----------|-------------|
| 1 | `147ea4a5` | W1 absorb re-plan | AX invariant 21 added; `waves/W1.md` full rewrite to 7 W1r sub-waves; PROGRESS.md absorb entry |
| 2 | `3429aaba` | W1r.0 | Revert W1.A/B hand-coded `bbnf::{json,css}::Value` duplicates (−6,128 LOC); sonic-rs → dev-dep; invariant 4/11/18 restoration |
| 3 | `5d5096eb` | W1r.1 | `RustNamedTypes::from_ir` IR-walker replaces static `BINDINGS`; diag `audit/W1r1-diag.md` |
| 4 | `a6429d3e` | W1r.2 | JSON canonical-form parity vs sonic-rs (10/1; `strip_insignificant_ws` symmetric normalizer) |
| 5 | `933d02fb` | W1r.3a.1 | `?w` / `OptionalWhitespace` prettify threads `@ws` pattern through regex emitter (comment-aware trim); schema bump 12→13 |
| 6 | `d11874db` | W1r.3a.2 | CSS L4 `@pretty` directives (stylesheet/ruleList/blockContent/qualifiedRule/atRule/mediaRule/keyframesRule) |
| 7 | `b930cf2c` | W1r.3a.3 | CSS canonical-parity harness + 20-rule `token_normalize`; 3/0 active tests (byte on normalize, scale+interop on bootstrap/tailwind) |
| 8 | `293be673` | W1r.3a.4 | Ignored tests → active scale+interop validation (invariant 18) |
| 9 | `53318493` | W1r.5 | BBNF self-parity 56/0 over 28 `.bbnf` fixtures |
| 10 | `f6a264e2` | W1r.4a.1 | `@pretty sep(X)` codegen fix via new `crates/core/src/backend/prettify/sep_rewrite.rs` module + 3-line leak fix in Repeat loop |
| 11 | `28fd46fc` | W1r.4a.2 | Bootstrap regen (+3 lines, cycle-1 = cycle-2 byte-identical) |
| 12 | `53d99e4a` | W1r.4a.3 | Sheets self-parity 84/0 (51 serialize + 30 prettify + 3 corpus) |
| 13 | `81627d7c` | W1r.6 | Typed-accessor surface audit 14/0 over 295 rules × 7 accessor classes |
| 14 | `ab7c218d` | W1r.7 | Twitter lazy-field bench via NodeView; AoS 4.14× SoA (ax-iter) / 1.67× (release) |

**Aggregate green state** at master: 13 parity + canonical harnesses
pass with **247 tests + 1 ignored** (`data_xl` debug-assertions-gated;
runs under `--release`).

## 2. Scope-reveals surfaced during W1

Three scope-reveals, each requires next-tranche follow-up:

### 2.1 W1r.1 — `TypeDesc::Named` collapses before Rust emit

Empirical probe (panic at `emit_direct_to_struct_projection` when
`ir.types` contains any `Named(_)`): **zero grammars admit Named at
Rust-emitter invocation time** across JsonParser (8 types), BbnfParser
(17), EbnfParser (12), BnfParser (5), CssParser (15),
GoogleSheetsParser (31).

VM-target path preserves `Named(String)` for JSON via
`universal_named_shape`; Rust-target path does not. CSS L4's three
`-> input : Color` rules: `colorFunction` / `colorMix` eliminated
entirely from `ir.rules`; `colorFn` survives as structural tuple
`Tuple([Span, U8, BoxedEnum×3, Option(BoxedEnum)])`.

The static `BINDINGS` table was dead code on every grammar. W1r.1's
IR-walker refactor (`RustNamedTypes::from_ir`) is **consumer-ready**
but upstream never feeds it.

Diag: `docs/tranches/AX/audit/W1r1-diag.md`. Five-file surgical fix
proposed in `02-named-type-preservation.md`. **Phase 1-A6 elaborates
the design.**

### 2.2 W1r.3/3a — lightningcss print is not byte-canonical

lightningcss `PrinterOptions { minify: false }` still performs:
- `calc()` arithmetic simplification (`calc(3rem + calc(1.5em +
  .75rem))` → `calc(1.5em + 3.75rem)`)
- Position-pair commutativity (`top X right Y` → `right Y top X`)
- Multi-value shorthand reordering

No symmetric bytes-level normalizer inverts these. `normalize.css`
passes byte-parity; `bootstrap.css` + `tailwind.css` ship as scale +
interop tests (bbnf parses + prettifies + output re-parses on both
parsers) rather than byte-parity.

Diags: `audit/W1r3-diag.md`, `audit/W1r3a-diag.md`. CSS semantic
canonicalizer (`calc()` evaluator, property-order table) tracked for
a dedicated workstream in new tranche. **Phase 1-A2 profiles CSS L4
parse hot paths; byte-canonical-parity scope is NOT re-opened in this
planning phase.**

### 2.3 W1r.4/4a — `@pretty sep(X)` double-emitted on `<<` bodies

Rule bodies that declare `@pretty sep(", ")` but also emit body-level
commas via `<<` double-emitted separators on every prettify pass
(e.g. `=SUM(A1:A10)` → `=SUM(A1:A10, )` → `=SUM(A1:A10, , )`).

Fixed in `crates/core/src/backend/prettify/sep_rewrite.rs` — new
module. Elides body-emitted separator via `emit_prettify_silent`
wrapper. Cross-grammar audit confirms only Sheets currently declares
`sep(X)`; 3-line leak fix in the iterated-Repeat loop applies
universally. **Closed; informs §Pattern in Phase 2 synthesis.**

## 3. Pre-existing AX debt (carried forward)

Surfaced during W1 execution, not W1-caused, blocking `cargo test
--workspace` clean:

### 3.1 Five stale W0a/W0b-era test files fail to compile

Reference retired predicates + carved GrammarProfile fields:

- `crates/core/tests/bbnf_profile_wire_contract.rs` — 8 compile errors on
  carved `GrammarProfile` fields (W0b.A retired 7 dead profile slots).
- `crates/core/tests/grammar_profile_wire_contract.rs` — 15 compile errors,
  same root cause.
- `crates/core/tests/json_parity_shape_emit.rs` — 2 compile errors, references
  deleted `dta_run_JsonGrammar` walker symbol (W0b.A deletion).
- `crates/core/tests/gate_predicate_wire_contract.rs` — 2 compile errors,
  references retired `has_w4_classified` / `has_full_shape_coverage` /
  `has_shape_dispatcher_entrypoint` shape predicates (W0a.2.j admission
  widening retired these gates).
- `crates/core/tests/aw_v_w5_2_per_ref_routing.rs` — 2 compile errors, same
  retired predicates.

Per AX invariant 14 ("every predicate disabling emission carries a
per-grammar wire-contract test"), these retire with their predicates.
W0b.D's "delete 8 DTA-coupled test suites" commit missed them.
**Retirement blocked by interactive permission denial last session;
new tranche wave will formalize retirement with an explicit commit
message authorizing the cleanup.**

### 3.2 `ebnf_prettify.rs` recognizer bug at offset 0

`EbnfParser::parse("digit = \"0\" | \"1\" | \"2\" ;")` fails with
`Syntax { offset: 0, rule: None }`. Pre-existing. Not caused by W1r
landings — `bbnf_self_parity` parses the same `ebnf.bbnf` successfully
via `BbnfEmit::parse`. Divergence is in the ebnf.bbnf-derived
`EbnfParser::parse` specifically. Investigation in new tranche.

### 3.3 AX-level unfulfilled contract items

- `post-AX-W1-close.json` bench matrix never captured (invariant 10:
  "Every wave runs the 19-entry matrix at mid-wave AND close").
- AX `FINAL.md` never written.
- AX wave schedule lists W2–W15 as unopened; the new tranche must
  decide whether to absorb into AX's remaining waves or pivot to a
  new letter.

## 4. Prior-audit-doc staleness map

Prior session (same orchestrator, different run) committed six audit
docs at `docs/tranches/AX/audit/next-tranche/`. Per-doc staleness
verdict:

| # | Doc | Verdict | Reason |
|---|-----|---------|--------|
| 01 | prior-tranche-archaeology.md | **KEEP** | Commit-based archaeology; no perf data. High-value lineage analysis. |
| 02 | named-type-preservation.md | **KEEP** | Pass-level IR audit; traces Named collapse through `span.rs::compute_sp_method_rules` + proposes 5-file surgical fix. Informs Phase 1-A6. |
| 03 | value-api-json-perf.md | **STALE (profile)** | Cites `.profiles/samply/json_monolithic/canada/profile.json.gz` dated Apr 17 18:03 — predates W0b (DTA walker deletion). Top symbol `__dta_walker_inline::run` at 50-61% self-time is the symbol W0b retired. Lever recommendations based on this profile are invalid. Bench numbers (bbnf 5.5-8.2× sonic-rs) may be fresh — re-verify on Phase 1-A1. |
| 04 | (missing) | **CREATE** | CSS L4 parse bench + profile never drafted. Phase 1-A2 owns this. |
| 05 | sheets-bbnf-profile.md | **PARTIALLY FRESH** | A5 agent re-captured profiles with `-az-a5` suffix on Apr 20 01:01-03, explicitly distinguishing from stale Apr 17 baselines. Universal hotspot union (push_structural 28-40%, finalise 15-22%, regex_scan 12-26% on token-heavy grammars) is fresh and consistent. Phase 1-A3 verifies. |
| 06 | compile-time.md | **VERIFY** | `cargo-timing` + per-crate rustc breakdown + CSS L4 9× cost claim. Independent of runtime walker retirement — likely valid at current HEAD. Phase 1-A4 re-runs. |

## 5. Root-cause of the staleness

`docs/instructions/PROFILING.md` §"Prepare a wave" states:
> Do not rerun `cargo expand` or `cargo bench` inside a wave after
> prepare has finished. Re-runs waste cycles and produce divergent
> artefacts. Sub-agents consume the prepared artefacts; they do not
> regenerate them.

The prior `prepare-profile-wave.sh` run was Apr 17 (pre-W0b). The A3
agent obediently consumed those prepared artefacts. The A5 agent
noticed the profiles bore stale binaries (walker symbols) and
explicitly **re-captured** with a suffix rather than violating the
no-rerun contract globally.

**Fresh baseline requires a fresh prepare run on current HEAD.**
Phase 0 of the new wave does exactly this.

## 6. Fresh-audit wave structure

### Phase 0 (serial, orchestrator-driven)
1. Clear `.bbnf-cache` directories workspace-wide.
2. Export shared `CARGO_TARGET_DIR`.
3. Run `scripts/prepare-profile-wave.sh` on HEAD `4f99b8c5`.
4. Verify `.profiles/samply/prebuild/wave.tsv` + `binaries.tsv` +
   per-bench `expand.rs`.
5. Commit prepare artefacts (cosmetic — the wave.tsv is
   orchestrator-state, not code).

### Phase 1 (6 parallel worktree agents, read-only)

| Agent | Scope | Inputs | Output |
|-------|-------|--------|--------|
| **A1** | JSON parse bench + profile fresh | `wave.tsv` row for `json_monolithic` (5 fixtures) + `json_competitors` for matched bench | `docs/tranches/AX/audit/next-tranche/A1-json-parse-fresh.md` |
| **A2** | CSS L4 parse bench + profile NEW | `wave.tsv` row for `css_l4` (3 fixtures) + `css_competitors` vs lightningcss/cssparser | `docs/tranches/AX/audit/next-tranche/A2-css-l4-parse-fresh.md` |
| **A3** | Sheets + BBNF parse bench + profile | `wave.tsv` rows for `google_sheets_monolithic` + `bbnf_monolithic` | `docs/tranches/AX/audit/next-tranche/A3-sheets-bbnf-parse-fresh.md` |
| **A4** | Compile-time on HEAD | No profile inputs; runs `cargo build --timings` + per-grammar derive test binaries | `docs/tranches/AX/audit/next-tranche/A4-compile-time-fresh.md` |
| **A5** | Value API apples-to-apples design | Read-only: W1r.7 bench source, `view/mod.rs`, `generate/serialize/mod.rs`, sonic-rs public API docs | `docs/tranches/AX/audit/next-tranche/A5-value-api-design.md` |
| **A6** | Named-type preservation fix design | Read-only: W1r.1 diag + doc 02 pointers + IR pipeline passes | `docs/tranches/AX/audit/next-tranche/A6-named-preservation-design.md` |

A1/A2/A3 profile runs contend for CPU under samply record. Per
`PROFILING.md` orchestration contract, up to 5 profile agents is
canonical — the contention is tolerated because profile captures are
brief (single-iter `bencher` passes). A4/A5/A6 do not capture
profiles so can run fully concurrent.

### Phase 2 (serial, synthesis)

Read all 6 reports + integrate. Decide tranche letter:

- **AY is reserved for replay/recovery/incremental** per existing
  `docs/tranches/AY/AY.md` (461 lines, fully drafted). Do NOT reuse.
- **AY is the next available letter** for a performance + projection
  tranche addressing: direct-to-struct preservation, tape hot-path
  optimization (push_structural/finalise/regex-scan), Value API
  materialization, compile-time reduction, stale-test retirement,
  ebnf_prettify recognizer fix, AX closure artefacts (bench matrix +
  FINAL.md).

Parent tranche doc + per-wave specs authored per `WAVE_SPEC.md`.
Committed together as `docs(AY): open tranche`.

## 7. Deferred items — folded as HIGH PRIORITY per user directive

Every item below lands in AY (or absorbed-AX) waves; no deferrals:

1. **Named-type preservation + direct-to-struct projection (HIGH)** —
   `->  input : <Name>` must reach emit-time; `emit_direct_to_struct_projection`
   activated universally; Color/ColorMix + all grammar-declared
   aggregate types emit `.as_<Name>()` accessors.
2. **Tape hot-path optimization (HIGH)** — `push_structural`
   28-40% + `finalise` 15-22% of self-time across grammars. Attack
   at substrate (SIMD push, fuse finalise into emit loop, eliminate
   double-pass).
3. **Regex scan optimization (HIGH)** — `__regex_scan_<Parser>`
   12-26% on CSS L4 + Sheets. Per A2/A3 profiling findings; propose
   specific DFA / HIR changes.
4. **Compile-time reduction (HIGH)** — CSS L4 877 MB RSS + 5.81s +
   13 MB cache. Per-shape emitter fusion, expand-time token reduction.
5. **Stale test retirement + ebnf_prettify fix (MEDIUM)** — 5
   compile-fail tests delete; EBNF recognizer parse-at-0 investigation.
6. **AX close artefacts (MEDIUM)** — `post-AY-open.json` bench
   baseline; optional AX `FINAL.md` if AY absorbs AX closure.
7. **Value API materialization (MEDIUM)** — opt-in
   `parsed.to_value::<T>() -> T` grammar-emitted surface for apples-to-
   apples vs `sonic_rs::Value` / `lightningcss::StyleSheet`. Must be
   grammar-derived per invariant 21 (no hand-coded duplicate).

## 8. Invariants the new tranche carries forward

All AX invariants 1-21 remain authoritative. The new tranche adds
zero invariants; it discharges the scope-reveals W1 surfaced without
widening surface. Specifically:

- Invariant 14 (gate-predicate symmetry) discharges via retirement of
  5 stale tests + any predicate carved alongside.
- Invariant 18 (no stubs/placeholders) discharges via concrete code —
  no `#[ignore]`, no `todo!()`, no `TypeOnly`.
- Invariant 20 (shape-emission-authoritative) discharges via
  preservation of Named through shape emitter.
- Invariant 21 (grammar-derived view surface) discharges via
  direct-to-struct activation + any materialized-tree Value API
  being grammar-emitted, not hand-coded.

## 9. Architectural transpositions admitted

Per user directive ("architectural transpositions in the sake of
elegance, simplicity, and performance above all are both necessary
and desirable"), the new tranche may:

- Rewrite `prepare_grammar → analyze_grammar → project_types` Rust
  pipeline passes that collapse Named (doc 02 §2 candidate passes:
  `try_flatten_pair`, `factor_common_prefixes`, `eliminate_epsilon`,
  `fuse_single_use`, `compute_sp_method_rules`).
- Restructure `tape::columns::push_structural` / `tape::finaliser`
  at the substrate level.
- Add grammar-level `@pretty` directives when they encode universal
  canonical-form behavior (not per-case normalizer rules).
- Refactor / rename the shape emitter modules if universal fusion
  landing calls for it.

No grammar DSL additions (invariant 4). No hand-coded `Value` enum
duplicates (invariant 21). No third-party comparator bridges
(invariant 21). No substrate-without-consumer landings (invariant 2).

## 10. Commits checkpoint

This recap commits as `docs(next-tranche): session recap +
audit-staleness map` alongside task-list reset. Phase 0 commits as
`chore(profile): fresh prepare-profile-wave on HEAD`. Each Phase 1
agent commits its own report. Phase 2 commits the new tranche plan
as `docs({LETTER}): open tranche`.

Per user directive: **/commit frequently**.
