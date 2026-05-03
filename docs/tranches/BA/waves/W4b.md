# BA.W4b — Public Parse / Document::get Wrappers

**Thesis** (the public `parse` / `parse_with` wrappers reroute through the W4a-elided private parse core; `JsonDocument::get<T>` reroutes through `parse_with(input, path)`; the post-parse linear walk vanishes; BA-G9's 4196× → ≤ 5× gap closes). **Closer-gate** (BA-G9 met: `JsonDocument::get<T>(twitter.json, &path)` ≤ 5× full parse on M1 Pro; `parse` and `parse_with` share one source; `rg -n 'pub fn parse_with' crates/core/src/grammar/generated/` returns ≥ 9).

## §1 — Deliverable

Hereupon the bifurcation between eager parse and cursor-driven parse — extant since the introduction of `parse_with` per `audit/RESTART-SKETCH-2026-05-03.md:35-41` — collapses into a single source. Today's surface (per `audit/RESTART-SKETCH-2026-05-03.md:28-34`) carries: eager `JsonParser::parse(input)` at `crates/core/src/grammar/generated/json.rs:3434`; lazy `runtime::json::parse_with(input, &TypedPath)` at `crates/core/src/runtime/json/parse_with.rs:77`; document accessor `JsonDocument::get<T>(path)` at `runtime/json/document.rs:156`. Per the restart sketch, "The eager path is the lazy path with `__EAGER_EMPTY_PATH`. That is the substrate `feedback_no_orthogonal_codepaths` invariant — but it is only honoured inside the dispatcher; the value-API hot path (`JsonDocument::get<T>` post-eager-parse) is a *second* operation that walks the materialized AST." W4b collapses the second-operation pathology: `JsonDocument::get<T>` reroutes through `parse_with(input, path)`; the post-parse linear AST walk vanishes.

Per surgery #10, W4b owns the public wrapper rewrite. The emitter rewrites every grammar's entry to `pub fn parse_with(input: &str, path: &TypedPath<G>) -> Result<<G>Document<'_>, ParseErr>` and `pub fn parse(input: &str) -> Result<<G>Document<'_>, ParseErr> { parse_with(input, &EMPTY_PATH) }`. The W4a-introduced codegen-time elision ensures `parse_with(input, &EMPTY_PATH)` constant-folds away the cursor consultation; the eager path pays no runtime cursor cost.

The closes-the-4196×-gap mechanism for `JsonDocument::get<T>` rewrites at this wave. Per `audit/RESTART-SKETCH-2026-05-03.md:210` (item 6: "Linear pair scan + recursive walk on every path step, post-parse"), today's `JsonDocument::get<T>(path)` traverses the materialised AST tree; sonic-rs's `get_unchecked` is ~0.1× of full parse (per `audit/SOTA-2026-05-03.md:36-37`). The mitigation at the same row: "`Document::get<T>(path)` reroutes through `parse_with(input, path)` (BA's W4 thesis)". W4b makes this concrete: `JsonDocument::get<T>` becomes a thin wrapper over `parse_with(input, path)`; the post-parse linear walk vanishes; the gap closes from 4196× to ≤ 5× per BA-G9.

The Era V failure mode is mitigated because W4b's substrate (the unified `parse_with` public surface) has the same-wave consumer of both eager `parse` and `JsonDocument::get<T>`; both API surfaces consume the unified core in-wave; bench harness verifies BA-G9 closure.

## §2 — Milestones

> **M0 — Unified `pub fn parse_with(input, &path)` per grammar**
>
> *Surface*: `crates/core/src/grammar/generated/{json,bbnf,css_l4,google_sheets,...}.rs` (the nine generated parsers per `audit/MODULES-2026-05-03.md:619-628`); `crates/core/src/backend/rust/emitter/grammar.rs` (the emitter source).
> *Action*: Per the BA.md §Wave summary BA.W4b row, "one `parse_with(input, &path)` per grammar". The emitter rewrites the per-grammar entry function: emit `pub fn parse_with(input: &str, path: &TypedPath<G>) -> Result<<G>Document<'_>, ParseErr>` and `pub fn parse(input: &str) -> Result<<G>Document<'_>, ParseErr> { parse_with(input, &__EAGER_EMPTY_PATH) }`. The four `runtime/<g>/parse_with.rs` files (deletion deferred to BA.W4c per surgery #9) are NOT yet touched; W4b rewrites the GENERATED parser file's entry surface.
> *Gate*: per generated grammar, `pub fn parse_with` is the canonical entry; `pub fn parse` is a one-line wrapper.
> *Exit-criteria*: `rg -n 'pub fn parse_with' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/ | wc -l | tr -d '\n'` returns ≥ 9 (one per grammar); `rg -n 'pub fn parse\(' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/ | wc -l | tr -d '\n'` returns ≥ 9; `cargo check -p bbnf 2>&1 | rg -c 'error\[' | tr -d '\n'` returns `0`.

> **M1 — `JsonDocument::get<T>` reroutes through `parse_with(input, path)`**
>
> *Surface*: `crates/core/src/runtime/json/document.rs` (per `audit/MODULES-2026-05-03.md:940`, 456 LOC; declares `JsonDocument::get<T>(path)` at line 156 per `audit/RESTART-SKETCH-2026-05-03.md:32`).
> *Action*: Rewrite `JsonDocument::get<T>` to invoke `parse_with(self.input, path)` rather than walking the materialised AST. The pre-W4b implementation at `runtime/json/document.rs:370-392` (the linear pair scan + recursive walk per RESTART-SKETCH:210) deletes; the post-W4b implementation is one line: `parse_with(self.input, path).and_then(|doc| extract_<T>(doc))`. The implementation is grammar-agnostic in shape; per BA.W5's direct-to-struct codegen for JSON, the `extract_<T>` becomes implicit at W5.
> *Gate*: `JsonDocument::get<T>(twitter.json, &path)` ≤ 5× the eager parse cost on M1 Pro per BA-G9.
> *Exit-criteria*: Bench harness `cargo bench --bench bench_json -- get_twitter` reports a ratio ≤ 5× vs. `cargo bench --bench bench_json -- twitter`; the bench output JSON's `bbnf_get_twitter / bbnf_parse_twitter` ratio ≤ 5.0.

> **M2 — Apply M1 across BBNF, CSS L4, Sheets `Document::get<T>` reroutes**
>
> *Surface*: `crates/core/src/runtime/bbnf/document.rs` (453 LOC per MODULES:979); `crates/core/src/runtime/css_l4/document.rs` (post-W2.M4 split per `audit/MODULES-2026-05-03.md:952`); `crates/core/src/runtime/google_sheets/document/mod.rs` (150 LOC per MODULES:967).
> *Action*: For each of BBNF, CSS L4, Sheets, rewrite `Document::get<T>` to invoke `parse_with(self.input, path)` per the same pattern as M1. The five cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math) follow the same pattern; their `Document::get<T>` reroutes via the simple-cohort template (per `audit/CENSUS-2026-05-03.md:520`).
> *Gate*: every grammar's `Document::get<T>` reroutes through `parse_with`; the post-parse linear walk is gone everywhere.
> *Exit-criteria*: `rg -n 'fn get<T>' crates/core/src/runtime/ 2>&1 | wc -l | tr -d '\n'` returns ≥ 9; each implementation calls `parse_with(...)` (verified via grep); `cargo nextest run -p bbnf -E 'test(get)' --profile ax-iter 2>&1 | rg 'test result: ok' | wc -l | tr -d '\n'` returns ≥ 1.

> **M3 — Run API tests for all grammars**
>
> *Surface*: `crates/core/tests/parse_with_*.rs` + `crates/core/tests/typed_accessor_surface.rs` (per `audit/CENSUS-2026-05-03.md:392-395` post-W0.M5 inline-test migration; per CENSUS:381-399 the existing test files at `tests/`).
> *Action*: Run `cargo nextest run -p bbnf -E 'test(parse_with) + test(get) + test(typed_accessor)' --profile ax-iter`; verify 100% pass-rate. Per surgery #10's "run API tests" requirement, this milestone is the API-surface verification.
> *Gate*: 100% pass-rate across the API test cohort.
> *Exit-criteria*: `cargo nextest run -p bbnf -E 'test(parse_with) + test(get) + test(typed_accessor)' --profile ax-iter 2>&1 | rg -c 'PASS' | tr -d '\n'` returns ≥ pre-W4b count.

## §3 — Closer gate

```
# BA-G9: get<T>(twitter.json, &path) ≤ 5× full parse on M1 Pro
cargo bench --bench bench_json -- twitter get_twitter \
  | jq '.bbnf_get_twitter.mean / .bbnf_parse_twitter.mean'
                                                     ; expect: ≤ 5.0

# parse and parse_with share one source per grammar
rg -n 'pub fn parse_with' crates/core/src/grammar/generated/ | wc -l
                                                     ; expect: ≥ 9

# zero cursor calls on eager path (verified at W4a; re-verified here)
samply record --save-only -- cargo bench --bench bench_json -- twitter
# inspect profile for PathCursor::decide / current_kind / match_field
# inclusive on eager path: < 0.5% samples

cargo nextest run -p bbnf -E 'test(parse_with) + test(get) + test(typed_accessor)'
                                                     ; expect: 100% pass
```

`get<T>` ≤ 5× full parse; `parse` and `parse_with` share one source per grammar; samply trace shows zero cursor calls on eager path.

## §4 — Invariants

§I1. **Lock 3** (cursor + byte-skip unified, cursor branch elided when path empty). One parse implementation per grammar — `parse_with(input, &path)`. The empty-path case (`__EAGER_EMPTY_PATH`) elides cursor calls entirely (W4a) so the eager fast path pays no consultation cost.

§I2. **No orthogonal codepaths** (per `feedback_no_orthogonal_codepaths`). The eager `parse` and lazy `parse_with` share one source; W4a's codegen distinguishes the empty-path case by constant-folding the cursor away.

§I3. **One codegen path** (per `feedback_one_codegen_path`). The `parse_with` emit is the single path; no fallback `parse`-without-`parse_with` survives.

§I4. **Beat sonic-rs target** (extended). BA-G9 closes the 4196× → ≤ 5× gap; sonic-rs's `get_unchecked` at 0.1× full parse is the asymptote BB pursues; W4b reaches ≤ 5×.

§I5. **No workarounds** (per `feedback_no_workarounds`). The post-parse linear walk in `Document::get<T>` (today's pathology) deletes; not preserved as a fallback.

§I6. **Lock 9** (slice-borrow primary). The post-W4b `parse_with(input: &'p str, &path) -> Result<<G>Document<'p>>` returns slice-borrowed; per `audit/SOTA-2026-05-03.md:122-123`, the default surface is `&'i str` slice. The `parse_in(input, &bump)` and `parse_owned(input)` escape hatches route to BB.W4.

## §5 — Risks specific to this wave

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| Cursor-unified parse impl loses path-driven test coverage that exercises non-eager paths | Medium | M2 + M3 close: `cargo nextest run -p bbnf -E 'test(parse_with)'` regression | Per the BA.md §Risks row 5: BA.W4a/W4b retain `parse_with(input, path)` for non-eager; only the eager-fast-path constant-folds out (W4a); `tests/parse_with_*.rs` pass-rate is gated |
| Bench harness's `bbnf_get_twitter` measurement is unstable (high variance on M1 Pro) | Low | Multiple bench runs; per `feedback_bench_single_run` "never run benchmarks in separate sequential commands; single invocation only" | Single invocation `cargo bench --bench bench_json -- twitter get_twitter` per `feedback_bench_single_run`; report mean of 100 iterations |
| `Document::get<T>` reroute through `parse_with` regresses CSS L4 + BBNF (whose specialised builders accumulate frame state) | Medium | M2 close: `cargo nextest run -p bbnf -E 'test(css_l4)'` + `'test(bbnf)'` | The reroute preserves CSS L4 / BBNF builder semantics: `parse_with` invokes the same per-grammar StructBuilder; the only change is the entry surface; behaviour is preserved |
| `__EAGER_EMPTY_PATH` argument leaks into the eager fast path (the runtime path argument carries through) | Medium | M0 cargo expand: verify the eager `parse(input)` body monomorphises away the path argument | Per surgery #10 ("No runtime path argument may remain on the eager fast path"), the codegen-time elision (W4a) constant-folds the path argument; W4b verifies post-expansion |

## §6 — Cross-references

- **Closes BA-G9** (per BA.md §Hard gates): `JsonDocument::get<T>(twitter.json, &path)` ≤ 5× the eager parse cost on M1 Pro.
- **Honours Lock 3** (per BA.md §13-Lock honoured): cursor + byte-skip unified.
- **Carry-tags TO BB**: BA→BB.C3 (per BA.md): "Cursor-unified `parse_with` + `__EAGER_EMPTY_PATH` substrate; BB extends this to all grammars at BB.W2." W4b produces this carry-tag's substrate.
- **Preceding wave**: BA.W4a (private parse core).
- **Following wave**: BA.W4c (legacy `parse_with.rs` deletion).
- **Routed-carry**: none specific to W4b.

## §7 — Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target | Notes |
|---|---|---|---|
| `cargo check --workspace --profile ax-iter` | ≤ 22 s | error count: 0 | Post-W4b check |
| `cargo nextest run -p bbnf -E 'test(parse_with) + test(get)' --profile ax-iter` | ≤ 18 s | 100% | API test cohort |
| `cargo bench --bench bench_json -- twitter get_twitter` | ≤ 60 s | ratio ≤ 5.0 | BA-G9 closer-gate |
| `rg -n 'pub fn parse_with' crates/core/src/grammar/generated/ \| wc -l` | < 1 s | ≥ 9 | Unified surface gate |

## §8 — Verification artefacts

W4b produces no audit artefact directly; bench output JSON + samply profile JSON are the verification surface.

## §9 — Audit lane forecast

Lane 02 sequencing: same-wave consumer (bench harness + API tests) verifies. Lane 03 cohesion: ownership of `parse_with` is JSON-scoped at BA close per the BA.md §Carry-tags table (C13 resolved by JSON-only ownership). Lane 04 SOTA: BA-G9 marked non-SOTA per surgery #12.
