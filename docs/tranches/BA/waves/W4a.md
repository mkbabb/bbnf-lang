# BA.W4a — Private Parse Core + Eager Cursor Elision

**Thesis** (the codegen-time `__EAGER_EMPTY_PATH` constant-fold lands first; cursor consultations elide entirely on the eager path; cursor's `Skip` decision generates byte-skip code at codegen, not at runtime; the public `parse` / `parse_with` / `Document::get<T>` wrappers and the legacy `parse_with.rs` deletions land at W4b and W4c respectively per surgery #10). **Closer-gate** (generated eager path has zero `cursor.decide`/`cursor.current_kind`/`cursor.match_field` calls; samply cursor inclusive < 0.5% on eager path).

## §1 — Deliverable

Hereupon the eager parse path's cursor consultation evaporates by codegen-time constant-folding. Today's `static __EAGER_EMPTY_PATH: LazyLock<TypedPath<Json,&str>>` declaration at `crates/core/src/grammar/generated/json.rs:3443` (per `audit/RESTART-SKETCH-2026-05-03.md:51-53`) constructs a `PathCursor::new(&EMPTY_PATH, |_,_,_| ParseFully)`; the cursor consultation calls (per `audit/RESTART-SKETCH-2026-05-03.md:216`, item 12, "`cursor.decide`, `cursor.current_kind`, `cursor.match_field` calls return constant `ParseFully` for the empty-path eager case") return constant `ParseFully` for empty-path. W4a hoists this constant-fold to codegen: when the path is statically empty, the cursor consultation generates as a no-op; when the path is non-empty, the cursor's `Skip` decision generates byte-skip code at codegen time (NOT runtime branch).

Per surgery #10 ("Split BA.W4 into **W4a** (private parse core + eager empty-path cursor elision) and **W4b** (reroute `parse` and `Document::get<T>`, run API tests). No runtime path argument may remain on the eager fast path."), W4a owns ONLY the codegen-time elision and the `Skip`-byte-skip generation. The public `parse` / `parse_with` wrapper rewrites land at W4b; the legacy `parse_with.rs` deletions land at W4c per surgery #9.

The exit-criteria correction per audit C03-2 ("W4.M0 exit criteria cites BA-G2 zero `Vec<OpenFrame>::clone`, but BA-G2 is explicitly W5-owned"): W4a.M0's exit-criteria is **cursor-only** evidence — generated eager path has zero `cursor.decide/current_kind/match_field` calls and samply cursor inclusive < 0.5%. The `Vec<OpenFrame>::clone` retirement is BA-G2 (W5-owned).

The Era V failure mode is mitigated because W4a's substrate (the codegen-time elision + Skip-byte-skip) has the same-wave consumer of every per-grammar generated parser at next-wave-boundary (W4b reroutes the public API; W5 consumes the JSON-direct-projection-emit). The W4a milestone gates verify the elision is observable post-regen via samply.

## §2 — Milestones

> **M0 — Codegen-time `__EAGER_EMPTY_PATH` constant-fold**
>
> *Surface*: `crates/core/src/backend/rust/emitter/grammar.rs` (per `audit/MODULES-2026-05-03.md:827`, 468 LOC; declares the per-grammar entry function); the existing `static __EAGER_EMPTY_PATH: LazyLock<TypedPath<Json,&str>>` declaration at `crates/core/src/grammar/generated/json.rs:3443`.
> *Action*: Rewrite the emitter at `backend/rust/emitter/grammar.rs` to detect the empty-path case at codegen time. When the path is statically empty (the eager `parse` entry), the emitted body skips cursor consultation entirely — the cursor's `decide` / `current_kind` / `match_field` calls do not appear in the emitted source. The non-empty path case retains cursor consultation; the cursor's `Skip` decision generates byte-skip via `*p += skip_count` rather than calling into the cursor at runtime.
> *Gate*: `samply` profile of `JsonParser::parse(twitter.json)` shows zero `PathCursor::decide` / `current_kind` / `match_field` calls; per `audit/RESTART-SKETCH-2026-05-03.md:216` (item 12), the constant-fold removes these calls.
> *Exit-criteria* (corrected per C03-2): generated eager path has zero `cursor.decide/current_kind/match_field` calls in `crates/core/src/grammar/generated/json.rs`; samply cursor inclusive < 0.5%. Verifiable via `cargo expand -p bbnf 2>&1 | rg -c 'cursor\.decide\|cursor\.current_kind\|cursor\.match_field' | tr -d '\n'` on the eager-path sites: returns `0`.

> **M1 — Verify empty-path detection at codegen for all 9 grammars**
>
> *Surface*: per-grammar `crates/core/src/grammar/generated/<g>.rs`; the BA.W1 metadata-driven `EmitStrategy::for_grammar` resolver.
> *Action*: For each of the 9 grammars (json, bbnf, css_l4, google_sheets, css_pretty, ebnf, bnf, csv, math), run `cargo xtask regen --grammar <g>`; verify the post-regen `generated/<g>.rs` carries the cursor-elided body for the eager `parse` entry. The metadata-driven strategy resolver (BA.W1) ensures the per-grammar emit consumes the same elision logic.
> *Gate*: `rg -n 'cursor.decide\|cursor.current_kind\|cursor.match_field' crates/core/src/grammar/generated/<g>.rs` returns 0 on the eager-path entry function (search constrained to within the `pub fn parse(input: &str)` body's siblings post-regen).
> *Exit-criteria*: per-grammar verification commits; aggregate `for g in json bbnf css_l4 google_sheets css_pretty ebnf bnf csv math; do cargo xtask regen --grammar $g; done` exits 0.

> **M2 — Cursor `Skip` decision generates byte-skip code at codegen**
>
> *Surface*: `crates/core/src/path/cursor.rs` (431 LOC per `audit/MODULES-2026-05-03.md:679`; declares `PathCursor` state machine + `Decision` + `SegmentKind`); `crates/core/src/backend/rust/emitter/path_plan.rs` (356 LOC per MODULES:829; "Path-plan emission (lazy bail-out paths)").
> *Action*: Rewrite the codegen at `path_plan.rs` to emit byte-skip code directly when the cursor would return `Skip`. Today's emission consults the cursor at runtime; W4a hoists the consultation to codegen — the emitter inspects the `Decision` shape per rule and emits direct byte-skip (`*p += <const>`) when the shape is statically known to be skippable. Per BA.md §13-Lock L3 ("Cursor + byte-skip unified, with cursor branch elided when path empty"), the cursor branch elides at codegen.
> *Gate*: emitter source generates byte-skip directly; the runtime cursor's `Skip` arm is dead code on the eager path.
> *Exit-criteria*: `rg -n 'Decision::Skip' crates/core/src/grammar/generated/' 2>&1 | wc -l | tr -d '\n'` returns 0 on the eager-path-only sites (verifiable post-regen).

> **M3 — Samply verification of cursor-elision**
>
> *Surface*: post-W4a generated parser at `crates/core/src/grammar/generated/json.rs`; samply harness.
> *Action*: Run `samply record --save-only -- cargo bench --bench bench_json -- twitter` (per `feedback_samply_symbols`, samply needs `debug=true` + interactive `samply record` for symbol resolution); inspect the resulting profile for: (a) absence of `PathCursor::decide`/`current_kind`/`match_field` on the eager path; (b) presence of `PathCursor::*` only on the lazy `get<T>` path. Per `feedback_actual_profiling`, run the actual profiler; do not guess from static analysis.
> *Gate*: samply profile inclusive samples for cursor methods on the eager path < 0.5%.
> *Exit-criteria*: bench output JSON inspected for cursor symbols (manual verification at W4a close); the inclusive sample share < 0.5%.

## §3 — Closer gate

```
# Generated eager path has zero cursor consultation
rg -n 'cursor\.decide\|cursor\.current_kind\|cursor\.match_field' \
  crates/core/src/grammar/generated/json.rs                      ; expect: 0 on eager-path sites

# Samply cursor inclusive < 0.5% on eager path
samply record --save-only -- cargo bench --bench bench_json -- twitter
# inspect profile for PathCursor::decide / current_kind / match_field
# inclusive on eager path: < 0.5% samples

# Generated LOC window per surgery G06-3
wc -l crates/core/src/grammar/generated/json.rs                  ; expect: ≤ 3,700
wc -l crates/core/src/grammar/generated/bbnf.rs                  ; expect: ≤ 22,000
wc -l crates/core/src/grammar/generated/css_l4.rs                ; expect: ≤ 110,000

cargo nextest run -p bbnf -E 'test(parse_with)'                  ; expect: 100% pass
```

## §4 — Invariants

§I1. **Lock 3 precursor** (cursor + byte-skip unified, cursor branch elided when path empty). The codegen-time elision is the unification mechanism's first phase. The empty-path case constant-folds out at codegen; the non-empty path case retains cursor consultation; the runtime cost on the eager path is zero.

§I2. **No orthogonal codepaths** (per `feedback_no_orthogonal_codepaths`). The eager `parse` and lazy `parse_with` share one source; the codegen distinguishes the empty-path case by constant-folding the cursor away, not by emitting two parsers.

§I3. **One codegen path** (per `feedback_one_codegen_path`). ONE monolithic codegen path; no combinator fallback.

§I4. **No deferrals** (per `feedback_no_deferrals`). The codegen-time elision lands in W4a; the public wrapper rewrite at W4b consumes W4a's substrate; the legacy deletion at W4c consumes W4b's substrate. Each sub-wave has its consumer in the next sub-wave.

§I5. **Generated LOC window** (per BA-G10 + surgery G06-3). Per-grammar windows: `json.rs ≤ 3,700`, `bbnf.rs ≤ 22,000`, `css_l4.rs ≤ 110,000`, aggregate ≤ +5% from W2.

## §5 — Risks specific to this wave

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| `__EAGER_EMPTY_PATH` LazyLock initialisation cost dominates on cold start | Low | M3 samply profile of cold-cache `JsonParser::parse(twitter.json)` | The LazyLock is a one-time initialisation at parse-fn first-call; per `feedback_no_warm_benches`, cold per-parse measurement is the gate; the LazyLock cost is amortised across parse-call N |
| Codegen-time `Skip` byte-skip emission misclassifies a rule as statically skippable when it depends on runtime path content | Medium | `cargo nextest run -p bbnf -E 'test(parse_with)'` covers the non-eager path | The static analysis is conservative: only when the path is empty AND the rule's structural alphabet does not depend on path content does the byte-skip emit; per `feedback_pluggable_components`, the cost-model decides |
| The cursor's `Skip` decision path generates byte-skip code that is incorrect for variable-width keys (e.g., a JSON object's quoted key whose length depends on input) | Medium | M2 close: `cargo nextest run -p bbnf -E 'test(parse_with) + test(get)'` | Variable-width skip cases retain runtime cursor consultation; the codegen-time hoist applies only to statically known skip distances |
| Per-grammar regen cascades into emitter source rewrite at multiple sites | Medium | M1 close: per-grammar `cargo xtask regen --grammar <g>` exits 0 | The emitter source rewrite at M0 is the central change; per-grammar regen is mechanical |

## §6 — Cross-references

- **Honours Lock 3** (precursor) per BA.md §13-Lock honoured row L3.
- **Carry-tags produced**: none direct to BB; W4a's outputs are consumed by W4b (public wrappers).
- **Preceding wave**: BA.W3c (legacy runtime path retiral).
- **Following wave**: BA.W4b (public wrappers).
- **Routed-carry**: none specific to W4a.

## §7 — Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target | Notes |
|---|---|---|---|
| `cargo check --workspace --profile ax-iter` | ≤ 22 s | error count: 0 | Post-W4a check |
| `cargo xtask regen --check` | ≤ 60 s (pre-halving) | exit 0 | Per-grammar regen verification |
| `cargo bench --bench bench_json -- twitter` | ≤ 60 s | not yet ≤ 400 µs (BA-G1 closes at W5) | W4a regression check |
| `samply record --save-only -- cargo bench --bench bench_json -- twitter` | ≤ 90 s | exit 0 | M3 samply verification |
| `cargo expand -p bbnf 2>&1 \| rg 'cursor.decide\|cursor.current_kind\|cursor.match_field' \| wc -l` | ≤ 90 s | 0 hits on eager path | Post-expansion verification that cursor consultation is constant-folded out |

## §8 — Verification artefacts

W4a produces no audit artefact directly; the samply profile JSON at M3 is the verification surface.

## §9 — Audit lane forecast

Lane 02 sequencing: same-wave consumer (samply profile) verifies. Lane 03 cohesion: M0 exit-criteria correction closes C03-2. Lane 06 budget: generated LOC windows per surgery G06-3 enforced.
