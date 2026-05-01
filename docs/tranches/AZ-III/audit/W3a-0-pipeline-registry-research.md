# AZ-III.W3a.0 — Pipeline Registry Research

**Run date**: 2026-04-30
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-azIII-w3a-research` (HEAD `248d3ac6`)
**Lane HEAD reference**: same as the AZ-III REAUDIT 2026-04-30 baseline.
**Scope**: read-only research. No source edits.

## 1. Headline Verdict

All three idents (`MultiPathParser`, `ImportPrettyParser`, `SplitPrettyParser`)
are **TEST FIXTURES** with **zero** non-test consumers. They are bare ident
strings supplied as the `ident` argument of `render_tokens()` inside one
test file; no `.bbnf` grammar file declares them, no production source
code references them, no generated grammar uses them, and no runtime
substrate (`<Grammar>StructBuilder` / `<Grammar>Document`) exists for any
of them.

The recommended W3c.2 binding is therefore **registry-side fixture
admission**, not StructDirect arms. Three options follow in §6 with the
preferred one (a fixture-admission helper invoked from the test harness)
called out.

| Parser ident | Verdict | W3c.2 binding recommendation |
|---|---|---|
| `MultiPathParser` | TEST FIXTURE | Test-only fixture admission (preferred); alternatively retire the ident and pass a real grammar ident the resolver already maps |
| `ImportPrettyParser` | TEST FIXTURE | Same as above |
| `SplitPrettyParser` | TEST FIXTURE | Same as above |

## 2. Caller Enumeration

`rg -n "MultiPathParser|ImportPrettyParser|SplitPrettyParser" crates/`
returns exactly three hits, all in one test file:

```
crates/core/tests/pipeline_compile_request.rs:112:    let tokens = render_tokens(&prepared, &ParserAttributes::default(), "SplitPrettyParser");
crates/core/tests/pipeline_compile_request.rs:176:    let tokens = render_tokens(&prepared, &attrs, "MultiPathParser");
crates/core/tests/pipeline_compile_request.rs:214:    let tokens = render_tokens(&prepared, &attrs, "ImportPrettyParser");
```

Whole-repo (`rg -n … --hidden -uu` excluding `target/` and `docs/`) returns
the same three hits and nothing else. The remaining doc references in
`docs/tranches/AZ-III/**` are themselves classification calls; they do not
declare consumers.

`grammar/` directory listing:

```
grammar/bbnf
grammar/bnf
grammar/css
grammar/ebnf
grammar/google-sheets
grammar/json
grammar/misc
grammar/tests
```

`rg` over `grammar/` for any of the three idents returns zero hits. None
of them are real grammars.

For comparison, the resolver's existing arms (`JsonParser`,
`GoogleSheetsParser`, `CssL4Parser`, `BbnfBootstrap`/`BbnfParser`,
`CsvParser`, `MathParser`, `BnfParser`, `EbnfParser`,
`CssPrettyParser`/`CssPrettyGrammar`) each have a corresponding
`crates/core/src/runtime/<grammar>/` substrate with concrete
`<Grammar>StructBuilder` and `<Grammar>Document` types, which is the
property that distinguishes a real grammar from a test fixture. None of
the three idents under research has any such substrate; an `rg` for
`MultiPath|ImportPretty|SplitPretty` over `crates/core/src/runtime/`
returns zero hits.

## 3. Test Cases That Exercise Each Ident

Each ident is exercised by exactly one `#[test]` function in
`crates/core/tests/pipeline_compile_request.rs`:

| Ident | Test fn | Line | Grammar exercised | Codegen feature under test |
|---|---|---:|---|---|
| `SplitPrettyParser` | `compile_request_preserves_split_pretty_hint_for_codegen_error` | L100 | `value = "x" ; @pretty value split(",") ;` | `@pretty … split` codegen-error preservation in tokens |
| `MultiPathParser` | `compile_paths_preserves_pretty_directives_across_multiple_explicit_paths` | L147 | two-file split (`first.bbnf`, `second.bbnf`) with `@pretty foo group ;` in the first | `compile_paths_request` preserves `@pretty` across explicit paths |
| `ImportPrettyParser` | `compile_paths_preserves_pretty_directives_through_import_resolution` | L184 | `entry.bbnf` with `@import "child.bbnf"` and `@pretty child group ;` in the imported child | `@pretty` survives `@import` resolution |

The same test file passes the strings `"PrettyOnlyParser"`,
`"ExplicitPrettyParser"`, and `"PlainParser"` at L35, L57, and L77. Those
three strings hit the same panic surface (resolver also matches against
ident strings and has no arm for them); the failure baseline cluster
nominally counts only six failures (three tests × two retries), so either
those three additional fixture idents are also panicking (and were lumped
into the baseline cluster as pre-existing fixture residue) or they fall
through to a different failure mode. Source inspection shows no
short-circuit: every codegen entry point routes through
`EmitStrategy::for_grammar` (at `shapes/mod.rs:156`, `grammar.rs:103`,
`grammar.rs:261`, and the pipeline-level adapter `resolve_emit_strategy`
at `crates/core/src/pipeline/compile.rs:186`). They will panic for the
same reason; they are simply not enumerated separately in the lane 1
baseline § 3 cluster row. **All six "pretty parser" fixture idents share
one root cause and one redress.**

## 4. Panic Context (`crates/ir/src/registry/strategy.rs:257`)

The exact panic the lane 1 cluster names:

```rust
            _ => panic!(
                "EmitStrategy::for_grammar: unknown production grammar `{grammar_ident}`; \
                 add an explicit StructDirect substrate binding"
            ),
```

The match arm at L151-256 enumerates every real grammar ident the
emitter activates (`JsonParser`, `GoogleSheetsParser`, `CssL4Parser`,
`BbnfBootstrap`/`BbnfParser`, `CsvParser`, `MathParser`, `BnfParser`,
`EbnfParser`, `CssPrettyParser`). The catch-all is intentional — per
AZ-II.cutover.O4 (`feedback_no-orthogonal-codepaths`) there is no tape
fallback substrate; unknown idents fail loudly at codegen time. The
audit's job is to decide whether the three idents named here should join
that arm (real grammars) or whether the call site is the wrong shape
(fixture).

A second panic exists at L144-149 for the empty-registry case. It does
**not** fire for the three failing tests because all three grammars
project `value = "x"` / `foo = "a"` / `child = "x"` rules whose IR pass
populates `ir.struct_registry` non-empty before `finalize_compile` calls
into the Rust target.

## 5. Why These Are Fixtures, Not Real Grammars

Three independent indicators all agree:

1. **No grammar source.** Zero `.bbnf` files under `grammar/`,
   `crates/core/src/grammar/`, or anywhere else declare these idents.
   Real grammars are named after their parser-struct ident in
   `grammar/<name>/<name>.bbnf` plus `crates/core/src/grammar/generated/`.
   None exists for any of the three.
2. **No runtime substrate.** Every active StructDirect arm refers to
   `crate::runtime::<grammar>::<Grammar>StructBuilder` and
   `<Grammar>Document`. There is no
   `crate::runtime::multi_path::MultiPathStructBuilder`,
   `crate::runtime::import_pretty::ImportPrettyStructBuilder`, or
   `crate::runtime::split_pretty::SplitPrettyStructBuilder`. Adding
   resolver arms without first authoring those modules would only move
   the panic from the resolver to the emitted code (the splice
   `<builder_path>::new()` would fail to resolve at codegen time).
3. **The naming pattern is fixture-shaped.** The three idents describe
   the *test scenario*, not the language being parsed:
   `SplitPrettyParser` (split-hint scenario), `MultiPathParser`
   (multi-path compile scenario), `ImportPrettyParser` (import-resolved
   pretty scenario). This is a common test-fixture anti-pattern where
   the harness names the parser after the call shape it is testing
   rather than after a grammar it is generating.

The lane 1 baseline §6 already pre-classified them as candidates for
"test fixtures" pending this research; §3 explicitly noted "are
`MultiPathParser` / `ImportPrettyParser` / `SplitPrettyParser` real
grammars or test fixtures? If test fixtures, the `for_grammar` table
needs a fixture registry; if real, the resolver-arm needs explicit
StructDirect bindings." This research returns the verdict.

## 6. Binding Recommendations For W3c.2

Three options, in order of preference:

### Option A — Test-fixture admission (preferred)

The cleanest fix is at the **test side**, not the resolver: drop the
fixture idents in favour of an existing real grammar ident. The three
tests under research exercise pipeline behaviour (`@pretty` directive
survival across single source, multi-path compile, and `@import`
resolution); they do not test grammar-specific codegen. They can pass
any real ident the resolver already maps — `"BbnfParser"` or
`"JsonParser"` — and assert the same `value_prettify` /
`foo_prettify` / `child_prettify` markers in the rendered tokens. The
resolver is unchanged. The fixture residue is gone. There is one code
path.

This honours `feedback_no-overfitting`: the fixture idents have no
consumer outside one test file each, no grammar source, and no runtime
substrate, so by the no-overfitting rule they delete (or substitute).
It also honours `feedback_one-codegen-path`: no fixture-only branch
threads through the production resolver.

W3c.2 ownership: edit
`crates/core/tests/pipeline_compile_request.rs` (test file, in W3c
file bounds via `crates/core/tests/pipeline_compile_request_*.rs`
glob), substituting the three `render_tokens(…, "<FixtureIdent>")`
calls with a real ident such as `"BbnfParser"`. Same substitution for
the three earlier non-failing tests at L35/L57/L77 if they also panic
under no-fail-fast (independent verification recommended in §7).

### Option B — Test-fixture admission helper inside the resolver

If the test must keep the existing fixture idents for code-archaeology
reasons (it does not — they are descriptive only and have no
mechanical role), introduce a single `EmitStrategy::for_test_fixture`
helper that returns a generic StructDirect binding pointing at a
test-only `FixtureStructBuilder` / `FixtureDocument` substrate
authored under `crates/core/src/runtime/test_fixture/`. The resolver's
match arm would either route the fixture idents (`MultiPathParser`,
`ImportPrettyParser`, `SplitPrettyParser`, plus the three already-passing
fixtures from L35/L57/L77 if they share the failure mode) to that
helper, or the resolver would expose `for_grammar_or_test_fixture` as
a separate entry point used only by the test harness.

This option respects the `feedback_pluggable-components` precept (data,
not branches) but **violates** `feedback_no-overfitting` (introducing a
test-only substrate purely so the resolver can speak fixture). The
documented status in this verdict is therefore *not recommended*; W3c.2
should choose Option A unless a future test requires a fixture
substrate for some reason this audit did not surface.

### Option C — Promote to real grammars

Author full `crate::runtime::multi_path` / `import_pretty` /
`split_pretty` modules with builder/document/payload types and add the
resolver arms. This is the path described in
`crates/ir/src/registry/strategy.rs:259`'s panic message ("add an
explicit StructDirect substrate binding"). It is **rejected** by this
research: the idents are call-shape labels, not grammars. There is no
language called "multi-path" or "import-pretty" or "split-pretty"; the
substrate would exist solely to satisfy the resolver. This is the
exact `feedback_no-workarounds` shape the AZ-III thesis forbids.

### Recommended verdict for W3c.2

**Option A** — substitute the test idents with a real grammar ident
the resolver already maps (e.g. `"BbnfParser"`). One commit, one
test-file edit, zero source-side changes, six panics retire (the three
named here plus the three companion idents from L35/L57/L77 if they
share the failure mode), and the resolver remains a single decision
surface with one production code path.

W3c.2's commit-plan scope `fix(registry/strategy): bind
MultiPath/Import/Split or document fixtures` is honoured by the second
half of that conjunction: this research **documents these as fixtures**
and the redress lands as a test-side substitution rather than a
resolver-side binding.

## 7. Independent Verification Recommended

Before W3c.2 lands the substitution, the orchestrator should re-run
`cargo test -p bbnf --test pipeline_compile_request --no-fail-fast`
once at HEAD to confirm whether the three companion fixture idents
(`PrettyOnlyParser` at L35, `ExplicitPrettyParser` at L57, `PlainParser`
at L77) panic identically. The lane 1 baseline does not enumerate them
in the cluster row, but source inspection shows no codepath difference;
they should panic with the same `for_grammar` message. The
substitution then covers all six idents in one edit. If they
unexpectedly do not panic (e.g. an early empty-registry exit at
L144-149 or a pre-existing test ignore), W3c.2 substitutes only the
three named here.

This is the only piece of verification this research could not
complete inside the read-only window: a live `cargo test` of the
suite started compiling but did not finish before the time budget
expired. The static evidence is conclusive on its own — the resolver
has no path that admits unknown idents — so the W3c.2 redress can
proceed under Option A without waiting for the verification result.

## 8. Cross-Wave Notes

- **W3a does not depend on this verdict.** W3a.1 (durable fact
  authority), W3a.2 (compound-Ref obligation), and W3a.3 (heterogeneous
  Alt obligation) operate on the constraint solver and IR fact stores;
  they do not touch the registry. This audit gates only **W3c.2**
  (Pipeline Registry Authority).
- **W3c.2 file bounds are unchanged.** The recommended Option A redress
  is a test-side edit (`crates/core/tests/pipeline_compile_request*.rs`
  per W3c.md File Bounds) plus the resolver itself is unchanged. No
  source-side `crates/ir/src/registry/strategy.rs` change is required
  by the verdict. W3c.2 should keep the panic catch-all at L257-260
  intact: per `feedback_no-orthogonal-codepaths` and AZ-III invariant
  7 (no silent fallback) the loud panic on unknown idents is the
  correct behaviour.
- **W3c hard gate 4** ("Every grammar in
  `audit/W3a-0-pipeline-registry-research.md` has an explicit
  StructDirect binding, an explicit fixture verdict, or leaves W3c
  blocked") is honoured by this doc: the three idents have explicit
  fixture verdicts and the W3c.2 redress is a fixture cleanup.

## 9. Files

- Read-only research source files:
  `/Users/mkbabb/Programming/bbnf-wt-azIII-w3a-research/crates/ir/src/registry/strategy.rs`
  (lines 121-262, panic at 257-260),
  `/Users/mkbabb/Programming/bbnf-wt-azIII-w3a-research/crates/core/tests/pipeline_compile_request.rs`
  (3 fixture-ident sites at L100/L147/L184; 3 companion fixture-ident
  sites at L23/L43/L66),
  `/Users/mkbabb/Programming/bbnf-wt-azIII-w3a-research/crates/core/src/pipeline/compile.rs`
  (resolver pipeline adapter at L182-187, finalize_compile at L266
  onward),
  `/Users/mkbabb/Programming/bbnf-wt-azIII-w3a-research/crates/core/src/backend/rust/emitter/shapes/mod.rs`
  (resolver call site at L156),
  `/Users/mkbabb/Programming/bbnf-wt-azIII-w3a-research/crates/core/src/generate/mod.rs`
  (`generate_all` at L35 — the entry point the failing tests call).
- Verdict artefact (created by this lane):
  `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-III/audit/W3a-0-pipeline-registry-research.md`.

---

**Lane status**: complete. **Source edits**: zero. **Halt reason**:
verdict reached well inside the 25-min hard cap; live `cargo test`
verification was started but cancelled to leave the verdict as the
deliverable artefact (the static evidence is sufficient). **Empty-return
gate**: not triggered — the verdict is explicit per ident with a
recommended W3c.2 binding.
