# PROTO-2 — LOCK-3 pass-1, probe 2: PROVE-12 as corollary (family 3D STAGED-RESIDUAL)

**VERDICT LINE: REFUTED (named negative) — RAN_UNMEASURED.** PROVE-12's
const-generic residual is NOT reproducible "from the annotated IR alone" on the
baseline this probe runs against: the CSS recognizer is a raw-string template, so
the delimiter set is not an IR node the BTA can lift. `binding_times` + the split
emitter cannot emit PROVE-12's patch; byte-identity fails structurally, before any
perf question. The corollary's *weaker* claim (delimiter set = clean S0 island)
is separately corroborated by the live tree's `ScanAlphabet`, but with a
different residual — never byte-identical to PROVE-12.

Worktree: `/Users/mkbabb/Programming/bbnf-lang/.claude/worktrees/wf_63419230-36d-26`
HEAD `d2b32f6e3` — **exactly the PROVE-12 baseline** (sk-v22 graduation runtime).
Host aarch64; all cargo confined to the worktree `skinny/` workspace. No x86.

---

## QUESTION

Is PROVE-12's hand patch (the CSS `find_component_delim` residualized to a
const-generic `u128` delimiter mask) an EMERGENT property of a stage-annotated IR
— i.e. does `binding_times` + a split (S0/S1) CSS leaf emitter reproduce the
const-generic residual byte-identically via `cargo xtask regen-css`, WITHOUT a
hand template edit? If yes, family 3D graduates from SPEC-only and the type plane
is subsumed as stage-0 erasure. PASS = regen-css output byte-identical to
`PROVE-12/patch.diff` + parity checksum (track1_full=1, track1_rich=17839,
mask-fold MATCH) + `find_component_delim<const DELIMS>` emitted, not authored.

## METHOD

1. **STEP 1 (ran).** Added `binding_times(grammar) -> Vec<Stage>` beside
   `nullability` in `crates/ir/src/lib.rs`: same monotone `Changed`-flag fixpoint,
   `Stage::{Comptime(S0), Runtime(S1)}` with a `join` lattice (S1 dominates).
   Seeds: `Literal`/`Regex`/`Annotation` = S0; `Ref`/`Repeat` (cursor-binding
   spine) = S1; `Seq`/`Alt`/`Optional` join operands. Added `residual_islands`
   (maximal S0 subtrees spliced into the S1 spine — the residual-count artifact)
   and `stage_leaks` (the no-silent-epsilon guard: an S0 leaf raised to S1 is
   raised/reported, never demoted). Test file `crates/ir/tests/binding_times.rs`.
2. **STEP 2 (structurally refuted — not run).** The brief's STEP 2 (split the CSS
   leaf emitter; evaluate the S0 delimiter island to a `u128`; splice
   `const fn delim_mask` + `find_component_delim<const DELIMS>`; thread stage_map
   through `LowerCtx`) targets seams that, on this baseline, do not carry the CSS
   delimiter set — see FINDINGS. It cannot be run as specified without first
   replacing the raw-string CSS template with an IR-driven emitter (a
   whole-subsystem rewrite, out of probe scope and against parsimony).

## WHAT RAN

`cargo test -p ir --test binding_times` (worktree `skinny/`, `test` profile):

```
running 4 tests
test a_literal_fed_a_ref_is_a_seq_not_a_leak ... ok
test clean_delimiter_grammar_has_no_stage_leaks ... ok
test residual_island_count_matches_the_four_stop_sets ... ok
test stop_islands_are_comptime_scan_is_runtime ... ok
test result: ok. 4 passed; 0 failed; finished in 0.00s
```

`binding_times` runs on a synthetic GrammarIr mirroring the CSS component scan
(the four STOP sets `;{}`, `{};`, `;}`, `:{};`, each an `Alt` of single-byte
`Literal`s, each wrapped in its own `Repeat` scan — as PROVE-12's five
`find_component_delim` call sites collapse to four distinct sets). Result: every
STOP-set Alt and every delimiter literal classifies **S0 (Comptime)**; every
`Repeat` scan spine classifies **S1 (Runtime)**; **residual_islands = 4**; **0
stage leaks**. The BTA machinery the corollary needs works.

## FINDINGS — why byte-identity is refuted

1. **On the baseline, the CSS recognizer is a raw-string template, not IR-driven
   output.** `crates/codegen/src/runtime_generator.rs:701`
   `const CSS_GENERATED_RS: &str = r#"…"#`; `regen-css` writes
   `("generated.rs", normalize(CSS_GENERATED_RS))` (line 91) — `generated.rs` is
   the template text verbatim (modulo whitespace normalize).
2. **The delimiter sets are hand-authored `&str` text, never IR nodes.**
   `find_component_delim(self.pos, b";{}")` at lines 1193/1212/1250/1285/1387 —
   `b";{}"` etc. are characters *inside* the raw string constant. They never enter
   `GrammarIr::exprs`, never reach `nullability`/`binding_times`. The CSS grammar
   is not lowered to `GrammarIr` and re-emitted; the recognizer is a hardcoded
   template.
3. **Therefore `binding_times` cannot see the CSS delimiter island** — it can only
   classify a *synthetic mirror* (what the tests do). No BTA over the annotated IR
   lifts a delimiter set that is not in the IR. The brief's STEP-2 seams confirm
   the mismatch: `derive_recognizers` (`passes/src/lib.rs:338`) harvests only the
   JSON/structural single-byte alphabet `{}[],:"` from `GrammarIr` — not the CSS
   `;{}`/`:` delimiters; and `LowerCtx` (`codegen/src/lib.rs`) serves the
   sink-only (JSON/Sheets) lowering, not the CSS template path.
4. **So PROVE-12's patch is, on this baseline, a hand edit of a hand-written
   template — structurally unrepeatable via the IR.** `regen-css` reproducing the
   const-generic seam would require the seam to already be *in* `CSS_GENERATED_RS`
   (i.e. a hand template edit — exactly what the brief forbids), or the template
   to be replaced by an IR-driven emitter.

## CONSTRUCTIVE NUANCE — the live tree corroborates the *weaker* claim

The main-repo tree (evolved past this baseline) DID replace the raw-string
template with an IR/BTA path: `lower::css_scan::ScanAlphabet::derive_alphabet`
harvests every delimiter byte from the CSS grammar's rule literals
(`ruleBlock`/`atRuleBody`/declaration `terminal_bytes()`), and `css_scan_direct`
emits them as grammar-derived `const AT_RULE_STOP: &[u8] = b";{}"` (etc.). That
independently confirms the corollary's *underlying* claim — the delimiter set IS a
clean S0 island a BTA lifts (`stage_leaks = 0`, as measured). BUT the emitted
residual is a `const &[u8]` feeding a SIMD `css_balanced_component_scan`, NOT
PROVE-12's const-generic `u128` mask, and `find_component_delim` is *retired*. So
even the evolved tree does not reproduce PROVE-12's patch byte-for-byte.

## THE NUMBERS

- residual S0 islands residualized (BTA on the CSS-shaped mirror): **4**
- stage leaks (no-silent-epsilon guard): **0**
- binding_times tests: **4/4 pass**
- byte-identity to `PROVE-12/patch.diff`: **FAIL** (structural — no
  `find_component_delim` emitter exists to residualize on the IR)
- parity checksum reproduction (track1_full=1 / track1_rich=17839): **N/A** — not
  reached; byte-identity fails first.
- `find_component_delim<const DELIMS>` emitted-not-authored: **NO**

## CONSEQUENCE FOR 3D

3D does **not** graduate from SPEC-only on the strength of PROVE-12 byte-identity.
The corollary as posed is refuted: the delimiter set is not cleanly an S0 island
*that this baseline's emit path exposes to the BTA*, and the const-generic seam
cannot be emitted structurally without a prior template→IR-emitter rewrite. What
survives is the machinery (`binding_times`, ran) and the weaker, live-tree-backed
claim that a grammar-derived delimiter island is BTA-liftable — but to a
`const &[u8]`, not PROVE-12's `u128` mask. Family 3D's residual-lift is real;
PROVE-12's *specific* const-generic patch is not the emergent form of it.
