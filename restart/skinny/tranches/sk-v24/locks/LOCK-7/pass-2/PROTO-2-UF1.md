# PROTO-2 (UF-1) — the structural-only (leaf-blind) bbnf oracle

LOCK-7 (Grammar Totality & Bootstrap), pass-2, probe 2. Family: the bbnf leg
(7D-bbnf Tier-R/S, 7A seed, 7B Plane-A) — bounds INC-5.

- **Measured HEAD:** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`
- **BASE-PIN:** worktree was *behind* the pin (`d2b32f6e3`, an ancestor of `ce51c36fc`),
  fast-forwarded `--ff-only ce51c36fc`. Post-ff `git merge-base --is-ancestor ce51c36fc HEAD` = **OK**.
  HEAD == the pin. **No wrong-artifact-risk.**
- **loadavg at measurement:** 7.11 / 5.85 / 6.64 (build+run took 4.26 s cold; this is a
  *static* structural probe — no perf claim rides on loadavg).
- **Verdict:** **FAIL-signal by the literal rubric, but the divergence REFUTES the anticipated
  cause. RAN_MEASURED.** The branch skeleton is essentially SOUND (52/53 leaf-blind); the sole
  divergence is a materialization-role misfire, not branch-skeleton poisoning and not the
  prime-suspect `term` alternation collapse.

---

## QUESTION (the RED-unknown this probe was cut for)

PROTO-1/PROTO-3 stopped at "leaf spans wrong → no projection." Nobody asked whether the
sink-only lowering's **BRANCH skeleton** matches the oracle `GrammarIr`'s when leaves are
treated as opaque. Is the INC-5 cross-repo block **purely at the leaf level** (bbnf is
structurally Tier-R; only `match_at` is missing) or does the leaf floor **poison the branch
skeleton too** (bbnf is not even structurally Tier-R)?

## METHOD (static; the recognizer is NOT run — it would fail at leaves)

New test `skinny/crates/codegen/tests/structural_oracle_probe.rs` (+ one-line
`pub(crate) mod lower` → `pub mod lower` so the test reaches
`codegen::lower::sink_only::lower_program` exactly as the brief specifies).

1. **ORACLE** = the hand parser's bbnf import-closure `GrammarIr` — `grammar::parse_grammar`
   over `types.bbnf` ⧺ `expressions.bbnf` ⧺ `bbnf.bbnf` (the three files under
   `grammar/bbnf/`; `@import`/`@pretty` lines are skipped by `parse_directive`).
2. **CANDIDATE** = lower that SAME `GrammarIr` through the `!has_rule("value")` path:
   `GrammarIr` → `BackendIr` (`passes::compile`) →
   `codegen::lower::sink_only::lower_program` (`sink_only.rs:126`) → `SinkOnlyProgram`.
3. Per rule, compare **LEAF-BLIND**: identical nesting/arity of `Seq/Alt/Optional/Repeat/CallRule`,
   collapsing every leaf (`Literal/Regex/Project/ByteLiteral/Annotation/Discard` →
   `ByteLiteral/RegexProgram/ValueProject/SpanMark/TapeEmit/DirectBuild/Return`) to one opaque
   token. `Entry` is unwrapped. Two readings reported:
   - **STRICT** — the lowering's `Annotation(?w) → Seq([])` rewrite leaves a 0-arity `Seq`
     branch node facing the oracle's `?w` leaf.
   - **CHARITABLE** — an empty `Seq([])` counts as an ε-leaf, so the `?w` rewrite is
     leaf-blind-invisible (whitespace is a leaf concern).

**Leaf-elision note (transparent).** The in-tree hand parser **cannot ingest the raw closure**:
`parse_grammar` fails with `unexpected token '-' at byte 2343`. This is the `?w`-then-`->`
adjacency (`parse_seq` consumes the standalone `?w` annotation, then `->` is met at the head of
`parse_atom`) — it fires at bbnf.bbnf's `big_comment`/`comment` (`( … ) ?w -> Span`), the only
two `?w -> ` sites in the corpus. Both tokens elided (`?w ` before `->`) are **leaves** (an
optional-whitespace annotation and a projection) that collapse to opaque tokens on BOTH sides,
so the elision is invisible to the leaf-blind BRANCH comparison. With the 2 leaf tokens elided
the parser yields the full **53-rule** GrammarIr (407 exprs), and the branch question can be
posed.

**Command.**
`CARGO_TARGET_DIR=/tmp/uf1 cargo test --manifest-path …/skinny/Cargo.toml -p codegen --test structural_oracle_probe -- --nocapture`
(the `codegen` package lives in the `skinny/` workspace; `-p codegen` alone from the repo root
finds no package).

## WHAT RAN — THE NUMBERS

```
RAW closure parse:      Err -> BBNF-PARSE: unexpected token `-` at byte 2343   (the ?w -> gap)
leaf-elision:           2 site(s) normalized (big_comment, comment)
NORMALIZED closure:     Ok / 53 rules (exprs=407)
sink_only::lower_program: Some / 53 rules, direct_shapes={"BbnfBool"}, has_rule("value")=false

STRICT     (Seq([]) is a branch node):  matched 25/53
CHARITABLE (Seq([]) == epsilon leaf):   matched 52/53
first divergent rule (both readings): `bool_lit`
    oracle    = Alt([Leaf, Leaf])
    candidate = Seq([Leaf, Alt([Leaf, Leaf]), Leaf, Leaf, Leaf, Leaf])
charitable mismatches (1): bool_lit
materialized (DirectBuild) rules: ["bool_lit"]
```

- `sink_only::lower_program` returns `Some` and `has_rule("value") == false` → bbnf genuinely
  takes the **`!has_rule("value")` (general_descent) path** the brief named.
- **52 of 53 rules are branch-isomorphic** under the leaf-blind charitable reading. The
  `Seq/Alt/Optional/Repeat/CallRule` nesting and arity survive `passes::lower_expr` faithfully
  (it maps `Seq→Seq, Alt→Alt, Optional→OptionalBranch, Repeat→RepeatLoop, Ref→CallRule` 1:1;
  the `SinkOnly` step mirrors `BackendExpr` 1:1).
- The **STRICT/CHARITABLE delta is 27 rules** — exactly the rules that use `?w`. Their only
  difference is `Annotation(?w) → Seq([])`, a leaf-level whitespace representation that is
  leaf-blind-invisible.
- The **single true structural divergence** is `bool_lit`. Its cause is **not** the leaf floor
  and **not** the anticipated alternation collapse: `derive_materialization_roles.bool_value`
  (passes) misclassifies bbnf's `bool_lit = "true" | "false"` as a JSON boolean **value** and
  `materialize_rule` wraps its body in
  `Seq[SpanMark(Start), body, SpanMark(End), TapeEmit, DirectBuild(BbnfBool), Return]`. The
  oracle `Alt([Leaf,Leaf])` becomes a 6-node `Seq` around that Alt. This is a **JSON
  materialization-role misfire**, a single named bug — the Alt itself is preserved inside the wrap.

**Prime suspect REFUTED.** research-B §4 predicted `term`'s 8-branch alternation "collapsing to
one uniform node kind per rule at emit.rs:388." At the `SinkOnlyExpr` level (the CANDIDATE this
probe compares) `term` is **not** divergent — its 8-branch `Alt` is preserved intact. The
collapse research-B feared is an `emit.rs` **render-time** concern, strictly downstream of the
`SinkOnlyExpr` structure; it does not touch the branch skeleton the lowering produces.

## THE VERDICT LINE

**FAIL-signal by the literal rubric — matched 25/53 (strict) and 52/53 (charitable), both < 53 —
but the block is NOT a branch-skeleton poisoning.** First (and only leaf-blind) divergent rule:
**`bool_lit`**, cause = the JSON `bool_value` materialization role wrapping
`"true" | "false"` in a `BbnfBool` `DirectBuild` sequence — **not** `term`'s alternation collapse
(term's 8-branch `Alt` survives), **not** the leaf floor.

**This TIGHTENS the born-RED DAG edge precisely.** bbnf **is** structurally Tier-R at the branch
level: 52/53 rules are leaf-blind-isomorphic through the sink-only lowering, and the sole
exception is one materialization misfire, not a control-structure loss. The INC-5 cross-repo
block therefore resolves into **three disjoint, non-branch seams**, in DAG order:

1. **Parser front-end (upstream of everything).** `parse_seq`/`parse_atom` reject `?w`-then-`->`
   (`byte 2343`); bbnf cannot be ingested to a `GrammarIr` at all until this is fixed. Leaf-level
   token adjacency, not skeleton.
2. **Leaf floor (PROTO-1/PROTO-3).** `match_at` / the general_descent leaf lexer recognizes only
   the Sheets/JSON leaf alphabet. Runtime, leaf-level.
3. **One materialization-role misfire.** `bool_value` fires on bbnf's `bool_lit` literal
   alternation, wrapping it as a JSON `BbnfBool`. A single, named, structural bug — bounded, not
   a systemic skeleton collapse.

None of the three poisons the `Seq/Alt/Optional/Repeat/CallRule` branch skeleton. The omega
gestalt should carry the edge as **"INC-5 = leaf-floor + parser-front-end + 1 materialization
misfire; branch skeleton SOUND (52/53),"** not **"bbnf is not even structurally Tier-R."**

## FILES

- Report: `PROTO-2-UF1.md` (this file). Named `-UF1` to avoid clobbering the pre-existing
  `PROTO-2.md` (a different probe's deliverable) in the pass-2 folder.
- Patch: `PROTO-2-UF1.patch` — `codegen/src/lib.rs` (`pub mod lower`, 1 line) +
  new `codegen/tests/structural_oracle_probe.rs` (289 lines).
- The probe RUNS green: `test result: ok. 1 passed` (the test asserts only that the full 53-rule
  closure was exercised; the matched count is data, not a harness gate).
