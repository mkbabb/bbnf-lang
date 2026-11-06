# LOCK-5 pass-2 · PROTO-1 — family 5B LAWFUL-FUNCTOR (un-bank/RETIRE gate)

**Probe 1 of the 5B un-bank gate: structural fuse + cross-domain functor + rule-death count.**

- BASE-PIN: worktree HEAD started at `d2b32f6e3` (NOT a descendant of `7cdbff4ef`).
  Fast-forwarded with `git merge --ff-only 7cdbff4ef` → **HEAD measured on
  `7cdbff4ef411154dd9bf4b80731c0616b1579d22`** ("locks(sk-v24): pass-1 pt full fold").
  All numbers below are on this clean, base-pinned artifact. No wrong-artifact-risk.
- loadavg during runs: ir test `8.03 10.40 11.26`; pt-scratch corpus `~9`; report-time `20.34`.
- No x86. parse-that consumed READ-ONLY via an rsync scratch copy
  (`scratchpad/pt-scratch`, isolated `CARGO_TARGET_DIR`, external patches repointed
  per R3 §8). The original tree was never mutated or compiled.

---

## Question

When the regex algebra is written once as a structural functor, (1) does the
434-LOC grammar-tier copy (`crates/ir/src/egraph/rules/regex.rs`) die by a counted
number, and (2) does the projection-dropping-union soundness bug appear under the
current string bridge and vanish under the functor? Decides **un-bank vs RETIRE** —
5B's whole value axis.

## Method (what ran)

The grammar tier and the regex tier are the SAME e-graph substrate (`egraph` crate)
over two languages: `GrammarENode` (`crates/ir`) and `HirENode` (bbnf-regex ==
`/Users/mkbabb/Programming/parse-that/rust/regex`, already a dep at
`crates/ir/Cargo.toml:14`). bbnf-regex already carries the functor:
`translate.rs::{insert_hir, extract_hir}` + `simplify_hir` (build → CSP-scheduled
saturate → cost-guided extract) running the five HIR rules
(flatten/dedup/superset-absorb/union-merge/repetition-absorb).

- **(a) Structural fuse vs string fuse** — RAN, pt-scratch, `probe_a`.
  Grammar tier fuses `[a-c]|[d-f]` with `parts.join("|")` (`regex.rs:300`) →
  `"[a-c]|[d-f]"`. Functor rebuilds it structurally (Alternation → union-merge →
  `[a-f]`). Compiled both to Hopcroft-minimized DFA; asserted language-equivalent.
- **(b) Cross-domain round-trip oracle** — RAN, pt-scratch, `probe_b`, over the
  `regex_hir_egraph` corpus. Oracle = **product-BFS symmetric-difference over all
  256 bytes** on the two minimized DFAs (canonical/minimize on, strict off): for each
  pattern, `parse → simplify_hir (== insert→saturate→extract) → minimized DFA` must be
  language-equivalent to `parse → minimized DFA`. This exercises the exact functor
  round-trip an `insert_bbnf/extract_bbnf` cross-domain bridge would drive, since the
  grammar-tier Regex/Literal leaves ARE regex strings.
- **(c) Projection-soundness** — RAN, `crates/ir` test
  `projection_union_merge_is_already_safe_under_string_bridge` (built with the
  production rule set via `build_and_saturate`). Fixture: `[a-c]->A | [d-f]->B`
  lowers to `Alt([ Map{Regex([a-c]),0}, Map{Regex([d-f]),1} ])`. Asserted the string
  bridge does NOT intern a merged `[a-f]` / fused pipe and that both `fn_id`s survive.
  Baseline `rule_union_merge_alt` (bare `Alt([Regex([a-c]),Regex([d-f])])`) confirms
  the merge DOES fire when there is no projection.
- **(d) Partiality** — analysed (not a differentiator). `Alt([Regex, Ref]) -> G`:
  a `Ref` child's e-class holds no `Regex`/`Literal` node, so the current
  `FuseAltRegexBranches` finder sets `all_fusable = false` and `continue`s — no
  crash, no cross-branch fusion. The functor's `insert_bbnf` would likewise return
  `None` on a `Ref` (no `HirENode` expresses a grammar rule reference). Both tiers are
  already partiality-safe; the functor changes nothing here.
- **LOC** — measured by deleting the subsumed rules/wrappers and `wc -l`.

## The numbers

| Probe | Result |
|---|---|
| (a) structural fuse == string fuse (DFA-equiv) | **PASS** — merged `[a-f]` DFA ≡ `"[a-c]\|[d-f]"` DFA; functor extracted a single `Hir::Class` (genuine simplification, not identity) |
| (b) corpus round-trip DFA-equivalence | **59 parsed / 59 DFA-checked / 59 equivalent / 0 non-equiv / 0 uncompilable = 100%** (corpus array = 59 live patterns; the "62" figure counts 3 `//`-comment section markers) |
| (c) projection-soundness under STRING BRIDGE | **string bridge is ALREADY SAFE** — no `[a-f]` interned, no pipe fused, both `fn_id`s (0,1) survive; test passes |
| (c) baseline — bare branches merge | `rule_union_merge_alt` passes: `[a-c]\|[d-f]` → `[a-f]` fires when NO projection present |
| (c) under FUNCTOR | also safe (Map barrier is representation-level, tier-agnostic) — **no delta vs string bridge** |
| LOC death | subsumed set (`class_regex`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `regex_escape_literal`, `pattern_has_top_level_pipe`) deleted = **250 lines removed**; `regex.rs` 434 → 184 |

Root cause of (c): `[a-c]->A` is `Map{ inner: Regex([a-c]), fn_id }`. The Alt's
children are the **Map** e-classes; the bare `Regex` lives in the *inner* class.
Every string-bridge rule locates a branch's regex via `class_regex(child)` /
a Regex|Literal finder over the CHILD's e-class (`regex.rs:34, 140, 272, 378`),
so a projection-wrapped branch resolves to `None` and the union/superset/fuse
rules are **structurally inert on projected branches**. The projection barrier is a
property of the Map-wrapped representation, not of the string-vs-structural bridge.

## Verdict

**5B → RETIRED.**

- Criterion 1 (copy dies ≥150 LOC): **MET** — 250 LOC.
- Criterion 3 (round-trip DFA-equiv on 100% of corpus): **MET** — 59/59.
- Criterion 2 (projection-soundness *fails-under-string-bridge* / passes-under-functor):
  **NOT MET** — the string bridge does not fail. The projection-dropping-union bug
  the family was banked to fix **does not exist** under the current Map-wrapper
  representation (empirically: `projection_union_merge_is_already_safe_under_string_bridge`
  passes). The functor and the string bridge are equi-sound here.

Un-bank requires the conjunction of all three; Criterion 2 is false. The 250-LOC
consolidation is real and lawful, but it is the *shared* payoff of reusing the
regex-side rules — achievable by a bare 5A string-bridge merge (`format!` the branch
patterns, let `bbnf_regex` simplify, re-intern) with identical correctness, since
there is no soundness gap for the structural functor to close. That is exactly the
RETIRE trigger **"dies trivially under a bare 5A merge."** 5B's distinguishing value
axis (lawful structural soundness) answers a non-bug; it carries no value the cheaper
5A merge does not already carry.

VERDICT LINE: **5B LAWFUL-FUNCTOR RETIRES — functor is lawful (59/59 DFA-equiv,
fusion equiv) and would delete 250 LOC, but the projection-dropping-union soundness
bug is a non-bug under the Map-wrapper representation, so the LOC win reduces to a
bare 5A merge and 5B has no independent value axis.**

## Artifacts

- `proto1-ir-projection-test.patch` — diff adding the RAN grammar-tier probe (c)
  to `crates/ir/tests/egraph/egraph_grammar.rs`.
- `lock5_proto1_corpus.rs` — the RAN pt-scratch probe (a)+(b): 59-pattern corpus
  round-trip with the product-BFS DFA-equivalence oracle + structural-fusion check.
- `regex_after_delete.rs` — the subsumed-rules-deleted snapshot used for the
  LOC measurement (434 → 184; non-compiling by design, it is a measurement artifact).
