# PROTO-2 — The Fork-gating grammar-tier regex e-graph fire census

**Lock:** LOCK-5 (THE REGEX CONTINUUM) · pass-3 · probe 2 · family 5B
**Registry directive:** #3 (the ONE number gating Fork-A ⊥ Fork-B) · feeds GESTALT via XL-A
**Verdict:** **RAN_MEASURED — PASS (Fork-B available on the regex tier)**
**Measured on HEAD:** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`
**Base-pin:** worktree began at `d2b32f6e3`; `ce51c36fc` was NEWER (merge-base = my HEAD),
fast-forwarded clean via `git merge --ff-only ce51c36fc`; post-FF
`git merge-base --is-ancestor ce51c36fc HEAD` = **ANCESTOR-OK**. Numbers are NOT
wrong-artifact-risk (clean FF, exact HEAD recorded).
**loadavg (paired, cold-compiled, single process):** before `{8.65 9.07 9.35}`, after `{8.65 9.07 9.35}`.

---

## QUESTION

Does the grammar-tier regex e-graph (`crates/ir/src/egraph/rules/regex.rs` — the surviving
434-LOC file, ~184 LOC of which are the three regex-specific rules after the H-7 de-dup)
**FIRE** on `Map`-wrapped (projected, `x -> Tag`) Alt branches, or only on bare
`Alt([Regex, Regex, …])` branches?

PROTO-1 found the three regex rules resolve a branch's regex via `class_regex(child)` — which
scans the child's e-class for a `GrammarENode::Regex` node — and return `None` on a
`Map { inner, fn_id }` child, because a Map e-class holds no bare `Regex` node.
`FuseAltRegexBranches` likewise sets `all_fusable = false` on a Map child. If so, projected
branches are **structurally inert** for the regex tier → the copy has no consumer where
projections dominate → Fork-B (abrogate the grammar-tier e-graph for free) is available and
5B's string bridge + 5C's grammar-tier rules lose their consumer (Y.13). If it fires materially
on projected branches, Fork-A must keep it.

## METHOD

The top-level `crates/ir` worktree is the only tree where the grammar-tier regex e-graph
physically exists (skinny ships without one — FAMILY-SPECS §5A). Built two `GrammarIR`s over
the SAME corpus of **33 Alt-of-regex patterns**, differing only in branch topology:

- **BARE:** `Alt([Regex(a), Regex(b), …], None)`
- **PROJECTED:** `Alt([Map(Regex(a), TagA), Map(Regex(b), TagB), …], None)` — each branch
  wrapped in a `Map` carrying a distinct `EnumWrap` tag, the exact IR shape `(a -> A | b -> B)`
  lowers to.

Ran the production saturation path `build_and_saturate_reported` (identical to
`build_and_saturate`, additionally returning the scheduler's `RunReport` — the same struct
`BBNF_EGRAPH_REPORT=1` prints). Tabulated `per_rule.work`, which the scheduler defines
(`crates/egraph/src/rewrite.rs`) as `node_delta + union_delta` — new e-nodes added plus e-class
unions attributed to each rule. **`work > 0` ⟺ the rule fired and changed the graph;
`work == 0` ⟺ inert.**

Corpus (each row = one Alt's branches), grouped to exercise every rule on the bare form:
Fuse family (all-regex/literal disjoint alts, ×15), Superset-absorb family (one class contains
another, ×8), Union-merge family (adjacent/overlapping classes, ×8), Dedup family (structurally
identical / duplicate branches, ×2). Strings interned with dedup (as the real `SharedStrings`
does), so identical branches share a `StringId` and hash-cons to one e-class — the identity
`DeduplicateAltBranches` keys on.

**Command:**
`BBNF_EGRAPH_REPORT=1 cargo test -p bbnf-ir --profile ax-iter --test backend_egraph_fire_census -- --nocapture --test-threads=1`

## WHAT RAN

- `crates/ir/tests/backend_egraph_fire_census.rs` (new, 270 LOC) — the census test.
  `test result: ok. 1 passed`.
- `crates/ir/src/egraph/mod.rs` (+19 / −2) — added `pub fn build_and_saturate_reported` returning
  the `RunReport`; `build_and_saturate` now delegates to it and drops the report. DRY, no
  behaviour change on the existing path.

Both saturations ran in one test process; built clean (`EXIT=0`) under `ax-iter` on the
base-pinned HEAD.

## THE NUMBERS (`per_rule.work`, cold, paired)

| rule                       | **bare.work** | **proj.work** |
|----------------------------|:-------------:|:-------------:|
| `superset-absorb-alt`      | **11**        | **0**         |
| `union-merge-alt`          | **19**        | **0**         |
| `fuse-alt-regex-branches`  | **30**        | **0**         |
| `deduplicate-alt-branches` | **3**         | **0**         |

Saturation-level report:

```
bare      : iters=2  total_applied=63  saturated=true   (initial_nodes=125 final=125 classes=86)
projected : iters=1  total_applied=0   saturated=true   (initial_nodes=184 final=184 classes=184)
```

The projected e-graph reaches its fixed point in a **single iteration having applied ZERO
rewrites** — `total_applied=0`, `final_nodes == initial_nodes`, `final_classes == initial_classes`
(no unions at all). Not "≤ noise" — **exactly zero**.

## VERDICT LINE

**PASS — Fork-B is available on the regex tier.** All four rules fire ≥1 on bare
(3 / 11 / 19 / 30) and are **exactly 0** on projected. The three regex-*specific* rules — the
actual consumers of 5B's string bridge and 5C's grammar-tier rewrites — resolve every
`Map`-wrapped branch to `None` and never fire. Consumer density on projected branches is not
"near zero"; the whole projected saturation applies zero work and terminates in one iteration.
PROTO-1's structural finding is confirmed empirically: **the `Map` barrier makes the grammar-tier
regex e-graph inert wherever projections dominate.**

## HONEST SCOPE (what this number does and does not license)

- **It measures the two topologies, not the real-grammar mix.** The decisive, model-independent
  result is the *structural inertness under projection* (`proj total_applied = 0`). Whether
  Fork-B is *taken* depends on GESTALT weighing how often real BBNF grammars present the grammar
  tier a **bare** `Alt([Regex,…])` versus a **projected** `Alt([Map,…])`. Real alternations almost
  always carry per-branch tags (`(a -> A | b -> B)`), so the Map barrier is the common case — but
  this probe does not itself count that frequency; it establishes that *when* projected, consumer
  density is 0.
- **The probe feeds raw Alts to the e-graph** to isolate firing. In a full compile the structural
  normalizer runs *before* the e-graph and may already consume much of the bare redundancy — so
  the bare 3/11/19/30 is an upper bound on what the e-graph would see live, not a claim about
  end-to-end contribution.
- **`DeduplicateAltBranches` is structural, not regex-specific.** It keys on canonical e-class
  identity and would fire on any duplicated node kind; under distinct-tag projection the Map
  wrappers differ, so it too is inert (0). Included for completeness, but not part of the
  regex-tier consumer question the fork gates on.

**Fork consequence (registry directive #3):** the ONE gating number is measured — on projected
branches, per-rule work for `superset-absorb-alt` / `union-merge-alt` / `fuse-alt-regex-branches`
is **0/0/0**. This makes **Fork-B (abrogate the grammar-tier e-graph) available for free** and
un-consumes 5B's string bridge (Y.13) in the projection-dominated regime; Fork-A is only required
if GESTALT finds real grammars route bare `Alt`-of-regex to the grammar tier.

---

### Reproduction

```
git -C crates/ir merge-base --is-ancestor ce51c36fc HEAD   # ANCESTOR-OK
BBNF_EGRAPH_REPORT=1 cargo test -p bbnf-ir --profile ax-iter \
  --test backend_egraph_fire_census -- --nocapture --test-threads=1
```

Artifacts in this directory:
- `PROTO-2-firecensus.md` — this report
- `PROTO-2-firecensus.patch` — combined diff (src `build_and_saturate_reported` + new census test)
- `PROTO-2-census-output.txt` — raw `--nocapture` stderr (both saturation reports + table)
