# LOCK-8 · pass-3 · PROTO-2 — 8C priority-DFA necessity (Route A vs Route B)

**Plane:** THE TS BACKBONE PLANE (parse-that) · family 8C correspondence · probe 2
**Verdict line:** **RAN_MEASURED — ROUTE A RETIRED (latent-only). Route B classify-gate ships.**
The priority-DFA was BUILT and PROVEN correct (col3==col1 on H1+H3, 44/44 green, worst state ratio **1.00×**, Hopcroft-safe), yet the P4 consumer census finds **ZERO** live literals that exercise a JS↔Rust divergence (H1/H3). Route A is a correct fix for a defect the live corpus never trips.

---

## Question

Is Route A (priority-preserving subset construction) **NEEDED**, or is the JS `RegExp` ↔ Rust-DFA divergence **LATENT-ONLY** for the live corpus so that Route B (classify-gate alone) suffices? And if Route A is built, does priority-keyed determinization realize leftmost-first on {H1,H2,H3} without state-count blowup (R1) or Hopcroft breakage (R2)?

This forks GESTALT-P and routes the confirmed LOCK-5 engine defect: the NFA carries `EpsilonEdge::priority` (`nfa.rs:21`, alternation index `:314`, greedy/lazy 0/1), but `epsilon_closure` over `BTreeSet<StateId>` (`dfa.rs:189–202`) **discards** it and `find_at` (`dfa.rs:116`) delivers leftmost-**longest**.

## Method — two artifacts, both RAN

parse-that is READ-ONLY; the priority-DFA was built on a full copy of `parse-that/rust/` at
`pt-scratch-8C-regex/` (patch paths rewritten to absolute; `CARGO_TARGET_DIR` inside the scratch;
base-pin `rust/regex` HEAD `ef10d5b7`). Floor confirmed **before** any edit: `cargo test -p bbnf-regex` = **44 passed / 0 failed** across 12 binaries.

**(a) P4 consumer-regex census.** Every `regex(/…/)` literal (not `string(…)` — exact-literal, hazard-free) extracted from the three named sources and classified against H1 (ordered-alt shorter-first `a|ab`), H2 (greedy-extent), H3 (lazy `\d+?`), H4 (backref), H5 (lookaround):
- **value.js** `src/parsing` @ pin `6abef80` (historical parse-that-era grammar; note HEAD `86a9a17` migrated OFF parse-that entirely — 0 imports)
- **latex-paper** `src/grammar` @ HEAD `c1404739` (the sole surviving live parse-that consumer: imports `regex, string`)
- **parse-that** first-party leaves (`typescript/src/parse/leaf.ts`, `parsers/{json,csv,utils}.ts`)

**(b) Priority-DFA scratch.** New `automata/pfa.rs` (369 LOC, additive — `dfa.rs` untouched): an **ordered ε-closure with match-cut** + `MatchKind::LeftmostFirst`. The DFA state key is a priority-ordered `Vec<StateId>` (not `BTreeSet` — `[A,B] ≠ [B,A]`, the crux of Route A); `visit()` descends ε-edges in priority order and **halts the whole traversal the instant the accept state is reached**, discarding every lower-priority thread. `find_at` is byte-for-byte identical to the longest engine — the discipline lives entirely in construction. Runner `examples/hazard_triples.rs` compiles each triple with both engines; JS column from `js-oracle.mjs` (`node`). loadavg 7.5–9.2 recorded at build.

---

## What ran — the numbers

### Three-column hazard differential (`hazard-triples.json`)

| hazard | pattern | input | **col1 JS `RegExp`** | **col2 longest-DFA** | **col3 priority-DFA** | flip? |
|---|---|---|---|---|---|---|
| **H1** ordered-alt shorter-first | `a\|ab` | `"ab"` | `"a"` | `"ab"` | **`"a"`** | ✅ **col3==col1** |
| **H2** greedy-extent | `\d+` | `"123"` | `"123"` | `"123"` | `"123"` | — all agree (non-divergent) |
| **H3** lazy | `\d+?` | `"123"` | `"1"` | `"123"` | **`"1"`** | ✅ **col3==col1** |
| **H4** backref | `(a)\1` | `"aa"` | `"aa"` | **REJECTED** | **REJECTED** | both engines refuse to compile |
| **H5** lookaround | `a(?=b)` | `"ab"` | `"a"` | **REJECTED** | **REJECTED** | both engines refuse to compile |

The priority-DFA reproduces JS leftmost-first **exactly** where the longest engine diverged (H1, H3). H2 (greedy) is identical across all three columns — greedy *is* longest *is* first, so H2 carries no divergence by construction. H4/H5 fail to build in either DFA engine (HIR has no backref and no lookaround; `Look` = line/text anchors only, `nfa.rs:254`), i.e. Route B's classify-gate is already enforced at HIR-parse.

### R1 — state-count blowup (priority vs longest, both Hopcroft-minimized)

Over the **entire compilable live corpus** (parse-that + value.js + latex leaves, plus the three DFA-reachable hazards):

| worst priority/longest ratio | **1.00×** |
|---|---|
| all 13 real-corpus literals | **1.00×** (identical state counts) |
| H1 `a\|ab` | **0.67×** (2 vs 3 — the cut *removes* the "ab" branch) |

**R1 PASS — comfortably ≤ 2×; the priority engine is never larger and sometimes smaller.**

### R2 — Hopcroft safety

`minimize=on` vs `minimize=off` match divergences across the corpus = **0**. Partition-refinement on the priority-DFA merges only accept-equal + transition-equal states; the match-cut is already baked into the transition table. **R2 PASS.**

### Floor

`cargo test -p bbnf-regex` **after** adding `pfa.rs` = **44 passed / 0 failed**. `regex_dfa_compile` bench builds clean (untouched — `pfa.rs` is additive, `dfa.rs` byte-identical to HEAD, so no timing regression is possible by construction; full divan timing not re-run under load).

---

## The census — the deciding artifact

**Total `regex()` literals across the three sources ≈ 33. Genuine divergence-bearing live literals: ZERO.**

| hazard | value.js @6abef80 | latex @HEAD | parse-that leaves | reaches DFA & diverges? |
|---|---|---|---|---|
| **H1** shorter-first alt | **0** | 0 | **0** | — every alternation is disjoint-prefix or longest-first/anchor-guarded (see below) |
| **H2** greedy `+ * {n,m}` | pervasive | pervasive | pervasive | **NO** — greedy==longest==first; col2==col1 already |
| **H3** lazy `*? +? {n,m}?` | 0 | **2** (`math.ts:45,57`) | 0 | **NO** — both are `[\s\S]*?(?=…)`, gated by their H5 lookahead at HIR-parse |
| **H4** backref | 0 | 0 | 0 | — none exist |
| **H5** lookaround | 0 | **13** (`commands/math/text.ts`) | 0 | **NO** — HIR rejects at compile (Route B gate) |

**Every alternation in the live corpus, enumerated and shown non-hazardous:**
- value.js `\b(alpha\|r\|g\|b\|…)\b` — longest-alt FIRST + `\b`-anchored whole-word (and `\b` is itself unsupported → rejected). Not shorter-first.
- value.js `\s*,\s*\|\s+` — alt-1 requires a comma; whenever both can match, alt-1 is the longer → first==longest.
- value.js `-?(?:\d+\.?\d*\|\.\d+)…` — inner alts are disjoint first-char (digit vs dot); never both match.
- parse-that `-?(?:0\|[1-9]\d*)…` (json-number) — disjoint first-char (`0` vs `1-9`).
- parse-that `"(?:[^"\\]\|\\(?:…))*"` (json-string) — disjoint (`\\`-escape branch vs non-`\\` branch); classic unambiguous tokenizer loop.

No `a|ab`-shaped literal (shorter alternative first, overlapping prefix) exists anywhere in the live corpus. The two lazy literals — the only H3 incidence — are `[\s\S]*?(?=\\\])` / `(?=\\\))`; each carries a positive lookahead and is therefore rejected at HIR-parse on the H5 ground (empirically confirmed: `a(?=b)` → REJECTED), never reaching the DFA where lazy-vs-greedy would matter.

---

## Verdict

**ROUTE A RETIRED — latent-only (named negative result). Route B classify-gate alone ships.**

The pass/fail gate requires BOTH (i) a live consumer literal exercising a divergence (H1/H3), AND (ii) the priority-DFA flipping col3==col1 with floors green and states ≤2×. **(ii) is satisfied in full** — the priority-DFA is a proven, correct, non-bloating (1.00×), Hopcroft-safe realization of leftmost-first. **(i) fails**: the census finds zero live H1 and zero DFA-reachable H3 literals; H2 is pervasive but definitionally non-divergent (the longest engine already equals JS on every greedy literal); H4/H5 are already refused by the HIR parser (Route B's gate operating for free).

Therefore the JS↔Rust regex divergence is **LATENT-ONLY** for the live corpus. **Route B** — reject {H4, H5} at grammar-compile (already enforced by HIR-parse rejection) — is the complete, sufficient response. Route A is a correct solution to a problem no live consumer poses; `pfa.rs` is banked as a validated shelf artifact should a future live grammar introduce a shorter-first alternation or a standalone lazy leaf, at which point the WARRANT flips on a single named consumer.

**Routing to LOCK-5:** the confirmed engine defect (priority discard) is REAL but **not consumer-load-bearing**. LOCK-5 should record it as a latent correctness gap fixed-in-principle by `pfa.rs` (Route A, shelved), guarded in practice by the classify-gate (Route B, shipped) — and must NOT let 8C's guard foreclose the `pfa.rs` fix, since the two are complementary, not exclusive.

---

### Deliverables in this directory
- `PROTO-2.md` (this file)
- `PROTO-2.patch` — unified diff (mod.rs +1 line; `pfa.rs` NEW 369 LOC; `hazard_triples.rs` NEW)
- `hazard-triples.json` — the H1–H5 corpus + JS oracle expectations
- `js-oracle.mjs` — column-1 generator (`node`)
- `pt-scratch-8C-regex/` — the parse-that copy the probe ran in (READ-ONLY discipline preserved)
- `baseline-8C-tests.txt`, `floor-tests-with-pfa.txt` — the 44/44 floor, before and after
