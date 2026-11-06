# PROTO-2 — 5C AUTOMATA-AS-EXTRACTION: worklist minimizer + derivative-saturator

**Lock** LOCK-5 (The Regex Continuum) · **Family** 5C · **Probe** 2 · **Pass** 1
**Status** RAN & MEASURED (both halves compiled, ran, gated). Not SPEC_ONLY.
**Engine** `parse-that/rust/regex` (`bbnf-regex` 0.1.0), consumed READ-ONLY via an
rsync scratch-copy per R3 §8. Bench profile `bench` (fat-LTO, cgu=1, O3),
toolchain `nightly-2026-04-11`, isolated `CARGO_TARGET_DIR` inside the copy.

## Question

Two candidate replacements for the incumbent minimizer
(`automata/dfa.rs:291 hopcroft_minimize` — naive Moore with a **global restart** at
`:358`, O(n²·k)):

- **(2A)** Is a non-restart worklist refinement (Hopcroft/Valmari) **EXACT** and
  **FASTER** than the incumbent, with an acyclic **congruence-closure** fast path
  that is exact-and-free?
- **(2B)** Does Brzozowski-derivative determinization, canonicalized *only* by the
  five existing HIR saturation rules (`egraph/rules/{flatten,union,superset,
  redundant,repetition}`), **under-minimize** (ρ>1 ⇒ BLOCKED-candidate) or
  **collapse determinize+minimize** into one pass (ρ=1 ⇒ landmark)?

## Method (what actually ran)

Scratch copy at `…/scratchpad/pt-scratch`; external patch-deps repointed to
absolute sibling repos; original tree untouched. Corpus = the 26 patterns hard-coded
in `benches/regex_dfa_compile.rs` (the brief says "50"; the file holds **26** — cyclic
`[a-z]+ \w+ 0x[0-9a-fA-F]+` and acyclic `#[0-9a-fA-F]{3,6}` all present, as required).

Code added to the scratch `dfa.rs` (diff: `proto2-dfa.patch`, 205 lines) and driven by
`examples/proto2.rs` (`cargo run --example proto2 --profile bench`; full log
`proto2-run.txt`):

- **2A** `worklist_partition` / `worklist_minimize` — FIFO `(block,class)` splitter
  worklist, predecessor lists, add-smaller-half discipline. **DEAD is modeled as one
  absorbing pseudo-block** (`usize::MAX` sentinel, never receives real states) exactly
  mirroring the incumbent's partial-transition trap — so the coarsest stable partition,
  hence the minimized count, is identical by construction. Witness `abc` distinguished
  by "which classes fall off the cliff." `is_acyclic` (iterative DFS, self-loops =
  cycles) + `congruence_partition_acyclic` (one reverse-topological signature pass;
  targets resolved before sources on a DAG ⇒ coarsest partition in a single linear
  sweep). Reconstruction is byte-for-byte the incumbent's tail (`rebuild_from_partition`).
- **2B** Brzozowski `derivative`/`nullable` over the HIR (∅ = `None`), determinized over
  the DFA's **byte-equivalence-class representatives** (identical alphabet granularity ⇒
  counts directly comparable), each derivative canonicalized by `simplify_hir` (the five
  rules). ρ = states(saturate-only) / states(Hopcroft-minimal). Look-assertions/runaway
  bail (none in corpus).

Correctness gate cross-checks: worklist vs incumbent **minimized state count** +
language equivalence (`find_at` over the match buffer, all offsets) + worklist-vs-
congruence **partition-relation** equality on the acyclic subset.

## The numbers

**2A — correctness (the gate):** **26/26 patterns byte-identical** minimized state count
AND language-equivalent — cyclic included (`[a-z]+`, `\w+`, `0x…` → 2/2/4 states, match).
The congruence fast path matches the incumbent on all **4/4** acyclic patterns
(`[0-9a-fA-F]{3,8}`=9, `#[0-9a-fA-F]{3,6}`=8, `\d{4}-\d{2}-\d{2}`=11,
`\d{1,3}\.…`=16). **Correctness = EXACT.**

**2A — perf (cold-paired, N=500, interleaved order-cancelled, loadavg ≈ 7.0–7.7):**

| head-to-head | total ns | ratio vs incumbent | gate |
|---|---|---|---|
| incumbent hopcroft (26 pats/iter) | 8,314,368 | 1.000 | — |
| **worklist (2A), whole corpus** | 29,317,794 | **3.526×** | **SLOWER — NO-GO** |
| congruence (2A), acyclic subset (4 pats) | 2,593,996 | **0.804×** | **FASTER — exact-and-free** |
| incumbent on same acyclic subset | 3,226,622 | 1.000 | — |

Acyclic fraction where congruence is exact-and-free: **4/26 patterns (15%)**,
44/121 minimal states.

**2B — ρ (29 patterns: 3 witness-family + 26 corpus):**

| pattern | sat | minimal | ρ |
|---|---|---|---|
| `(ab)*a` | 2 | 2 | 1.000 |
| `a(ba)*` | 2 | 2 | 1.000 |
| **`c(ab)*a\|da(ba)*`** (composite) | **5** | **3** | **1.667 — UNDER-MIN** |
| **`\w+(?:[-_]\w+)*`** (organic corpus) | **3** | **2** | **1.500 — UNDER-MIN** |
| all other 25 corpus patterns | = | = | 1.000 |

worst ρ = **1.667**; 2/29 measured under-minimize. Direct witness proof:
`simplify_hir((ab)*a)` and `simplify_hir(a(ba)*)` are **DISTINCT** saturated forms —
the five rules do **not** unify the equal language, so a derivative automaton keying
states on saturated form keeps the two residuals apart (they only co-occur, hence
inflate the count, when reached together — as in the composite and the organic
`\w+(?:[-_]\w+)*`).

## Verdict

- **2A correctness: GO (EXACT).** Byte-identical on 26/26; the worklist + DEAD-as-
  absorbing-block reproduces the incumbent's coarsest partition exactly. The
  congruence closure is exact on 4/4 acyclic patterns.
- **2A perf: NO-GO as a wholesale replacement.** Worklist is **3.53× slower** on this
  corpus of tiny DFAs (n ≤ 16): Hopcroft's O(n·k·log n) win is latent, and its
  predecessor-list / per-split HashMap / per-class in-work allocations dominate at
  small n where the incumbent's alloc-light O(n²·k) restart wins. **The closer is NOT
  worklist-over-incumbent here.** The one shippable sliver is the **acyclic congruence
  fast path — exact AND 0.80× (20% faster)** — but it covers only **15%** of patterns,
  so it is a *targeted* fast path (dispatch acyclic → congruence, else incumbent), not
  a minimizer replacement.
- **2B: BLOCKED-candidate CONFIRMED (ρ>1, theory-bounded under-minimization).** ρ up to
  1.667, and an **organic corpus miss** at `\w+(?:[-_]\w+)*` (ρ=1.5). Saturating the
  five rules does **not** collapse determinize+minimize — **not a landmark.** 2B cannot
  be the closer.

**VERDICT LINE:** 2A is EXACT but not free (3.53× slower whole-corpus; congruence-acyclic
0.80× on 15%); 2B is BLOCKED (ρ_max=1.667, organic ρ=1.5). 5C's shippable core is the
**acyclic congruence fast path**, not the general worklist and not the derivative-
saturator — and even that needs the alloc-light rewrite to matter beyond the 15% acyclic
slice.

## Caveats / honesty

- Corpus is 26 (per the actual bench file), not the 50 the brief cites.
- Perf is minimizer-only head-to-head on pre-built un-minimized DFAs (identical inputs,
  paired, order-cancelled) — the fair A/B for the swap. Not full-compile wall time.
- The worklist is a correctness-first implementation (Vec-of-Vec predecessors, HashMap
  buckets); an arena/flat-CSR rewrite would cut the constant that loses at n≤16, but
  that is out of this probe's parsimony scope and unproven here.
- 2B derivative engine handles Literal/Class/Concat/Alternation/Repetition(★+?{n,m}) /
  Group; bails on Look (none in corpus). ρ=1 on 25/26 corpus patterns is consistent with
  the incumbent DFA counts, corroborating engine correctness (no ρ<1 anywhere).

## Deliverables

- `proto2-dfa.patch` — additions to `automata/dfa.rs` (worklist + congruence + acyclicity).
- `proto2.rs` — the driver/harness (2A verify+timing, 2B ρ).
- `proto2-run.txt` — full captured run.
