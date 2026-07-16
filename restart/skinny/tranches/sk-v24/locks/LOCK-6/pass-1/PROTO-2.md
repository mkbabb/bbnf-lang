# PROTO-2 — LOCK-6 (THE ISOMORPHISM PLANE) / family 6A / tier **T2 (surface correspondence)**

*Prototype agent, pass 1, probe 2 (t2). parse-that consumed READ-ONLY. Env: darwin aarch64,
18 cores. loadavg during the syntactic diff ~6.1; during the rustdoc run ~6–9 (recorded inline).
Elapsed within the 50-min cap.*

---

## The question

6A's **T2 tier** extracts each mirrored module's *public surface* (item set, field names,
variant sets, arities) to a committed snapshot and diffs it *through* a name map. Two things gate
whether T2 is a real mechanism rather than prose:

1. **Toolchain (open-Q ii / research-A risk 5):** does the Rust surface extractor (rustdoc-JSON via
   `cargo-public-api`/`cargo-semver-checks`) actually **run**, and is its nightly demand a **HARD
   blocker**?
2. **The "where reasonable ends" NUMBER (research-A prototype-test 3):** on the parse-that
   combinator core — Rust `parse.rs` + `combinators/methods/*` vs TS `parser.ts` — what fraction of
   the `Parser` surface maps **1:1** vs needs an **idiom escape hatch**? A high ratio validates 6A;
   a low ratio means the trees are too idiomatic to mirror and the family weakens. Previously
   UNMEASURED (separate repo, F6).

## Method (what ran, and the honest caveats)

- **Ratio:** a syntactic surface extractor + name-map differ, `t2-surface-diff.py` (this dir),
  run READ-ONLY over the real parse-that trees. It extracts the `Parser` method surface from both
  sides at exactly T2's granularity — **name + arity** — applies a 4-entry hand name map
  (`new↔constructor`, `parse_return_state↔parseState`, `map_with_state↔mapState`,
  `look_ahead↔lookAhead`; snake↔camel is mechanical, not a hand entry) and a reviewed one-sided
  **idiom allow-list**, then classifies every item. This is a lighter stand-in for the ts-morph TS
  extractor + a `syn`-level Rust extractor; it does *not* resolve types (unnecessary for the
  name/arity correspondence ratio). Caveat: full-fidelity arities would come from rustdoc-JSON on
  the built crate (below) — the syntactic arities matched the rustdoc arities on the probe crate,
  so the ratio is defensible; a heavy real-crate rustdoc run is the natural fidelity upgrade, not a
  correction.
- **Toolchain:** an actual `cargo +nightly-2026-04-11 rustdoc -- -Z unstable-options
  --output-format json` run on a minimal 12-line `Parser`-twin crate (no deps → seconds, the
  parsimonious mechanism proof). This answers "does the extractor run + what does it demand"
  without a cold build of parse_that's egraph/csp-solver/bbnf dep graph. The nightly-vs-stable
  verdict does not need the heavy build: R3 §2 already establishes parse-that is **nightly-pinned**
  (`portable_simd` + `smallbox` nightly feature + transitive path-deps), so the toolchain answer is
  determined by that pin, and the run confirms the JSON output shape.

## What ran — the numbers

### 1. The clean-map ratio (`python3 t2-surface-diff.py`, loadavg ~6.1)

`Parser` combinator-core surface: **34 Rust methods, 24 TS methods, 36 distinct mirror items.**

| class | count | fraction |
|---|---|---|
| **CLEAN** (name-map + identical arity) | 17 | **47%** |
| ARITY-DRIFT (name maps, arity differs) | 5 | 14% |
| **MAPPED total** (clean + arity-drift) | **22** | **61%** |
| RUST-ONLY idiom (allow-listed) | 12 | 33% |
| TS-ONLY idiom (allow-listed) | 2 | 6% |
| **idiom allow-list total** | **14** | **39%** |
| **UNMAPPED (would be RED)** | **0** | **0%** |

- **The 17 CLEAN pairs** are the core combinator vocabulary:
  `then · or · chain · map · mapState · skip · next · opt · not · minus · peek · lookAhead ·
  recover · call · parse · eof · parseState`. This is the reusable spine, and it mirrors exactly.
- **The 5 ARITY-DRIFT pairs cluster at one seam — parameterization idiom.** Rust factors variants
  into *separate methods* and uses range/typed args; TS *folds* them into one method with default
  params: `many(Range)`↔`many(min=0,max=∞)`, `sep_by(sep)`+`sep_by_ws`+`sep_by_ws_until`↔one
  `sepBy(sep,min,max)`, `trim`↔`trim(...,discard)`, `wrap/2`↔`wrap/3`, `new/1`↔`constructor/2`.
  T2's arity check *catches* these — they are the measured boundary where the idioms genuinely
  diverge in how they parameterize.
- **The 14 idiom items are genuinely one-sided.** Rust-only: `many_small`/`sep_by_small`
  (SmallVec inline-storage), `sep_by_ws`/`sep_by_ws_until` (ws folded into TS `ParserContext`
  instead), `parse_with_context`/`map_with_ctx`/`parse_return_state_with_context` (lifetime-threaded
  `&Context`), `parse_or_error` (`Result` vs TS state-carried error), `save_state`/`trim_whitespace`
  (flag-setting builders — TS routes these through a `ParserContext` object, verified at
  `parser.ts:494,516` + `state.ts:157`), `negate`/`trim_keep` (convenience). TS-only: `debug`,
  `toString` (JS conventions).

### 2. rustdoc-JSON T2 Rust extractor (`cargo +nightly-2026-04-11 rustdoc … --output-format json`)

```
Finished `dev` profile in 0.22s   |   wall 0.25s   |   exit 0   |   format_version 57
methods (name, arity incl. self): [('many',2), ('then',2), ('trim_whitespace',1), …]
enum variants extracted:          ['Eof', 'Mismatch', 'Unexpected']
```

- **RUNS.** Produces item-granularity JSON with **method arities and full enum variant sets** —
  precisely the data T2 diffs on. 0.25s cold on the minimal crate (well within the proof-gate lane
  cost model; loadavg ~6–9 at the run).
- **Toolchain demand:** `-Z unstable-options --output-format json` ⇒ **nightly**. But parse-that is
  **already** pinned to `nightly-2026-04-11` (R3 §2: `portable_simd`, `smallbox` nightly,
  transitive path-deps), so rustdoc-JSON's nightly demand is **subsumed — NOT a new blocker**. The
  "stable-only HARD blocker" branch of open-Q ii is **moot for parse-that**; it bites only a
  hypothetical stable-only greenfield.
- **Concrete T2 implementation note (measured, not in the specs):** rustdoc-JSON emits **blanket
  std-trait noise** (`borrow`, `borrow_mut`, `from`, `into`, `try_from`, `try_into`, `type_id`)
  alongside the crate-local items. The differ **must scope to crate-local `impl Parser` items** or
  the surface diff floods. This is a real, cheap-to-handle detail the extractor owes.

## Verdict

**RAN_MEASURED.** T2 is a live, cheap mechanism, and the family's decisive number is now measured.

- **T2 runs.** rustdoc-JSON extracts method arities + variant sets in 0.25s on parse-that's already
  nightly-pinned toolchain; the "stable-only" HARD-blocker branch of open-Q ii **does not apply** to
  parse-that. The TS side is a symmetric ts-morph/line-scan extractor (proven here by the running
  differ).
- **The "where reasonable ends" number: 47% strict-clean / 61% name-mapped / 0% unmapped, with a
  39% idiom allow-list.** The combinator *spine* (17 items) mirrors cleanly and zero items are left
  unmapped after a **4-entry** name map — 6A is **feasible** on the combinator core, refuting the
  "trees too idiomatic to mirror" failure. **But the 39% exemption fraction lands squarely on
  research-A risk 4 (allow-list inflation):** two of every five `Parser` items sit in the escape
  hatch, so a 6A linter here is proving correspondence on ~60% of the surface while *budgeting*
  the other 40% as reviewed idiom. The exemption-ratio budget (gate d) is therefore **load-bearing,
  not decorative** — it is the number that decides whether 6A's T2 stays honest or goes vacuous.
- **Corroborates the family disposition:** T2 is necessary-but-not-sufficient. Arity-drift is
  caught structurally (good), but the CLEAN spine says nothing about behavior — the LIVE
  `$.statuses[0].nope` asymmetry passes green here — so T2 without T3 is provably insufficient
  (probe for that seam is PROTO-1/T3 territory).

**One-line:** *6A's T2 surface tier RUNS (rustdoc-JSON, 0.25s, already-nightly → no new blocker),
and the combinator-core clean-map ratio is 47% strict / 61% name-mapped / 0% unmapped — feasible,
but with a 39% idiom allow-list that makes the exemption-ratio budget the family's real hinge.*

---

### Artifacts
- `t2-surface-diff.py` — the runnable T2 syntactic surface extractor + name-map differ (this dir).
- rustdoc-JSON probe crate: `…/scratchpad/rustdoc-probe/` (throwaway; proves the extractor).
- No mutation of parse-that (verified `git status` unchanged: only pre-existing `??` untracked).
