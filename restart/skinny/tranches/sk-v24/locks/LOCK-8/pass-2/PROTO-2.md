# LOCK-8 · pass-2 · PROTO-2 (probe t2, family 8C — Brief 2)

**Plane:** THE TS BACKBONE PLANE (parse-that) · **Family:** 8C ISOMORPHIC MIRROR
**Probe:** the regex alternation-order behavioral differential (R-C1) — Rust bespoke DFA vs TS native sticky `RegExp`
**Verdict line:** **RAN_MEASURED — 8C regex row = MIRRORED-WITH-GUARD (not unconditional LAWFUL-NON-MIRROR).** The two-engine divergence is REAL and now measured (6 paired synthetic cases: Rust leftmost-**longest** vs JS leftmost-**first**), spanning **two** hazard classes — ordered-alternation-shorter-first **and** lazy-quantifier — but **all 10 real parse-that regex leaves agree byte-for-byte**. The regex row is a lawful behavioral mirror on today's corpus only; it requires a build-time GUARD forbidding both hazard classes, because any future leaf that used either would silently become DRIFT.

---

## Question

Research-C §3/§8 (R-C1) names the single highest-value 8C correspondence uncertainty: JS `RegExp` is leftmost-**first** with backtracking (`/a|ab/` on `"ab"` → `"a"`), a classical DFA is leftmost-**longest** (`→ "ab"`). **Two sub-questions, both un-run:**
1. Does parse-that's actual Rust regex engine (the one the runtime `regex()` leaf drives) genuinely disagree with the TS `regex()` leaf on ordered alternation — or is the DFA priority-ordered (leftmost-first, like BurntSushi's `regex` crate) so there is no divergence at all?
2. If it does diverge, does **any real parse-that regex leaf** trigger it — i.e. is the regex ledger row a lawful behavioral MIRROR (no leaf triggers it) or silent DRIFT (a leaf does)?

A "behavioral mirror" claim for regex is false unless (1) is characterized and (2) is answered by census + differential, not assumed.

## Method

Scratch copy per R3 §8 (parse-that is READ-ONLY): `rsync -a` of `/Users/mkbabb/Programming/parse-that` → scratchpad `pt-scratch/` (excluding `target/ .git/ node_modules/ data/`), external patch deps repointed to originals in `rust/.cargo/config.toml`, isolated `CARGO_TARGET_DIR` inside the copy. No mutation of the original tree.

**Both twins drive the exact runtime matchers, not re-implementations:**
- **Rust leg** — `bbnf_regex::Dfa::compile(pat)` + `dfa.find_at(bytes, 0)`, the *identical* call the runtime `regex()` leaf makes (`parse_that/src/leaf.rs:74` `dfa_impl` → `find_at`; confirmed the leaf has no alternate matcher path). Added `rust/regex/tests/alternation_order_differential.rs`, run via `cargo test -p bbnf-regex`.
- **JS leg** — `new RegExp(source, "y")` + `sticky.test()` + `lastIndex` delta, the *identical* construction the TS `regex()` leaf makes (`typescript/src/parse/leaf.ts:321-347`). `js-differential.mjs` under **Node v26.0.0** (Maglev/production tier).

Identical `(id, pattern-source, input)` triples fed to both; each emits `<ENGINE> <id> <matched-len|NONE>`; outputs paired and diffed. Corpus = 9 synthetic probes (ordered alternation both orders + lazy quantifiers) + **10 real regex leaves censused from parse-that** (`json.ts:21-22` number/string, `csv.ts` `[^"]+`/`[^,]+`, `leaf.ts:397` `/\s*/`, `utils.ts` hex4/number). loadavg recorded: build start `{26.86, 21.34, 15.88}`, read `{15.95, 19.06, 15.36}` (this is a **correctness** differential — no perf claim rides on load).

### The mechanism, read from source first (then confirmed by measurement)

- `Dfa::find_at` (`automata/dfa.rs:116-139`) returns `last_accept` — the **last** accepting position reached before DEAD/EOF = **leftmost-longest**. Doc comment: *"Returns the end position of the longest match."*
- `subset_construction` (`dfa.rs:218-256`) + `epsilon_closure` (`:189-202`) build DFA states as **plain `is_accept` sets** and **discard the NFA's priority annotations entirely** — priority is never read during construction.
- The NFA *does* build priority-ordered ε-edges intending leftmost-first (`automata/nfa.rs:313` `"Priority = index → leftmost-first semantics"`, `:18-19` greedy/lazy priority) — **but the DFA throws them away.** So the engine's stated intent (leftmost-first, JS-mirroring) is **not realized** by the DFA path the runtime uses. This also means a **lazy quantifier collapses to greedy** in the DFA (its priority is discarded), a *second* divergence class R-C1 did not name.

## What ran, and the numbers

Paired Rust(`find_at`, leftmost-longest) vs JS(sticky `RegExp`, leftmost-first). Matched length in bytes.

| id | pattern | input | rust | js | verdict |
|---|---|---|--:|--:|---|
| alt_a_ab__ab | `a\|ab` | `ab` | 2 | 1 | **DIVERGE** |
| alt_a_ab__abc | `a\|ab` | `abc` | 2 | 1 | **DIVERGE** |
| alt_ca_cat__cats | `ca\|cat` | `cats` | 3 | 2 | **DIVERGE** |
| alt_foo_foobar__fb | `foo\|foobar` | `foobar` | 6 | 3 | **DIVERGE** |
| lazy_digits__123 | `\d+?` | `123` | 3 | 1 | **DIVERGE** (lazy→greedy) |
| lazy_astar_b__axbxb | `a.*?b` | `axbxb` | 5 | 3 | **DIVERGE** (lazy→greedy) |
| alt_ab_a__ab | `ab\|a` | `ab` | 2 | 2 | agree (longer-first) |
| alt_foobar_foo__fb | `foobar\|foo` | `foobar` | 6 | 6 | agree (longer-first) |
| alt_cat_ca__cats | `cat\|ca` | `cats` | 3 | 3 | agree (first alt longest) |
| **json_num__neg** | `-?(?:0\|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?` | `-12.5e3` | 7 | 7 | **agree** |
| **json_num__zero** | ″ | `0` | 1 | 1 | **agree** |
| **json_num__int** | ″ | `123` | 3 | 3 | **agree** |
| **json_num__zeroish** | ″ | `0123` | 1 | 1 | **agree** |
| **json_str__esc** | `"(?:[^"\\]\|\\(?:["\\/bfnrt]\|u[0-9a-fA-F]{4}))*"` | `"a\nb"` | 6 | 6 | **agree** |
| **csv_notquote__ab** | `[^"]+` | `ab,cd` | 5 | 5 | **agree** |
| **csv_notcomma__ab** | `[^,]+` | `ab,cd` | 2 | 2 | **agree** |
| **ws_star__spaces** | `\s*` | `   x` | 3 | 3 | **agree** |
| **ws_star__none** | `\s*` | `x` | 0 | 0 | **agree** |
| **hex4__cafe** | `[0-9a-fA-F]{4}` | `cafe99` | 4 | 4 | **agree** |

**Totals: 13 agree / 6 diverge.** Every diverging row is synthetic; **every real parse-that regex leaf (the 10 bold rows) agrees.**

### Why the real corpus agrees (census, mechanical)

`grep -rnoE 'regex\(/…/\)'` over `typescript/src` + `grep 'regex(_span)?\("…"'` over `parse_that/src` → the complete set of runtime regex-leaf patterns. **None** contains a shorter-first ordered alternation and **none** contains a lazy quantifier:
- The two `|` sites that exist — `0|[1-9]\d*` (json number) and `[^"\\]|\\…` (json string) — have **disjoint first bytes**, so at any position exactly one branch can match; leftmost-longest ≡ leftmost-first when branches don't overlap.
- Every quantifier is greedy (`*`,`+`,`{4}`), and JS greedy ≡ DFA longest.

So the two engines are *behaviorally identical on the corpus that exists*, and provably diverge only on inputs the corpus does not contain.

## Findings

1. **R-C1 is CONFIRMED, not speculative — and BROADER than stated.** The Rust runtime regex path is genuinely leftmost-**longest** (`find_at` returns `last_accept`), disagreeing with JS leftmost-first on ordered alternation. Research-C named *one* hazard class (ordered alternation); the probe surfaces a **second**: because the DFA discards *all* NFA priority, **lazy quantifiers collapse to greedy** and also diverge. Both are measured.
2. **The engine is internally self-contradictory about its match semantics** — a latent defect, not just a mirror question. `nfa.rs:313` builds priority edges *intending* leftmost-first (to mirror JS); `dfa.rs` subset construction discards them and `find_at` delivers leftmost-longest. The design intent (mirror JS) is silently unmet on the DFA path. Un-triggered today, but any priority-sensitive leaf would expose it inside Rust alone.
3. **The regex ledger row is MIRRORED on the current corpus, but the mirror is CONDITIONAL.** It is **not** an unconditional `LAWFUL-NON-MIRROR(two-engines; HIR-isomorphic)`. Correct classification:
   `regex :: LAWFUL-NON-MIRROR(two-engines) + MIRRORED(behavioral, GUARDED)` — the parity holds **only while a build-time guard forbids (a) ordered alternation with a shorter-first alternative and (b) any lazy quantifier in a regex leaf.** Without the guard the row is a latent DRIFT waiting for one grammar edit.
4. **The 6D parity corpus must include both hazard classes.** Research-C's brief said "ordered-alternation + anchoring"; this probe proves it must *also* carry lazy-quantifier cases, or the harness would green a corpus that hides the second divergence.

## Verdict

**RAN_MEASURED.** The single highest-value 8C uncertainty is resolved: the two-engine regex divergence is **real, measured, and two-class** (ordered-alternation-shorter-first + lazy-quantifier), yet **no current parse-that regex leaf triggers it** (census + 10/10 corpus agreement). The regex row is therefore a **guarded** behavioral mirror, not an unconditional lawful non-mirror — the ledger row carries a build-time guard (forbid shorter-first alternation + lazy quantifiers in leaves) and the 6D corpus carries both hazard classes. The family's signature falsifier for this row — "the behavioral-mirror claim is silently false" — did **not** fire on the corpus, but **would** fire on one grammar edit absent the guard; that is the material result.

*Files: patch `proto-2.patch` (adds `rust/regex/tests/alternation_order_differential.rs`); JS twin `js-differential.mjs`; raw evidence `evidence-rust-differential.out` / `evidence-js-differential.out`. Scratch: `pt-scratch/` (READ-ONLY original untouched; isolated CARGO_TARGET_DIR). Seams read: `parse_that/src/leaf.rs:72-98` (regex leaf → `dfa_impl` → `find_at`), `rust/regex/src/automata/dfa.rs:116-139/189-256` (leftmost-longest + priority discard), `rust/regex/src/automata/nfa.rs:18-19/313` (NFA leftmost-first intent), `typescript/src/parse/leaf.ts:316-347` (sticky RegExp leaf).*
