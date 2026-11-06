# LOCK-9 · PROTO-2 — Tier-2 Sheng feasibility census (vectorized-DFA go/no-go)

**Family 9C · probe 2 · pass 1.** Deterministic, no bench. Verdict feeds the
LOCK-2 scan-plan-selection island and the LOCK-5 coupling.

---

## Question

Do the archetypal scanner leaves minimize to a DFA small enough to lower as a
NEON `vqtbl` transition kernel — the Sheng idiom? This is 9C's distinguishing
novum and its go/no-go. If most leaves blow the register budget, 9C degrades to
Tiers 0/1/3 (prefilter / shufti / simdjson) with dense classify staying
hand-written — a named partial.

**Pass/fail gate.** Tier-2 GO iff BOTH the JSON string-body leaf AND the
number-span leaf fit `≤ 64` `state × class` entries (the `vqtbl4q_u8` budget).
If either exceeds 64, Tier-2 Sheng is a minority tier — report the measured
negative and mark 9C's coverage a percentage.

Two register budgets are in play:

- **Tier-2 strict — `vqtbl1q_u8`**: `state_count ≤ 16` **AND** `class_count ≤ 16`.
  One 16-byte NEON table lookup realises the whole per-class transition
  (`state' = vqtbl1q_u8(row[class], state_vec)`).
- **tbl4 — `vqtbl4q_u8`**: `state_count × class_count ≤ 64` (four chained
  128-bit tables). The looser ceiling the pass/fail gate names.

---

## Method

`parse-that` is READ-ONLY. Per R3 §8 the working tree was `rsync`-copied to a
scratch prover, external sibling patch deps repointed to the originals
(read-only), and built with an isolated `CARGO_TARGET_DIR` under the copy —
zero mutation of the original, no `$SRC/rust/target` touched.

- Scratch copy: `…/scratchpad/pt-scratch` (rsync of `parse-that`, excluding
  `target/ .git/ node_modules/ data/ *.png`).
- Source of record: `parse-that@ef10d5b` (`docs(coordination): VALUEJS-PT-E`).
- Toolchain: `nightly-2026-04-11` (repo pin, resolved from the copied
  `rust-toolchain.toml`).
- Isolated target dir; external patches (`egraph`, `egraph-derive`,
  `csp-solver`, `pprint`, `bbnf*`) repointed absolute.

**Probe** (the smallest that answers the question): a scratch integration test
`rust/regex/tests/sheng_census.rs` compiles each leaf through the existing
pipeline — `automata/dfa.rs::Dfa::compile_with` with `minimize: true`
(→ subset construction → `hopcroft_minimize`) — and prints `state_count()`
(dfa.rs:105) `×` `class_count()` (dfa.rs:110) per leaf.

The DFA operates on **byte equivalence classes** (`byte_classes:[u8;256]`,
`num_classes`), computed by `sets/equiv.rs::compute_byte_classes` from the NFA's
transition byte-sets — exactly the alphabet a `vqtbl` byte-shuffle consumes.

A NEON byte shuffle consumes **bytes**, so the honest Sheng model is **byte
mode** (`unicode: false`). The census also emits the `unicode: true` column (the
literal `Dfa::compile` default) for transparency; the two coincide for all five
leaves (the engine's `\d`/`\s`/`.` shorthands and the escaped-dot in the
string/number leaves did not expand the byte-class alphabet beyond ASCII here),
so the verdict is robust to the mode question.

Leaves (exactly as named in the brief; classify.rs `RegexClass` canonical
archetypes):

| leaf | pattern |
|---|---|
| number | `-?(?:0\|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?` |
| string-body | `"(?:[^"\\]\|\\.)*"` |
| ws+comment | `(?:\s\|/\*.*?\*/)*` |
| identifier | `[A-Za-z_][A-Za-z0-9_]*` |
| hex | `[0-9a-fA-F]+` |

**Command** (run in the copy, isolated target):

```bash
cd $DST/rust && CARGO_TARGET_DIR=$DST/target \
  cargo test -p bbnf-regex --release --test sheng_census -- --nocapture
```

`loadavg` at run: `13.19 12.47 12.09` (deterministic census — recorded for
completeness, not a perf claim). Compiled clean, `test result: ok. 1 passed`.

---

## What ran / the numbers

Minimized DFA per leaf (**byte mode = unicode mode**, identical for all five):

| leaf | states | classes | state × class | Tier-2 strict (≤16 & ≤16 · `vqtbl1q_u8`) | tbl4 (s×c ≤ 64 · `vqtbl4q_u8`) |
|---|---:|---:|---:|:---:|:---:|
| **number** | 9 | 7 | **63** | **YES** | **YES** |
| **string-body** | 4 | 4 | **16** | **YES** | **YES** |
| ws+comment | 5 | 5 | 25 | YES | YES |
| identifier | 2 | 3 | 6 | YES | YES |
| hex | 2 | 2 | 4 | YES | YES |

- tbl4-fitting leaves: **5/5 (100%)**.
- Tier-2 strict (`vqtbl1q_u8`) fitting leaves: **5/5** — every leaf fits the
  *tight* single-16-byte-register budget, not merely the looser tbl4 ceiling.

Notes on the two gate leaves:

- **string-body 4×4 = 16.** States: `expect-quote → body → escape → closed`;
  classes `{"}`, `{\}`, `{other}`, plus the initial-partition class. A textbook
  small automaton, well inside `vqtbl1q`.
- **number 9×7 = 63.** The tightest fit — **one entry below the 64 tbl4
  ceiling**, yet it *still* satisfies Tier-2 strict (9 states ≤ 16, 7 classes ≤
  16), so it needs only a single `vqtbl1q_u8`, not tbl4. Classes: `{-}`, `{+}`,
  `{0}`, `{1-9}`, `{.}`, `{eE}`, `{other}` = 7; the 9 states track
  sign/int-lead-zero/int/frac/exp-sign/exp with the accept structure. The
  report's `~6–8 × 7` estimate was close; the measured height is 9.

The `s×c = 63` for number vindicates why this had to be **measured, not
estimated**: the leaf lands one unit under the tbl4 gate, and the estimate band
(6–8 states) bracketed the boundary. On states-and-classes independently it is
comfortably inside the *strict* budget regardless.

---

## Verdict

> **TIER-2 SHENG — GO.**
> Both gate leaves fit `≤ 64` state × class: **string-body 4×4 = 16 FIT**,
> **number 9×7 = 63 FIT**. Moreover all **5/5** archetypal leaves fit the
> **strict `vqtbl1q_u8`** budget (≤16 states AND ≤16 classes) — a single
> 16-byte NEON table lookup per class realises each leaf's transition; tbl4 is
> not even required. 9C's distinguishing novum is feasible; no degrade, no
> coverage-percentage caveat. Dense classify stays hand-written as the class
> map that feeds the shuffle; Sheng carries the transition step for the leaf
> family measured here.

**Coverage of the measured leaf set: 100% Tier-2-eligible.** No leaf falls back
to Tiers 0/1/3 on register-budget grounds. (The Tier 0/1/3 fabric remains the
right tool for structural / prefilter / bulk-string work — this census only
adjudicates the *transition-kernel* question, and answers it GO.)

### Downstream

- **LOCK-2 (scan-plan selection):** Sheng is admissible as the transition
  strategy for the archetypal leaf family; the plan selector may treat
  `state ≤ 16 ∧ class ≤ 16` as the lowering predicate (all five archetypes pass).
- **LOCK-5 (coupling):** the class map (dense classify) and the Sheng shuffle
  compose — classify produces the `class` byte, Sheng's `vqtbl1q_u8` advances
  the state. No orthogonal codepath.

---

## Artefacts

- `sheng_census.patch` — unified diff of the scratch test (new file
  `parse-that/rust/regex/tests/sheng_census.rs`, 157 lines). READ-ONLY probe:
  lives only in the scratch copy; the patch is the record.
- `sheng_census.run.log` — full captured run (compile + table + verdict).

### Reproduce

```bash
SRC=/Users/mkbabb/Programming/parse-that
DST=<scratch>/pt-scratch
rsync -a --exclude target/ --exclude .git/ --exclude node_modules/ \
      --exclude data/ --exclude '*.png' "$SRC/" "$DST/"
# repoint $DST/rust/.cargo/config.toml external patch prefixes to absolutes
# (../../pprint → /Users/mkbabb/Programming/pprint, likewise bbnf-lang, csc411)
cp sheng_census.patch → apply to $DST/rust/regex/tests/sheng_census.rs
cd "$DST/rust" && CARGO_TARGET_DIR="$DST/target" \
  cargo test -p bbnf-regex --release --test sheng_census -- --nocapture
```
