# PROTO-2 (pass 2) — regex Tier-S under a pinned ORACLE_REL_V1 (family 7D META-CIRCULAR-ORACLE)

Probe 2 of LOCK-7 pass 2. A **real prototype**, RAN cold and paired against a pt-scratch copy
of parse-that (origin READ-ONLY, verified byte-identical to HEAD `ef10d5b`, untouched after the
run). The relation is **pinned and committed as code** (`ORACLE_REL_V1`), every number cites it,
and all four planted mutations trip before any green is trusted. Deliverable code: `PROTO-2.patch`
(one probe helper added to the copy's `regex/mod.rs`; one new example harness).

---

## Question

Under a pinned `ORACLE_REL_V1`, what is regex **structure**-parity between the `parse_generated`
PRODUCER (`regex/mod.rs:34`, → `Hir`) and the primary hand `parse()` (`regex/hir/parser`, → `Hir`),
and is **every residual divergence** a **LOCK-5 `host.rs` OBLIGATION** (deterministic fold — fix in
place) or an **oracle VERDICT** (genuine pre-fold parser/grammar divergence — bank in the ledger)?
I.e. is regex the first **PROMOTABLE** subsystem, or **ORACLE-ONLY**?

**Answer (measured): ORACLE-ONLY.** Not every residual is a `host.rs` obligation. Of 12
divergences, **9 are LOCK-5 `host.rs` obligations** (one bug class, bounded `{n,m}`) and **3 are
genuine oracle VERDICTS** rooted in `bootstrap/regex.bbnf` (the `.` metachar and the `(?s)`
inline-flag syntax) — a locus **distinct from LOCK-5**. So even after LOCK-5's host fold is
completed, regex stays ORACLE-ONLY pending grammar fixes, and it independently fails the perf gate
(**GEN 1.77× slower**). This resolves XL-2 routing: **9 → LOCK-5 `host.rs`, 3 → oracle bank
(`regex.bbnf`)**.

## Binding pin (anti-vacuity)

Tier-S is bound to the PRODUCER `parse_that::regex::parse_generated_with -> Result<Hir, _>`
(generated combinator parser + `host::lower`), **never** the `generated.rs:2288`
`parse_generated -> bool` twin. The bool twin is used ONLY as a pre-lower *recognizer witness* for
classification; planted mutation **M3** proves that binding Tier-S to it is rejected as vacuous.

## Method

- **Copy, not mutate** (R3 §8): `rsync` copy, external patch-deps repointed to originals,
  `CARGO_TARGET_DIR` inside the copy, toolchain `nightly-2026-04-11` (repo pin). `git status` on the
  origin after the run: regex-path / bootstrap trees clean vs `ef10d5b`.
- **`ORACLE_REL_V1 = R1 ∘ R2 ∘ R3`, pinned as the function `norm(&Hir) -> Hir`** in the harness:
  - **R1** — bottom-up unit-collapse to a fixpoint: `Concat([]) ≡ Empty ≡ Alt([])`,
    `Concat([x]) ≡ x`, `Alt([x]) ≡ x` (both sides emit `Hir::Empty` at `host.rs:44,54,89`).
    `norm` is verified **idempotent** (fixpoint sanity).
  - **R2** — satisfied by construction: both sides terminate in `host::lower` / the hand parser at
    the **same stage** (post-parse `Hir`, no linker pass) with a **shared** `ParseOptions::default()`
    (`unicode:true`).
  - **R3** — group / class / quantifier structure preserved **field-for-field**
    (`Repetition{min,max,greedy}`, `Group` nesting, `Class` ranges/negated).
- **Divergence partition** — the operative law: `host::lower` is a **deterministic, total function of
  `(AST, src, opts)`**; both sides share `opts` (R2). Therefore
  - a **both-accept, `N`-differs** row is impossible unless the generated parser committed to a
    *different tree* → **pre-lower → oracle VERDICT** (the printed AST names which wrong decision);
  - a **membership** row where the recognizer consumed all input (`pre_recognizer_full=true`) but the
    fold **errored** is a fold *partiality* → **post-lower → `host.rs` OBLIGATION** (name the clause).
- **Corpus**: the committed **51-pattern PROTO-2-7D set** + **7** `parse_that/tests/regex` patterns
  (`(?s).`, `(?:abc)`, `[^abc]`, `[a-z]`, `\/`, `\d`, `\w`) = **58**.
- **Perf**: cold, **paired, interleaved**, release `-O3`, **3000 reps**, per-corpus-pass median,
  `black_box` vs DCE, 50-iter untimed prime (no result caching — every parse allocates a fresh
  `Hir`, so every timed pass is cold). loadavg captured around each run. **3 runs** for stability.

## What ran

`cargo run --release --example oracle_7d -p regex-bootstrap` inside pt-scratch (harness =
`bootstrap/examples/oracle_7d.rs`; probe helper `parse_generated_ast_debug` added to the copy's
`regex/mod.rs`). Executed 3×; parity + classification deterministic across runs.

## The numbers

**Planted-mutation falsification (all four MUST trip — they did):**

| mutation | what it perturbs | tripped |
|---|---|---|
| M1 quantifier greedy→lazy in a gen copy | `Repetition.greedy` field | **yes** — R3 flags |
| M2 drop a `Group` wrapper | group nesting | **yes** — R3 flags |
| M3 bind the `bool` twin where the producer is expected | twin returns `bool` (no `Hir`); it maps `(a)` and `a` to the SAME `true` while their `Hir`s differ → provably vacuous | **yes** — binding rejected |
| M4 mis-associate an alt arm (`Alt([a,b,c])`→`Alt([Alt([a,b]),c])`) | alt associativity | **yes** — R1 collapses only singletons, does NOT mask it |
| (`norm` idempotent / R1 fixpoint) | — | **yes** |

**Parity ledger under `ORACLE_REL_V1` (deterministic; 58 patterns):**

| metric | value |
|---|---|
| strict Hir-eq (pre-R1) | 45/58 = **0.7759** |
| R1 delta (`Concat([]) ≡ Empty`, the `""` row) | +1 |
| both accept + `N`-equal | 46 |
| both accept + `N`-**diverge** | 3 |
| membership disagree (HAND-OK / GEN-ERR) | 9 |
| both reject | 0 |
| **PARITY (Empty-normed, ORACLE_REL_V1)** | **46/58 = 0.7931** |
| — committed-51 subset only | 40/51 = **0.7843** (reconciles pass-1's 40/51) |

**Divergence ledger — 12 rows, 100% cleanly classified:**

- **9 × `host.rs` OBLIGATION** — one bug class, **bounded `{n,m}`**. Every bounded quantifier
  (`{n}`, `{n,m}`, `{n,}`) is HAND-OK / GEN-ERR `"invalid repetition bound"`, with
  `pre_recognizer_full=true` (the combinator parser consumed the whole input and produced a full
  `quantified((atom, quantifier(span)))` AST — the *same* structural decision as the hand parser).
  The fold then fails: **`host.rs::parse_braced_quantifier` (via `lower_quantifier`) receives the
  span WITH its braces (`{3,5}`) and never strips them**, so `num_part[..comma]` = `"{3"` fails
  `u32::parse`. Hits: JSON `u[0-9a-fA-F]{4}`, `[0-9a-fA-F]{3,8}`, `a{3,5}`, `#…{3,6}`,
  `\d{4}-\d{2}-\d{2}`, `\d{2}:\d{2}:\d{2}`, IPv4 `\d{1,3}…`, the UUID pattern, `(?:[a-z]{2,4})+`.
  → **route to LOCK-5**; deterministic fold, fix in `host.rs`.
- **3 × oracle VERDICT** — genuine pre-lower (`regex.bbnf`) divergences, witnessed by the AST:
  - `.` → parser emits `atom(class_atom_0("."))` — the dot is parsed as a **class-literal node**,
    never the `atom_0` dot-metachar. `host.rs::make_dot` is never reached; `N(gen)=Class([46])`
    (literal dot) vs `N(hand)=Class([^\n])`.
  - `(?s).` and `(?s)(?:\s|/\*.*?\*/)*` → `(?s)` emits **plain** `group(class_atom_0("s"))`, never
    the bare-flag `group_1`. `host.rs::apply_flags` is never reached; the inline flag becomes a
    literal capturing group and the dotall `.` stays a literal.
  → **bank in the oracle ledger**; these need `regex.bbnf` grammar fixes, NOT `host.rs`.

**Perf — cold paired, per 58-pattern corpus-pass (3 runs):**

| run | HAND µs | GEN µs | ratio GEN/HAND | loadavg(1m) |
|---|---|---|---|---|
| A | 15.167 | 26.875 | **1.772×** | 8.90 |
| B | 15.375 | 27.083 | **1.761×** | 7.47 |
| C | 15.416 | 27.250 | **1.768×** | 7.47 |

GEN is **1.76–1.77× slower** (median 1.768×; range < 1%). Consistent with pass-1's 1.77–1.81×.

## Verdict

**VERDICT: RAN (measured). regex Tier-S under `ORACLE_REL_V1` = parity 0.7931 Empty-normed
(45→46/58 after R1), which is BELOW the 0.80 gate; classification 100% clean = 9 `host.rs`
OBLIGATIONS (bounded `{n,m}` — LOCK-5's incomplete fold, `parse_braced_quantifier` brace-strip) + 3
oracle VERDICTS (`regex.bbnf` dot-metachar + inline-flag). Perf GEN/HAND ≈ 1.77× slower. Disposition:
ORACLE-ONLY. regex is NOT the first PROMOTABLE subsystem — it fails promotion on TWO independent
counts: (i) 3 residual oracle verdicts that LOCK-5 cannot discharge (grammar locus), and (ii) the
perf gate. state = BLOCKED with the row.**

**Ledger row.** `rel_id=ORACLE_REL_V1 · binding=parse_generated_with(->Hir) · parity_pct=0.7931 ·
perf_ratio=1.768× · state=BLOCKED (ORACLE-ONLY)`. `row_complete=false` by the strict conjunction
(parity ≥ 0.80 FAILS at 0.7931; classification-clean HOLDS). It is nonetheless the **first fully
partitioned** regex row: XL-2 routing is settled — **9 → LOCK-5 `host.rs`, 3 → oracle bank**.

**Corrections to pass-1.** Pass-1 lumped `.` and `(?s)` as "genuine semantic bugs" while its
headline implied only the `{n,m}` class was the non-verdict residue. The pinned oracle sharpens
this: the `{n,m}` class is 9 `host.rs` obligations; the `.` / `(?s)` pair are **3 oracle verdicts
in the grammar**, not host obligations. The deterministic-fold law makes the partition mechanical:
both-accept-Hir-differ ⟹ pre-lower ⟹ verdict; recognizer-full + fold-error ⟹ post-lower ⟹ obligation.

**Honesty caveats.** (1) release `-O3`, not the `bench` fat-LTO profile — immaterial to a
same-binary ratio (both sides identical profile). (2) Machine under concurrent load (loadavg
7.5–8.9); paired interleaving makes the ratio robust (spread < 1%). (3) The 7 added test patterns
lower parity by one row (`(?s).`), a real surface, not a stack of the deck; committed-51-only parity
is 0.7843, likewise < 0.80. None of these move the verdict: both promotion gates fail by wide
margins, and the residual carries genuine oracle verdicts.
