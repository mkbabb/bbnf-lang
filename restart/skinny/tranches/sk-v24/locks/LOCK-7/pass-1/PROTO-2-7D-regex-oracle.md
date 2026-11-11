# PROTO-2 — the regex differential-oracle ledger row (LOCK-7, family 7D)

Probe 2 of LOCK-7 pass 1. Ran a real prototype against a pt-scratch copy of parse-that
(READ-ONLY origin, files verified byte-identical to HEAD `ef10d5b`). No source mutated — one
example added (`PROTO-2.patch`). Numbers are measured, cold, paired, load recorded.

---

## Question

FAMILY-SPECS §6 hinge 2 — the **promotion-vs-oracle-only** posture (INC-2: 7B
*promote-to-production* vs 7D *oracle-first*), discriminable **today** on the one edge with
committed producer code on both sides: the regex grammar. Precisely:

> Does the self-hosted generated regex parser (`bootstrap/regex.bbnf` → `RegexParserEnum` →
> `host::lower` → `Hir`) reach **parity + perf** against the hand `bbnf_regex` RD parser — so
> the 7B *promote* gate fires — or does it stay **ORACLE-ONLY** (7D's lawful terminal)?

Secondary, from 7D's open questions: is a **producer-level** equivalence (not merely the bool
recognizer FAMILY-SPECS §0 saw) even computable today, and how sensitive is parity to the choice
of ≡ (7D-Q1)?

## A correction to FAMILY-SPECS §0 (load-bearing)

§0 fact 4 states the generated regex parser "produces a `RegexParserEnum` AST internally but the
public `parse_generated -> bool` **discards it**", framing the edge as recognizer-only. That is
true of `generated.rs::parse_generated` — but it **misses** `regex/mod.rs:34`, which already
wraps the producer:

```
pub fn parse_generated(pattern) -> Result<Hir, ParseError>   // generated parser + host::lower
```

`host::lower` (`regex/host.rs`, 524 LOC, **byte-identical to HEAD**) folds `RegexParserEnum` into
the **same `Hir`** the hand parser yields (`Hir` derives `PartialEq, Eq`). So the strongest ≡ —
full **structural Hir equality** — is computable **now**, with no new functor. G-7D-2/G-7D-3's
"recognizer→producer" gap is *already closed* on this edge; the recognizer-only framing was a §0
miss. This is the finding that makes probe 2 runnable at all.

## Method

- **Copy, not mutate.** pt-scratch copy per R3 §8; `CARGO_TARGET_DIR` inside the copy; parse-that
  origin untouched. Regex-path files (`generated.rs`, `regex/mod.rs`, `regex/host.rs`,
  `bootstrap/{lib.rs,benches/regex_parse.rs}`) verified `diff`-identical to HEAD `ef10d5b`.
- **One example, zero source edits** — `rust/bootstrap/examples/oracle.rs` (patch attached).
- **≡ pinned at Hir structural equality**: `regex::parse_with` (HAND) vs
  `regex::parse_generated_with` (GEN), both with a **shared** `ParseOptions::default()`
  (`unicode:true`; identical on both sides — both public entry points default to it).
- **Corpus**: the 51-pattern **union** of the two committed `regex_parse` bench corpora
  (`bootstrap/benches` 29 + `regex/benches` 52, deduped) — every regex class across the BBNF
  grammars (JSON, CSS idents/numbers, ws/comments, char-classes, quantifiers, groups, escapes,
  URLs, dates, IPs, UUIDs, hex/oct/bin literals).
- **Perf**: cold **paired, interleaved**, release `-O3`, **3000 reps**, per-corpus-pass median,
  `black_box` against DCE, 50-iter icache prime (no result caching — every parse allocates a
  fresh `Hir`, so every pass is cold). loadavg captured around each run.

## What ran

`cargo run --release --example oracle -p regex-bootstrap` inside pt-scratch. Deps came from
prebuilt release rlibs (example link `0.35s`). Executed **3×** for stability. Toolchain
`nightly-2026-04-11` (repo pin). Machine: 18 cores, 1-min loadavg 10.9–13.9 during runs
(concurrent probes present — paired interleaving cancels shared contention in the ratio).

## The numbers

**Parity ledger** (deterministic across all 3 runs; 51 patterns; ≡ = Hir structural equality):

| metric | value |
|---|---|
| both accept + Hir-equal | 39 |
| both accept + Hir-**diverge** | 3 |
| both reject | 0 |
| membership disagree (HAND-OK / GEN-ERR) | 9 |
| **structural parity** | **39/51 = 0.7647** |
| **membership parity** | **42/51 = 0.8235** |

Divergence ledger — 12 rows, the oracle's checkable spec of what the generated grammar/host must
fix before promotion is even discussable:

- **9× HAND-OK / GEN-ERR** `invalid repetition bound` — the generated→Hir path **rejects every
  bounded quantifier** `{n}`, `{n,m}`, `{n,}` (a `host::lower` gap). Hit: `[0-9a-fA-F]{3,8}`,
  `a{3,5}`, the JSON string's `u[0-9a-fA-F]{4}`, `#[0-9a-fA-F]{3,6}`, `\d{4}-\d{2}-\d{2}`,
  `\d{2}:\d{2}:\d{2}`, IPv4 `\d{1,3}…`, the UUID pattern, `(?:[a-z]{2,4})+`.
- **2× STRUCT-DIVERGE — genuine semantic bugs**:
  - `.` → GEN lowers to a **literal dot** `Class([46])` instead of any-char-except-newline
    `Class([^\n])`. The `.` metachar is mishandled.
  - `(?s)(?:\s|/\*.*?\*/)*` → GEN treats the inline flag `(?s)` as a **literal group**
    `Group([s])` (no dotall applied; the inner `.` stays literal). Inline-flag semantics absent.
- **1× STRUCT-DIVERGE — cosmetic (≡-sensitivity)**: `""` → HAND `Empty`, GEN `Concat([])` —
  semantically equal, structurally distinct. This is 7D-Q1 made concrete: a strict structural ≡
  **over-counts**; ≡ must normalize `Empty ≡ Concat([])`. Under that normalization structural
  parity is **40/51**; substantive divergences drop to **11** (9 reject + 2 wrong-Hir).

**Perf** — cold paired, per 51-pattern corpus-pass:

| run | HAND µs/pass | GEN µs/pass | ratio GEN/HAND | loadavg(1m) |
|---|---|---|---|---|
| 1 | 17.105 | 30.917 | **1.808×** | 13.88 |
| 2 | 14.792 | 26.250 | **1.775×** | 10.89 |
| 3 | 14.583 | 25.875 | **1.774×** | 10.89 |

GEN is **1.77–1.81× slower** than the hand parser (ratio stable to ~2% across the load swing;
paired interleaving makes the ratio robust to absolute-timing noise).

## Verdict

**VERDICT: RAN (measured). On the sole regex edge decidable today, the generated self-hosted
parser meets NEITHER promotion gate — structural parity 0.76 (< 1.0) AND perf 1.77–1.81× slower
(< 1.0). The 7B promote-to-production posture does NOT fire; ORACLE-ONLY (7D's lawful terminal)
is the correct disposition for this edge now.** This is direct measured evidence resolving INC-2
toward **7D** on the one edge where the contest is real. The oracle earned its keep the instant it
ran: a 12-row checkable-spec ledger naming exactly what must be fixed — `host::lower` bounded-`{n,m}`
lowering, the `.` metachar, and inline-flag `(?s)` — before promotion is discussable. Corollary
for 7D-Q1: pin ≡ with `Empty`-normalization or the relation over-counts (the `""` row).

**Scope / honesty caveats.** (1) release `-O3`, not the `bench` fat-LTO profile — immaterial to a
*same-binary ratio* (both sides identical profile); fat-LTO would lift both absolutes, not the
1.8× gap. (2) Machine under concurrent-probe load; paired interleaving mitigates, ratio stable.
(3) Corpus is the committed 51; grammar-driven fuzzing (G-7D-4) would only *lower* parity by
exercising more surface (bounded repetition alone is pervasive). None of these move the verdict:
both gates fail by wide margins.
