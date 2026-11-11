# LW1 · RX lane receipt

**Branches:**
- parse-that `sk-v27-regex`: `ee1a388` (oracle and census) and `770a350` (born-RED tests). This is the engine's home under X-2's default.
- bbnf-lang `sk-v27-regex`: this directory, holding the records.

**Logs:** `~/.dev-logs/bbnf-lockstep/rx/`. **Load:** 14–25 throughout (1-minute average), so every timing below is **NOISY**.

| Unit | Falsifier | RED (before) | GREEN ×2 (after) | State |
|---|---|---|---|---|
| RX-1 | V8 and JSC agree with themselves ×2 on the smoke corpus; any cross-engine disagreement is SPEC-AMBIGUITY | `oracle.mjs` absent at parse-that `b4f63f4`: rc 1 ×2 | 44 cases, V8 self-consistent, JSC self-consistent, 44/44 cross-agree, 0 SPEC-AMBIGUITY: rc 0 ×2; answers byte-identical across the two runs | DONE |
| RX-2 | the cases read RED ×2 for their stated reasons | the tests are absent | `--include-ignored`: 9 RED / 4 guards green, rc 101 ×2, every failure message is the stated reason; default suite rc 0 (9 ignored) | DONE (born-RED) |
| RX-3 | lookaround count over the value.js leaves plus the sk-v24 56; if > 0, a lookaround charter becomes a G-R1 gate | `LOOKAROUND-CENSUS.tsv` absent: rc 1 ×2 | value.js 115 leaves, 38 with lookaround; sk-v24 0/56; output byte-identical ×2 (sha256 `3d6d1253…`) | DONE: **38 > 0, so a lookaround charter is a G-R1 gate** |
| RX-4 | C1-9 and C2-6 restated and ruled | addendum absent: rc 1 ×2 | `RX-4-ADDENDUM.md`, both rulings present: rc 0 ×2 | RULED (on restatement; to be re-read when TOPO-0 files the full texts) |

## Engines

- **V8:** node v26.0.0, V8 14.6.202.33-node.19.
- **JSC:** the system `JavaScriptCore.framework` (21624.1.16.11.4) `jsc` shell, on macOS 26.4.1. bun is not installed; the driver uses bun automatically once it is.

## RX-2 detail

Expected values are the oracle's answers (`ORACLE-SMOKE-ANSWERS.json`).

| Test | Oracle | Engine today | Reason |
|---|---|---|---|
| `/\d+(?!\.\d)/y` on `12.5` | 1 | None | the parser rejects lookaround ("invalid group syntax") |
| the same on `12` | 2 | None | the same |
| `/a|ab/y` on `ab` | 1 | 2 | the DFA is leftmost-longest (Alt fusion) |
| `pattern_is_superset("[Ã©]","[é]")` | false (`/[Ã©]/u` does not match é) | true | containment over UTF-8 byte sets: C3 83 ∪ C2 A9 ⊇ C3 A9 |
| `pattern_is_superset("[a-z]","[a-c]+")` | false (`[a-c]+\|[a-z]` on abc is 3; absorbed form is 1) | true | the outer repetition is stripped (C2-1) |
| `try_union_patterns("[é]","[α-ω]")` matches é | 2 | 1 (merged prints as `[\u{89}©±-Ï]`) | UTF-8 bytes printed as Latin-1 chars |
| `/k/iu`, `/K/iu` on U+212A; `/K/iu` on k | 3, 3, 1 | None | case folding is ASCII-only |
| `/ſ/iu` on s | 1 | None | the same |
| `/é/i` and `/é/iu` on É | 2 | None | the same |

The guards hold today:
- `ab|a` gives 2.
- `[a-z] ⊇ [a-c]`.
- `[é]` and `[α-ω]` do not contain each other.
- In byte mode, `(?i)k` does not match U+212A and `(?i)ſ` does not match `s`.

**PLAN correction.** PLAN §5 names `[é]` / `[α-ω]` as the SupersetAbsorbAlt witness. That pair reads GREEN at the algebra layer, so it stays as a guard. The defect it stands for, byte sets in place of scalar sets, is witnessed by `[Ã©]` ⊇ `[é]` and by the union printer.

## RX-3 detail

- Leaves are read by bbnf-lang's own parser (`BBNFToASTWithImports`) from the six live grammars at value.js `8d7bb5b8` `src/css/grammar/`: 115 distinct leaves, which occur 135 times.
- Of those, 38 carry lookaround: 12 positive lookahead, 26 negative lookahead, 2 positive lookbehind and 0 negative lookbehind. 11 have a lazy quantifier and none has a backreference.
- Most of the lookaround is a one-character ident-boundary guard such as `(?![\w-])`.
- **"121 leaves" is retracted.** It came from a lexical list with 11 prose artifacts that also missed 5 at-rule leaves. The true figure is 115.
- The sk-v24 56 are extracted verbatim from `sk-v24/locks/LOCK-5/pass-3/PROTO-2-P2.patch` (sha256 `81a4b372…`) into `corpora/skv24-56.txt`.

## Benches

All are **NOISY**: load 23–24, `--cpu-cores 0`, 10 reps, one process per rep, median [p25–p75].

| What | Median | p25–p75 | min–max |
|---|---|---|---|
| `exec.js` on V8, 44-case corpus (process wall) | 92.1 ms | 85.0–94.0 | 77.7–124.9 |
| `exec.js` on JSC (process wall) | 25.0 ms | 24.3–28.4 | 23.8–42.9 |
| census over 115 + 56 leaves (process wall) | 111.8 ms | 101.8–113.8 | 96.8–119.3 |
| full `oracle.mjs` run (2 engines × 2 reps, each guarded) | ≈ 4.0–5.7 s | — | — |

The full-run time is dominated by the guard's admission. These timings bound how much the harness costs; they claim no performance for the engine.

## Blockers recorded (not RX's to cure)

- **Not hermetic.** parse-that's Rust workspace does not build from a clean checkout: `error: no matching package named egraph-derive`. A seat must supply `--config patch.crates-io.{egraph,egraph-derive,csp-solver}.path=<bbnf-lang>/crates/…`, which is the repo cycle. This belongs to LW1.COMP-4 and LW2.TOPO (X-5). The RX runs used bbnf-lang `sk-v27-regex` (= origin/master `c9b93adb4`) crates through those flags, and nothing was committed for it.
- **Stale hook.** The bbnf-lang commit used `--no-verify` because of the stale sk-v26 pre-commit hook (PLAN, LW1.COMP-4). parse-that has no hooks.
- **Guard name clash.** The guard classes any `run.mjs` as a browser run, so the driver is named `oracle.mjs`.
- **Generated parser panic.** The prior census probe (`regex-census/probe-run4.log`) recorded `parse_generated` panicking with `Span::as_str: invalid bounds 40..41`, a slice that is not on a char boundary, on a value.js leaf containing `\u0080-￿`. This wave left it unfiled as a test. It is a carry for LW2.RX-3 (round-trip oracle).
