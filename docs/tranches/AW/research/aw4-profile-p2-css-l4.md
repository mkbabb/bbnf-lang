# AW-IV P2 — CSS L4 samply profile (post-W1.4-aggressive)

Binary `css_l4-c82f8bbe86d63cb0`, HEAD `2ca0f7af`. Artefacts at
`.profiles/samply/css_l4/<entry>/` (all seven files). Attribution:
innermost-frame counts resolved against syms ranges.

## 1. Per-entry top-20 self-time

Top-20 columns: normalize (N, 3555 samples) / bootstrap (B, 7900) /
tailwind (T, 93 754). % self-time.

| symbol | N | B | T |
|---|---|---|---|
| `try_branch` (dispatch_one inlined) | 70.9 | 76.1 | 78.9 |
| `<CssL4Parser>::parse` (stage-1 scan) | 9.2 | 4.5 | 4.9 |
| `__regex_scan_CssL4Parser` | 5.5 | 5.8 | 4.8 |
| `advance_or_pop_with.3011` | 5.0 | 5.1 | 4.5 |
| `css_l4::main` + entry harness | 4.1 | 4.0 | 3.3 |
| `finaliser::finalise` | 2.8 | 2.4 | 2.0 |
| `<FrameStack>::nearest_variant_frame` | 0.6 | 0.7 | 0.5 |
| `0x237de8` (unresolved PLT gap) | 0.6 | 0.4 | 0.3 |
| `css_l4::load` | 0.4 | 0.4 | 0.5 |
| `__dta_walker_inline::run` | 0.2 | 0.1 | 0.1 |
| `__cold_state_2409` (ruleItem) | 0.1 | 0.1 | 0.0 |
| `__cold_state_844` | 0.1 | 0.0 | 0.0 |
| `<f64>::from_str` | 0.1 | 0.0 | 0.0 |
| `<Vec<u8>>::resize` | — | 0.1 | 0.0 |
| `advance_or_pop_with` (hot variant) | 0.1 | 0.0 | 0.0 |
| `core::str::from_utf8` | 0.0 | 0.0 | 0.1 |
| `__cold_state_2413` | — | 0.0 | 0.0 |
| `bbnf_tape::psi::write_decoded` | — | 0.0 | 0.0 |
| alloc tail (mi_*, raw_vec) | 0.2 | 0.1 | 0.1 |
| misc unresolved | 0.1 | 0.2 | 0.2 |

## 2. Bench numbers

| entry | ns/iter | MB/s | bytes |
|---|---|---|---|
| normalize | 246 312 (±5 669) | 24 | 6 138 |
| bootstrap | 19 051 995 (±480 607) | 14 | 280 311 |
| tailwind | 227 968 733 (±7 095 708) | 15 | 3 642 321 |

W1.4-aggressive delta (16→25, 9→15, 10→16) reproduces. +50-67%.

## 3. Cycles/byte at 3.5 GHz

| entry | ns/byte | cyc/byte |
|---|---|---|
| normalize | 40.1 | 140.5 |
| bootstrap | 68.0 | 237.9 |
| tailwind | 62.6 | 219.0 |

JSON canada at post-AU ~4.8 cyc/byte. CSS L4 is 29-50× slower.

## 4. Category aggregation

| category | normalize | bootstrap | tailwind |
|---|---|---|---|
| `try_branch` (dispatch_one inlined) | 70.9% | 76.1% | 78.9% |
| regex scan | 5.5 | 5.8 | 4.8 |
| tape advance (`.3011` cold) | 5.1 | 5.1 | 4.5 |
| parse entry + stage-1 SIMD | 9.2 | 4.5 | 4.9 |
| finaliser | 2.8 | 2.4 | 2.0 |
| bench harness | 4.5 | 4.4 | 3.8 |
| walker outer + cold arms | 0.4 | 0.2 | 0.2 |
| everything else | 1.6 | 1.5 | 0.9 |

**100% of `try_branch` samples have `__dta_walker_inline::run` as
ancestor** — the helper IS the walker's dispatcher for CSS.

## 5. Structural-alphabet fix verification

`expand.rs:324` — `__GRAMMAR_PROFILE_ALPHABET: [u8; 53usize] = [32,33,35,
…,125,126]`. 53 singletons, not 128. Post-W1.γ fix firing (ASCII
punctuation + `a-z` + `{|}~`; digits and most uppercase excluded).

`bbnf_simd_scan::neon::scan` has **no standalone symbol** — inlined
into `<CssL4Parser>::parse`. The 4.9-9.2% parse self-time is stage-1
scan + parse-entry harness. Reasonable for 53-singleton NEON.

Walker per-byte cost is **not** dominated by the inlined DFA body; it
is dominated by the `try_branch` helper at 76-79%.

## 6. `__compoundSelector` arm — is it still hot?

State 2098 = rule 110 = `compoundSelector` (`expand.rs:363847`).
Emitted as `__cold_state_2098` (`#[cold] #[inline(never)]`) but
**never visible on the sample stack** — zero samples in any of the
three profiles. Reason: compoundSelector's 5-way AltLinear is entered
via `try_branch(2098)` from inside an outer `try_branch` loop, and
dispatch_one (inlined into try_branch) handles state 2098's body
directly via its runtime `match table.states[state_idx]`; the emitted
`__cold_state_2098` is dead code because no outer-walker `match cur`
arm reaches 2098.

So compoundSelector's arm cost is **folded into `try_branch`'s 78.9%
bucket** and is not separately profile-observable. It cannot have
dropped below the pre-AU 33-43%; it is just now in the aggregate.

State 2409 = rule 147 = `ruleItem` (2-way AltLinear) is the one
cold-state that IS profile-visible: 92.5% **inclusive** in every run
— every top-level CSS rule enters via the outer walker's `match cur`
switch, and every nested try_branch is under it.

## 7. Per-byte cost breakdown

`try_branch` = 13 388 bytes of machine code (`syms.json` rva=191124
size=13388). No `dispatch_one` symbol — fully inlined into
try_branch. The 13 KB is the runtime `match table.states[state_idx]`
dispatch plus the retry loop. Every AltLinear branch probe in CSS L4
enters this match.

| source of per-byte cost | fraction | evidence |
|---|---|---|
| Walker outer `match cur` | 0.1-0.2% | `walker_inline::run` self-time |
| Inlined DFA bodies (WS DFA + state bodies) | subset of above | shares walker self-time |
| `try_branch`/inlined `dispatch_one` (AltLinear bridge) | 70.9-78.9% | `try_branch` self-time |
| `advance_or_pop_with.3011` cold-path advance | 4.5-5.1% | standalone |
| Stage-1 SIMD scan (inlined in parse) | ≤9% | `<CssL4Parser>::parse` self-time (parse entry incl. scan) |
| `__regex_scan_CssL4Parser` | 4.8-5.8% | standalone symbol |
| `finaliser::finalise` | 2.0-2.8% | standalone |
| Allocator + misc | < 2% | tail |

**The load-bearing finding:** W1.4-aggressive's 26 inlined DFA
bodies contribute 0.1-0.2% of CSS L4's runtime. The inlining IS
present (`walker_inline::run` is 157 556 bytes — 154 KB confirming the
bodies) but is virtually unexecuted because CSS L4's hot path never
reaches the walker's outer `match cur`. It is stuck inside try_branch's
runtime match — the cold path.

## 8. i-cache pressure

| region | size |
|---|---|
| `__dta_walker_inline::run` | 157 556 B (154 KB) |
| `try_branch` (with dispatch_one) | 13 388 B (13 KB) |
| `__regex_scan_CssL4Parser` | 5 768 B |
| `advance_or_pop_with.3011` | 3 232 B |
| hot-path working set | ~170 KB |

Apple M P-core L1 I-cache = 192 KB (Firestorm). ~170 KB fits with
11% margin. No `__cold_state_*` symbol shows disproportionate
self-time vs. its op count — the hot path is not in the walker's
per-state bodies. The 154 KB walker is essentially unused for CSS L4
regardless of cache footprint.

## 9. Conclusion per entry

- **normalize:** bottlenecked by `try_branch` dispatch bridge at 70.9% (~100 of 140 cyc/byte); recoverable by emitting AltLinear arm bodies inline at the walker level.
- **bootstrap:** same bridge at 76.1%; same remedy. The 5-point gap vs. normalize reflects higher AltLinear depth per byte (more nested try_branch recursion per rule).
- **tailwind:** same bridge at 78.9% (~172 of 219 cyc/byte); same remedy; the further 3 points above bootstrap is try_branch's share saturating over 3.6 MB amortised.

## 10. Is CSS L4 viable at all, or does it need a separate route?

**Viable under the current architecture, one change away from closure. No separate route needed.**

Evidence:

1. **The walker is not the hot path.** 154 KB of inlined DFA bodies
   run for 0.1-0.2% of cycles. W1.4-aggressive's +50-67% gain came
   from the small subset of linear-chain rule sequences that DO reach
   the outer `match cur` (mostly the inlined WS DFA prefix at
   `expand.rs:478491`) plus register-pressure relief. It did not
   touch the AltLinear bottleneck.

2. **`try_branch` is the real dispatcher.** 70.9-78.9% self-time;
   dispatch_one fully inlined (13 KB combined). This single function
   is the entire dispatch cost. It is not grammar-specialised — does
   a runtime `match table.states[state_idx]` against a 2400+-state
   table. It is identical to the cold-path replay surface.

3. **Why CSS lives here when JSON doesn't.** JSON's top-level is one
   4-way AltLinear; every token fires it once. CSS's `ruleItem`
   wraps every rule AND contains nested AltLinears at
   `compoundSelector` (5-way), `colonSelector` (2-way), at-rule
   prelude, etc. Every CSS byte traverses multiple AltLinear layers;
   every traversal is a `try_branch` invocation = a runtime match,
   not an inlined switch.

4. **Recoverable lever.** Emit the AltLinear branch-probe as
   walker-inlined code. At each AltLinear state the codegen knows
   (a) the branch list (const `__DTA_ALT_LIN_<N>`), (b) the savepoint
   shape, (c) the return semantic. Unrolling the `for &branch in
   branches` loop and dispatching each branch into the walker's own
   per-state `match cur` — not `try_branch` — turns the 78.9% helper
   into 5-10% of inlined case dispatch, unlocking the 154 KB walker
   for actual execution. This is a W2.x/W3.x-scale change (~100
   `match cur` AltLinear arms emitted alongside the 2760 existing
   state arms), not a new tranche.

5. **The architecture is correct.** One codegen path, grammar-
   specialised walker. The implementation has one piece unshipped:
   AltLinear walker-inlining. Routing CSS to a separate parser is
   architectural surrender; the post-AU 735 MB/s on JSON proves the
   architecture can reach that throughput class — the CSS debt is
   the one unfired lever.

The samply evidence is unambiguous: the walker's inlined DFA bodies
are not the bottleneck. `try_branch` is.

## Artefact citations

- bench — `.profiles/samply/css_l4/<entry>/bench.txt`
- self-time — `.profiles/samply/css_l4/<entry>/profile.json.gz` + `profile.json.syms.json`
- walker size — `profile.json.syms.json` rva=24816 size=157556
- try_branch size — rva=191124 size=13388
- structural alphabet — `.profiles/samply/prebuild/expand/css_l4/expand.rs:324`
- compoundSelector state mapping — `expand.rs:363847` (2098↔110); grammar `grammar/css/l4/selectors.bbnf:87`
- ruleItem state mapping — `expand.rs:402029` (2409↔147)
- binary — `.profiles/shared-target/release/deps/css_l4-c82f8bbe86d63cb0`
