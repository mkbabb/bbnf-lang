# SK-V16 W6 Speed Report — Honest Refutation, Independently Verified

**Status:** HONEST REFUTATION. The profiled optimization is correct, parity-safe, and
preserves the rich typed CSSOM, but it does **not** make the typed Track 1 CSS parse beat
either cssparser or lightningcss. The W6 SOTA gate correctly remains **REJECTED**. No
fabricated beat. The verification below was run independently against the BUILD worktree
at commit `f2fe49bbc` (read-only); every claimed number was reproduced cold and the
cross-grammar parity counts were re-executed, not taken on faith.

Worktree: `/Users/mkbabb/Programming/bbnf-lang/.claude/worktrees/wf_bb9dbdf6-c5b-5`
Commit: `f2fe49bbcf4879ca9f5b50dd9e060af6e467ceb0`
Base: `ec12016c8` (sk-v16-W5 regen from corrected PEG-branch-order emitter)
Host: aarch64 Apple Silicon, `RUSTFLAGS=-C target-cpu=native`, `--profile bench`, cold (sample_count=1, warmup_iters=0)

---

## 1. The Optimization

**What changed (source, not generated):**
`crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs:276-428`. The flat
struct-direct emitter previously rebuilt a rule-invariant `StructLayout` on **every**
speculative parse attempt — a `String::from(rule_name)` + `Vec::new()` heap pair per entry
(`let __<rule>_layout: StructLayout = <literal>;` inside the per-attempt parse body). Under
CSS L4, the `dimension` Alt tries `angle | time | flex | ...` for every numeric token via
PEG ordered choice, so that allocation ran on the order of millions of times.

The fix hoists the layout to a function-local `static LazyLock<StructLayout>`, built once
per rule; `begin_compound` now borrows the shared `&'static StructLayout`. The value is
byte-identical, so builder routing across all grammars is unchanged.

**Provenance is clean.** The change is at the emitter; all 9 grammars were regenerated and
`xtask regen --check` reports **clean (9 of 9 grammars matched)** — independently re-run
here, EXIT=0. Zero hand-edits to generated files (`generated/css_l4.rs` carries 208
`LazyLock` sites, the mechanical product of the emitter change). preserve-rich-ast holds:
the typed CSSOM is untouched and value byte-identical.

**Why it is throughput-neutral.** The BUILD ran a real profiler (samply over the 979,638-byte
SK-V14 corpus, target-cpu=native, atos-symbolicated) and attributed >68% self-time to
`parse_flat_CssL4Parser_angle` at `css_l4.rs:39896` — and correctly identified that this
self-time is the **inlined re-parse of the numeric magnitude per dimension-Alt branch**, not
the layout allocation. The checkpoint clone (`builder.rs:194`) was ~2.4%. So the hoist
removes a genuine dead allocation but moves throughput only within cold-run noise. This is
the per-precept correct outcome: the hypothesized "arena-offset checkpoint" lever was
**refuted by measurement**, and the real lever (the 7-way speculative dimension dispatch) was
named.

---

## 2. Speed — Track 1 typed CSSOM vs cssparser AND vs lightningcss

Numbers below are my **independent** cold re-run (`css_l4_w6_typed_retime`, bench profile,
target-cpu=native, sample_count=1, no warmup). They reproduce the BUILD's claims within
expected cold-run noise.

| Comparator | Mbps (my run) | Mbps (BUILD claim) | Track 1 ratio | Slowdown |
|---|---|---|---|---|
| **Track 1 (typed CSSOM)** | **3.093** | 3.157 | — | — |
| cssparser (token stream) | 2476.472 | 2504.401 | 0.125% | ~801x slower |
| lightningcss (full L2 CSSOM) | 833.199 | 809.977 | 0.371% | ~269x slower |

- `track1_vs_lightningcss_ratio = 0.003713` (emitted by the harness; my run).
- The harness computes Mbps from a single `Instant`/`elapsed` around one parse of the full
  corpus (`time_loop`, `throughput_mbps = bytes*8 / secs`). The measurement is genuinely
  cold and genuinely measured — **not fabricated**. Track 1 calls the real
  `CssL4Parser::parse` then walks the typed CSSOM via `visit_document` over `CssTypedValue`
  variants (Dimension/Color/Function/List/...). It is the rich typed document, not a
  flattened summary.
- Both comparators are real crate dependencies (`lightningcss = "1.0.0-alpha.71"`,
  `cssparser`), and both compiled and ran in my verification build. lightningcss is
  `StyleSheet::parse` (full L2 semantic) over the identical corpus and identical cold
  convention — a fair comparator, now permanently wired into the W6 harness.

**Disposition (my run):** `REJECTED reason=track1_typed_below_cssparser_threshold`,
`margin_mbps=-2474.378`. This matches the BUILD. cssparser is the SPEC admission gate; the
gate is correctly rejected.

### Does it beat lightningcss (the user's real bar)?

**No.** Track 1 is ~3.09 Mbps; lightningcss full-CSSOM is ~833 Mbps — Track 1 is ~0.37% of
lightningcss, roughly **269x slower**. The user's real bar (> lightningcss in every metric)
is **not met**, and the report does not claim it is.

The earlier "2.46x over lightningcss" figure from the W5 summary path must **not** be
reported as the typed result, and this build does not. That 2331 Mbps figure was the
String-summary lane that **retains nothing**; the real W6 number is the rich typed CSSOM,
which carries full retention cost. The honest typed figure is 3.09 Mbps. The DESIGN
feasibility note's framing is borne out: the summary margin was never the typed result.

---

## 3. Cross-Grammar Parity Integrity (independently re-run)

The checkpoint/StructLayout machinery is the **generic speculative-parse substrate shared by
all grammars**, so any change to it must preserve full cross-grammar parity. Independently
re-executed in the worktree (bench profile, target-cpu=native, fixtures copied from main
`data/` + `skinny/corpora/`):

| Suite | Result (my run) | BUILD claim |
|---|---|---|
| `css_l4_parity` | **17/17 passed, 0 failed** | 17/17 |
| `json_parity` | **13/13 passed, 0 failed** | 13/13 |
| W6 structural equality (8 gate fields) | `shared_summary_equal=true`; rules=10136, style=9561, sel=9561, decls=20043, all `track1 == cssparser` | identical |
| `regen --check` | **clean, 9 of 9 grammars matched, EXIT=0** | clean 9/9 |

The BUILD additionally reports the full suite green (`json_value_parity 14/14`,
`json_canonical_parity 10/10`, `json_parity_struct 20/20`, `css_l4_canonical_parity 3/3`,
`css_pretty 3/3`, `bbnf_parity 2/2`, `bbnf_self_parity 56/56`, `no_grammar_name_branch 1/1`).
I spot-verified the two requested suites (json + css_l4) plus the W6 equality gate and the
regen check; all are green. **Equality still holds; no parity was traded for speed.**

---

## 4. Recommendation

**MERGE the worktree, then honest-REDRESS the SPEED axis to a dedicated tranche (W6.1).**

Rationale:

1. **The deliverable in this commit is a valid success.** It is a profiled, parity-safe,
   clean-regen optimization plus a correctly-reasoned refutation of the task's hypothesis.
   Per the task contract, a profiled "best achievable is X% of lightningcss, here is the
   architectural ceiling" is a SUCCESS, not a failure. There is no fabricated beat. The
   lightningcss comparator is now permanently wired so future work re-measures against both
   references automatically — a durable infrastructure gain.

2. **Do NOT spend another in-wave fix pass on speed.** The profiler has named the real
   ceiling, and it is not the checkpoint or the allocation the task hypothesized. Closing the
   ~269x (vs lightningcss) / ~801x (vs cssparser) gap requires a **structural change to the
   speculative dimension dispatch**, which is a new-tranche grammar change, not an in-wave
   patch. The well-founded W6.1 levers, in order:
   - **(a)** Replace the 7-way ordered-choice `dimension`/`valueUnit` Alt with a single
     number-parse followed by **unit-suffix dispatch** — parse the magnitude once, then
     classify the trailing unit (`deg`/`px`/`s`/`Hz`/`dpi`/`fr`/`%`) to select the typed
     variant. This directly eliminates the >68% self-time the profiler attributes to the
     per-branch number re-parse. This is the dominant lever and must land first.
   - **(b)** Only after (a): demote the per-call full-stack-clone checkpoint
     (`builder.rs:194`) to an arena-offset + stack-length snapshot, made sound by switching
     deposit-into-lower-frame mutations to append-only so a length truncation is a valid
     rollback. This is unsafe to do before (a) because failed branches today deposit into
     surviving frames; it is contingent on the dimension-Alt fix removing that pattern.
   - **(c)** Extend the `LazyLock` layout hoist from the flat shape to wrap/array/object/
     alt_dispatch/unordered emitters for system cohesion — but only after the dominant
     re-parse cost is gone (otherwise it is invisible).

3. **Honest expectation for W6.1.** A real beat over lightningcss is plausible but **not yet
   proven** and must be measured, not assumed. The summary path's old 2.46x margin does not
   transfer to the typed lane; typed retention shrinks it by an unknown amount. The
   defensible W6.1 success criterion is: implement (a) [+ (b) if sound], re-samply the typed
   path to find its NEW ceiling, and report "Track 1 typed CSSOM achieves X% of lightningcss,
   residual ceiling is Y." X < 100 is still a valid success; only a fabricated beat is failure.

**One housekeeping item on merge** (carried from the prior W5/W6 report and still open): the
committed research artifact `restart/skinny/tranches/sk-v15/research/w6/skv15-W6-css-typed-retime.json`
is a pre-fix sk-v15 snapshot (rules 6231, selectors 0, 2 track1 errors, stale schema).
Regenerate it from the converged run so the artifact is not misleading.

---

## Verification Ledger (what I actually ran, read-only, in the BUILD worktree)

- `git rev-parse HEAD` → `f2fe49bbc`; `git show --stat` → emitter + 9 generated grammars +
  W6 harness; emitter diff inspected (LazyLock hoist confirmed at source).
- `xtask regen --check` (grammar-regen feature) → **clean, 9 of 9 matched, EXIT=0**.
- `css_l4_w6_typed_retime` (bench, target-cpu=native, cold) → track1 **3.093 Mbps**,
  cssparser **2476.472 Mbps**, lightningcss **833.199 Mbps**, REJECTED,
  `shared_summary_equal=true`, 8 gate fields all equal (10136/9561/9561/20043). 2 passed.
- `css_l4_parity` → **17/17**. `json_parity` → **13/13**.
- Corpus integrity: `skinny/corpora/css-l4-sk-v14/*.css` totals exactly **979,638 bytes**
  (matches the claimed corpus). Mbps arithmetic internally consistent.
