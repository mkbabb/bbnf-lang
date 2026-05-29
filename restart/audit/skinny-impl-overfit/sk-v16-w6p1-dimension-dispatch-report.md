# SK-V16 W6.1 Lever-1 Status — Dimension Suffix-Class Dispatch

**Status: HONEST REFUTATION (lever-1), independently verified.** The change is correct,
parity-preserving, regen-clean, and preserves the rich typed CSSOM byte-identically. It does
**not** meaningfully improve cold throughput, and it does **not** approach lightningcss or
cssparser. The W6 SOTA gate correctly remains **REJECTED**. No fabricated beat. Lever-1
empirically refutes the W6 report's hypothesis that the dimension speculation was the dominant
throughput lever.

- Worktree (read-only): `/Users/mkbabb/Programming/bbnf-lang/.claude/worktrees/wf_cbf0ca6c-833-2`
- Commit: `85b4edd88e736fff51b9b90ed52338f99af53738`
- Base / current master: `f4b6f757c` (W6: flat StructLayout LazyLock hoist + lightningcss W6 comparator wired); descends `ec12016c8` (W5 grammar-derived CSS structural equality)
- Host: aarch64 Apple M5 Max, `RUSTFLAGS=-C target-cpu=native`, `--profile bench`, cold (W6_SAMPLE_COUNT=1, warmup_iters=0)
- Verification convention matches the predecessor W6 speed report exactly (bench, native, single cold pass).

---

## 1. The Change

W6.1 lever-1 is precisely the W6 report's prescribed **lever (a)**: replace the ordered-choice
dimension Alt with a single magnitude parse followed by unit-suffix dispatch.

**Grammar (source, not generated):**
`grammar/css/l4/value-unit.bbnf:77-79`, `grammar/css/l4/values.bbnf:29`. Before, the value-unit
plane was an 8-way Alt of independent `X = number , Xunit` rules
(`length|angle|time|frequency|resolution|flex|percentage|unitless`); because
`factor_common_prefixes` (`crates/ir/src/passes/prefix.rs`) cannot see through a `Ref` boundary,
every dimension re-scanned `number` under a full-stack `builder.checkpoint()`/`rollback()` per
speculated branch. After:

```
unitSuffix = lengthUnit | angleUnit | timeUnit | frequencyUnit | resolutionUnit | flexUnit | percentageUnit ;
valueUnit  = number , unitSuffix ? ;
```

The magnitude is parsed once; the trailing unit resolves through the existing keyword
byte-dispatch trie. An absent suffix is `Unitless`. The dispatch is expressed **in the grammar**
(no CSS-special-cased hand-coded branch).

**Kind recovery (generic infra, no grammar-name leak — Lock 14):** because one rule can no
longer carry `NumericKind` by identity, the kind is recovered from the matched suffix class. A
new generic `StructBuilder::set_alt_branch_class(class: u32)` — default no-op
(`crates/core/src/runtime/builder.rs:141`) — is emitted by the inline-Alt struct-direct emitter
(`crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs:848`), reporting the matched
branch's target **rule id** (order-independent) to the open compound. Only the CSS L4 `Numeric`
frame consumes it, mapping unit-class-rule-id -> `NumericKind` via a `suffix_kinds` projection
table (`xtask/runtime-projections/css_l4.toml:542`; decode in `xtask/src/regen_css.rs`
`end_compound`, `kind_class: Option<u32>`). The per-unit `-> Nu8` discriminant projections are
kept verbatim, so the `(kind, unit)` pair fed to `end_compound` — and the emitted
`CssTypedValue::Dimension` — is byte-identical.

**Unreachable cold rules eliminated.** I verified in the regenerated parser
(`crates/core/src/grammar/generated/css_l4.rs`) that the per-kind named rules
`length/angle/time/frequency/resolution/flex` are gone (0 `parse_flat_CssL4Parser_*` fns each),
reached only from grammar modules outside the stylesheet import graph
(filters/transforms/gradients/easing). `percentage` survives (1 fn — `colorPercentage` in
`color.bbnf`) on its rule-identity numeric route. `valueUnit` survives (1 fn); `unitSuffix`,
`lengthUnit`, `angleUnit` are inlined into it (0 standalone fns; the inlined Alt carries 7
`set_alt_branch_class` sites, one per suffix class).

**Provenance clean.** The generic emitter change adds inert `set_alt_branch_class` calls to all
grammars (bbnf/bnf/csv/ebnf/google_sheets/css_pretty — pure insertions, default no-op). All 9
grammars were regenerated; `cargo xtask regen --check` is **clean, 9 of 9, EXIT=0**
(independently re-run here). Zero hand-edits to generated files.

---

## 2. Speed — Track 1 typed CSSOM vs cssparser AND vs lightningcss

Numbers below are my **independent** cold re-run (`css_l4_w6_typed_retime`, bench profile,
target-cpu=native, sample_count=1, warmup_iters=0) over the 979,638-byte corpus. They reproduce
the BUILD's claims within cold-run noise.

| Comparator | Mbps (my run) | Mbps (BUILD claim) | Track 1 ratio | Slowdown |
|---|---|---|---|---|
| **Track 1 (typed CSSOM)** | **3.178** | 3.153 | — | — |
| cssparser (token stream) | 2498.579 | 2418.673 | 0.127% | ~786x slower |
| lightningcss (full L2 CSSOM) | 832.285 | 818.443 | 0.382% | ~262x slower |

- Harness emits `track1_vs_lightningcss_ratio = 0.003819` (my run).
- **Speedup vs the ~3.1 Mbps W6 baseline: negligible, ~1.02x.** The predecessor W6 report
  measured track1 at 3.093 (independent) / 3.157 (claim); this lever lands at 3.178 — within
  cold-run noise. The 8-way number re-scan and per-branch full-stack speculation on the hot
  `valueUnit` path are genuinely removed, but cold W6 throughput is statistically unchanged.
- The measurement is genuinely cold and genuinely measured (`time_loop` wraps one
  `Instant`/`elapsed` around one parse of the full corpus; `black_box` guards;
  `crates/core/tests/css_l4_w6_typed_retime.rs:362,899-905`). Track 1 calls the real
  `CssL4Parser::parse` and walks the typed CSSOM — the rich typed document, not a flattened
  summary. Not fabricated.

**Does it beat lightningcss (the user's bar)? No.** Track 1 is ~3.18 Mbps; lightningcss
full-CSSOM is ~832 Mbps — Track 1 is ~0.38% of lightningcss, ~262x slower. Lever-1 alone does not
approach, let alone beat, either reference. The W6 SOTA gate (cssparser + epsilon) is correctly
`REJECTED reason=track1_typed_below_cssparser_threshold, margin_mbps=-2496.400`.

**The honest finding.** The W6 profiler attributed >68% self-time to the per-branch number
re-parse and named lever (a) "the dominant lever [that] must land first." Lever-1 implements
exactly that and removes exactly that cost — yet throughput did not move. **This refutes the
hypothesis that dimension speculation was the throughput floor.** The cost lives elsewhere: the
typed-AST materialization itself — per-leaf deposits, arena/builder indirection through the
`StructRegistry` — as documented in `docs/benchmarks/post-AZ-IV.json` (28-118x retention
regression). The dimension path was not the bottleneck.

---

## 3. Cross-Grammar Parity Integrity (independently re-run)

The `set_alt_branch_class` / checkpoint / `StructBuilder` machinery is the **generic
speculative-parse substrate shared by all grammars**, so the emitter change must preserve full
cross-grammar parity. Independently re-executed in the worktree (bench profile, native, with
`data/json` + `data/css` fixtures present — 6 json, 4 css, matching main repo exactly):

| Suite | Result (my run) | BUILD claim |
|---|---|---|
| `css_l4_parity` | **17/17 passed, 0 failed** | 17/17 |
| `json_parity` | **13/13 passed, 0 failed** | 13/13 |
| `json_value_parity` | **14/14** | 14/14 |
| `json_parity_struct` | **20/20** | 20/20 |
| `css_l4_canonical_parity` | **3/3** | 3/3 |
| `css_pretty` | **3/3** | 3/3 |
| `bbnf_parity` | **2/2** | 2/2 |
| W6 structural equality (8 gate fields) | `shared_summary_equal=true`; rules=10136, style=9561, sel=9561, decls=20043, errors=0, all `track1 == cssparser` | identical |
| `regen --check` | **clean, 9 of 9, EXIT=0** | clean 9/9 |

The BUILD's note that the first parity run showed failures is borne out as environmental: those
were missing-fixture errors (`data/json/*.json: No such file or directory`), not regressions. With
fixtures present, every suite is green.

**preserve-rich-ast holds exactly.** The W5 8-field structural equality with cssparser is byte-
identical (rules=10136, style_rules=9561, selectors=9561, declarations=20043,
shared_summary_equal=true, errors=0). The typed `CssDimension` value is unchanged. **No parity
was traded for speed.**

---

## 4. Build's Redress Claim — Verified, with One Minor Cite Correction

The BUILD's redress (lever-2 / arena-offset checkpoint NOT unblocked by lever-1) is **correct and,
if anything, conservatively stated**. I inspected the regenerated
`parse_flat_CssL4Parser_valueUnit`:

- Function location: `crates/core/src/grammar/generated/css_l4.rs:44363` (the BUILD cited 44231 —
  a stale-regen line offset; the function exists and the substantive claim holds).
- Over the full function body (lines 44363-45147), I count **9 `checkpoint()` and 10 `rollback(`
  sites** (the BUILD's "5 / 4" was scoped to a sub-region). Either way, the qualitative claim is
  exact: the inner `unitSuffix` Alt **still speculates with per-branch checkpoint/rollback** (now
  over a few suffix bytes, no longer re-scanning `number`), so the builder can still roll back
  deposits. **Deposits are NOT append-only-safe.**
- Lever-2 (arena-offset + stack-length-snapshot checkpoint) requires eliminating the inner-Alt
  rollback. The unit-class first-byte sets overlap (`d` => `deg|dpi|dvw`, `r` => `rad|rem`,
  `m` => `ms|mm`, `s` => `s|svw`), so a naive byte-disjoint single dispatch with no fallback try
  is not yet possible; the inner rollback persists.

So lever-2 is genuinely blocked, and the W6 prediction that "(b) is contingent on (a) removing the
deposit-into-surviving-frames pattern" is itself refuted: (a) landed and did **not** remove that
pattern.

---

## 5. Recommendation

**MERGE the worktree, then honest-REDRESS — do NOT proceed to lever-2 (arena-offset checkpoint)
as scoped.**

Rationale:

1. **Lever-1 is a valid success deliverable.** It is a clean, parity-safe, regen-clean,
   grammar-expressed (no Lock-14 leak) left-factoring that removes a genuine dead cost (the 8-way
   number re-scan + per-branch full-stack speculation) and ships a correctly-reasoned refutation.
   Per the task contract, a profiled "best achievable is X% of lightningcss, here is the
   architectural ceiling, no fabricated beat" is a SUCCESS. Merge it: the dead speculation is gone
   and the dispatch is cleaner, even though throughput is flat.

2. **Lever-2 (arena-offset checkpoint) is NOT unblocked and must NOT be attempted as scoped.**
   Lever-1 was supposed to make deposits append-only-safe by removing inter-dimension speculation;
   it did not — the inner `unitSuffix` Alt still rolls back. Pursuing lever-2 against the current
   generated code would be unsound.

3. **Redirect W6 perf effort at the actual bottleneck, in a new tranche.** Lever-1 proved by
   measurement that the dimension path is not the throughput floor. The ~262x gap to lightningcss
   lives in **typed-value materialization / `StructRegistry` arena+builder indirection** (per
   `docs/benchmarks/post-AZ-IV.json`), not dimension dispatch. Further dimension micro-optimization
   (a re-scoped lever-2 = first-byte single-dispatch over unit classes to drop the inner rollback)
   is at best a precondition for an append-only checkpoint and is invisible until the materializa-
   tion cost is addressed; on its own it will not move the floor either. The defensible next move
   is a tranche targeting the typed-deposit / registry-indirection path, re-samplied to find the
   NEW typed ceiling, reported as "Track 1 typed CSSOM achieves X% of lightningcss, residual
   ceiling is Y." X < 100 remains a valid success; only a fabricated beat is failure.

**One disposition note for the merge.** The W6 gate test passes (it checks accounting consistency
+ `shared_summary_equal`, both true) even though the SOTA disposition is `REJECTED`; this is the
intended honest behavior, not a regression. The lightningcss comparator is permanently wired into
the W6 harness, so future work re-measures against both references automatically.
