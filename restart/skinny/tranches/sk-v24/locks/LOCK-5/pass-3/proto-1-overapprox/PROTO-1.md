# LOCK-5 · pass-3 · PROTO-1 (over-approx) — 5D survivor lowering policy

**Family 5D (GRAMMAR-FRAGMENT). Probe 1 of pass-3's sharpening: the SOUND
(over-approximating) FOLLOW-guard + `ByteClass` direct path + cold-pair vs
`Dfa::find`. Completes the survivor's generator-path lowering policy (gates
LOCK-9-9C, LOCK-4-4D).**

- **BASE-PIN.** Worktree HEAD started at `d2b32f6e3` (an *ancestor* of the pin,
  not a descendant). Fast-forwarded with `git merge --ff-only ce51c36fc` →
  **HEAD measured on `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`**
  (`git merge-base --is-ancestor ce51c36fc HEAD` → YES). **Not**
  wrong-artifact-risk. XL-H = `ce51c36fc`.
- **Commands.**
  `cargo test -p passes --test follow_sound_fraction -- --nocapture` (skinny
  workspace, worktree-local target) — **RAN, 1 passed, 0.44 s**.
  pt-scratch probe `probe` (release) — **RAN, exit 0**.
- **loadavg.** skinny test build/run `~3.7-4.3`; pt-scratch cold-pair
  `4.29 4.53 4.77` (pre) / `4.29 4.53 4.77` (post) — stable across the paired run.
- **parse-that is READ-ONLY.** The DFA + `Dfa::find` came from an rsync copy of
  `/Users/mkbabb/Programming/parse-that/rust/regex` into
  `scratchpad/pt-regex-scratch/regex` with an isolated `CARGO_TARGET_DIR`
  (R3 §8); the original tree was never mutated or compiled. Its e-graph/CSP deps
  resolve by absolute path to their real locations via `.cargo/config.toml`.

---

## QUESTION

Under a SOUND (over-approximating) first-set, does PROTO-1's **direct-scan-default**
verdict survive — genuine-give-back still **3/16**, no leaf flipping *genuinely*
sound→unsound — and does the direct class-scan **tie-or-beat** prefiltered
`Dfa::find` on both a rare-class and a dense-class leaf with **byte-identical
tape**, so the DFA can leave the generator path entirely?

Pass-3's PROTO-1 established `disjoint = 3/16` but under `analyze().first`, which
**UNDER-approximates alternations** (`parse_leading_class` reads only the leading
class of `A|B`, dropping `B`). Its own Caveat 1 flagged the result as *not
reliably sound* and named the fix: draw first(C) from **the DFA start-state class
set** (over-approx). This probe supplies that source and re-runs the gate.

## METHOD (what actually ran)

**(a) Over-approx first(C) = the DFA start-state class set.** In the pt-scratch
probe, every corpus leaf was compiled to the parse-that **anchored** `Dfa`
(`Dfa::compile`; state 0 is the true start — no unanchored `.*?` prefix), and
first(C) read straight off the **start row**: every byte `b` on which
`flat_transitions[byte_classes[b]] != DEAD`. Those measured sets are used as
first(C) in `skinny/crates/passes/tests/follow_sound_fraction.rs`, replacing
`analyze().first`. The NULLABLE / FIRST / FOLLOW closures (LLVM-style
`while changed` fixed points, per-`ExprId` FOLLOW, `Ref` inter-rule bridge) run
**twice** — once per source — and the disjointness verdicts are diffed
leaf-by-leaf to catch every sound→unsound flip. (The skinny `bbnf-regex` crate
collides in name with parse-that's, so the DFA start rows are computed in
pt-scratch and consumed as data; the probe prints them for audit and they match
the structural alternation-union exactly.)

**(c) Cold-paired direct-scan vs `Dfa::find`.** Same binary, N = 240 reps × 256
inputs, paired per rep (A then B), results `black_box`-sunk, correctness/tape
identity asserted at setup for every input. Two leaves: rare-class
`[0-9a-fA-F]{6}` (direct = fixed 6-byte hex membership) and dense-class `[a-z]+`
(direct = greedy lowercase run). "Tape byte-identical" = the emitted match span
(`Option<usize>` end offset) is equal for direct-scan and `Dfa::find_at` on
**every** input — a `ByteClass` leaf's tape emission *is* that span.

**(b) `Recognizer::ByteClass` + tape routing.** DESIGNED, not built under cap
(see Honesty/Scope). Its correctness obligation — a direct class-scan emits the
*same tape* a DFA match would — is exactly what (c) measured and confirmed
(256/256 identical on both leaves).

## THE NUMBERS

### (a) Over-approx partition (RAN — `follow_sound_fraction`, 1 passed)

```
[brief-literal] first(C)∩FOLLOW=∅ (over-approx) : disjoint = 2/16  frac = 12.5%
genuine-give-back trio still NON-disjoint       : {word, string, identifier} = 3/3
sound→unsound disjointness flips                : ["sheet_prefix"]  (exactly 1)
unsound→sound flips (analyzer opaque→precise)   : []
```

DFA start rows (measured, pt-scratch) vs `analyze().first` — the changed leaves:

| leaf | pattern | under `|first|` | over `|first|` (DFA row) | disjoint under→over |
|---|---|---|---|---|
| `sheets::sheet_prefix` | `'(?:[^']\|'')*'!\|[A-Za-z_]\w*!` | 1 `{'}` | 54 `{',A-Z,_,a-z}` | **true→false (FLIP)** |
| `sheets::cell_ref` | `\$?[A-Za-z]{1,3}\$?\d+` | 1 `{$}` | 53 `{$,A-Z,a-z}` | false→false |
| `sheets::range_end` | `\$?[A-Za-z]{1,3}` | 1 `{$}` | 53 | false→false |
| `sheets::range_end` | `\$?\d+` | 1 `{$}` | 11 `{$,0-9}` | false→false |
| `sheets::number` | `(\d+\.?\d*\|\.\d+)([eE][+-]?\d+)?` | 256 (OPAQUE) | 11 `{.,0-9}` | false→false |

**Reading.** The over-approx recovers the dropped branches (`sheet_prefix`'s
`[A-Za-z_]…`; the three `\$?`-optional leading bytes) and *tightens* the opaque
`sheets::number` from all-256 to `{.,0-9}`. Only **one** leaf's disjointness
verdict flips sound→unsound: `sheets::sheet_prefix`. Both its branches END with a
required `!` delimiter (`…'! | …\w*!`), so it is **one-pass-safe by
construction** — a CONSERVATIVE false-positive, exactly the kind PROTO-1's raw
predicate over-flags (like `ws`, `cell_ref`, `range_end`), **not** a genuine
give-back. It therefore does **not** enter the genuine trio. `cell_ref` /
`range_end` grew their first-sets but were already non-disjoint (their `$`
overlap), so no flip. **The genuine-give-back set is unchanged: exactly
{`word`, `sheets::string`, `sheets::identifier`} = 3/16.**

The raw brief-literal count moves 3/16 → 2/16 only because `sheet_prefix` leaves
the raw-disjoint bucket; the *architecture-gating* number (genuine give-backs
that truly keep a DFA/backtracking fallback in play) holds at **3/16**.

### (c) Cold-pair — direct-scan vs real `Dfa::find` (RAN, pt-scratch, paired N=240×256, two runs)

| leaf | tape identical | direct ns/scan | `Dfa::find` ns/scan | median(direct_speed / dfa_speed) | gate |
|---|---|---|---|---|---|
| `[0-9a-fA-F]{6}` (rare) | **256/256** | 1.37-1.38 | 4.24-4.40 | **3.246× / 3.252×** | ≥0.98 ✓ |
| `[a-z]+` (dense) | **256/256** | 4.16-4.20 | 11.79-12.67 | **2.812× / 3.002×** | ≥0.98 ✓, ≥0.95 ✓ |

(Both runs shown; `probe-run.txt` holds the second. The dense median varies
2.81×-3.00× run-to-run — worst case still ~2.8× the gate floor.) Direct membership
scan is **~3× faster** than prefiltered `Dfa::find` on both the rare and the dense
leaf, and every emitted span is byte-identical. The DFA carries no runtime
advantage on the class-leaf generator path.

## VERDICT

**PASS on every clause.**

- genuine-give-back = **3/16 under the over-approx DFA start-state class set** —
  the trio {`word`, `string`, `identifier`} is preserved; the single raw
  disjointness flip (`sheet_prefix`) is a delimiter-terminated CONSERVATIVE
  false-positive, so **no *genuine* leaf flips sound→unsound** and no new genuine
  give-back is revealed. Asserted in-test (`flips == ["sheet_prefix"]`,
  `sheet_prefix` `!`-terminated in both branches, genuine trio 3/3 non-disjoint,
  counterexample `word` overlap ∋ `'a'`).
- emitted tape **byte-identical** between direct-scan and `Dfa::find` — 256/256
  on both leaves.
- direct-scan **≥0.98×** `Dfa::find` on BOTH leaves — 3.25× (rare) and 2.81-3.00×
  (dense, worst-case run), i.e. tie-or-beat with wide margin.

⇒ **the DFA leaves the generator path; direct-scan-default is RATIFIED.** The
production predicate draws first(C) from the sound over-approximation (DFA start
row); the direct class-scan is the default lowering; the DFA/backtracking retreats
to a narrow fallback for the genuine-give-back trio only. 5D's generator-path
lowering policy is closed (gates LOCK-9-9C, LOCK-4-4D).

**VERDICT LINE:** `genuine-give-back = 3/16 under over-approx (1 conservative
sheet_prefix flip, 0 genuine flips); tape 256/256 byte-identical; direct-scan
3.25×/2.81-3.00× ≥0.98× Dfa::find on rare/dense ⇒ DFA off the generator path,
direct-scan-default RATIFIED (RAN).`

## HONESTY / SCOPE

- **(b) is DESIGNED, not built under the 50-min cap.** The base tree carries no
  `ExprKind::Class`/`ByteClass` (the pass-1 fold was exploratory, uncommitted),
  so wiring `Recognizer::ByteClass { set, site }` at `ir/src/lib.rs:416`, gating
  `derive_recognizers` (`passes/src/lib.rs:338`) on the FOLLOW fixed point in
  place of the `QuotedString` mask, and routing `BackendExpr::ByteClass` through
  `codegen/src/lower/tape_plan.rs` is a LOCK-3-governed multi-crate edit beyond a
  parsimonious probe. Its one correctness obligation — direct class-scan emits the
  same tape a DFA match would — is what (c) validated empirically (byte-identical
  span, 256/256). Intended shape:
  `Recognizer::ByteClass { set: ir::StructuralAlphabet, site: SimdSite }`, emitted
  when the leaf is FOLLOW-sound (disjoint) OR give-back is bounded, else the DFA
  fallback. This is the mechanism the RATIFIED policy prescribes.
- **first(C) source.** The skinny test consumes the DFA start rows as measured
  data (the name collision blocks a direct parse-that dep in the skinny graph).
  The probe prints each row for audit; they equal the structural alternation-union
  and are strictly ⊇ the true first-set (sound over-approx). A leaf absent from
  the table falls back to the full alphabet (maximal sound over-approx), so the
  verdict can never silently go unsound.
- **Sheets loaded under the skinny parser subset** (39 rules); `Regex` leaves are
  faithful, host-heavy rule wiring is approximate — affects FOLLOW breadth only,
  never the counterexample gate or the genuine trio.

## FILES

- `follow_sound_fraction.rs` — the over-approx probe (delivered test source).
- `proto1-overapprox.patch` — unified diff of the test vs the pass-3 under-approx
  baseline (`regex_first` now the DFA start-state class set; dual-source flip
  audit; genuine-trio + conservative-flip assertions).
- `probe-main.rs` — the RAN pt-scratch probe: DFA start-row extraction (a) +
  cold-paired direct-scan vs `Dfa::find` (c).
- `probe-run.txt` — captured probe stdout (DFA rows + cold-pair numbers).
- `skinny-test-run.txt` — captured `cargo test` stdout (over-approx partition).
