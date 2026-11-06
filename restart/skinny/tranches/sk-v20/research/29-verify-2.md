# 29 — Adversarial Verification Pass (verify-2)

Six load-bearing claims from sibling audits, each re-derived from source at HEAD
`36ca10646`. Verdicts default to skepticism; evidence cited independently.

---

## Claim 01 — DTA/PSI deletion reclaimed ~78,500 LOC — **QUALIFIED**

The core figure is empirically defensible but its attribution and provenance are
loosely stated.

- `docs/tranches/AX/AX.md:12` does say "Total reclaim ~78,500 LOC" — CONFIRMED as
  text. But the figure is a *bundled projection*, not a measured DTA/PSI-only
  reclaim. The same line itemizes the 78,500 as DTA machinery **plus** PSI, plus
  `simd-scan::emit/*`, 7 dead `GrammarProfile` slots, `bbnf-tape-codegen`,
  deprecated `csp_strategy` aliases, Lever 4, ~3,900 LOC test suites, and
  **~57,481 LOC of generated.rs walker output**. Attributing the whole 78,500 to
  "the DTA/PSI interpreter deletion" overcounts: the interpreter *source* is
  ~12,000 LOC (`docs/tranches/AX/waves/W0b.md:11`); the dominant 57,481 is
  *generated* output, not hand-written substrate.
- The number originated as a *planned hard-gate target*, not a post-hoc
  measurement: `docs/tranches/AX/waves/W0b.md:107` lists "LOC delta:
  `git diff --stat master...HEAD` showing ~78,500 LOC reclaim" as a gate metric.
- Independent measurement nonetheless vindicates the magnitude. The committed
  AX.W0b range (`51e77d015^..0adabb233`) shows `145 files changed, 8278
  insertions(+), 95387 deletions(-)`; generated.rs alone churns ~83,848 lines in
  that range. So the deletion is real and if anything *under*-counted at gross.
- "~400 commits across AV–AW-V" — corroborated by `AX.md:1`/`:5` ("the 400-commit
  work") and report-01:73. CONFIRMED.
- "largest single substrate-without-consumer reversal" — CONFIRMED. The catalogue's
  next-largest reversals are an order of magnitude smaller: combinator path
  ~2,370 LOC (report-01:42), columnar tape revert (D-13), structural-dispatch
  delete. None approach ~87K net.
- "direct empirical basis for AX invariant 13" — CONFIRMED. Invariant 13
  ("Ledger-only wave = re-plan trigger") sits at `AX.md:36`; report-01:86 records
  it was "written retrospectively" off this failure.

Qualification: the headline number conflates interpreter-source with
generated-output and with several non-interpreter carves, and is a target rather
than a measurement. The *spirit* (largest reversal; basis for invariant 13) holds.

---

## Claim 02 — MASTER-PLAN §13.1 x86 vs LOCKS.md:635 "direct contradiction" — **QUALIFIED**

The two cited facts are accurate; the "direct contradiction" framing is overstated,
and the "MASTER-PLAN is stale" inference is imprecise.

- (a) CONFIRMED: MASTER-PLAN §13.1 (header `MASTER-PLAN.md:700`; allowlist body
  720+) retains the full x86_64 AVX-512 VBMI2 table at `:734`-`:747`
  (`_mm512_mask_compressstoreu_epi8`, `_mm512_ternarylogic_epi64`, GFNI, IFMA,
  VNNI, etc.).
- (b) CONFIRMED: `LOCKS.md:635` ("Lock 16/8 aarch64-ONLY clause") marks "The whole
  x86 surface (`skinny/crates/bbnf-simd/src/x86_64/` + `ext/x86/` + the nasm driver
  + the `diagnostic-x86` gate exclusion)" a "P1 DELETION target, not a measured
  plane."
- (c) **"direct contradiction" — REFUTED in the strong sense.** The two operate on
  different objects. `LOCKS.md:635` orders deletion of the *skinny x86 source/build
  surface* while in the same sentence *preserving* x86 as "totality-SECONDARY
  architecture pressure that closes NO skinny row." MASTER-PLAN §13.1 retains x86
  AVX-512 precisely as that documented architecture-pressure literature (the table
  header `:734` frames it "the route to outclass asmjson"), and MASTER-PLAN itself
  already brands x86 as non-primary: `:732` ("x86/SVE remain diagnostic;
  source-present-without-consumer is a manifest entry, not an admit"), `:942`
  ("aarch64 only. x86 and AVX-512 remain diagnostic"). A citation manifest retaining
  x86 literature is not in contradiction with a source-crate deletion order that
  explicitly keeps x86 as literature.
- (d) **"v+1 boundary means MASTER-PLAN is stale" — REFUTED as stated.** The
  governance boundary at `LOCKS.md:652`-`661` constrains what v+1 *may authorize*
  ("No implementation wave may use v+1 wording as permission to add a directive …
  expand `BackendShape` …"); it does not declare MASTER-PLAN §13.1 stale.

Qualification: there IS a genuine documentation tension worth a cleanup — MASTER-PLAN
§13.1 should annotate its x86 AVX-512 table as "source-deleted; literature-only
architecture pressure" to mirror `LOCKS.md:635`. But that is a precision gap, not a
"direct contradiction" or a staleness invalidation.

---

## Claim 03 — sk-v7..v10 kernels all failed; only tape-activation succeeded — **QUALIFIED**

Every enumerated micro-fact is corroborated by the rejected-route ledger; the
sweeping generalization "NEON work in isolation reliably fails" overgeneralizes.

- W4 single-quartet Unicode classifier — rejected: `skinny/REDRESS.md:2285`-`2287`
  (Item 82). CONFIRMED.
- W5 StringBlock16 tiny probe — rejected: `skinny/REDRESS.md:2318`-`2320` (Item 83).
  CONFIRMED.
- W6 object-pair value-byte compaction — rejected: `skinny/REDRESS.md:2358`-`2360`
  (Item 84). CONFIRMED.
- W10 PMULL prefix-XOR — rejected: `skinny/REDRESS.md:2512`-`2540` (Item 88).
  CONFIRMED.
- W10b CTZ/bulk consumer — rejected: `skinny/REDRESS.md:2542`+ (Item 89). CONFIRMED.
- W8 hex escape: micro-proof aggregate `1.268x` — `skinny/REDRESS.md:3185` (Item
  107); "moves no row and wires no new production behavior" `:3194`. W9 then
  rejected because "its exact `unescape_string` caller already consumed
  `unescape_uxxxx_x4_neon` before W9" (`:3200`-`3202`, Item 108), with the direct
  Criterion floors FAILing (`:3217`-`3218`). CONFIRMED exactly.

The implication is **QUALIFIED**: "NEON work in isolation reliably fails" is too
absolute. NEON *structural-scan* work succeeded — SK-V17 NEON structural-index
acceleration (`6bb4b2a6c`, report-03:243) and SK-V18 G6 NEON structural classify
(`369f4ffe7`, report-03:264-265) both landed as admitted accelerations. The honest
explanation is the SK-V8 substrate-ceiling thesis (report-03:97): the per-leaf
string/escape kernels optimized a scalar rediscovery pass that should not exist; the
structural-scan path that *replaced the cost site* succeeded. The claim itself states
this caveat, so the chain is self-consistent — only the headline "reliably fails"
phrasing overreaches.

---

## Claim 04 — astral layer null-impact (no Omega conclusion invalidated) — **CONFIRMED**

The claim invited a CRUD-LOG search for any Omega-decision reversal. I performed it
independently across `restart/audit/totality/astral/V1..V10/CRUD-LOG.md`.

- Report-04:119-120 states "There is no evidence that the astral layer has
  invalidated a Pass Omega conclusion or triggered a code correction." Accurate
  characterization of its absence-of-evidence method.
- Independent search finds **zero** Omega reversals. Every "re-open" token in the
  CRUD-LOGs is a *negative* assertion: `V5/CRUD-LOG.md:206` and `:242` ("No
  re-opened REDRESS"); `:288` ("none re-opened as active admit"). The one
  Omega-adjacent hardening note, `V9/hardening/CONSOLIDATED.md:15`, *preserves*
  ("Preserve SK-V14/Omega V8 as historical/pre-block evidence only") — it affirms,
  does not overturn.
- The astral CRUD operations are governance-surface reconciliations
  (master-plan/locks/architecture splices) that thread Omega decisions forward, not
  invalidate them.

The independent CRUD-LOG search corroborates the null-impact rather than refuting
it. CONFIRMED. (The downstream judgment "its value is undemonstrated" is an opinion
the verdict does not adjudicate.)

---

## Claim 05 — BackendShape absent from crates/ir; MP.SK15.W8/W9 active pending — **CONFIRMED**

- `rg BackendShape crates/ir/src` returns zero hits. CONFIRMED (re-run
  independently; zero files match in the totality `crates/ir/src`).
- `MASTER-PLAN.md:970` (MP.SK15.W8 BackendShape harness) and `:971` (MP.SK15.W9
  EventTape/SinkOnly/CollapsedStage all-five gate) both read "active pending".
  CONFIRMED verbatim.

Strengthening note (does not alter the verdict): `BackendShape` *is* defined — in
the **skinny** IR (`skinny/crates/ir/src/cost.rs`, `skinny/crates/ir/src/lib.rs`)
and consumed across `skinny/crates/{passes,codegen}`. The absence is specifically in
the **totality** `crates/ir`, which is exactly what "active pending" predicts: the
harness/lowerer enum lives in skinny and has not been adopted into totality
`crates/ir`. The claim's cited grep targets the totality tree from repo root, so the
zero-hit reading is correct and consistent with the W8/W9 status.

---

## Claim 06 — x86 met in skinny; violated only in totality simd-scan — **QUALIFIED**

Core directive-compliance finding is correct; two stated details are imprecise.

- **Skinny x86 source absence — CONFIRMED in substance.** `skinny/crates/bbnf-simd`
  has no `x86_64/` subdir and no `ext/x86/`. The few x86-string matches are false
  positives (`_mm` inside `debug_assert!`) and an explicit disclaimer comment
  "aarch64 NEON ONLY (no x86, no SVE)"
  (`skinny/crates/bbnf-simd/src/aarch64/structural_classify_64.rs:31`).
- **"contains only aarch64/ and scalar/" — REFUTED as a literal directory claim.**
  `ls skinny/crates/bbnf-simd/src/` returns `aarch64/`, `scalar/`, **plus**
  `classifier.rs`, `dispatch.rs`, `lib.rs`. No x86 subdir, but the listing is not
  "only aarch64/ and scalar/".
- **Cited commit `7de58aad` — REFUTED (does not exist).** `git cat-file -t
  7de58aad` → "Not a valid object name." The actual x86-prune commit is
  **`7de58acad`** ("feat(sk-v18-W-PRUNE): P1-P5 deletion") — the only commit
  touching a deleted `skinny/crates/bbnf-simd/src/x86_64/*` path. A dropped `c`; the
  substance (P1-P5 prune deleted the skinny x86 tree) holds.
- **Totality violation — CONFIRMED and sharper than stated.** `crates/simd-scan/src`
  carries live cfg-gated x86 SIMD source: `avx2.rs` (370 LOC), `avx512.rs` (149
  LOC), `wasm.rs` (433 LOC) — exact LOC matches. `crates/simd-scan/src/lib.rs:58`-`65`
  cfg-gates the `avx2`/`avx512`/`wasm` modules and `:93`-`110` dispatches to
  `avx512::scan` / `avx2::scan` / `wasm::scan` inside `scan_structural`. This is a
  real, live aarch64-only-directive violation in the totality tree, absent from
  skinny.

Qualification: the directive-compliance asymmetry (met in skinny, violated in
totality simd-scan) is correct and well-evidenced; only the prose detail "only
aarch64/ and scalar/" and the one-character-corrupted commit hash are wrong.

---

## Verdict roll-up

| Claim | Verdict |
|---|---|
| 01 DTA/PSI ~78,500 LOC reclaim | QUALIFIED (bundled projection; magnitude real) |
| 02 §13.1 vs LOCKS:635 contradiction | QUALIFIED (facts true; "direct contradiction" overstated) |
| 03 sk-v7..v10 kernel failures | QUALIFIED (facts exact; "reliably fails" overgeneralizes) |
| 04 astral null-impact | CONFIRMED (independent CRUD-LOG search corroborates) |
| 05 BackendShape absent / W8-W9 pending | CONFIRMED |
| 06 x86 met-skinny / violated-totality | QUALIFIED (substance right; dir claim + commit hash wrong) |
