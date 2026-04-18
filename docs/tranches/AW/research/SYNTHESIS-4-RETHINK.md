# AW-IV → AW-V Rethink (post-W3-flat)

AW-IV.W3 closed with the consumer levers ledger-marked active but the
bench functionally flat:

| Entry | post-AW-IV-W2 | post-AW-IV-W3 | Δ |
|---|---:|---:|:---:|
| json twitter | 280 | 277 | flat |
| json citm | 289 | 287 | flat |
| css bootstrap | 14 | 14 | flat |
| css tailwind | 16 | 16 | flat |
| sheets (all) | — | unchanged | flat |
| bbnf (family) | — | small bumps only | near-flat |

W3's own close ledger admits why: ShapeRef still needs a tape-side
promotion path (emitter annotates, walker falls through to normal
`push_compound_fused`); CTNS admitted `0`; bounded regex judged
unsound; throughput translation deferred. The levers landed as markers,
not hot-path wins.

## Diagnosis: wrong-granularity hot loop

CSS's 154 KB walker is 0.1–0.2% self-time (P2). `try_branch` is
70.9–78.9%. Every AltLinear probe loops through `dispatch_one` and
re-enters cold interpreter semantics — AW-IV's consumer activations
did not change that. The generated walker arms still call
`try_branch(...)` verbatim (`generated.rs:4549`); `try_branch` still
loops on `dispatch_one(...)` (`driver.rs:1277`). More mined levers
does not remove the interpreter boundary.

JSON's walker is monolithic per W4.d (+W1.4-aggressive), but ~33% of
cycles are still sonic-doesn't-do-this work: `advance_or_pop_with`,
`finaliser::finalise`, PSI writes, `nearest_variant_frame`, plus a
second full tape-walk via `walk_cursor` to materialise Value.

## Repeated failure mode

**Substrate → partial consumer → bypassed hot path.** Observed in
every AW-IV wave ledger. Observed in X (charclass kernels exist but
three emit sites bypass them; backend logic rediscovers decisions).
The governance problem is generic: IR decisions become *advisory*
rather than *authoritative*; the backend falls through to legacy
paths when the consumer is "too narrow" to cover a case.

## The pivot

1. **AW-IV freezes as a cleanup tranche post-W4.** W4 finishes as
   in-flight; W5 + W6 narrow to **only** work that directly removes
   `try_branch` + helper-call boundaries on the existing hot path.
   No new architectural levers. No throughput gate beyond
   "cross-crate boundary removed where it still stands". `post-AW-IV.json`
   lands as honest close even if throughput is still flat on CSS /
   Sheets / BBNF.

2. **AW-V is the real performance path, narrowed:**
   - **JSON prototype first** (`bbnf-wt-aw5-prototype` worktree,
     in flight).
   - **One-pass parse plus monomorphic visitor.** `ValueVisitor`
     path skips the tape entirely; no second `walk_cursor`.
   - **`TapeVisitor` path retained for AX** — the tape is the AX
     substrate, not the value-benchmark path.
   - **Keep `DTA_TABLE` + `dispatch_one` only as cold replay
     substrate.** Hot path never reaches them.
   - Exceed-sonic gate on single-thread NEON, measured head-to-head.

3. **Generalise only after prototype validates**, and with the
   **11-shape taxonomy** (H1 audit):
   - 7 B4 shapes + ArgList + Flat + Wrap + HRegex.
   - Permanent interpreter fallback for `funcBody` /
     `customPropertyDecl` / `genericDecl` (~3–5% CSS).
   - CSS L4 hot/cold partitioning built in from W4.1 opening per
     H2's L1-fit analysis — not a future retrofit.

4. **N1 / N2 novel-exceed layer stays OUT of the critical path.**
   Speculative parsing, JIT, PMC feedback, e-graph rewrite codegen
   are deferred per scope decisions in `SYNTHESIS-3-PROTOTYPE-PATH.md`
   §Deferred. They are premature until the basic per-shape emitter
   beats the current walker decisively.

5. **Governance rule applied to X:** IR decisions become
   **authoritative consumers**, not advisory substrate the backend
   can bypass. Concretely:
   - X §2 item 1 (`backend/patterns/{delim_scan,key_dispatch,cache}.rs`
     still in workspace): migration path must land inside X, not
     be deferred as a substrate gesture.
   - X §2 item 2 (`prefix_class.rs` 17-line stub, `sep_list.rs`
     23-line lookup): close or delete; no more "wired-but-not-consulted"
     kernels.
   - X §2 item 3 (`seq/repeat/ref/wrap_strategy.rs` standalone
     classifiers): either consume `ir.recognizer_decisions` or
     document explicitly why the smaller decision spaces don't
     warrant CSP consumption.
   - Same invariant applies forward to every future tranche:
     substrate-without-authoritative-consumer is rejected at wave
     close per `docs/instructions/README.md` §Wave-verification-ledger.

## AW-IV close contract (post-rethink)

AW-IV.W4 finishes in-flight per the current plan. AW-IV.W5 + W6
narrow to:

- **W5**: remove every remaining cross-crate helper-call boundary on
  the hot path. Concretely the 4 helpers P1 flagged
  (`advance_or_pop_with`, `finaliser::finalise`, `psi::write_decoded`,
  `nearest_variant_frame`). Inline-emit their bodies into the walker
  arms via the `bbnf-tape-codegen` infrastructure; or if that
  infrastructure hasn't landed, `#[inline(always)]` + workspace LTO
  as the fallback.
- **W6**: FINAL + `post-AW-IV.json` with full 19-entry matrix.
  Honest close. If throughput is still flat on
  AltLinear-dominated grammars (CSS / BBNF / Sheets), the FINAL
  records it and cites the AW-V path as the remediation — no
  pretence.

The existing AW-IV W5 (`reduce_column<C, R>` + SIMD pack + parity
harnesses + cost-model grid) items move forward:

- **`reduce_column<C, R>` + 4-lane SIMD pack**: retained. It's a
  consumer API over the tape; ships alongside the helper-inlining.
  Parity-harness-ready.
- **sonic-rs + lightningcss parity harnesses**: retained. They are
  CI-gated correctness verifications, not throughput levers; land
  in AW-IV.W5 or AW-IV.W6 regardless of the narrowing.
- **Cost-model grid sweep (AM.6 chronic)**: defers to successor
  tranche. Not part of the interpreter-removal path.
- **AVX2 u8x32 widening, scanner cluster consolidation, NEON
  17-digit, bloom + GADT dedup, document-parallel fork**: all
  defer. These are multipliers over the interpreter-shaped hot
  path; applying them to AW-IV changes nothing structural. They
  either (a) fold into AW-V's per-shape emitter where applicable,
  or (b) land in a successor tranche when the interpreter is
  actually gone.

## AW-V execution contract (post-rethink)

AW-V as specified in `docs/tranches/AW/AW-V.md` (post `fada0daa`
commit folding in 11 shapes + deferral list) already embodies the
narrowing. The rethink makes the following explicit:

1. **AW-V is the primary performance path post-AW-IV-close.** No
   waiting for a successor tranche. AW-V.W1 opens immediately after
   AW-IV.W6 close.
2. **AW-V.W2.1 gate — the benchmark constraint**: the prototype
   bench is JSON-value materialisation via `ValueVisitor` *only*;
   no second tape walk. The AW-III-era `walk_cursor` second-pass
   gets bypassed explicitly. If sonic-parity is not achievable on
   the `ValueVisitor` single-pass path, the prototype's arch is
   wrong; iterate until it works.
3. **AW-V.W2.3 gate — exceed-sonic**: the 6 novel levers applied
   to the prototype, measured head-to-head on single-thread NEON.
   If the sonic-parity `ValueVisitor` hits the gate with only
   3–4 of the 6 levers, the remaining levers *don't land in AW-V*
   — they wait for AW-VI or AX (per the scope principle).
4. **AW-V.W4 CSS hot/cold partition mandatory**. Not a retrofit.
   The first CSS shape emitter in W4 carries the partition from
   its opening commit per H2's L1-fit projection.

## Scope principle reiterated

**Prove substrate viability with AW-V. Everything else layers on top
of proven ground.** AW-IV finishes on honest terms; AW-V builds the
honest performance path; AX consumes the proven substrate for gradient
parsing, speculative parsing, and custom visitors. Successor tranches
(e-graph rewrite, runtime CPU autotune, PMC feedback, JIT) layer
meta-optimisation over proven emitters — never before.

Indefatigable. Substrate-without-authoritative-consumer is the
pattern to eliminate across every tranche. IR decisions must reach
the hot path as compile-time code, not runtime advice.
