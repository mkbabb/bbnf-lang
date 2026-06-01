# S-P0 audit-overfit hardening V2 — CH4 COST (post-fold confirm)

Lens (CH4 COST per ORCHESTRATOR §3W): the audit's LOC budget, prune budget, addenda
incremental cost, and wave-cost framing are accurate, disk-true, and do not seed an
un-satisfiable or mis-budgeted gate; every kernel/primitive carries a same-wave consumer.
V2 is the POST-FOLD confirm: the single V1 CH4 REVISE (R1-CH4 — the G1 JSON-projection ±5%
line-count tripwire must be stated as SOFT, the binding cost-control the oracle diff-match)
must be DISCHARGED, and the one NEW cost the fold introduces (the R16 `RuntimeTarget:
PartialEq` derive) must be surfaced + bounded, not smuggled.

**Every figure below independently re-measured on disk this pass at live HEAD `83b66db42`**
(not carried from V1). Scope (this lens): the cost surface of `a0`/`a1`/`a2`/`a3` +
`SYNTHESIS-AUDIT-OVERFIT.md`; the PRUNE-sequencing cost couplings live in `a2`.

## V1 REVISE discharge (R1-CH4) — DISCHARGED

The ±5% line-count tripwire is now stated as SOFT and the oracle byte-for-byte diff-match as
the binding cost-control in EVERY binding artefact, re-grepped this pass:

- `SYNTHESIS-AUDIT-OVERFIT.md:120-121`: "(not a ±5% line-count delta) is the BINDING proof
  the projection is real, not a re-stringification. The ±5% line-count is a SOFT tripwire
  only; a faithful projection may legitimately reorder/dedupe past it."
- `a0:472-477` (CH4 R1 fold): "the G1 `json/generated.rs` ±5% line-count tripwire (today
  1235 LOC, ±5% = ±62 LOC) is a SOFT tripwire ONLY … State the ±5% as soft and the oracle
  diff-match as binding wherever the [budget is cited]." Disk-confirmed today's
  `json/generated.rs` = **1235 LOC** (so ±5% = ±62), and `a0:535`/`:563` carry the fold into
  the alphaE feeder + SYNTHESIS §2.1.
- `a2:45` (V2 fold ledger): R1-CH4 routed as "NOT-a2-primary (alphaE feeder / a0 §2.1
  fold)"; a2 §7 G1 reference carries the oracle-diff-match framing — correct attribution, no
  inverted HARD-gate survives in a2.

A faithful projection that legitimately reorders/dedupes/renames (`parse_w11_1_number` →
`parse_number`, P5) may exceed ±5% while remaining a true projection — the SOFT framing
correctly prevents a mis-budgeted HARD-reject gate. DISCHARGED (no orphan REVISE).

## Dispositions (each re-measured at HEAD `83b66db42`)

- **A1 — PRUNE net LOC ≈ −10800 (ACCEPT, disk-verified exact).** Re-measured every surface
  this pass: P1 = `crates/bbnf-simd/src/x86_64/` **24 files / 742 LOC** + `ext/x86/` **4
  files / 3554 LOC** (`bbnf.asm`,`x86util.asm`,`x86inc.asm`,`LICENSE-VENDOR`) + `build.rs`
  **102 LOC** = 742+3554+102 = **4398 ≈ the cited −4500**; P3 = 6 × **910** (the 7
  `css_l4_*/generated.rs` all disk-measured exactly **910 LOC**, all md5
  `b654562ccff46ed62dd48e9ace325830`, distinct-md5 set = 1) = **−5460 EXACT**; P2 ~−700
  (partial gut of the 3737-LOC `nonjson_css_l4.rs`, KEEP oracle `:451` +
  `css_canon_bench.rs`); P4 +~15; P5 rename-only. Sum ≈ **−10560…−10800** — accurate, if
  anything understated, and a REDUCTION, so zero `[generated-size-budget]` overflow risk.
  Cited at `a2:192-195`, `SYNTHESIS:151`, `HARDENING-S-P0-CONSOLIDATED:56`.

- **A2 — the two net-LOC figures are SCOPE-DISTINCT and both correct (ACCEPT).** `a0:43`
  cites "net LOC ≈ −12,650…−12,850" (the WHOLE-CYCLE figure: PRUNE −10800 + B1/B2 dedup + G5
  −100 + PROVE/G6 +250 capped); `a2:192`/`SYNTHESIS:151` cite "−10800" labeled "PRUNE net
  LOC" (the PRUNE cluster ALONE). Both trace to the alphaE LOC budget; each carries the scope
  it measures. No contradiction (a0 measures the cycle's net deletion; a2/SYNTHESIS measure
  the PRUNE cluster).

- **A3 — no un-satisfiable count gate (ACCEPT, disk-verified).** The checkasm count is the
  disk-true **14** (re-enumerated this pass: 12 single-kernel `checkasm_<kernel>.rs` +
  `checkasm_common.rs` + `checkasm_parity.rs`), NOT the stale "18" — grepped all 5 artefacts,
  the ONLY occurrence of "18" is the explicit correction at `a2:277` ("checkasm count = 14 =
  12 single-kernel + 2 aggregate, NOT 18"); zero gate keys on an unmeetable count. P1's gate
  is satisfiable-BY-CONSTRUCTION: the deletion list (a)–(i) is reach-matched to the verify
  grep (24+4 files + `build.rs` + `Cargo.toml:19` `nasm-rs = "0.3"` ACTIVE dep, confirmed
  present this pass + companion comments `:14-17`, + the 9 `checkasm_parity.rs` scalar
  sites), so the gate cannot ship RED-by-construction.

- **A4 — the P1↔`checkasm_parity.rs` build-soundness coupling is COSTED, not smuggled
  (ACCEPT, disk-verified exact).** Re-grepped this pass: `checkasm_parity.rs` carries exactly
  **9 ACTIVE** `bbnf_simd::x86_64::…::*_scalar(…)` call sites at
  **`:458,:464,:467,:477,:478,:484,:493,:497,:502`** (verbatim match to `a2:104`/§3) + 11
  `x86_64` tokens total. `a2 §3` correctly prices the `src/x86_64/` deletion as
  build-BLOCKING without decoupling these 9 sites, and makes `cargo build` AND `cargo test
  --no-run` clean the exit gate WITH the decouple in the SAME wave (no intermediate
  broken-build commit). A real cost surfaced and bounded — not an assumed one-line `rm -rf`.
  The 12 aarch64 single-kernel differentials + `checkasm_common.rs` are preserved.

- **A5 — the 6 addenda add ZERO new code/hardcoding cost, and the ONE incidental cost is
  surfaced + bounded + disk-confirmed viable (ACCEPT).** Every addendum lens (a1 §L1–L6) is a
  grep/diff/md5/samply over the EXISTING benched tree — no code, no hardcoding added (`a1:21`
  "executable check (grep / diff / md5 / samply)"). The G6 acceleration body ceiling is
  committed+capped at **+~150 per real NEON body** (PMULL `bitmap_prefix_xor_64` is the ONE
  committed body; every other kernel RETIRED or honestly relabelled UNLESS a same-wave
  hot-path consumer exists — `alphaE:211`, the same-wave-consumer discipline §3W demands),
  net PROVE/G5/G6 ≈ +250 capped; G6's retire branch is gated on a samply MEASUREMENT (an
  S-P1 cost already in plan), not an assertion. The R16 nested-struct gate-recipe fold names
  ONE incidental cost: realizing the structural P3 collapse-check via the cleanest sufficient
  mechanism (`RuntimeTarget: PartialEq`) adds **one derive line** — disk-confirmed
  `regen.rs:5` carries only `#[derive(Clone, Copy, Debug)]`. I verified the derive is
  genuinely VIABLE: BOTH nested-struct fields AND the `emitter` enum derive `PartialEq, Eq`
  (`grammar_provider.rs:39` `RuntimeEmitterKind`, `:45` `RuntimeFrontendRequirements`, `:91`
  `RuntimeOutputLabels`), and every remaining field is `&'static str` / `&'static [&'static
  str]` (all PartialEq) — so the one-line derive compiles and is the minimal sufficient
  mechanism. This cost is surfaced in all binding artefacts (`a2:377-384`, `a3 §3
  :169-174`, `SYNTHESIS:214-217`, `HARDENING-S-P0-CONSOLIDATED:28/:73-76`), correctly priced
  as one line, not smuggled.

## REVISE (0) / REJECT (0)

The single V1 CH4 REVISE (R1-CH4) is discharged across SYNTHESIS §2.1 + a0 §CH4-R1 fold +
the alphaE feeder; the one new cost (R16 one-line `PartialEq` derive) is surfaced, bounded,
and disk-confirmed viable; every cost figure is disk-true; no un-satisfiable count gate, no
mis-budgeted gate, no orphan REVISE.

## Tally

ACCEPT 5 · REVISE 0 · REJECT 0 — **100%**. Second-consecutive-cycle CH4 at 100% (V2 confirms
the V1 83.3% → R1-CH4 discharged). Every figure independently re-measured this pass at HEAD
`83b66db42` and disk-true: PRUNE −10800 (P1 4398 ≈ −4500, P3 −5460 EXACT, P2 ~−700), cycle
−12,650…−12,850, checkasm **14** (not 18), `json/generated.rs` 1235 LOC, 9 checkasm scalar
sites `:458…:502`, `RuntimeTarget` 12 fields deriving `Clone, Copy, Debug`-only. The R16
one-line `RuntimeTarget: PartialEq` derive cost is surfaced + bounded + disk-confirmed viable
(all fields PartialEq); the G6 body ceiling is committed+capped at +~150 with a same-wave
consumer gate per §3W. Zero un-satisfiable gate, zero mis-budgeted gate, zero orphan REVISE.

TALLY accept=5 revise=0 reject=0
