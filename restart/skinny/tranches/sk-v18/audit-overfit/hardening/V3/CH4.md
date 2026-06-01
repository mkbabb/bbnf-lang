# S-P0 audit-overfit hardening V3 — CH4 COST (2nd-consecutive confirm, independent re-measure)

Lens (CH4 COST per ORCHESTRATOR §3W): the audit's LOC budget, PRUNE budget, addenda
incremental cost, count gates, and the one NEW cost the fold introduces are accurate,
disk-true, and seed no un-satisfiable or mis-budgeted gate; every kernel/primitive carries a
same-wave consumer. V3 is the SECOND-consecutive ≥95% confirm — the V1 CH4 raised exactly one
REVISE (R1-CH4: the G1 ±5% line-count tripwire must read SOFT, the oracle byte-for-byte
diff-match binding), discharged at V2; V3 re-confirms the discharge held AND that the one new
cost (R16 `RuntimeTarget: PartialEq` derive) remains surfaced + bounded + viable.

**Every figure below INDEPENDENTLY re-measured on disk this pass at live HEAD `83b66db42`** —
not carried from V1/V2. Scope (this lens): the cost surface of `a0`/`a1`/`a2`/`a3` +
`SYNTHESIS-AUDIT-OVERFIT.md` + `HARDENING-S-P0-CONSOLIDATED.md`; the PRUNE-sequencing cost
couplings live in `a2`.

## V1 REVISE re-confirm (R1-CH4) — STILL DISCHARGED at V3

The ±5% line-count tripwire reads SOFT and the oracle byte-for-byte diff-match reads BINDING in
every binding artefact, independently re-grepped this pass:

- `SYNTHESIS-AUDIT-OVERFIT.md:122-124`: "(not a ±5% line-count delta) is the BINDING proof the
  projection is real… The ±5% line-count is a SOFT tripwire only; a faithful projection may
  legitimately reorder/dedupe past it." (`SYNTHESIS-AUDIT-OVERFIT.md §2.1 item 1`.)
- `a0-goalset-residual-overfit.md:485-489` (CH4 R1 fold): "the G1 `json/generated.rs` ±5%
  line-count tripwire (today 1235 LOC, ±5% = ±62 LOC) is a SOFT tripwire ONLY… The BINDING
  cost-control is the byte-for-byte `json_templates/` oracle diff-match BEFORE deletion." Disk
  re-measured: `crates/runtime/src/grammars/json/generated.rs` = **1235 LOC** exactly, so
  ±5% = ±62 — the figure the fold prints is disk-true.
- `a2-prune-sequencing.md:45` routes R1-CH4 as "NOT-a2-primary (alphaE feeder / a0 §2.1 fold)";
  the a2 §7 G1 reference carries the oracle-diff-match framing. Correct attribution — no
  inverted HARD ±5%-reject gate survives in a2.

A faithful projection that legitimately reorders/dedupes/renames (`parse_w11_1_number` →
`parse_number`, P5) may exceed ±5% while remaining a true projection — the SOFT framing
correctly forecloses a mis-budgeted HARD-reject gate. **STILL DISCHARGED at V3** (no orphan
REVISE; the discharge held through the V2→V3 re-grep).

## CH4 dispositions (each re-measured at HEAD `83b66db42`)

- **A1 — PRUNE net LOC ≈ −10800 (ACCEPT, disk-verified exact).** Re-measured every surface
  this pass: P1 = `crates/bbnf-simd/src/x86_64/` **24 files / 742 LOC** + `ext/x86/` **4 files /
  3554 LOC** (`bbnf.asm` 485, `x86util.asm` 1036, `x86inc.asm` 1978, `LICENSE-VENDOR` 55) +
  `build.rs` **102 LOC** = 742+3554+102 = **4398 ≈ the cited −4500** (`a2:208-209`,
  `SYNTHESIS:161`); P3 = 6 × **910** (all 7 `css_l4_*/generated.rs` disk-measured exactly
  **910 LOC**, all md5 `b654562ccff46ed62dd48e9ace325830`, distinct-md5 set = **1**) =
  **−5460 EXACT** (`a2:164`, `SYNTHESIS:169`); P2 ~−700 (partial gut of the 3737-LOC
  `nonjson_css_l4.rs`, KEEP oracle `:451` + `css_canon_bench`); P4 +~15; P5 rename-only.
  Sum ≈ **−10560…−10800** — accurate, if anything conservatively understated, and a REDUCTION,
  so zero `[generated-size-budget]` overflow risk (`a2:209-211` "this is a *reduction* — no
  overflow risk"). Cited at `a2:208`, `SYNTHESIS:153`, `HARDENING-S-P0-CONSOLIDATED:56`.

- **A2 — the two net-LOC figures are SCOPE-DISTINCT and both correct (ACCEPT).** `a0:54` cites
  "net LOC ≈ −12,650…−12,850" (the WHOLE-CYCLE figure: PRUNE −10800 + B1/B2 dedup + G5 + PROVE
  capped); `a2:208`/`SYNTHESIS:153`/`HARDENING:56` cite "−10800" labeled "PRUNE net LOC" (the
  PRUNE cluster ALONE). Each carries the scope it measures — no contradiction: a0 measures the
  cycle's net deletion, a2/SYNTHESIS/HARDENING the PRUNE cluster. Both trace to the alphaE LOC
  budget.

- **A3 — no un-satisfiable / mis-budgeted count gate (ACCEPT, disk-verified).** No gate keys on
  an exact LOC figure — the artefacts state it explicitly: `a1:88` "DESCRIPTIVE only — no gate
  keys on the LOC; the binding gate is `verbatim_blob_present == false`"; `SYNTHESIS:130-132`
  "The CSS-courier LOC is cohort-carried… no gate keys on the exact figure"; `a0:24`/`:109`
  "no gate keys on the LOC". The checkasm count is the disk-true **14** (re-enumerated this
  pass: 12 single-kernel `checkasm_*.rs` + `checkasm_common.rs` + `checkasm_parity.rs`), NOT a
  stale "18" — the only "18" in the artefacts is the explicit correction at `a2:293-294`
  ("checkasm count = 14 = 12 single-kernel + 2 aggregate, NOT 18"). P1's gate is
  satisfiable-BY-CONSTRUCTION: the deletion list (a)–(i) is reach-matched to the verify grep
  (24+4 files + `build.rs` + `Cargo.toml:19` `nasm-rs = "0.3"` ACTIVE dep — confirmed present
  this pass + companion comment `:15` + `build = "build.rs"` `:8` — + the 9 `checkasm_parity.rs`
  scalar sites), so the gate cannot ship RED-by-construction.

- **A4 — the P1↔`checkasm_parity.rs` build-soundness coupling is COSTED, not smuggled (ACCEPT,
  disk-verified exact).** Re-grepped this pass: `checkasm_parity.rs` carries exactly **9 ACTIVE**
  `bbnf_simd::x86_64::…::*_scalar(…)` call sites at **`:458,:464,:467,:477,:478,:484,:493,:497,
  :502`** (verbatim match to `a2:259`/§3) + 11 total `x86_64` tokens. `a2 §3` prices the
  `src/x86_64/` deletion as build-BLOCKING without decoupling these 9 sites, and makes
  `cargo build` AND `cargo test --no-run` clean the exit gate WITH the decouple in the SAME
  wave (no intermediate broken-build commit). A real cost surfaced and bounded — not an assumed
  one-line `rm -rf`. The 12 aarch64 single-kernel differentials + `checkasm_common.rs` are
  preserved (`a2:291-294`).

- **A5 — the 6 addenda add ZERO new code/hardcoding cost, and the ONE incidental new cost (R16)
  is surfaced + bounded + disk-confirmed VIABLE (ACCEPT).** Every addendum lens (a1 §L1–L6) is a
  grep/diff/md5/samply over the EXISTING benched tree — no code, no hardcoding added
  (`a1:567-568` "This registry adds ZERO new code or hardcoding — it is a binding lens
  specification. Every check is a grep/diff/md5/samply over the existing benched skinny tree").
  The G6 acceleration cost carries the same-wave-consumer discipline (`SYNTHESIS:69-70` "every
  primitive lands WITH its hot-path consumer in the same commit (no orphan kernel)") and the
  RETIRE branch is gated on a samply non-top-N MEASUREMENT, an in-plan S-P1 cost
  (`a1:489`,`:511`; `a0:229`), not an assertion. The R16 nested-struct gate-recipe fold names
  ONE incidental cost: realizing the structural P3 collapse-check via the cleanest sufficient
  mechanism (`RuntimeTarget: PartialEq`) adds **one derive line**. I re-verified this cost is
  genuinely VIABLE on disk this pass:
  - `xtask/src/regen.rs:5` carries only `#[derive(Clone, Copy, Debug)]` on `RuntimeTarget`
    (NOT `PartialEq`) — so the realization is exactly ONE added derive token.
  - `RuntimeTarget` is exactly **12 fields** (`regen.rs:7-18`); `frontend_requirements` is
    field #11 (line 17) and `output_labels` is field #12 (line 18) — matching the a3 §3 /
    SYNTHESIS §5 fact-5 "BOTH nested struct" recipe-pin.
  - BOTH nested-struct fields derive `PartialEq, Eq`: `RuntimeFrontendRequirements`
    (`grammar_provider.rs:45`) AND `RuntimeOutputLabels` (`grammar_provider.rs:91`); the
    `emitter` field's `RuntimeEmitterKind` also derives `PartialEq, Eq` (`:39`); every other
    field is `&'static str` / `&'static [&'static str]` / `Option<…>` (all `PartialEq`). So the
    one-line derive compiles and is the minimal sufficient mechanism — surfaced in all binding
    artefacts (`a2:393-400`, `a3:189-195`/`:209-211`, `SYNTHESIS:214-221`,
    `HARDENING-S-P0-CONSOLIDATED:76-78`), correctly priced as one line, not smuggled.

## REVISE (0) / REJECT (0)

The single V1 CH4 REVISE (R1-CH4) is still discharged across `SYNTHESIS §2.1` + `a0 §7` CH4-R1
fold + the alphaE feeder; the one new cost (R16 one-line `PartialEq` derive) is surfaced,
bounded, and disk-confirmed viable; every cost figure is disk-true; no un-satisfiable count
gate, no mis-budgeted LOC-keyed gate, no orphan REVISE.

## Tally

ACCEPT 5 · REVISE 0 · REJECT 0 — **100%**. Second-consecutive-cycle CH4 at 100% (V2 discharged
the V1 83.3% → R1-CH4; V3 independently re-confirms the discharge held). Every figure
re-measured this pass at HEAD `83b66db42` and disk-true: PRUNE −10800 (P1 4398 ≈ −4500 — 24
files/742 + 4 files/3554 + build.rs 102; P3 −5460 EXACT — 7×910, single md5; P2 ~−700),
cycle −12,650…−12,850, checkasm **14** (not 18), `json/generated.rs` 1235 LOC (±5% = ±62),
9 checkasm scalar sites `:458…:502`, `Cargo.toml:19` `nasm-rs="0.3"`, `RuntimeTarget` 12 fields
deriving `Clone, Copy, Debug`-only with both nested structs `PartialEq, Eq`. The R16 one-line
derive cost is surfaced + bounded + disk-confirmed viable; the G6 body ceiling carries a
same-wave-consumer gate + measurement-gated retire per §3W; no gate keys on an exact LOC figure.
Zero un-satisfiable gate, zero mis-budgeted gate, zero orphan REVISE.

TALLY accept=5 revise=0 reject=0
