# S-P0 audit-overfit hardening V1 — CH4 Cost

Lens: the audit's LOC figures, the prune budget, the addenda's incremental cost, and the
wave-cost framing are accurate, disk-true, and do not seed an un-satisfiable or mis-budgeted
gate. CH4 verifies (1) the residual audit is COST-COMPLETE (every pruned/added surface is
LOC-accounted), (2) the 6 addenda are executable at ZERO new cost and carry no un-satisfiable
count gate, (3) the PRUNE-sequencing's costs (build-soundness coupling, capped additions) are
budgeted, not smuggled.

Live HEAD `83b66db42`; every figure below re-measured on disk this pass.

## Dispositions (ACCEPT)

- **A1 — PRUNE net LOC ≈ −10800 (ACCEPT, disk-verified).** Re-measured each prune surface:
  P1 = `src/x86_64/` 742 LOC (24 files) + `ext/x86/` 3554 LOC (4 files) + `build.rs` 102 LOC ≈
  −4398 ≈ the cited −4500; P3 = 6 × 910 LOC (collapse-to-one of the 7 byte-identical css_l4
  `generated.rs`, each 910 LOC, all md5 `b654562c…`) = −5460 EXACT; P2 ~−700 (partial gut of
  the 3737-LOC `nonjson_css_l4.rs`, KEEP oracle+canon); P4 +~15; P5 rename-only. Sum
  ≈ −10560…−10800 — the cited figure is accurate, if anything understated (matches
  `alphaE:108` "disk-recompute = −10600…−10800, if anything understated"). A REDUCTION; no
  `[generated-size-budget]` overflow risk.

- **A2 — the two net-LOC figures are SCOPE-DISTINCT and both correct (ACCEPT).** a0 §0 cites
  "net LOC ≈ −12,650…−12,850" (the WHOLE-CYCLE figure: PRUNE −10800 + B1 −800 + B2 −1500 +
  B3 ±0 + B4 +250, `alphaE:227`); a2:173/SYNTHESIS-AUDIT-OVERFIT:150 cite "−10800" labeled
  "PRUNE net LOC" (the PRUNE cluster ALONE). Both trace to the alphaE source and each carries
  the scope it measures — there is no contradiction (a0 measures the cycle's net deletion; a2
  measures the prune cluster). Non-blocking clarity note, not a cost defect: the two figures
  could be cross-referenced once, but each is correctly labeled.

- **A3 — no un-satisfiable count gate (ACCEPT, disk-verified).** The checkasm count is the
  disk-true **14** (12 single-kernel `checkasm_<kernel>.rs` + `checkasm_common.rs` +
  `checkasm_parity.rs`, all enumerated this pass), NOT the stale "18" — a2 §3 + SYNTHESIS §2
  carry 14 and explicitly correct the slip. No prune/addendum gate keys on a count that cannot
  be met on a clean tree. P1's gate is satisfiable-BY-CONSTRUCTION: the deletion list (a)–(i)
  is reach-matched to the verify grep (24+4 files + `build.rs` + `Cargo.toml:19` nasm-rs dep +
  the 9 `bbnf_simd::x86_64::…::*_scalar` sites in `checkasm_parity.rs`, all confirmed live), so
  the gate cannot ship RED-by-construction.

- **A4 — the P1↔checkasm_parity build-soundness coupling is COSTED, not smuggled (ACCEPT).**
  a2 §3 correctly prices the `src/x86_64/` deletion as build-BLOCKING without decoupling the 9
  active call sites, and makes `cargo test --no-run` clean the exit gate WITH the decouple in
  the SAME wave (no intermediate broken-build commit). This is a real cost surfaced and bounded,
  not an assumed one-line `rm -rf`.

- **A5 — the 6 addenda add ZERO new cost (ACCEPT).** a1 §"Honest residual note" + SYNTHESIS
  §2.1 are correct: every lens is a grep/diff/md5/samply over the EXISTING benched tree — no
  code, no hardcoding added. R16's nested-`output_labels` recipe pin is a SPEC clarification for
  S-P3 (changes how the gate is COMPUTED, not its LOC) — cost-free. G6's retire branch is gated
  on a samply MEASUREMENT (an S-P1 cost already in the plan), and the G6 BODY ceiling is
  committed+capped at +~150 per real NEON body (`alphaE:211`, one PMULL body, others
  retired/relabeled unless a same-wave consumer exists) — no hidden cost.

## REVISE (1)

- **R1-CH4 — the G1 JSON-projection ±5% line-count tripwire must be stated as SOFT, the
  binding cost-control as the oracle diff-match.** `alphaE:128` binds "regen MUST show
  `json/generated.rs` line-count within ±5% of today's" AND self-flags "true LOC unknown until
  written" and "Softest budget in the shortlist." Today's `json/generated.rs` = 1235 LOC, so
  ±5% = ±62 LOC. A faithful projection that legitimately reorders/dedupes/renames
  (`parse_w11_1_number`→`parse_number`, P5) could exceed ±5% while remaining a true projection —
  so a HARD ±5% reject would be a mis-budgeted gate. SYNTHESIS-AUDIT-OVERFIT §2.1 item 1 already
  folds the correct framing ("the ±5% line-count is a SOFT tripwire only … the BINDING proof is
  the diff-match against the `json_templates/` byte-for-byte oracle BEFORE deletion"), and the
  SK-V18 SYNTHESIS G1 row (`:331`/`:377`) binds the oracle diff-match as primary. The residual:
  the soft framing is folded only in §2.1, while the ±5% survives un-qualified in the alphaE
  feeder. Non-blocking, single-edit: state the ±5% as a soft tripwire and the oracle
  byte-for-byte diff-match as the binding cost-control wherever the G1 budget is cited.

## Tally

ACCEPT 5 · REVISE 1 · REJECT 0 — **83.3%**. Zero REJECT. Every cost figure (−10800 PRUNE,
−12,850 cycle, checkasm 14, P1 −4500, P3 −5460) is disk-true at `83b66db42`. No un-satisfiable
gate, no un-budgeted residual, no hidden cost in the 6 addenda. The single REVISE softens a
carried LOC tripwire to its binding form (oracle diff-match) — single-edit, non-architectural.

TALLY accept=5 revise=1 reject=0
