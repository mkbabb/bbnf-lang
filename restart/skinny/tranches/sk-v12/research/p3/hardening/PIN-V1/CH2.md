# SK-V12 S-P3 CH2 - Generality / Lock 14

Cycle: PIN-V1.
Date: 2026-05-20.
Lens: CH2 grammar generality, Lock 14, generated non-JSON proof shape.
Verdict: REVISE.
Confidence: 94%.

## Scope

Reviewed the PIN-V1 S-P3 packet against `USER-PIN-W1-CSS-L4-SOTA.md`, the
six 2026-05-20 audits, and the current P3 artifacts. The worktree was clean
before this file was written, and no cargo/rustc/xctrace/samply process was
active.

## Blocking Finding

### CH2-1 - P3-B routes Sheets/BBNF fallback after the wrong wave

`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:98`
starts the fallback-order section correctly, but
`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:100`
through `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:102`
says Sheets/BBNF-self may enter only after **W2** records a measured CSS L4
redress attempt. W2 is the `escape_mask_64` correctness gate, not the CSS L4
redress wave. The user pin requires fallback only after a CSS L4 redress
attempt fails, and the packet otherwise binds that attempt to W1b:

- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:20` through
  `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:24` make CSS L4
  authoritative and Sheets/BBNF-self fallback-only after CSS redress.
- `restart/skinny/tranches/sk-v12/SPEC.md:67` through
  `restart/skinny/tranches/sk-v12/SPEC.md:69` require measured CSS evidence
  before fallback can be considered.
- `restart/skinny/tranches/sk-v12/SPEC.md:369` through
  `restart/skinny/tranches/sk-v12/SPEC.md:388` place that CSS attempt in W1b
  and require fallback to follow a recorded W1b BLOCKED/FAIL route.
- `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:235`
  through `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:241`
  also make fallback contingent on the CSS L4 redress record, not the W2
  correctness gate.

Impact: this is a Lock 14/generalization sequencing defect. It does not open
Sheets/BBNF early by itself, because SPEC and P3-C are correct, but P3-B is the
wave topological source and must not imply that an escape-mask wave can satisfy
the CSS-redress prerequisite.

## Passing Checks

- CSS L4 is consistently authoritative and the admission bar is the pin bar:
  `track1_mbps > lightningcss_mbps + 1`. See
  `restart/skinny/tranches/sk-v12/SPEC.md:39` through
  `restart/skinny/tranches/sk-v12/SPEC.md:52`,
  `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:18`
  through `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:23`,
  and `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:116`
  through `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:128`.
- The seven Lock 14 leaks are named from the audit and are made executable gate
  evidence, not prose. See
  `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md:63`
  through `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md:108`,
  `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:172`
  through `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:195`,
  and `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:145`
  through `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:162`.
- Generic crates are barred from grammar-name branches, generic JSON policy,
  new directives, BIR variants, `BackendShape` expansion, and public substrate
  APIs. See `restart/skinny/tranches/sk-v12/SPEC.md:219` through
  `restart/skinny/tranches/sk-v12/SPEC.md:223`,
  `restart/skinny/tranches/sk-v12/SPEC.md:257` through
  `restart/skinny/tranches/sk-v12/SPEC.md:273`, and
  `restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:60`
  through `restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:61`.
- CSS proof is benchmark/equality based. Hand-only witnesses, report-only
  rows, stale `sheets_witness`, and prose Lock 14 are pre-blocked. See
  `restart/skinny/tranches/sk-v12/SPEC.md:349` through
  `restart/skinny/tranches/sk-v12/SPEC.md:388`,
  `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:216`
  through `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:231`,
  and `restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:120`
  through `restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:131`.

## Required Fold

1. In `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:100`
   through `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:102`,
   replace the W2 fallback trigger with W1b / CSS L4 redress wording:
   Sheets and BBNF-self may enter only after **W1b** records a measured CSS L4
   redress attempt as BLOCKED or REJECTED.
2. After the fold, re-scan the S-P3 packet for `W2 records a measured CSS L4
   redress attempt`; the string should be absent.

No other CH2 fold is required in PIN-V1.
