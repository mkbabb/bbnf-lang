# SK-V11 W3-R5: Numeric Pre-Blocked Ledger

Date: 2026-05-20.
Scope: numeric REDRESS and pre-block ledger for W3.
Output: this file.

## §1 — Findings

- REDRESS 80 is the binding numeric rejection: canada had zero mantissa
  overflows, zero ambiguous Eisel-Lemire returns, and zero `str::parse::<f64>()`
  fallbacks, so the fallback rate was 0.0000% (`skinny/REDRESS.md:2217`).
- REDRESS 31 already rejected direct `raw.parse::<f64>()` after canada parity
  mismatch (`skinny/REDRESS.md:378`).
- REDRESS 39 closed the missing materializer gap with shared Eisel-Lemire
  i64/u64/f64 support, without taking the raw-parse shortcut
  (`skinny/REDRESS.md:520`).
- REDRESS 46 improved numeric direct rows but rejected another local digit-scan
  round as the SOTA close; `numbers` lifted, while `canada`, `mesh`, and
  `marine_ik` stayed red (`skinny/REDRESS.md:633`).
- SK-V10 close remained `N-direct / NoGo` as a measured state, not an open
  implementation task (`skinny/REDRESS.md:3268`).
- SK-V10 parse-only firewall remains binding; parse-only rows stayed
  `S / NO-GO` and validator rejects parse-only SOTA movement
  (`skinny/REDRESS.md:3040`).
- W3 is only the numeric direct closure slice, with selected generated
  direct/typed numeric consumers and one or two target rows unless microbench
  evidence justifies more (`SPEC.md:434`).
- W3 explicitly forbids f64 fallback, mantissa table, sign/exponent/suffix/
  conversion policy changes, and pre-blocks REDRESS 80, generic number policy,
  parse-only numeric evidence, and W0-clamp admission without measured
  provenance (`SPEC.md:466`, `SPEC.md:485`).
- P3-E names REDRESS 31, 39, 46, and 80 as W3 pre-blocks; digit microkernels
  require same-wave product row movement
  (`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:172`).

## §2 — Recommendations

- Frame W3 around digit-span or number-slot emission consumed by generated
  direct/typed numeric rows with unchanged semantics.
- Require generated Track 1, independent Track 2/oracle, strict same-run sonic
  direct comparator, floor clearance, REDRESS provenance, and same-wave
  `gate-json` consumption for row movement.
- Treat `instruments`, `numbers`, and `unicode_mixed` as W0-clamped planning
  rows until a behavior wave supplies measured provenance (`SPEC.md:31`).

## §3 — Risks

- Do not reopen f64 fallback, mantissa widening, generic number/JSON policy, W0
  clamp bypass, parse-only numeric evidence, primitive-only UDOT/digit
  microbench claims, or table-only numeric edits.
- SPEC §13 hard-blocks parse-only admission, generic JSON policy, numeric
  fallback/mantissa/f64 policy rewrites, primitive-owned number grammar, and
  PMU/cycles/structural-scan facts as behavior producers (`SPEC.md:773`).
- W3 may proceed after W2 only as direct-plane work, carrying W2's blocked
  non-JSON route into close (`HANDOFF.md:91`).

## §4 — Sources

- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v10/research/close/close-redress.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`
