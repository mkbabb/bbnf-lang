# SK-V10 W9 CHALLENGE Consolidated

Pass: Wave CHALLENGE.
Cycle: W9.
Date: 2026-05-19.
Plan reviewed: `restart/skinny/tranches/sk-v10/research/w9/w9-plan.md`.
Disposition: ACCEPT measured-rejection plan.

## CH1 Correctness

ACCEPT. The plan correctly distinguishes W8 proof closure from W9 production
authority. `unescape_string` already calls the x4 helper on aarch64, so W9
cannot satisfy "named production caller consumes the primitive in the same
commit" by restating the existing call graph.

## CH2 Generality / Lock 14

ACCEPT. The plan avoids moving JSON escape policy into `bbnf-simd` or other
generic crates. A replacement production delta would need a fresh proof because
the W8 proof is exact to the current `unescape_string` caller and fixed-width
Unicode escape semantics.

## CH3 Regression / REDRESS

ACCEPT WITH WATCHPOINTS. The redress phase must run parity and fresh targeted
direct measurements for `unicode_escapes` and `y_string_unicode`. If the
measurements unexpectedly clear Section 0.2 floors, W9 may update the plan
before redress commit. Otherwise it must record a rejection without touching
`RESULTS.md`.

## CH4 Cost

ACCEPT. A no-op wrapper, feature re-gate, or constant would add surface area
without changing the production path. The cost is unjustified and would weaken
the micro-prove-first discipline.

## CH5 Hidden Coupling

ACCEPT. The rejected alternatives couple W9 to either W7's failed full-string
proof or a direct/typed output contract not covered by W8. The accepted plan
keeps W8 as proof-only and leaves any segment-fold or output-materializer work
to a future SPEC/CHALLENGE route.

## CH6 Anti-Paper-Close

ACCEPT. Claiming same-wave integration for a pre-existing call would be a
paper close. The challenge explicitly bars cosmetic source edits whose only
purpose is to satisfy wording. W9 must either find a real source delta before
redress or reject with measurement.

## Final Disposition

The plan is ACCEPTED. Proceed to redress as a measured rejection unless a real,
non-no-op source delta is discovered before implementation. Required redress
evidence:

- primitive parity;
- strict checkasm parity;
- `parse-that-regex` unescape policy tests;
- targeted direct Criterion capture for `unicode_escapes` and
  `y_string_unicode`;
- REDRESS entry stating that W8 remains accepted proof-only evidence and W9
  moves no `RESULTS.md` rows.
