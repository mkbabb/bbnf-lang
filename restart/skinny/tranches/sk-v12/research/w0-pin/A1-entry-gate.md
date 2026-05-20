# SK-V12 W0 PIN Research A1 - Entry Gate

Date: 2026-05-20.
Scope: read-only W0 entry and exit gate consistency under
`USER-PIN-W1-CSS-L4-SOTA.md`.
Verdict: PASS.

## Findings

W0 is dispatchable. `SPEC.md` marks S-P3 converged under the user pin and W0 as
the first dispatchable wave. `DISPATCH-PROMPT.md` gives W0 first dispatch
authority, and `HANDOFF.md` is `ready-for-wave-W0`.

The W0 exit gate is narrow:

- artifacts exist;
- run ids are current enough to prove no drift;
- JSON seed state is reconciled;
- no behavior/source drift exists.

W0 does not authorize parser, scanner, SIMD, codegen behavior, generated
runtime output, or benchmark behavior edits.

## Notes

Two wording issues are non-blocking:

- `run ids are current` must mean revalidated against the pin profile
  authority, not rerun, because W0 is explicitly revalidated rather than
  redone.
- W0 cap wording differs between the SPEC manifest and dispatch prompt; the
  dispatch prompt's phase caps bind this implementation wave.

The downstream fallback rule is intentionally stricter than generic fallback
wording: Sheets and BBNF-self remain blocked until W1b-2 records measured CSS
L4 lightningcss comparator/admission redress or the user re-pins.

## Sources

- `restart/skinny/tranches/sk-v12/SPEC.md`
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
