# SK-V15 W4 CHALLENGE V1

Input plan: `skv15-W4-plan.md` at commit `f12299cb0`.

Verdict: REVISE.

| Lens | Verdict | Finding |
|---|---|---|
| CH1 correctness | ACCEPT | Generator-owned final bytes plus non-writing comparison can satisfy W4. |
| CH2 generality | ACCEPT | Command dispatch is acceptable if projection data drives semantics. |
| CH3 regression | ACCEPT | CSS shim deletion stays blocked. |
| CH4 cost | REVISE | Plan lacked explicit W4 budget, estimates, caps, and block trigger. |
| CH5 hidden coupling | ACCEPT | Header-only close was rejected by byte-compare intent. |
| CH6 anti-paper-close | ACCEPT | Executable evidence was required in principle. |
| CH7 overfit-prune | REVISE | Check universe needed path-set equality, exact roots, projection/output-dir validation, and directory-specific headers. |

Required revisions: add cost/cap discipline; narrow owner roots to the 67
Pattern H include set; make no-arg `check-runtime` the close command; make
`--grammar` diagnostic only; require path-set equality, projection-set
validation, output-dir validation, directory-specific headers, and dirty-slice
staging checks.

Disposition: revised before implementation; dispatch V2 against the revised
plan.
