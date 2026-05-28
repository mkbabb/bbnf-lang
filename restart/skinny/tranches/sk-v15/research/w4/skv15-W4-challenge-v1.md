# SK-V15 W4 CHALLENGE V1

Input plan: `restart/skinny/tranches/sk-v15/research/w4/skv15-W4-plan.md`
at commit `f12299cb0`.

## Verdict

REVISE. The plan direction is accepted, but redress cannot begin until the
cost/cap block, check universe, and fail-closed scans are explicit.

| Lens | Verdict | Finding |
|---|---|---|
| CH1 correctness | ACCEPT | Generator-owned final bytes plus non-writing byte comparison satisfies `DEP-W4-PATTERN-H-PROVENANCE` if implemented exactly. Root runtime deletion and CSS shim removal remain forbidden. |
| CH2 generality | ACCEPT | `check-runtime` is command dispatch over projection data, not a new semantic grammar-family layer, if implementation avoids extra grammar switches beyond projection selection. |
| CH3 regression | ACCEPT | The plan keeps `LegacyPath` / `LegacySegment` routed and blocked rather than deleting the CSS shim before W5/W6 typed proof. |
| CH4 cost | REVISE | The plan must quote W4 risk, manual LOC, generated-output status, docs LOC, phase caps, estimate, and intrinsic-block trigger. |
| CH5 hidden coupling | ACCEPT | Header-only closure is rejected by the non-writing byte comparison, provided the check compares final header-bearing bytes after formatting. |
| CH6 anti-paper-close | ACCEPT | W4 close requires executable evidence, not docs or write-command output alone. |
| CH7 overfit-prune / gate-exclusion | REVISE | The plan must require path-set equality, exact include/exclude roots, projection-set validation, output-dir validation, and header mapping by owning directory. |

## Required Revisions

- Add a `Cost / Cap Discipline` section with SPEC W4 budgets and the redress
  cap.
- Narrow the runtime owner set to the 67 Pattern H include roots and exclude
  root/shared runtime files.
- Make no-argument `check-runtime` the required close command. Treat
  `check-runtime --grammar ...` as diagnostic only.
- Require `check-runtime` to compare expected generated path set against the
  actual Pattern H path set, not only the emitted file list.
- Validate runtime projection files against workspace grammar metadata and
  reject missing or extra root runtime projections.
- Validate every projection `output_dir` against its expected runtime module
  directory.
- Make header validation directory-specific so `math/*.rs` cannot carry the
  `regen-css` header.
- Add pre/post dirty-state and staged-slice checks for unowned files.

## Disposition

Plan has been revised in this commit before redress. Dispatch CHALLENGE V2
against the revised plan before editing implementation files.
