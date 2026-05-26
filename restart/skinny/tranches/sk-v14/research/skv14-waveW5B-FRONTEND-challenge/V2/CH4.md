# SK-V14 W5B-FRONTEND CHALLENGE V2 CH4 Cost

Date: 2026-05-26.
Lens: CH4 Cost.
Disposition: REVISE.

## Findings

1. The plan creates four 30-minute W5B-internal sub-slices at
   `skv14-W5B-FRONTEND-plan.md:47`-`81` and still requires a final gate at
   `:126`-`:176`. W5B's wave cap remains <=90 minutes, not four 30-minute
   slices plus final verification, per `SPEC.md:244`, `SPEC.md:254`-`260`,
   `DISPATCH-PROMPT.md:85`, and `DISPATCH-PROMPT.md:211`.
2. W5B.0 is too broad for a 30-minute slice. It combines Lock 14
   routing/tests, import DAG resolution, missing/cycle failures, and public
   `@ws` rejection at `skv14-W5B-FRONTEND-plan.md:51`-`63`. Current routing
   only has W5A before fallthrough at `lock14_baseline.rs:1611`-`1622`, and
   current grammar support is a flat fact scan, not an import closure, at
   `grammar/src/lib.rs:141`-`164`, `:188`-`:210`, and `:320`-`:326`.
3. LOC enforcement omits redress/report paths even though the table budgets
   them. The `diff --numstat` command at `skv14-W5B-FRONTEND-plan.md:184`-`193`
   covers only source files, while SPEC counts named hand-written doc/result
   edits at `SPEC.md:254`-`258`.
4. Same-wave intent is stated at `skv14-W5B-FRONTEND-plan.md:17`-`20`, but the
   consumer wording weakens to "same redress commit set" at `:217`. SPEC and
   DISPATCH require same-commit consumer evidence at `SPEC.md:753`-`756` and
   `DISPATCH-PROMPT.md:221`-`223`.

## Required Fold

- Add an aggregate W5B budget proving all W5B.0-W5B.3 work plus final
  verification fits the W5B redress ceiling, or amend SPEC to formalize W5B
  sub-waves with an aggregate cap.
- Split W5B.0 or narrow it to one risk surface.
- Add redress report and `skinny/REDRESS.md` to LOC accounting when touched.
- Change "same redress commit set" to required same-commit closure, or state no
  sub-slice can land as accepted until the final same-commit consumer gate is
  present.
