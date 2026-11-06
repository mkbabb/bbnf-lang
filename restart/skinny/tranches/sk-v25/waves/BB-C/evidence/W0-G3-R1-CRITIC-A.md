# BB-C.W0 G3 deletion R1 — hostile critic A

Model: `gpt-5.6-sol`  
Reasoning: `ultra`  
Workflow: `v2`  
Role: blind read-only hostile critic A

Verdict: **NOT CLEAN; narrow deletion appears correct.**

The critic verified the four subject hashes in `W0-G3-R1-AUTHOR.md` and found:

1. **P1 — missing semantic planes.** `run_vm` reduces `ParseResult` to
   `(success, offset)`. The witness never observes `value`, the value's actual
   span/tag/children, or `diagnostics`. Cursor zero does not prove epsilon value
   or span semantics. This fails BB-A.W0 and BB-C.W0.
2. **P1 — no durable deletion census or born-RED receipt.** Static inspection
   found the implementation, export, registration, and positive tests gone, but
   no test enumerates `default_rules()` or proves the forbidden route absent.
   No immutable pre-fix failure or repeated post-fix receipt exists.
3. **P2 — stale route name.** Existing regression prose names
   `EliminateEpsilonInAlt` instead of the deleted `WrapOfEpsilonScalar` /
   `wrap-of-epsilon-scalar` route.
4. **P3 — residual API inputs.** `_ir` and `_rule_body_ids` remain unused in
   `default_rules`. This is not a semantic blocker; it requires deliberate API
   compatibility or later KISS removal.

Positive facts: production implementation/export/registration are absent; locked
focused universal, nullable, and left-recursion tests passed; `git diff --check`
passed. No wave-completion credit follows.

