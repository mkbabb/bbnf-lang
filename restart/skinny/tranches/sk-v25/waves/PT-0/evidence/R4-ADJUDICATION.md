# PT-0 common harness R4 — terminal adjudication

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Canonical adjudicator: `/root/pt0_r4_adjudicator`.

Verdict: **REJECT R4 AS CURRENT PT-0 EVIDENCE; RETAIN NARROW STATIC
HARNESS CREDIT; FULL PT-0 CREDIT REMAINS ZERO; CAMPAIGN REMAINS ACTIVE.**

Frozen identities:

- author: `fcb9b81ccc478d6eb7ef95021cb8b9c182c3594c80c6d5b744996e08a224e611`
- Critic A: `6ace8ff232e54edb543abace2cebd4bcafa4b505dc7af1227afbd6013f672f73`
- Critic B: `4c4af675c34a54f9326f75d7c9439d5b90908d8a528bc88b1dfb6428f8f623d7`

Current verification exits `PT-0 profile receipt drifted`: the post-capture author
record is inside captured `docs/tranches/T`, so bundle `c072160f…` is not the
current baseline.

## Rulings

- Circular captured evidence: **correct**.
- `run.census` semantic binding defect: **correct**. Only its receipt digest is
  checked; duplicated repository/tool/config/profile summaries can be falsified
  and resealed.
- Exact parse-only profile and absence of configured Value operands: **correct**.
- Categorical ambient Value non-read: **unproven**. No actual Value access was
  demonstrated.
- Exact bundle membership/canonical bytes, no-replace member creation, seal-last
  ordering, modes, hashes, and 25/25 suite: **correct static facts**.
- Git census/tool identity: **incomplete**. Census uses bare ambient Git while the
  recorded payload is resolved separately afterward.
- No-x86 closure: **unproven, not disproved**. Native Apple ARM Node/Rust is
  established; the selected execution slice for universal Git is not.
- Process-kill and individual syscall fault coverage: **absent**. This limits the
  durability claim but proves no accepted corruption.
- Replace/execute/restore and ancestry-swap allegations: **unproven theoretical
  races**, not demonstrated defects.
- Ambient Git hooks causing reads: **unproven**.
- Full PT-0/current-bundle/package/product/performance/movement credit: **zero**.

## R5

1. Keep every current parse-that evidence byte frozen and place the R5 author,
   critics, and adjudication outside captured parse-that operands; do not weaken
   the complete tranche census merely to avoid circularity.
2. Delete duplicated `run.census`; the receipt hash is sufficient.
3. Resolve Git once and use that exact payload through an explicit Apple-arm64
   execution route for every census and version invocation.
4. Require config, profile, and package inputs to be contained regular files with
   non-symlink ancestry.
5. Claim only that Value is absent from declared operands and no R5 code accesses
   it; make no ambient-process universal claim.
6. Add child-process interruption witnesses after receipt and run publication;
   prove interrupted bundles are rejected without claiming power-loss or
   malicious same-user filesystem resistance.
7. Freeze all governed bytes, create one new unique bundle, verify it twice, then
   run two fresh critics and a fresh adjudicator from evidence stored outside the
   parse-that census.

This RED is a technical repair gate, never an ownership blocker and never a basis
for waiting on Claude.
