# CH2 GENERALITY - V5 Final Confirmation

Target packet: `77b6e9fd7` (`docs(sk-v15-t-p3): repair V4 citation finding`)

Verdict: ACCEPT

## Scope

CH2 audited the V5 packet for Lock 14 generality, JSON narrowing, concrete
non-JSON receivers, and forbidden directive/BIR/substrate/public API/sidecar/
sixth-shape additions. The target packet changes only
`restart/audit/totality/p3/3A-architecture-synthesis.md`, replacing the stale
V2 CH4 citation with the in-range `restart/audit/totality/p3/hardening/V2/CH4.md:36`
citation; no substantive generality text changes in this packet.

## Required Local Checks

- `git show --stat --oneline 77b6e9fd7 -- restart/audit/totality/p3`:
  `77b6e9fd7 docs(sk-v15-t-p3): repair V4 citation finding`; one file changed,
  `restart/audit/totality/p3/3A-architecture-synthesis.md`, 1 insertion and
  1 deletion.
- `git diff --check 77b6e9fd7^ 77b6e9fd7 -- restart/audit/totality/p3`:
  passed with no output.
- Extracted `3C-locks-v+1-diff.md` diff and checked with `git apply --check`:
  passed with no output. I used an equivalent pipe to `git apply --check -` to
  avoid creating a second file under the one-file ownership constraint.
- `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md`: `16`.
- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`: `67`.
- Required stale-pattern `rg` over 3A..3F: no matches.

## Findings

No CH2 defects found.

Lock 14 holds. The governing Lock 14 text still requires grammar source,
workspace metadata, and optional per-grammar declaration crates as the only
declarative inputs, while banning grammar-named generic branches, public generic
types, feature flags, and hand-written per-grammar runtime files
(`restart/locks/LOCKS.md:349`). It also requires provider manifests/facts rather
than JSON/CSS runtime branches (`restart/locks/LOCKS.md:368`-`375`) and
per-wave gates with included/excluded roots and non-JSON witnesses
(`restart/locks/LOCKS.md:377`-`390`). The V5 packet preserves this in 3C:
`D-L14-generated-provider-generalisation` keeps generic code on generated
manifests/facts and blocks JSON string/number APIs as CSS semantics
(`restart/audit/totality/p3/3C-locks-crystallisation.md:55`), and the proposed
diff carries the same full-surface gate/exclusion and source/metadata-only
onboarding language (`restart/audit/totality/p3/3C-locks-v+1-diff.md:60`).

No JSON narrowing. The authority says CH2 must keep 3A/3B generalised to
non-JSON, make 3E concrete for CSS L4 / Sheets / BBNF-self, and allow no 3C
JSON-narrowing amendment (`restart/prompts/totality/PASS-3-SYNTHESIS.md:108`-`111`;
`restart/prompts/totality/PASS-3-SYNTHESIS.md:212`). The target artifacts keep
JSON as scoped guard evidence only: 3A demotes CSS until typed provider and
same-workload retime proof (`restart/audit/totality/p3/3A-architecture-synthesis.md:64`),
3D says JSON is not CSS or arbitrary-grammar closure
(`restart/audit/totality/p3/3D-skinny-fold.md:50`), and 3E explicitly states
that it proposes no JSON narrowing (`restart/audit/totality/p3/3E-grammar-generalisation.md:50`-`53`).

Concrete non-JSON receivers are present. 3E makes CSS L4 the positive
non-JSON receiver only after scoped typed value/document/view/visitor output and
same-workload `cssparser` retime, then requires Sheets and BBNF-self as
negative-control falsifiers for manifests, shape facts, and primitive policies
(`restart/audit/totality/p3/3E-grammar-generalisation.md:35`-`48`). The receiver
matrix requires CSS plus Sheets or BBNF-self for generic surface claims
(`restart/audit/totality/p3/3E-grammar-generalisation.md:68`), then spells out
Sheets and BBNF-self onboarding fixtures (`restart/audit/totality/p3/3E-grammar-generalisation.md:74`-`76`)
and the non-JSON proof matrix across provider, CSS typed provider,
BackendShape, primitive policy, and future onboarding surfaces
(`restart/audit/totality/p3/3E-grammar-generalisation.md:80`-`89`). 3D mirrors
the same bridge from JSON guard evidence to CSS plus Sheets/BBNF-self
generality (`restart/audit/totality/p3/3D-skinny-fold.md:67`).

No forbidden directive, BIR, substrate, public API, retained sidecar, or
sixth-shape addition is introduced. The dispatch boundary requires preserving
16 locks and five `BackendShape` variants, with any new lock, directive, BIR
variant, public substrate API, retained sidecar, or sixth shape G-Omega-gated
(`restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:72`-`74`). 3A repeats that
boundary (`restart/audit/totality/p3/3A-architecture-synthesis.md:32`-`35`),
3C states the proposed addendum creates none of those forbidden surfaces
(`restart/audit/totality/p3/3C-locks-crystallisation.md:31`), and the extractable
diff preserves the exact five variants while barring new directive/BIR/shape
shortcuts (`restart/audit/totality/p3/3C-locks-v+1-diff.md:40`,
`restart/audit/totality/p3/3C-locks-v+1-diff.md:56`). FactStream also remains an
output-plane/admitted-product classification, not a sixth `BackendShape` or
retained sidecar (`restart/locks/LOCKS.md:100`-`109`;
`restart/audit/totality/p3/3C-locks-v+1-diff.md:42`).

## Verdict

ACCEPT. CH2 finds no Lock 14 generality defect, no JSON-only narrowing, no lack
of concrete non-JSON receivers, and no forbidden surface addition in target
packet `77b6e9fd7`.
