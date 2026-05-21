# T-P3 V2 CH1 Correctness

Verdict: REVISE.

## Scope

CH1 reviewed the V2 T-P3 artifacts for correctness regressions introduced by
the V1 fold: stale cycle authority, contradiction with current dispatch state,
false current-state claims, and source-map gaps. The V1 consolidated record
required V2 to fold the CH4/CH6 revise set and rerun challenge on a revised
packet, not to continue presenting body text as V1-cycle output
(`restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:32`-`53`,
`restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:57`-`58`).

## Evidence

The substantive V2 repair surfaces are present. 3A adds a per-delta cost and
routing ledger plus receiver/blocker/gate open questions
(`restart/audit/totality/p3/3A-architecture-synthesis.md:59`-`85`). 3B adds
numeric propagation and receiver gates for MP-3B-D1 through MP-3B-D9
(`restart/audit/totality/p3/3B-master-plan-reconciliation.md:148`-`173`). 3C
adds a cost/disposition ledger and makes ACCEPT/MODIFY lock-text dispositions,
not implementation admissions
(`restart/audit/totality/p3/3C-locks-crystallisation.md:117`-`143`). 3D, 3E,
and 3F add the missing receiver-bound ledgers, including the D06 generated
fixture receiver and the Omega CRUD/G-Omega routing question
(`restart/audit/totality/p3/3D-skinny-fold.md:102`-`128`,
`restart/audit/totality/p3/3E-grammar-generalisation.md:158`-`181`,
`restart/audit/totality/p3/3F-migration-handoff.md:167`-`193`). The proposed
LOCKS diff also keeps the G-Omega boundary explicit and does not grant source
authority from proposed text (`restart/audit/totality/p3/3C-locks-v+1-diff.md:12`-`16`,
`restart/audit/totality/p3/3C-locks-v+1-diff.md:398`-`415`).

The packet still contains stale cycle claims that contradict its V2 metadata.
3A declares `cycle: V2` but says the empty prior-cycle state exists "because
this is the V1 artifact" (`restart/audit/totality/p3/3A-architecture-synthesis.md:4`,
`restart/audit/totality/p3/3A-architecture-synthesis.md:27`). 3B declares
`cycle: V2` but its carried row says "this is 3B V1"
(`restart/audit/totality/p3/3B-master-plan-reconciliation.md:4`,
`restart/audit/totality/p3/3B-master-plan-reconciliation.md:29`). 3E declares
`cycle: V2` but says "This is T-P3 V1 for agent 3E"
(`restart/audit/totality/p3/3E-grammar-generalisation.md:4`,
`restart/audit/totality/p3/3E-grammar-generalisation.md:44`). These are not
harmless V1-surface labels; they are cycle/current-artifact claims and would
mislead the G3 source map about whether V1 revise feedback has actually been
folded.

One source-map hygiene issue remains in 3C. The executive summary cites bare
`PASS-3-SYNTHESIS.md` references instead of the resolved path, while the same
artifact later uses the fully qualified
`restart/prompts/totality/PASS-3-SYNTHESIS.md` form
(`restart/audit/totality/p3/3C-locks-crystallisation.md:23`,
`restart/audit/totality/p3/3C-locks-crystallisation.md:107`). The propagation
paragraph repeats the bare form
(`restart/audit/totality/p3/3C-locks-crystallisation.md:115`). CH1 does not
find a missing underlying file, but V2 should normalize these to the resolved
path before G3 so every authority citation is unambiguous.

No contradiction was found in the G-Omega/CRUD sequencing after the V2 fold.
3F records the tension and routes pre-G-Omega CRUD as proposed diffs/logs while
authoritative merge waits for user G-Omega
(`restart/audit/totality/p3/3F-migration-handoff.md:135`-`145`,
`restart/audit/totality/p3/3F-migration-handoff.md:185`-`193`), which matches
the Pass Omega sign-off contract (`restart/prompts/pass-contracts/PASS-OMEGA.md:92`-`110`).

## Required Revisions

1. Replace the stale cycle claims in 3A, 3B, and 3E with V2-accurate wording:
   "no prior accepted T-P3 cycle is carried" or equivalent. Keep "V1 surface"
   only when it names the target totality surface, not the artifact cycle.
2. Normalize 3C's bare `PASS-3-SYNTHESIS.md` citations to
   `restart/prompts/totality/PASS-3-SYNTHESIS.md`.
3. Rerun CH1 after those corrections; no additional CH1 revisions are required
   for the V2 cost/routing ledgers.
