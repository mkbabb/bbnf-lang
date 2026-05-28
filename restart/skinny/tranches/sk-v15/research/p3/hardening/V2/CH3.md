# SK-V15 S-P3 V2 CH3 REGRESSION

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V2. Lens: CH3.
Date: 2026-05-28.
HEAD: `39e186ee3`.
Scope: audit the committed V2 S-P3 packet for wave-graph regressions,
delete-before-provider cycles, normalized pre-blocks, W11 anti-deferral, and
visible dependency-table consumption from final dispatch surfaces.

## Verdict

REVISE.

The V2 packet repairs the stale W0-W9 topology and preserves the main
delete-before-provider ordering, but it does not yet make the NEW-CH3-V5-01
dependency rows executable from `SPEC.md` plus `DISPATCH-PROMPT.md`. P3-B has a
compact dependency ledger, while SPEC has only a shortened rule table and
DISPATCH has only a pre-dispatch reminder. That is still too weak for CH3
because a wave agent can miss the authoritative retired artifact, retire wave,
provider wave, proof command, provider-no-later status, and pre-block cluster.

## Findings

| id | status | evidence | required follow-up |
|---|---|---|---|
| CH3-V2-01 | ACCEPT | The final wave graph is canonical W0..W11 in P3-B (`p3b-wave-sequencing.md:15-26`), P3-C (`p3c-falsifiability-gates.md:12-15`), SPEC dispatch lock (`SPEC.md:36-43`), and DISPATCH challenge range (`DISPATCH-PROMPT.md:57-63`). | Preserve W0..W11; do not reintroduce PRUNE/REBUILD aliases as dispatch ids. |
| CH3-V2-02 | REVISE | V1 required the dependency table schema and initial rows to land in final dispatch surfaces (`HARDENING-S-P3-V1-CONSOLIDATED.md:44`). The redeploy note specifies `row_id`, retired artifact, delete/retire wave, provider wave, proof command, provider-no-later status, consuming exits, and pre-block cluster (`DEPENDENCY-PREBLOCK-FOLD-NOTES.md:20-37`). SPEC now has only a four-column summary (`SPEC.md:176-186`), and DISPATCH only says to verify a dependency-table row (`DISPATCH-PROMPT.md:45-52`). | Promote the canonical dependency schema and initial rows into SPEC and either mirror them or point to them explicitly from DISPATCH before per-wave envelopes. Use V2 wave ids. |
| CH3-V2-03 | ACCEPT | The actual provider ordering no longer authorizes the old W2R/W4R cycle. W5 provides typed CSS output without retiring old proof (`p3b-wave-sequencing.md:56`), W6 performs retime and old-proof retirement only after W5 (`p3b-wave-sequencing.md:57`), SPEC blocks CSS provider/template deletion before W5/W6 proof (`SPEC.md:180-181`), and DISPATCH forbids provider deletion in W1 unless W5/W6-grade proof lands same wave (`DISPATCH-PROMPT.md:82-92`). | Preserve the split. The accepted ordering still needs the dependency rows from CH3-V2-02 to become enforceable. |
| CH3-V2-04 | ACCEPT | Pre-block coverage is now normalized, including `242-247` and FNV production migration, across P3-B (`p3b-wave-sequencing.md:101-104`), P3-C (`p3c-falsifiability-gates.md:313-328`), P3-E (`p3e-preblocked-ledger.md:32-37`), SPEC (`SPEC.md:414-431`), and DISPATCH (`DISPATCH-PROMPT.md:238-240`). | No CH3 follow-up. Keep the exact cluster list stable. |
| CH3-V2-05 | ACCEPT | W11 no longer uses SK-V16 as close evidence. P3-C requires PASS-IMPL V2 ACCEPT or row-level intrinsic-block proof and states SK-V16 routing is only routed remainder (`p3c-falsifiability-gates.md:293-311`); SPEC repeats that close rule (`SPEC.md:396-412`); DISPATCH aborts W11 on unresolved dependency rows, measurement repair, or implementation fixes (`DISPATCH-PROMPT.md:217-229`). | No CH3 follow-up. |
| CH3-V2-06 | REVISE | SPEC/DISPATCH do not visibly consume named dependency rows per wave. The redeploy note requires per-wave exit consumption and DISPATCH schema/mirror (`DEPENDENCY-PREBLOCK-FOLD-NOTES.md:77-103`), but SPEC wave sections do not name `DEP-*` rows and DISPATCH has no compact dependency table. `rg` finds no `DEP-W1-CSS-BROADCAST`, `DEP-W5-CSS-GENERATED-RS`, or `DEP-W8-FNV-QUARANTINE` in SPEC/DISPATCH. | Add per-wave "Dependency rows consumed" bullets or table entries for W1, W3, W4, W5, W6, W7, W8, W9, W10, and W11, rekeyed to V2 wave ids. |

## Required Follow-Up

1. Promote the canonical NEW-CH3-V5-01 dependency schema into
   `restart/skinny/tranches/sk-v15/SPEC.md` Section 2.1 and make
   `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` either mirror it or
   point to the SPEC table before any per-wave envelope.
2. Promote P3-B's compact rows into stable `DEP-*` rows using V2 wave ids:
   CSS broadcast demotion, CSS old-proof retirement, CSS provider/template
   deletion, Pattern H provenance/destructive delete, CSS legacy shim, Decision
   spine, W8/W9 lowerer scaffolds, W10 FNV quarantine, and W11 no-orphans.
3. Add per-wave dependency-row consumption to SPEC and DISPATCH exits. A wave
   that deletes, retires, demotes, or neutralizes an artifact must fail before
   redress if no matching dependency row exists.

## Verification

Commands run:

```sh
git rev-parse --short=9 HEAD
rg -n "DEP-W1-CSS-BROADCAST|DEP-W5-CSS-GENERATED-RS|DEP-W8-FNV-QUARANTINE" restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md || true
rg -n "row_id|retired_or_deleted_artifact|provider_lands_no_later|conditional_status|consuming_exit_gates|preblock_cluster" restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md || true
rg -n "W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11|W1-W11|W2, W3, W4, W5, W6, W7, W8, W9, and W10|242-247|FNV closed-enum production migration|SK-V16 routing" restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
```

Result: HEAD is `39e186ee3`; the first command returned no SPEC/DISPATCH
`DEP-*` rows; the second returned only the `measurement_row_id` substring false
positive in SPEC telemetry; the third confirmed W0..W11, normalized
pre-blocks, and anti-deferral wording are present.
