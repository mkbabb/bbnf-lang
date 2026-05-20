# SK-V11 S-P3 V4 CH3: Regression, REDRESS, And Pre-Blocks

Pass: S-P3 CHALLENGE. Cycle: V4.
Date: 2026-05-20.
Scope: CH3 only. This stability challenge verifies that the V4 packet preserves
the V3-accepted regression/REDRESS surface: guard floors, REDRESS pre-blocks, W3
retirement, rejected-family boundaries, and measurable failed/fixpoint outcomes.
Output: this file.
Disposition: ACCEPT.

## Standard

CH3 accepts only if the V4 packet preserves the V3 semantics accepted by CH3 and
does not reopen a REDRESS route under a narrower name. The governing standard is
the S-P3 CH3 lens: the pre-blocked ledger must enumerate routes each wave must
not reopen, no wave may silently reopen a pre-blocked route, and SPEC must carry
the full pre-block list (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:122-126`).
V4 is a stability cycle because V3 consolidated at 6/6 ACCEPT with no open
critical defects or REVISE, but needed one more clean cycle under §3Z's two-cycle
rule (`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:19-23`,
`restart/prompts/ORCHESTRATOR.md:118-123`).

## Findings

1. V4 is the intended stability packet, not a new source or wave-dispatch
   surface.

   P3-A through P3-F are all marked Cycle V4, and SPEC is a V4 draft
   (`p3a-candidate-shortlist.md:3`, `p3b-wave-sequencing.md:3`,
   `p3c-falsifiability-gates.md:3`, `p3d-telemetry-schema.md:3`,
   `p3e-preblocked-ledger.md:3`, `p3f-spec-draft.md:3`, `SPEC.md:3-7`). SPEC
   still authorizes no behavior source change until S-P3 converges and a specific
   wave entry gate passes (`SPEC.md:9-12`). This matches the V3 consolidation
   instruction: preserve V3 semantics and rerun the six lenses for the second
   clean cycle (`HARDENING-S-P3-V3-CONSOLIDATED.md:45-47`).

2. Guard floors are preserved exactly.

   V3 CH3 accepted the corrected direct guard floors
   `18191/17431`, `11028/9996`, `8759/9248`, `2253/2182` and typed floors
   `17385/15593`, `29928/17321`, `8308/6754`, `11633/12029`,
   `11613/10150`, `9214/7739`, `11552/9894`
   (`V3/CH3-regression-redress.md:33-52`). V4 carries the same values in P3-A
   (`p3a-candidate-shortlist.md:43-67`), P3-C (`p3c-falsifiability-gates.md:128-159`),
   and SPEC (`SPEC.md:136-161`). Behavior gates and close gates also continue to
   fail closed on guard misses: W3-W7 include guard-floor exit or revert
   conditions (`SPEC.md:465-479`, `SPEC.md:518-530`, `SPEC.md:569-585`,
   `SPEC.md:623-638`, `SPEC.md:672-685`), and W8/W9 require existing direct and
   typed guards before fixpoint or close (`SPEC.md:723-732`, `SPEC.md:755-765`).

3. REDRESS pre-blocks remain load-bearing in both ledger and SPEC.

   P3-E's V4 ledger maps wave families to blocked routes, candidate surfaces to
   REDRESS entries, hard blocks, material-differential-only routes, and proof-only
   surfaces (`p3e-preblocked-ledger.md:31-63`, `p3e-preblocked-ledger.md:64-97`).
   Its per-wave section carries the route-specific blocks into W0-W9, including
   W3 numeric, W4 dispatch/control, W5 string, W6 escape, W8 fixpoint, and W9
   close (`p3e-preblocked-ledger.md:172-212`). SPEC then repeats hard pre-blocks
   for W3 substrate, parse-only movement, sidecars, direct/typed proof confusion,
   generic JSON policy, PMULL/CTZ, string materialization, numeric fallback, and
   object/value-byte carry (`SPEC.md:772-803`). This is the same CH3 authority
   V3 accepted, now preserved in the V4 packet.

4. W3 union/event/class-column retirement is preserved, not renamed.

   REDRESS 96 measured the class-column plus move-consumed structural-index W3
   implementation; correctness was green but every W3 target and maintain row
   failed (`skinny/REDRESS.md:2797-2848`). REDRESS 97 measured an allocation-free
   streaming cursor variant; it also missed every W3 target and maintain row
   (`skinny/REDRESS.md:2852-2906`). REDRESS 98 retired
   `G-W3-UNION-SUBSTRATE`, including class-lane-only and W4 cascade-lock
   routes (`skinny/REDRESS.md:2910-2940`). REDRESS 102 firewalled parse-only
   movement and found no live dispatch route through W3 substrate, retained class
   columns, `UnionTape`, cursors, projections, or W4-through-W3 dependency
   (`skinny/REDRESS.md:3042-3058`). V4 preserves that in SPEC close conditions
   (`SPEC.md:36-41`), non-negotiables (`SPEC.md:163-183`), P3-B global
   pre-blocks (`p3b-wave-sequencing.md:123-138`), P3-D telemetry rejection
   (`p3d-telemetry-schema.md:215-218`), and P3-E hard blocks
   (`p3e-preblocked-ledger.md:214-222`).

5. Rejected numeric, PMULL/CTZ, string/Unicode, and object/control families stay
   bounded by measured REDRESS facts.

   Numeric fallback remains blocked because REDRESS 80 found zero mantissa
   overflows, zero ambiguous Eisel-Lemire returns, zero `str::parse::<f64>()`
   fallbacks, and no measured fallback pool (`skinny/REDRESS.md:2217-2227`).
   V4 turns that into W3 and hard-ledger blocks (`SPEC.md:473-486`,
   `SPEC.md:792-793`, `p3c-falsifiability-gates.md:222-229`).

   PMULL and CTZ remain rejected as default production hot paths. REDRESS 88
   passed primitive/checkasm evidence but regressed JSON parse rows such as
   `numbers/track1_generated -10.04%` and `unicode_escapes/track1_generated
   -12.66%` (`skinny/REDRESS.md:2510-2540`). REDRESS 89 kept prefix-XOR scalar
   and narrowed to CTZ/bulk, but six Track 1/2 rows still violated the maintain
   invariant (`skinny/REDRESS.md:2544-2585`). V4 only allows these as support
   after a complete source delta, scalar oracle, parity, same-wave consumer, and
   row gate (`p3e-preblocked-ledger.md:86-87`, `p3e-preblocked-ledger.md:221-222`,
   `SPEC.md:788`).

   String and Unicode families remain bounded. REDRESS 60-62 rejected retained
   boundary collapse and wide/delayed-wide scanner routes (`skinny/REDRESS.md:1346-1488`);
   REDRESS 66-69 rejected direct source-hook, parser-owned scratch, byte-output,
   and semantic string fact routes (`skinny/REDRESS.md:1688-1884`); REDRESS 82
   and 83 rejected single-quartet and `StringBlock16` wrappers after correctness
   or parity passed (`skinny/REDRESS.md:2287-2355`); REDRESS 106-108 keep the
   full-string proof and x4 proof-to-production path blocked unless there is a
   new source delta and product consumer (`skinny/REDRESS.md:3152-3222`). V4
   carries those facts into W5/W6 and the hard ledger (`SPEC.md:590-591`,
   `SPEC.md:643-644`, `SPEC.md:789-791`, `p3e-preblocked-ledger.md:184-194`).

   Object/key/value-byte carry also stays bounded. REDRESS 63 admitted only the
   narrow generated JSON array carry and kept it grammar-shaped, not generic
   policy (`skinny/REDRESS.md:1492-1501`); REDRESS 65 rejected object next-key
   carry after same-row falsification (`skinny/REDRESS.md:1639-1681`); REDRESS 84
   rejected object-pair value-byte compaction and forbade compensating by
   reopening object next-key carry, separator elision, generic SWAR whitespace,
   or EventCursor sidecars (`skinny/REDRESS.md:2360-2395`). V4 maps those to W4
   and hard-ledger blocks (`SPEC.md:535-536`, `SPEC.md:794-795`,
   `p3e-preblocked-ledger.md:58-61`).

6. Failed waves and fixpoint outcomes remain measurable, not prose close.

   SPEC's close condition still requires every residual direct row to become
   strict same-run `A / GO` on generated Track 1 plus independent Track 2/oracle,
   or receive a per-row uncloseable REDRESS proof with measurement
   (`SPEC.md:26-29`). P3-C's W8 gate requires attempted intervention, Track 1,
   Track 2, sonic direct Mbps, floor, guard result, exhausted route, admitted
   non-JSON intervention, and all guard floors (`p3c-falsifiability-gates.md:86-87`).
   P3-D makes the necessary fields gate-consumed, including comparator source,
   measured validation path, REDRESS entry, wave id, run id, same-wave consumer,
   and Track 2 independence (`p3d-telemetry-schema.md:98-148`,
   `p3d-telemetry-schema.md:229-232`). SPEC W8 and W9 preserve the same rule
   (`SPEC.md:715-732`, `SPEC.md:757-765`). This matches REDRESS 110's close
   accounting pattern: close is documentation-only, lists every wave disposition,
   uses final measured report authority, and passes `gate-json` plus
   `git diff --check` (`skinny/REDRESS.md:3259-3277`).

## Accepted Facts

- V4 keeps the V3-accepted guard floor authority intact across P3-A, P3-C, and
  SPEC.
- P3-E and SPEC preserve the REDRESS pre-block ledger and carry it into wave
  gates rather than leaving it as advisory prose.
- W3 union/event/class-column, streaming cursor, class lane, `UnionTape`, sidecar
  substrate, parse-only movement, and W4-through-W3 cascade lock remain retired.
- Numeric fallback/mantissa widening, PMULL/CTZ default rewires,
  retained/string/Unicode materialization replays, proof-only x4 promotion,
  already-wired `unescape_string` reuse, object next-key carry, and value-byte
  compaction remain blocked unless a future wave writes the required material
  differential before redress.
- Failed rows, failed waves, and residual fixpoint outcomes remain measurable and
  gate-consumed.

## Final Judgment

ACCEPT. The S-P3 V4 packet satisfies CH3 as a stability cycle. It preserves the
V3 regression/REDRESS semantics, keeps guard floors exact, keeps REDRESS
pre-blocks executable, keeps W3 retired and parse-only firewalled, retains
rejected-family boundaries, and requires failure/fixpoint outcomes to close only
through measured REDRESS evidence.
