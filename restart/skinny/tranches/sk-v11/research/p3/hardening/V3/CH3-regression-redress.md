# SK-V11 S-P3 V3 CH3: Regression, REDRESS, And Pre-Blocks

Pass: S-P3 CHALLENGE. Cycle: V3.
Date: 2026-05-20.
Scope: CH3 only. This challenges whether the V3 packet reopens REDRESS routes,
weakens guard floors, leaks through W3 union/firewall blocks, relabels rejected
PMULL/CTZ/string/numeric families, or paper-closes failed/per-row outcomes.
Output: this file.
Disposition: ACCEPT.

## Standard

CH3 accepts only if the V3 packet satisfies the S-P3 regression obligations:

1. The pre-blocked REDRESS ledger names the rejected routes each wave must not
   reopen, and the SPEC carries those blocks into wave gates.
2. Existing admitted direct and typed rows have exact guard floors and cannot
   silently demote during residual-row reclamation.
3. W3 union/event/class-column substrate and parse-only movement stay retired,
   not renamed as dispatch/control, telemetry, or fixpoint work.
4. Rejected PMULL/CTZ, string, Unicode, and numeric families remain rejected
   unless a wave names a material differential, scalar/oracle proof, same-wave
   consumer, and row gate before redress.
5. Failed waves and uncloseable residual rows are measurable outcomes: they
   record Track 1, Track 2/oracle, comparator, floor, guard status, REDRESS
   provenance, and routed remainder rather than closing by prose.

This is the CH3 lens required by `PASS-3-SYNTHESIS-PLAN.md:122-126` and the
six-lens challenge contract in `SKINNY-TRIUMVIRATE.md:116-123`.

## Findings

1. Guard-floor authority is now coherent and fail-closed.

   V2 failed convergence for CH1 because stale P3-A typed guard floors and a
   Unicode guard wording conflict had to be folded into V3
   (`HARDENING-S-P3-V2-CONSOLIDATED.md:35-47`). V3 folds those items for CH3:
   P3-A states direct guard floors at `citm_catalog 18191/17431`,
   `apache_builds 11028/9996`, `marine_ik 8759/9248`, and
   `unicode_basic 2253/2182` (`p3a-candidate-shortlist.md:43-52`), and typed
   guard floors at `twitter 17385/15593`, `citm_catalog 29928/17321`,
   `apache_builds 8308/6754`, `github_events 11633/12029`,
   `update_center 11613/10150`, `mesh 9214/7739`, and
   `marine_ik 11552/9894` (`p3a-candidate-shortlist.md:54-67`). P3-C repeats
   the same direct and typed guard tables as binding maintain floors
   (`p3c-falsifiability-gates.md:128-159`), and SPEC §0.5 carries the same
   formulas and numbers (`SPEC.md:136-161`). Behavior waves also gate on guard
   preservation: W3 through W7 all include guard-floor exits or reverts
   (`SPEC.md:471-479`, `SPEC.md:524-530`, `SPEC.md:574-585`,
   `SPEC.md:630-638`, `SPEC.md:678-685`), and W8/W9 require existing direct and
   typed guards before fixpoint or close (`SPEC.md:725-732`,
   `SPEC.md:757-768`). No CH3 guard-floor revision remains.

2. The REDRESS pre-block ledger is complete enough for dispatch authority.

   P3-E names the direct residual close surface and forbids row movement without
   generated Track 1, independent Track 2/oracle, strict same-run comparator,
   output plane, REDRESS id, and same-wave gate consumption
   (`p3e-preblocked-ledger.md:12-16`). It maps wave families to specific
   pre-blocked routes (`p3e-preblocked-ledger.md:31-43`), maps candidate
   surfaces to blocked REDRESS entries (`p3e-preblocked-ledger.md:47-62`), and
   separates hard blocks from material-differential-only routes
   (`p3e-preblocked-ledger.md:64-90`). The SPEC carries these blocks into both
   wave sections and the hard ledger: W3 blocks REDRESS 80 and parse-only/W0
   clamp admission (`SPEC.md:484-486`); W4 blocks W3 substrate, sidecars, and
   object/value carry (`SPEC.md:535-536`); W5 blocks REDRESS 28/33, 60-62, 72,
   83, and 106 plus decoded scratch and retained semantic facts
   (`SPEC.md:590-591`); W6 blocks REDRESS 64, 66-69, 82, 83, 107, 108, and
   already-wired `unescape_string` reuse (`SPEC.md:643-644`); the hard ledger
   blocks W3 substrate, parse-only movement, sidecars, typed/direct proof
   confusion, generic JSON policy, PMULL/CTZ default hot paths, string replays,
   numeric fallback/mantissa widening, and PMU/structural-scan producers
   (`SPEC.md:772-803`). P3-F's short summary is not the controlling exhaustive
   ledger, but it points the draft at the same hard blocks
   (`p3f-spec-draft.md:103-123`). CH3 accepts because the authoritative P3-E and
   SPEC surfaces carry the route blocks into executable wave gates.

3. W3 union/event/class-column substrate is retired and firewalled.

   REDRESS 96 measured a class-column plus move-consumed structural-index
   implementation; correctness checks were green, but it missed every W3
   must-improve row and every W10b maintain row (`skinny/REDRESS.md:2797-2848`).
   REDRESS 97 removed the full vector and used an allocation-free streaming
   cursor; correctness remained green, but it again missed every W3 target and
   maintain row (`skinny/REDRESS.md:2852-2906`). REDRESS 98 therefore retired
   `G-W3-UNION-SUBSTRATE`, including class-lane-only and W4 cascade-lock routes
   (`skinny/REDRESS.md:2910-2940`). REDRESS 102 then firewalled parse-only:
   the active audit found no dispatch route through union/event substrate,
   retained class column, `UnionTape`, structural/streaming cursor,
   class-lane-only route, parser-owned projection, or W4-through-W3 cascade
   lock, and the validator rejects parse-only SOTA movement
   (`skinny/REDRESS.md:3042-3058`). V3 preserves that firewall in the close
   condition (`SPEC.md:36-41`), non-negotiables (`SPEC.md:163-183`), P3-B's
   global pre-blocks (`p3b-wave-sequencing.md:123-138`), P3-D's telemetry
   rejection rule (`p3d-telemetry-schema.md:215-218`), and P3-E's hard block
   list (`p3e-preblocked-ledger.md:214-222`). No wave can dispatch W3 by
   renaming it as dispatch, byte-set, telemetry, or fixpoint work.

4. PMULL/CTZ, numeric, and string/Unicode rejected families remain blocked
   unless materially reframed.

   Numeric fallback widening stays closed: REDRESS 80 found 111126 numbers,
   111080 f64 candidates, zero mantissa overflows, zero ambiguous
   Eisel-Lemire returns, zero `str::parse::<f64>()` fallbacks, and a 0.0000%
   fallback rate (`skinny/REDRESS.md:2217-2248`). V3 therefore blocks f64
   fallback, mantissa/table widening, generic number policy, and primitive-owned
   grammar in W3 and the hard ledger (`SPEC.md:473-486`,
   `SPEC.md:792-793`).

   PMULL/CTZ stay rejected as default production hot paths. REDRESS 88 passed
   primitive/checkasm and asm proof but regressed JSON parse rows, including
   `numbers/track1_generated -10.04%` and `unicode_escapes/track1_generated
   -12.66%` (`skinny/REDRESS.md:2510-2540`). REDRESS 89 kept prefix-XOR scalar
   and narrowed to CTZ/bulk, but the refreshed report still falsified six
   Track 1/2 maintain rows (`skinny/REDRESS.md:2542-2585`). V3 blocks PMULL
   prefix-XOR and CSSC CTZ/bulk emission as default hot paths (`SPEC.md:788`,
   `p3e-preblocked-ledger.md:86-87`, `p3e-preblocked-ledger.md:221-222`).

   String and Unicode rejected families are not reopened. REDRESS 60-62
   measured retained trusted-string boundary and wide/delayed-wide scanner
   routes; the first regressed every measured row, the second failed the full
   advisory gate, and the third regressed sentinel rows before Criterion
   (`skinny/REDRESS.md:1344-1488`). REDRESS 66-69 exhausted direct
   source-hook, parser-owned scratch, byte-output unescape, and semantic string
   fact routes under current direct workloads (`skinny/REDRESS.md:1686-1884`).
   REDRESS 83 rejected `StringBlock16` retained wrapper after correctness and
   parity passed but every named Track 1 parse row regressed beyond guard
   (`skinny/REDRESS.md:2318-2355`). REDRESS 106-108 keep full-string proof and
   existing `unescape_string`/x4 proof-to-production routes blocked: the full
   string caller microproof was `0.774x` versus `1.08x`, the x4 proof moved no
   row, and existing escape production failed direct floors on `unicode_escapes`
   and `y_string_unicode` (`skinny/REDRESS.md:3150-3222`). V3 converts those
   facts into W5/W6 gate blocks, not fresh permission (`SPEC.md:557-591`,
   `SPEC.md:611-644`, `p3e-preblocked-ledger.md:184-194`).

5. Failed and per-row uncloseable outcomes are honestly measurable.

   The close condition requires every SK-V11 residual direct row either to become
   strict same-run `A / GO` on generated Track 1 and independent Track 2/oracle
   or to receive a per-row uncloseable REDRESS proof with measurement
   (`SPEC.md:26-29`). P3-C binds the direct residual floors and states that both
   tracks must clear one same-run strict direct comparator for admission
   (`p3c-falsifiability-gates.md:106-126`). It also rejects any unmeasurable wave
   before redress if it lacks a named row, Mbps threshold, generated Track 1,
   independent Track 2/oracle, strict comparator, scalar/checkasm plan,
   same-wave consumer, guard block, gate consumer, or revert protocol
   (`p3c-falsifiability-gates.md:187-205`). W8 must record failed residual rows
   with attempted candidate, measured tracks, comparator, floor, and guard status
   (`SPEC.md:715-732`), and W9 requires the close proof to name attempted
   intervention, Track 1, Track 2/oracle, comparator, floor, guard result, and
   routed remainder (`SPEC.md:757-759`). P3-D makes these fields gate-consumed
   telemetry, including `redress_entry`, `wave_id`, `run_id`, comparator source,
   profile artifact, Track 2 independence, and same-wave consumer class
   (`p3d-telemetry-schema.md:98-148`, `p3d-telemetry-schema.md:229-232`).
   Therefore a miss cannot become a routed-residual paper close.

## Accepted Facts

- The V3 guard floors are no longer split between P3-A and P3-C/SPEC.
- P3-E and SPEC keep REDRESS 96/97/98/102, parse-only firewalling, and sidecar
  bans binding across all waves.
- W3 union/event/class-column, streaming cursor, class lane, `UnionTape`, and
  W4-through-W3 cascade-lock routes remain retired, not renamed.
- PMULL/CTZ, numeric fallback/mantissa widening, retained string scans,
  StringBlock16 wrappers, decoded scratch/stats/facts, x4 proof-to-production,
  and already-wired `unescape_string` production are blocked unless a future
  plan supplies the required material-differential package before redress.
- Per-row failed/uncloseable outcomes are measurable and gate-consumed.

## Final Judgment

ACCEPT. The S-P3 V3 packet satisfies CH3. It preserves REDRESS pre-blocks,
keeps W3 and parse-only routes firewalled, carries exact guard floors, retains
the rejected PMULL/CTZ/string/numeric families as rejected unless materially
reframed, and requires failed or uncloseable residual rows to close only through
measured REDRESS evidence.
