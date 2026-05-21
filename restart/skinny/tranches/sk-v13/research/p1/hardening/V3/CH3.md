# SK-V13 S-P1 V3 CH3: Regression / REDRESS Review

Pass: S-P1 Profile. Cycle: V3.
Date: 2026-05-21.
Lens: CH3 REGRESSION.
Scope: adversarial review of the V3 `p1a` through `p1f` profile artifacts and
`support/evidence-ledger-v3.md` for silent reopening of rejected REDRESS routes.
Disposition: ACCEPT.

## §1 - Method

Reviewed:

- `restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH3, especially the rule that
  anomaly text pointing at a pre-blocked route must cite/mark it as pre-blocked.
- `skinny/REDRESS.md`, with emphasis on REDRESS 96/97/98, REDRESS 119/120,
  pre-pin route families, and REDRESS-126.
- `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`.
- `restart/skinny/tranches/sk-v13/research/p1/hardening/V2/CH3.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md`.
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`.

CH3 standard: S-P1 profile artifacts may surface anomalies and signals, but
they must not let those signals read as a reopened implementation route already
blocked in REDRESS. If a signal touches rejected history, the profile needs an
inline guard that preserves the prior disposition and names the material
differential required before later S-P2/S-P3 work can use it.

## §2 - Findings

### CH3-V3-R1 - REDRESS 119/120 direct-row guards are inline enough

Disposition: ACCEPT.

V2 required direct-row progress language beside the direct profile table and
comparator/sidecar anomalies. V3 now supplies it:

- `p1b-samply-mode-2.md:127`-`130` says direct profile signals do not reopen
  REDRESS 119/120 and requires prior-fixpoint citation, a fresh material
  differential, and same-harness strict comparator evidence.
- `p1f-results-delta.md:124`-`129` keeps every classification as
  `profile_signal_not_gate_admission` and repeats that V2 direct profile signals
  are not direct-row reopens or admissions.
- `support/evidence-ledger-v3.md:117`-`119` makes the same guard canonical for
  the V3 ledger.
- `skinny/REDRESS.md:3497`-`3552` records REDRESS 119/120 as measured fixpoint
  history, and the user-pin addendum makes those rows wave-eligible only with
  prior-fixpoint citation plus material differential.

This closes the V2 ambiguity. V3 direct rows can be read as profile evidence,
not as direct-row admission, demotion, or REDRESS-free reopen authority.

### CH3-V3-R2 - Pre-pin route-family guards are inline enough

Disposition: ACCEPT.

V2 required route guards near dispatch, masking, tiny-string, and unescape
signals. V3 now carries those guards at the signal sites and in the ledger:

- `p1a-samply-mode-1.md:141`-`145` blocks reopening dispatch-table/function-
  pointer, parser-local cursor, event sidecar, source-method digest, and
  decoded-string stats routes from parse `dispatch_value`, tiny-string, or
  hex-escape signals.
- `p1b-samply-mode-2.md:131`-`134` applies the same guard to generated direct
  envelopes and `unescape_string`.
- `p1e-hot-leaf-attribution.md:112`-`115` binds direct progress, dispatch,
  masking, unescape, PEXT, and ASM-sidecar signals to their REDRESS guards.
- `support/evidence-ledger-v3.md:120`-`124` canonicalizes the rejected family
  list: parse-time aux side tables, parser-local structural cursors, event
  sidecars, dispatch-table/function-pointer alternates, decoded-string stats
  sinks, generic source visitors, and source-method digest folds.

That is sufficient for CH3. The wording no longer leaves `dispatch_value`,
generated direct envelopes, tiny-string leaves, or `unescape_string` as
uncited shortcuts around the pre-pin rejected families.

### CH3-V3-R3 - REDRESS 96/97/98 remain fenced

Disposition: ACCEPT.

V2 already mostly closed the union-substrate reopen risk, and V3 preserves that
binding:

- `p1c-samply-mode-3.md:115`-`117` says structural SIMD wins are scanner
  micro-results and do not reopen REDRESS 96/97/98 without material
  differential and row movement.
- `p1d-pmu-cycles.md:131`-`133` repeats that REDRESS 96/97/98 remain binding
  history for any new union route.
- `p1e-hot-leaf-attribution.md:104`-`106` treats structural SIMD as a
  fresh-measurement antecedent, not an implementation route.
- `support/evidence-ledger-v3.md:90`-`91` classifies structural scan rows as
  JSON scanner micro-results and explicitly says REDRESS 96/97/98 are not
  reopened.
- `skinny/REDRESS.md:2797`-`2950` records the measured failures and gate
  retirement for the union-substrate thesis.

No V3 artifact converts the fast structural SIMD rows into a retained union
substrate, parser-owned structural projection, or same-route amendment.

### CH3-V3-R4 - REDRESS-126 zero-orphan guards are inline enough

Disposition: ACCEPT.

V2 required explicit zero-orphan carry-forward beside PEXT, SIMD, ASM sidecar,
and comparator-sidecar gaps. V3 now has enough inline guard coverage:

- `p1c-samply-mode-3.md:124`-`133` marks PEXT unsupported on aarch64, leaves
  line-poor NEON as attribution risk, and states that PEXT, line-poor NEON,
  `bulk_emit_positions_64_neon`, and absent C/C++ sidecars do not create
  orphan SIMD primitives or reopen production PMULL/CSSC/prefix-XOR/bulk-
  emission routes.
- `p1e-hot-leaf-attribution.md:109`-`115` keeps ASM/system leaves explicit and
  blocks REDRESS-126 reopening or orphan SIMD creation.
- `p1f-results-delta.md:130`-`131` keeps pre-pin families and REDRESS-126
  zero-orphan accounting in force for profile signals.
- `support/evidence-ledger-v3.md:125`-`128` makes the zero-orphan rule
  canonical: future SIMD routes need scalar reference, checkasm/parity,
  feature-mask disclosure, same-wave consumer, and zero-orphan disposition.
- `skinny/REDRESS.md:3864`-`3872` records REDRESS-126 as route-production split
  plus final zero aarch64 orphan state.

P1-F is concise, but read with P1-C, P1-E, and the canonical ledger it is
inline enough for CH3. The V3 corpus no longer leaves absent PEXT/SIMD sidecars
or function-only ASM leaves as unowned SIMD work.

## §3 - Non-Findings

- No V3 artifact proposes a source patch, generated runtime rewrite,
  benchmark/gate semantic change, or `skinny/RESULTS.md` movement.
- The V3 evidence ledger states that all rows are
  `profile_signal_not_gate_admission`.
- Typed missing rows remain product-surface gaps, not profiler omissions or
  route admissions.
- CSS declaration-values remains a profiled equality/throughput signal whose
  top leaves are timer/fact-sink overhead, not a new parser primitive.

## §4 - Final Disposition

Disposition: ACCEPT.

Rationale: The specific CH3 defects from V2 have been folded. REDRESS 119/120,
the pre-pin rejected route families, REDRESS 96/97/98, and REDRESS-126
zero-orphan accounting are now present at the relevant anomaly sites and in the
canonical V3 evidence ledger. Later waves still need material differentials and
gate-consumed evidence before implementation dispatch, but S-P1 V3 no longer
silently reopens rejected REDRESS routes.
