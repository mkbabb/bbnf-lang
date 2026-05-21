# SK-V13 Pass Alpha - Alpha-D Validated / Invalidated Ledger

Pass: Pass Alpha SK-V12 -> SK-V13.
Agent: alpha-D.
Date: 2026-05-21.
Scope: update the SK-V12 validated / invalidated / demoted / still-open
ledger into SK-V13 under the 2026-05-21 full-SOTA addendum.
Output: this file only.

## Contract Boundary

This ledger carries SK-V12 close evidence forward, but it does not carry the
old SK-V12 close bar forward. SK-V12 closed by `PASS-ADMIT` on one CSS L4 row:
`css_l4/declaration_values/direct_to_struct/main`, output plane
`css_l4_declaration_value_fact_stream`, generated Track 1
`429.34420791225705 Mbps`, cssparser oracle `217.42665242186035 Mbps`,
lightningcss strict same-plane comparator `168.92962215656692 Mbps`, and
strict equality `pass:track1=cssparser=lightningcss`.

The binding 2026-05-21 addendum supersedes every weaker reading:

- Full CSS parity across every non-OUT_OF_SCOPE SK-V13 parity-matrix feature is
  mandatory. A single declaration-values win is not a campaign close.
- Every JSON row, across all 17 corpora and all 3 planes, must beat sonic-rs
  strict on the same plane/corpus/strictness, or carry architectural-level
  intrinsic-block proof.
- `parse_only` is admission-eligible again. The SK-V12 "diagnostic only" rule
  is invalid for SK-V13.
- Implementation-limited blocks are reopens, not closes. Only architectural
  intrinsic-block proof can close a row or feature short of SOTA/parity.

## Source Map

Required authority read for this Alpha-D bracket:

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`
- `restart/skinny/tranches/sk-v12/research/w5/E-campaign-close.md`
- `restart/skinny/tranches/sk-v12/research/w5/D-close-doc-agreement.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` entries 119-127, with REDRESS 88/89/90 and 96/97/98
  as historical route context
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-pass-framework-leverage.md`

Close commit anchor observed in SK-V13 decision-engine scoping:
`7a6b3c4d` (`docs(sk-v12-waveW5-redress): close SK-V12 PASS-ADMIT`).

## Validated Ledger

### V1 - SK-V12 CSS Win Carries As One Admitted Feature

The SK-V12 CSS row is a real admitted row and remains a load-bearing proof that
the generated CSS L4 path can beat lightningcss on a strict same-plane fact
stream. The row carries forward as `ADMITTED-PARITY` for the declaration-values
fixture surface only:

| Row | Track 1 | cssparser oracle | lightningcss | Margin | Carry-forward status |
|---|---:|---:|---:|---:|---|
| `css_l4/declaration_values/direct_to_struct/main` | 429.34420791225705 | 217.42665242186035 | 168.92962215656692 | +259.41458575569015 | Valid SK-V12 admit; one CSS parity row |

SK-V13 implication: use this as the seed CSS proof and guard floor, not as full
CSS parity. The SK-V13 parity matrix records 1 PARITY row, 7 PARTIAL rows, 16
MISSING rows, and 6 OUT_OF_SCOPE rows. The addendum requires every non-OUT_OF_SCOPE
feature to become ADMITTED-PARITY or carry architectural intrinsic-block proof.

### V2 - GrammarConfig / Lock 14 Legality Partially Carries

SK-V12 W1a validated the legal direction: generated metadata moved structural
bytes, literal patterns, flag policy entry points, and decode policy behind
per-grammar config modules rather than a public generic trait. This carries as
partial Lock 14 progress.

The SK-V13 value/API scoping prevents overclaiming. Post-W1a status is:

| Leak family | Status in SK-V13 scoping | Carry-forward meaning |
|---|---|---|
| Structural alphabet hardcoding | RESOLVED | Validated pattern: per-grammar config owns structural bytes. |
| Value dispatch | PARTIAL | Inline JSON dispatch remains; CSS/Sheets need config-driven dispatch. |
| String escape/quote policy | PARTIAL | Decode flags moved; JSON quote/backslash/control logic remains. |
| Number policy | PARTIAL | JSON-tuned number matcher remains; CSS number policy needs generated config. |
| Key quoting assumption | PARTIAL | JSON object key/colon path remains hardcoded. |
| `OffsetFlags` semantics | UNRESOLVED | JSON semantic bits remain embedded in shared tape type. |
| `JsonSink` trait | UNRESOLVED | Direct sink surface is still JSON-specific. |

SK-V13 implication: GrammarConfig is not a closed item. It is a validated
partial bridge that must expand into dispatch, whitespace/comment, string,
number, literal/keyword, and grammar-specific sink/view generation before full
non-JSON generality can be claimed.

### V3 - `escape_mask_64` Fix Carries As Resolved Correctness Blocker

SK-V12 W2 resolved the historical `escape_mask_64` NEON correctness blocker.
The xorshift falsifier `0xCAFEF00DBAADF00D` no longer reproduces in the strict
scanner parity harness at HEAD, and Lock 16 is no longer blocked by the old
state-handoff bug.

Carry-forward limit: the fix is a prerequisite, not a blanket SIMD admission.
SK-V13 SIMD/ASM waves still need scalar reference, direct or caller-level
checkasm parity, corpus/equality proof, and same-wave production consumers.
The scoping notes still call for dedicated direct `escape_mask_64` differential
coverage and pinned caller-level adversarial windows before any related SIMD
admission relies on it.

### V4 - ASM Microbench Route Carries As Measured Route, Not Production Admit

SK-V12 W4 records a real ASM-gen route attempt for
`find_ascii_set_member64(bytes, cursor, end, b"{};")`. The retained microbench
artifact reports parity `pass`, decision `pass`, scalar `18.510497846 ns/iter`,
candidate `3.923145814 ns/iter`, speedup `4.718279341x`, and threshold `1.01`.

Carry-forward limit: this is `ROUTE-PRODUCTION-SPLIT`, not a production SIMD or
CSS admission. SK-V13 may route it into a narrow W4b-style production split, but
that split must wire a same-wave CSS scan-block consumer, run fresh equality and
Criterion evidence, consume a current gate/report, and maintain Lock 14.

### V5 - JSON Guard State Held, But No Longer Closes Rows

SK-V12 W5 verified the checked-in JSON guard floors after adding the CSS row,
with no measured JSON demotion. The previously admitted JSON direct and typed
guard rows remain protected against silent regression.

Carry-forward limit under the addendum: guard preservation is necessary but not
sufficient. REDRESS 119 direct fixpoint is history only, and every JSON row is
wave-eligible for `> sonic-rs strict` admission or architectural intrinsic-block
proof. The old SK-V12 JSON guard/fixpoint ledger cannot be used to close SK-V13.

### V6 - Strict Comparator / Output-Plane Discipline Still Binds

Strict-vs-strict, same-corpus, same-output-plane, independent oracle/Track 2,
gate-consumed provenance, and no silent demotion all carry forward. The addendum
strengthens this discipline: every CSS feature and every JSON row must clear its
domain comparator, not merely maintain a local guard.

## Invalidated Ledger

### I1 - Single-Row CSS Close Is Invalid As SK-V13 Close

SK-V12's declaration-values row is valid, but treating it as full CSS parity is
invalidated by the addendum. SK-V13 must close the remaining non-OUT_OF_SCOPE
CSS feature matrix, including selectors, stylesheet root, declarations,
at-rules, nested rules, custom properties, calc/var/url, color functions,
gradients, transforms, filters, easing, media queries, pseudo-classes,
pseudo-elements, attribute selectors, and other non-OOS partial/missing rows.

No feature may close as PARTIAL. No implementation-limited block can close.

### I2 - REDRESS 119 / 120 As Active JSON Closure Is Invalidated

REDRESS 119 and REDRESS 120 remain historical measurements, but their fixpoint
authority is lifted. The 13 direct residual rows are no longer protected from
reopen by the old "no current kernel route" ranking, and the 10 rows outside the
old 3-reopen shortlist are equally eligible.

SK-V13 implication: every JSON row, including old A/GO guard rows, old
N-direct rows, typed rows, and parse rows, must be reported against sonic-rs
strict on the same plane. Any shortfall remains open unless there is
architectural-level intrinsic-block proof.

### I3 - `parse_only` Diagnostic-Only Status Is Invalidated

The SK-V12 D6 rule said `parse_only` remained diagnostic-only. The addendum
explicitly amends it: all 17 `parse_only` rows are admission-eligible and must
beat sonic-rs strict `parse_only` on the same corpus or carry intrinsic-block
proof.

SK-V13 implication: Alpha and downstream S-P1/S-P2/S-P3 must include parse-only
rows in the rolling SOTA delta and wave target ledger. They cannot be excluded
as diagnostic guard noise.

### I4 - GrammarConfig-As-Complete Is Invalidated

W1a's config modules validate the route, but SK-V13 scoping shows the surface is
not complete. Inline JSON value dispatch, JSON string/escape assumptions, JSON
number policy, key-colon structure, `OffsetFlags` semantics, and `JsonSink`
remain live coupling. Any SK-V13 plan that says GrammarConfig is "done" or uses
it to close Lock 14 for all grammars is invalid.

### I5 - Implementation-Limited Block Closes Are Invalidated

The addendum replaces weaker fallback language. CSS features and JSON rows can
close short of SOTA only with architectural-level intrinsic-block proof:
lightningcss-can-do / bbnf-cannot for CSS, or same-plane sonic-strict block for
JSON. "Not implemented yet", "no current route", stale profile, codegen
complexity, or production wiring not landed are reopens.

### I6 - Support-Only SIMD/ASM Admission Is Invalidated

SK-V12 W4's microbench route is valid evidence, but the addendum's no-deferral
and per-wave row-movement target invalidate any attempt to land new support-only
primitives. Every new primitive must wire to a row-moving same-wave consumer or
be removed/demoted with evidence before close.

## Demoted Ledger

| Item | SK-V12 status | SK-V13 status | Handling |
|---|---|---|---|
| Five aarch64 orphans: `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `byte_context`, `cache_hints` | REDRESS-126 `inventory_demoted_with_evidence`; final orphan count zero | History only; not admitted primitives | Keep as closed inventory history. New related attempts must cite REDRESS 88/89/126, name material differential, pass CHALLENGE, and wire a same-wave consumer. |
| `a64_ascii_set_run_skip` / delimiter find | Microbench pass, routed production split | Candidate route, not production admit | SK-V13 production split must own source wiring, equality, Criterion, gate/report, and Lock 14. No further deferral. |
| REDRESS 119 direct residual fixpoint | SK-V11/SK-V12 historical fixpoint authority | History only under addendum | Every direct row is reopen-eligible for > sonic-rs strict or intrinsic block. |
| JSON guard floors | Maintained guard rail | Necessary regression guard only | Cannot close SK-V13 JSON rows by guard maintenance alone. |
| Sheets / BBNF-self fallback | SK-V12 fallback history after CSS redress | Secondary history | Not a CSS parity substitute and not a JSON row substitute. |
| W1a non-JSON report lane | Useful infrastructure | Infrastructure only | Must feed admitted rows; no support-only close. |

## Category Reopened / Untouched Ledger

### R1 - Union Category Remains Unblocked But Untouched By SK-V12 Close

USER PIN D3 unblocked union/substrate at category level. REDRESS 96/97/98 remain
historical measured failures of specific implementations. SK-V12 did not land a
fresh union attempt because the tranche closed by CSS ADMIT, not FIXPOINT.

The addendum now makes union a close gate: SK-V13 needs at least one union
variant ADMITTED or architectural-block proof. Candidate routes in scoping
include GrammarConfig-driven heterogeneous union, e-graph-selected per-rule
union shape, and SIMD-first PMULL+CSSC union. Each must cite REDRESS 96/97/98,
name a material differential, pass CHALLENGE, provide scalar/reference parity,
and move a row or record intrinsic-block proof.

### R2 - ASM-Gen Category Remains Unblocked With One Routed Attempt

USER PIN D4 unblocked ASM-gen at category level. REDRESS 88/89/90 remain
historical measured failures. SK-V12 W4 adds one measured delimiter microbench
route, but no production SIMD/ASM admission.

SK-V13 implication: the narrowest carry-forward candidate is production wiring
for `a64_ascii_set_run_skip`. Broader PMULL/CSSC/UDOT/TBX/EOR3 routes remain
legal only with material differentials, scalar and checkasm proof, same-wave
consumer, and measured row impact or intrinsic block.

## Still-Open Ledger For SK-V13

### O1 - Full CSS L4 Parity

Close every non-OUT_OF_SCOPE feature in the SK-V13 CSS parity matrix. The
SK-V12 declaration-values row is the seed; all PARTIAL/MISSING non-OOS features
must become ADMITTED-PARITY with Track 1 > lightningcss + 1, strict equality,
same output plane, and independent oracle, or carry architectural intrinsic
block proof.

### O2 - Every JSON Row > Sonic-RS Strict

All 17 corpora x 3 JSON planes are active SK-V13 campaign rows. The 13 N-direct
rows, all parse-only rows, existing A/GO rows, and typed rows require current
same-plane sonic-rs strict comparison. No silent demotion of old admits is
allowed, but old admits must still satisfy the new comparator bar or reopen.

### O3 - Decision-Engine Fold

The decision engine remains hardcoded. SK-V13 scoping records no code changes
since the prior audit: no `egg` Language implementation in skinny, no active CSP
solver, passive cost facts, hand-curated recognizer mining, and a hardcoded
P1-P8 cascade in backend-shape selection.

Close gate G2 remains open: extract bbnf-regex, wire e-graph Language and
rewrites, turn cost into an active `egg::CostFunction`, add CSP resolver, and
delete or replace the hardcoded cascade without JSON regression.

### O4 - GrammarConfig Phase 2 / Value API Generality

The per-grammar config surface needs dispatch, whitespace/comment, string,
number, keyword, sink, view, and flag semantics expanded without public
substrate API churn or JSON policy leakage. CSS and future Sheets/BBNF-self
rows cannot rely on the current JSON-owned dispatch/sink surface.

### O5 - ASM Production Split And No-Orphan Continuity

SK-V13 must either wire `a64_ascii_set_run_skip` to a measured same-wave
production consumer or leave it as route history. Any new SIMD/ASM primitive
must avoid orphan creation by landing with a consumer and measurement, or be
removed/demoted before close with REDRESS evidence.

### O6 - Union Admission Or Architectural Block

The union category is no longer preblocked, but it is also not satisfied.
SK-V13 must land at least one materially differentiated union variant or prove
an architectural intrinsic block. Historical REDRESS 96/97/98 failures alone do
not satisfy the addendum.

### O7 - Fresh Profile Truth And Rolling SOTA Delta

SK-V12 W5 verified JSON floors but did not fresh-measure all rows. SK-V13 S-P1
must produce fresh PMU/hot-leaf truth for CSS and JSON, then publish the rolling
SOTA delta covering all 51 JSON rows plus every CSS feature:

`row | plane | T1_current | T1_sota | margin | tranche_admitted`

## Alpha-D SK-V13 Verdict

VALIDATED:

- SK-V12 CSS declaration-values row is a real strict same-plane win over
  lightningcss.
- W1a GrammarConfig/config-module direction is valid but partial.
- `escape_mask_64` correctness blocker is resolved as a prerequisite.
- W4 delimiter ASM microbench route is real measured evidence.
- JSON guard floors held with no demotion at SK-V12 close.
- Strict comparator, output-plane, independent oracle, and gate-consumption
  discipline all carry forward.

INVALIDATED BY ADDENDUM:

- Any SK-V13 close based on the single SK-V12 CSS row.
- Any JSON closure based on REDRESS 119/120 fixpoint history.
- `parse_only` diagnostic-only exclusion.
- Any implementation-limited block as a close condition.
- Any support-only primitive or deferred-consumer landing.
- Any claim that GrammarConfig/Lock 14 is fully solved.

DEMOTED / HISTORY ONLY:

- Five SK-V12 aarch64 orphan demotions.
- REDRESS 119 direct residual fixpoint.
- REDRESS 96/97/98 union failures and REDRESS 88/89/90 ASM failures as specific
  implementation history.
- Sheets/BBNF-self fallback research as non-authoritative for the SK-V13 close.

STILL OPEN:

- Full CSS parity across the non-OOS matrix.
- All 51 JSON rows > sonic-rs strict or architectural intrinsic block.
- Decision-engine fold from hardcoded cascade to bbnf-regex + egraph + cost +
  CSP resolver.
- Union admission or intrinsic block.
- ASM production wiring with same-wave row movement.
- GrammarConfig/value API phase 2 and fresh rolling SOTA reporting.
