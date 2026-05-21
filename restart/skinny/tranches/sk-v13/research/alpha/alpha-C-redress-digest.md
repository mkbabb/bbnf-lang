# Alpha-C Redress Digest for SK-V13

Date: 2026-05-21.
Role: alpha-C for SK-V13 Pass Alpha.
Authority read: `restart/prompts/pass-contracts/PASS-ALPHA.md`,
`skinny/REDRESS.md` through REDRESS-127,
`restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`,
`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`, and the
SK-V13 scoping packet.

## Binding Interpretation

The USER PIN addendum supersedes the older REDRESS-119/120 fixpoint close.
REDRESS-119 and REDRESS-120 remain evidence of measured SK-V11 attempts, but
they no longer close JSON direct residuals or the parse-only plane. For SK-V13:

- every JSON `direct_to_struct` residual row reopens unless the row has an
  architectural-level intrinsic-block proof;
- every JSON `parse_only` row reopens under amended D6 and must beat
  `sonic-rs` strict on the same plane, or carry an architectural-level
  intrinsic-block proof;
- the ten rows outside the pre-SK-V13 three-row shortlist are equally
  wave-eligible because the ranking predated W1a, W2, W4, and the planned
  decision-engine fold;
- category-level union and ASM-gen blocks are lifted, but the historical
  implementations remain pre-blocks unless a fresh route names material
  differential, passes CHALLENGE, and wires a same-wave consumer;
- implementation-limited failures are reopens, not closes.

This digest therefore separates **pre-blocked exact routes** from
**category-unblocked routes**. A route is pre-blocked only at the level of the
same implementation shape; it does not block a materially different route under
the pin.

## Current Close Baseline

SK-V12 closed by `PASS-ADMIT`, not by `FIXPOINT`. The admitted row is
`css_l4/declaration_values/direct_to_struct/main` on
`css_l4_declaration_value_fact_stream`: generated Track 1
`429.34420791225705 Mbps`, cssparser oracle `217.42665242186035 Mbps`,
lightningcss `168.92962215656692 Mbps`, threshold
`169.92962215656692 Mbps`, margin `259.41458575569015 Mbps`, strict equality
`pass:track1=cssparser=lightningcss`, fact-stream SHA-256
`caf97bee6e413157e6114985bc1108bc3a8fbf597a1e519b3ccff905d2e5236c`.

SK-V12 also records:

- REDRESS-121: GrammarConfig / Lock 14 legality gate admitted for generated
  metadata; it does not close the remaining dispatch/string/number/sink leaks.
- REDRESS-122: `escape_mask_64` correctness prerequisite admitted.
- REDRESS-123/124/125: CSS generated Track 1 scaffold, lightningcss comparator,
  and CSS SOTA report gate admitted.
- REDRESS-126: ASM delimiter route recorded as `ROUTE-PRODUCTION-SPLIT`; final
  aarch64 orphan count is zero by demotion with evidence.
- REDRESS-127: close reconciliation promotes the CSS candidate to `PASS-ADMIT`.

## Pre-Blocked Exact Routes

The following routes should pre-block SK-V13 waves unless the wave states a
material differential and proves it with fresh same-plane evidence.

| REDRESS | Exact route pre-blocked | SK-V13 treatment |
|---|---|---|
| 16 | Pair-token-free object projection | Do not repeat explicit key/value cursor-pair removal without a new representation and measured row lift. |
| 17 | 256-entry function-pointer dispatch table | Pre-block stale dispatch-table alternates and any report-only duplicate of canonical parser. |
| 18 | Skipless 12-byte token shape | Pre-block same narrow token/span derivation unless replaced by a materially different lazy-offset or union route. |
| 28/33 | Earlier `match_tiny_plain_string` / NEON tiny-string wiring | Pre-block same tiny-string kernel reuse without lower-overhead inline/ASM proof and same-wave consumer. |
| 50 | Retained projection side tables / parse-time aux side tables | Pre-block sidecar projection tables. |
| 51/53 | Event cursor / structural cursor variants | Pre-block parser-owned cursor/list routes and sidecar structural cursors. |
| 54/55 | Sink-local decoded stats and quote-source fused materializer/hash | Pre-block decoded-byte host-sink/source-method folds on the same `JsonDirectDigest` seam. |
| 64, 66-69 | Escape-tail and direct string/Unicode materializer family | Pre-block repeated escaped-string materialization/source-hook routes. |
| 80 | Mantissa table/fallback widening for Canada/numeric rows | Pre-block table-only Eisel-Lemire widening absent measured fallback pool. |
| 82 | Single-quartet Unicode escape classifier | Pre-block per-quartet materializer helper reuse; x4/segment routes need distinct consumer proof. |
| 83 | Generated retained StringBlock16 tiny wrapper | Pre-block the same 16-byte wrapper in generated retained parsing. |
| 84 | Object-pair value-byte control compaction | Pre-block direct post-colon value-byte return / boundary shaving as a close route. |
| 88 | PMULL prefix-XOR body as previously framed | Historical primitive block only; reopened category requires material differential and consumer. |
| 89 | CSSC CTZ / bulk emit route as previously framed | Historical primitive block only; reopened category requires material differential and consumer. |
| 90 | EOR3 / related ASM-gen surface as previously framed | Historical primitive block only; no admission without hot-leaf attribution and consumer. |
| 92 | Tape plus structural-projection over scanner/tape mismatch | Pre-block structural-projection paper route without retained event-grammar proof and row gate. |
| 93 | Hand Track 2 scalar-parent fold | Pre-block digest arithmetic/parent folding unless a W4/V13-aware gate and independent backstop exist. |
| 96 | Union V1 class-column + move-consumed structural vector | Pre-block full class-column side vector and move-consumed `scan_structurals` integration. |
| 97 | Union V2 streaming cursor over scanner | Pre-block allocation-free streaming cursor/class-lane integration that keeps the same parse-loop cost shape. |
| 98 | Union V3 class-lane-only / no producer | Pre-block proof-only retained `JsonNodeKind::at_cursor` without producer and same-wave row movement. |
| 103 | Fixed object-root typed `instruments` row | Pre-block typed row admission where independent Track 2/oracle remains below floor. |
| 106 | Full-string primitive caller proof | Pre-block aggregate `match_string_at_quote_trusted_utf8` wrapper unless profile shows a different hot owner. |
| 108 | Existing escape production reuse of already-consuming x4 primitive | Pre-block cosmetic production reuse of W8 proof without a real source delta. |
| 112/113 | SK-V11 non-JSON baseline and generated CSS entry block | Historical block is superseded by SK-V12 CSS admission; do not cite it to block CSS expansion. |
| 114 | `number_span_emit_slot` direct closure | Pre-block same scalar slot route, especially for `mesh`, without fresh decision-engine differential. |
| 115 | `container_tail_next` direct dispatch | Pre-block same scalar container-tail helper for `random` without fresh route. |
| 116 | Bounded string span entry route | Pre-block unless CHALLENGE accepts a new span shape and malformed-input parity. |
| 117 | Escaped segment decoded-source digest fold | Pre-block decoded-source fold that reopens REDRESS-54 seam. |
| 118 | Output digest/hash host-sink route | Pre-block no-source host-sink/digest bucket route absent independent oracle and row floor proof. |
| 119/120 | SK-V11 direct fixpoint / close | History only under pin; cannot pre-block row reopen by itself. |
| 126 | W4 microbench-only ASM production split | Pre-block claiming production SIMD admission from the microbench alone; SK-V13 must wire production and gate it. |

## Category-Unblocked Routes

### JSON Direct Rows

All direct rows are reopen obligations under A2. The current residual authority
comes from REDRESS-119 history plus SK-V12 guard verification, but no residual
row is closed unless SK-V13 produces an architectural-level intrinsic-block
proof. Direct rows must admit by strict equality vs `sonic-rs` strict on the
same plane and Track 1 `> sonic-rs strict + 1 Mbps`, with no silent demotion of
previous A/GO rows.

The REDRESS-119 rows to reopen are:
`twitter`, `canada`, `github_events`, `update_center`, `mesh`, `random`,
`gsoc-2018`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`,
`distinct_values`, and `y_string_unicode`.

### JSON Parse-Only Rows

REDRESS-102's parse-only firewall and REDRESS-119/120 close assumptions are
overridden by A3. All 17 `parse_only` rows are admission-eligible and must beat
`sonic-rs` strict `parse_only` on the same corpus/plane or carry an
architectural-level intrinsic-block proof. Parse-only remains subject to strict
comparator discipline; permissive or lossy rows are flaw probes only.

### Union Substrate

USER PIN D3 and SK-V12 close unblock union at the category level. REDRESS
96/97/98 remain historical implementations, not category bans. Legal SK-V13
union routes must:

- cite 96/97/98 and state the material differential;
- avoid sidecar vectors, parser-owned structural cursors, parallel `UnionTape`,
  new directives, new BIR variants, new `BackendShape`, and public substrate
  API churn unless explicitly reauthorized;
- keep the single shared tape surface stable;
- pass scalar/reference parity, checkasm where applicable, CHALLENGE, and
  same-wave consumer measurement.

SK-V13 scoping identifies viable materially different families: per-grammar or
per-rule GrammarConfig/codegen shape selection, e-graph-selected union shape,
and ARMv9.2 SIMD-first union using PMULL+CSSC-CTZ. These are not automatically
admitted; they are legal reopen routes.

### ASM-Gen / SIMD

USER PIN D4 and SK-V12 REDRESS-126 unblock ASM-gen at the category level.
Historical REDRESS 88/89/90 and the five W4 orphan demotions block only their
old implementations. Any SK-V13 SIMD/ASM wave must carry scalar reference,
checkasm parity, strict corpus parity where relevant, same-wave production
consumer, and row movement. `a64_ascii_set_run_skip` is the nearest production
split: its W4 microbench passed at `4.718279341x`, but it has no production CSS
consumer yet.

### CSS L4 Expansion

SK-V12 admitted only declaration-value token facts on a 187-byte fixture. The
CSS category is not blocked; instead it now carries the full parity obligation:
every non-OUT_OF_SCOPE feature in the SK-V13 parity matrix must become
ADMITTED-PARITY against lightningcss or receive architectural-level
intrinsic-block proof. Missing or partial families include stylesheet root,
selectors, at-rules/media, variables, calc/var/url, color functions, gradients,
transforms, filters, easing, nested rules, pseudo-classes/elements, attribute
selectors, vendor/custom at-rules, comments/whitespace where in scope.

## SK-V13 Material-Differential Requirements

Every reopen wave must include one of these differentials; "try harder" or
renaming an old route is insufficient.

1. **Decision-engine differential**: bbnf-regex extraction, e-graph Language,
   `egg::CostFunction`, CSP resolver, and deletion of the P1-P8 hardcoded
   cascade. This can reopen rows previously lacking a current kernel route only
   if it produces a concrete generated candidate and same-wave measured row
   movement.
2. **GrammarConfig/config-module differential**: generated per-grammar dispatch,
   string, number, keyword, whitespace/comment, view, and sink surfaces that
   remove residual JSON policy leaks without adding forbidden public substrate
   API. This is required for CSS expansion and non-JSON generality.
3. **Union differential**: codegen-time per-rule or per-grammar shape selection,
   e-graph-selected shape, or SIMD-first vector-lane union. It must differ from
   REDRESS 96/97/98 in data layout, producer/consumer ownership, and measured
   cost path.
4. **ASM/SIMD differential**: new primitive route tied to a named hot leaf and
   same-wave consumer. Historical PMULL/CTZ/EOR3 blocks are overcome only by
   consumer-coupled proof, not by standalone microbench evidence.
5. **CSS parity differential**: expanded corpus and same-plane oracle coverage
   against lightningcss, not another narrow W1b declaration-token fixture.
6. **Parse-only differential**: rows need a real parse-plane producer and
   strict `sonic-rs` comparator; parse-only is no longer diagnostic-only.
7. **Intrinsic-block proof differential**: if a row/feature cannot admit, the
   proof must be architectural-level: lightningcss/sonic can do it and bbnf
   cannot for an intrinsic reason. Implementation-limited failures reopen.

## Reopen Obligations for S-P3

S-P3 should encode these as wave-gate obligations:

- W10.N CSS waves: one per non-OUT_OF_SCOPE CSS parity feature not admitted,
  with strict equality vs lightningcss and independent oracle.
- W11.N JSON direct waves: one per direct residual row unless admitted earlier;
  each wave cites the prior REDRESS fixpoint and material differential.
- W14.N parse-only waves: all 17 parse-only rows become admission-eligible
  under strict-vs-strict comparator rules.
- Union wave(s): at least one material differential from 96/97/98, or an
  architectural-level intrinsic-block proof.
- ASM/SIMD production split: `a64_ascii_set_run_skip` may proceed only as a
  production/gate split with CSS consumer, Lock 14 parent authorization,
  equality artifacts, and fresh Criterion evidence.
- Guard maintenance: previously admitted CSS and JSON A/GO rows must not be
  silently demoted.

## Alpha-C Classification Summary

| Class | Entries / rows | Disposition for SK-V13 |
|---|---|---|
| Admitted and carried forward | 77, 78, 79, 81, 85-87, 94, 95, 99-102, 104, 105, 107, 109, 110, 111, 121-125, 127 | Carry as baseline/evidence; do not treat proof-only entries as behavior admission. |
| Rejected exact implementations | 16-18, 80, 82-84, 92, 93, 96-98, 103, 106, 108, 114-118 | Pre-block same shape; allow materially different reopen under pin. |
| Category-unblocked historical blocks | 88/89/90, 96/97/98, 119/120 | History only at category level; exact implementations remain pre-blocked. |
| Mandatory reopen set | 13 JSON direct residuals, 17 JSON parse-only rows, 23 CSS parity features after SK-V12 row | Must admit or intrinsic-block; REDRESS history is not closure authority. |
| Production-split route | 126 | Microbench proof is usable evidence, not production admission. |

The SK-V13 redress posture is therefore aggressive but bounded: reopen rows and
categories under the pin, preserve the exact rejected-route ledger, and require
fresh material differential plus measured same-plane gates for every claimed
admission.
