ACCEPT

# SK-V12 S-P1 V2 CH2: Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 GENERALITY / Lock 14.
Scope: review the SK-V12 S-P1 packet after `d1e6938a` for grammar-neutral
primitive attribution, non-JSON priority boundaries, and JSON-only / Mode III
evidence limits. This file owns only CH2 generality and Lock 14.

## Findings

1. ACCEPT - the V1 Lock 14 vocabulary defect is folded.

   PASS-1 requires hot leaves to be attributed to grammar-neutral primitives
   rather than JSON-named roles (`restart/prompts/skinny/PASS-1-PROFILE.md:129`-`:135`).
   V1 CH2 required P1-A and P1-B to replace residual labels such as
   `string_tiny_scan`, `whitespace_skip`, `dispatch_walk`,
   `container_next` / `key_colon`, `string_full_scan` / `string_escape`, and
   `unicode_escape_hex` with the P1-E canonical family vocabulary
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V1/CH2.md:101`-`:121`).
   The fold ledger records that replacement explicitly
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V1/FOLD-REVISIONS.md:22`-`:30`).

   Current P1-A now defines the primitive source map as
   `bounded_plain_string_scan`, `ascii_whitespace_skip`, `simd_movemask`,
   `container_dispatch`, `number_digit_span`, `string_escape_decode`,
   `unicode_escape_hex_decode`, `memory_copy`, and `runtime_support`
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:113`-`:125`).
   Its row table and summary use those family names instead of JSON role labels
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:132`-`:150`,
   `:157`-`:165`).

   Current P1-B likewise names product families canonically:
   `bounded_plain_string_scan`, `string_escape_decode`,
   `unicode_escape_hex_decode`, `number_digit_span`, `ascii_whitespace_skip`,
   `simd_movemask`, `container_dispatch`, `output_digest_hash`,
   `typed_direct_projection`, and `serde_json_oracle_read_parse`
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:206`-`:224`).
   Its accepted-family prose now says sequence/value dispatch rather than the V1
   `sequence/object` wording (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:226`-`:234`).
   JSON-specific function names remain only as evidence members under
   `container_dispatch`, which is acceptable for Lock 14 attribution
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:205`-`:215`).

2. ACCEPT - non-JSON priority and generated-baseline boundaries remain explicit.

   The SK-V12 opening authority requires exactly one generated non-JSON direct
   or typed parser baseline before any JSON-only micro-wave, with CSS L4
   declaration values first, then Sheets, then BBNF-self
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`-`:49`;
   `restart/skinny/tranches/sk-v12/HANDOFF.md:51`-`:67`;
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:33`-`:45`).
   P1-E preserves that priority, identifies the current `json_provider` /
   missing runtime blocker, and states that a JSON-only micro-wave before the
   non-JSON priority resolves would contradict the opening contract
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:299`-`:322`,
   `:379`-`:380`).
   P1-F independently records zero CSS L4, Sheets, or BBNF-self generated
   baseline rows and keeps generated non-JSON as the first material target
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:77`-`:83`,
   `:206`-`:207`, `:230`-`:244`).

   The packet also keeps the REDRESS 111 report lane separate from a generated
   baseline. REDRESS 111 admitted only a non-JSON report/gate lane and no row
   movement (`skinny/REDRESS.md:3284`-`:3309`), REDRESS 112 rejected the
   generated CSS L4 baseline because codegen/runtime remained JSON-profiled
   (`skinny/REDRESS.md:3313`-`:3338`), and REDRESS 113 blocked the intervention
   because no baseline Mbps existed (`skinny/REDRESS.md:3342`-`:3355`). P1-E
   carries that same distinction in its pre-block matrix
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:324`-`:336`).

3. ACCEPT - JSON-only profile facts are fenced as diagnostics, guards, or
   residual ledger rows; they are not overgeneralized into grammar proof.

   P1-A keeps `parse_only` diagnostic only and says no parse row can count toward
   SK-V12 SOTA admission or close
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:105`-`:107`,
   `:167`-`:171`). P1-B states direct rows are JSON digest-plane rows, typed rows
   are guarded JSON typed direct rows, and neither replaces the required
   non-JSON baseline
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:140`-`:143`,
   `:195`-`:204`, `:248`-`:260`). P1-E and P1-F preserve the result surface:
   16 `S / NO-GO` parse rows plus one `L / NO-GO`, 4 direct guards, 13 direct
   residuals, 7 typed guards, no generated non-JSON baseline, and unchanged
   overall `N-direct / NoGo`
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:338`-`:356`;
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:77`-`:87`).

   The direct residual reopen burden remains attached to REDRESS 119/120:
   W8 admitted no direct row, W0-clamped row, source primitive, or non-JSON
   generated intervention (`skinny/REDRESS.md:3497`-`:3527`), and W9 closed
   SK-V11 as a measured fixpoint rather than a grammar-generalization admission
   (`skinny/REDRESS.md:3531`-`:3553`). P1-C, P1-E, and P1-F carry those rows as
   pre-blocked unless a later pass supplies fresh material evidence after the
   non-JSON priority resolves
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:178`-`:199`,
   `:224`-`:245`;
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:247`-`:279`,
   `:324`-`:336`;
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:138`-`:181`).

4. ACCEPT - Mode III is not promoted beyond its actual evidence.

   P1-C says there is no fresh Mode III samply call-stack capture: `/tmp/skv12-p1`
   contains parse/direct/typed captures only, with 0/17 fresh probe call-stack
   rows and no fresh structural-scan capture
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:13`-`:19`,
   `:54`-`:63`, `:145`-`:161`). It treats W0 Mode III and structural-scan values
   as raw Criterion throughput diagnostics, not product routes or admission
   claims (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:129`-`:143`,
   `:197`-`:227`). The capture manifest repeats the same boundary and forbids
   S-P2/S-P3 from using Mode III symbols as fresh SK-V12 hot-leaf authority
   without a later explicit capture
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:160`-`:168`).

## Remediation

None required for CH2. Carry the canonical primitive-family vocabulary forward
as the load-bearing attribution surface, keep JSON source paths only as evidence
members under those families, and preserve the generated non-JSON baseline-first
entry gate before any JSON residual planning.

## Verdict

ACCEPT. The V2 packet satisfies the CH2 generality / Lock 14 bar: residual
JSON-role vocabulary has been replaced in the load-bearing S-P1 profile
surfaces, non-JSON priority and Lock 14 boundaries remain explicit, and
Mode III / JSON-only evidence is fenced from grammar-generalized claims.
