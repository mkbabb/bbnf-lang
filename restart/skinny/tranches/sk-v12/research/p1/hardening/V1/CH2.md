REVISE

# SK-V12 S-P1 V1 CH2: Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 GENERALITY / Lock 14.
Scope: review SK-V12 S-P1 artifacts P1-A through P1-F, PASS-1-PROFILE,
SK-V12 SYNTHESIS/HANDOFF/G-Alpha, REDRESS 111-120, and SK-V11 P1/P2
grammar-neutral precedent. This file owns only CH2 generality and Lock 14.

## Findings

1. REVISE - canonical primitive vocabulary exists, but the packet has residual
   non-canonical and JSON-role wording in load-bearing profile surfaces.

   P1-E has the correct bridge: canonical hot families are
   `bounded_plain_string_scan`, `string_escape_decode`,
   `unicode_escape_hex_decode`, `number_digit_span`,
   `ascii_whitespace_skip`, `container_dispatch`, `simd_movemask`, and
   `output_digest_hash` (`p1e-hot-leaf-attribution.md:182`-`:198`,
   `:327`-`:332`). P1-B mostly follows that bridge in its product-family table
   (`p1b-samply-mode-2.md:208`-`:219`).

   The remaining Lock 14 problem is narrow. P1-A's primitive source map and
   row table still use local aliases as primitive labels:
   `string_tiny_scan`, `whitespace_skip`, `dispatch_walk`, `number_scan`,
   `container_next` / `key_colon`, `string_full_scan` / `string_escape`, and
   `unicode_escape_hex` (`p1a-samply-mode-1.md:113`-`:128`,
   `:130`-`:148`). P1-A also summarizes rows as "String/object-heavy" and
   "dispatch/key-colon leaves" (`p1a-samply-mode-1.md:155`-`:163`).
   P1-B's accepted product-family summary includes "sequence/object value
   dispatch" (`p1b-samply-mode-2.md:221`-`:229`). Those are not merely
   row-local source symbols; they are profile attribution wording. The
   PASS-1 CH2 rule requires primitive names rather than JSON roles
   (`restart/prompts/skinny/PASS-1-PROFILE.md:129`-`:135`).

   SK-V11 precedent makes this exact bar load-bearing: V2 CH2 kept REVISE for
   residual `array-walk`, `object leaves`, and `Number/array rows` summary
   wording (`restart/skinny/tranches/sk-v11/research/p1/hardening/V2/CH2.md:33`-`:47`,
   `:60`-`:72`), while V4 accepted only after load-bearing summaries used the
   canonical primitive families and kept JSON/generated/serde names as evidence
   only (`restart/skinny/tranches/sk-v11/research/p1/hardening/V4/CH2.md:13`-`:29`).

2. ACCEPT - JSON-only profile facts are not promoted into grammar-generalization
   proof.

   P1-B states that `direct_to_struct` rows are JSON digest-plane rows and that
   `real_typed_struct` rows are guarded JSON typed rows, not non-JSON baselines
   (`p1b-samply-mode-2.md:129`-`:137`). Its delta table explicitly says no
   non-JSON product row exists and JSON product profiling does not substitute
   for the generated non-JSON baseline (`p1b-samply-mode-2.md:236`-`:248`).
   P1-F records 41/41 live RESULTS rows as unchanged JSON rows, with zero CSS
   L4, Sheets, or BBNF-self generated baseline rows (`p1f-results-delta.md:77`-`:87`,
   `:206`-`:207`). This matches SK-V11 P2 precedent: non-JSON generality must
   be measured through a generated direct/typed parser; prose or JSON-only
   telemetry is insufficient (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:23`-`:32`).

3. ACCEPT - the CSS/Sheets/BBNF-self baseline blocker is accurately surfaced.

   REDRESS 111 admitted only a non-JSON report/gate lane and did not create a
   generated baseline (`skinny/REDRESS.md:3284`-`:3309`). REDRESS 112 rejected
   the generated CSS L4 baseline because codegen still routes through
   `json_provider::ensure_runtime_profile` and no generated CSS L4 runtime
   exists (`skinny/REDRESS.md:3313`-`:3338`). REDRESS 113 then blocked the
   intervention because there was no baseline Mbps to consume
   (`skinny/REDRESS.md:3342`-`:3355`).

   P1-E rechecks the current tree and reports the same blocker:
   `json_provider` accepts only `backend.grammar_name == "json"`, direct and
   typed emission call that provider, runtime contains generated `json` plus
   `sheets_witness` but no generated `css_l4`, `css_l4_declaration_values`,
   `sheets`, or `bbnf_self`, and the W1a report lane is not a generated Track 1
   baseline (`p1e-hot-leaf-attribution.md:275`-`:298`). P1-F states the same
   priority and distinguishes the REDRESS 111 report lane from a generated
   baseline (`p1f-results-delta.md:167`-`:193`). This matches SK-V12 opening
   authority: generated non-JSON baseline first, preferred order CSS L4
   declaration values, Sheets, then BBNF-self
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`-`:49`;
   `restart/skinny/tranches/sk-v12/HANDOFF.md:51`-`:58`).

4. ACCEPT - parse-only and direct residual pre-blocks are preserved.

   P1-A keeps parse-only diagnostic and says no parse-only row admits
   (`p1a-samply-mode-1.md:105`-`:107`, `:165`-`:188`). P1-C preserves
   `parse_only` as diagnostic, the 13 direct residual rows as pre-blocked, and
   the REDRESS 119/120 reopen burden (`p1c-samply-mode-3.md:157`-`:190`,
   `:223`-`:240`). P1-E carries the same result surface and pre-block matrix
   (`p1e-hot-leaf-attribution.md:203`-`:205`, `:227`-`:257`,
   `:300`-`:312`, `:316`-`:325`). P1-F ties the row table to REDRESS 119/120
   and records the unchanged surface: 16 `S / NO-GO` plus one `L / NO-GO`
   parse rows, four direct guards, thirteen `N-direct / NO-GO` residuals,
   seven typed guards, and overall `N-direct / NoGo`
   (`p1f-results-delta.md:138`-`:165`, `:230`-`:244`).

   This agrees with REDRESS 119 and 120: W8 admitted no direct row, no
   W0-clamped row, no source primitive, and no non-JSON generated intervention
   (`skinny/REDRESS.md:3497`-`:3527`); W9 closed SK-V11 as a fixpoint, not a
   grammar-generalization admission, and routed SK-V12 to the generated
   non-JSON baseline first (`skinny/REDRESS.md:3531`-`:3553`).

## Required Fold

1. Canonicalize P1-A's primitive labels to the P1-E vocabulary:
   `string_tiny_scan` -> `bounded_plain_string_scan`,
   `whitespace_skip` -> `ascii_whitespace_skip`,
   `dispatch_walk` / `container_next` / `key_colon` ->
   `container_dispatch`, `number_scan` / `number_digit_scan` ->
   `number_digit_span`, `string_full_scan` / `string_escape` ->
   `string_escape_decode`, and `unicode_escape_hex` ->
   `unicode_escape_hex_decode`.
2. Rewrite P1-A summary prose from "String/object-heavy" and
   "dispatch/key-colon leaves" to `bounded_plain_string_scan` plus
   `container_dispatch` wording.
3. Rewrite P1-B's accepted product-family phrase "sequence/object value
   dispatch" to `container_dispatch` / generated sequence dispatch wording.
4. Preserve JSON source function names such as `parse_object_value_at_direct`,
   `parse_array_element_at_direct`, and `parse_key_colon` only as row-local
   evidence members under `container_dispatch`.
5. Do not change row classifications, capture facts, gate status, or the
   SK-V12 priority order. No new profile capture, source change, gate change, or
   `skinny/RESULTS.md` movement is required for this CH2 fold.

## Verdict

REVISE. The packet is structurally sound on the non-JSON blocker, JSON-only
evidence boundary, and parse/direct pre-blocks. The required fold is a narrow
Lock 14 vocabulary repair so all load-bearing profile attribution uses
grammar-neutral primitive names before S-P2 consumes the P1 packet.
