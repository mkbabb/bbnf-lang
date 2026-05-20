# SK-V12 S-P2 Hardening Converged

Pass: S-P2 Research.
Date: 2026-05-20.
Status: CONVERGED UNDER USER PIN.

The pre-pin S-P2 convergence is superseded by the 2026-05-20 user pin. The
pin-aware S-P2 research cohort opened at commit `8017a90b`, folded PIN-V1 at
`31859478`, folded PIN-V2 at `75233b2b`, recorded the first clean post-reset
cycle at `b407583e`, and converged on PIN-V4.

## §3Z Disposition

| Cycle | CH1 | CH2 | CH3 | CH4 | CH5 | CH6 | Disposition |
|---|---:|---:|---:|---:|---:|---:|---|
| PIN-V1 | REVISE 86 | ACCEPT 96 | ACCEPT 96 | REVISE 78 | ACCEPT 96 | ACCEPT 96 | Folded to V2; clean counter reset. |
| PIN-V2 | ACCEPT 96 | ACCEPT 97 | ACCEPT 97 | REVISE 89 | ACCEPT 96 | ACCEPT 97 | Folded to V3; clean counter reset. |
| PIN-V3 | ACCEPT 96 | ACCEPT 97 | ACCEPT 96 | ACCEPT 97 | ACCEPT 96 | ACCEPT 97 | Clean cycle 1 of 2. |
| PIN-V4 | ACCEPT 97 | ACCEPT 98 | ACCEPT 97 | ACCEPT 97 | ACCEPT 97 | ACCEPT 97 | Clean cycle 2 of 2; S-P2 converged. |

PIN-V3 and PIN-V4 are two consecutive clean cycles after the last REVISE. This
satisfies `ORCHESTRATOR.md` §3Z and `PASS-2-RESEARCH.md` §4 for S-P2 under the
user pin.

## Accepted Research Cohort

The accepted S-P2 research cohort is the Cycle V3 packet:

1. `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`.
2. `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`.
3. `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`.
4. `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`.
5. `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`.
6. `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`.

Load-bearing facts for S-P3:

- CSS L4 remains the authoritative first non-JSON target. Sheets and BBNF-self
  are fallback-only after measured CSS L4 redress.
- The close bar is generated CSS L4 Track 1 strictly greater than
  `lightningcss_mbps + 1` on the same corpus, same output plane, same host, and
  strict equality semantics.
- JSON `parse_only` remains diagnostic. JSON rows nominate primitive families
  and preserve guard context, but they cannot satisfy CSS L4 admission.
- Selectable PIN S-P2 candidates are limited to the candidate rows that carry
  pin S-P1 antecedents, scalar-reference status, checkasm/parity or explicit
  N/A, micro-proof or explicit N/A, same-wave consumer/proof or ineligible
  status, and orphan disposition.
- P2-C selectable aarch64 rows are C1 `a64_tbl_tbx_byte_class_mask64`, C3
  `a64_udot_digit_run_span`, C4 `a64_wide_string_special_scan64`, C5
  `a64_hex_quartet_decode_x4`, and C6 `a64_ascii_set_run_skip`. C2, C9, and
  C11 are inventory/drop in this cycle; C7, C8, C10, and C12 are support-only
  until a later folded pass adds same-wave consumer evidence and prerequisites.
- P2-D contributes no current shortlist-ready tape/union primitive. Any
  same-tape CSS-local union route is conditional after generated CSS Track 1,
  same-plane lightningcss comparator, strict equality, CSS hot-leaf attribution,
  REDRESS 96/97/98 material differential, and CHALLENGE.
- `escape_mask_64` remains a correctness prerequisite before new SIMD
  admission.
- USER PIN D3/D4 unblock union and ASM-gen categories only at the category
  level. Historical REDRESS implementations remain material-differential
  evidence, not implementation authority.
- Zero orphan aarch64 production primitives remains a campaign close target.

Next move: `ready-for-S-P3-synthesis-sk-v12`.
