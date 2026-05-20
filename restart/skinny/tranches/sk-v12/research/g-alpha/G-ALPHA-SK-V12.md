# SK-V12 G-Alpha Presentation

Date: 2026-05-20.

Gate: G-Alpha.
Tranche: SK-V12.
Status: PENDING V4 HARDENING.

This presentation replaces the stale pre-pin/V2 presentation. It is not a
`G-Alpha PASS` record yet. G-Alpha is presented only after the pin-aware Alpha
packet and this presentation converge under hardening with no unresolved
REVISE disposition.

## Authority

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`
- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V3/CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 120

`restart/skinny/tranches/sk-v12/SPEC.md` and the pre-pin S-P1/S-P2/S-P3
artifacts are context only where measured, revalidated, and non-conflicting
with the user pin. They are not implementation dispatch authority after the
pin.

## Presented Contract

The pin-aware SK-V12 Alpha packet asks G-Alpha to authorize the next pass
sequence only:

1. SK-V12 S-P1 Profile under the user pin.
2. SK-V12 S-P2 Research under the user pin.
3. SK-V12 S-P3 Synthesis-Plan under the user pin.
4. Downstream W0-W5 wave dispatch only after S-P3 materializes replacement
   `SPEC.md` and `DISPATCH-PROMPT.md` authority.

Alpha does not authorize behavior source edits, does not create replacement
`SPEC.md`, and does not create replacement `DISPATCH-PROMPT.md`.

## Close Contract

SK-V12 closes only by ADMIT or FIXPOINT.

ADMIT:

- CSS L4 is authoritative and first. Sheets and BBNF-self are fallbacks only
  after a CSS L4 redress attempt records measured BLOCKED or REJECTED evidence.
- A generated CSS L4 row admits only when generated Track 1 Mbps is strictly
  `> lightningcss_mbps + 1` on the same corpus, same output plane, same host,
  and strict equality semantics. Equality at `+1` is a miss.
- The output plane is one canonical CSS fact stream shared symmetrically by
  generated Track 1, independent Track 2/oracle, and lightningcss.
- Lock 14 is executable: generic JSON policy leaks must be extracted through
  `GrammarConfig` or equivalent grammar-derived metadata before CSS emission is
  legal.
- Lock 16 is executable: the `escape_mask_64` NEON correctness blocker is
  verified and resolved before any new SIMD admission.
- The carried aarch64 orphan set is admitted, removed, or inventory-demoted
  with evidence: `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
  `bulk_emit_positions_64`, `byte_context`, and `cache_hints`.
- JSON direct/typed guard floors hold, or misses are recorded as measured
  REDRESS demotions. `parse_only` is diagnostic-only.

FIXPOINT:

- ADMIT is measured uncloseable across a full Pass Alpha bracket.
- CSS L4 has at least one measured redress attempt before Sheets/BBNF-self are
  considered.
- The closing tranche records a new measured union-substrate implementation
  attempt and a new measured ASM-gen implementation attempt.
- Union attempts cite REDRESS 96/97/98; ASM-gen attempts cite REDRESS 88/89/90
  where adjacent. These are historical implementation evidence, not
  category-level blockers after the user pin.
- Both attempts carry fresh profile, microbench, equality/parity, scalar or
  reference proof, and same-wave consumer evidence.
- Orphan production SIMD primitives at close invalidate FIXPOINT.

## Telemetry Required At G-Alpha

The downstream S-P3 packet and waves must make the CSS L4 gate or companion
report fail closed on missing:

- `row_id`, `grammar_id`, `domain`, `workload`, `output_plane`, and
  `strictness`;
- generated Track 1 source path and generated runtime path;
- grammar source or generated metadata checksum;
- fixture/input provenance and byte count;
- independent oracle or Track 2 source path and independence status;
- lightningcss comparator command, artifact, version, strictness, output
  plane, and Mbps;
- strict equality result and measured validation path;
- Track 1 Mbps, oracle Mbps, sample count, sample cost, run id, host triple,
  feature mask, build flags, profile artifact, and benchmark artifact;
- Lock 14 status, Lock 16 status where applicable, same-wave consumer class,
  JSON guard state, gate status, wave id, and REDRESS id.

Producer-only fields, stale run ids, oracle coupling, grammar-name branches in
generic crates, missing lightningcss evidence, parse-only admission, and orphan
SIMD primitives reject the wave.

## G-Alpha Seed

| Seed | Target row / role | Hand LOC cap | Minute caps | REDRESS adjacency | Close contribution | Failure action |
|---|---|---:|---|---|---|---|
| S-P1 | Fresh JSON 17-corpus plus CSS L4 target profile | docs/profiling | pass prompt | profile-truth audit | establishes open baseline | rerun until §3Z or BLOCKED |
| S-P2 | CSS/lightningcss, Lock 14/16, union, ASM-gen research | docs | pass prompt | REDRESS 88/89/90, 96/97/98, 112/113, 119/120 | names legal routes | rerun until §3Z or BLOCKED |
| S-P3 | SPEC + DISPATCH-PROMPT under pin | docs | pass prompt | all carried | materializes wave authority | REVISE on stale pre-pin gate |
| W0 | Revalidate `f788eb97` telemetry/gate lock | docs-only | 20/15/30 | W0 lock | preserves open surface | return to S-P3 on drift |
| W1a | `GrammarConfig` legality / JSON parity | <=360 | 20/15/30 | Lock 14 leaks | legalizes CSS emission | save `/tmp/skv12-waveW1a-rejected.patch` |
| W1b | CSS L4 generated baseline + lightningcss comparator | <=620 | 20/15/30 | REDRESS 112/113 | possible ADMIT | save `/tmp/skv12-waveW1b-rejected.patch` |
| W2 | `escape_mask_64` correctness | <=180 | 20/15/30 | Lock 16 bug | unblocks SIMD | save `/tmp/skv12-waveW2-rejected.patch` |
| W3 | CSS-local same-tape union attempt | <=420 | 20/15/30 | REDRESS 96/97/98 | ADMIT lift or FIXPOINT union evidence | save `/tmp/skv12-waveW3-rejected.patch` |
| W4 | ARMv9.2 TBL/TBX or selected ASM-gen consumer | <=430 | 20/15/30 | REDRESS 88/89/90 | ADMIT lift or FIXPOINT ASM evidence, zero-orphan disposition | save `/tmp/skv12-waveW4-rejected.patch` |
| W5 | Close / G-Alpha feedback | docs-only | 20/15/30 | REDRESS close | ADMIT or measured FIXPOINT | synthesize SK-V13 if close unmet |

## Gate Result

PENDING V4 HARDENING.

When hardening accepts this presentation, G-Alpha may authorize SK-V12 S-P1
Profile under the user pin. Until then, no implementation wave is dispatchable.
