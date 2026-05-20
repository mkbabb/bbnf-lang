# SK-V12 Alpha Hardening V4 - CH1 Correctness

Date: 2026-05-20.

Scope: correctness review of the pin-aware V4 Pass Alpha packet at head
`3e5dd574`, including the folded `research/g-alpha/G-ALPHA-SK-V12.md`.

## Verdict

PASS.

Required folds: none.

## Findings

1. Strict lightningcss admission is consistent. The user pin requires CSS L4
   first and raises close to generated CSS L4 beating lightningcss on the same
   corpus/output plane with strict equality
   (`USER-PIN-W1-CSS-L4-SOTA.md:18-35`). The packet carries the strict integer
   gate as `generated_track1_mbps > lightningcss_mbps + 1`, with equality at
   `+1` a miss, in `SYNTHESIS.md:42-53`, `HANDOFF.md:53-58` and `:117-118`,
   `alpha-B-competitor-deltas.md:37-58`, `alpha-E-candidate-shortlist.md:38-41`
   and `:116-128`, `alpha-F-contract-draft.md:71-84` and `:228-231`, and
   `research/g-alpha/G-ALPHA-SK-V12.md:56-60`.

2. CSS output-plane/equality is bound to one symmetric fact stream. Generated
   Track 1, independent Track 2/oracle, and lightningcss share the selected CSS
   fact plane; bbnf-only or lightningcss-only bridges are rejected
   (`alpha-B-competitor-deltas.md:84-113`,
   `alpha-E-candidate-shortlist.md:79-86` and `:105-123`,
   `alpha-F-contract-draft.md:71-84`,
   `research/g-alpha/G-ALPHA-SK-V12.md:58-62`). The gate/provenance schema
   consumes output plane, strictness, oracle, lightningcss artifact, equality,
   sample, host, profile, and guard state (`SYNTHESIS.md:183-204`,
   `HANDOFF.md:139-151`, `alpha-F-contract-draft.md:173-192`,
   `research/g-alpha/G-ALPHA-SK-V12.md:88-109`).

3. Pass order is correct. PASS-ALPHA requires user G-Alpha before SK-V12 P1 and
   no G-Alpha without CHALLENGE convergence
   (`PASS-ALPHA.md:167-178`, `:180-205`). The current contract authorizes only
   G-Alpha, then pin-aware S-P1, S-P2, and S-P3 before any implementation packet
   or W0-W5 wave authority (`SYNTHESIS.md:5-11` and `:235-254`,
   `HANDOFF.md:103-108` and `:168-173`, `alpha-F-contract-draft.md:11-16` and
   `:146-171`, `research/g-alpha/G-ALPHA-SK-V12.md:36-48` and `:126-131`).

4. Close conditions are measurable and mutually consistent. ADMIT requires the
   strict CSS/lightningcss row, gate-consumed equality/provenance, Lock 14/16,
   zero carried orphan primitives, JSON guard disposition, and close-doc
   agreement (`SYNTHESIS.md:33-70`, `HANDOFF.md:70-85`,
   `alpha-F-contract-draft.md:63-99`, `research/g-alpha/G-ALPHA-SK-V12.md:50-73`).
   FIXPOINT requires ADMIT to be measured uncloseable, a measured CSS redress
   before Sheets/BBNF-self fallback, new measured union and ASM-gen attempts, and
   zero orphan production SIMD primitives (`SYNTHESIS.md:72-94`,
   `HANDOFF.md:87-95`, `alpha-F-contract-draft.md:100-120`,
   `research/g-alpha/G-ALPHA-SK-V12.md:74-86`).

5. Evidence citations and rollback facts are adequate for CH1. Alpha-A records
   absent generated CSS L4 evidence and the unmeasured lightningcss floor
   (`alpha-A-results-extraction.md:52-72`) and ties the carried JSON result
   surface to `skinny/RESULTS.md` plus REDRESS 120
   (`alpha-A-results-extraction.md:89-95`). Alpha-B cites REDRESS 111-113/120
   and local CSS comparator/parity files for CSS authority
   (`alpha-B-competitor-deltas.md:62-82`) and treats CSS deltas as `UNMEASURED`
   until required fields exist (`alpha-B-competitor-deltas.md:84-127`). W0 is
   revalidated, not redone (`SYNTHESIS.md:228-233`, `HANDOFF.md:97-101`,
   `alpha-F-contract-draft.md:166-167`), and W1a-W4 carry rejected-patch or
   dependency rollback paths (`alpha-E-candidate-shortlist.md:135-138`,
   `:188-191`, `:227-229`, `:282-284`, `:341-345`).

6. The V3 G-Alpha blocker is folded. V3 required replacement of the stale
   pre-pin/V2 G-Alpha presentation with a pending, pin-aware one carrying CSS
   first, strict lightningcss floor, telemetry, zero-orphan close, and union/ASM
   category unblocks (`alpha-hardening/V3/CONSOLIDATED.md:28-46`). The current
   G-Alpha presentation no longer claims `G-Alpha PASS`; it is pending hardening
   (`research/g-alpha/G-ALPHA-SK-V12.md:7-12` and `:126-131`), carries the
   strict CSS-first close contract (`:50-86`), carries telemetry fail-closed
   requirements (`:88-109`), and repeats the S-P1/S-P2/S-P3 plus W0-W5 seed
   table with failure actions (`:111-124`).
