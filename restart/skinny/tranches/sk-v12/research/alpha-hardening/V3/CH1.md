# SK-V12 Alpha Hardening V3 - CH1 Correctness

Date: 2026-05-20.

Scope: correctness review of the current Pass Alpha packet at head `b9b5d9de`
under `USER-PIN-W1-CSS-L4-SOTA.md`, with emphasis on strict lightningcss gate
semantics, CSS output-plane/equality consistency, pass order, close conditions,
evidence citations, rollback facts, and the V2 CH5 folds in Alpha-E.

## Verdict

PASS.

No CH1 correctness blockers remain. Required folds: none.

## Findings

1. Strict lightningcss admission semantics are consistent. The pin requires CSS
   L4 first and closes only when generated CSS L4 beats lightningcss on the same
   corpus/output plane with strict equality
   (`USER-PIN-W1-CSS-L4-SOTA.md:18-35`). The packet uses the strict integer gate
   `generated_track1_mbps > lightningcss_mbps + 1`, with equality at `+1` a
   miss, in `SYNTHESIS.md:42-53`, `HANDOFF.md:53-58` and `:117-118`,
   `alpha-B-competitor-deltas.md:37-58`, `alpha-E-candidate-shortlist.md:38-41`
   and `:116-128`, and `alpha-F-contract-draft.md:71-84` and `:228-231`.

2. CSS output-plane and equality are bound to one plane. The canonical CSS fact
   stream is shared by generated Track 1, independent Track 2/oracle, and
   lightningcss; symmetric fact extraction rejects bbnf-only or
   lightningcss-only bridges (`alpha-B-competitor-deltas.md:84-113`,
   `alpha-E-candidate-shortlist.md:79-86`, `:105-123`,
   `alpha-F-contract-draft.md:71-84`). The telemetry gate also consumes output
   plane, strictness, oracle, lightningcss artifact, equality, sample, host, and
   guard provenance (`SYNTHESIS.md:183-204`, `HANDOFF.md:139-151`,
   `alpha-F-contract-draft.md:173-192`).

3. Pass order is pin-aligned. PASS-ALPHA keeps G-Alpha ahead of SK-V12 dispatch
   (`PASS-ALPHA.md:167-178`, `:201-205`). The current packet requires G-Alpha,
   then fresh pin-aware S-P1, S-P2, and S-P3 before implementation authority
   (`SYNTHESIS.md:5-11`, `:237-254`; `HANDOFF.md:103-108`, `:168-173`;
   `alpha-F-contract-draft.md:11-16`, `:146-171`). Alpha-E now qualifies pre-pin
   S-P artifacts as context only after measured revalidation
   (`alpha-E-candidate-shortlist.md:23-27`).

4. Close conditions remain measurable and mutually consistent. ADMIT requires
   CSS L4/lightningcss strict admission, Lock 14/16 evidence, zero carried
   aarch64 orphans, JSON guards, and close-doc agreement
   (`SYNTHESIS.md:33-70`, `HANDOFF.md:70-85`,
   `alpha-F-contract-draft.md:63-99`). FIXPOINT requires a measured CSS redress
   attempt before fallback, plus new measured union and ASM-gen attempts and
   zero orphan production primitives (`SYNTHESIS.md:72-94`, `HANDOFF.md:87-95`,
   `alpha-F-contract-draft.md:100-120`). Sheets and BBNF-self stay
   post-CSS-redress only (`alpha-E-candidate-shortlist.md:347-355`).

5. Evidence citations are adequate for CH1. Alpha-A ties the carried result
   surface and guard floors to `skinny/RESULTS.md`, REDRESS 119, and REDRESS 120
   (`alpha-A-results-extraction.md:89-95`, `:168-174`). SYNTHESIS, HANDOFF, and
   Alpha-F carry the same local result/REDRESS citations
   (`SYNTHESIS.md:110-148`, `HANDOFF.md:44-47`,
   `alpha-F-contract-draft.md:136-139`). Alpha-B cites the absent CSS admission
   and comparator authority through REDRESS, local CSS bench/parity files, and
   lockfiles (`alpha-B-competitor-deltas.md:62-82`) and treats missing CSS rows
   as `UNMEASURED`, not wins (`alpha-B-competitor-deltas.md:102-127`).

6. Rollback and stale-authority facts are explicit. W0 is revalidated, not
   redone, unless drift is measured (`SYNTHESIS.md:228-233`, `HANDOFF.md:97-101`,
   `alpha-F-contract-draft.md:166-167`). W1a-W4 carry rejected-patch paths and
   dependent-wave blocking/replan behavior (`alpha-E-candidate-shortlist.md:135-138`,
   `:188-191`, `:227-229`, `:282-284`, `:341-345`), and the G-Alpha seed repeats
   the wave failure actions (`SYNTHESIS.md:260-271`,
   `alpha-F-contract-draft.md:215-226`).

7. The V2 CH5 folds landed in Alpha-E. V2 required E2/W1a to inherit the full
   JSON guard refresh/demotion rule and required Alpha-E to qualify `SPEC.md`
   plus pre-pin S-P artifacts (`alpha-hardening/V2/CONSOLIDATED.md:27-38`;
   `alpha-hardening/V2/CH5.md:140-145`). Alpha-E now marks `SPEC.md` as pre-pin
   context only where it does not conflict with the pin and marks S-P1/S-P2/S-P3
   artifacts as context only after measured revalidation
   (`alpha-E-candidate-shortlist.md:14-27`). E2/W1a now requires direct/typed
   JSON guard refresh or measured REDRESS demotion unless no JSON-producing path
   moved and `skinny/RESULTS.md` is proven unchanged
   (`alpha-E-candidate-shortlist.md:179-182`).
