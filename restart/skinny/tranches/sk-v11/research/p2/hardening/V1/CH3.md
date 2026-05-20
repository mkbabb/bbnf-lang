# SK-V11 S-P2 CH3 Regression
Pass: S-P2 CHALLENGE. Cycle: V1.
Date: 2026-05-19.
Scope: REDRESS regression review of S-P2 candidates.
Output: this file.
Disposition: REVISE.
Accept rate contribution: 0.

## Findings
1. Major - The x4 escape "production" candidates reopen REDRESS 107/108 unless
   downgraded to proof-only. P2-B names `ESCAPE_UXXXX_X4_PRODUCTION` and sends
   it to the exact `unescape_string` caller (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:258`).
   P2-C repeats this as "x4 escape codec production" that packs four quartets,
   joins surrogate pairs, and emits chars (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:32`-`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:33`).
   REDRESS 107 explicitly admits only a proof with no `RESULTS.md` row or new
   production behavior (`skinny/REDRESS.md:3174`-`skinny/REDRESS.md:3196`),
   and REDRESS 108 rejects reusing the already-consuming `unescape_string`
   caller without a real source delta (`skinny/REDRESS.md:3200`-`skinny/REDRESS.md:3222`).
   Fresh S-P1 evidence names `unicode_escape_hex_decode`, but not a new caller
   or source delta that escapes REDRESS 108 (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:41`-`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:52`).

2. Major - The widened/full string-block family is too close to rejected string
   routes. P2-C lists a 64-byte widened `StringSpecialBlock` candidate
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:30`-`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:31`),
   while P2-A keeps a `string_special_block16_or64` candidate with widening
   only caveated as research (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:57`-`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:66`).
   REDRESS 61 and 62 rejected always-wide and delayed-wide retained trusted
   string scans after same-wave scalar/checkasm/caller attempts
   (`skinny/REDRESS.md:1382`-`skinny/REDRESS.md:1488`); REDRESS 83 rejected the
   generated-retained StringBlock16 wrapper (`skinny/REDRESS.md:2320`-`skinny/REDRESS.md:2356`);
   REDRESS 106 rejected the full string primitive micro-proof as caller-level
   insufficient despite primitive correctness (`skinny/REDRESS.md:3152`-`skinny/REDRESS.md:3170`).
   V2 must make the non-reopening boundary explicit: scalar bounded-string
   factoring is eligible, but 64-byte retained scan, tiny NEON parser wiring,
   StringBlock16 wrapper, and "primitive parity implies production" are not.

3. Major - Container-dispatch candidates risk reopening object/key/value-byte
   carry. P2-D's `container_tail_next` is phrased for an "element/member" and
   all output planes (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:30`),
   and P2-F's C6 permits generated dispatch to carry a next byte
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:37`).
   The only admitted prior shape is the generated array `ContainerNext` /
   next-byte carry, with no generic crate or sidecar leak (`skinny/REDRESS.md:1492`-`skinny/REDRESS.md:1501`).
   Object next-key carry and object-pair value-byte compaction are explicitly
   rejected (`skinny/REDRESS.md:1639`-`skinny/REDRESS.md:1684`,
   `skinny/REDRESS.md:2360`-`skinny/REDRESS.md:2395`). V2 must split "generated
   FIRST-set/container dispatch" from any object/key/value-byte carry and mark
   the latter out of S-P3 eligibility without fresh P1 evidence plus a materially
   different row plan.

4. Minor - The W3/substrate surface is mostly guarded, but P2-A and P2-D need a
   harder negative statement. P2-A's `live_mask_to_tape_or_sink` allows a live
   mask to write tape or sink events and says a retained class column is allowed
   only if it is the tape projection (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:136`-`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:149`).
   P2-D's `tape_sparse_flag_delta_lane` is inside the existing tape, but is
   motivated by sparse flag and materialization counters (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:34`).
   REDRESS 96/97 rejected class-column and streaming-cursor W3 implementations
   after correctness-green measurement, REDRESS 98 retired the union-substrate
   thesis, and REDRESS 102 makes W3 proof-only with no behavior/source row
   movement (`skinny/REDRESS.md:2797`-`skinny/REDRESS.md:2949`,
   `skinny/REDRESS.md:3042`-`skinny/REDRESS.md:3058`). V2 must say retained
   structural class lanes, parser-owned projection, structural vectors, and
   streaming cursors are non-candidates; only transient same-loop producers
   feeding the existing tape/direct sink may remain.

5. Major - Several candidate descriptions still let retained parse callers act
   like admission consumers. P2-D's D1 includes retained parse tape emission
   (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:30`), and
   P2-E's bounded string candidate names generated retained string/key parse
   paths as same-wave consumers (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:63`-`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:65`).
   S-P1 accepts parse-only, PMU, structural scan, masking, and lazy-tape facts as
   diagnostic planning evidence only (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:34`-`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:48`).
   REDRESS 102 records 17 parse rows and no parse row outside `S / NO-GO`, and
   bars W4+ from naming W3 as substrate dependency (`skinny/REDRESS.md:3042`-`skinny/REDRESS.md:3058`).
   V2 must distinguish retained-parse micro-proofs/guards from direct or typed
   product-plane admissions.

6. Minor - Numeric fallback and bitmap-body regressions are correctly blocked in
   substance. P2-A's digit candidate disallows mantissa/table-only fallback
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:107`-`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:120`),
   and P2-E confines digit work to run/span accumulation without f64 fallback
   changes (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:71`-`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:86`).
   P2-C keeps PMULL and CSSC CTZ as inventory rather than production routes
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:18`-`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:19`).
   This matches REDRESS 80, 88, and 89 (`skinny/REDRESS.md:2217`-`skinny/REDRESS.md:2248`,
   `skinny/REDRESS.md:2510`-`skinny/REDRESS.md:2585`). V2 should preserve this
   wording unchanged.

## Required folds
1. Downgrade `ESCAPE_UXXXX_X4_PRODUCTION` / x4 escape codec production to
   proof-only escaped-segment research, or drop it. Any S-P3 production
   candidate must name a new source delta and product-plane consumer beyond the
   already-consuming `unescape_string` path.
2. Reframe string-block candidates so V2 admits only scalar bounded-string
   factoring or a narrowly different proof. Explicitly exclude 64-byte retained
   scans, StringBlock16 generated-retained wrappers, NEON tiny-string parser
   wiring, and primitive-parity-only production claims.
3. Split container dispatch into admissible generated scalar/FIRST-set dispatch
   and pre-blocked object/key/value-byte carry. Keep REDRESS 63's array
   next-byte carry as historical precedent only, not a blanket object member
   carry admission.
4. Add an explicit W3 pre-block to P2-A/P2-D language: no retained structural
   class column, structural vector, streaming cursor, parser-owned projection,
   sidecar, or parse-only movement. Same-loop transient masks may feed only the
   existing tape or direct/typed sink.
5. Mark retained parse call sites and parse-only rows as proof/guard surfaces.
   Candidate admission for S-P3 must be direct or typed product-plane, or a
   non-JSON generated direct/typed parser intervention, with same-wave measured
   consumer evidence.

## Accepted facts
None. CH3 V1 is REVISE; no S-P3 fact is accepted until the REDRESS folds above
land in V2.
