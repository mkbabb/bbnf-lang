# SK-V13 S-P2 V2 CH2: Generality / Lock 14

## Verdict

ACCEPT.

## Evidence

- The CH2 contract is exact: every candidate must carry a P2-F
  grammar-neutral verdict, and a JSON-only primitive with no byte-set,
  classifier, tape, or per-grammar-template expression must be revised or
  rejected (`restart/prompts/skinny/PASS-2-RESEARCH.md:102`-`107`).
  Lock 14 remains the controlling rule: generic crates may not carry
  grammar-named modules, grammar-specific public types, grammar feature
  flags, or grammar `match` arms; per-grammar runtime modules must be
  generated from one grammar-agnostic template (`restart/locks/LOCKS.md:78`).

- V1's CH2 blocker was precise: P2-F covered B/C/D/E but did not
  explicitly cross-read P2-A or provide literal C1-C8 verdicts
  (`restart/skinny/tranches/sk-v13/research/p2/hardening/V1/CH2.md:34`-`42`).
  The V1 consolidation folded that as a required V2 action: P2-F had to
  include P2-A in cross-read/sources and map C1-C8 into the P2-F verdict
  vocabulary (`restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:31`-`46`).

- V2 resolves that blocker. P2-F is stamped Cycle V2 and scopes itself to
  sibling P2-A/B/C/D/E V2 artefacts (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:3`-`8`).
  Its findings state that the cross-read is complete against P2-A/B/C/D/E
  and that P2-A contributes C1-C8 comparator-led candidates
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:12`-`18`).
  Its source list now includes P2-A alongside P2-B/C/D/E
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:223`-`227`),
  and its V2 cross-read disposition says the literal C1-C8 mapping resolves
  the cross-read blocker (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:229`-`236`).

- P2-A C1-C8 now carry P2-F verdicts. P2-A defines C1-C8 as
  `class_mask64_transient`, `bounded_special_string_end`,
  `escape_segment_hex_decode`, `digit_run_accumulate`,
  `generated_first_follow_probe`, `same_loop_structural_mask_consume`,
  `ascii_set_member_find64_css`, and `output_digest_fold_u64x2_sink`
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:70`-`81`).
  P2-F maps those same eight candidates literally to admissible,
  conditional, or fact-stream-only verdicts, with rejection boundaries for
  retained sidecars, JSON object/array/key-colon branches, parser-speed
  digest claims, and `JsonDigestSink` internals
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:104`-`115`).

- P2-B candidates are covered by P2-F verdict rows or inventory status.
  B1 escape decode maps to the unicode/escape decoder verdict; B2 maps to
  `ByteSetRunSkip64`; B3 maps to grammar structural scan / same-substrate
  union routes; B4's prefix-XOR, next-bit, and bulk-emission support
  primitives are inventory or conditional-only; B5 maps to the byte-set
  classifier verdict (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:50`-`118`;
  `restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:78`-`88`).
  P2-B's own Lock-14 gate also requires grammar-supplied quote, escape,
  control, delimiter, numeric, or no-string/no-number policy before shared
  code admission (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:120`-`133`).

- P2-C candidates C-P2C-1..7 are covered. P2-C stamps C-P2C-6 and
  C-P2C-7 as `NOT-S-P3-ELIGIBLE` inventory/close hygiene
  (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:30`-`43`),
  and its own grammar-neutrality table classifies C-P2C-1..7 as byte-set,
  parameterized structural, decimal digit, partially generalizable escape,
  policy-parameterized string, unsupported neutral bit algebra, or hygiene
  (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:45`-`55`).
  P2-F carries corresponding rows for byte-set run-skip, byte-set
  classifier, UDOT digit-run, PMULL/CSSC structural scan, string-special
  scan, escape decode, EOR3, TBX/LD4 refinements, and byte-context
  inventory/drop status (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:78`-`92`).

- P2-D D1-D5 are covered. P2-D itself classifies D1 as not standalone
  S-P3 eligible, then defines D2 same-tape event projection, D3
  mask-to-tape writer, D4 SinkOnly event adapter, and D5 sparse flag policy
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:146`-`351`).
  Its grammar-neutrality table requires metadata-driven capacity,
  generated event/fact vocabulary, grammar-supplied structural alphabets,
  generated per-grammar sink traits, and physical-only flag bits
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:365`-`373`).
  P2-F maps D1 to `NOT-S-P3-ELIGIBLE`, D2/D3 to union C1/C2/C3 conditional
  same-substrate verdicts, D4 to generated DirectSink/view emission, and
  D5 to per-grammar flag policy (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:76`-`77`,
  `:85`, `:100`-`:102`).

- P2-E P2E-1..8 are covered by direct P2-F rows. P2-E defines the eight
  parse-that gaps and gives each JSON/CSS/Sheets/BBNF-self status
  (`restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md:35`-`134`).
  P2-F maps P2E-1 string policy, P2E-2 string-special scan, P2E-3 escape
  decode, P2E-4 number policy, P2E-5 digit-run, P2E-6 byte-set run-skip,
  P2E-7 regex analysis, and P2E-8 grammar structural scan into admissible
  or conditional grammar-neutral verdict rows
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:74`-`84`,
  `:95`).

- P2-F preserves explicit rejection and inventory status for non-neutral or
  support-only routes. It rejects JSON object/array/key-specific dispatch,
  `JsonSink`-specific acceleration, hardcoded JSON quote/backslash widening,
  and any grammar-neutrality claim based only on JSON profile envelopes
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:146`-`161`).
  It also binds inventory/drop decisions for EOR3/BCAX, cache hints,
  standalone prefix/next/bulk bitmap primitives, standalone `byte_context`,
  LD4/TBX/SMIN/SMAX refinements, and standalone D1 lazy capacity
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:153`-`157`,
  `:238`-`:243`).

## Blockers / Fold Requirements

No CH2 blocker remains. V2 resolved the P2-A cross-read failure, and every
P2-A/B/C/D/E candidate family now carries a P2-F grammar-neutral verdict,
explicit JSON-overfit rejection, or inventory / `NOT-S-P3-ELIGIBLE` status.

Carry-forward requirements for S-P3 are constraints, not V2 CH2 blockers:

1. Do not promote any inventory-only or `NOT-S-P3-ELIGIBLE` primitive without
   a later accepted research fold naming fresh P1 evidence, scalar reference,
   parity/checkasm where applicable, and a same-wave row consumer.
2. Preserve P2-F's rejection of generic-crate JSON/CSS/Sheets/BBNF branches,
   hand-written per-grammar runtime files, `JsonSink`-specific acceleration,
   JSON-only quote/backslash policy, object/array/key-colon dispatch rewrites,
   and retained structural sidecars.
3. If S-P3 renames or decomposes a candidate, it must retain the P2-F verdict
   lineage so the Lock-14 status remains auditable.

Non-blocking editorial note: P2-F's final V2 cross-read sentence says it
incorporates "V1 sibling artefacts" while the header, scope, findings, source
list, and C1-C8 mapping all point at the V2 sibling packet
(`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:3`-`18`,
`:223`-`:236`). This wording should be cleaned up when P2-F is next edited,
but it does not undermine the CH2 verdict.

## Disposition

CH2 generality / Lock 14 accepts S-P2 V2. The P2-A cross-read blocker is closed.
From the CH2 lens, the surviving candidate pool may proceed to the consolidated
S-P2 V2 disposition subject to the other challenge lenses.
