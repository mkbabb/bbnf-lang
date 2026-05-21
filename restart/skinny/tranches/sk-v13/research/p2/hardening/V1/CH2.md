# SK-V13 S-P2 V1 CH2: Generality / Lock 14

## Verdict

REVISE.

## Evidence

- The S-P2 CH2 contract requires every candidate to carry a P2-F
  grammar-neutral verdict; a JSON-only primitive with no byte-set,
  classifier, tape, or per-grammar-template expression must be revised or
  rejected (`restart/prompts/skinny/PASS-2-RESEARCH.md:102`-`107`).
  Lock 14 is the controlling constraint: generic crates may not carry
  grammar-named modules, grammar-specific public types, grammar feature
  flags, or `match grammar` arms (`restart/locks/LOCKS.md:78`).

- S-P1 converged with the right boundary for P2: JSON envelopes, JSON
  typed leaves, CSS timer/fact-sink samples, and JSON-confirmed
  scanner/unicode candidates remain profile evidence, not grammar-neutral
  proof (`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md:12`-`24`,
  `:53`-`:61`).

- P2-F defines an adequate verdict vocabulary and Lock-14 surface:
  `ADMISSIBLE-GRAMMAR-NEUTRAL`, `CONDITIONAL-GRAMMAR-NEUTRAL`,
  `JSON-OVERFIT`, and `INVENTORY-ONLY`; it also states that generic crates
  carry no grammar arms and per-grammar modules must come from one
  grammar-agnostic template (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:28`-`42`).

- The B/C/D/E candidate families are mostly covered with grammar-neutral
  verdicts. P2-F maps SIMD/string/number/structural/union/decision-engine
  candidates into verdict rows (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:77`-`101`),
  P2-B states the primitive must be a policy-driven byte-set/window/carry
  unit rather than a JSON or CSS scan (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:107`-`120`),
  P2-C labels every aarch64 candidate as generalisable, conditional, or
  inventory-only (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:40`-`50`),
  P2-D gives D1-D5 Lock-14 verdicts (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:361`-`377`),
  and P2-E gives P2E-1..8 JSON/CSS/Sheets/BBNF-self verdicts
  (`restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md:118`-`129`).

- JSON-only routes are generally re-expressed or rejected. P2-F rejects
  JSON object/array/key-specific dispatch rewrites, `JsonSink`-specific
  acceleration, hardcoded JSON quote/backslash widening, and claims based
  only on JSON profile envelopes (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:132`-`145`).
  P2-A's JSON direct digest candidate is already bounded as a
  JSON-output-plane sink, not a parser primitive (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:81`,
  `:100`), and P2-F preserves that shape only as a grammar-neutral
  fact-stream digest, not `JsonDigestSink` internals
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:92`).

## Blockers / Fold Requirements

- P2-F does not explicitly cross-read P2-A. Its V1 cross-read says it
  folds P2-B/C/D/E (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:12`-`17`),
  and its source list names P2-B/C/D/E but not P2-A (`:207`-`:210`),
  while P2-A defines eight candidate primitives C1-C8 and its own
  grammar-neutrality table (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:70`-`81`,
  `:89`-`:100`). Because CH2 asks whether every candidate carries a
  P2-F verdict, semantic overlap is not enough.

- Fold requirement: update P2-F to explicitly include P2-A in the
  cross-read and sources, and add a literal C1-C8 mapping to the existing
  P2-F verdict rows or to `JSON-OVERFIT` / `INVENTORY-ONLY` rejection.
  The expected mapping can remain narrow: C1 byte-set classifier, C2
  string policy/block scan, C3 escape decoder, C4 digit-run, C5 generated
  dispatch, C6 grammar structural scan / union, C7 ASCII set run-skip,
  and C8 fact-stream digest with the JSON-output-plane caveat.
