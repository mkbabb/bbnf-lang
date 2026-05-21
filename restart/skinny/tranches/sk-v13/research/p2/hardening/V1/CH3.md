# SK-V13 S-P2 V1 CH3 Regression / REDRESS

Verdict: ACCEPT.

## Evidence

- S-P2 is authorized from a converged S-P1 packet, but S-P1 still fences every
  row as profile evidence only. The convergence record says later waves need
  material differentials for REDRESS 119/120, 96/97/98, pre-pin routes, and
  REDRESS-126 (`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md:19`-`24`),
  and the canonical ledger says all rows are
  `profile_signal_not_gate_admission`
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:8`-`23`).

- The string and unicode candidates do not silently reopen REDRESS 28/33,
  50-55, 60-72, or 82-84. P2-A requires grammar-owned string/escape policy and
  same-wave row movement for C2/C3, rejecting naive tiny-string and
  single-quartet replays (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:75`-`76`,
  `:129`-`:132`). P2-E makes the same guard explicit for NEON tiny-string,
  parser side tables/cursors, decoded-string sink hooks, direct materialization,
  REDRESS 80 numeric one-row patches, single-quartet unicode, and object-pair
  compaction (`restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md:133`-`141`).
  The cited histories are real measured rejects, including parser aux/cursor
  routes (`skinny/REDRESS.md:715`-`767`, `:784`-`:813`), decoded-string sink
  routes (`skinny/REDRESS.md:815`-`:880`), direct string materialization
  (`skinny/REDRESS.md:1688`-`:1732`, `:1819`-`:1886`), and the unicode/string
  focused rejects (`skinny/REDRESS.md:2285`-`:2348`).

- The union/substrate candidates preserve REDRESS 96/97/98 instead of
  re-running W3. P2-D states that structural SIMD is only a scanner
  micro-signal and does not reopen retained SIMD-position vectors, streaming
  cursors, class columns, or parser-owned structural cursors
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:62`-`85`).
  Its D2 and D3 variants name material differentials: codegen-time monomorphic
  same-tape routing with no class side vector, or mask-to-offset writeback where
  the projection is the tape (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:186`-`223`,
  `:225`-`:272`). This is aligned with REDRESS 96/97/98, which record two
  correctness-green union implementations missing every required row and
  retire the old union gate (`skinny/REDRESS.md:2795`-`:2949`).

- PMULL/CSSC/bitmap candidates are category-unblocked but not stale
  implementation replays. P2-C says PMULL and CSSC CTZ are admissible only as a
  new SIMD-first union route, not default hot-body substitutions
  (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:16`),
  and its candidate table requires same-wave row movement plus REDRESS
  88/89/96/97/98 citation (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:32`-`38`,
  `:54`-`:60`). P2-B similarly bounds bitmap next-bit, bulk emit, and
  prefix-XOR by consumer proof (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:79`-`91`).
  That matches REDRESS 88/89/90: PMULL and CTZ/bulk implementations were
  correct but measured as regressions, while only canary hardening admitted
  (`skinny/REDRESS.md:2510`-`:2618`).

- REDRESS 119/120 direct-row fixpoints are handled as history under the user
  pin, not ignored. The addendum lifts the fixpoint only by making every row
  wave-eligible with prior-fixpoint citation and a fresh material differential
  (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:58`-`74`). P2-A,
  P2-D, and P2-F repeat that direct reopens need prior-row citation,
  material differential, and same-harness strict comparator evidence
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:117`-`121`;
  `restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:396`-`400`;
  `restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:162`-`166`).
  REDRESS 119/120 themselves record measured fixpoint/no-source-close status,
  not row admission (`skinny/REDRESS.md:3497`-`:3553`).

- REDRESS-126 and zero-orphan discipline are preserved. P2-B flags the
  `a64_ascii_set_run_skip` route as a production split, not retroactive row
  movement, and requires a named CSS scanner consumer before admission
  (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:51`-`63`).
  P2-C keeps `byte_context` as wire-or-delete hygiene and says `cache_hints`
  is non-selectable without a later hot leaf (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:38`,
  `:60`). P2-F requires scalar reference, checkasm parity, material
  differential, and same-wave consumer for PMULL/CSSC/orphan inventory
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:155`-`160`).
  This matches REDRESS-126, which explicitly records W4 as
  `ROUTE-PRODUCTION-SPLIT` and not production admission, with five aarch64
  orphans demoted separately (`skinny/REDRESS.md:3766`-`:3814`,
  `:3864`-`:3872`).

## Blockers / Fold Requirements

No V1 CH3 blocker.

Carry forward to S-P3: B2 / C-P2C-1 / P2E-6 (`a64_ascii_set_run_skip` /
`ByteSetRunSkip64`) has the weakest fresh-S-P1 antecedent because the CSS
profile is timer/fact-sink dominated. It remains acceptable only under the
already-written constraint: a narrow CSS production consumer, strict
lightningcss equality, and same-wave row movement are required before it can be
shortlisted as a behavior wave. A second microbench-only landing would be a
REDRESS-126 replay and must be rejected.
