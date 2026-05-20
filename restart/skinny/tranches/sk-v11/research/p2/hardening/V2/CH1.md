# SK-V11 S-P2 V2 CH1 Correctness

Pass: S-P2 CHALLENGE. Cycle: V2. Lens: CH1 CORRECTNESS.
Date: 2026-05-19.
Scope: correctness review of the six S-P2 V2 research artifacts.
Output: this file.

## Lens Contract

CH1 checks whether every candidate primitive traces to a named S-P1 hot leaf,
whether comparator claims cite the correct strictness plane and source, and
whether ISA claims are grounded in local source or architecture references
(`restart/prompts/skinny/PASS-2-RESEARCH.md:95`,
`restart/prompts/skinny/PASS-2-RESEARCH.md:98`,
`restart/prompts/skinny/PASS-2-RESEARCH.md:99`;
`restart/prompts/ORCHESTRATOR.md:83`).

S-P1's accepted candidate vocabulary is the eight hot families:
`bounded_plain_string_scan`, `string_escape_decode`,
`unicode_escape_hex_decode`, `number_digit_span`, `ascii_whitespace_skip`,
`container_dispatch`, `simd_movemask`, and `output_digest_hash`
(`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:41`).
S-P1 also says diagnostic PMU/parse-only/lazy-tape facts do not admit rows and
that W3/substrate, sidecar/cursor, PMULL/CTZ default rewires, rejected string
families, generic numeric fallback, and object/value-byte carry remain
pre-blocked (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:46`,
`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:49`).

## Findings

1. ACCEPT - V2 candidate antecedents now resolve to S-P1 hot leaves.

   P2-A lists only C1-C5 parser candidates plus C8 as a non-parser
   output-plane surface, and each has explicit P1 antecedents:
   C1 byte class maps to whitespace/string/container/movemask,
   C2 to bounded string/escape/movemask, C3 to whitespace/container,
   C4 to container dispatch, C5 to digit span, and C8 to output digest/hash
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:47`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:74`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:103`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:126`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:150`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:174`).
   P2-B similarly gives every SIMD/process row a P1 field or demotes it to
   proof/process-only (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:261`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:263`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:270`).
   P2-C lists five row-moving AArch64 candidates with P1 antecedents and keeps
   movemask/PMULL/CTZ/SHA3/cache rows as support or inventory only
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:25`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:34`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:43`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:52`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:61`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:71`).
   P2-D D1-D5, P2-E's four parse-that gaps, and P2-F C1-C7 also resolve to
   the accepted leaves or are explicitly non-parser/accounting surfaces
   (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:29`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:21`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:43`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:30`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:32`).

2. ACCEPT - comparator strictness is correctly bounded.

   V2 does not treat parse-only or historical C++ sidecars as strict product
   anchors: P2-A points at direct/typed as the unresolved product surface and
   repeats that C++ sidecars are historical or absent in W0
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:22`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:24`;
   `skinny/RESULTS.md:143`, `skinny/RESULTS.md:146`). Comparator-derived
   material is classified as candidate, support-only, or pressure rather than
   admission (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:28`).
   The strictness caveats are source-backed: asmjson is downgraded because of
   its whitespace/control handling caveat (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:30`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:269`);
   sonic direct/typed is the strict product comparator while lossy/unchecked
   paths are excluded (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:32`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:281`);
   simdjson/yyjson claims are kept to architecture pressure or support rows
   with commit-pinned source ranges (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:35`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:37`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:287`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:309`).

3. ACCEPT - ISA claims are grounded and AArch64-only.

   P2-C binds SK-V11 implementation scope to AArch64 Apple Silicon only and
   excludes x86 implementation work (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:11`).
   TBL/TBX, UDOT, PMULL, SHA3 EOR3/BCAX, EXT, ADDV, and CNT claims are tied to
   in-tree source plus Arm ACLE source lines (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:14`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:18`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:19`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:20`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:113`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:121`).
   Non-official CTZ/PRFM/STNP references are explicitly inventory-only and
   cannot support admission claims (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:21`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:122`).
   P2-B grounds the checkasm process in local harness files and VideoLAN/FFmpeg
   process references, then requires strict parity before native bodies count
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:32`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:64`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:92`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:371`).

4. ACCEPT - proof-only and accounting surfaces are not incorrectly promoted.

   The x4 hex path is proof-only until it gets a scalar x4 oracle, strict
   invalid/mixed/alignment coverage, and a real source delta
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:263`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:52`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:48`).
   `output_digest_hash` stays benchmark/oracle or product-host-sink only
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:174`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:29`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:46`).
   C9 is explicitly not a hot-leaf primitive and has no S-P1 antecedent, so it
   is kept as Lock-1/output-plane accounting (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:47`).

## Required Redress

None for CH1. S-P3 must preserve the V2 boundary: support, inventory, oracle,
and accounting rows cannot become wave-scoped implementation candidates unless
the wave packet supplies fresh S-P1-compatible behavior evidence, scalar
reference, strict parity where applicable, and a same-wave product consumer.

## Disposition

Disposition: ACCEPT.

Accept-rate contribution: 1/6.
