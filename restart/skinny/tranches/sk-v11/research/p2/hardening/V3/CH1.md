# SK-V11 S-P2 V3 CH1 Correctness

Pass: S-P2 CHALLENGE. Cycle: V3. Lens: CH1 CORRECTNESS.
Date: 2026-05-20.
Scope: correctness review of the six S-P2 V3 research artifacts.
Output: this file.

## Lens Contract

CH1 checks whether every candidate primitive traces to a named S-P1 hot leaf,
whether comparator claims cite the correct strictness plane and source, whether
ISA claims are grounded, and whether the V3 stability fold stayed inside the V2
accepted facts (`restart/prompts/skinny/PASS-2-RESEARCH.md:95`-`100`;
`restart/prompts/ORCHESTRATOR.md:83`; `restart/prompts/ORCHESTRATOR.md:104`-`123`).

The accepted S-P1 hot-leaf vocabulary remains
`bounded_plain_string_scan`, `string_escape_decode`,
`unicode_escape_hex_decode`, `number_digit_span`, `ascii_whitespace_skip`,
`container_dispatch`, `simd_movemask`, and `output_digest_hash`
(`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:41`-`45`).
S-P1 also keeps PMU/parse-only/lazy-tape facts diagnostic, and keeps W3,
sidecar/cursor, PMULL/CTZ default rewires, rejected string/materialization
families, generic numeric fallback, and object/value-byte carry routes
pre-blocked (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:46`-`55`).

## Findings

1. ACCEPT - V3 stayed within the V2 accepted facts.

   V2 accepted CH1-CH6, recorded 100% ACCEPT, and required V3 only to preserve
   the candidate pool and carry forward five facts: C1-C7 parser pool, C8
   oracle/host-sink only, C9 accounting only, x4 proof-only, digest non-parser,
   W3 closed, and `json_provider` as an S-P3 Lock 14 gate
   (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:17`-`24`,
   `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:41`-`56`).
   The six V3 artifacts preserve that fold: P2-A keeps C1-C5 plus C8 and blocks
   W3/parse-only movement (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:20`-`30`);
   P2-B keeps x4 and digest proof/oracle-only
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:9`-`12`);
   P2-C carries AArch64-only scope and inventory demotions unchanged
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:9`);
   P2-D keeps the existing offset tape plus direct/typed consumer union only
   (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:9`-`10`);
   P2-E keeps four parse-that gaps and support/oracle demotions
   (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:13`-`15`);
   and P2-F preserves C1-C7/C8/C9 plus the `json_provider` S-P3 gate
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:10`-`17`).

2. ACCEPT - candidate hot-leaf traceability holds.

   P2-A's parser candidates each name S-P1 antecedents, while C8 is explicitly
   non-parser output-plane work (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:59`-`84`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:86`-`113`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:115`-`136`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:138`-`160`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:162`-`184`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:186`-`212`).
   P2-B's SIMD/process rows either name a P1 leaf or remain proof/process-only
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:266`-`275`).
   P2-C's five AArch64 candidate rows name antecedents and keep movemask, EXT,
   PMULL/CTZ, SHA3, and cache hints as support/inventory rows
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:27`-`81`).
   P2-D D1-D5 map to container, string, number, digest, and sparse-flag pressure
   without claiming a second substrate (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:30`-`48`).
   P2-E retains only four parse-that gaps and demotes `container_dispatch`,
   `simd_movemask`, and `output_digest_hash` out of the parse-that primitive
   pool (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:21`-`31`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:45`-`50`).
   P2-F normalizes the pool as C1-C7, with C8 and C9 outside parser primitives
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:39`-`56`).

3. ACCEPT - comparator strictness and source usage are correctly bounded.

   P2-A grounds the unresolved product surface in direct/typed comparison and
   treats parse-only and historical or absent C++ sidecars as non-admission
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:34`-`38`;
   `skinny/RESULTS.md:3`-`18`; `skinny/RESULTS.md:143`-`146`).
   Comparator-derived material is classified as candidate, support-only, or
   comparator pressure rather than row admission
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:40`-`50`).
   The strictness caveats are source-backed: asmjson is downgraded because its
   README allows broader control-byte whitespace and does not scan string
   contents for unescaped controls (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:42`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:281`-`282`;
   external primary source:
   `https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/README.md#L209-L222`).
   Sonic-rs is correctly treated as the strict product comparator for direct
   Rust-struct parsing with no temporary tape, while lossy/unchecked paths remain
   excluded (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:44`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:283`-`298`;
   external primary source:
   `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/README.md#L53-L90`).

4. ACCEPT - ISA claims are grounded and AArch64-only.

   The tranche handoff binds SK-V11 SIMD/ASM to Apple Silicon AArch64 and requires
   scalar reference, differential/checkasm where applicable, same-host microbench,
   feature gate, and same-wave consumer (`restart/skinny/tranches/sk-v11/HANDOFF.md:67`-`80`,
   `restart/skinny/tranches/sk-v11/HANDOFF.md:105`-`114`).
   P2-C respects that scope and ties TBL/TBX, UDOT, ADDV/CNT, PMULL, EOR3/BCAX,
   and EXT to in-tree source plus Arm ACLE references
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:13`-`23`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:116`-`124`).
   External spot checks against Arm ACLE resolve the load-bearing mappings:
   `vqtbl4q_u8` maps to A64 `TBL`, `vdotq_u32` to `UDOT`, `veor3q_u8` to
   `EOR3`, and `vaddvq_u8` to `ADDV`
   (`https://arm-software.github.io/acle/neon_intrinsics/advsimd.html`).
   Non-official CTZ/PRFM/STNP links are explicitly inventory-only, not admission
   support (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:21`-`23`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:125`-`127`).

5. ACCEPT - proof-only and accounting surfaces are not promoted.

   `HEX_QUARTET_X4_PROOF` remains proof-only until a scalar x4 oracle, strict
   valid/invalid/mixed/alignment/surrogate coverage, real source delta, and
   same-wave product consumer exist (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:268`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:54`-`62`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:50`).
   `output_digest_hash` stays benchmark/oracle or per-product host sink only
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:275`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:29`-`31`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:51`-`56`).
   C9 has no S-P1 hot-leaf antecedent and is only Lock-1/output-plane accounting
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:39`-`56`).

6. Advisory - P2-F has narrow sibling-summary ranges, but no correctness defect.

   P2-F line 21 cites narrow ranges for the sibling candidate summaries
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:21`).
   The exact sibling rows resolve at broader ranges in the owned artifacts:
   P2-B candidate table at `p2b-dav1d-process.md:266`-`275`, P2-C candidates and
   support inventory at `p2c-arch-esoterica.md:27`-`81`, and P2-D D1-D5 at
   `p2d-substrate-tape.md:30`-`36`. Because P2-F's own normalized pool is correct
   at `p2f-grammar-neutral.md:39`-`68`, this is not a CH1 redress item.

## Required Redress

None for CH1. S-P3 must preserve the V3 boundary: support, inventory, proof,
oracle, and accounting rows cannot become wave-scoped implementation candidates
unless the wave packet supplies fresh S-P1-compatible behavior evidence, scalar
reference, strict parity where applicable, feature/fallback, and a same-wave
direct, typed, or generated non-JSON product consumer.

## Disposition

Disposition: ACCEPT.

Accept-rate contribution: 1/6.
