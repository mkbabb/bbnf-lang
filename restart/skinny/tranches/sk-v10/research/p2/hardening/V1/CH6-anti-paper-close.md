# SK-V10 S-P2 V1 CH6: Anti-Paper-Close

Disposition: REVISE.
Date: 2026-05-19.
Scope: S-P2 candidate closure honesty: measurable row gates, strict comparators/oracles, same-wave consumers, and no future-phase promises.
Output: this file.

## Authority Read

- S-P2 contract: `restart/prompts/skinny/PASS-2-RESEARCH.md`.
- SK-V10 contract: `restart/skinny/tranches/sk-v10/HANDOFF.md`; `restart/skinny/tranches/sk-v10/SYNTHESIS.md`.
- S-P1 hardening: `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`.
- S-P2 cohort: `p2a-sota-teardown.md`, `p2b-dav1d-process.md`, `p2c-arch-esoterica.md`, `p2d-substrate-tape.md`, `p2e-parse-that-gaps.md`, `p2f-grammar-neutral.md`.
- Alpha row-gate authority used by the P2 packet: `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md`.

## Verdict

REVISE. The packet is not a paper close for the five Alpha/Synthesis SK-V10
candidate boundaries, because those boundaries have numeric row floors,
strict-oracle language, same-wave consumers, and REDRESS dispositions. However,
the S-P2 cohort itself expands that shortlist into many primitive aliases and
architecture inventory entries. Several of those aliases are still framed as
"later", "future host", "S-P3 would need", or "must be paired" without an
orchestrator-citable SK-V10 row gate and same-wave consumer in the P2 artifact
that nominates them.

This is fixable without source edits. V2 must fold a canonical candidate ledger
or per-artifact row-gate capsules before S-P3 may shortlist from this packet.

## Passing Closure Checks

1. The governing contract is clear that S-P2 selects and sequences nothing; S-P3
   consumes only candidates that survive CHALLENGE (`PASS-2-RESEARCH.md:10-11`,
   `:192-195`). This protects against treating research prose as implementation
   authority.
2. The direct frontier has measurable floors in Alpha-E: each direct corpus row
   names current Track 1/Track 2 Mbps, sonic direct Mbps, and
   `ceil(sonic / 1.10)` floors; the row still cannot move without output-plane
   equivalence, independent Track 2/oracle status, comparator semantics, and
   fresh same-run Criterion rows (`alpha-E-candidate-shortlist.md:64-86`).
3. `instruments` typed admission has full-fixture generated/serde/sonic/Track 2
   checksum parity, same-run typed comparator rows, the 1.10x sonic typed floor,
   existing typed-row maintain requirements, and explicit `gate-json`/RESULTS
   same-wave consumers (`alpha-E-candidate-shortlist.md:112-145`).
4. Root typed generalization is honestly proof-only unless same-wave typed
   comparator rows are present; `github_events` and `gsoc-2018` have no row
   movement in a root-only wave (`alpha-E-candidate-shortlist.md:169-199`).
5. The existing-substrate unicode/string kernel pair has target direct floors,
   W10b maintain floors, scalar/checkasm requirements, current production caller
   consumers, and explicit rejection on missing scalar/checkasm/caller evidence
   (`alpha-E-candidate-shortlist.md:227-275`).
6. Comparator/telemetry refresh is gate-only: any new field must be consumed by
   `gate-json` in the same wave and cannot move behavior rows by itself
   (`alpha-E-candidate-shortlist.md:290-315`).

## Blocking Defects

### D1 - Candidate aliases are not normalized to one row-gate ledger

P2-A/B/C/E/F name overlapping versions of the same families:
`Strict tiny-string terminator`, `tiny_plain_string_first_special_64`,
`STRING_FIRST_SPECIAL_16_INLINE`, `bounded_plain_string_end`, and `Tiny string
scan`; similarly for full-string/escape, unicode decode, digit/number scan,
whitespace/classify, direct output, and telemetry. The strongest gates exist in
Alpha-E and partly in P2-B/E, but not every alias carries the same measurable
row gate, strict oracle, same-wave consumer, and REDRESS status where it is
nominated.

Required fix: add a canonical S-P2 candidate ledger before S-P3. Each row must
map all aliases to exactly one status:

- `admissible only with row gate`;
- `proof-only, no RESULTS movement`;
- `gate-only, no behavior movement`;
- `inventory-only, not S-P3 eligible`;
- `rejected`.

Each admissible row must name corpus/plane, current Mbps, comparator Mbps,
floor, oracle/comparator, same-run run-id requirement, same-wave consumer, and
REDRESS pre-blocks. Do not let S-P3 select an alias that lacks the ledger entry.

### D2 - Architecture inventory still contains future-host candidate promises

P2-C's x86 table lists AVX2, AVX-512 VBMI2/BITALG/compress/kmask,
VPCLMUL/PCLMUL, VNNI, and IFMA candidates with scaffold/no production checkasm
and "future x86" same-wave consumers (`p2c-arch-esoterica.md:38-47`). On the
current SK-V10 host, P1 records aarch64 Apple M5 Max, so these rows are not
measurable SK-V10 row candidates.

Required fix: demote every non-current-host x86 row to `inventory-only, not
S-P3 eligible for SK-V10`, unless V2 supplies a same-host x86 dispatch context,
target corpus rows, floors, scalar oracles, checkasm paths, and same-wave
consumers. The current P2-C wording must not let "future x86 host wave" count as
survived S-P2 candidate closure.

### D3 - ISA claims need instruction-level manual anchors

The S-P2 CH6 contract requires an ISA claim to cite the manual section
(`PASS-2-RESEARCH.md:133-138`). P2-C currently cites broad Arm and Intel source
pages for PMULL/PMULL2, CSSC CTZ, UDOT/DotProd, TBL/TBX, wide shifts, GFNI,
VBMI2, VPCLMUL, VNNI, and IFMA (`p2c-arch-esoterica.md:75-95`). That is not
section-level evidence.

Required fix: for every architecture candidate kept as more than inventory,
add instruction-specific manual anchors or section identifiers. If a candidate
cannot cite its ISA semantics precisely, demote it to rejected/inventory-only.

### D4 - Contract-only substrate entries need no-row-movement status or gates

P2-D C1 names tape capacity/flag economy as a contract surface and lists
materialization metrics to compare, but it has no numeric acceptance threshold
and no same-wave consumer (`p2d-substrate-tape.md:28-34`). C3 and C4 describe
future primitives that "must have" scalar references and same-wave generated
consumers, but do not bind a concrete row gate in P2-D (`p2d-substrate-tape.md:42-54`).

Required fix: mark C1 as proof-only/no row movement unless V2 adds explicit
thresholds for logical bytes, allocated bytes, flag bytes, payload bytes, and
Track 1/Track 2 Mbps maintain floors plus a same-wave consumer. For C3/C4,
either cite the Alpha-E direct/kernel matrices directly or demote them to
contract-only non-candidates until a row-bound primitive is named.

### D5 - Missing scalar/checkasm candidates must be explicitly non-eligible

P2-B and P2-E correctly say several shapes need fresh scalar oracles or
checkasm before wiring, but some phrasing still reads as "S-P3 will detail it":
`tiny_plain_string_first_special_64`, `string_full_scan_escape_control_64`,
`unicode_escape_hex4_decode`, `number_digit_run_classify_64`,
`whitespace_skip_mask_64`, `plain_string_special_span`,
`escape_run_decode_x4`, `digit_run_span_64`, `number_span_parts`, and
`ascii_class_skip` (`p2b-dav1d-process.md:164-168`,
`p2e-parse-that-gaps.md:28-36`).

Required fix: every such candidate needs one of two V2 outcomes:

- add the exact scalar-oracle path, checkasm expectation, row floor, and
  same-wave direct/typed consumer; or
- mark it `not S-P3 eligible` / `research-only` / `maintain-only` until those
  fields exist.

The existing rejected/non-candidate treatment is correct for
`allocation_elision_string_materializer`, `array_object_walk_dispatch_hint`,
`structural_cursor_from_movemask`, default PMULL/CTZ production rewires, W3
sidecars, and Canada typed shortcuts. V2 should keep those rejected unless it
adds a fresh Alpha/S-P3 contract and row evidence.

## Candidate Audit Matrix

| Candidate family | Closure status | Required V2 action |
|---|---|---|
| Direct output/control-path contract / Direct SAX-style sink / C2 / P2-F direct contract | Conditionally pass. Alpha-E supplies row floors, strict Track 2/oracle and comparator semantics, `gate-json`/RESULTS consumers. | Add explicit alias ledger entry that all direct aliases inherit `alpha-E:64-86`; no digest row moves without same-run output/control contract. |
| `instruments` typed admission | Pass. Full typed oracle/comparator gate and consumers are measurable. | Preserve as one typed product candidate; no admission without full-fixture parity and same-run sonic/serde/Track 2 rows. |
| Root typed generalization | Pass as proof-only. It has no row movement unless same-wave typed rows are generated. | Ledger must mark root-only wave as no RESULTS movement. |
| Tiny string scan aliases | Revise. P1 rows and consumers are named, but not every alias carries the Alpha-E row floors and same-wave caller. | Normalize aliases and attach target direct/typed rows, floors, scalar oracle, checkasm, and current direct/typed string caller. |
| Full string / escape / unicode aliases | Revise. Alpha-E has unicode target floors and W10b maintain floors; P2 aliases do not all cite them. | Attach unicode target matrix, maintain matrix, scalar unescape/string oracle, checkasm, and current caller only. |
| Number / digit scan aliases | Revise. Hot rows exist, but P2-B admits no proven same-wave consumer for `number_digit_run_classify_64`; Canada typed remains blocked. | Add row-bound direct/typed numeric consumer and floors, or mark research-only. |
| Whitespace / byte-class / movemask aliases | Revise. These are maintain/transient primitives unless paired with a row-bound caller; W3 consumption is blocked. | Mark maintain-only/inventory-only unless a direct/typed caller and maintain floors are supplied. |
| Tape capacity / container walk / lazy string contracts | Revise. C1 lacks thresholds/consumer; C3/C4 need direct Alpha-E row-gate binding. | Add no-row-movement labels or explicit row gates and consumers. |
| Comparator / telemetry refresh | Pass as gate-only. | Preserve no behavior movement; every emitted field must be consumed by `gate-json` same-wave. |
| X86 secondary instruction table | Revise. Current-host gates are absent and several bodies are scaffold/future. | Demote to inventory-only for SK-V10 or supply a same-host x86 dispatch context and full gates. |

## Final Disposition

REVISE. The S-P2 packet is close, but CH6 cannot certify "every candidate" while
architecture inventory and primitive aliases remain selectable without their own
measurable row gates and same-wave consumers. No source edits are required for
the fold; V2 should be a documentation/ledger correction in the S-P2 research
surface.
