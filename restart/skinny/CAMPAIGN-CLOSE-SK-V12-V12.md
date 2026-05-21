# Campaign Close SK-V12 -> V12

Date: 2026-05-21.

Disposition: `PASS-ADMIT` by USER PIN close clause (a). This is not a
FIXPOINT close.

## Admitted Row

| Field | Value |
|---|---|
| Row | `css_l4/declaration_values/direct_to_struct/main` |
| Output plane | `css_l4_declaration_value_fact_stream` |
| Generated Track 1 | `429.34420791225705 Mbps` |
| cssparser oracle | `217.42665242186035 Mbps` |
| lightningcss | `168.92962215656692 Mbps` |
| Threshold | `169.92962215656692 Mbps` |
| Margin | `259.41458575569015 Mbps` |
| Ratio vs lightningcss | `2.5415566697611705x` |
| Strict equality | `pass:track1=cssparser=lightningcss` |
| Fact stream SHA-256 | `caf97bee6e413157e6114985bc1108bc3a8fbf597a1e519b3ccff905d2e5236c` |
| Report | `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json` |

## Tranche Disposition

| Wave | REDRESS | Disposition |
|---|---|---|
| W0 | artifact-only revalidation | Revalidated telemetry/gate authority. |
| W1a | REDRESS-121 | GrammarConfig / Lock 14 legality and JSON guard refresh. |
| W2 | REDRESS-122 | `escape_mask_64` correctness prerequisite closed. |
| W1b-1 | REDRESS-123 | CSS generated Track 1 plus cssparser oracle scaffold admitted. |
| W1b-2a | REDRESS-124 | lightningcss same-plane comparator admitted. |
| W1b-2b | REDRESS-125 | CSS SOTA report gate produced `PASS-ADMIT-CANDIDATE`. |
| W4 | REDRESS-126 | delimiter ASM-gen route recorded; final orphan count zero. |
| W5 | REDRESS-127 | campaign close reconciliation promoted `PASS-ADMIT`. |

W3 is not required for this close. SPEC Section 10 requires W3 disposition only
for FIXPOINT or when no prior CSS row satisfies ADMIT; W1b-2b supplied the
already-admitted CSS path.

## JSON Guards

JSON guard floors held with no measured demotion. W5 reran the checked-in AWK
floor proof after adding the CSS row to `skinny/RESULTS.md`. The CSS close row
is not validated through the legacy JSON-shaped `gate --check-results` path;
CSS admission remains gate-consumed by `sk-v12-css-l4-sota-v1`.

## SIMD And Orphans

W2 resolved the `escape_mask_64` correctness blocker before later SIMD/ASM
work. W4 records the final aarch64 orphan state as zero:
`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
`byte_context`, and `cache_hints` are all `inventory_demoted_with_evidence`.

The W4 selected candidate `a64_ascii_set_run_skip` is accounted separately from
the five-row orphan inventory. Its retained microbench over
`find_ascii_set_member64(bytes, cursor, end, b"{};")` records decision `pass`,
parity `pass`, scalar `18.510497846 ns/iter`, candidate
`3.923145814 ns/iter`, speedup ratio `4.718279341`, and threshold `1.01`.
Production CSS wiring is routed to a separate future production/gate split.

## Union Disposition

USER PIN D3 unblocks the union-substrate category at the category level.
REDRESS 96/97/98 remain historical measured implementations. No fresh
union-substrate attempt is required for SK-V12 because the campaign closes by
ADMIT rather than FIXPOINT.

## ASM-Gen Disposition

USER PIN D4 unblocks ASM-gen at the category level. W4 provides the measured
new route attempt for the closing tranche record through the delimiter
microbench. It is not a production SIMD/ASM admission and does not alter the
CSS ADMIT evidence, which remains W1b-2b's scalar generated parser and
same-plane lightningcss comparator.

## Routed Remainder

- Optional W4 production/gate split: wire `find_ascii_set_member64` into CSS
  scan-block production, add a current report/gate, rerun Lock 14
  authorization, fresh Criterion/equality artifacts, and W2 prerequisite proof
  if needed.
- Future union attempts remain legal under USER PIN D3 only with material
  differential from REDRESS 96/97/98, CHALLENGE, scalar/reference proof,
  microbench, equality/parity, and same-wave consumer.
- JSON direct residuals remain governed by REDRESS-119 and are not SK-V12
  close blockers.
- Sheets and BBNF-self remain fallback history. CSS L4 satisfied the
  authoritative close target before fallbacks were needed.

## Totality Fold Deltas

- Lock 1 carries REDRESS 96/97 history; SK-V12 does not rewrite substrate
  cardinality.
- Lock 14 now has concrete GrammarConfig and non-JSON gate evidence through
  CSS L4, not prose.
- Lock 16's `escape_mask_64` blocker is resolved before later SIMD work.
- BENCH gains the `sk-v12-css-l4-sota-v1` companion report gate.
- SK-V11 close remains guard/routed history.
- W4 inventory demotion is valid zero-orphan evidence.
- No totality architecture rewrite is implied by SK-V12 close.
