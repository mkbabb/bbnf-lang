# SK-V11 P3-F: SPEC And Dispatch Draft

Pass: S-P3 Synthesis-Plan. Cycle: V4.
Date: 2026-05-20.
Scope: compose the SK-V11 SPEC and dispatch prompt from the converged S-P1/S-P2 packet.
Output: this file + `restart/skinny/tranches/sk-v11/SPEC.md` + `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`.
Pass Alpha goalset: direct-plane GO or measured direct fixpoint, plus one admitted benchmarked non-JSON generated direct/typed parser intervention, with typed/direct guard rows held.
Candidate pool: `research/p2/` V3 post-CHALLENGE survivors only.

## §1 — Synthesis

The SK-V11 implementation packet is bounded by the SK-V11-open profile
authority in `HARDENING-S-P1-CONVERGED.md`: 16 `parse_only S / NO-GO` rows
plus one `parse_only L / NO-GO` row are diagnostic; `direct_to_struct` is the
primary closure surface at 4 `A / GO` plus 13 `N-direct / NO-GO`; and
`real_typed_struct` is a 7-row guard/product credibility surface.

S-P2 V3 converged two consecutive 6/6 ACCEPT cycles and leaves exactly this
admissible pool for S-P3:

- C1-C7 parser primitives: byte-set/class-table masking, bounded special-byte
  scan, escape/hex segment decode, digit span/accumulate, byte-set layout skip,
  generated FIRST/prefix/lookahead dispatch, and movemask/bitmap support only
  when consumed by C1/C2/C6.
- C8 output digest/hash is an oracle or per-product host sink only. It is not
  parser vocabulary and cannot enter generic parser crates as semantics.
- C9 is Lock-1/output-plane accounting only. It is not a row-moving primitive.

The V4 SPEC therefore sequences waves around product-plane consumers, not
parse-only substrate repair. W3 union/event/class-column/streaming-cursor
routes are REDRESS 96/97/98-falsified and REDRESS 102-firewalled. Any route
that re-derives W3 under another name fails the SPEC entry gate.

The SK-V11-specific change from earlier skinny packets is that Lock 14 must be
measured, not asserted. The live `json_provider` codegen path is an S-P3 gate:
before any generic/codegen/runtime-outside-JSON edit can claim generality, a
wave must stand up a generated non-JSON direct or typed parser benchmark and
consume its telemetry in the same wave gate.

## §2 — Deliverable

The draft `SPEC.md` mirrors the SK-V8 shape:

- §0 close condition, comparator classes, outcome enum, telemetry, and
  opening row goalset.
- §1 non-negotiables.
- §2 wave manifest, budgets, phase caps, micro-prove-first gate, and Lock 14
  generality gate.
- W0, W1a, W1b, and W2 through W9, each with owner paths, tasks, entry gate, exit
  gate, revert protocol, downstream effect, and pre-blocked routes.
- A pre-blocked route ledger and G-Alpha/dispatch scope.

After the sibling P3 artifacts landed, this P3-F draft binds to the P3-B
topological sequence and folds P3-C/P3-D/P3-E gate facts into the SPEC:

| Wave | Purpose | Candidate surface |
|---|---|---|
| W0 | SK-V11-open telemetry lock | closed by S-P1/W0 authority |
| W1a | non-JSON gate/report schema lane | C9 accounting + Lock 14 gate |
| W1b | generated non-JSON baseline and oracle lane | C9 accounting + generated baseline harness |
| W2 | CSS L4/non-JSON generated intervention proof | C1/C2/C4/C5/C6 with C7 support |
| W3 | numeric direct closure slice | C4 + P2-D D4 |
| W4 | generated dispatch and byte-set control slice | C1/C5/C6 with C7 support + P2-D D1/D2 |
| W5 | bounded string span/special-byte scan | C2 + P2-D D3 |
| W6 | escaped string segment/hex decode | C3 |
| W7 | output digest/hash host sink | C8 only |
| W8 | direct residual fixpoint and row reclamation | remaining C1-C8 measured residuals |
| W9 | close and Alpha feedback | docs/gate reconciliation |

## §3 — Falsifiability Binding

The direct residual floor for every target row is `ceil(sonic-rs direct / 1.10)`
from the SK-V11-open same-run strict direct comparator:

| Row | Floor Mbps |
|---|---:|
| `twitter/direct_to_struct` | 13740 |
| `canada/direct_to_struct` | 10637 |
| `github_events/direct_to_struct` | 13403 |
| `update_center/direct_to_struct` | 10059 |
| `mesh/direct_to_struct` | 8675 |
| `random/direct_to_struct` | 7878 |
| `gsoc-2018/direct_to_struct` | 3737 |
| `instruments/direct_to_struct` | 8969 |
| `numbers/direct_to_struct` | 2425 |
| `unicode_mixed/direct_to_struct` | 2588 |
| `unicode_escapes/direct_to_struct` | 3441 |
| `distinct_values/direct_to_struct` | 2658 |
| `y_string_unicode/direct_to_struct` | 3950 |

Row admission requires generated Track 1 and independent Track 2/oracle on the
same output plane to clear the floor under one coherent run id, with
`gate-json` consuming the provenance. W0-clamped rows (`instruments`,
`numbers`, `unicode_mixed`) are not admitted from W0 throughput alone.

Non-JSON closure requires an admitted generated direct or typed benchmark for
CSS L4 declaration values, Sheets, or BBNF-self. The preferred proof is CSS L4;
Sheets is the fallback for byte-set layout; BBNF-self is the fallback for
literal/dispatch work. The gate must include generated Track 1, independent
oracle or Track 2, strict output equality, before/after throughput, primitive
self-time when applicable, and no JSON policy in generic crates.

## §4 — Pre-Blocked Routes

Hard pre-blocks carried into the draft:

- REDRESS 96/97/98 and REDRESS 102: no W3 union/event/class-column,
  structural-position vector, streaming cursor, `UnionTape`, class lane,
  sidecar producer, parse-only SOTA movement, or W4 cascade-lock through W3.
- REDRESS 50/51/53: no aux columns, event cursors, whitespace cursors,
  structural cursor sidecars, or parser-owned projections.
- REDRESS 54/55/60-72/82/83/106/107/108: no decoded-string scratch, retained
  wide string scans, generated-retained `StringBlock16`, primitive-only string
  production, x4 proof-to-production promotion, or reuse of an already-wired
  `unescape_string` caller as same-wave production.
- REDRESS 80: no numeric fallback/mantissa widening or f64 policy rewrite.
- REDRESS 88/89: PMULL prefix-XOR and CSSC CTZ/bulk emission are not default
  production routes.
- REDRESS 63/65/84: no object next-key carry or value-byte compaction outside
  a same-loop, generated, measured direct/typed consumer.
- Generic Lock 14: no JSON policy in `bbnf-simd`, `parse-that-regex`,
  `codegen`, or runtime outside generated per-grammar modules.
- x86 implementation is out of scope for SK-V11.

## §5 — Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/research/close/close-redress.md`
- `restart/skinny/tranches/sk-v9/research/p3/hardening/HARDENING-S-P3-CONVERGED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
