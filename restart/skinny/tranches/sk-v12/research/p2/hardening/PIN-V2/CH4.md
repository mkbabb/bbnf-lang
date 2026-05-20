# SK-V12 S-P2 PIN-V2 CH4 Cost / Scalar-Reference / Checkasm Review

Verdict: REVISE

Score: 89%

## Blocking Findings

1. P2-D still lacks explicit checkasm/parity accounting for every row in
   its `§2` table. PASS-2 CH4 requires scalar-reference status,
   checkasm/parity expectation, and same-wave-consumer note for each
   candidate (`restart/prompts/skinny/PASS-2-RESEARCH.md:119`-`:124`),
   and this dispatch also requires explicit N/A where checkasm is not
   implicated. P2-D names five rows under `Candidate primitives` with
   scalar-ref, same-wave consumer, and micro-proof columns, but no
   checkasm/parity or explicit N/A column
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:100`-`:112`).
   The rows are mostly diagnostic or rejected, which is fine, but CH4
   cannot infer "non-SIMD therefore N/A" when the table itself is the
   row-accounting surface.

2. P2-B's §2 candidate/process table is still globally, not row-level,
   complete for micro-proof and orphan disposition. The process correctly
   states micro-prove-first and zero-orphan rules (`p2b-dav1d-process.md:29`-`:37`),
   and the table carries scalar-ref, checkasm, and same-wave consumer
   columns (`p2b-dav1d-process.md:41`-`:57`). But the prompt asks every
   candidate/support/oracle/accounting row to carry micro-proof or
   explicit N/A and orphan consume/remove/demote handling where aarch64
   production support is touched. Rows such as
   `BITMAP_PREFIX_XOR_64_SUPPORT_GATE`, `BITMAP_NEXT_SET_BIT_SUPPORT_GATE`,
   `BULK_EMIT_POSITIONS_64_CONSUMER_GATE`, `BYTE_CONTEXT_SHIFT_SUPPORT_GATE`,
   and `CACHE_HINTS_INVENTORY_GATE` are aarch64 support/inventory rows
   (`p2b-dav1d-process.md:50`-`:56`), but their orphan disposition is
   not represented as a per-row column. USER-PIN D5 makes the five orphan
   primitives a close target (`USER-PIN-W1-CSS-L4-SOTA.md:71`-`:78`), so
   per-row consume/demote/remove status must be explicit in this source
   table, not only in surrounding process prose.

3. P2-F's grammar-neutral verdict table lacks the CH4 row-accounting
   columns for support/oracle/accounting families. P2-F correctly states
   that inventory/drop, support-only, diagnostic-only, and
   parser-candidate-ineligible rows are outside the current S-P3 candidate
   pool unless later evidence adds scalar oracle, micro-proof, and
   same-wave consumer (`p2f-grammar-neutral.md:26`-`:28`). Its table,
   however, has only `Primitive or family`, grammar-neutral expression,
   CSS L4 proof surface, scalar-ref/checkasm need, and verdict
   (`p2f-grammar-neutral.md:30`-`:59`). That leaves same-wave
   consumer/proof, micro-proof or N/A, and orphan disposition implicit for
   rows that clearly touch support/orphan surfaces, including
   `BULK_EMIT_POSITIONS_64_CONSUMER_GATE`, bitmap next/prefix support,
   `byte_context`, and `cache_hints` (`p2f-grammar-neutral.md:39`-`:43`).
   Because P2-F is the cross-artifact legality map, implicit row
   accounting here can let S-P3 over-read a support family as candidate
   evidence.

## Nonblocking Notes

- P2-A now satisfies the stricter CH4 shape. Its table has row-level
  scalar reference, checkasm/parity, micro-proof, same-wave consumer,
  `escape_mask_64`/Lock 16 prerequisite, and orphan disposition columns
  for C1-C7 (`p2a-sota-teardown.md:29`-`:37`). It also marks C6 as
  output-plane/oracle contract and C7 as codegen legality surface rather
  than standalone parser row movers (`p2a-sota-teardown.md:36`-`:37`).
- P2-C resolved the PIN-V1 split. It declares only C1/C3/C4/C5/C6 as
  selectable, marks C2/C9/C11 inventory/drop, and marks C7/C8/C10/C12
  support/inventory (`p2c-arch-esoterica.md:43`-`:59`). Each detailed
  C1-C12 entry carries scalar-ref status, micro-proof need, and same-wave
  consumer or nonselectable status (`p2c-arch-esoterica.md:61`-`:167`).
- P2-E is the strongest row-accounting table: it includes candidate class,
  scalar-ref status, checkasm/parity expectation, same-wave consumer,
  orphan/Lock 16 disposition, and micro-proof need for parser, support,
  output-plane, oracle, and accounting rows (`p2e-parse-that-gaps.md:31`-`:42`).
- The `escape_mask_64` prerequisite is carried correctly. USER-PIN requires
  the xorshift falsifier to be verified and resolved before new SIMD
  admission (`USER-PIN-W1-CSS-L4-SOTA.md:104`-`:106`), CHECKASM records the
  current strict-mode divergence (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:102`-`:126`),
  and P2-A/P2-B/P2-C/P2-E/P2-F all treat string-region SIMD as blocked on
  that fix.
- The local checkasm basis is adequate for S-P3 gating once the row
  accounting is explicit: the harness records reference-vs-candidate calls,
  cloned inputs, alignment sweeps, stack canaries, signal guards, strict
  mode, and deterministic xorshift inputs (`CHECKASM-REPORT.md:41`-`:63`).
  The coverage audit also preserves the current Lock 16 truth: only three
  primitives are compliant, wrappers/x4/digit helpers are blocked or
  proof-only, and the orphan count remains five
  (`skv12-aarch64-simd-coverage-audit.md:117`-`:140`, `:191`-`:199`).

## Exact Fold Edits If REVISE/REJECT

1. In `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`,
   add a `Checkasm/parity status` column to the §2 table.
   - `css_fact_stream_same_tape_kind`: `N/A for SIMD/checkasm; requires
     strict generated CSS fact-stream parity against independent oracle and
     lightningcss adapter`.
   - `offset_tape_capacity_policy`: `N/A for checkasm; requires equal
     offset stream, sparse flags, payload counters, and JSON guard parity`.
   - `sparse_flag_same_tape_policy`: `N/A for checkasm unless SIMD bit-pack
     appears; requires `flags_at` parity and lazy semantics equality`.
   - `retained_view_skip_same_tape_fact`: `N/A for checkasm; requires
     retained-view/fact-stream parity and same-tape proof`.
   - `parallel_structural_cursor_or_class_lane`: `N/A / rejected; no legal
     checkasm path in this shape because the retained sidecar shape fails
     Lock 1`.

2. In `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`,
   expand the §2 table with `Micro-proof / explicit N/A` and
   `Orphan disposition` columns. Populate every support/oracle row, not
   only the selectable rows:
   - bitmap prefix/next/bulk rows: consume inside a named same-wave caller
     or demote/remove; no default-body admission.
   - `BYTE_CONTEXT_SHIFT_SUPPORT_GATE`: consume under same-wave string or
     escape boundary caller or demote/remove.
   - `CACHE_HINTS_INVENTORY_GATE`: inventory/drop unless a fact-stream or
     tape writer consumes it with no semantic delta.
   - `OUTPUT_DIGEST_HASH_ORACLE_GATE`: parser-candidate-ineligible; checkasm
     N/A unless a concrete SIMD hash is proposed; micro-proof is output-plane
     equality.

3. In `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`,
   add a compact CH4 accounting supplement after the §2 table or extend the
   table with columns for `Micro-proof / same-wave proof` and
   `Orphan disposition`. The supplement must cover every row family whose
   verdict is support-only, inventory/drop, diagnostic-only, oracle-only, or
   parser-candidate-ineligible, and must spell out whether checkasm is N/A,
   inherited from a helper, or required before admission.

4. Redispatch PIN-V2 CH4 after the fold. Acceptance criteria: no row in
   P2-A through P2-F relies on global prose for any of the following fields:
   scalar-reference status, checkasm/parity or explicit N/A, micro-proof or
   explicit N/A, same-wave consumer/proof or explicit ineligible status,
   `escape_mask_64` prerequisite where implicated, and aarch64 orphan
   consume/remove/demote handling where production support is touched.

## Commands Used

- `git status --short && git rev-parse --short HEAD && git show --no-patch --format='%h %s' 31859478`
- `ps -axo pid,comm,args | rg '(cargo|rustc|xctrace|samply)' || true`
- `rg --files restart/skinny/tranches/sk-v12 restart/prompts skinny | rg '(PASS-2-RESEARCH|USER-PIN-W1-CSS-L4-SOTA|CHECKASM-REPORT|aarch64-simd-coverage-audit|p2[a-f]-|Lock|REDRESS|RESULTS|PIN-V2)'`
- `sed -n '1,220p' restart/prompts/skinny/PASS-2-RESEARCH.md`
- `sed -n '220,420p' restart/prompts/skinny/PASS-2-RESEARCH.md`
- `sed -n '1,240p' restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `sed -n '1,260p' skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- `sed -n '1,260p' restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`
- `sed -n '1,240p' restart/locks/LOCKS.md`
- `find restart/skinny/tranches/sk-v12/research/p2 -maxdepth 2 -type f | sort`
- `sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `sed -n '1,280p' restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `sed -n '1,340p' restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `sed -n '1,320p' restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `sed -n '1,360p' restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `sed -n '1,360p' restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `find restart/skinny/tranches/sk-v12/research/p2/hardening -maxdepth 3 -type f | sort`
- `sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md`
- `sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V1/CH4.md`
- `sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/hardening/V2/CH4.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md | sed -n '24,76p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md | sed -n '22,96p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md | sed -n '58,228p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md | sed -n '92,150p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md | sed -n '26,44p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md | sed -n '20,118p'`
- `nl -ba restart/prompts/skinny/PASS-2-RESEARCH.md | sed -n '112,128p'`
- `nl -ba restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md | sed -n '70,108p'`
- `nl -ba skinny/crates/bbnf-simd/CHECKASM-REPORT.md | sed -n '40,68p;102,126p;238,254p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md | sed -n '34,62p;117,146p;184,199p'`
- `nl -ba restart/locks/LOCKS.md | sed -n '84,114p'`
- `rg -n 'Scalar reference|Scalar-ref status|Checkasm/parity expectation|Checkasm expectation|Micro-proof|Micro-proof need|Same-wave consumer|Orphan disposition|Orphan / Lock 16 disposition|Inventory/drop|Support-only|parser-row-mover ineligible|parser-candidate-ineligible|escape_mask_64|same-wave' restart/skinny/tranches/sk-v12/research/p2/p2{a,b,c,d,e,f}-*.md restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md skinny/crates/bbnf-simd/CHECKASM-REPORT.md restart/prompts/skinny/PASS-2-RESEARCH.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/locks/LOCKS.md`
