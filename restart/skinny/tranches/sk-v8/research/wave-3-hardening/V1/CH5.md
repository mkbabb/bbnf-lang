# SK-V8 W3 Hardening V1 CH5 Review

Date: 2026-05-18.
Reviewer: CH5.
Target reviewed: `fc91c217`
(`docs(sk-v8-wave3-plan): reject Tier A implementation on fit gate`).
Lens: blocked cursor, sidecar, direct, and string route reopening discipline.

## Verdict

Verdict: ACCEPT.

Confidence: 96%.

## Findings

1. HEAD refuses W3 source implementation rather than opening a cursor or sidecar
   route. The reviewed commit adds only `skv8-W3-plan.md`, marks the W3 plan as
   "REVISE/reject before implementation", and ends with "No W3 source
   implementation is authorized by this plan" (`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:4`,
   `restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:114`). That is the
   correct CH5 posture for a plan that cannot name a bounded same-wave generated
   parser consumer.

2. The refusal is grounded in the current event-model mismatch, not in a vague
   cost concern. The plan states that scanner positions are structural punctuation
   plus real quotes, while retained tape offsets are generated parser events:
   container opens/closes, opening quotes, number starts, and literal starts
   (`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:33`). Current code
   matches that: `StructuralIndex` stores only positions and backend
   (`skinny/crates/bbnf-simd/src/lib.rs:72`), JSON scan emits punctuation plus
   quotes (`skinny/crates/runtime/src/grammars/json/scan.rs:22`), retained parsing
   writes offsets through `TapeBuilder::push_plain_offset`
   (`skinny/crates/runtime/src/grammars/json/parser.rs:35`), and generated JSON
   emits number/literal starts while delimiters such as commas and colons are
   generally consumed without tape offsets (`skinny/crates/runtime/src/grammars/json/generated.rs:205`,
   `skinny/crates/runtime/src/grammars/json/generated.rs:221`,
   `skinny/crates/runtime/src/grammars/json/generated.rs:246`). The benchmark
   parity test pins the example offset stream `[0, 1, 5, 6, 8, 12, 13]` for
   `{"a":[1,true]}` (`skinny/crates/bbnf-bench/src/parity.rs:95`), matching the
   plan's mismatch example.

3. REDRESS 50-55 stay blocked. The W3 plan explicitly refuses parse-time
   aux/projection side tables, decoded stats sinks, quote-source fused
   materializers, and the byte-class/event-cursor families
   (`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:80`). That preserves
   the historical constraints that aux side tables regressed parse (REDRESS 50),
   byte-class `JsonEventCursor` is non-canonical (REDRESS 51), parser-local
   structural-mask cursors are second scanners (REDRESS 53), and sink-local
   decoded/hash helpers remain direct-string misses (REDRESS 54-55)
   (`skinny/REDRESS.md:715`, `skinny/REDRESS.md:742`,
   `skinny/REDRESS.md:784`, `skinny/REDRESS.md:815`,
   `skinny/REDRESS.md:846`).

4. REDRESS 60-72 stay blocked or properly separated. The W3 plan does not claim
   Tier B string-boundary, quote/backslash/parity, density-policy, or
   CostFacts-template work inside Tier A (`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:91`).
   It also rejects direct/materialization families (`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:86`).
   This keeps the SK-V6 retained string retries, Unicode run validator, object
   key carry, direct source-hook/scratch/byte-output/semantic-fact materializers,
   hand typed sink, typed Vec reuse, and global cap-16 policy from being
   relabeled as W3 structural projection (`skinny/REDRESS.md:1346`,
   `skinny/REDRESS.md:1382`, `skinny/REDRESS.md:1441`,
   `skinny/REDRESS.md:1584`, `skinny/REDRESS.md:1639`,
   `skinny/REDRESS.md:1688`, `skinny/REDRESS.md:1736`,
   `skinny/REDRESS.md:1789`, `skinny/REDRESS.md:1839`,
   `skinny/REDRESS.md:1890`, `skinny/REDRESS.md:1944`,
   `skinny/REDRESS.md:1996`).

5. REDRESS 82-84 and 88-89 stay blocked. The plan explicitly lists the Unicode,
   tiny-probe, object-pair, PMULL, and CTZ/bulk route families as not reopened
   (`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:87`,
   `restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:88`). That matches the
   historical rejections for the single-quartet Unicode classifier, generated
   retained `StringBlock16`, object-pair value-byte control compaction, PMULL
   prefix-XOR, and CSSC CTZ/bulk consumer (`skinny/REDRESS.md:2287`,
   `skinny/REDRESS.md:2320`, `skinny/REDRESS.md:2360`,
   `skinny/REDRESS.md:2508`, `skinny/REDRESS.md:2542`).

6. P3-E is honored. P3-E globally blocks new directives, BIR variants,
   `BackendShape`, `UnionTape`, public substrate APIs, parallel substrates,
   sidecar producers, parser-owned projections/cursors, aux density tables,
   sidecar event vectors, lossy/sidecar strict admission, and telemetry-only W3
   consumers (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:40`).
   The W3 plan repeats those blocks and rejects `tape_vs_tape`,
   `simd_structural_scan`, Track 2, comparator rows, and retained-view-only checks
   as production consumers (`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:71`,
   `restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:89`). It therefore
   does not paper over the missing same-wave generated retained parser consumer.

7. The only required downstream action is ordinary route accounting after
   challenge, not a CH5 blocking fold. The plan already says W3 should add a
   REDRESS entry, update HANDOFF, and feed SK-V9/Pass Omega with a split
   event-grammar precursor after challenge (`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:94`).
   That is consistent with SPEC Section 6, which allows W4 only after W3 is
   admitted, rejected, routed, or blocked (`restart/skinny/tranches/sk-v8/SPEC.md:590`).

## Verification

- `git rev-parse HEAD`: confirmed `fc91c2173e8451dd06733381346bd800b0711f6e`.
- `git status --short` before writing this file: clean.
- `git show --stat --oneline --decorate --no-renames HEAD`: confirmed HEAD adds
  only `restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md`.
- Reviewed `restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md` and
  `restart/skinny/tranches/sk-v8/research/skv8-W3-tape-structural-research.md`.
- Reviewed P3-E pre-blocks in
  `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`.
- Reviewed historical REDRESS entries 50-55, 60-72, 82-84, and 88-89 in
  `skinny/REDRESS.md`.
- Read current scanner, generated parser, tape builder, and parity-test code to
  confirm the scanner/tape event mismatch and no-op `attach_structural_index`
  surface.
- I did not run cargo tests because this assignment restricts edits to one owned
  review file and cargo would write build artifacts. The review is evidence-only
  against existing code and docs.

## Required Folds

None.
