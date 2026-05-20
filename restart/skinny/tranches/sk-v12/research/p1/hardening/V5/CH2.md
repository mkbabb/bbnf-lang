Verdict: ACCEPT

# SK-V12 S-P1 Hardening V5 CH2: Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 generality / Lock 14 confirmation after V4 all-ACCEPT.
Scope: audit current repo commit `fe7ae2ab` and the SK-V12 S-P1 packet for
grammar generality claims.

## Evidence

1. Commit `fe7ae2ab` is the V4 all-ACCEPT archive, not a packet or behavior
   scope change. Its diff adds only `V4/CH1.md` through `V4/CH6.md` and
   `V4/CONSOLIDATED.md`; it does not touch source, `skinny/RESULTS.md`,
   `skinny/REDRESS.md`, `SYNTHESIS.md`, `HANDOFF.md`, the P1 artifacts, the
   capture manifest, or the replay TSV. V4 itself records six-of-six ACCEPT,
   zero open REVISE findings, no row or gate movement, and routes this V5
   confirmation cycle
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V4/CONSOLIDATED.md:19`-`:28`).

2. JSON-only profile facts remain fenced from grammar-neutral proof. P1-B says
   `direct_to_struct` rows are JSON digest-plane rows and `real_typed_struct`
   rows are guarded JSON typed rows, not non-JSON baseline rows
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:143`-`:151`).
   The same file states the capture set has no non-JSON product row and does
   not substitute JSON product profiling for the required generated non-JSON
   baseline; JSON residuals stay diagnostic/pre-blocked, and the implementation
   target remains a generated non-JSON baseline followed by that row's measured
   grammar-generalized intervention
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:256`-`:268`).

3. The grammar-neutral primitive vocabulary is attribution vocabulary, not a
   grammar-totality claim. P1-E defines the names as canonical labels for
   grouping fresh xctrace leaf symbols, with row percentages and top leaves in
   the retained self-time TSVs
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:198`-`:215`).
   It then states the top-leaf percentages are self-time attribution and row
   admission still belongs to Criterion/`skinny/RESULTS.md`
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:351`-`:365`).
   P1-D likewise says PMU values are profile evidence only and do not move or
   admit rows
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:84`-`:92`).

4. Non-JSON and SK-totality limits remain explicit. `SYNTHESIS.md` requires
   exactly one generated non-JSON direct or typed baseline before any JSON-only
   micro-wave, then a measured same-baseline grammar-generalized intervention;
   SK-V12 closes by that baseline plus intervention, or by a measured generated
   baseline `BLOCKED` route, not by another JSON-only cycle
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`-`:56`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:80`-`:84`). `HANDOFF.md`
   repeats the priority order and keeps parse-only diagnostic with W3,
   parse-only, and JSON residual pre-blocks carried forward
   (`restart/skinny/tranches/sk-v12/HANDOFF.md:51`-`:67`).
   P1-F confirms the live surface is unchanged, has zero generated non-JSON
   baseline rows, and treats SK-V12-open as freshness rebinding rather than new
   JSON row movement
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:77`-`:87`,
   `:167`-`:186`, `:232`-`:244`).

5. Replay/source/symbol fold evidence does not widen scope. The manifest keeps
   the replay TSV as the authoritative command surface, with 506 parse/direct/
   typed replay rows across PMU, samply, xctrace, and export lanes; samply is
   retained artifact-only evidence, while self-time percentages come from
   exported xctrace Time Profiler XML
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:38`-`:62`).
   The V3 fold says it re-parsed existing xctrace XML, recorded no fresh
   benchmark/profile runs, preserved replay/Mode III boundaries, and left
   `RESULTS`, `REDRESS`, and behavior source unchanged
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V3/FOLD-REVISIONS.md:9`-`:12`,
   `:56`-`:63`). Direct validation for this audit found 82 summary rows and
   410 detail rows with zero `:0` or unresolved markers in the relevant symbol
   and source fields, matching the manifest invariant
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:165`-`:169`).

6. Mode III/source limits are still absence boundaries, not hidden producers.
   P1-C records no fresh `/tmp/skv12-p1/samply/probes`, no `json_probes_*`
   capture, and no structural-scan capture under `/tmp/skv12-p1`; S-P2/S-P3
   may not use Mode III symbols as fresh SK-V12 hot-leaf authority without a
   later capture
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:54`-`:63`).
   It also states S-P1 proposes no behavior route and can be used only after
   respecting the priority order: generated non-JSON baseline first, same-row
   grammar-generalized intervention second, and JSON direct residuals only
   after the REDRESS 119/120 reopen burden
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:224`-`:246`).

## Required Fold

None.

## Verdict

ACCEPT. No JSON-only profile fact is promoted as grammar-neutral proof;
non-JSON/SK totality limits remain explicit; and the replay/source/symbol fold
improves profile attribution without widening SK-V12 scope.
