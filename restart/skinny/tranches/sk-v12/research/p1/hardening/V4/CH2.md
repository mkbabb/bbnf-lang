Verdict: ACCEPT

# SK-V12 S-P1 Hardening V4 CH2: Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 generality / Lock 14.
Scope: audit commit `6d19429f` and the SK-V12 S-P1 packet for grammar
generality claims after the V3 symbol-label fold.

## Evidence

1. Commit `6d19429f` is a CH1 provenance/symbol fold, not a scope fold. It adds
   `V3/FOLD-REVISIONS.md` and updates `skv12-p1-capture-manifest.md`; no source,
   `skinny/RESULTS.md`, or `skinny/REDRESS.md` path is touched. The fold states
   it re-parsed existing xctrace Time Profiler XML, recorded no fresh benchmark
   or profile runs, preserved the replay TSV as the exact command surface,
   preserved Mode III as an absence boundary, and left behavior source,
   `RESULTS`, and `REDRESS` unchanged
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V3/FOLD-REVISIONS.md:9`,
   `:11`, `:56`-`:63`;
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:165`-`:169`,
   `:193`-`:201`).

2. The symbol/source repair remains bounded to attribution quality. The summary
   table is validated as 82 rows with zero `:0` in `top_leaf` or
   `top_leaf_source`, zero any-field `:0`, and zero unresolved markers; the
   details table is validated as 410 rows with zero `:0` in `symbol` or
   `source`, zero any-field `:0`, and zero unresolved markers. This resolves
   line-zero labels without creating new rows, grammars, profiles, or admission
   authority (`restart/skinny/tranches/sk-v12/research/p1/hardening/V3/FOLD-REVISIONS.md:22`-`:54`;
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:157`-`:169`).

3. JSON-only profile facts are still not promoted as grammar-neutral proof.
   P1-A keeps parse-only diagnostic and says no parse-only row can count toward
   SK-V12 SOTA admission or close
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:112`-`:114`).
   P1-B says direct rows are JSON digest-plane rows, typed rows are guarded JSON
   typed rows, and neither replaces the generated non-JSON baseline
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:148`-`:151`,
   `:205`-`:212`, `:258`-`:268`). P1-D states PMU values are profile evidence
   only and do not move `skinny/RESULTS.md` or admit rows
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:84`-`:92`).

4. The grammar-neutral labels are attribution labels, not grammar-totality
   proof. P1-E defines them as canonical family labels used to group fresh
   xctrace leaf symbols, with row-level percentages and top leaves remaining in
   the retained self-time TSVs
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:198`-`:215`).
   The same file explicitly says top-leaf percentages are self-time
   attribution, while row admission remains with Criterion/`skinny/RESULTS.md`
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:351`-`:365`).

5. Non-JSON and SK-totality limits remain explicit. `SYNTHESIS.md` and
   `HANDOFF.md` require one generated non-JSON direct or typed baseline before
   any JSON-only micro-wave, followed by a measured same-row
   grammar-generalized intervention
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`-`:56`, `:80`-`:84`;
   `restart/skinny/tranches/sk-v12/HANDOFF.md:51`-`:67`). P1-E and P1-F carry
   the same boundary: the current blocker is the generated non-JSON baseline,
   the report lane is not a generated baseline, and JSON residual work remains
   deferred until that priority succeeds or honestly blocks
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:299`-`:322`,
   `:374`-`:380`;
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:172`-`:186`,
   `:206`-`:207`, `:239`-`:244`).

6. Replay/source/symbol evidence does not widen scope. The replay ledger remains
   parse/direct/typed over the JSON corpus set, with no CSS L4, Sheets,
   BBNF-self, or generated non-JSON rows. The manifest says samply rows are
   retained artifact-only evidence and xctrace Time Profiler XML supplies
   self-time percentages (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:38`-`:62`).
   P1-C keeps Mode III and structural-scan material diagnostic/nonproducer only,
   and bars S-P2/S-P3 from using absent Mode III call-stack symbols as fresh
   SK-V12 hot-leaf authority without a later capture
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:54`-`:63`,
   `:224`-`:246`).

## Required Fold

None.

## Verdict

ACCEPT. No JSON-only profile fact is promoted as grammar-neutral proof;
non-JSON/SK totality limits remain explicit; the replay/source/symbol fold
improves attribution without widening scope; and the S-P2/S-P3 handoff remains
bounded by profile evidence and the generated non-JSON baseline-first contract.
