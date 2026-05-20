# SK-V12 S-P1 PIN-V3 CH3 - Regression / REDRESS

Verdict: ACCEPT
Score: 96%

## Blocking Findings

None.

## Nonblocking Notes

1. Parse-only remains diagnostic-only and does not create a SOTA lane. The user
   pin keeps `parse_only` diagnostic-only, and the folded handoff repeats that
   no parse-only row can count as SOTA admission
   (`USER-PIN-W1-CSS-L4-SOTA.md:80-94`; `HANDOFF.md:53-64`). The revised P1
   docs preserve the same boundary: P1-A marks parse hot families as diagnostics
   rather than CSS L4 admission evidence, P1-C records JSON `parse_only` as
   never a SOTA target, and P1-F classifies 16 `S / NO-GO` plus one
   `L / NO-GO` parse row as diagnostic-only (`p1a-samply-mode-1.md:142-170`;
   `p1c-samply-mode-3.md:116-120`; `p1f-results-delta.md:80-87`,
   `:198-205`).

2. Union and ASM-gen are unblocked only as future category-level routes with
   material differential and challenge gates. The pin preserves REDRESS
   88/89/90 and 96/97/98 as measured historical entries while rescinding
   category preblocks (`USER-PIN-W1-CSS-L4-SOTA.md:39-69`, `:108-121`). The
   handoff says reopened union plus ASM-gen work needs a new
   material-differential plan that can pass CHALLENGE (`HANDOFF.md:41-43`,
   `:66-68`, `:127-128`). P1-C and P1-E carry the same rule: W0 diagnostics do
   not become wave authority, and new union or ASM-gen candidates need material
   differential, fresh profile, micro-prove-first evidence, scalar/parity or
   checkasm coverage, same-wave consumer, and CHALLENGE acceptance
   (`p1c-samply-mode-3.md:83-87`; `p1e-hot-leaf-attribution.md:186-193`).

3. Historical REDRESS entries remain preserved. REDRESS 88 and 89 still record
   rejected PMULL prefix-XOR and CSSC CTZ/bulk-consumer implementations, while
   REDRESS 90 admits only the stack-canary hardening slice and keeps bitmap asm
   bodies rejected (`skinny/REDRESS.md:2508-2618`). REDRESS 96/97/98 still
   record the class-column and streaming-cursor measured misses plus the W3
   union gate retirement (`skinny/REDRESS.md:2795-2925`). REDRESS 111-113 and
   119-120 remain the non-JSON and direct-fixpoint authorities; `git diff
   --name-status db2c999b..HEAD -- skinny/RESULTS.md skinny/REDRESS.md` is
   empty, matching P1-F's no-diff statement (`p1f-results-delta.md:68-70`,
   `:145-179`).

4. No revised S-P1 artifact scopes an intervention without micro-prove-first.
   P1-A explicitly proposes no intervention and says profile evidence alone
   still needs S-P2 material differential and micro-proof before scoping
   (`p1a-samply-mode-1.md:181-186`). P1-B says no JSON direct residual is
   reopened before CSS L4 unless a later wave names material differential,
   fresh profile, micro-proof, same-wave consumer, and gate-consumed measurement
   (`p1b-samply-mode-2.md:206-213`). P1-E proposes no intervention and records
   only fresh JSON self-time plus the missing CSS L4 lane
   (`p1e-hot-leaf-attribution.md:180-193`).

5. JSON guard deltas are honest. P1-F states `skinny/RESULTS.md` is unchanged
   from SK-V11 close for 17 parse, 17 direct, and seven typed rows, and that no
   current file proves an admitted CSS L4 skinny row (`p1f-results-delta.md:24-35`).
   It keeps JSON rows out of the CSS L4 `lightningcss_mbps + 1` close bar,
   reports every JSON row delta as unchanged, and preserves REDRESS 119/120 as
   the direct residual/fixpoint authority (`p1f-results-delta.md:89-99`,
   `:101-169`, `:196-210`). The handoff requires refreshed JSON guards or a
   measured REDRESS demotion for any JSON-producing path change
   (`HANDOFF.md:58-62`).

6. Generated-size and O(N) routing does not bypass REDRESS. The handoff requires
   generated CSS runtime size before W1b redress, including generated LOC,
   module byte size, regen/check command, and an O(N) grammar-size guard; an
   overflow blocks W1b until traced (`HANDOFF.md:119-123`). The telemetry
   contract makes generated LOC, module byte size, O(N) grammar-size status,
   JSON guard state, gate status, wave id, and REDRESS id gate-consumed fields
   (`HANDOFF.md:142-151`). Producer-only telemetry, stale IDs, parse-only
   admission, missing lightningcss evidence, and orphan SIMD primitives fail
   closed (`HANDOFF.md:153-155`).

7. PIN-V2 replay fold checks relevant to CH3 are clean. The tracked pin replay
   ledger has 458 data rows with zero noncanonical modes and no stale
   `update-center` corpus key; lane counts are 82 PMU, 82 samply, 82 primary
   Time Profiler, 82 CPU Counters, 48 product-v2 Time Profiler, and 82 exports.
   The manifest now names `cf7848b2` as capture source, `d4ef80b2` as the
   PIN-V2 review base, keeps `skinny/RESULTS.md` as result authority, and
   records the stdout-backed `rc=54` acceptance policy with a 185-hit validation
   (`skv12-p1-capture-manifest.md:8-29`, `:70-73`, `:109-145`).

## Exact Fold Edits If REVISE

None; ACCEPT.
