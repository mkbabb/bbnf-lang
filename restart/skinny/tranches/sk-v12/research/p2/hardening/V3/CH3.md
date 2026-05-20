# SK-V12 S-P2 CHALLENGE V3 - CH3 Regression

Pass: S-P2 Research.
Cycle: V3.
Lens: CH3 REGRESSION.
Date: 2026-05-20.
Disposition: ACCEPT.

## Scope

This convergence check re-ran the regression lens over the V1-folded S-P2
research cohort after the V2 all-ACCEPT cycle. The question is unchanged from
`restart/prompts/skinny/PASS-2-RESEARCH.md:109`: whether any current candidate
reopens a `skinny/REDRESS.md` route without fresh S-P1 evidence and materially
new framing.

Read set: the six folded S-P2 artifacts, V1/V2 hardening files, REDRESS 28+33,
50-55, 60-72, 80, 82-84, 88, 89, 96-120, `skinny/RESULTS.md`,
`restart/locks/LOCKS.md`, and
`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`.

## Findings

1. The V2 ACCEPT boundary still holds. V1 required candidate triad accounting,
   demotion of speculative ISA entries, P2-D diagnostic reframing, and explicit
   oracle/accounting-only splits; V2 confirmed those folds as closed
   (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:22`-`:45`,
   `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:10`-`:27`).
   The current packet still says candidates are research input, not selected
   waves: P2-A has candidate shapes only, P2-D has zero selectable S-P1 tape
   candidates, P2-F marks F7 oracle-only and F8 accounting-only, and V2 CH3
   recorded no reopen route.

2. Retained sidecar and W3 union routes remain closed. Lock 1 says retained
   structural offsets are the tape and SIMD masks are transient
   (`restart/locks/LOCKS.md:52`). P2-A treats simdjson's retained structural
   index as comparator evidence, P2-B rejects retained class columns/side
   tables/second scans, P2-D rejects `structural_class_lane_union`, and P2-F
   blocks sidecars, second scanners, retained class columns, `UnionTape`, and
   alternate structural indexes. This does not reopen REDRESS 50/51/53 or the
   REDRESS 96/97/98/102 W3 substrate falsification.

3. String, tiny-string, Unicode, and escaped-segment routes remain guarded
   rather than reopened. P2-A C2/C3, P2-B string/hex gates, P2-C C4/C5, P2-E
   `pt_bounded_plain_string_end`, `pt_hex_quad_decode`, and
   `pt_escaped_string_segments`, and P2-F F2/F3 all require scalar parity,
   grammar-owned policy, and same-wave generated consumers. They explicitly
   avoid StringBlock16/tiny-string retreads, decoded-byte sidecars, eager
   materialization, and host-sink dependency. That preserves REDRESS 28/33,
   54/55, 60-69, 72, 82/83, 106/107/108, 116/117, and 119.

4. Numeric, container, PMULL, CTZ, and digest/hash routes remain pre-blocked.
   P2-A C4/C5/C6, P2-B digit/bitmap/digest gates, P2-C C3/C7 plus PMULL
   inventory, P2-E digit/dispatch exclusions, and P2-F F4/F6/F7 keep numeric
   materialization, JSON container-tail dispatch, PMULL prefix-XOR, CTZ bulk
   consumer, and digest-host-sink claims behind their prior REDRESS gates. This
   preserves REDRESS 80, 88, 89, 114, 115, and 118.

5. Generated non-JSON baseline priority is still the routing guard. S-P1
   convergence records the current surface as `N-direct / NoGo` and requires a
   measured generated non-JSON direct or typed parser baseline before behavior
   implementation waves
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:41`-`:63`).
   `skinny/RESULTS.md` still reports JSON direct residual rows and no admitted
   non-JSON generated baseline (`skinny/RESULTS.md:143`-`:146`). REDRESS
   119/120 keep the 13 JSON direct residual rows as measured fixpoint unless a
   future packet supplies material evidence beyond REDRESS 114-119. The folded
   S-P2 packet repeats that priority across P2-A through P2-F.

## Verdict

ACCEPT. The V3 convergence check finds no S-P2 candidate that reopens a
REDRESS-blocked route as implementation authority. S-P3 may consume this
research only under the carried guards: generated non-JSON baseline first; no
retained sidecar, parallel substrate, or parser-owned structural cursor; scalar
reference and strict parity before SIMD/native use; same-wave generated or
runtime consumer before behavior admission; output digest remains oracle or
row-owned product evidence; JSON direct residual rows remain fixpoint/pre-
blocked unless a later packet supplies fresh P1 evidence and a materially new
framing beyond REDRESS 114-120.

## Revise List

None for CH3 REGRESSION V3.
