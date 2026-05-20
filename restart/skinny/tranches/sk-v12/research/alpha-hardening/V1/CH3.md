# SK-V12 Pass Alpha CH3 - Regression / REDRESS

Pass: Pass Alpha SK-V11 -> SK-V12.
Cycle: CHALLENGE V1 under USER PIN.
Lens: CH3 regression / REDRESS.
Date: 2026-05-20.
Scope: Alpha-C / Alpha-D / Alpha-E / Alpha-F under
`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`, with
`skinny/REDRESS.md` through REDRESS 120.
Output: this file only.

## Disposition

REVISE.

The re-bracket is directionally REDRESS-honest: Alpha-C and Alpha-D correctly
convert REDRESS 96/97/98 and 88/89/90 from category-level blocks into
historical specific rejections, and they keep REDRESS 111 report-only
infrastructure plus REDRESS 112/113 future-phase close blocks intact. Alpha-E
also names real material differentials for the CSS-local union and ASM-gen
candidates.

V1 still needs revision before CH3 can accept. The defects are concrete and
bounded: the ADMIT close path does not carry the zero-orphan SIMD target as
strongly as the FIXPOINT path, Alpha-E has comparator-floor drift, and Alpha-E's
JSON guard shortcut is too weak for a wave that also touches generic
codegen/runtime roots.

## Authority Read

- `restart/prompts/ORCHESTRATOR.md` Section 3W: CH3 checks that proposals do
  not reopen REDRESS routes, that the pre-block list is correct, and that
  admitted rows are not silently regressed.
- `restart/prompts/pass-contracts/PASS-ALPHA.md` Section 3: Pass Alpha CH3
  cross-checks Alpha A-F against REDRESS entries 1-N and the next-tranche
  pre-block list.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-D-validated-invalidated.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `skinny/REDRESS.md` entries 88/89/90, 96/97/98, and 111-120.

## Accepted REDRESS Accounting

### Union substrate category

ACCEPT with respect to historical specific rejections.

The user pin unblocks the architectural category, but keeps REDRESS 96/97/98
as specific failed implementations: class-column plus move-consumed structural
vector, streaming cursor plus retained class lane, and class-lane-only
CHALLENGE rejection (`USER-PIN-W1-CSS-L4-SOTA.md:39-56`;
`skinny/REDRESS.md:2795-2950`). Alpha-C carries exactly that distinction:
the category is open, but any new union attempt must cite REDRESS 96/97/98,
name the material differential, pass CHALLENGE, wire a same-wave consumer, and
measure against CSS L4 or a JSON guard hot leaf
(`alpha-C-redress-digest.md:51-81`, `:234-255`). Alpha-D repeats the same
close rule and requires at least one new measured union attempt for FIXPOINT
(`alpha-D-validated-invalidated.md:208-222`).

Alpha-E's E4 material differential is admissible for CH3: it is CSS-local,
output-plane-owned, consumed inside the CSS declaration-value direct parser,
and explicitly excludes a second structural vector, public substrate API,
parser-owned sidecar, retained structural vector, and parse-only admission
(`alpha-E-candidate-shortlist.md:202-245`). This is not a replay of REDRESS
96/97/98 on its face.

### ASM-gen category

ACCEPT with respect to historical specific rejections.

The user pin unblocks ASM-gen at category level while preserving REDRESS 88,
89, and 90 as historical evidence (`USER-PIN-W1-CSS-L4-SOTA.md:58-69`).
Alpha-C keeps PMULL default prefix-XOR, CSSC CTZ bulk position-emission, and
canary hardening as row movement out of the admitted surface
(`alpha-C-redress-digest.md:83-107`, `:236-255`). Alpha-D likewise requires
micro-proof, scalar reference, checkasm/differential parity, same-wave
consumer, and measured row or guard impact for any new ASM-gen attempt
(`alpha-D-validated-invalidated.md:224-238`).

Alpha-E's E5 is materially different from REDRESS 88/89/90 because it targets a
CSS row-local byte-class or digit-run consumer, not PMULL as default
prefix-XOR, not CSSC CTZ bulk emit, and not canary hardening as row movement
(`alpha-E-candidate-shortlist.md:253-306`). CH3 does not find a replay of a
rejected ASM body.

### Report-only and future-phase blocks

ACCEPT.

REDRESS 111 admitted only a companion non-JSON report lane; it did not create
generated Track 1, an independent oracle, a benchmark row, `RESULTS.md`
admission, or parser row movement (`skinny/REDRESS.md:3282-3309`). Alpha-C
keeps report-only close blocked (`alpha-C-redress-digest.md:111-128`,
`:230-233`). REDRESS 112 rejected the generated CSS baseline because runtime
and codegen were still JSON-profiled and no generated CSS L4 runtime existed
(`skinny/REDRESS.md:3311-3338`). REDRESS 113 blocked an intervention wave from
creating the first measurable non-JSON row as a future-phase side effect
(`skinny/REDRESS.md:3340-3355`). Alpha-C supersedes REDRESS 112/113 only for
the explicit CSS mandate and keeps future-phase close blocked
(`alpha-C-redress-digest.md:120-128`, `:231-233`). Alpha-E's E1 is the first
CSS generated baseline, while E4/E5 are later intervention candidates gated on
the CSS row being measurable (`alpha-E-candidate-shortlist.md:62-65`,
`:101-120`, `:236-245`, `:294-306`).

### JSON direct residual fixpoint

ACCEPT with one guard-strength REVISE below.

REDRESS 119 closes the 13 direct residual rows as a measured fixpoint with no
row movement, and REDRESS 120 carries SK-V11 as `N-direct / NoGo`
(`skinny/REDRESS.md:3495-3553`). Alpha-C keeps those rows guard-only unless a
later route supplies fresh profile, micro-proof, and material differential
beyond REDRESS 114-119 (`alpha-C-redress-digest.md:257-258`). Alpha-D records
banked direct and typed guard floors and says demotion requires measured gate
disposition (`alpha-D-validated-invalidated.md:53-88`). Alpha-F and
`SYNTHESIS.md` both keep JSON guard floors as close conditions
(`alpha-F-contract-draft.md:89-90`; `SYNTHESIS.md:55-57`).

## Concrete Defects

### CH3-R1 - Zero-orphan SIMD is weaker on ADMIT than FIXPOINT

Severity: REVISE.

The user pin makes zero orphan kernels a campaign close target
(`USER-PIN-W1-CSS-L4-SOTA.md:71-78`). Alpha-C and Alpha-D carry that as a hard
pre-block or close blocker (`alpha-C-redress-digest.md:246-247`;
`alpha-D-validated-invalidated.md:248-250`). The FIXPOINT close path in
Alpha-F and `SYNTHESIS.md` also makes orphan primitives invalidate FIXPOINT
(`alpha-F-contract-draft.md:108-110`; `SYNTHESIS.md:76-78`).

The ADMIT close path is weaker. Alpha-F ADMIT only requires Lock 16 for
"every admitted primitive" and JSON guard floors (`alpha-F-contract-draft.md:85-90`);
`SYNTHESIS.md` does the same for "any SIMD/ASM admission"
(`SYNTHESIS.md:51-57`). E5's gate says "zero new orphan aarch64 primitive at
close" rather than zero admitted-or-inventory-visible orphan primitives
(`alpha-E-candidate-shortlist.md:294-301`). That leaves a path where CSS L4
admits and the campaign closes while the five known orphan primitives remain
undisposed.

Required revision: add the D5 zero-orphan disposition to ADMIT as well as
FIXPOINT. The close text should require each known orphan primitive to be
admitted with a consumer, removed, or explicitly demoted/rejected with measured
evidence before any SK-V12 close route, not only FIXPOINT and not only "new"
orphans.

### CH3-R2 - Alpha-E comparator gates drift from the pin-aware close bar

Severity: REVISE.

The user pin says CSS L4 must beat lightningcss on the same corpus and output
plane (`USER-PIN-W1-CSS-L4-SOTA.md:29-37`), and Alpha-F / `SYNTHESIS.md`
strengthen that to generated Track 1 being strictly greater than
`lightningcss_mbps + 1` (`alpha-F-contract-draft.md:70-73`;
`SYNTHESIS.md:39-41`). Alpha-E's E1, E4, and E5 gates instead use
`generated Track 1 Mbps >= lightningcss_mbps + 1`
(`alpha-E-candidate-shortlist.md:101-103`, `:236-241`, `:294-299`).

This is a REDRESS-facing defect because REDRESS 112/113 are superseded only for
the explicit user-pinned CSS mandate, not for a weaker comparator. If Alpha-E
means integer floor arithmetic, it must state the rounding convention that makes
`>= floor(lightningcss + 1)` equivalent to the close bar. Otherwise the gates
must say `>` and match Alpha-F / `SYNTHESIS.md`.

Required revision: normalize Alpha-E gates to the pin-aware close comparator,
or add a single rounding rule consumed by the gate/report schema.

### CH3-R3 - Alpha-E's JSON guard shortcut is too loose for W1/E2 owner paths

Severity: REVISE.

Alpha-E E1 allows the JSON guard rows to hold either by refreshed run or by
proving `skinny/RESULTS.md` unchanged because "no JSON-producing path moved"
(`alpha-E-candidate-shortlist.md:108-111`). But the same Alpha-E packet couples
E1 with E2 in W1 (`alpha-E-candidate-shortlist.md:62-65`), and E2's owner paths
include generic runtime/codegen/template surfaces that can affect JSON output:
`runtime/src/tape/*`, `runtime/src/lib.rs`, `codegen/src/lib.rs`, and
`codegen/src/json_templates/generated.rs` (`alpha-E-candidate-shortlist.md:122-151`).

This can silently regress JSON guard rows if the no-refresh path is interpreted
loosely. The user pin keeps JSON direct and typed guard floors binding, and
demotion requires measured gate disposition (`USER-PIN-W1-CSS-L4-SOTA.md:80-95`).

Required revision: the no-refresh shortcut is legal only when the wave proves
no JSON runtime, codegen, generated-output, bench, report, or gate source moved
and `skinny/RESULTS.md` remains unchanged under the existing checked report.
If W1 touches E2's generic roots, the wave needs a refreshed JSON guard run or
a measured REDRESS demotion.

## Surface Disposition Matrix

| Surface | CH3 disposition | Finding |
|---|---|---|
| Alpha-C REDRESS digest | ACCEPT | Correctly preserves historical specific rejections, material-differential gates, report-only/future-phase blocks, JSON guard status, and orphan-SIMD close blocker. |
| Alpha-D validated/invalidated ledger | ACCEPT | Correctly records banked JSON guards, REDRESS 119/120 fixpoint authority, union/ASM category reopen rules, and zero-orphan close target. |
| Alpha-E candidate shortlist | REVISE | Material differentials are present, but comparator wording, JSON guard shortcut, and "zero new orphan" wording must be tightened. |
| Alpha-F contract draft | REVISE | Category unblocks and report/future blocks are sound, but ADMIT must inherit the zero-orphan close requirement. |
| `SYNTHESIS.md` | REVISE | Same ADMIT zero-orphan gap as Alpha-F. |
| `HANDOFF.md` | ACCEPT | Handoff repeats the user-pin refusal conditions, including category-level unblocks and no orphan SIMD at close. |

## Required Fold For V2

1. Add zero-orphan aarch64 disposition to ADMIT close, not only FIXPOINT.
2. Replace Alpha-E's `>= lightningcss_mbps + 1` gates with the pin's strict
   comparator or a gate-consumed integer rounding convention.
3. Tighten the JSON guard shortcut so generic codegen/runtime/template edits
   require refreshed guard measurement or measured REDRESS demotion.

After those edits, CH3 can accept the category-level union and ASM-gen unblocks
because the specific REDRESS rejections remain preserved and the report-only /
future-phase blocks remain closed.
