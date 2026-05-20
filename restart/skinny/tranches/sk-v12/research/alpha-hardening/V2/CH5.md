# SK-V12 Pass Alpha CH5 Hidden Coupling V2

Pass: Alpha SK-V11 -> SK-V12 hardening V2 under `USER-PIN-W1-CSS-L4-SOTA.md`.
Date: 2026-05-20.
Lens: CH5 hidden coupling.
Output: this file only.

## Disposition

REVISE.

V2 folds most USER-PIN hidden-coupling requirements: CSS is first, the close
plane is lightningcss-bound, Track 1 / Track 2 / oracle provenance is
gate-consumed, E4 is fenced away from public substrate expansion, and the
aarch64 orphan set is a close blocker. Two hidden-coupling defects remain before
G-Alpha:

1. E2 can still shortcut JSON guard refresh while touching generic
   runtime/codegen surfaces.
2. Alpha-E still reads pre-pin SPEC/S-P artifacts as unqualified authority,
   which can re-import the stale selected-baseline ordering and threshold model.

## Blocking Findings

### CH5-1 - E2 leaves a JSON guard shortcut on generic codegen/runtime edits

Result: REVISE.

The global contract correctly says any wave that changes generic runtime,
codegen, generated-output, benchmark, report, or gate paths that can produce
JSON must refresh JSON guards or record measured REDRESS demotion
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:144-148`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:60-62`). V1 required the same fold
(`restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CONSOLIDATED.md:59-61`).

Alpha-E E2 owns exactly those generic/codegen/runtime paths:
`skinny/crates/runtime/src/tape/grammar_config.rs`,
`skinny/crates/runtime/src/tape/mod.rs`,
`skinny/crates/runtime/src/tape/assembler.rs`,
`skinny/crates/runtime/src/lib.rs`,
`skinny/crates/codegen/src/lib.rs`,
`skinny/crates/codegen/src/json_templates/generated.rs`, generated config/output,
bench report paths, and lock14 report paths
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:143-158`).
But the E2 gate only requires JSON generated-output parity and a CSS
generated-config compile smoke
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:159-178`).

That is a hidden guard coupling: a generic `GrammarConfig`/template/runtime
change could keep generated JSON parity green while still regressing the JSON
direct/typed performance guard floors. E2 must inherit the full JSON guard rule
locally: refreshed direct/typed JSON guard run, or measured REDRESS demotion,
unless no JSON-producing path moved and `skinny/RESULTS.md` is proven unchanged.

### CH5-2 - Alpha-E still has stale pre-pin SPEC/artifact authority coupling

Result: REVISE.

The folded top-level contract correctly demotes `SPEC.md` to pre-pin context
only where it does not conflict with the USER PIN
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:18-19`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:22-23`) and requires G-Alpha followed
by fresh S-P1 -> S-P2 -> S-P3 under the pin
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:237-254`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:103-125`). Alpha-F also states the
existing implementation packet predates the pin and remains stale where it
treats CSS/Sheets/BBNF-self as preflight-equivalent or blocks union/ASM-gen
categories (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:11-16`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:235-237`).

Alpha-E's authority section still lists `SPEC.md` and the S-P1/S-P2/S-P3
converged artifacts without the same pre-pin/revalidation qualifier
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:13-25`).
That is not harmless: the stale SPEC still carries CSS/Sheets/BBNF-self
selection order, fallback-on-CSS-preflight, and `ceil(baseline_mbps * 1.01)`
threshold language (`restart/skinny/tranches/sk-v12/SPEC.md:181-190`,
`restart/skinny/tranches/sk-v12/SPEC.md:501-502`). Under the USER PIN, CSS L4 is
authoritative and fallback is legal only after a measured CSS redress attempt
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-24`,
`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:29-37`).

Alpha-E must demote SPEC and pre-pin S-P artifacts in its authority list using
the same qualifier as SYNTHESIS/HANDOFF/Alpha-F, or stale pre-pin implementation
authority can leak back into candidate selection.

## Non-Blocking Checks

### CSS Track 1 / Track 2 / lightningcss plane symmetry

PASS. The folded packet requires generated CSS Track 1 to beat
`lightningcss_mbps + 1` on the same corpus, same output plane, and strict
equality semantics (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:42-53`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:53-58`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:71-84`).
Alpha-B and Alpha-E require one canonical CSS fact stream shared by generated
Track 1, independent Track 2/oracle, and lightningcss
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:107-113`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:77-84`).

### Bbnf-only or comparator-only normalization

PASS. Alpha-B explicitly rejects a bbnf-only bridge or lightningcss-only
normalization path and requires symmetric fact extraction from both generated
bbnf and lightningcss parses
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:107-113`).
Alpha-E's E1 oracle also bars calls into generated Track 1, generated CSS sink
helpers, or report fixtures
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:103-110`).

### Public substrate leakage from E4

PASS. E4 is constrained to a generated, CSS-local, immediately consumed
same-tape event projection and explicitly rejects a second retained substrate,
public substrate API, parser-owned sidecar, retained structural vector, and
parse-only scanner
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:225-240`).
Its owner list allows `event_grammar.rs` only for existing sealed/internal bounds
and forbids exported public substrate additions
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:241-249`).
The E4 gate requires public API diff proof that no directive, BIR variant,
BackendShape variant, `UnionTape`, generic event side vector, retained
cursor/list, or parser-owned fact slot was added
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:268-272`).

### SIMD orphan loopholes

PASS. The USER PIN names five orphan aarch64 primitives and makes zero orphan
kernels a campaign target
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71-79`).
SYNTHESIS and HANDOFF require the set to be zero by admission, removal, or
inventory demotion with evidence for ADMIT/FIXPOINT
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:57-65`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:87-89`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:81-85`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:91-95`).
Alpha-E E5 rejects dispatch-table-only/checkasm-only work as orphan and requires
the carried orphan set to be zero or inventory-demoted with evidence
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:318-339`).

## Required Fold

1. Add the full JSON guard refresh/demotion rule to Alpha-E E2/W1a because E2
   touches generic runtime/codegen/generated-output paths.
2. Qualify Alpha-E's SPEC and S-P1/S-P2/S-P3 authority reads as pre-pin context
   only after measured revalidation, matching SYNTHESIS/HANDOFF/Alpha-F.

CH5 remains REVISE until both folds land.
