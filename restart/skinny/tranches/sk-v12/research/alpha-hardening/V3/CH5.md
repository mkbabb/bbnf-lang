# SK-V12 Pass Alpha CH5 Hidden Coupling V3

Pass: Alpha SK-V11 -> SK-V12 hardening V3 under
`USER-PIN-W1-CSS-L4-SOTA.md`.
Date: 2026-05-20.
Lens: CH5 hidden coupling.
Output: this file only.

## Disposition

PASS.

V3 folds the V2 CH5 defects. The Alpha-E W1a/`GrammarConfig` gate now carries
the local JSON direct/typed guard refresh or measured-demotion rule, and the
Alpha-E authority list qualifies pre-pin `SPEC.md` plus S-P artifacts as context
only under the user pin. No remaining hidden-coupling blocker was found across
CSS plane symmetry, symmetric normalization, public substrate leakage, SIMD
orphans, JSON guard shortcuts, or stale SPEC coupling.

## Findings

### CH5-V3-1 - PASS: W1a local JSON guard refresh is no longer shortcuttable

V2 required Alpha-E E2/W1a to inherit the full JSON guard rule because E2 owns
generic runtime, tape, codegen, generated output, benchmark/report, and Lock 14
paths. Current Alpha-E names those owner paths at
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:145-160`.
Its W1a gate now requires direct/typed JSON guards to refresh or record measured
REDRESS demotion unless no JSON-producing path moved and `skinny/RESULTS.md` is
proven unchanged
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:179-182`).

This matches the top-level rule in
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:144-148`. E1 and E5 also carry the
same no-shortcut posture for JSON-producing paths
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:125-128`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:336-338`).

### CH5-V3-2 - PASS: Alpha-E stale authority coupling is qualified

Alpha-E no longer imports stale implementation authority unqualified. It reads
`SPEC.md` only as pre-pin context where it does not conflict with the user pin
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:14-15`),
and it reads pre-pin S-P1/S-P2/S-P3 converged artifacts only as context after
measured revalidation under the user pin
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:23-27`).

The stale SPEC clauses still exist: CSS/Sheets/BBNF-self are ordered as
preflight-style candidates and W2 still names
`ceil(baseline_mbps * 1.01)` in
`restart/skinny/tranches/sk-v12/SPEC.md:181-190` and
`restart/skinny/tranches/sk-v12/SPEC.md:499-502`. They are fenced off by
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:18-19`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:22-23`, and Alpha-F's stale-packet
warning at
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:11-16`
plus `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:235-237`.

### CH5-V3-3 - PASS: CSS Track 1 / Track 2 / lightningcss plane is symmetric

The close contract requires generated CSS L4 Track 1 to beat
`lightningcss_mbps + 1` on the same corpus, same output plane, same host, and
strict equality semantics, with one canonical CSS fact stream shared by
generated Track 1, independent Track 2/oracle, and lightningcss
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:42-53`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:53-58`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:71-84`).

Alpha-B closes the prior normalization bridge risk: the equality adapter must
derive the same declaration-value facts from generated bbnf and lightningcss,
must not be bbnf-only or lightningcss-only, and must move row id, fixture,
oracle, and comparator together if S-P3 selects a full-stylesheet plane
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:107-113`).
Alpha-E E1 carries the same declaration-value/full-stylesheet symmetry and
requires symmetric lightningcss fact extraction in the W1b gate
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:79-86`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:117-123`).

### CH5-V3-4 - PASS: E4 public substrate leakage is fenced

E4 is limited to a generated, CSS-local, immediately consumed same-tape event
projection. It rejects a second retained substrate, public substrate API,
parser-owned sidecar, retained structural vector, and parse-only scanner
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:242-246`).
Its owner list allows `event_grammar.rs` only to consume existing
sealed/internal bounds and forbids exported public substrate additions
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:252-255`).
The gate requires a public API diff proving no directive, BIR variant,
BackendShape variant, `UnionTape`, generic event side vector, retained
cursor/list, or parser-owned fact slot was added
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:274-278`).

The same refusal exists in the top-level packet:
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:216-217`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:165-166`, and
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:208-209`.

### CH5-V3-5 - PASS: SIMD orphan loopholes are closed for ADMIT and FIXPOINT

The user pin names five orphan aarch64 primitives and makes zero orphan kernels
a close target
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71-78`), matching
the SIMD audit inventory
(`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:34-61`).

V3 requires that carried set to be zero by admission, removal, or
inventory-demotion with evidence in ADMIT and FIXPOINT
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:57-65`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:87-89`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:81-85`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:91-95`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:95-98`).
Alpha-E E5 rejects checkasm-only or dispatch-table-only work as orphan and
requires the carried orphan set to be zero or inventory-demoted with evidence
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:324-338`).
Its revert rule leaves no orphan native body after a failed movement attempt
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:341-345`).

### CH5-V3-6 - PASS: JSON guard shortcut and stale SPEC coupling fail closed

The residual JSON guard shortcut is closed by the combination of SYNTHESIS and
Alpha-E local gates: changed JSON-producing runtime/codegen/generated-output,
bench, report, or gate paths must refresh JSON guards or record measured REDRESS
demotion; the no-refresh shortcut is legal only when no JSON-producing path
moved and `skinny/RESULTS.md` is proven unchanged
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:144-148`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:179-182`).
HANDOFF's shorter summary still requires refresh for JSON-producing changes
unless the result surface is proven unchanged
(`restart/skinny/tranches/sk-v12/HANDOFF.md:58-62`).

The stale SPEC is not erased, but it is no longer implementation authority for
the pin-conflicting baseline order or threshold. Current SYNTHESIS says only the
downstream pin-aware S-P3 packet may materialize replacement implementation
authority (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:5-11`) and requires
G-Alpha followed by S-P1, S-P2, and S-P3 under the user pin
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:237-254`).
HANDOFF mirrors that boundary and says downstream S-P3 may update `SPEC.md` and
`DISPATCH-PROMPT.md` only after those passes converge
(`restart/skinny/tranches/sk-v12/HANDOFF.md:168-173`).

## Result

CH5 returns PASS for Pass Alpha hardening V3. No hidden-coupling REVISE finding
remains in the reviewed surfaces.
