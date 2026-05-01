# AX.W0c — AW-V Doc Cleanup

**Opens after**: W0b close
**Agents**: 1 serial
**Hard gate**: `docs/tranches/AW/AW-V.md` rewritten in RD language; no DTA branding.

## Scope

1. Strike Lever 4 `push_compound_fused_v32` from AW-V.md (§§Novel-levers table, wave-schedule references).
2. Strike "17-digit NEON lever" from AW-V.md projected-performance table.
3. Rewrite §Invariants.3 (drop cold-path replay hedge — the interpreter is gone, AX handles replay differently via AY).
4. Rewrite §Wave-schedule W5/W6 references to remove walker mentions; reframe walker role as historical scaffold retired in W0a/W0b — AST-level `*_parity.rs` semantic harnesses are the forward correctness oracle per AX invariant 20.
5. Rewrite §Delete-manifest to point to `docs/tranches/AW/audit/{dead-code-manifest,psi-and-dead-substrate,full-codebase-prune}.md`.
6. Rewrite §Successor-chain: `AW-V → AX (RD reckoning) → AY (replay + JIT)`. No AW-VI.
7. Retire "compile DTA into hot-path code" branding throughout. The thesis becomes *"fn-per-rule over shape templates; DTA-era IR facts feed the shape emitter; the scaffold came down as the consumer emerged."*
8. Add a §Pivot subsection documenting the W0a.2.h shape-emission-authoritative pivot. Cite `docs/benchmarks/archive/post-AX-W0a2g-progress.md` §Remaining-blockers (four deferred walker-parity deltas that motivated the pivot) + `docs/tranches/AX/audit/R4-plan-redress.md` (retrospective + invariant 20). The subsection is ≤ 30 lines; positions walker-parity chasing as a category-error the AW-V arc inherited, not a failure of the shape emitter itself.

## File bounds

| File | Access |
|---|---|
| `docs/tranches/AW/AW-V.md` | modify (extensive rewrite, not deletion) |
| `docs/tranches/AW/PROGRESS.md` | append W0c close entry |

## Hard gate

1. `grep -iE 'lever 4|push_compound_fused_v32|17-digit' docs/tranches/AW/AW-V.md` returns zero.
2. `grep -E 'cold-path replay|dta_run_cold|compile.*DTA.*into.*hot' docs/tranches/AW/AW-V.md` returns zero (RD-language rewrite complete).
3. Diff of AW-V.md shows every wave-schedule row references shape-emitter mechanisms, not walker.

## Dependencies

- Depends on: W0b (the interpreter must be gone before we document its retirement).
- Blocks: W1 (bench expectations for W1 need the corrected AW-V projection table).

## Archaeology

W0c mechanises the correction `SYNTHESIS-5-AW-V-RECKONING.md` §4 prescribes: retire DTA-compilation branding; rename the architectural thesis to fn-per-rule. AW-V's plan document was written pre-reckoning; this wave aligns the document with the landed architecture.
