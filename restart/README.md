# Restart — Greenfield Re-architecture

This directory carries the entire restart effort for bbnf-lang: the prompt suite that governs it, the audit material that informs it, the corpora it cites, the locks that constrain it, the tranches it produces, and pointers back to the inheritance source.

## Contents

```
restart/
├── README.md                  ← this file (top-level orchestration)
├── prompts/                   ← the 5-prompt suite
│   ├── README.md              ← suite orchestration
│   ├── PASS-A.md              ← parse-front pass
│   ├── PASS-B.md              ← codegen-mid pass
│   ├── PASS-C.md              ← periphery + commit-chain pass
│   ├── SYNTHESIZER.md         ← master-plan synthesizer
│   └── HARDENING.md           ← double-back hardening (any target)
├── audit/                     ← restart-effort audit outputs
│   ├── passes/
│   │   ├── PASS-A.md          ← Pass A synthesis (829 lines)
│   │   ├── PASS-B.md          ← Pass B synthesis (548 lines)
│   │   └── PASS-C.md          ← Pass C synthesis (486 lines)
│   ├── per-agent/             ← 18 per-agent reports (6 per pass)
│   ├── master-plan/
│   │   └── MASTER-PLAN.md     ← Synthesizer's master plan (1,418 lines)
│   └── hardening/             ← double-back audit outputs (post-tranche-draft)
├── corpora/                   ← input corpora the audit cites
│   ├── CENSUS.md              ← kill-list (grammar-specific, tape residue, dupes, god modules)
│   ├── MODULES.md             ← per-file fates + 17-step pipeline
│   ├── RESTART-SKETCH.md      ← JSON parse trace + post-restart sketch
│   ├── SOTA.md                ← sonic-rs / simdjson / lightning-css research
│   ├── HARDENING-PLAN-SYNTHESIS.md   ← Phase-3 9-lane audit synthesis
│   ├── PHASE-4-SYNTHESIS.md   ← Phase-4 partial synthesis (quota-capped)
│   └── lanes/                 ← Phase-3 8 audit lanes
├── locks/
│   ├── 14-LOCKS.md            ← canonical 14-lock master (the architectural commitments)
│   └── PHASE-4-DIRECTIVE.md   ← Phase-4 spec-depth directive
├── tranches/                  ← FULLY-SPECIFIED tranches A through J
│   ├── A/                     ← Workspace genesis
│   ├── B/                     ← bbnf-error + bbnf-pipeline foundation
│   ├── C/                     ← Parse + IR foundation
│   ├── D/                     ← Codegen IR contract
│   ├── E/                     ← Per-grammar declaration crates + runtime template (convergent pivot)
│   ├── F/                     ← Optimiser pipeline
│   ├── G/                     ← Slice-borrow API + pointer macro + visitor
│   ├── H/                     ← TS + WASM emitters
│   ├── I/                     ← Sister-crate publication
│   └── J/                     ← Cross-backend parity + close
└── legacy-source/
    └── INHERITANCE-INDEX.md   ← pointer to docs/tranches/{BA,BB,BC,BD}/ + mapping
```

## Provenance

- `docs/precepts/` — git submodule; voice + discipline; read-only by the restart effort
- `docs/tranches/{Y..BD}/` — legacy tranche set; BA-BD carry the inheritance specifications the new tranches consume; archive disposition (per-Pass-C: keep verbatim + branch reset; tag `pre-restart-2026-05-03`) executes at Tranche A.W0 during execution

## Status

| Phase | Status | Output |
|---|---|---|
| Phase 1 — Pass A (parse-front) | ✅ committed `6e74a4b1` | `audit/passes/PASS-A.md` |
| Phase 1 — Pass B (codegen-mid) | ✅ committed `803c7f46cd` | `audit/passes/PASS-B.md` |
| Phase 1 — Pass C (periphery + commit-chain) | ✅ committed `39263770` | `audit/passes/PASS-C.md` |
| Phase 2 — Synthesizer | ✅ committed `a9a85f45` | `audit/master-plan/MASTER-PLAN.md` |
| Phase 3 — Tranche full-spec drafting | ⏳ in flight | `tranches/{A..J}/` |
| Phase 4 — Hardening (master plan) | pending | `audit/hardening/MASTER-PLAN.md` |
| Phase 5 — Tranche execution | OUT OF SCOPE | (post-restart) |

## Discipline

The restart honours fourteen architectural commitments enumerated at `locks/14-LOCKS.md`. Particularly Lock 14 — full grammar generalisation; zero overfitting — is the most consequential enforcement target. The future-grammar onboarding test (a hypothetical 10th grammar `yaml.bbnf` adds via three declarative surfaces only — source file, workspace metadata, declaration crate) gates every tranche's ratification.

The voice is calibrated, archaic-permissive, no metalanguage. The mandate is greenfield: no quick solutions, no workarounds, no legacy code uncontested, idiomatic gestalt. Path:line citations on every concrete claim.

## What is NOT in this directory

- `docs/precepts/` — submodule; the source-of-truth for voice + LESSONS-LEARNED + ORCHESTRATION precepts
- The bbnf-lang source tree (`crates/`, `grammar/`, `xtask/`, etc.) — read by audit; not modified by the restart suite
- The legacy tranche tree (`docs/tranches/{Y..BD}/`) — inheritance source; remains in place until execution-time archive ceremony

## Closing posture

Hereupon the restart directory is self-contained as the planning workspace. Every artefact governing the next ~6-12 months of bbnf-lang work lives herein. The hardening pass at `prompts/HARDENING.md` is the gate; the tranche execution agents (out of scope for this suite) consume `tranches/{A..J}/` to draft full implementations.
