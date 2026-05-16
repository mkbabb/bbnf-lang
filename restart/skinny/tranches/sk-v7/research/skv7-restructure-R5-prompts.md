# SK-V7 Restructure R5 — `restart/prompts/`

User mandate, verbatim: "the prompts dir should be better structured, named thereof."

The current directory carries nine files with three distinct authorship vintages (pre-V7, V7-author, mixed), three conceptual roles (orchestrator / sub-orchestrator / pass-contract), and naming patterns that have drifted across the corpus. The README the V7 author shipped declares files that do not exist (`TOTALITY-PASS-1-RESEARCH.md`, `TOTALITY-PASS-2-PROFILE.md`, `TOTALITY-PASS-3-HARDENING.md`). This restructure proposal resolves all of the above with a single subdirectory-based layout, a uniform naming convention within each subdirectory, the two load-bearing renames the user's mandate implies (`HARDENING.md` → `HARDENING-LENS-SET.md`; `SKINNY-PASSES.md` → `SKINNY-TRIUMVIRATE.md`), and an honest README that maps to the files that actually exist.

## §1 — Conceptual taxonomy (the nine files, classified)

The nine files cluster into four roles. The classification is mechanical from each file's opening paragraph.

| File | Opening role declaration | Role classification |
|---|---|---|
| `README.md` (162 LOC) | `restart/prompts/README.md:1` "Prompt Suite — Iterative Auto-Convergent Multi-Pass Framework" | **Gestalt** (framework-level introduction; non-dispatching) |
| `ORCHESTRATOR.md` (145 LOC) | `restart/prompts/ORCHESTRATOR.md:3` "single main orchestrator prompt for the bbnf-lang greenfield restart" | **Top-level orchestrator** (phase identification + fan-out to one of three sub-orchestrators) |
| `HARDENING-ORCHESTRATOR.md` (151 LOC) | `restart/prompts/HARDENING-ORCHESTRATOR.md:1` "You are the hardening sub-orchestrator" | **Sub-orchestrator** (dispatches hardening cycles V1-V9+) |
| `RESEARCH-FOLD-ORCHESTRATOR.md` (262 LOC) | `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md:1` "This document is a runnable orchestrator specification" | **Sub-orchestrator** (research deep-dive + fold cycles) |
| `AMENDMENT-DISPATCH.md` (189 LOC) | `restart/prompts/AMENDMENT-DISPATCH.md:1` "You are the amendment-dispatch sub-orchestrator" | **Sub-orchestrator** (verify-then-patch surgical reconciliation cycles) |
| `PASS-ALPHA.md` (206 LOC) | `restart/prompts/PASS-ALPHA.md:3` "Pass Alpha is the skinny astral synthesis pass" | **Pass contract** (skinny SK-V{N+1} synthesis, six-agent fan-out + CHALLENGE + CRUD) |
| `PASS-OMEGA.md` (182 LOC) | `restart/prompts/PASS-OMEGA.md:3` "Pass Omega is the totality astral synthesis pass" | **Pass contract** (totality V1 spec cohesion + skinny fold-in) |
| `SKINNY-PASSES.md` (210 LOC) | `restart/prompts/SKINNY-PASSES.md:3` "the contract for skinny passes 1-n... per-iteration triumvirate cycles" | **Pass contract** (single contract: per-wave research/plan/redress triumvirate) |
| `HARDENING.md` (268 LOC) | `restart/prompts/HARDENING.md:3` "You are the hardening agent. Your role is to challenge..." | **Audit specification** (per-target lens contract; load-bearing input to every hardening dispatch) |

Four roles, four destinations in the proposed layout:

| Role | Count | Proposed subdirectory |
|---|---|---|
| Gestalt | 1 | `restart/prompts/` (top-level) |
| Top-level orchestrator | 1 | `restart/prompts/` (top-level; the one entry point) |
| Sub-orchestrator | 3 | `restart/prompts/sub-orchestrators/` |
| Pass contract | 3 | `restart/prompts/pass-contracts/` |
| Audit specification | 1 | `restart/prompts/audit-specs/` |

## §2 — Naming inconsistency inventory

The current names carry five inconsistency classes.

### §2.1 — Suffix drift (`-ORCHESTRATOR` vs. bare)

`HARDENING-ORCHESTRATOR.md` carries the `-ORCHESTRATOR` suffix to declare its role; `RESEARCH-FOLD-ORCHESTRATOR.md` does the same. `AMENDMENT-DISPATCH.md` is the third sub-orchestrator but drops the suffix in favour of the `-DISPATCH` action verb. Three sub-orchestrators; two naming patterns. Choose one.

### §2.2 — `HARDENING` ambiguity (the central naming fault)

Two files share the `HARDENING` prefix but do different things:

- `restart/prompts/HARDENING.md` (268 LOC) — the **per-target audit specification** that each dispatched hardener agent reads as its operational contract. Per `restart/prompts/HARDENING.md:1` "You are the hardening agent". It contains lens definitions (A through K), per-item discipline (Pro/Con/Explication/Challenge), verdict classes (KEEP/REINVENT/DISCARD + SIMPLIFY/CONSOLIDATE/LEVERAGE/HYBRID/LOAD-BEARING/ASPIRATIONAL/SPECULATIVE), and the output contract.
- `restart/prompts/HARDENING-ORCHESTRATOR.md` (151 LOC) — the **sub-orchestrator** that dispatches hardening cycles. Per `restart/prompts/HARDENING-ORCHESTRATOR.md:1` "You are the hardening sub-orchestrator. Your role is to coordinate a hardening cycle (V1 through V8+) by dispatching four parallel hardener agents."

These are not parent/child — they are dispatcher and audit-spec-contract. The `-ORCHESTRATOR` suffix is doing the disambiguation work, but reading `HARDENING.md` first gives the wrong mental model (one would expect the contract file to be the orchestrator, not the audit-spec).

The `restart/prompts/ORCHESTRATOR.md:14-16` required reading already calls out the awkwardness:

```
6. `restart/prompts/HARDENING.md` — per-target audit specification (the contract each hardening agent reads).
7. `restart/prompts/HARDENING-ORCHESTRATOR.md` — sub-orchestrator for hardening cycles.
```

The parenthetical at line 6 is doing the disambiguation work the filename should do. Rename: `HARDENING.md` → `HARDENING-LENS-SET.md` (or equivalent). The lens set is what makes `HARDENING.md` distinctive — it is the only file in the directory that defines an adversarial lens vocabulary. `restart/skinny/HARDENING.md:19` already uses the phrase "lens stack" to refer to this content.

### §2.3 — `SKINNY-PASSES.md` plurality

The filename is plural but `restart/prompts/SKINNY-PASSES.md:3` declares "the contract for skinny passes 1-n" — singular contract for an unbounded count of cycle invocations. The content is a single triumvirate-pattern specification (research/plan/redress), not three pass definitions. The plurality is misleading; readers expect either (a) one file per pass type or (b) a single contract whose name reflects its content.

Rename: `SKINNY-PASSES.md` → `SKINNY-TRIUMVIRATE.md`. The triumvirate is the load-bearing concept (research/plan/redress; three commits per wave; `restart/prompts/SKINNY-PASSES.md:191-200` "Triumvirate role separation (load-bearing)"). The triumvirate is also the SK-V6/SK-V7 vocabulary in flight — `restart/skinny/audit/HANDOFF-SK-V7.md:12` cites the file by its triumvirate role.

### §2.4 — `PASS-ALPHA` / `PASS-OMEGA` opacity

`PASS-ALPHA` and `PASS-OMEGA` are short and memorable but opaque to a first-time reader. The README.md author already supplies the long-form gloss inline (`restart/prompts/README.md:7-8` "skinny astral synthesis" / "totality astral synthesis"). The choice is between:

- Keep the bracket short names (the vocabulary is already established in the corpus at `restart/skinny/audit/GRAND-SYNTHESIS-SK-V7.md:11` and `IMPLEMENTATION-PACKET-SK-V7.md:12`).
- Lengthen to `PASS-ALPHA-SKINNY-ASTRAL.md` / `PASS-OMEGA-TOTALITY-ASTRAL.md` (descriptive but verbose).
- Drop the Greek-letter prefix and use `SKINNY-ASTRAL.md` / `TOTALITY-ASTRAL.md` (clearer about role; loses the Alpha/Omega bracket symmetry that the README leans on).

Decision: keep `PASS-ALPHA.md` / `PASS-OMEGA.md`. The Greek-letter bracket carries semantic weight (Alpha = next-cycle creation; Omega = totality fold; named gates G-Alpha / G-Omega per `restart/prompts/README.md:25`). Lengthening reduces clarity in the cross-corpus citations. The subdirectory placement (`pass-contracts/`) supplies the role context the filename lacks.

### §2.5 — Sort-order opacity

In `ls`, the nine files sort alphabetically (`AMENDMENT-DISPATCH`, `HARDENING`, `HARDENING-ORCHESTRATOR`, `ORCHESTRATOR`, `PASS-ALPHA`, `PASS-OMEGA`, `README`, `RESEARCH-FOLD-ORCHESTRATOR`, `SKINNY-PASSES`) — the alphabetical order is the inverse of the reading order. A first-time reader hitting `ls` first reads `AMENDMENT-DISPATCH.md` before `README.md`. Two remedies:

- Numeric prefixes (`00-README.md`, `10-ORCHESTRATOR.md`, `20-sub-orchestrators/`, `30-pass-contracts/`, `40-audit-specs/`). Forces sort-order = reading-order. Cost: every cross-doc reference (the 26+ citations enumerated in §6 below) needs updating.
- Subdirectory hierarchy (no numeric prefix; subdirs do the structural work; README's reading-order block is the prose-level guide). Cost: no path renames except the two §2.2 / §2.3 renames + the move-to-subdir.

Decision: subdirectory hierarchy without numeric prefix. The subdirectory names (`pass-contracts/`, `sub-orchestrators/`, `audit-specs/`) carry the role information that numeric prefixes would carry. Less brittle to corpus reference churn. Aligns with the hassio-config `restart/prompts/dispatch/` pattern at `/Users/mkbabb/Programming/hassio-config/docs/restart/prompts/dispatch/` (which uses a single subdirectory for per-pass dispatch templates; no numeric prefix).

## §3 — Proposed layout

```
restart/prompts/
├── README.md                                   ← framework gestalt (rewritten per §5)
├── ORCHESTRATOR.md                             ← top-level orchestrator (unchanged content)
├── sub-orchestrators/
│   ├── HARDENING.md                            ← formerly HARDENING-ORCHESTRATOR.md
│   ├── RESEARCH-FOLD.md                        ← formerly RESEARCH-FOLD-ORCHESTRATOR.md
│   └── AMENDMENT-DISPATCH.md                   ← (unchanged name; subdir conveys role)
├── pass-contracts/
│   ├── PASS-ALPHA.md                           ← (unchanged name; skinny astral)
│   ├── PASS-OMEGA.md                           ← (unchanged name; totality astral)
│   └── SKINNY-TRIUMVIRATE.md                   ← formerly SKINNY-PASSES.md
└── audit-specs/
    └── HARDENING-LENS-SET.md                   ← formerly HARDENING.md
```

The subdirectory names eliminate three suffixes that the subdirectory itself carries semantically:

- `sub-orchestrators/HARDENING.md` reads as "sub-orchestrator: hardening" without the `-ORCHESTRATOR` suffix being load-bearing in the filename.
- `sub-orchestrators/RESEARCH-FOLD.md` reads as "sub-orchestrator: research-fold" similarly.
- `audit-specs/HARDENING-LENS-SET.md` reads as "audit spec: hardening lens set" — the `LENS-SET` portion of the filename is now the disambiguator against `sub-orchestrators/HARDENING.md`, which is exactly the relationship the user mandate names.

The four-deep nesting (`prompts/{sub-orchestrators,pass-contracts,audit-specs}/`) carries the role taxonomy; the leaf filenames carry the specific role-content. No filename in the tree disambiguates against another via suffix alone.

## §4 — Rename + move mapping table

| Old path | New path | Operation | Rationale |
|---|---|---|---|
| `restart/prompts/README.md` | `restart/prompts/README.md` | Rewrite content | Existing content declares non-existent files (`TOTALITY-PASS-1/2/3-*.md`); rewrite to match actual layout per §5 |
| `restart/prompts/ORCHESTRATOR.md` | `restart/prompts/ORCHESTRATOR.md` | Keep | Top-level orchestrator stays at top level; content unchanged at file level but the §1 required-reading list (`restart/prompts/ORCHESTRATOR.md:14-17`) needs path updates per §6 |
| `restart/prompts/HARDENING-ORCHESTRATOR.md` | `restart/prompts/sub-orchestrators/HARDENING.md` | Move + rename | Subdir conveys "sub-orchestrator"; the `-ORCHESTRATOR` suffix becomes redundant |
| `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` | `restart/prompts/sub-orchestrators/RESEARCH-FOLD.md` | Move + rename | Same rationale as HARDENING; `-ORCHESTRATOR` suffix retired |
| `restart/prompts/AMENDMENT-DISPATCH.md` | `restart/prompts/sub-orchestrators/AMENDMENT-DISPATCH.md` | Move | Subdir conveys role; filename already had `-DISPATCH` action verb (kept; less redundant with subdir than `-ORCHESTRATOR` would have been) |
| `restart/prompts/PASS-ALPHA.md` | `restart/prompts/pass-contracts/PASS-ALPHA.md` | Move | Subdir conveys "pass-contract" role |
| `restart/prompts/PASS-OMEGA.md` | `restart/prompts/pass-contracts/PASS-OMEGA.md` | Move | Same |
| `restart/prompts/SKINNY-PASSES.md` | `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` | Move + rename | Plural → singular; "triumvirate" names the actual content (per `restart/prompts/SKINNY-PASSES.md:7` "the triumvirate structure" + `:191` "triumvirate role separation (load-bearing)") |
| `restart/prompts/HARDENING.md` | `restart/prompts/audit-specs/HARDENING-LENS-SET.md` | Move + rename | Resolves the `HARDENING` ambiguity with `HARDENING-ORCHESTRATOR`; "lens set" names the distinctive content per `restart/prompts/HARDENING.md:189-194` cycle-lens table + `restart/skinny/HARDENING.md:19` "lens stack" vocabulary |

Operation counts: **2 renames** (`HARDENING.md` → `HARDENING-LENS-SET.md`; `SKINNY-PASSES.md` → `SKINNY-TRIUMVIRATE.md`), **5 moves-with-implicit-rename** (the `-ORCHESTRATOR` suffix retirement on two files; the bare moves on three files), **1 rewrite** (`README.md`), **1 keep** (`ORCHESTRATOR.md` content; only required-reading paths update).

No file is deleted. No file is added beyond the proposed `restart/prompts/dispatch/` subdir question addressed in §7.

## §5 — README rewrite (the load-bearing fix)

The current `restart/prompts/README.md:1-10` reading-order block declares five files that do not exist:

```
5. The four per-pass contracts: `TOTALITY-PASS-1-RESEARCH.md`, `TOTALITY-PASS-2-PROFILE.md`,
   `TOTALITY-PASS-3-HARDENING.md`, `SKINNY-PASSES.md`.
```

Three of those four files (`TOTALITY-PASS-1-RESEARCH.md`, `TOTALITY-PASS-2-PROFILE.md`, `TOTALITY-PASS-3-HARDENING.md`) do not exist in `restart/prompts/`. The README was written aspirationally to match a desired layout, not the layout that landed.

Further, the repository-layout block at `restart/prompts/README.md:101-140` declares the same non-existent files. The repository-layout block at lines 101-111:

```
restart/
├── prompts/                                 ← THIS DIRECTORY (the pass contracts)
│   ├── README.md                            ← framework gestalt
│   ├── ORCHESTRATOR.md                      ← dispatch + iteration governance
│   ├── PASS-ALPHA.md                        ← skinny astral synthesis (SK-n creation)
│   ├── PASS-OMEGA.md                        ← totality astral synthesis
│   ├── TOTALITY-PASS-1-RESEARCH.md          ← totality pass 1 contract
│   ├── TOTALITY-PASS-2-PROFILE.md           ← totality pass 2 contract
│   ├── TOTALITY-PASS-3-HARDENING.md         ← totality pass 3 contract
│   └── SKINNY-PASSES.md                     ← skinny passes 1-n contract
```

The actual layout includes `HARDENING.md`, `HARDENING-ORCHESTRATOR.md`, `RESEARCH-FOLD-ORCHESTRATOR.md`, `AMENDMENT-DISPATCH.md` — four files the README omits — and lacks `TOTALITY-PASS-1-RESEARCH.md`, `TOTALITY-PASS-2-PROFILE.md`, `TOTALITY-PASS-3-HARDENING.md` — three files the README claims. Net delta: 4 omitted + 3 confabulated = 7 README–reality drift entries.

The rewrite re-anchors the README to the actual layout. The new reading-order block:

```
1. This README (framework gestalt).
2. `ORCHESTRATOR.md` (phase identification + dispatch matrix + iteration governance).
3. `sub-orchestrators/HARDENING.md` (hardening cycle sub-orchestrator).
4. `sub-orchestrators/RESEARCH-FOLD.md` (research deep-dive + fold sub-orchestrator).
5. `sub-orchestrators/AMENDMENT-DISPATCH.md` (verify-then-patch surgical reconciliation).
6. `pass-contracts/PASS-ALPHA.md` (skinny astral synthesis — SK-V{N+1} cycle creation).
7. `pass-contracts/PASS-OMEGA.md` (totality astral synthesis — V1 spec cohesion).
8. `pass-contracts/SKINNY-TRIUMVIRATE.md` (per-wave research/plan/redress contract).
9. `audit-specs/HARDENING-LENS-SET.md` (lens contract — lenses A through K).
```

The repository-layout block at lines 101-140 of the current README is rewritten with the same correction. The framework gestalt prose at lines 11-160 stands largely unchanged — it correctly describes the two-track architecture (totality V1 + skinny subset), the two-astral-pass discipline (Alpha + Omega), and the convergence-criterion vocabulary. Only the file-mapping anchors change.

## §6 — Reference updates required (cross-corpus citations)

Renaming and moving files invalidates the following cross-corpus citations. Each must be updated before commit; the alternative is leaving stale path:line citations across the corpus, which is itself a lens-H hallucination class.

| Citing path | Citing line | Old reference | New reference |
|---|---|---|---|
| `restart/README.md` | 420 | `restart/prompts/ORCHESTRATOR.md` | unchanged (file stays at top level) |
| `restart/README.md` | 421 | `restart/prompts/HARDENING-ORCHESTRATOR.md` | `restart/prompts/sub-orchestrators/HARDENING.md` |
| `restart/README.md` | 422 | `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` | `restart/prompts/sub-orchestrators/RESEARCH-FOLD.md` |
| `restart/README.md` | 423 | `restart/prompts/AMENDMENT-DISPATCH.md` | `restart/prompts/sub-orchestrators/AMENDMENT-DISPATCH.md` |
| `restart/README.md` | 424 | `restart/prompts/HARDENING.md` | `restart/prompts/audit-specs/HARDENING-LENS-SET.md` |
| `restart/README.md` | 466 | "The five prompts at `restart/prompts/` dispatch next" | "The five sub-orchestrator + pass-contract files at `restart/prompts/{sub-orchestrators,pass-contracts}/` dispatch next" (or equivalent restatement) |
| `restart/prompts/ORCHESTRATOR.md` | 14 | `restart/prompts/HARDENING.md` | `restart/prompts/audit-specs/HARDENING-LENS-SET.md` |
| `restart/prompts/ORCHESTRATOR.md` | 15 | `restart/prompts/HARDENING-ORCHESTRATOR.md` | `restart/prompts/sub-orchestrators/HARDENING.md` |
| `restart/prompts/ORCHESTRATOR.md` | 16 | `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` | `restart/prompts/sub-orchestrators/RESEARCH-FOLD.md` |
| `restart/prompts/ORCHESTRATOR.md` | 17 | `restart/prompts/AMENDMENT-DISPATCH.md` | `restart/prompts/sub-orchestrators/AMENDMENT-DISPATCH.md` |
| `restart/prompts/ORCHESTRATOR.md` | 35-39 | phase-type table cell references | each row's sub-orchestrator path |
| `restart/prompts/sub-orchestrators/HARDENING.md` (formerly HARDENING-ORCHESTRATOR.md) | 5, 13-15 | required-reading list | path updates throughout |
| `restart/prompts/sub-orchestrators/RESEARCH-FOLD.md` (formerly RESEARCH-FOLD-ORCHESTRATOR.md) | 16-18 | required-reading list | path updates throughout |
| `restart/prompts/sub-orchestrators/AMENDMENT-DISPATCH.md` | 10-13 | required-reading list | path updates throughout |
| `restart/prompts/pass-contracts/PASS-ALPHA.md` (no internal cross-refs to peer prompts beyond §6 output structure) | — | — | no internal references require updates |
| `restart/prompts/pass-contracts/PASS-OMEGA.md` | 11, 146-154 | references to `RESEARCH-FOLD-ORCHESTRATOR.md`, `HARDENING-ORCHESTRATOR.md`, `AMENDMENT-DISPATCH.md` | each cited path updates per the move table §4 |
| `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (formerly SKINNY-PASSES.md) | (no internal cross-refs to peer prompts) | — | no internal references require updates |
| `restart/prompts/audit-specs/HARDENING-LENS-SET.md` (formerly HARDENING.md) | 196 | "The hardening orchestrator (`HARDENING-ORCHESTRATOR.md`) selects the lens set per cycle" | "The hardening sub-orchestrator (`restart/prompts/sub-orchestrators/HARDENING.md`) selects the lens set per cycle" |
| `restart/research/fold-pass-1.md` | 38 | `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` Phase 2 | `restart/prompts/sub-orchestrators/RESEARCH-FOLD.md` Phase 2 |
| `restart/research/fold-pass-1.md` | 39 | `restart/prompts/AMENDMENT-DISPATCH.md` §1 | `restart/prompts/sub-orchestrators/AMENDMENT-DISPATCH.md` §1 |
| `restart/research/fold-pass-2.md` | 22-23 | same | same updates |
| `restart/research/fold-pass-3.md` | 23-24 | same | same updates |
| `restart/research/fold-synthesis.md` | 31-32 | same | same updates |
| `restart/research/INDEX.md` | 5 | `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` | `restart/prompts/sub-orchestrators/RESEARCH-FOLD.md` |
| `restart/research/CORPUS-AUDIT-1-TOP-LEVEL-PROMPTS.md` | 4 + 15 + 49-53 + 93 + 133 + 175 + 208 + 215 + 261-262 + 273 + 296-299 + 312 | extensive prompts/ citations | full sweep per move table §4 |
| `restart/research/CORPUS-AUDIT-2-PASS-DIRS.md` | 159 | `restart/prompts/HARDENING.md` | `restart/prompts/audit-specs/HARDENING-LENS-SET.md` |
| `restart/research/CORPUS-AUDIT-3-HARDENING-DIR.md` | 141 + 223 | `restart/prompts/HARDENING.md` + `restart/prompts/AMENDMENT-DISPATCH.md` | per move table |
| `restart/research/CORPUS-AUDIT-SYNTHESIS.md` | 10 + 41-45 + 77 + 131 + 170-171 | extensive prompts/ citations | per move table |
| `restart/skinny/HARDENING.md` | 9 + 19 + 42-43 + 54 + 147 + 192 + 198 | references to V1 hardening prompt | each updates to `restart/prompts/audit-specs/HARDENING-LENS-SET.md` (or `restart/prompts/sub-orchestrators/HARDENING.md` where it cites the sub-orchestrator) |
| `restart/skinny/INDEX.md` | 27 | `restart/prompts/HARDENING.md` | `restart/prompts/audit-specs/HARDENING-LENS-SET.md` |
| `restart/skinny/audit/HANDOFF-SK-V7.md` | 9-12 | `restart/prompts/README.md`, `ORCHESTRATOR.md`, `PASS-ALPHA.md`, `SKINNY-PASSES.md` | latter two update per move table |
| `restart/skinny/audit/GRAND-SYNTHESIS-SK-V7.md` | 11 + 23 | `restart/prompts/PASS-ALPHA.md` + `{README,PASS-ALPHA,PASS-OMEGA,SKINNY-PASSES}.md` | per move table |
| `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V7.md` | 12-13 + 83 | `restart/prompts/PASS-ALPHA.md` + `SKINNY-PASSES.md` + `README.md` | per move table |
| `restart/audit/hardening/HARDENING-PASS-1-V9.md` | 86 | `restart/prompts/HARDENING.md:230-246` | `restart/prompts/audit-specs/HARDENING-LENS-SET.md:230-246` |
| `restart/audit/hardening/HARDENING-PASS-3-V9.md` + `HARDENING-PASS-3-V9.1.md` + `HARDENING-CONSOLIDATED-V9.1.md` + `HARDENING-MASTER-PLAN-V9.1.md` + `HARDENING-SYNTHESIS-V6.md` | various | `restart/prompts/HARDENING.md` and `restart/prompts/ORCHESTRATOR.md` references | sweep per move table |

Total identified citing files: **22 files** with **~50+ individual line references**. The actual sweep is a single `rg -l 'restart/prompts/'` + targeted `sed`-class rewrites; the deliverable is a single follow-on commit. All references are mechanical path substitutions; no semantic content changes.

## §7 — `dispatch/` subdir question (per hassio-config pattern)

The hassio-config restart corpus at `/Users/mkbabb/Programming/hassio-config/docs/restart/prompts/dispatch/` carries:

```
dispatch/
├── PASS-1.md
├── PASS-2.md
├── PASS-3.md
└── README.md
```

These are per-pass dispatch templates — the operational prompts that the hassio-config orchestrator parameterises at dispatch time. The pattern is: the parent `restart/prompts/` carries the meta-orchestrator + the role-contract specs; the `dispatch/` subdir carries the ready-to-fire per-pass templates the orchestrator instantiates.

Does bbnf-lang need an analogous `dispatch/` subdir?

Reading the current bbnf-lang sub-orchestrators:

- `restart/prompts/HARDENING-ORCHESTRATOR.md:110-122` ("Per-dispatch prompts you compose") declares the dispatch prompts are **composed at dispatch time, not pre-written**. The contract at `HARDENING.md` is the per-agent specification.
- `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md:231-246` ("Per-agent dispatch prompt template") similarly declares the orchestrator-agent **composes** dispatch prompts at dispatch time.
- `restart/prompts/AMENDMENT-DISPATCH.md:75-89` ("Per-Wave Dispatch Prompts") similarly **composes** at dispatch time.

bbnf-lang's pattern is **deliberately non-template**: every dispatch composes the prompt from the role-contract spec + the cycle-specific parameters. A `dispatch/` subdir with pre-written templates would contradict the composability discipline.

**Recommendation: reject `dispatch/` subdir for bbnf-lang.** The hassio-config pattern fits hassio-config's per-pass repeatable structure (PASS-1, PASS-2, PASS-3 are fixed pass shapes); bbnf-lang's passes are dynamically parameterised per cycle (V1, V2, … V9+ with cycle-specific lens sets, target lists, wave-specific punch lists) and do not admit pre-written templates.

If a future user-mandate requires per-pass templates (e.g., to standardise the dispatch composition step itself), the subdirectory can be added then. For the current restructure, the absence of `dispatch/` is a deliberate design choice, not an omission.

## §8 — Decision matrix (summary)

| Decision | Choice | Rationale |
|---|---|---|
| Naming scheme | Subdirectory hierarchy (no numeric prefix) | Less brittle to corpus reference churn; subdirs carry role taxonomy; aligns with hassio-config `dispatch/` precedent for using a subdir as the structural unit |
| `HARDENING.md` rename | `audit-specs/HARDENING-LENS-SET.md` | Resolves `HARDENING.md` vs. `HARDENING-ORCHESTRATOR.md` ambiguity; "lens set" names the distinctive content; consistent with `restart/skinny/HARDENING.md:19` "lens stack" vocabulary |
| `SKINNY-PASSES.md` rename | `pass-contracts/SKINNY-TRIUMVIRATE.md` | Resolves plurality/singular content mismatch; "triumvirate" is the load-bearing concept (`restart/prompts/SKINNY-PASSES.md:191-200`) |
| `PASS-ALPHA` / `PASS-OMEGA` rename | Keep as-is, place in `pass-contracts/` | Greek-letter bracket carries semantic weight (gates G-Alpha / G-Omega); subdir supplies role context |
| Sub-orchestrator suffix retirement | `-ORCHESTRATOR` retired from `HARDENING.md`, `RESEARCH-FOLD.md`; `-DISPATCH` retained on `AMENDMENT-DISPATCH.md` | Subdir conveys role; `-ORCHESTRATOR` redundant; `-DISPATCH` is an action verb (not a role marker), kept |
| README rewrite | Mandatory | Current README declares 7 nonexistent-or-mislabeled files; standalone fault even pre-restructure |
| `ORCHESTRATOR.md` content | Keep; update §1 required-reading paths only | Top-level orchestrator content is correct; only the path citations need updating per §6 |
| `dispatch/` subdir for bbnf-lang | Reject | Contradicts the compose-at-dispatch-time discipline that all three current sub-orchestrators establish |
| Cross-corpus reference sweep | Mandatory, mechanical, ~22 files | Stale path:line citations are a lens-H hallucination class; the sweep is a single follow-on commit |

## §9 — Pruning summary

- **Files renamed**: 2 (`HARDENING.md` → `HARDENING-LENS-SET.md`; `SKINNY-PASSES.md` → `SKINNY-TRIUMVIRATE.md`)
- **Files moved**: 7 (all six non-top-level files into one of three subdirs; the renames above are also moves)
- **Files at top level after restructure**: 2 (`README.md` + `ORCHESTRATOR.md`)
- **Files deleted**: 0
- **Files added**: 0 (no `dispatch/` subdir per §7; no new pass contracts)
- **Subdirectories added**: 3 (`sub-orchestrators/`, `pass-contracts/`, `audit-specs/`)
- **Files rewritten**: 1 (`README.md`; existing content declares nonexistent files per §5)
- **Files with internal path-citation updates only**: 5 (all six non-README files reference each other; each requires path updates per §6)
- **Cross-corpus citing files requiring sweep**: 22 (per §6 enumeration)

## §10 — Closing posture

The directory's structural fault is that it grew organically across three authorship vintages (pre-V7 sub-orchestrators, V7-author pass contracts, mixed README). The three role taxonomies (orchestrator / sub-orchestrator / pass-contract / audit-spec) exist implicitly in the content but are not surfaced in the filenames or layout. The two suffix conventions (`-ORCHESTRATOR` vs. bare) and the two `HARDENING`-prefixed files of distinct roles compound the readability cost.

The proposed restructure surfaces the role taxonomy as subdirectories, retires the redundant suffixes the subdirectories now carry, resolves the two confusing names (`HARDENING.md` → `HARDENING-LENS-SET.md`; `SKINNY-PASSES.md` → `SKINNY-TRIUMVIRATE.md`), and rewrites the README that currently declares nonexistent files. The cross-corpus reference sweep is mechanical (~22 files; ~50 line references; single follow-on commit).

The work is bounded by the move table at §4 + the citation sweep at §6. No file is deleted. No new pass contract is authored. The naming + structure carries the role taxonomy the content already establishes.

Hereupon: the user adjudicates the renames at §4 + §8, then a single restructure commit lands the moves + the README rewrite + the cross-corpus reference sweep.
