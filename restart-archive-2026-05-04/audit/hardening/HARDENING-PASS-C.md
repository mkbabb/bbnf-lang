# Hardening Pass — PASS-C (Periphery + Tooling + Docs + Commit Chain)

Date: 2026-05-03. Hardening agent: first adversary against `restart/audit/passes/PASS-C.md`.

The Pass-C synthesis covers ~1/3 of the project: analysis, lsp, archived crates (ser, gorgeous), docs (excluding the precepts submodule), audit corpora, scripts, server, extension, playground, wasm, workspace top-level files, sibling repos, **and the 2,628-commit chain itself**. The commit-chain decision is the suite's most consequential governance output; the docs re-do is the largest operational artefact. Both flow from this pass.

This audit applies the nine HARDENING lanes (per `restart/prompts/HARDENING.md`) to the Pass-C corpus: the synthesis at `restart/audit/passes/PASS-C.md` (486 lines) and six per-agent reports under `restart/audit/per-agent/pass-c-agent-{1..6}-*.md` (1,970 lines aggregate). The hardening reads the locks, the precepts, the corpora, and the master-plan amendment as ground truth; it ratifies what survives, surfaces what does not, and recommends surgery before the pass advances.

---

## §1 — Target identification

| Item | Value |
|---|---|
| Target | PASS-C synthesis + 6 per-agent reports |
| Synthesis path | `restart/audit/passes/PASS-C.md` |
| Synthesis lines | 486 |
| Per-agent paths | `restart/audit/per-agent/pass-c-agent-{1..6}-*.md` |
| Per-agent lines | 397 + 210 + 354 + 418 + 390 + 201 = 1,970 |
| Total lines audited | 2,456 |
| Synthesis commit | (Pass-C synthesis lands as part of the restart suite; in-flight) |
| Audit commit hash at HEAD | `9dde66ab` (`chore(tranches): archive pre-restart BA/BB/BC; clean slate for re-draft`) |
| Time budget | 45 minutes; minute 41 commit |

The Pass-C synthesis sits within `restart/audit/passes/`, which is the suite-internal layout; the synthesis's own `audit/restart/...` paths are misplaced (audited under §3 Cohesion).

---

## §2 — Cohort verdict

| Lane | Verdict | Faults | Recommendation |
|---|---|---:|---|
| 1 Lock-Adherence | partial | 4 | Fix Lock-12 archive directive; re-anchor Lock 14 verdict against Amendment 01; explicit Lock 13 sub-budget for analysis-merge target |
| 2 Sequencing | n/a (single-pass synthesis) | 0 | — |
| 3 Cohesion | partial | 7 | Re-anchor self-reference paths; reconcile commit-count drift; reconcile §1 verdict-bucket label collisions |
| 4 SOTA Anchoring | n/a (Pass-C contains no perf gates) | 0 | — |
| 5 Grammar-Authoritative | partial | 3 | Reconcile bbnf-language-server verdict against Amendment 01's "zero per-grammar crates"; surgery to harden directives/hints.rs; ratify analysis treatment |
| 6 Generated-Code Budget | violated | 4 | Add per-doc rewrite-line-budget table for waves 2-3; bound SPEC.md ≤ 1,500; bound architecture.md ≤ 800; per-tranche stub LOC ceiling |
| 7 Friction Forecast | partial | 3 | Verbatim error messages for archive-cutover; cookbook entry for path-crate triplet rename; LSP-endpoint-coupling friction |
| 8 Carry & Deferral | partial | 4 | Receiver/blocker/gate triple for `bbnf-cli` defer + `bbnf-py` defer + parse-that disposition + extension/server/ stale check |
| 9 Greenfield Discipline | honoured-mostly | 2 | Sharper "no quick solutions" stance on per-doc rewrite vs full rewrite; remove dual-disposition language ("rename OR merge") in §1 |

**Final decision: requires amendments before the pass advances to the synthesizer.**

The faults are surgical, not structural. The commit-chain disposition (Option 3 + branch reset) survives lane 9 cleanly. The Lock 12 archive ceremony's "blocking precondition" status survives lane 1. The docs re-do six-wave plan survives lane 9 and lane 6 with budget addenda. The largest faults are: (a) the `bbnf-language-server` consolidation collides with Amendment 01's "zero per-grammar crates" stance unless explicitly framed as cross-grammar generic; (b) the path-cohesion gap between the synthesis's `audit/restart/...` self-references and the actual `restart/audit/...` layout; (c) absent rewrite-LOC budgets on the docs re-do waves.

The pass is ratifiable after a punch list is applied (see §12).

---

## §3 — Lane 1: Lock-Adherence

For each of the 14 locks, walked the Pass-C synthesis + per-agent corpus. Per-lock verdict: **honoured / violated-with-recommendation / silent (must add)**.

### Lock 1 — Tape and columnar variants are fully dead

PASS-C.md:69 ratifies Lock 1's "violated in user-facing docs" verdict and prescribes the sweep `rg -ni 'TapeRec|TapeBuilder|TapeCursor|tape-?first|columnar' docs/{bbnf,performance,parse-that,pprint,gorgeous,cookbook,optimizer,migration}/`. The directive is correct; the sweep is mechanical; the surgery is bounded.

`pass-c-agent-3-lock-adherence.md:11-25` walks tape-residue surfaces in Pass-C scope; archived crates (`ser`, `gorgeous`) are honoured-by-archive — when Lock 12 lands, their tape vocabulary moves to `archive/` as provenance.

**Verdict: honoured.** No tape residue resurfaces in any Pass-C proposal. The sweep is gated; the archive ceremony is gated; the docs re-do absorbs the user-facing scrub.

### Lock 2 — Layout lowering is the canonical IR pass name

PASS-C.md:70 prescribes `rg -wn 'TypeDesc|StructLayout|TypeMap|type-projection|type-collapsing|schema synthesis|LayoutDesc' docs/`. Sweep is correct.

`pass-c-agent-3-lock-adherence.md:33-53` enumerates surfaces likely to carry retired terms.

**Verdict: honoured.** The Pass-C synthesis does not propose any new surface using retired vocabulary. The pre-existing user-facing docs that violate are slated for rewrite under Wave 2.

### Lock 3 — Cursor-parse + byte-skip unified, cursor branch elided when path empty

PASS-C.md:71 marks Lock 3 silent-must-add with recommendation "confirm `docs/cookbook/path-macro.md` states empty-path elision invariant". Surgery is bounded.

**Verdict: silent-must-add → honoured-with-surgery.** The recommendation is correct.

**Fault 1.A.** PASS-C.md:71 says "confirm" — that's a verification step, not a deliverable. The pass should commit to: *if* the cookbook entry does not state the invariant, add a verbatim invariant-statement paragraph during Wave 2 of the docs re-do.

| Site | path:line | Surgery |
|---|---|---|
| `restart/audit/passes/PASS-C.md:71` | "confirm `docs/cookbook/path-macro.md` states empty-path elision invariant" | Replace with: "Wave 2 of docs re-do verifies `docs/cookbook/path-macro.md` states the empty-path elision invariant; if absent, adds a paragraph stating: *`pointer![]` (the empty path) elides the cursor consultation entirely; the eager fast path pays no consult cost.*" |

### Lock 4 — Per-domain orthogonal optimization

PASS-C.md:72 marks honoured; no fault. `pass-c-agent-3-lock-adherence.md:74-86` confirms: Pass-C scope does not fuse CSP + e-graph.

**Verdict: honoured.**

### Lock 5 — IR + per-backend lower

PASS-C.md:73 marks silent-must-add; recommendation: docs/cookbook/, optimizer/, migration/ should reference IR-as-contract. Surgery is bounded.

**Verdict: silent-must-add → honoured-with-surgery.**

### Lock 6 — xtask emits committed source artefacts

`pass-c-agent-3-lock-adherence.md:108-114` confirms `xtask/src/{main,lib,regen}.rs` is the regen entry; no proc-macro for codegen output.

**Verdict: honoured.**

### Lock 7 — `crates/path/` is the consolidated path crate

PASS-C.md:75 prescribes "sweep `bbnf-path` references in docs; replace with `crates/path/`, `crates/path-core/`, `crates/path-ts/`". Surgery is correct per Lock 7's verbatim text.

**Verdict: honoured.**

### Lock 8 — Surpass sonic-rs / simdjson / lightning-css; AU silent

PASS-C.md:76 prescribes "full rewrite of `docs/perf/*`; replace AU references with sonic-rs / lightning-css / simdjson". This is the correct Lock-8 redress.

**Fault 1.B.** PASS-C.md:33 says `docs/performance/` "KEEP-MODIFY ... relocate to `docs/perf/`; full rewrite (Lock 8 — AU silent; Lock 1 — tape silent; Lock 2 — Layout canon)". The Lock 8 directive is sound, but the verdict bucket "KEEP-MODIFY" is misleading: a full rewrite is closer to ABROGATE-REPLACE than KEEP-MODIFY. The bucket conflates "preserve filename" with "preserve content".

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:33` | `docs/performance/` row says KEEP-MODIFY but prescribes "full rewrite" | bucket-collision | Change to ABROGATE-REPLACE; surgery column states "delete content; new file at `docs/perf/<topic>.md` per restart vocabulary" |

### Lock 9 — Slice-borrow primary; bumpalo + owned escape hatches

PASS-C.md:77 marks silent-must-add; recommendation: confirm `docs/cookbook/lifetime-surfaces.md` reflects three-way split.

**Verdict: silent-must-add → honoured-with-surgery.**

### Lock 10 — Pratt + SIMD auto-detected; no `@pratt` / `@simd` directives

PASS-C.md:78 prescribes audit of `crates/analysis/src/directives/hints.rs`; ZERO `@pratt`/`@simd` entries. Surgery is correct.

**Fault 1.C.** PASS-C.md:78 calls this "silent-must-add"; given that the hints catalogue is grammar-author-facing, the audit should be a hard gate of the bbnf-language-server consolidation — *not* a defer-to-later-wave action. If `directives/hints.rs` carries `@pratt` or `@simd` entries, the consolidation commit must strip them in the same commit.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:78` | "silent-must-add"; defers verification | Lock 10 verification deferred without receiver-gate triple | Promote to Pre-restart-blocking #3a: when consolidating analysis + lsp into bbnf-language-server, verify `directives/hints.rs` carries zero `@pratt`/`@simd` entries; if any exist, delete in the consolidation commit |

### Lock 11 — Path-deps for incubating sister crates

PASS-C.md:79 marks honoured-mostly with confirmation step for parse-that disposition.

**Verdict: honoured-with-surgery.** The "confirm parse-that disposition" is operational; the punch list at §8 should commit to: *parse-that stays external until the TS backend lands; workspace path-dep at the same time as `crates/path-ts/` activates*.

### Lock 12 — ser + gorgeous archive BEFORE BA.W0

PASS-C.md:80 marks **violated-with-blocking-rec**; surgery is the verbatim Lock 12 archive ceremony. PASS-C.md:97-108 (§3.2) and PASS-C.md:375-378 (§8.1, item 1) provide concrete `git mv` sequence.

`pass-c-agent-1-inventory.md:53-72` confirms: `Cargo.toml` line 2 still lists `crates/ser` and `crates/gorgeous`; `archive/` directory does not exist. Verified: `ls /Users/mkbabb/Programming/bbnf-lang/archive` returns "No such file or directory"; `Cargo.toml` line 2 reads `members = ["crates/core", "crates/analysis", "crates/ir", "crates/lsp", "crates/ser", "crates/gorgeous", ...]`.

**Verdict: violated-with-blocking-rec → ratified.** The hardening confirms the diagnosis. The surgery as PASS-C.md:101-105 prescribes is correct.

**Fault 1.D — minor.** PASS-C.md:101-105 prescribes `git mv crates/ser archive/ser` directly. `git mv` requires the parent directory; if `archive/` does not exist, the first `git mv` will create it. But the synthesis should explicitly state: *the first `git mv` materialises the `archive/` directory; no separate `mkdir archive` is needed*. Otherwise an executor could `mkdir -p archive && git mv ...` redundantly.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:101-105` | omits "archive/ materialises on first git mv" note | minor friction | Add comment: `# First git mv creates archive/; no separate mkdir needed.` |

### Lock 13 — No god directories

PASS-C.md:81 prescribes restructure for `docs/`, `docs/tranches/`, `scripts/`, `audit/`. PASS-C.md:113-127 (§3.3) provides the docs/ target shape (5 immediate children + GESTALT + README).

`pass-c-agent-3-lock-adherence.md:230-265` enumerates the four god-directory faults and the surgery for each.

**Verdict: honoured-with-surgery.** The proposed shapes for `docs/` (`lang/perf/howto/process/audit/spec`), `scripts/` (`profile/test/orchestrate/deploy/hooks/`), `audit/` (3 wave subdirs), `docs/tranches/` (archive + A-J + meta-audit) all sit within Lock 13's 4-10-children rule.

**Fault 1.E.** The proposed `docs/process/` carries 4 immediate children (`precepts, restart, instructions, tranches`) — within Lock 13's 4-10 rule, but at the floor. If the suite later adds another process subdir (e.g., for migration cookbooks or deployment runbooks), `process/` becomes a god directory by attrition. Add explicit: *`docs/process/` capped at 6 children; future process artefacts merge into one of the existing four or open a sibling top-level dir*.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:115-126` | `docs/` target shape silent on Lock 13 child-count ceiling | Lock 13 child-budget unstated for `docs/process/` and `docs/howto/` | Add note: "`docs/process/` and `docs/howto/` capped at 6 children each; growth past 6 opens a sibling top-level dir or merges concerns" |

### Lock 14 — Full grammar generalisation; zero overfitting

PASS-C.md:82 marks Lock 14 **violated for `crates/analysis/`**; surgery: rename to `crates/bbnf-analysis/` OR merge into `crates/bbnf-language-server/`. The OR-disposition is the largest hardening concern.

`pass-c-agent-3-lock-adherence.md:281-331` walks Lock 14: the verbatim list ("Generic crates — `bbnf-parse`, `bbnf-codegen`, ..., `analysis`, `lsp` — carry ZERO `match grammar { ... }` arms") includes `analysis` and `lsp` as generic crates. Per `crates/analysis/src/features/formatting.rs:6-8` (CENSUS §2.3), the import `use bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView}` is a Lock 14 violation if treated as generic.

**Critical conflict** with Amendment 01 (`restart/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md`): Amendment 01 rules **zero per-grammar crates** in the post-restart workspace. Pass C's "rename to `crates/bbnf-analysis/` (per-grammar declaration crate)" path proposes exactly the kind of per-grammar Rust crate Amendment 01 abrogates.

The merge-into-`bbnf-language-server/` path is consistent with Amendment 01 IF the consolidated server is *grammar-agnostic* (i.e., dispatches per-grammar analysis through workspace metadata). Per Amendment 01:7, future grammars onboard via "grammar source file + workspace metadata block"; an LSP that dispatches by grammar metadata (not match-on-name) honours both Lock 14 and Amendment 01.

The Pass-C synthesis names the consolidated server "single per-grammar crate" at PASS-C.md:92 — language that contradicts Amendment 01's "zero per-grammar crates". The wording must be reconciled.

**Fault 1.F.** Reconciliation: `crates/bbnf-language-server/` must be **generic + metadata-dispatched**, not a per-grammar declaration crate.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:92` | "merge analysis + lsp + dap into single per-grammar crate `crates/bbnf-language-server/`. Honours Lock 13 + Lock 14." | Per-grammar framing collides with Amendment 01 | Replace with: "merge analysis + lsp + dap into `crates/bbnf-language-server/`, **generic + metadata-dispatched** per Amendment 01. Per-grammar analysis features dispatch through workspace metadata, not match-on-name. Honours Lock 13, Lock 14, and Amendment 01." |
| `restart/audit/passes/PASS-C.md:23-24` | "rename to `crates/bbnf-analysis/` OR merge into..." | OR-disposition leaves the per-grammar crate path open | Strike the rename option; commit to merge-into-bbnf-language-server only |
| `restart/audit/passes/PASS-C.md:82` | "rename to `crates/bbnf-analysis/` OR merge..." | same | same surgery |
| `pass-c-agent-3-lock-adherence.md:293, 297, 301` | "either (a) `crates/analysis/` is renamed `crates/bbnf-analysis/`" | per-grammar rename path persists | Strike rename path; the agent report supersedes per Amendment 01 |
| `pass-c-agent-4-architectural-transposition.md:22-37` | Option A (rename + co-locate) and Option C (multi-grammar future) name per-grammar crates | Per-grammar crates retracted by Amendment 01 | Mark Option A, Option C as superseded; Option B (merge) is the only honoured path |

**Lane 1 Verdict: partial.**

| Lock | Verdict |
|---|---|
| 1 | honoured |
| 2 | honoured |
| 3 | silent-must-add → honoured-with-surgery |
| 4 | honoured |
| 5 | silent-must-add → honoured-with-surgery |
| 6 | honoured |
| 7 | honoured |
| 8 | honoured (with bucket-label fix) |
| 9 | silent-must-add → honoured-with-surgery |
| 10 | silent-must-add (must promote to blocking gate) |
| 11 | honoured |
| 12 | violated-with-blocking-rec → ratified |
| 13 | honoured-with-surgery |
| 14 | violated-with-rec — requires Amendment-01 reconciliation |

Faults: 1.A, 1.B, 1.C, 1.D, 1.E, 1.F.

---

## §4 — Lane 2: Sequencing Discipline

**Verdict: n/a.** The HARDENING prompt (§Lanes, Lane 2) gates this lane to multi-wave-plan targets. Pass C is a single-pass synthesis. The Pass-C synthesis enumerates the docs re-do as 6 waves (PASS-C.md:204-211), but those waves consume themselves; no per-wave handoff to a subsequent tranche is named *within Pass C alone* (the consuming tranche is the master plan's tranche A, out of Pass-C scope).

Lane 2 N/A.

---

## §5 — Lane 3: Cohesion

Every claim in the target must be verifiable from artefacts the target produces or cites. Walked Pass-C for orphan claims and orphan deliverables.

### §5.1 — Self-reference path collisions

Pass C's per-agent paths are cited at PASS-C.md:10-15 as `audit/restart/per-agent/pass-c-agent-{1..6}-...md`. The actual layout is `restart/audit/per-agent/pass-c-agent-{1..6}-...md` (note: `restart/audit/`, not `audit/restart/`). This collides with the working-tree layout per `find /Users/mkbabb/Programming/bbnf-lang/restart -maxdepth 3 -type d`.

The HARDENING.md prompt (`restart/prompts/HARDENING.md:112`) specifies output at `audit/restart/HARDENING-{TARGET}-2026-MM-DD.md` — but the actual layout puts the hardening at `restart/audit/hardening/HARDENING-PASS-C.md`. The directive layout and the actual layout diverged. Pass C inherited the divergence.

**Fault 3.A.** Pass C's self-references should track the actual layout (`restart/audit/...`), not the directive layout (`audit/restart/...`).

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:10-15` | "audit/restart/per-agent/pass-c-agent-1-inventory.md" etc. | path-references stale (actual: `restart/audit/per-agent/...`) | Replace `audit/restart/...` with `restart/audit/...` throughout the synthesis |
| `restart/audit/passes/PASS-C.md:97, 110, 122, 139, 154, 179, 183, 187, 197` | Same throughout per-agent output paths | same | same |
| `restart/audit/passes/PASS-C.md:418-426` | `audit/HARDENING-2026-05-03-...md` cited inside the §8.1 audit/ restructure surgery | `audit/` corpora live at workspace-root `audit/`; this is correct, but `audit/restart` then cited at line 426 mismatches | Disambiguate: workspace-root `audit/` (the existing corpus dir) vs. the suite's `restart/audit/` (this suite's own outputs); the §8.1 restructure must clarify which |

### §5.2 — Commit count drift

PASS-C.md:262-264 cites `git log --oneline | wc -l → 2621` and `git log origin/master..HEAD | wc -l → 1724`. PASS-C.md:267 says "the current state is 2,621". Verified at hardening time: actual `git log --oneline | wc -l` returns 2,628; `git log origin/master..HEAD | wc -l` returns 1,731. The drift is 7 commits (Pass C and earlier passes have continued landing during the suite execution).

**Fault 3.B.** The drift is small but the hardening should call it out: by the time the master plan executes, the count will have drifted further. The decision matrix and the recommendation (Option 3) is robust to small drift, but the synthesis should declare: *the commit count is approximate; provenance preservation does not depend on the exact count*.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:262-267` | "2,621 commits" cited as pinned number | drift not acknowledged | Add note after line 267: "Counts are point-in-time per the synthesis date; the recommendation (Option 3) does not depend on exact counts; subsequent commits between synthesis and cutover continue to anchor on the same provenance tag." |

### §5.3 — `crates/parse-that/` workspace status orphan

PASS-C.md:79 says "Lock 11 — honoured-mostly | confirm parse-that disposition". `pass-c-agent-1-inventory.md:282` cites `parse-that` as external repo with no workspace-internal coupling at present.

Verified at hardening: `Cargo.toml` line 2 lists 14 members; `parse-that` is NOT among them. The pass-c-agent-1-inventory.md claim is correct: parse-that is external.

But Pass-C.md:60 says `Sibling repos (parse-that, csc411, bbnf-buddy, gorgeous-external, pprint-external) | KEEP | external; brand and architectural separation`. The KEEP verdict is correct for parse-that-as-external. But pass-c-agent-3-lock-adherence.md:200 says `parse-that not visible — likely external repo at this point`. This is "likely" — a hedge. The hardening should make this verdict definite.

**Fault 3.C.** Pass C should commit to: parse-that is external; remains external; promotes to workspace path-dep when the TS backend (planned) consumes it.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `pass-c-agent-3-lock-adherence.md:200` | "likely external repo at this point" | hedged verdict | Replace with: "parse-that is external (verified absent from `Cargo.toml` `[workspace] members`); remains external until the TS backend lands and consumes it as a path-dep" |
| `restart/audit/passes/PASS-C.md:79` | "confirm parse-that disposition" | confirm-step orphan | Replace with: "ratified: parse-that external until TS backend lands" |

### §5.4 — `package.json` (workspace top-level) orphan

PASS-C.md:51 marks workspace-top-level `package.json` / `package-lock.json` / `node_modules/` as `ABROGATE-DELETE if no consumer | per Agent 4 §6.6; verify before delete`. This is a conditional ABROGATE; the verification step is orphan (no named verifier).

**Fault 3.D.** Pass C should commit to a definite verification path. The verification: `rg 'workspaces|scripts' package.json` — if scripts present, keep + .gitignore; otherwise delete.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:51` | "verify before delete" | verification orphan | Add: "Verification: read top-level `package.json` for `workspaces` field or non-trivial `scripts`. If neither, DELETE; if either, KEEP + .gitignore `node_modules/`" |

### §5.5 — `extension/server/` orphan

PASS-C.md:55 marks `extension/` as KEEP-MODIFY with surgery "verify `extension/server/` not stale". This is verification-orphan; no named verifier.

`pass-c-agent-1-inventory.md:212` lists `extension/server/` as "Stub for LSP server".

**Fault 3.E.** Pass C should commit to: read `extension/server/` once; if it's a stub for `server/bbnf-lsp` (the committed binary slated for deletion), delete in the same commit; if it's a runtime artefact from extension build, .gitignore.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:55` | "verify `extension/server/` not stale" | orphan verifier | Replace with: "Read `extension/server/` once during pre-restart; if it's a stub for the `server/bbnf-lsp` committed binary (which is being deleted), delete in the same commit; if it's a runtime artefact from extension build, .gitignore `extension/server/`" |

### §5.6 — Wave 5 + Tranche letter set

PASS-C.md:209 says "Wave 5 — Tranche archive relocation | `git mv` letter-tranches under `archive/legacy-Y-BD/` | ~30 minutes". PASS-C.md:410 then specifies the verbatim letter list: `{Y,Z,AA,AB,AC,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY-I,AY-II-I,AY-III,AZ-I,AZ-II,AZ-III,AZ-IV,B0,B1,B2,B3,B4,B5,B6,B7,BA,BB,BC,BD,W,X}`.

Verified at hardening: `find docs/tranches -maxdepth 1 -type d | sort` returns 49 entries: AA, AB, AC, AE, AF, AG, AH, AI, AJ, AK, AL, AM, AN, AO, AP, AQ, AR, AS, AT, AU, AV, AW, AX, AY-I, AY-II-I, AY-III, AZ-I, AZ-II, AZ-III, AZ-IV, B0, B1, B2, B3, B4, B5, B6, B7, BA, BB, BC, BD, W, X, Y, Z, archive, meta-audit. The letter list at PASS-C.md:410 matches the working tree (modulo `archive` and `meta-audit` which are correctly preserved).

**Fault 3.F — minor.** PASS-C.md:410 letter-list has the spelling `AY-II-I` for the AY-II-I tranche; verified. The list is correct. But the synthesis should note: AD is NOT in the alphabet (gap between AC and AE). A reader scanning the letters might wonder if AD was missed.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:410` | letter list | AD-gap silent | Add footnote: "AD does not exist in the legacy tranche set; the letter was skipped; the list above is exhaustive" |

### §5.7 — `audit/` workspace-root vs `audit/codebase-2026-05-03/` etc.

PASS-C.md:135 says "`audit/` restructure into `audit/{codebase-2026-05-03, plan-2026-05-03, restart-2026-05-03}/`". PASS-C.md:418-426 specifies the verbatim `git mv` sequence. Verified: workspace-root `audit/` exists with 22 files (per `pass-c-agent-1-inventory.md:307-313`); the corresponding restructure target is plausible.

But PASS-C.md:43 says `audit/restart/` | KEEP | this suite; preserve. And PASS-C.md:135 says restructure into 3 wave-subdirs INCLUDING `restart-2026-05-03`. The two are in tension: PASS-C.md:43 says preserve `audit/restart/`, but the actual location (verified) is `restart/audit/...` — i.e., the suite's outputs sit in a subtree of `restart/`, not under workspace-root `audit/`.

**Fault 3.G.** PASS-C.md:43 conflates `audit/restart/` (the directive's prescribed path) with `restart/audit/...` (the actual layout). The pass needs to disambiguate.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:43` | `audit/restart/` cited as KEEP path | Path mismatches actual layout | Replace with: "`restart/audit/` (this suite's actual location) | KEEP | preserve as restart-suite working tree" |
| `restart/audit/passes/PASS-C.md:135` | `audit/restart-2026-05-03` cited as restructure target | Confuses workspace-root audit/ with restart/audit/ | Reconcile: workspace-root `audit/` restructures into 3 wave subdirs; the `restart/audit/...` suite tree is a separate concern; rename the third bucket to `restart-suite/` or relocate the suite tree under workspace-root `audit/restart-2026-05-03/` (operationally cheaper) |

**Lane 3 Verdict: partial.** Faults: 3.A through 3.G.

---

## §6 — Lane 4: SOTA Anchoring

Every parse-throughput gate cites a competitor + dataset + platform. Per HARDENING.md:60, "non-throughput engineering gates must NOT claim Lock 8 honour".

Walked Pass-C synthesis + per-agent reports for parse-throughput gates: **Pass C contains zero parse-throughput gates**. Pass-C scope (analysis, lsp, archived crates, docs, audit, scripts, tools, server, extension, playground, wasm, sibling repos, commit chain) is all non-throughput-relevant. The single perf-related claim (PASS-C.md:33: "`docs/performance/` ... Lock 8 — AU silent") is a directive to delete AU references in the docs re-do — *not* a perf gate.

Pass C does not claim Lock 8 honour for any of its surgeries; correct under HARDENING §SOTA-erasure.

**Lane 4 Verdict: n/a.** Lane is N/A by scope.

---

## §7 — Lane 5: Grammar-Authoritative Discipline (Lock 14 deep dive)

Verbatim from HARDENING.md:67: "Per-X tables for every 'all grammars' / 'every grammar' / 'all backends' claim; Future-grammar onboarding test (a hypothetical 10th grammar `yaml.bbnf` adds via 3 declarative surfaces only); Per-grammar code lives ONLY in per-grammar declaration crates (`crates/<grammar>/`) or workspace metadata".

But Amendment 01 supersedes the per-grammar declaration crates: "Zero per-grammar crates in the post-restart workspace. The greenfield is fully grammar-driven and fully agnostic. Every grammar plugs into the fleet via two declarative surfaces: (1) Grammar source file ... (2) Workspace metadata block ..." (Amendment 01:11-16). Per Amendment 01:158, "tranche-drafting agents read both documents" and "where the master plan and the amendment disagree, the amendment wins".

Therefore for Lane 5, the future-grammar onboarding test is **2 surfaces, not 3**: (a) grammar source, (b) workspace metadata. No per-grammar declaration crate.

### §7.1 — `match grammar { Json => ..., CssL4 => ..., ... }` scan

Ran `grep -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>'` over Pass-C synthesis + 6 per-agent reports. Result: **zero matches**. Pass-C's text contains zero proposed match-on-grammar arms. Honoured.

### §7.2 — Grammar-name-mention scan

Ran `grep -nE 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|math'` over PASS-C.md. Hits classified:

| Hit | path:line | Classification |
|---|---|---|
| `crates/analysis/` row mentions `bbnf-analysis` | PASS-C.md:23 | per-grammar-rename path — must retract per Amendment 01 (covered under Fault 1.F) |
| `crates/lsp/` row mentions `bbnf-lsp` | PASS-C.md:24 | same |
| `docs/bbnf/` cited | PASS-C.md:32 | per-language doc directory; legitimate (bbnf-the-grammar has its own docs; that is a per-grammar artefact, not per-grammar code) |
| `docs/lang/{bbnf, parse-that, pprint, gorgeous}` | PASS-C.md:32, 118 | same; per-language docs colocated under `docs/lang/` |
| `Cargo.toml ... [workspace.metadata.bbnf]` | PASS-C.md:44 | workspace-metadata reference; honours Amendment 01 surface (1) |
| Sibling repos list | PASS-C.md:60 | external repo names |
| `bbnf-language-server` | PASS-C.md:23, 24, 90, 92, 304-308, 386-389, 482 | the consolidated server name; framing must be generic + metadata-dispatched (Fault 1.F) |
| `bbnf-test-fixtures` | PASS-C.md:58, 187 | per Amendment 01:64-69, this crate carries fixture *files* organised by grammar; no per-grammar Rust |
| `bbnf-cli` | PASS-C.md:191 | future user-facing CLI; generic + metadata-dispatched |
| `bbnf-py` | PASS-C.md:195 | future Python bindings; generic |

**Verdict: honoured-mostly.** The grammar-name mentions are all (a) per-language doc directory names (legitimate), (b) workspace-metadata-block references (the Amendment 01 surface), or (c) generic-crate names beginning with `bbnf-` (the project namespace, not per-grammar). The single fault (analysis/lsp framed as "per-grammar") is captured at Fault 1.F.

### §7.3 — Future-grammar onboarding test under Amendment 01

A hypothetical 10th grammar `yaml.bbnf` (per Amendment 01:18-23):
- Drop `grammar/yaml/yaml.bbnf` into the source tree
- Add `[workspace.metadata.bbnf.grammars.yaml]` block to `Cargo.toml`
- Run `cargo xtask regen`

Pass-C-scope verification:
- `crates/bbnf-language-server/` (post-consolidation): does adding YAML require a code change? **Per Amendment 01: NO**. The LSP must dispatch by metadata. If the formatting feature reads a per-grammar table from workspace metadata, YAML's row plugs in; if it imports `BbnfCompoundKind` directly, it FAILS the test.

This sharpens Fault 1.F: the consolidated server's *internal architecture* must be metadata-dispatched. Pass C's surgery does not yet specify this; the hardening must add the requirement.

**Fault 5.A.** The bbnf-language-server consolidation must explicitly frame as "metadata-dispatched LSP server"; the hardening commits to: when `crates/{analysis, lsp}` merge into `crates/bbnf-language-server/`, every per-grammar import (`use bbnf::runtime::bbnf::*`, `use bbnf::runtime::json::*`, etc.) must be replaced by a metadata-driven dispatch surface that reads `[workspace.metadata.bbnf.grammars]`.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:90-94` | §3.1 | Internal architecture not specified | Add: "Internal architecture: bbnf-language-server dispatches per-grammar features (formatting, hints, selection-range) through a metadata-driven surface that reads `[workspace.metadata.bbnf.grammars]`. Every per-grammar import (`use bbnf::runtime::<g>::*`) is replaced by a generic dispatch trait that lifts from metadata" |

### §7.4 — `directives/hints.rs` audit

Per `pass-c-agent-3-lock-adherence.md:190-192`, `crates/analysis/src/directives/hints.rs` "must NOT include `@pratt`/`@simd` per Lock 10". This is captured under Fault 1.C.

Per Fault 5.A, the consolidation must also strip any per-grammar-name match in `directives/hints.rs`. The hardening promotes both audits to blocking gates of the consolidation commit.

**Fault 5.B.** When the consolidation commit lands (per PASS-C.md:386-389), `directives/hints.rs` undergoes two audits in the same commit: (a) zero `@pratt`/`@simd` entries (Lock 10), (b) zero per-grammar-name match arms (Lock 14 + Amendment 01).

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:386-389` | consolidation commit | Lock 10 + Lock 14 audits absent | Add gate: "Pre-commit verification: (a) `rg '@pratt|@simd' crates/bbnf-language-server/src/` returns zero; (b) `rg -nE 'match\s+\w+\s*\{[^}]*\"json\"\s*=>|\"css_l4\"\s*=>|\"bbnf\"\s*=>|\"google_sheets\"\s*=>' crates/bbnf-language-server/src/` returns zero" |

### §7.5 — Lock 14 in user-facing docs

Per Lock 14, every "all grammars" / "every grammar" claim in proposed crates must enumerate the grammars in a per-X table. Pass C does this for the docs re-do (PASS-C.md:204-211, 6-wave plan addresses every doc). Honoured.

### §7.6 — Per-grammar fixture surface

Pass C proposes `crates/bbnf-test-fixtures/` (PASS-C.md:187, replacement design Agent 5 §4). Per Amendment 01:64-67 + 86-91, fixtures live under `crates/bbnf-test-fixtures/<name>/` as fixture files (`.json`, `.css`, etc.), not Rust code. Pass C's wording ("per-grammar test fixtures" at PASS-C.md:189; 600 LOC source + ~5-10 MB fixtures at PASS-C.md:189) is consistent if interpreted as "the test harness is generic; fixtures are per-name file directories". The hardening should make this explicit.

**Fault 5.C.** Pass C's `crates/bbnf-test-fixtures/` description omits the Amendment 01 framing.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:187-189` | `crates/bbnf-test-fixtures/` | Per-grammar Rust risk | Add: "Per Amendment 01: this crate carries fixture *files* (`.json`, `.css`, `.bbnf`, etc.) organised under `<name>/` subdirectories. Rust code is generic — a single fixture-loader trait that reads metadata. No per-grammar Rust." |

**Lane 5 Verdict: partial.** Faults: 5.A, 5.B, 5.C.

---

## §8 — Lane 6: Generated-Code + LOC Budget

For every proposed crate / module / wave: is there a generated-LOC budget? An xtask regen-cycle budget? A per-grammar LOC delta projection?

Pass-C scope is mostly non-codegen (docs, scripts, archive, etc.). The relevant budgets:

### §8.1 — Replacement-design new crates

| Crate | LOC budget cited | Verdict |
|---|---|---|
| `crates/bbnf-test-fixtures/` | ~600 LOC + ~5-10 MB fixtures (PASS-C.md:189) | budget present |
| `crates/bbnf-cli/` | ~800-1200 LOC (PASS-C.md:193) | budget present |
| `crates/bbnf-py/` | not specified | budget silent (deferred per Amendment 01 timing; acceptable at defer-state) |

**Fault 6.A.** `bbnf-py/` budget silent; if it lands later, it should declare an LOC budget at landing time. Add carry note: *bbnf-py LOC budget declared at landing tranche*.

### §8.2 — Spec/architecture/migration docs

| Doc | Line budget cited | Verdict |
|---|---|---|
| `docs/spec/SPEC.md` | ~1,000-1,500 lines (PASS-C.md:175) | budget present |
| `docs/spec/architecture.md` | ~600-800 lines (PASS-C.md:181) | budget present |
| `docs/howto/migration/2026-restart.md` | ~500 lines (PASS-C.md:185) | budget present |

Honoured.

### §8.3 — Docs re-do waves 2 + 2b + 2c

| Wave | Budget cited | Verdict |
|---|---|---|
| Wave 1 (mechanical relocate) | ~4 hours (PASS-C.md:206) | time budget; LOC budget not relevant (mechanical) |
| Wave 2 (`docs/lang/*`) | "3-5 days" (PASS-C.md:206) | time budget but **no LOC delta budget** |
| Wave 2b (`docs/perf/*` rewrite) | "Lock 8 (AU silent; SOTA-only)" (PASS-C.md:457) | **no LOC budget** |
| Wave 2c (`docs/howto/*`) | "Mostly-honoured; minor metalanguage strip" (PASS-C.md:458) | **no LOC budget** |
| Wave 3 (new spec docs) | "3-5 days" (PASS-C.md:207) | time budget; LOC delegated to per-doc |
| Wave 4 (migration record) | "1 day" (PASS-C.md:208) | time budget; LOC delegated to per-doc |
| Wave 5 (tranche archive relocate) | "30 minutes" (PASS-C.md:209) | mechanical |
| Wave 6 (validation) | "1 day" (PASS-C.md:210) | gate-time budget; no LOC change |

**Fault 6.B.** Waves 2 + 2b + 2c carry no LOC delta budget. Per LESSONS-LEARNED 2026-04-30 ("Generated Code Has A Size Budget"), every artefact-changing wave declares an expected output line-count window. The docs re-do's substantive waves (2 + 2b) should declare: *target final line count per doc; rewrite shrinks each by ≥30% (banned-words sweep + AI-writing-sign cleanup) on average; aggregate target ≤ pre-rewrite total*.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:206-208, 456-459` | docs re-do waves 2-2c | LOC delta budgets silent | Add per-wave table: "Wave 2 LOC budget: every `docs/lang/*` file's rewrite targets ≤80% of pre-rewrite line count, ≥60%; aggregate target ≤ pre-rewrite total. Wave 2b: every `docs/perf/*` file's rewrite targets ≤70% (AU references purged + tape vocabulary stripped). Wave 2c: ≤95% (minor metalanguage strip)." |

### §8.4 — Per-tranche stub LOC

PASS-C.md:445-447, item 9: "Stub `docs/process/tranches/{A,B,C,D,E,F,G,H,I,J}/{A,B,...,J}.md` with placeholder thesis + waves placeholder". No LOC budget for stubs.

**Fault 6.C.** Per-tranche stub size budget silent. Per Amendment 01:122 + master plan, tranche stubs at restart-prelude time should be small (~50-100 lines each) — actual draft body lives in tranche execution. Declare: *each tranche stub at the restart-prelude commit ≤ 100 lines; full draft lands at tranche execution*.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:445-447` | tranche A-J stubs | Stub size budget silent | Add: "Each tranche stub at the restart-prelude commit ≤ 100 lines (placeholder thesis + waves placeholder); full draft lands at per-tranche execution time" |

### §8.5 — Operational-sequence script-LOC

PASS-C.md:312-348 lists the Operational Sequence (`bash` block ~36 lines). PASS-C.md:392-407 lists the §8.2 surgery (verbatim git mv sequence ~30 commands). These are operational scripts; no LOC budget needed (they're inline shell, not committed code).

Honoured.

### §8.6 — Generated-code-impact for archives

The Lock 12 archive ceremony moves `crates/ser/` and `crates/gorgeous/` from `crates/` to `archive/`. This is `git mv`, not regen. No generated-LOC impact.

The `crates/{analysis, lsp}` consolidation is also `git mv` + import-rewrite, not regen. No generated-LOC impact (the codegen output `crates/core/src/grammar/generated/*` is unaffected).

Honoured.

**Fault 6.D.** PASS-C.md does not explicitly state that the consolidation does NOT change `crates/core/src/grammar/generated/*`. Add: *the analysis/lsp consolidation is mechanical; generated-LOC delta zero*.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:386-389` | consolidation commit | Generated-LOC impact unstated | Add: "Generated-LOC delta: zero. The consolidation rewrites `use ... ::analysis::*` and `use ... ::lsp::*` imports in active crates; `crates/core/src/grammar/generated/*` unaffected" |

**Lane 6 Verdict: violated.** Faults: 6.A, 6.B, 6.C, 6.D.

---

## §9 — Lane 7: Friction Forecast

Per HARDENING.md:84-94: where will users / grammar authors hit the proposed API and not understand it?

Pass-C scope is mostly non-API (docs, archive, commit-chain). The friction surfaces:

### §9.1 — Archive cutover

The user (the restart executor) will run `git mv crates/ser archive/ser` followed by `cargo check --workspace`. If `Cargo.toml` still references `bbnf-ser` from another crate, `cargo check` fails. The user expects: clear error message; clear surgery.

**Fault 7.A.** PASS-C.md:101-105 omits the diagnostic for this case.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:101-105` | archive ceremony surgery | Diagnostic for cargo-check failure absent | Add verbatim error message + surgery: "If `cargo check --workspace` fails after `git mv`, `cargo` will report: `error: no matching package named 'bbnf-ser' found`. Surgery: `rg -l 'bbnf-ser|bbnf-gorgeous' crates/` enumerates remaining users; convert each to `dev-dependencies` or strip the dependency. Re-run `cargo check --workspace`." |

### §9.2 — Docs path-rename friction

The docs re-do moves `docs/cookbook/path-macro.md` to `docs/howto/cookbook/path-macro.md`. Cross-references inside other docs that say "see `docs/cookbook/path-macro.md`" break.

PASS-C.md:392-407 (§8.2 item 4) states "Update internal cross-references via grep + edit". The verification at line 406 is `find docs -name '*.md' | xargs rg -l '\(.*docs/' | xargs rg -L '\(.*docs/(lang|perf|howto|process|spec|audit)/' ` returns zero — checks that every `docs/`-prefixed link targets the new layout. The verification is correct in spirit but the regex is fragile (Markdown links may use bare paths without parens).

**Fault 7.B.** PASS-C.md:406 verification regex is brittle.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:406` | verification regex | brittle (matches only paren-wrapped links) | Replace with: "`rg -n 'docs/(bbnf|parse-that|pprint|gorgeous|performance|cookbook|optimizer|migration|instructions)/' docs/` returns zero outside `docs/process/tranches/archive/`. Tranche-archive references retain old paths (legitimate)." |

### §9.3 — Path-crate triplet rename friction

Per Lock 7 + PASS-C.md:75, `bbnf-path` references in docs replace with `crates/path/`, `crates/path-core/`, `crates/path-ts/`. Users following older docs will hit broken `bbnf-path` references.

**Fault 7.C.** PASS-C.md does not commit to a cookbook entry explaining the triplet rationale.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:75` | path-crate triplet sweep | No cookbook entry on rationale | Add: "Cookbook addendum (lands in Wave 2c): `docs/howto/cookbook/path-crates.md` — explains `crates/path/` (Rust proc-macro shell), `crates/path-core/` (non-proc-macro shared logic), `crates/path-ts/` (TS cdylib shell); the triplet exists because Rust toolchain forbids proc-macro path-dep sharing." |

### §9.4 — Cargo.toml `[members]` rewrite friction

PASS-C.md:382-388 prescribes the consolidation: rename + import-rewrite. Users with in-flight branches against `crates/analysis/` or `crates/lsp/` paths break.

**Fault 7.D.** Pass C does not commit to a `git log` reference for finding the rename commit (the user's `git log --diff-filter=R --name-only` hint).

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:386-389` | consolidation commit | No "how to find the rename" guidance | Add: "Post-consolidation, users with in-flight branches against `crates/analysis/` rebase onto the new HEAD; the rename commit is findable via `git log --diff-filter=R --name-only -- crates/analysis crates/lsp crates/bbnf-language-server`." |

### §9.5 — `cargo xtask regen` after metadata-driven LSP

Per Fault 5.A, the consolidated bbnf-language-server reads `[workspace.metadata.bbnf.grammars]` for per-grammar dispatch. When a grammar author changes a metadata block, the LSP behaviour changes — but `cargo xtask regen` doesn't necessarily re-build the LSP.

This is a future-grammar friction (Lock 14 onboarding test):
- Author drops `yaml.bbnf` and adds metadata block
- Runs `cargo xtask regen` (per Amendment 01:25)
- Restarts LSP — does the new grammar appear?

If the LSP caches the metadata at startup, the user must restart. If the LSP reads metadata on every dispatch, the user gets immediate feedback but pays per-dispatch cost.

**Fault 7.E.** Pass C is silent on this. Worth a friction note for the master plan.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:90-94` | bbnf-language-server | Metadata-cache friction silent | Add: "Friction note: the LSP server reads `[workspace.metadata.bbnf.grammars]` once at startup; new grammars require LSP restart. Master plan tranche I (LSP consolidation) commits to: documented restart procedure in `docs/howto/cookbook/lsp-restart.md`; future enhancement: filesystem watch on `Cargo.toml` for hot-reload" |

### §9.6 — `bbnf-cli` defer surface

Per PASS-C.md:191-193, `crates/bbnf-cli/` defers to 1.0. Users today reach for `cargo xtask regen`. The xtask is workspace-internal — outside-the-workspace users have no entry point until 1.0.

**Fault 7.F.** Pass C is silent on the user-facing experience for outside-the-workspace consumers in the 0.x window.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:191-193` | bbnf-cli defer | Outside-the-workspace user friction silent | Add: "0.x window (pre-bbnf-cli): outside-the-workspace users have two entry points: (1) clone the workspace, run `cargo xtask regen`; (2) install via `cargo install --path crates/xtask` and run `cargo-xtask regen` from outside. Document both in `docs/howto/cookbook/xtask-as-cli.md`." |

**Lane 7 Verdict: partial.** Faults: 7.A through 7.F.

---

## §10 — Lane 8: Carry & Deferral Audit

Every "deferred to" / "future" / "TBD" / "user adjudicates" must name (a) receiver, (b) blocker, (c) receiving gate.

### §10.1 — `bbnf-cli` deferred to 1.0

PASS-C.md:191-193: "DEFER to 1.0 release. Until then, `cargo xtask regen` + LSP via extension cover the dev-loop and end-user-via-extension cases".

(a) Receiver: tranche... unstated (the master plan must locate). **Fault.**
(b) Blocker: stable user-facing API (implicit). **Fault — should be explicit.**
(c) Receiving gate: 1.0 release (operational, not a tranche gate). **Fault — should be a tranche gate.**

**Fault 8.A.** Defer-triple incomplete.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:191-193` | bbnf-cli defer | Receiver/blocker/gate triple incomplete | Replace with: "DEFER to tranche post-J (1.0-release tranche, not yet drafted in restart letter set). Blocker: stable user-facing API surface (lifetime variants stable, error type stable, IR contract stable). Receiving gate: when (parse_in/parse_owned/parse), Document::get<T>(path), pointer![…] all reach API freeze, the bbnf-cli surface absorbs them in a single tranche" |

### §10.2 — `bbnf-py` deferred post-1.0

PASS-C.md:195-197: "DEFER to post-1.0. No Python consumer materialised; speculative work."

(a) Receiver: post-1.0. **Fault — vague.**
(b) Blocker: "no Python consumer" — but this is a non-action; if no one needs it, it never lands. **Fault — should commit to a triggering condition.**
(c) Receiving gate: none. **Fault.**

**Fault 8.B.** Defer-triple incomplete; speculative-work framing is correct but the receiver-blocker-gate trio is missing.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:195-197` | bbnf-py defer | Triple absent | Replace with: "DEFER post-1.0. Triggering condition: a Python consumer (csc411 CSP-solver Python bindings consume bbnf grammar tooling, OR a downstream Python user surfaces). Receiver: a yet-to-name post-1.0 tranche. Receiving gate: that tranche's W0 declares the Python binding API shape; W1 implements PyO3 cdylib OR subprocess-CLI binding (decision per timing)." |

### §10.3 — `parse-that` external disposition

PASS-C.md:79: "honoured-mostly; confirm parse-that disposition". PASS-C.md:60: "external; brand and architectural separation".

(a) Receiver: tranche I (LSP + sister-crate publication, per master plan); but Pass C does not name it. **Fault.**
(b) Blocker: TS backend lands. **Implicit; should be explicit.**
(c) Receiving gate: tranche I's gate that adds parse-that as path-dep. **Should be cited.**

**Fault 8.C.** Defer-triple incomplete.

| Site | path:line | Fault | Surgery |
|---|---|---|---|
| `restart/audit/passes/PASS-C.md:79` | parse-that | Receiver/blocker/gate triple absent | Replace with: "Receiver: tranche I (LSP consolidation + sister-crate disposition). Blocker: TS backend (tranche H) needs parse-that combinators in production. Receiving gate: tranche I's W? consumes parse-that as workspace path-dep (registers `parse-that` in `[workspace] members`)" |

### §10.4 — `extension/server/` stale check deferred

PASS-C.md:55: "verify `extension/server/` not stale". This is verification-deferred; no receiver / blocker / gate.

This is captured under Fault 3.E; the surgery there ("Read `extension/server/` once during pre-restart...") plus the receiver-blocker-gate triple resolves it: receiver = pre-restart; blocker = none; gate = restart-prelude commit 6 (.gitignore + committed-artefact deletions).

**Fault 8.D.** Captured under 3.E; covered.

### §10.5 — Dual-disposition carries (rename OR merge)

PASS-C.md:23-24: "rename to `crates/bbnf-analysis/` OR merge into `crates/bbnf-language-server/` per Lock 14". This is a dual-disposition; the choice is not committed.

Per HARDENING.md:97 ("any without all three" is fault), a defer with two open options is a dual-disposition. The Pass-C synthesis should ratify ONE.

Captured under Fault 1.F. The hardening commits to: merge-only (per Amendment 01).

### §10.6 — `audit/` workspace-root vs `docs/audit/` decision

PASS-C.md:42-43: workspace-root `audit/` keeps + restructures. PASS-C.md:179-184 (Agent 4 §4) considers two options (A — keep at workspace root; B — move under `docs/audit/`); ratifies Option A.

The ratification is in Agent 4; the synthesis at PASS-C.md ratifies via §1's KEEP-MODIFY verdict at line 42. Check: PASS-C.md:42 says `audit/` (workspace root) | KEEP-MODIFY | restructure into `audit/{codebase-2026-05-03, plan-2026-05-03, restart-2026-05-03}/` per Lock 13. Honoured.

Honoured.

### §10.7 — `docs/precepts/` submodule disposition

PASS-C.md:28: `docs/precepts/` (submodule) | KEEP | unchanged; submodule pin. Receiver: none (it's a permanent decision). Blocker: none. Gate: none.

This is a settled disposition, not a defer. Honoured.

### §10.8 — Cutover decision

PASS-C.md:344-348 (in §7.5 operational sequence) and PASS-C.md:469-471 (in §8.5) defer cutover decision to USER:
- Option A: master-greenfield-2026-05-03 → master (force-push); pre-restart tag preserves prior chain
- Option B: keep both branches

(a) Receiver: USER. **Honoured per HARDENING-prompt; the user is the named receiver.**
(b) Blocker: hardening pass returns *ready*. **Honoured.**
(c) Receiving gate: post-hardening cutover. **Honoured.**

Honoured.

**Lane 8 Verdict: partial.** Faults: 8.A, 8.B, 8.C; 8.D covered by 3.E.

---

## §11 — Lane 9: Greenfield Discipline

Particular foci: no quick solutions; no workarounds; no legacy code survives uncontested; idiomatic gestalt approaches; architectural transpositions for elegance / simplicity / performance are mandatory.

### §11.1 — No quick solutions

Pass C's surgical agenda is bounded but substantive. The Lock 12 archive ceremony is straightforward `git mv` + `Cargo.toml` edit — "quick" in a mechanical sense, but it executes a rooted architectural commitment (Lock 12). This is *not* a quick solution; it is a long-overdue ceremony.

The docs re-do six waves are substantive (8-13 days). Not quick.

The bbnf-language-server consolidation is mechanical (git mv + import rewrite). The hardening promotes this to "metadata-dispatched" via Fault 5.A — *that* is the substantive surgery, not the rename.

The commit-chain disposition (Option 3 + branch reset) is the project's decision against quick solutions. Squash-all (Option 2) would be quickest; Option 3 honours provenance.

**Honoured-mostly.** One concern: the dual-disposition ("rename OR merge") at PASS-C.md:23-24 is a quick-solution-shaped offering. The hardening commits to merge-only via Fault 1.F.

### §11.2 — No workarounds

Pass-C surgical agenda contains no workarounds. The Lock 12 archive is not a workaround for the gorgeous-per-grammar-wrappers fault; it's the lock's named resolution. The bbnf-language-server consolidation is not a workaround for `crates/analysis/`'s grammar-coupling; it's the architecturally honest fix.

The dual-disposition language at PASS-C.md:23-24 is the closest thing to a workaround — "rename OR merge" leaves the per-grammar-rename path open, which Amendment 01 abrogates. Captured under Fault 1.F.

**Honoured-mostly with surgery 1.F.**

### §11.3 — No legacy code survives uncontested

Pass C's bucket allocation:
- KEEP outright: 5 surfaces (`docs/precepts/`, `xtask/`, `rust-toolchain.toml`, `.gitmodules`, sibling repos)
- KEEP-MODIFY: ~15 surfaces (most of the docs tree, Cargo.toml, README.md, scripts/, audit/, etc.)
- ABROGATE-MOVE: `crates/{ser, gorgeous}` (Lock 12), `docs/tranches/{Y..BD}/` (legacy archive), `data/`, `docs/PHASE-4-DIRECTIVE`
- ABROGATE-DELETE: `server/bbnf-lsp`, `extension/*.vsix`, `wasm/pkg*/`, `package.json` (workspace top-level if no consumer), `docs/codegen-paths.md`

Every surface has a verdict; every legacy item is contested.

**Honoured.**

### §11.4 — Idiomatic gestalt approaches

Pass C's docs target shape (`lang/perf/howto/process/audit/spec`) honours the sonic-rs / lightning-css / simdjson cohesion standard (per Lock 13's verbatim text). The 5 + 2 children at the top level mirror sonic-rs's `src/{parser, value, serde, util, lazyvalue, ...}`.

The bbnf-language-server consolidation (post-Fault-1.F + 5.A surgery) is gestalt: one server crate; metadata-dispatched; per-grammar features absorbed via the workspace metadata surface.

The commit-chain disposition (Option 3 + tag + new branch) is gestalt: provenance preserved as ancestry; new chain clean; cutover via `git branch -m` + `git push --force-with-lease`.

**Honoured.**

### §11.5 — Architectural transpositions for elegance / simplicity / performance

The largest transposition is the bbnf-language-server consolidation: 2 crates → 1 crate; 4 cross-crate coupling sites collapse to 0; LSP server, analysis engine, DAP, WASM bindings all converge under one cohesive concern. Elegance + simplicity.

The docs re-do six-wave plan transposes 14 top-level subdirs + 4 prompt files into 5 immediate children + 2 top-level files. Simplicity.

The audit/ restructure (3 wave subdirs) transposes 22 flat files into 3 cohesive groupings. Simplicity.

The commit-chain disposition is a non-transposition (preserve verbatim), but the framing is gestalt: the choice to *not* squash is itself an architectural commitment to provenance.

**Honoured.**

### §11.6 — Surgical-tightness

Per HARDENING.md:170 ("No friction-vagueness"; specify the user, model, point of confusion, verbatim error message), Pass C's friction surfaces lack verbatim error messages. Captured under Faults 7.A through 7.F. Surgery applies.

**Honoured-with-surgery.**

### §11.7 — Two final faults

**Fault 9.A.** PASS-C.md:23-24's dual-disposition language ("rename OR merge") fails the no-quick-solutions discipline. Captured under Fault 1.F; ratified.

**Fault 9.B.** PASS-C.md:33's KEEP-MODIFY verdict for `docs/performance/` (slated for full rewrite) collides with no-workarounds: a "KEEP-MODIFY" bucket-label for content slated for full rewrite is a workaround on the bucket-label level. Captured under Fault 1.B.

**Lane 9 Verdict: honoured-mostly.** Faults: 9.A (covered by 1.F), 9.B (covered by 1.B).

---

## §12 — Punch list

Ordered surgical edits to apply BEFORE the target advances to the synthesizer (or to the master plan, if Pass C feeds master plan amendment-02).

| # | Target file:line | Edit | Owner | Scope | Lane |
|---:|---|---|---|---|---|
| 1 | `restart/audit/passes/PASS-C.md:23-24, 82, 90-94, 482` | Strike "rename to `crates/bbnf-analysis/` OR" — commit to merge-only into `crates/bbnf-language-server/`, framed as **generic + metadata-dispatched per Amendment 01** | Pass-C orchestrator | multi-section | 1, 5, 9 |
| 2 | `restart/audit/passes/PASS-C.md:90-94` (§3.1) | Add: "Internal architecture: bbnf-language-server dispatches per-grammar features through `[workspace.metadata.bbnf.grammars]`. Every per-grammar import (`use bbnf::runtime::<g>::*`) is replaced by a generic dispatch trait that reads from metadata. Future-grammar onboarding requires zero code change in this crate." | Pass-C orchestrator | paragraph | 5 |
| 3 | `restart/audit/passes/PASS-C.md:386-389` (§8.1 item 3) | Add pre-commit gate: "(a) `rg '@pratt\|@simd' crates/bbnf-language-server/src/` returns zero; (b) `rg -nE 'match\s+\w+\s*\{[^}]*\"json\"\s*=>|\"css_l4\"\s*=>|\"bbnf\"\s*=>|\"google_sheets\"\s*=>' crates/bbnf-language-server/src/` returns zero; (c) generated-LOC delta zero" | Pass-C orchestrator | paragraph | 1, 5, 6 |
| 4 | `restart/audit/passes/PASS-C.md:78` | Promote Lock 10 verification from "silent-must-add" to "blocking gate of consolidation commit"; surgery: see item 3 | Pass-C orchestrator | single-line | 1 |
| 5 | `restart/audit/passes/PASS-C.md:33` | Change `docs/performance/` row bucket from KEEP-MODIFY to ABROGATE-REPLACE; surgery column states "delete content; new file at `docs/perf/<topic>.md` per restart vocabulary" | Pass-C orchestrator | single-line | 1, 9 |
| 6 | `restart/audit/passes/PASS-C.md:71` | Replace "confirm `docs/cookbook/path-macro.md` states empty-path elision invariant" with verbatim Wave-2 surgery: "Wave 2 verifies `docs/cookbook/path-macro.md` states the empty-path elision invariant; if absent, adds: *`pointer![]` (the empty path) elides the cursor consultation entirely; the eager fast path pays no consult cost.*" | Pass-C orchestrator | single-line | 1 |
| 7 | `restart/audit/passes/PASS-C.md:101-105` | Add diagnostic for cargo-check failure post-archive ceremony (verbatim error message + surgery sweep) | Pass-C orchestrator | paragraph | 1, 7 |
| 8 | `restart/audit/passes/PASS-C.md:115-126` | Add Lock 13 ceiling note: "`docs/process/` and `docs/howto/` capped at 6 children each; growth past 6 opens a sibling top-level dir or merges concerns" | Pass-C orchestrator | single-line | 1 |
| 9 | `restart/audit/passes/PASS-C.md:10-15, 97, 110, 122, 139, 154, 179, 183, 187, 197` | Replace `audit/restart/...` self-references with `restart/audit/...` (actual layout) | Pass-C orchestrator | multi-section | 3 |
| 10 | `restart/audit/passes/PASS-C.md:43, 135, 418-426` | Disambiguate workspace-root `audit/` (existing corpus) from `restart/audit/` (this suite); reconcile §1 row 17 with §3.5 §8.1 surgery | Pass-C orchestrator | multi-section | 3 |
| 11 | `restart/audit/passes/PASS-C.md:262-267` | Add note acknowledging commit-count drift: "Counts are point-in-time per the synthesis date; the recommendation does not depend on exact counts" | Pass-C orchestrator | single-line | 3 |
| 12 | `restart/audit/passes/PASS-C.md:79` | Replace "honoured-mostly | confirm parse-that disposition" with full carry triple per Fault 8.C | Pass-C orchestrator | single-line | 3, 8 |
| 13 | `restart/audit/passes/PASS-C.md:51` | Replace "verify before delete" with verbatim verification: "read top-level `package.json` for `workspaces` field or non-trivial `scripts`; if neither, DELETE; if either, KEEP + .gitignore `node_modules/`" | Pass-C orchestrator | single-line | 3 |
| 14 | `restart/audit/passes/PASS-C.md:55` | Replace "verify `extension/server/` not stale" with verbatim verification + decision tree per Fault 3.E + 8.D | Pass-C orchestrator | single-line | 3, 8 |
| 15 | `restart/audit/passes/PASS-C.md:410` | Add footnote: "AD does not exist in the legacy tranche set; the letter was skipped" | Pass-C orchestrator | single-line | 3 |
| 16 | `restart/audit/passes/PASS-C.md:206-208, 456-459` | Add per-wave LOC delta budget table (Wave 2 ≤80%/≥60%; Wave 2b ≤70%; Wave 2c ≤95%) | Pass-C orchestrator | paragraph | 6 |
| 17 | `restart/audit/passes/PASS-C.md:445-447` | Add per-tranche stub size budget: "each stub ≤ 100 lines at restart-prelude commit; full draft at tranche execution" | Pass-C orchestrator | single-line | 6 |
| 18 | `restart/audit/passes/PASS-C.md:386-389` | Add: "Generated-LOC delta: zero" | Pass-C orchestrator | single-line | 6 |
| 19 | `restart/audit/passes/PASS-C.md:191-193` | Reconcile `bbnf-cli` defer with full receiver/blocker/gate triple per Fault 8.A | Pass-C orchestrator | paragraph | 7, 8 |
| 20 | `restart/audit/passes/PASS-C.md:191-193` | Add 0.x-window xtask-as-CLI workaround note + cookbook entry | Pass-C orchestrator | single-line | 7 |
| 21 | `restart/audit/passes/PASS-C.md:195-197` | Reconcile `bbnf-py` defer with triggering condition + receiver + gate per Fault 8.B | Pass-C orchestrator | paragraph | 8 |
| 22 | `restart/audit/passes/PASS-C.md:75` | Add cookbook addendum entry to Wave 2c: `docs/howto/cookbook/path-crates.md` explaining triplet rationale | Pass-C orchestrator | single-line | 7 |
| 23 | `restart/audit/passes/PASS-C.md:386-389` | Add user-facing rebase guidance after consolidation: `git log --diff-filter=R --name-only -- crates/analysis crates/lsp crates/bbnf-language-server` | Pass-C orchestrator | single-line | 7 |
| 24 | `restart/audit/passes/PASS-C.md:90-94` | Add LSP metadata-cache friction note + future hot-reload commitment | Pass-C orchestrator | single-line | 7 |
| 25 | `restart/audit/passes/PASS-C.md:406` | Replace brittle verification regex with: "`rg -n 'docs/(bbnf\|parse-that\|pprint\|gorgeous\|performance\|cookbook\|optimizer\|migration\|instructions)/' docs/` returns zero outside `docs/process/tranches/archive/`" | Pass-C orchestrator | single-line | 7 |
| 26 | `restart/audit/passes/PASS-C.md:101-105` | Add comment: `# First git mv creates archive/; no separate mkdir needed.` | Pass-C orchestrator | single-line | 1 |
| 27 | `restart/audit/passes/PASS-C.md:187-189` | Add Amendment-01 framing: "this crate carries fixture *files* organised under `<name>/` subdirectories. Rust code is generic — single fixture-loader trait reading metadata. No per-grammar Rust." | Pass-C orchestrator | paragraph | 5 |
| 28 | `restart/audit/per-agent/pass-c-agent-3-lock-adherence.md:200` | Replace "likely external repo at this point" with definite verdict: "parse-that is external (verified via `Cargo.toml` `[workspace] members` absence); remains external until TS backend lands" | per-agent author | single-line | 3 |
| 29 | `restart/audit/per-agent/pass-c-agent-3-lock-adherence.md:293, 297, 301` | Strike per-grammar-rename path; the agent supersedes per Amendment 01 | per-agent author | multi-section | 1, 5 |
| 30 | `restart/audit/per-agent/pass-c-agent-4-architectural-transposition.md:22-37` | Mark Option A (rename + co-locate) and Option C (multi-grammar future) as superseded by Amendment 01 | per-agent author | paragraph | 1, 5 |

Estimated cumulative scope: ~30 surgical edits, mostly single-line or paragraph. The largest is item 1 (multi-section reconciliation of bbnf-language-server framing across the synthesis); the most consequential is item 2 (internal-architecture commitment for the consolidated server).

No re-draft required. No multi-section structural rewrites required.

---

## §13 — Final readiness

> **Decision: amendment-required**
>
> Pass C's three most consequential outputs — (1) the Lock 12 archive ceremony as blocking precondition, (2) the commit-chain disposition (Option 3 + branch reset), (3) the docs re-do six-wave plan — survive hardening cleanly. The Pass-C corpus diagnoses the right faults (analysis/lsp grammar-coupling, docs tree god-directory, scripts/ god-directory, audit/ god-directory, committed-build-artefacts, stale README) and prescribes correct surgery for each. The decision matrix for the commit chain is fair; the recommendation (Option 3) is justified per `accurate-perf-narrative` and the project's archaeological commitments; the operational sequence is concrete (tag name, branch name, ~8 prelude commits, cutover protocol, reversibility via the tag).
>
> What requires amendment is Pass-C's **bbnf-language-server framing** — currently dual-disposition ("rename OR merge") and tagged "per-grammar"; under Amendment 01, the consolidation must be merge-only, generic, and metadata-dispatched. The hardening also surfaces seven cohesion faults (path self-references, commit-count drift, parse-that hedge, package.json verifier orphan, extension/server/ verifier orphan, AD-gap silence, audit/-tree path collision), four LOC-budget faults (waves 2-2c silent on rewrite-LOC budgets, per-tranche stubs unbounded, bbnf-py LOC silent, consolidation generated-LOC delta unstated), three friction-forecast gaps (archive-cutover diagnostic, path-crate triplet cookbook, consolidation-rebase guidance), and three carry-deferral incompletes (bbnf-cli, bbnf-py, parse-that triples).
>
> The 30 surgical edits in §12 are bounded; total scope ~half a workday. Pass C is ratifiable after surgery.
>
> Hereupon Pass C may advance to the synthesizer once the punch list lands. The bbnf-language-server framing reconciliation is the highest-priority surgery; the cohesion fixes are the most numerous; the LOC-budget addenda are the most procedurally important. None of the faults rise to re-draft severity. The greenfield mandate is honoured; the locks are honoured (with surgery); the precepts are honoured.
