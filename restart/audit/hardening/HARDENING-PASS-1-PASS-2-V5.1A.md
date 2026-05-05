# HARDENING PASS-1/PASS-2 V5.1A — Narrow Citation Hygiene Recheck

## §1 Target and commits verified

Target route: PASS-1/PASS-2 narrow V5.1 residue.

Verified commits:

| Commit | Role |
|---|---|
| `b64a18a1` | Phase 0.5 PASS-1/PASS-2 amendment. |
| `e647139c` | V5.1 PASS-1/PASS-2 verification report, verdict `AMENDMENT-REQUIRED`. |
| working amendment | This V5.1A pass removes the four narrow shifted local line citations named by `e647139c`. |

Input residue from V5.1:

| Residue | Prior fault |
|---|---|
| `PASS-1.md:218` | Cited shifted local diagnostic lines. |
| `PASS-2.md:92` | Cited shifted PASS-1 diagnostic range. |
| `PASS-2.md:112` | Cited shifted PASS-1 diagnostic range. |
| `PASS-2.md:359` | Cited shifted PASS-1 diagnostic range. |

Scope applied: provenance-only. No Backend IR, diagnostic vocabulary, recognizer, YAML, WASM, rare-declaration, or runtime contract semantics changed.

## §2 Bundle closure table

| Bundle | Current evidence | Verdict | Rationale |
|---|---|---|---|
| 3 - PASS-local citation drift | `restart/audit/pass-1-substrate/PASS-1.md:218` now refers to the §2 error vocabulary and diagnostic strings table instead of shifted line numbers. | CLOSED | The line-level citation was the only remaining PASS-1 fault. Section citation is stable under nearby rare-fence insertions. |
| 3 - PASS-2 cross-PASS diagnostic ownership | `restart/audit/pass-2-codegen/PASS-2.md:92` now cites PASS-1 §2 diagnostic strings. | CLOSED | The owner remains PASS-1; the brittle range is gone. |
| 3 - PASS-2 hand-off contract | `restart/audit/pass-2-codegen/PASS-2.md:112` now cites PASS-1 §2 invariants, variant ownership, and diagnostic strings. | CLOSED | The contract still names the owned surfaces without depending on exact line offsets. |
| 3 - PASS-3 diagnostic vocabulary gate | `restart/audit/pass-2-codegen/PASS-2.md:359` now cites PASS-1 §2 diagnostic strings. | CLOSED | The gate still round-trips the same six diagnostic codes through PASS-3. |
| 9 - Diagnostic alias polish | `BBNF1004`, `BBNF-LOOKBEHIND-WIDTH`, `LookbehindWidth`, and `BBNF-SEM040` remain present in their intended PASS-1/PASS-2 rows. | CLOSED | The alias chain remains semantic; only citation form changed. |

## §3 Pathology regression scan

| Lens | Checked text | Result | Notes |
|---|---|---|---|
| F - pseudo precision | Removed exact local line anchors from explanatory prose. | PASS | Current prose uses section ownership where line offsets are not load-bearing. |
| F - buzzword reliance | Diagnostic ownership references still name concrete codes and tables. | PASS | No broad "diagnostic surface" claim stands without mechanism. |
| F - hedging | Amended sentences use ownership language, not "should" or "may". | PASS | No new soft commitment introduced. |
| G - pattern lift | PASS-2 remains payload refiner, not BIR re-owner. | PASS | No new architectural pattern was introduced. |
| G - missing alternative | Rare declaration and YAML alternatives remain in existing rows; this pass did not alter them. | PASS | No new decision point was added. |
| H - wrong-line citations | Stale grep for the four cited shifted ranges returns zero. | PASS | The remaining PASS-1 line references in PASS-2 are not the diagnostic residue named by V5.1. |
| H - provenance drift | PASS-1 §2 remains the cited owner for diagnostics. | PASS | The amended citations point at a stable owning section. |

## §4 Gate rerun

| Command | Result |
|---|---|
| `git status --short` | Dirty only in `PASS-1.md` and `PASS-2.md` during this amendment. |
| `rg -n "PASS-1.md:90\|PASS-1.md:96\|PASS-1.md:92-101\|PASS-1.md:96-101" restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md` | Zero matches. |
| `rg -n "path!\|@pratt\|@simd\|OpenFrame\|LayoutFacts\|LayoutSink\|passes::layout\|pointer!\|select!\|LookbehindWidth\|BBNF-LOOKBEHIND-WIDTH\|BBNF1004\|@host fn\|waves-v4\|wave-4\|Wave 4\|WASM\|incubat\|rare\|yaml\|diagnostic\|BBNF-SEM040" restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md` | Matches classify as expected settled terms, deletion archaeology, or diagnostic contract rows; no retired `path!`, `@pratt`, `@simd`, or Wave-4 stale hit appears. |
| `git diff --check` | Clean. |

## §5 Residue ledger

None for the PASS-1/PASS-2 V5.1 route. The four post-V5.1 residual citation rows are closed.

## §6 Final verdict

Verdict: `READY`.

Phase 0.5 is therefore cohort-ready when combined with:

| Route | V5.1 report | Verdict |
|---|---|---|
| SYNTHESIS | `HARDENING-SYNTHESIS-V5.1.md` | `READY` |
| PASS-1/PASS-2 | this V5.1A report | `READY` |
| PASS-3 | `HARDENING-PASS-3-V5.1.md` | `READY` |

## §7 Closing posture

V5 found real post-V4 drift; V5.1 closed the substantive amendments; V5.1A closes the remaining PASS-local citation hygiene without widening scope. The corpus can proceed to Phase 1 research deep-dives, with the V6 hardening pass still obligated to re-check research-folded text for F/G/H pathologies.
