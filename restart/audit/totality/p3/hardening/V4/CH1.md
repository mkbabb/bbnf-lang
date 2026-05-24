# T-P3 V4 CHALLENGE — CH1 CORRECTNESS Lens (CONFIRMING — LOCK-TRIGGER cycle)

Pass: T-P3 Synthesis. Cycle: V4. Lens: CH1 CORRECTNESS.
Date: 2026-05-24. HEAD: b9b800e14 (V3 close; the 7 T-P3 artefacts under
review are V3-stable through HEAD `89686aac3` — V4 context seed touches
only `restart/audit/totality/p3/hardening/V4/CHALLENGE-CONTEXT.md`, zero
edits to `3A`/`3B`/`3C-cryst`/`3C-diff`/`3D`/`3E`/`3F`; verified via
`git diff b9b800e14 HEAD -- 'restart/audit/totality/p3/3*.md'` →
empty). HARD CAP: 20min.

## Scope

Confirming-wave verification per V4 CHALLENGE-CONTEXT.md §2 CH1 row:
V3 CH1 evidence re-executed at HEAD; zero-drift confirmation. Every
V3 ACCEPT (10/10) claim must remain reproducible. Per V4 CHALLENGE
§2 CH1 expectation: "V3 100% (10/10) confirms → V4 second consecutive
≥95% NO caveat → 4-cycle LOCK extension (V1 95.7% / V2 100% / V3 100%
/ V4 100%)".

## Findings

### F-V4-CH1-RE-01 — 3C-diff:69 `32:69` canonical refutation density (V3 F-V3-CH7-3C confirm)

DISCHARGED at HEAD. Executable evidence:

- `grep -n "32:69" restart/audit/totality/p3/3C-locks-v+1-diff.md`
  → `69:+SK-V14 cohort 32:69 = 31.7% refutation density preservation;
  anti-paper-close` (EXIT:0, single hit at `:69` exactly as V3 predicted).
- `grep -n "31:69" restart/audit/totality/p3/3C-locks-v+1-diff.md`
  → EXIT:1, zero hits. V2-residue `31:69` remains eradicated at HEAD.

Zero-drift from V3 ACCEPT. Cite:
`restart/audit/totality/p3/3C-locks-v+1-diff.md:69`.

### F-V4-CH1-RE-02 — 3B `-maxdepth 2` eradication (V3 F-V3-CH7-3B confirm)

DISCHARGED at HEAD. Executable evidence:

- `grep -n "maxdepth 2"
  restart/audit/totality/p3/3B-master-plan-reconciliation.md` → EXIT:1,
  zero hits. Both V2-era `-maxdepth 2` sites (:124 MP-3B-V1-D03 bound
  command + :217 code-block illustration) remain scrubbed.
- `restart/audit/totality/p3/3B-master-plan-reconciliation.md:124`
  MP-3B-V1-D03 row continues to read `find crates/core/src/runtime
  -mindepth 2 -type f -name '*.rs' \| wc -l` — canonical Pattern H
  command intact.

Zero-drift from V3 ACCEPT.

### F-V4-CH1-RE-03 — 3F `-maxdepth 2` eradication (V3 F-V3-CH7-3F confirm)

DISCHARGED at HEAD. Executable evidence:

- `grep -n "maxdepth 2"
  restart/audit/totality/p3/3F-migration-handoff.md` → EXIT:1, zero hits.
  V2-era `:123` site remains scrubbed; canonical Pattern H command body
  intact.

Zero-drift from V3 ACCEPT. Cite:
`restart/audit/totality/p3/3F-migration-handoff.md:123`.

### F-V4-CH1-RE-04 — Pattern H 67-file canonical census (V3 confirm)

VERIFIED at HEAD. Executable evidence:

- `find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime
  -mindepth 2 -type f -name '*.rs' | wc -l` → `67` exactly.

LAC-1E-15 67-canonical (D-1E-15 receiver row at
`restart/audit/totality/p1/1E-locks-evidence.md:102`; source row at
`:125`) remains the live ground truth bound across 3B:124, 3B:217,
3F:123 per V3 CH1 §F-V3-CH7-3B/3F. Zero-drift from V3 ACCEPT.

### F-V4-CH1-RE-05 — V4-4 Target A per-hunk `git apply --check --recount` (V2 carry-forward confirm)

VERIFIED at HEAD. Executable evidence:

- `sed -n '150,171p' restart/audit/totality/p3/3C-locks-v+1-diff.md >
  /tmp/v4_4_targetA.diff && git apply --check --recount
  /tmp/v4_4_targetA.diff` → EXIT:0.
- Header at `restart/audit/totality/p3/3C-locks-v+1-diff.md:152` reads
  `@@ -113,5 +113,19 @@ 5. **IR + per-backend lower**. Codegen emits
  a backend-agnostic typed IR; per-backend lowerers produce native
  source...` — V2 F-V2-CH1-3C-C unified-diff header intact.

V4-4 Lock 6 round-trip applies cleanly against current
`restart/locks/LOCKS.md`. Zero-drift from V3 ACCEPT.

### F-V4-CH1-RE-06 — V4-4 Target B per-hunk `git apply --check --recount` (V2 carry-forward confirm)

VERIFIED at HEAD. Executable evidence:

- `sed -n '177,193p' restart/audit/totality/p3/3C-locks-v+1-diff.md >
  /tmp/v4_4_targetB.diff && git apply --check --recount
  /tmp/v4_4_targetB.diff` → EXIT:0.
- Header at `restart/audit/totality/p3/3C-locks-v+1-diff.md:179` reads
  `@@ -227,7 +227,15 @@     tests/proof fixtures routed through
  generic roots, or grammar-shaped policy` — V2 F-V2-CH1-3C-C
  unified-diff header intact.

V4-4 Lock 14 round-trip applies cleanly against current
`restart/locks/LOCKS.md`. Zero-drift from V3 ACCEPT.

### F-V4-CH1-RE-07 — V2 CH1-3B carry-forward (MP-3B-V1-D03 pair-role explicit)

VERIFIED at HEAD. Executable evidence:

- `grep -n "1E-locks-evidence.md:125"
  restart/audit/totality/p3/3B-master-plan-reconciliation.md` → `:124`.
- `:124` row preserves pair-role attribution
  `restart/audit/totality/p1/1E-locks-evidence.md:125 (LAC-1E-15
  source) + :102 (D-1E-15 receiver row)` verbatim. V2 F-V2-CH1-3B
  intact; V3 `-mindepth 2` token swap left prose untouched; V4 zero-edit
  preserves the V3 V2-LOCK posture.

Zero-drift from V3 ACCEPT.

### F-V4-CH1-RE-08 — V2 CH1-3C-A carry-forward (frontmatter split + LAC row count)

VERIFIED at HEAD. Executable evidence:

- `grep -n "proposed_candidate_count\|proposed_hunk_count"
  restart/audit/totality/p3/3C-locks-crystallisation.md` →
  `9:proposed_candidate_count: 51` + `10:proposed_hunk_count: 18`.
  V2 F-V2-CH1-3C-A separate keys preserved.
- `grep -c "^| LAC-\|^| T2A-LAC-"
  restart/audit/totality/p3/3C-locks-crystallisation.md` → `51`
  (38 ACCEPT + 13 MODIFY canonical). LAC inventory intact.

Zero-drift from V3 ACCEPT.

### F-V4-CH1-RE-09 — V2 CH1-3C-B carry-forward (V4-7 hunk-index `:263`)

VERIFIED at HEAD. Executable evidence:

- `sed -n '28p' restart/audit/totality/p3/3C-locks-v+1-diff.md` →
  `| V4-7 | Lock 14 Pattern H census + byte_class_from_range_64
  sibling | append after restart/locks/LOCKS.md:263 (grammar-neutral
  primitives paragraph close; was :253 in V1, corrected to :263 per
  HEAD e12c5323d verification — :253 is mid-Lock 14 per-wave gate
  enforcement, :263 is the close of the Shared bbnf-simd... paragraph)
  | LAC-1E-15 + LAC-2F-V5-03 |`.

V2 F-V2-CH1-3C-B `:263` hunk-index correction prose intact verbatim.
Zero-drift from V3 ACCEPT.

### F-V4-CH1-RE-10 — V2 CH1-3F carry-forward (LAC-2F-V5-02 attribution alignment)

VERIFIED at HEAD. Executable evidence:

- `grep -n "HARDENING-T-P2-V3-CONSOLIDATED.md:182"
  restart/audit/totality/p3/3F-migration-handoff.md` → `:82`.
- `:82` paragraph preserves V2 F-V2-CH1-3F attribution prose
  `canonical T-P2 V3 LOCK evidence at
  restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:182-192;
  V5 was confirmation cycle re-passing V4 packet unchanged per
  HARDENING-T-P2-V5-CONVERGED.md` intact at HEAD.

Zero-drift from V3 ACCEPT.

### Zero-drift integrity gate

`git diff b9b800e14 HEAD -- 'restart/audit/totality/p3/3*.md'` returns
EMPTY output — confirming the 7 T-P3 artefacts (3A, 3B, 3C-cryst,
3C-diff, 3D, 3E, 3F) are byte-identical between the V3 close commit
`b9b800e14` and current HEAD `89686aac3`. The V4 confirming wave
operates against a frozen artefact surface per CHALLENGE-CONTEXT §1
declaration.

## Accept Rate

10/10 = **100%** — ACCEPT.

3 V3 CH7-driven edits (F-V3-CH7-3C :69 + F-V3-CH7-3B :124/:217 +
F-V3-CH7-3F :123) remain discharged at HEAD; 5 V2 CH1 carry-forward
findings (3B pair-role, 3C-A frontmatter split + LAC count, 3C-B
:263, 3C-C V4-4 @@ headers, 3F LAC-2F-V5-02 attribution) remain
intact; 2 canonical re-checks (Pattern H census = 67; V4-4 A+B
per-hunk apply-clean = exit:0/exit:0) both pass. Zero new CH1 REVISE;
zero CH1 REJECT.

## Verdict

`G-T-P3-V4-CH1`: **ACCEPT**. V4 confirming wave reproduces every V3
ACCEPT claim byte-identically at HEAD; zero-drift integrity gate
empirically green (empty `git diff b9b800e14 HEAD -- 3*.md`); 13 of
13 executable re-runs match V3 transcripts exactly (3 grep
discharges, 1 Pattern H census = 67, 2 per-hunk apply-clean exit:0,
5 V2 carry-forward grep/sed citations, plus the zero-drift integrity
gate itself).

## LOCK Trajectory

V1 95.7% → V2 100% → V3 100% → V4 100% — **4-cycle LOCK extension**
(V1+V2+V3+V4 all ≥95%; V2+V3+V4 trio all at 100%). Per V4
CHALLENGE-CONTEXT §2 CH1 prediction ("V3 100% (10/10) confirms → V4
second consecutive ≥95% NO caveat → 4-cycle LOCK extension")
achieved exactly. V4 = 4/5 ceiling consumed; 1-cycle margin to V≤5
ceiling preserved per §3Z, but cohort §3Z LOCK eligibility satisfied
at this V4 cycle.

## Revise Queue

**Empty.** V2 LOW prophylactic note (5 non-V4-4 hunks ship as
prose-styled `+` lines without `@@` headers, so full-file `git apply
--check --recount` on 3C-diff returns EXIT:128 "corrupt patch at
line 172") carries forward unchanged from V3 §Revise Queue as
NON-BLOCKING per V2 §6 disposition and V3 CONSOLIDATED §5 routing
("may defer to Pass Omega CRUD-3 consumption; not blocking for §3Z
LOCK"). Per-hunk extraction returns exit:0 on V4-4 Target A and
Target B at HEAD (F-V4-CH1-RE-05 + F-V4-CH1-RE-06); V3 :69 preface
modification preserves all per-hunk apply-clean invariants by
construction (the edit modifies `+` content only, not hunk-header
arithmetic or context lines).
