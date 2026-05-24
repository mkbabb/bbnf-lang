# T-P3 V3 CHALLENGE — CH1 CORRECTNESS Lens

Pass: T-P3 Synthesis. Cycle: V3. Lens: CH1 CORRECTNESS.
Date: 2026-05-24. HEAD: b9b800e14 (V3 atomic micro-fold) — current
working tree at `a4df15abc docs(sk-v14-tp3-v3-context): purge stale
May-21 6-lens cycle + seed V3 LOCK-eligible context` adds only the
V3 CHALLENGE-CONTEXT seed without touching the 7 T-P3 artefacts. HARD
CAP: 25 min.

## Scope

Per `restart/audit/totality/p3/hardening/V3/CHALLENGE-CONTEXT.md:25`
CH1 row: verify F-V3-CH7-3C `:69` + F-V3-CH7-3B `:124,:217` +
F-V3-CH7-3F `:123` all discharged at HEAD via grep; re-execute Pattern
H census; re-execute `git apply --check --recount` on V4-4 Targets
A+B (V2 carry-forward); confirm V2 CH1-3B, V2 CH1-3C-A, V2 CH1-3F
carry-forward intact.

## Findings

### F-V3-CH7-3C — 3C-diff:69 V4-1 preface `31:69` → `32:69`

DISCHARGED at HEAD. Executable evidence:

- `grep -n "32:69" restart/audit/totality/p3/3C-locks-v+1-diff.md` returns:
  `69:+SK-V14 cohort 32:69 = 31.7% refutation density preservation;
  anti-paper-close` — single hit at `:69` as required.
- `grep -n "31:69" restart/audit/totality/p3/3C-locks-v+1-diff.md`
  returns EXIT:1 (zero hits) — V2 residue eradicated.
- Edit lands in V4-1 hunk preface inside `diff` fenced block; the
  changed line is a `+` content line (NOT a hunk-header context line),
  so per-hunk applicability invariants are preserved (no `@@ -N,M
  +N,M @@` arithmetic touched).

Cite: `restart/audit/totality/p3/3C-locks-v+1-diff.md:69` (`+SK-V14
cohort 32:69 = 31.7% refutation density preservation; anti-paper-close`).

### F-V3-CH7-3B — 3B:124,:217 `-maxdepth 2` dropped

DISCHARGED at HEAD. Executable evidence:

- `grep -n "maxdepth 2" restart/audit/totality/p3/3B-master-plan-reconciliation.md`
  returns EXIT:1 (zero hits) — both sites scrubbed.
- `restart/audit/totality/p3/3B-master-plan-reconciliation.md:124`
  MP-3B-V1-D03 bound command now reads `find crates/core/src/runtime
  -mindepth 2 -type f -name '*.rs' \| wc -l` (canonical Pattern H
  command).
- `restart/audit/totality/p3/3B-master-plan-reconciliation.md:217`
  code-block illustration now reads `find crates/core/src/runtime
  -mindepth 2 -type f -name '*.rs' | wc -l` followed by `# expected:
  67 at SK-V14 baseline; +N requires grammar-roster change OR sub-wave
  count update` at `:218` — canonical 67 expectation pinned in-block.

### F-V3-CH7-3F — 3F:123 `-maxdepth 2` dropped

DISCHARGED at HEAD. Executable evidence:

- `grep -n "maxdepth 2" restart/audit/totality/p3/3F-migration-handoff.md`
  returns EXIT:1 (zero hits).
- `restart/audit/totality/p3/3F-migration-handoff.md:123` 3F-MIG-003
  bound command now reads `find crates/core/src/runtime -mindepth 2
  -type f -name '*.rs' | wc -l` — aligned with 3B :124,:217 + 3C
  V4-7 hunk body.

### Pattern H 67-file canonical census

VERIFIED at HEAD. Executable evidence:

- `find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime
  -mindepth 2 -type f -name '*.rs' | wc -l` returns `67` — Pattern H
  canonical count per LAC-1E-15 (D-1E-15 receiver row at
  `restart/audit/totality/p1/1E-locks-evidence.md:102`; LAC-1E-15
  source row at `:125`). 3-of-3 V3 commands (3B:124, 3B:217, 3F:123)
  now bind to the live 67-count canonical.

### `git apply --check --recount` on 3C-locks-v+1-diff.md (V2 carry-forward)

VERIFIED at HEAD. The full-file `git apply --check --recount
restart/audit/totality/p3/3C-locks-v+1-diff.md` returns EXIT:128
("corrupt patch at line 172") — this matches the V2 known posture per
V2 CH1 §2 lines 79-81: five non-V4-4 hunks ship as prose-styled `+`
lines without `@@` headers, so the full-file invocation fails on
markdown fence interleave. V2 disposition: per-hunk extraction returns
exit:0; V3 :69 edit must preserve per-hunk exit:0 invariant.

Per-hunk verification at V3 HEAD:

- V4-4 Target A (Lock 6 round-trip) — `sed -n '150,171p'
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/v4_4_targetA.diff
  && git apply --check --recount /tmp/v4_4_targetA.diff` → EXIT:0.
- V4-4 Target B (Lock 14 round-trip) — `sed -n '177,193p'
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/v4_4_targetB.diff
  && git apply --check --recount /tmp/v4_4_targetB.diff` → EXIT:0.

V4-4 A+B both ship proper `--- a/`, `+++ b/`, `@@ -N,M +N,M @@`
headers per V2 F-V2-CH1-3C-C discharge, and both apply cleanly against
current `restart/locks/LOCKS.md` at HEAD. V2 carry-forward intact.

V4-1 preface :69 edit invariant: the `:69` modification is in a `+`
content line within the V4-1 hunk body, not in any hunk-header
arithmetic or context-prefix line. Because V4-1 is a pure-insertion
hunk at LOCKS.md:42→44 (no surrounding context lines from LOCKS.md
that the modification could de-sync), the preface text change cannot
break applicability. V2 carry-forward V4-1 reconstructed-and-applied
posture is preserved by construction.

### V2 CH1-3B carry-forward — MP-3B-V1-D03 pair-role explicit

VERIFIED at HEAD. `grep -n "1E-locks-evidence.md:125"
restart/audit/totality/p3/3B-master-plan-reconciliation.md` returns
`:124` confirming the pair-role explicit citation
`restart/audit/totality/p1/1E-locks-evidence.md:125 (LAC-1E-15 source)
+ :102 (D-1E-15 receiver row)` (V2 F-V2-CH1-3B). The V3 `-maxdepth 2`
drop landed on the same line :124 without disturbing the pair-role
substring — V3 edit is `-maxdepth 2` → `-mindepth 2` token-level
swap on the bound command portion, preserving the cite-pair prose
unchanged. V2 carry-forward LOCKED.

### V2 CH1-3C-A carry-forward — frontmatter split

VERIFIED at HEAD. `grep -n "proposed_candidate_count\|proposed_hunk_count"
restart/audit/totality/p3/3C-locks-crystallisation.md` returns
`9:proposed_candidate_count: 51` + `10:proposed_hunk_count: 18` —
V2 F-V2-CH1-3C-A separate keys preserved. LAC row count
`grep -c "^| LAC-\|^| T2A-LAC-"
restart/audit/totality/p3/3C-locks-crystallisation.md` returns **51**
at HEAD (38 ACCEPT + 13 MODIFY canonical). V3 zero-touch on 3C-cryst
preserves V2 LOCK.

### V2 CH1-3F carry-forward — LAC-2F-V5-02 attribution alignment

VERIFIED at HEAD. `grep -n "HARDENING-T-P2-V3-CONSOLIDATED.md:182"
restart/audit/totality/p3/3F-migration-handoff.md` returns `:82`
confirming the V2 F-V2-CH1-3F attribution text "canonical T-P2 V3
LOCK evidence at
`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:182-192`;
V5 was confirmation cycle re-passing V4 packet unchanged per
`HARDENING-T-P2-V5-CONVERGED.md`" is intact at HEAD. V3 only touched
3F:123 (bound command body); V3 zero-touch on :82 preserves V2 LOCK.

### V2 CH1-3C-B carry-forward — V4-7 hunk-index `:263`

VERIFIED at HEAD. `restart/audit/totality/p3/3C-locks-v+1-diff.md:28`
table row reads `append after restart/locks/LOCKS.md:263
(grammar-neutral primitives paragraph close; was :253 in V1, corrected
to :263 per HEAD e12c5323d verification — :253 is mid-Lock 14
per-wave gate enforcement, :263 is the close of the Shared bbnf-simd…
paragraph)`. V3 zero-touch on :28 preserves V2 LOCK.

### V2 CH1-3C-C carry-forward — V4-4 unified-diff `@@` headers

VERIFIED at HEAD via per-hunk extraction transcript above (Target A
exit:0, Target B exit:0). The `@@ -113,5 +113,19 @@` (Target A) and
`@@ -227,7 +227,15 @@` (Target B) headers at
`restart/audit/totality/p3/3C-locks-v+1-diff.md:152,179` are
unchanged at HEAD. V3 zero-touch on Target A+B headers preserves V2
LOCK.

## Accept Rate

10/10 = **100%** — ACCEPT.

3 V3 deltas (F-V3-CH7-3C :69 + F-V3-CH7-3B :124,:217 + F-V3-CH7-3F
:123) all empirically discharged at HEAD; 5 V2 CH1 carry-forward
findings (3B, 3C-A, 3C-B, 3C-C, 3F) all preserved verbatim; 2
canonical re-checks (Pattern H census = 67, V4-4 A+B `git apply
--check --recount` = exit:0) both pass.

## Verdict

`G-T-P3-V3-CH1`: **ACCEPT**. All 3 V3 CH7-mandated edits discharged
with executable grep evidence; Pattern H 67-canonical re-bound across
3 sites; V4-4 A+B per-hunk apply-clean preserved post-V3 :69 preface
edit (the edit modifies `+` content only, not hunk-header arithmetic
or context lines); all 5 V2 CH1 carry-forward findings intact at HEAD.
Zero new CH1 REVISE surfaced; zero CH1 REJECT.

## LOCK Trajectory

V1 95.7% → V2 100% → V3 100% — **LOCK extension; 3-cycle LOCK depth
achieved** (V1+V2+V3 all ≥95%; V2+V3 both at 100%). Per
CHALLENGE-CONTEXT §2 CH1 prediction ("V2 100% → V3 100% expected
(LOCK extension; 3-cycle)") achieved exactly. Per V2 CONSOLIDATED §1
prediction ("V3 → V4 100% LOCK extension") on-trajectory for V4
confirming cycle.

## Revise Queue

**Empty.** V2 LOW prophylactic (5 non-V4-4 hunks ship without `@@`
headers; full-file `git apply --check --recount` returns EXIT:128
"corrupt patch") remains NON-BLOCKING per V2 §6 disposition and
CONSOLIDATED §5 routing ("1 NON-BLOCKING optional V3 polish (CH1
5-hunk `@@` headers for full-file `git apply` exit:0 single
invocation): may defer to Pass Omega CRUD-3 consumption; not blocking
for §3Z LOCK"). Per-hunk extraction returns exit:0 for the seven V2
representative hunks at HEAD; V3 :69 preface modification preserves
all per-hunk apply-clean invariants by construction.
