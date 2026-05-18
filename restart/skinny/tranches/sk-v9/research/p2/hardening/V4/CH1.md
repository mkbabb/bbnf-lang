# SK-V9 S-P2 CHALLENGE V4 — CH1 CORRECTNESS

Pass: S-P2 Research. Cycle: V4. Lens: CH1 CORRECTNESS.
Date: 2026-05-18.
Authority: `restart/prompts/ORCHESTRATOR.md` §3W / §3Z.
Re-verification cycle: CH1 already converged — V1 (96.7%), V2 (98.2%),
V3 (97.6%): three consecutive ≥95%. V4 verifies that the single-edit
V4 fold (the one 1-token correctness fix the V3 cycle surfaced as its
lone REVISE) landed correctly and introduced nothing new.

Inputs verified this cycle:
- `restart/skinny/tranches/sk-v9/research/p2/hardening/V3/CH1.md`
  (the V3 cycle's §4 LOW REVISE — defect #1, the S16 citation offset).
- The V4-folded `skv9-p2-D-aarch64-asm-opportunities.md` §3.6 + §8.
- `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs`
  (lines 1-100 — the cited function declaration site).
- `git show 1eee3375` — the V4 fold commit, full diff + stat.

## §1 — V3-REVISE resolution

V3 CH1 carried exactly one defect into V4: **§4 defect #1 (the S16
spot-check REVISE)**. P2-D cited `match_tiny_plain_string_neon` at
`match_tiny_plain_string.rs:79` in **two** sites — the §3.6 prose
cross-reference and the §8 Sources list. Line 79 is the
`#[cfg(target_arch = "aarch64")]` attribute; the function declaration
is two lines lower. V3 graded it LOW severity (citation offset,
non-load-bearing, non-V3 origin — pre-dated the V3 fold) and carried
it forward as a one-token edit candidate for S-P3 close or the V4
fold.

### §1.1 — V4 fold landed the 1-token fix — **RESOLVED**

The V4 fold (commit `1eee3375`, "docs(sk-v9-p2-v4): fold V3 CHALLENGE
— match_tiny_plain_string line fix") corrected **both** citation
sites. `git show 1eee3375 --stat`: one file changed, 2 insertions /
2 deletions — exactly the two `:79`→`:81` edits, nothing else.

- **§3.6 prose** (`skv9-p2-D` line 490): "fold the range tests into a
  *single* TBL via a 64-entry low-6-bit table (the Class A
  `match_tiny_plain_string` shape per `match_tiny_plain_string.rs:81`)"
  — was `:79`, now `:81`.
- **§8 Sources** (`skv9-p2-D` line 1117): "`…/match_tiny_plain_string.rs:81`
  (`match_tiny_plain_string_neon` — low-6 TBL shape referenced in §3.6;
  line 79 is the `#[cfg]` attribute)." — was `:79`, now `:81`, with a
  clarifying parenthetical naming line 79 as the `#[cfg]` attribute.

Both sites resolve to `:81`. D-V3-#1 is **fully closed**.

### §1.2 — `:81` verified against source — **CONFIRMED**

This cycle read `match_tiny_plain_string.rs` lines 1-100 to adjudicate
the corrected line number:

- Line **79**: `#[cfg(target_arch = "aarch64")]` — the attribute the
  V4 §8 parenthetical now names explicitly.
- Line **80**: `#[inline]` — the second attribute.
- Line **81**: `pub unsafe fn match_tiny_plain_string_neon(` — the
  function declaration. The signature continues `haystack: *const u8,`
  (82), `table: uint8x16x4_t,` (83), `) -> (u16, Option<u8>) {` (84).

The corrected citation `:81` lands on the declaration line exactly.
The original `:79` was a two-line offset onto the `#[cfg]` attribute.
The V4 fold is **net-corrective** — it replaces an off-by-two
citation with the verified-exact one and adds an honest parenthetical
that disambiguates the attribute lines.

## §2 — V4 verification (dispositions)

Verdicts: ACCEPT (claim verified against evidence), REVISE (defect
requires fold), REJECT (load-bearing falsification).

| # | Check | Verification | Verdict |
|---:|---|---|---|
| V4-1 | P2-D §3.6 prose cites `match_tiny_plain_string.rs:81` | Line 490: `…shape per `match_tiny_plain_string.rs:81`)` — reads `:81` | **ACCEPT** |
| V4-2 | P2-D §8 Sources cites `match_tiny_plain_string.rs:81` | Line 1117: `…/match_tiny_plain_string.rs:81` (`match_tiny_plain_string_neon` …)` — reads `:81` | **ACCEPT** |
| V4-3 | Source line 81 carries the `match_tiny_plain_string_neon` declaration | `match_tiny_plain_string.rs:81`: `pub unsafe fn match_tiny_plain_string_neon(` — exact, the function signature opens here | **ACCEPT** |
| V4-4 | Source line 79 is the `#[cfg]` attribute (the V4 parenthetical's claim) | `match_tiny_plain_string.rs:79`: `#[cfg(target_arch = "aarch64")]` — confirmed; line 80 is `#[inline]`, line 81 the fn — the §8 parenthetical "line 79 is the `#[cfg]` attribute" is accurate | **ACCEPT** |
| V4-5 | The V4 fold touched only the two `:79`→`:81` edits | `git show 1eee3375 --stat`: 1 file, `2 insertions(+), 2 deletions(-)`; full diff shows exactly the §3.6 line + the §8 line, no third hunk | **ACCEPT** |
| V4-6 | No other report was touched by the V4 fold | V4 commit stat names only `skv9-p2-D-aarch64-asm-opportunities.md`; P2-A..C, P2-E, P2-F carry no V4 edit — load-bearing F1 wiring fix + F2 PMU rederivation untouched | **ACCEPT** |
| V4-7 | No measured number, verdict, gate, or arithmetic disturbed | The two edits are pure line-number digits inside file:line citations; no claim, table cell, or §5.3.1 gate altered — monotone-conservative | **ACCEPT** |
| V4-8 | The §8 parenthetical introduces no falsifiable claim | "line 79 is the `#[cfg]` attribute" is a provenance note verified true (V4-4); it asserts no measured behaviour, only disambiguates the attribute stack — no new falsifiability surface | **ACCEPT** |
| V4-9 | The §3.6 cross-reference remains invocable post-fix | §3.6 (the §3 escape-codec wave) references the Class A low-6 TBL shape; `match_tiny_plain_string_neon` at `:81` is the named primitive — the reference now resolves to the declaration, strengthening invocability | **ACCEPT** |
| V4-10 | The V3-fold REDRESS line ranges (`:324-337` / `:394-418`) survive V4 untouched | §5.5 + §8 + §0 footer still carry the V3-landed ranges; the V4 diff does not touch lines 944-951, 1113, or 1189-1191 — the V3 D11-V2 closure holds | **ACCEPT** |
| V4-11 | The V3-fold §0 footer (lines 1180-1194) survives V4 untouched | V4 diff hunks are at lines 490 and 1117 only; the §0 V3-fold footer recording the five V3 edits is unaltered — V4 added no §0 footer entry (consistent with a sub-edit 1-token fold) | **ACCEPT** |
| V4-12 | The corrected citation is uniquely resolvable | `match_tiny_plain_string_neon` is uniquely named within `match_tiny_plain_string.rs`; `:81` lands on its sole declaration — no ambiguity | **ACCEPT** |

**V4 verification: 12 ACCEPT, 0 REVISE, 0 REJECT.** Twelve
dispositions — beyond the ≥8 floor.

## §3 — Aggregate verdict

| Cohort | ACCEPT | REVISE | REJECT | Total | ACCEPT rate |
|---|---:|---:|---:|---:|---:|
| §2 V4 verification | 12 | 0 | 0 | 12 | 100% |
| **Aggregate** | **12** | **0** | **0** | **12** | **100%** |

**Verdict.** CH1 CORRECTNESS re-verifies clean at **100% ACCEPT**
with **zero REVISEs and zero REJECTs** — a fourth consecutive
qualifying cycle (V1 96.7%, V2 98.2%, V3 97.6%, V4 100%).

- The **V3 carried-forward LOW REVISE (S16 / §4 defect #1)** is
  **fully closed**. The V4 fold corrected both `:79` citations to
  `:81` — the §3.6 prose cross-reference and the §8 Sources entry.
  This cycle read `match_tiny_plain_string.rs` and confirmed line 81
  carries the `pub unsafe fn match_tiny_plain_string_neon` declaration
  and line 79 the `#[cfg(target_arch = "aarch64")]` attribute. The fix
  is net-corrective and the §8 parenthetical it added ("line 79 is the
  `#[cfg]` attribute") is itself verified true.
- The **V4 fold touched nothing else.** `git show 1eee3375 --stat`
  confirms one file, 2 insertions / 2 deletions — exactly the two
  citation digits. No other report, no measured number, no verdict,
  no gate, no arithmetic was disturbed. The V3-landed REDRESS line
  ranges and the §0 V3-fold footer survive intact. The fold is
  monotone-conservative — it cannot reopen a route or introduce a
  claim.
- **No new defect.** The V4 verification surfaced zero new findings.
  The lone V3 REVISE is resolved; the cohort carries forward clean.

Per §3Z: CH1 was converged on V1+V2+V3 (three consecutive ≥95%). V4
confirms the convergence holds post-fold — the single-edit V4 fold is
verified clean for CH1. **CH1 CORRECTNESS remains converged.**

## §4 — New defects

None. The V4 fold introduced no defect; the V3 carried-forward LOW
REVISE is fully closed. No CH1 finding carries forward from this
cycle.

End of report.
