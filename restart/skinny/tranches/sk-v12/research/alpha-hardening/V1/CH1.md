# SK-V12 Pass Alpha Hardening V1 - CH1 Correctness

Date: 2026-05-20.

Scope: CH1 correctness review for the Pass Alpha SK-V11 -> SK-V12
re-bracket under `USER-PIN-W1-CSS-L4-SOTA.md`. This lens checks the
requirements in `ORCHESTRATOR.md` Section 3W and `PASS-ALPHA.md`
Section 3: resolving citations, measurable falsifiability gates, and
competitor deltas on the correct strictness/output plane.

Files reviewed:

- `restart/prompts/ORCHESTRATOR.md` Section 3W.
- `restart/prompts/pass-contracts/PASS-ALPHA.md` Section 3.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-A-results-extraction.md`
  through `alpha-F-contract-draft.md`.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` through REDRESS 120.

## Overall Disposition

REVISE.

The USER-PIN re-bracket is directionally correct and should not be
rejected. CSS L4 is now authoritative, the old Sheets-first plan is no
longer dispatch authority, JSON rows are guard/fixpoint evidence rather
than the close target, and union/ASM-gen are reopened only through new
measured material-differential attempts.

Revision is still required before CH1 can accept. Two defects are
load-bearing:

1. The CSS close threshold is not spelled consistently. The pin and the
   top-level contract require generated Track 1 to be strictly greater
   than `lightningcss_mbps + 1`, but alpha-B and alpha-E still use `>=`
   in gate-like text.
2. Several result-surface and guard-floor tables carry correct-looking
   numbers, but they do not cite resolving `RESULTS.md` rows, REDRESS
   entries, commit SHAs, or formula sources next to the claims. CH1's
   citation standard is per-claim, not only a source list at the top of
   the artifact.

## Critical Findings

1. REVISE: CSS close gate has an operator mismatch.

   Authority:

   - USER PIN D2 says the CSS L4 close target is generated CSS L4
     throughput that beats lightningcss, and the admission floor is
     `lightningcss_mbps + 1`
     (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:29`).
   - The user campaign close clause for this run states ADMIT as generated
     CSS L4 Track 1 `>` `lightningcss_mbps + 1`.
   - `SYNTHESIS.md` and `alpha-F-contract-draft.md` correctly use
     "greater than" / `>` in their close summaries
     (`SYNTHESIS.md:39`, `alpha-F-contract-draft.md:70`,
     `alpha-F-contract-draft.md:198`).

   Defect:

   - `alpha-B-competitor-deltas.md:38` states
     `css_l4_track1_mbps >= lightningcss_mbps + 1`.
   - `alpha-E-candidate-shortlist.md:102`, `:240`, and `:298` state
     generated CSS Track 1 `>= lightningcss_mbps + 1`.

   This is a CH1 correctness defect because equality at exactly
   `lightningcss_mbps + 1` would pass alpha-B/E wording but fail the
   campaign close as pinned. Revise those gate clauses to strict `>`, or
   define an integer-rounded floor that is provably equivalent to strict
   `> lightningcss_mbps + 1`.

2. REVISE: Numeric result surfaces need local resolving citations.

   The Alpha-A extraction appears consistent with `skinny/RESULTS.md` and
   REDRESS 119/120, but it does not cite enough local evidence beside the
   copied numbers:

   - Alpha-A's CSS-absence bullets cite no file:line evidence for the
     absence scan or the REDRESS rows they summarize
     (`alpha-A-results-extraction.md:54`).
   - Alpha-A's parse/direct/typed tables carry many Mbps, comparator, and
     outcome values without per-row `RESULTS.md` citations
     (`alpha-A-results-extraction.md:98`, `:125`, `:152`).
   - `SYNTHESIS.md:85` and `HANDOFF.md:28` summarize the same result
     surface without direct local citations to `RESULTS.md` or REDRESS
     120.

   The underlying evidence exists: `skinny/RESULTS.md` carries the JSON row
   table, and REDRESS 120 records the unchanged SK-V11 close state
   (`skinny/REDRESS.md:3529`). The revision is to attach compact citations
   to the result-surface summaries rather than relying on a source map.

3. REVISE: JSON guard floors are concrete but under-sourced.

   The packet gives concrete direct and typed guard floors:

   - Alpha-A direct/typed guard tables
     (`alpha-A-results-extraction.md:168`, `:177`).
   - Alpha-D banked guard rows (`alpha-D-validated-invalidated.md:63`,
     `:72`).
   - `SYNTHESIS.md:99` and `HANDOFF.md:49` carry the guard requirement.

   The numbers are plausible and match the carried SK-V11/SK-V12 guard
   surface, but the packet should cite the formula/source next to the
   tables. The resolving source is still SK-V11 SPEC Section 0.5 for the
   carried guard formulas (`restart/skinny/tranches/sk-v11/SPEC.md:147`)
   plus `skinny/RESULTS.md` row evidence and REDRESS 120. Without that
   citation, the guard floors read as asserted constants.

## Accepted Checks

### CSS L4 Authority And Sheets Demotion

ACCEPT.

No stale Sheets-first plan remains authoritative in the rebased Alpha
packet. The pin itself obsoletes commit `e24a7e01`
(`USER-PIN-W1-CSS-L4-SOTA.md:18`, `:123`). Alpha-A, Alpha-C, Alpha-D,
Alpha-E, Alpha-F, `SYNTHESIS.md`, and `HANDOFF.md` all carry CSS L4 as the
first target and Sheets/BBNF-self as post-CSS-redress fallbacks only.

### CSS Gate Measurability

ACCEPT with the operator revision above.

The gate is otherwise measurable. The packet requires:

- generated CSS L4 Track 1 throughput;
- same-corpus and same-output-plane lightningcss throughput;
- strict equality against an independent oracle or Track 2;
- fixture/input provenance and run/build/host fields;
- gate-consumed Lock 14, Lock 16 where applicable, JSON guard state, wave id,
  and REDRESS id.

Alpha-B identifies local lightningcss comparator sources
(`crates/core/benches/css/competitors.rs:153`,
`:166`, `:179`), parity-test shape
(`crates/core/tests/lightningcss_parity.rs:96`, `:111`, `:127`), and Rust
crate version (`Cargo.lock:1908`). Alpha-F and `SYNTHESIS.md` require those
facts to be consumed by the CSS report or `RESULTS.md` row before close.

### Strictness And Output Plane

ACCEPT.

Alpha-B correctly treats CSS/lightningcss as the close comparator and keeps
JSON sonic/serde deltas as guard/freshness facts only. It does not count
parse-only rows as SOTA admission, and it records absent C++ sidecars as
absent/unmeasured rather than wins (`alpha-B-competitor-deltas.md:116`,
`:174`).

### Union And ASM-Gen Fixpoint Requirements

ACCEPT.

The fixpoint route is measurable enough for Pass Alpha. Alpha-C and Alpha-D
preserve REDRESS 88/89/90 and 96/97/98 as historical specific rejects while
reopening the categories under the user pin. Alpha-F and `SYNTHESIS.md`
require, for any fixpoint close, at least one new measured union-substrate
attempt and one new measured ASM-gen attempt in the closing tranche, each with
material differential, fresh profile, microbench evidence, equality/parity or
checkasm evidence, and same-wave consumer evidence. That is falsifiable; a
future S-P3 SPEC still has to assign exact wave gate IDs and owner paths.

## Artifact Dispositions

| Artifact | CH1 disposition | Notes |
|---|---|---|
| `alpha-A-results-extraction.md` | REVISE | Correct pin framing and row extraction, but copied row tables and CSS absence claims need resolving citations near the claims. |
| `alpha-B-competitor-deltas.md` | REVISE | Comparator discipline is correct, but the CSS threshold code block uses `>=` where the pin requires strict `>`. |
| `alpha-C-redress-digest.md` | ACCEPT | REDRESS 111-120 treatment is aligned with the pin; CSS, union, and ASM-gen routes remain measurable and non-paper. |
| `alpha-D-validated-invalidated.md` | ACCEPT | Correctly invalidates Sheets-first, baseline-plus-1%, and category-level union/ASM-gen blocks while preserving historical rejects. |
| `alpha-E-candidate-shortlist.md` | REVISE | Candidate stack is correct, but E1/E4/E5 gate bullets use `>= lightningcss_mbps + 1`; change to the strict pinned comparator. |
| `alpha-F-contract-draft.md` | ACCEPT | Close and fixpoint contract is measurable and pin-aligned; add citations only if the orchestrator wants all summary docs to be self-resolving. |
| `SYNTHESIS.md` | REVISE | Goalset is correct, but current-result and guard-floor summaries need compact `RESULTS.md` / REDRESS / formula citations. |
| `HANDOFF.md` | REVISE | Dispatch boundary is correct; add citations for the carried result state and guard surface. |

## Required Revisions Before CH1 Accept

1. Replace every CSS gate occurrence of `>= lightningcss_mbps + 1` with the
   pinned strict comparator, or define an equivalent rounded integer rule.
2. Add local resolving citations to Alpha-A, `SYNTHESIS.md`, and `HANDOFF.md`
   result-surface summaries.
3. Cite the guard-floor formula/source next to every JSON guard-floor table,
   with enough `RESULTS.md`/REDRESS references to audit the numbers.
4. Keep Sheets/BBNF-self fallback-only and keep union/ASM-gen reopen language
   tied to material differential plus measurement; those parts should be
   preserved while revising.
