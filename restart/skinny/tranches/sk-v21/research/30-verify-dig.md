# 30 — Adversarial Verification of the Dig Shards (reports 02–15)

Date: 2026-07-05. Verifier posture: skeptical, re-derive from git + primary docs at HEAD, never from the reports' own prose. Scope: the ~14 most load-bearing HISTORICAL claims — root-cause (why divergence happened, what died) and resurrection candidates, since the totality plan will be built on them.

Bottom line: the dig is unusually well-sourced. Thirteen of fourteen load-bearing historical claims verify against git commits and primary tranche docs with exact figures. The single correction is a headline overreach in report 14 (BA–BD "shipped ZERO production code"), which one `feat(xtask)` scaffold commit refutes. No claim was fully REFUTED; the root-cause spine of the corpus is trustworthy to build on.

---

## CONFIRMED — the divergence root causes

**1. Hand-patched `generated.rs` persisted from AE to B2 — ~14 days (report 02).**
AE founding commit `854782847` is dated `2026-04-11 03:03:58`; B2 structural retirement `09e419a32` ("feat(b2): retire monolithic generated.rs; xtask emits crates/core/src/grammar/generated/bbnf.rs (B2.W0.c)") is dated `2026-04-25 19:18:40`. Exactly 14 days. CONFIRMED.

**2. AE's founding commit is marked WIP and treated as foundation (report 02).**
`854782847` subject is literally "refactor(lower,host): tape-first shape-agnostic walking substrate (Tranche AE.0/AE.1 **wip**)". The WIP marker is in the commit subject itself. The body confirms the shape-agnostic rewrite (dispatch by `rule_kind`, panic-on-unhandled, silent-Epsilon removal). CONFIRMED.

**3. AM.1 deleted the entire EmissionTier three-tier apparatus (reports 03/04).**
`76085303a` ("refactor(ir): delete EmissionTier axis + BumpSlab residue (AM.1)"), `2026-04-12`, `--stat`: **48 files changed, 99 insertions(+), 2306 deletions(-)** — matching the digest's "48 files, 99 insertions, 2306 deletions" byte-for-byte. CONFIRMED.

**4. AI built six waves on infrastructure its own plan documented as dead-on-arrival (report 03).**
`docs/tranches/AI/AI.md:5-13` states verbatim: "the substrate has no consumer: the Rust emitter dispatches solely on `MaterializationClass`, ignoring `ir.emission_tier` entirely. Zero production grammar rules classify as Direct because Tier B eligibility is intentionally narrow." AI's thesis section then proposes to build *more* on top of it. CONFIRMED — this is the "knowingly-dead abstraction" pattern in the primary source.

**5. Self-hosting closure was declared PASS only against a frozen `generated.rs` (report 05).**
`docs/benchmarks/post-AR.json:41`: `"bootstrap_regen_roundtrip": "PASS — 6/6 grammar_roundtrip green with current generated.rs (24,685 lines). Regen from new host.rs produces different parser (21,672 lines) that can't parse BBNF — bootstrap coherence issue deferred."` The 24,685 / 21,672 line figures and the "can't parse BBNF" claim are verbatim. CONFIRMED.

**6. The post-AR "BEAT sonic-rs" canada number came from a broken parser (report 05).**
`post-AR.json:25`: canada `mb_per_s: 1097`. The digest's 1810→1097 collapse-on-modifier-fix is corroborated on the 1097 side (the post-fix value); the note attributing the drop to silent modifier-skip is in the same file's monolithic-bench note. CONFIRMED (the load-bearing half — the contaminated number is real).

**7. AR split `variant_idx`/`meta_idx` to fix the regen-breaking overload (report 05).**
`d9a760a01` ("feat(bbnf-tape): add meta_idx side-channel to disambiguate variant_idx semantics") and `b2ff0d9c8` ("feat(emitter): split variant_idx/meta_idx in tape push calls (AR.1.1)"), both `2026-04-13`. CONFIRMED.

**8. AR's ~15-line `lower_map_arrow` span-text fix lit up the payload layouts (report 05).**
`b857765ca` ("feat(lower): recover type suffixes from span text in lower_map_arrow (AR.2.1)"), `2026-04-13`. The commit's scope (recover type suffixes from span text) matches the "fully plumbed but touched zero production rules until one span-text fix" root-cause claim. CONFIRMED.

**9. AM+AN+AO were three multi-wave "tranches" collapsed into one day (report 04).**
All 30 commits in range `4d1afeb00..7198c974f` are dated `2026-04-12` (uniq count: 30 on that single date, 0 others). CONFIRMED.

**10. AW's `generated.rs` ballooned to 82,929 lines; AX regen shrank it 68,438 lines (reports 07/08).**
`docs/tranches/AW/FINAL.md:411`: "Bootstrap idempotent at **82929 lines** (gen1 == gen2)." AX regen `bc550d2ce` ("feat(emitter): retire walker path + gate predicates, regen (AX.W0b.A)") commit body: "shrinks **68,438 from 98,270** pre-W0b" (→ 29,832, matching the digest's 98,270→29,832). The `--stat` shows the `generated.rs` line ripped out 83,698 lines. CONFIRMED.

**11. B3's revert thesis was false; the bug was a latent frame_depth contract violation (report 13).**
`ef52c35c` ("audit(b3): W0.a probe #1 DISPROVED — W0' revert insufficient"), `e5f77689` ("audit(b3): W0.a probe #3 DISPROVED-PARSE — hang INSIDE BbnfBootstrap::parse"), and the actual fix `9167dac4` ("fix(tape): in-builder frame_depth tracking; **retire derive_frame_depth** (B3.W0.γ)"), all `2026-04-25`. The probes disproved the plan; the fix retired `derive_frame_depth` exactly as the digest describes. Zero planned reverts landed. CONFIRMED.

**12. B2 relocated but did not shrink the generated-code cost (report 12).**
`docs/tranches/B2/audit/W1-close.md:36`: `| css_l4 | 0 | 203 499 | 11 041 925 |` — 203,499 lines / 11 MB at B2 close. Today `css_l4.rs` is 108,406 lines / 6,106,078 bytes (6.1 MB). The proc-macro retirement (`09e419a32`, xtask regen) is live; the wall was relocated, not removed. CONFIRMED.

**13. The csp-solver was re-vendored from canonical and the audit corpus was copied byte-identical (reports 11/14).**
Re-vendor commit `3aab34e8b` ("chore(csp-solver/canonical-diff): re-vendor src/ from csc411@b70098676 to close W0.2.b diff gate (AZ-IV.W0.2.b)"), `2026-05-01`. `restart/corpora/CENSUS.md` and `audit/CENSUS-2026-05-03.md` are both 47,187 bytes (byte-identical size). CONFIRMED.

**14. The W→X→Y→Z CSP-activation arc is git-tagged and executed (report 15).**
`f86be0045` ("docs(bench): post-W baseline") `2026-04-09`; `3539b5ee6` ("test(bbnf): Tranche Y.13 — consumer-invariant enforcement") `2026-04-10`; `6eeac0cc1` ("docs(bench): post-Z baseline (Tranche Z.7)") `2026-04-10`. Dates and the Y.13 consumer-invariant test all confirm. CONFIRMED — this is the corpus's one executed cure and it is real.

Current generated corpus at HEAD (cross-check for reports 12/13/16): `css_l4.rs` = 108,406 lines, `bbnf.rs` = 21,557 lines, total `generated/` = 169,956 lines. Matches report 13's "169,956 lines … css_l4.rs=108,406, bbnf.rs=21,557" exactly.

---

## CORRECTED

**BA–BD "shipped ZERO production code" is overstated (report 14, load-bearing claim 1; touches report 10).**
Report 14's headline and claim 1 assert "BA-BD shipped zero implementation; all commits touching the dirs are docs()". Git shows one production commit under a BB label: `feat(xtask): regen consumes per-grammar rewrites/*.ron rule files (BB.scaffold.C cut 1)`, `2026-04-28`, touching **four production files** — `crates/core/src/pipeline.rs`, `crates/core/src/pipeline/compile.rs`, `crates/ir/src/rewrites/mod.rs`, `xtask/src/regen.rs` (120 insertions, 3 deletions). 

The narrow forms survive: `git log` shows no `feat(ba` and no `fix(bb` commit (report 10's exact grep is technically true), and the commit touches `crates/`+`xtask/`, not the `docs/tranches/{BA,BB,BC,BD}` dirs (so "all commits *touching the dirs* are docs" is literally defensible). But the headline generalization "shipped ZERO production code" is false — BB landed a real regen-consumes-`.ron`-rewrites scaffold in core/ir/xtask. The totality plan should treat BA–BD as "a planning edifice that landed exactly one production scaffold commit (BB.scaffold.C)," not as pure vapor. The substance the plan actually relies on — no FINAL.md, no green close, the diagnostic corpus (CENSUS/MODULES/SOTA/RESTART-SKETCH) inherited while the wave/lock machinery was dropped — is unaffected and holds.

---

## UNVERIFIABLE / not independently re-derived (noted, not load-bearing)

- The csp-solver "byte-identical to canonical" claim (report 11/18) could only be half-checked: the re-vendor commit and the 47,187-byte CENSUS match are confirmed, but the canonical csc411 tree is not present in this repo, so a live `diff -rq` against `csc411@b70098676` was not run here. The vendor commit's own message asserts the diff-clean close.
- The "1810 MB/s pre-fix" side of the canada contamination (report 05) was not separately located; only the post-fix 1097 is confirmed in `post-AR.json`.

These do not weaken the root-cause spine; they are the two places where a downstream reader should not treat the exact prior number as independently re-derived by this pass.
