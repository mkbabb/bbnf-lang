# CRIT-freeze-confirm — the un-read-surface adversarial pass

Date: 2026-07-09. Scope: the ONE surface the agglomerator flagged un-read at the 91-close — the
Rev-2 rewritten sections (SPEC-DRAFT §3.2(B), §3.2(C), and the §3.4 vendor-mirror / cross-repo
adjudication) — read adversarially against their cited evidence (research/02, 03, 05) and against
the rest of the draft + the SK-V21 settled facts. Plus the three highest-consequence orderings and
the H1 framing. Verdict up front: **zero blocking issues; every residual is wave-owned; freeze.**

---

## 1. §3.2(B) — one-regex-home MECHANIZED (draft:272-285) — COHERENT, evidence-cited

- Canonical engine keeps name+home (`parse-that/rust/regex`, crate `bbnf-regex`, edition 2024):
  confirmed 03:19-22 (manifest) and 05:186 (6,742 LOC). ✓
- Vendor mirror at `crates-infra/bbnf-regex` + `sync-bbnf-regex-vendor.sh` = the ratified
  W0.5/csp-solver mechanism: the mechanism's prior ratification is real — 02:155 (csp-solver
  "PORT-IN (vendor)... the csc411 re-vendor checkpoint (SPEC W0.5) governs") and 03:46 ("the
  graduated tree mirrors it exactly as skinny already does"). Instantiating it for the engine is a
  legitimate re-use, not a novel invention. ✓
- "No published pin is required (03 §1A)": 03 §1A (17-53) shows the engine resolved only by the
  gitignored `.cargo/config.toml` path patch, never a registry — so no pin exists to take. 05:240
  offers "published version pin OR workspace-relative path"; the vendor-mirror path satisfies the
  latter disjunct. Coherent. ✓
- The apparent `graduated-egraph ← parse-that-regex ← graduated-runtime` cycle dissolves into two
  acyclic graphs + a sync protocol: verified by construction — the graduated runtime consumes the
  vendored COPY (crates-infra/bbnf-regex → egraph workspace member: acyclic in-tree), while
  parse-that resolves egraph via its repointed patch table (its one cross-repo edge). The vendor is
  a copy, so no reverse edge closes the loop. ✓
- Recognizer folds UPSTREAM, local crate deletes at GW2b: consistent with 05:188-191 (322-LOC
  functional family recognizer — H6 already corrected "stub"→"recognizer") and with PT-R.4
  (draft:323-332). ✓ Internally consistent with the bidirectional fold-back of 05:199.

No new contradiction. The "zero path-deps escaping the workspace" GW0 gate (draft:119) holds
verbatim because the mirror is in-tree (draft:277-279). ✓

## 2. §3.2(C) — bbnf-simd resident, parse-that gains NO dependency (draft:287-294) — COHERENT

This is the adjudication of 05's open question (C) (05:315-317), and it makes a MORE careful call
than 05 §0's loose phrasing "adopt bbnf-simd as the SIMD façade" (05:42) / RU-1's "route all
std::simd through the bbnf-simd façade" (05:268-270). The draft instead has the {ws,quoted,number}
family scanners transpose UPSTREAM as engine-owned CODE while `bbnf-simd` (the crate) stays resident
in the graduated tree and parse-that takes NO dependency on it. This is not a contradiction — it is
the correct resolution of (C): a parse-that→bbnf-simd dependency would re-introduce the very
reverse cross-repo edge §3.2(B) exists to forbid. The draft is internally consistent on this:
PT-R.2 says "core::arch::aarch64 intrinsics **on the bbnf-simd pattern**" (draft:310, emphasis the
pattern, not the crate) and §3c R-1 says "stable core::arch re-founding **on the bbnf-simd
pattern**" (draft:401). PT-R.2's gate (draft:314-315) requires no bbnf-simd dependency. ✓
- `eisel_lemire` one-home = the engine, reaching bbnf via the vendor: 05:211-214 places the
  duplicated table under `parse_that/src/parsers/` today; the draft's PT-R.4 (draft:327) decides it
  homes in the engine post-fold. A real transposition PT-R.4 owns; coherent with the fold-back. ✓
- aarch64/Apple-M-only, no x86 SIMD work: matches SK-V21 SPEC §0 (cited draft:293). ✓

## 3. §3.4 — cross-repo sequencing (draft:376-389) — COHERENT; one citation-precision nuance

- "bbnf gates on PT-R.5 before GW0.c removes crates/": consistent across §3.1 (draft:238-241),
  §2.1 step 4 (draft:187-188), PT-R.5's own "Blocks GW0.c's crates/ removal" (draft:338), and
  05:236-244 / 07 R-7. ✓
- "GW3 ↔ PT companion commit for the float-tax fix": consistent with §1.2 TW4.1 (draft:69). ✓
- **Nuance (non-blocking):** the parenthetical "the engine crate itself carries no `#![feature]`
  and is already edition 2024, 03:19-22" attaches citation 03:19-22, which supports the edition-2024
  half but NOT the no-`#![feature]` half. The no-`#![feature]` claim is really carried by 05's
  granular audit, which localizes ALL four nightly gates to the COMBINATOR crate —
  `rust/parse_that/src/lib.rs:1-3` (three `#![feature]`) + `parse_that/Cargo.toml:97` (smallbox) —
  and audits the engine (05 §3) without finding any (05:14-33). 03:58's looser statement ("the
  graduated tree must carry ... this nightly pin for the regex crate to compile") reads against the
  draft's claim, but 03 attributes that pin to build-speed toolchain features (cranelift /
  parallel-frontend, 03:55-56), i.e. the shared repo-wide toolchain file, not a per-crate language
  requirement — so 05's per-crate audit is the stronger, non-contradicting evidence. Critically, the
  claim is DEFENSIVELY GATED: GW2b's close requires "the canonical engine builds on the graduated
  stable toolchain" (draft:143-145) as a GATE — if the engine turned out to need nightly, GW2b
  fails and PT-R.2's de-nightly scope extends. This is a wave-owned execution proof, not an
  assertion. Residual: tighten the citation to 05 §0/§3 at authoring time. NOT a blocker.

## 4. The three highest-consequence orderings — all UNAMBIGUOUS

- **GW0.b → PT-R.5 → GW0.c** (both repoints before any crates/ removal): stated as "the single
  highest-consequence ordering constraint" with the brick-two-trees rationale (draft:238-241);
  echoed at §2.1 step 4 (draft:187-188), §3.4 (draft:377-378), PT-R.5 (draft:338). GW0.b's close
  "explicitly verifies both repoints before any removal" (draft:241). Consistent four ways. ✓
- **GW0 → GW1 → GW2, single-variable capture rationale**: wave order draft:30-31; §1.4 point 2
  (draft:103-107) is explicit — walls (GW2a) run on the FINAL toolchain so the paired regression
  capture has "ONE variable (the wall fix), not two (wall fix + toolchain)," naming the A3
  nightly-vs-stable channel confound (SPEC.md:31-32) as the recorded cost "we do not pay twice."
  Points 1 (pure-move parity while floor freshest) and 3 (edition cheap-first) do not compete with
  this — all three converge on move→toolchain→walls. Unambiguous. ✓
- **Tags before removals**: §2.1 is titled "tags first, then removals — order is normative"
  (draft:169); step 1 tag set "before ANY removal — 01 §9" (draft:171); removals are step 4
  (draft:187); the GW0 gate verifies "the full tag set exists and each named tag resolves"
  (draft:124-125). Unambiguous. ✓

## 5. H1 paired re-capture — framed as a wave-owned execution gate (GW0.e), NOT an open question

The §4 PERFORMANCE-THESIS row makes the DESIGN decision fully (anchor to 04 alone; carry the
+36% sonic-side baseline-drift disclosure; never cite 33:59 for a 2.10× claim) and converts the
lone empirical unknown into a named **PRECONDITION (gates GW0.e, §1.5)**: "one paired twitter
sonic/parse_tape re-capture per §5 adjudicates 2.10× vs 1.59×" (draft:427). The GW0 close gate
carries it inside the wave: "the PERFORMANCE-THESIS precondition (the H1 twitter-sonic
reconciliation capture, §4) has run" (draft:128-129). The ledger residual 1 names owner (GW0.e
precondition) and method (§5). This is a wave-owned execution item with the design decided — not an
open design question. ✓

## 6. No new contradiction with the rest of the draft or SK-V21 settled facts

Cross-checks all clean: the vendor-mirror decision preserves the GW0 "zero escaping path-deps" gate
(mirror is in-tree); egraph's home is `crates-infra/egraph` uniformly (draft:83, 216, 234-238, 336);
csp-solver vendors at `crates-infra/csp-solver` with a csc411 sync script (draft:84) so the
graduated tree carries "NO build-time cross-repo edge at any point" (draft:383); aarch64-only binds
bbnf while parse-that's platform envelope is its own call (draft:293-294, matching Charter Finding
D). The §3.2 consumer-map premise ("no Rust-combinator consumer survives") matches 05 §2's table
(05:113-126). ✓

---

## Verdict

Zero blocking issues. §3.2(B)/(C) and §3.4 are coherent, evidence-cited, and introduce no new
contradiction; the three orderings are unambiguous; H1 is a wave-owned GW0.e execution gate. The
single residual — a citation-precision tightening on the engine's no-`#![feature]` claim — is
already defensively gated by GW2b and is authoring polish, not a design gap. By the freeze
criterion (every residual a wave-owned execution item, zero open design questions) with the
anti-asymptote rule (zero blocking + all residuals wave-owned = 100): **score 100. FREEZE.**
