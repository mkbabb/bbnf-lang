# SK-V8 W0 Hardening V4 CH6

Date: 2026-05-18.

## Verdict

ACCEPT.

Confidence: 96%.

W0 V4 is not a CH6 paper close. The current target `077aadad` has executable
gate evidence for the committed `SK-V8-open` report, the same evidence replays
from a copied target root, stale/incoherent evidence fails before report
acceptance, the sidecar contract now matches the implementation, and the packet
does not allow W0 to paper-unlock W1-W6. This is only a CH6 disposition for V4;
it does not by itself satisfy ORCHESTRATOR two-cycle convergence.

## Scope

Lens: end-to-end anti-paper-close evidence for SK-V8 W0 after the V3 rejection
fold. I reviewed the live packet docs, ORCHESTRATOR CH6/convergence rules, the
W0 report/gate implementation, the committed `skinny/RESULTS.md` manifest, and
the executable gate behavior against clean/copy target roots.

Reviewed target: `077aadad8aacf95e3250ec157f30ba6ab873bf6b`
(`fix(sk-v8-wave0): fold hardening V3 gate blockers`).

## Evidence

- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results` from `skinny/`: PASS.
- Copied `/tmp/skv8-w0-target` to `/tmp/skv8-ch6-copy-target.2X0Msm`, then ran `CARGO_TARGET_DIR=/tmp/skv8-ch6-copy-target.2X0Msm RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results` from `skinny/`: PASS. This proves the W0 run id and checked report are not absolute-path bound.
- Mutated the copied canada SIMD metadata parity hash, then reran the copied-root gate: expected FAIL with `canada SIMD metadata invalid: SIMD metadata parity hash does not match scalar scan`.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench`: PASS, 51 library tests and 8 gate-bin tests.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-test-target RUSTFLAGS='-C target-cpu=native' cargo xtask check-json`: PASS.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-test-target RUSTFLAGS='-C target-cpu=native' cargo xtask check-real-typed`: PASS.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-test-target RUSTFLAGS='-C target-cpu=native' cargo xtask check-conformance`: PASS, 21 valid fixtures accepted and 7 invalid fixtures rejected.
- `git diff --quiet 0bd16f6d..077aadad -- <Lock-14 frozen roots>`: PASS. The Criterion metadata was captured at `0bd16f6d`, but no parser, scanner, SIMD, codegen, generated output, product, parity, materialization, fixture, or schema frozen root changed between that capture commit and `077aadad`.

## Findings

1. No blocker: W0 report evidence is gate-consumed, row-complete, and replayable from a copied target root.

   `skinny/RESULTS.md` contains the W0 manifest section (`skinny/RESULTS.md:44`)
   with 38 main rows and 38 manifest rows; the first manifest row carries
   `wave=SK-V8-open`, a content fingerprint run id, profile artifact, sample
   cost, host/build/feature metadata, CostFacts placeholder, substrate facts,
   gate-only consumer, Track 2 independence, native comparator evidence, and
   historical/absent sidecar evidence (`skinny/RESULTS.md:46`,
   `skinny/RESULTS.md:48`). The report validator enforces exact baseline row
   count, unique known row ids, required row presence, and +/-1.0% Track 1/Track
   2 drift from `SK_V8_OPEN_BASELINE` (`skinny/crates/bbnf-bench/src/report.rs:493`,
   `skinny/crates/bbnf-bench/src/report.rs:501`,
   `skinny/crates/bbnf-bench/src/report.rs:508`,
   `skinny/crates/bbnf-bench/src/report.rs:511`,
   `skinny/crates/bbnf-bench/src/report.rs:850`).

2. No blocker: stale or incoherent Criterion/SIMD evidence fails closed before report acceptance.

   The gate validates Criterion metadata before constructing rows
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:51`) and requires input hash,
   input byte count, capture coherence, and required benchmark specs
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1054`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1072`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1084`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1086`). SIMD metadata is read
   fallibly and checked for fixture identity, capture policy, semantic shape, and
   scalar parity hash (`skinny/crates/bbnf-bench/src/bin/gate.rs:1353`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1364`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1419`). The copied-root parity-hash
   mutation reproduced this rejection.

3. No blocker: sidecar manifest wording and implementation now align.

   SPEC says W0 has no structured same-run sidecar manifest and any
   `sidecar-same-run` claim rejects until a later accepted wave adds a parser and
   gate (`restart/skinny/tranches/sk-v8/SPEC.md:73`,
   `restart/skinny/tranches/sk-v8/SPEC.md:77`). W0 exit wording now says
   populated sidecars are historical non-manifest planning signals and
   `sidecar-same-run` rejects without a structured manifest
   (`restart/skinny/tranches/sk-v8/SPEC.md:328`,
   `restart/skinny/tranches/sk-v8/SPEC.md:331`). The validator enforces this:
   sidecars require DOM/strict/freshness-source coherence and reject
   `sidecar-same-run` (`skinny/crates/bbnf-bench/src/report.rs:1101`,
   `skinny/crates/bbnf-bench/src/report.rs:1119`,
   `skinny/crates/bbnf-bench/src/report.rs:1125`), while strict admission only
   accepts same-run native strict anchors with `sidecar_freshness=n/a`
   (`skinny/crates/bbnf-bench/src/gate.rs:135`,
   `skinny/crates/bbnf-bench/src/gate.rs:172`).

4. No blocker: W0 closure does not paper-unlock W1-W6.

   ORCHESTRATOR requires two consecutive >=95% ACCEPT challenge cycles with zero
   open critical defects or unresolved REVISE before advancement
   (`restart/prompts/ORCHESTRATOR.md:118`) and says the next pass does not
   dispatch until convergence (`restart/prompts/ORCHESTRATOR.md:123`). The live
   packet says current authority covers W0 only and W1-W6 remain blocked until
   W0 closes plus exact owner paths, row gates, challenge acceptance, and
   orchestrator/user dispatch (`restart/skinny/tranches/sk-v8/SPEC.md:31`,
   `restart/skinny/tranches/sk-v8/SPEC.md:36`;
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:90`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:94`;
   `restart/skinny/tranches/sk-v8/HANDOFF.md:225`). A V4 ACCEPT cycle can only
   be the first qualifying post-V3 cycle; it cannot close W0 alone.

5. No blocker: no-behavior-change evidence is executable enough for CH6.

   W0 exit forbids parser, scanner, SIMD, asm, codegen behavior, product-plane
   behavior, or generated parser output changes (`restart/skinny/tranches/sk-v8/SPEC.md:333`;
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:85`). The gate invokes Lock
   14 validation before report generation (`skinny/crates/bbnf-bench/src/bin/gate.rs:41`).
   Lock 14 freezes the relevant grammar, runtime, IR, passes, codegen, SIMD,
   generated-output, Track 2, parity, scan, and materialization roots
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`) and checks both dirty
   state and parent diff for those roots (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`).
   I also checked those frozen roots from the metadata capture commit through
   `077aadad`; there was no diff.

## Required Disposition If Rejected

Not applicable. No CH6 material blocker found.

## Residual Risks

- The Criterion metadata records `bbnf_commit=0bd16f6d`, not `077aadad`. I do
  not treat this as a blocker because the intervening changes are gate/report,
  docs, hardening artifacts, and xtask flag plumbing, and the Lock 14 frozen
  behavior roots have no diff across that range. A future W0/W1 gate would be
  stronger if it recorded either the measured behavior commit plus frozen-root
  no-diff proof in the manifest, or a current-HEAD capture id.
- This ACCEPT does not close W0. After the V3 rejection, W0 still needs the
  full V4 cohort disposition and a subsequent qualifying ACCEPT cycle under
  ORCHESTRATOR §3Z before W1 can be considered.
