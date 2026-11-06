# CRIT-P3 — Critique of PROTO-P3 (edition-2024 + pinned-stable whole-repo build)

Critique agent, SK-V21 pass-1. Adversarial review of `prototypes/PROTO-P3.md` against the question it
was assigned in `SPEC-DRAFT-P1.md §3` (the stack abrogation, TW-S) and its coupling to §7.3.
Date 2026-07-05.

## What the prototype gets right (evidence quality is high)

The method is honest by the tranche's own discipline. sccache disabled (`RUSTC_WRAPPER=""`) for cold
walls; fresh `CARGO_TARGET_DIR` per toolchain; `/usr/bin/time -l`; both build legs pinned to
edition 2024 to isolate the toolchain variable. Every wall figure carries a loadavg label
(stable 5.5→6.7 vs nightly 18.1→23.7), and the report explicitly disowns the wall delta as
load-confounded, pivoting to user-CPU (49.3 s vs 60.0 s) as the load-insensitive proxy. It corrects
its own charter (four `unsafe extern` wraps, not three; only stable 1.85.0 pre-installed, not the
charter's 1.96.1). It withholds the cold-bench numbers rather than publish them under loadavg 15-18.
This is high-integrity work; the critique below is about the verdict's *significance* and the spec
implications, not about fabrication.

Two primary facts are verified independently and stand:
- The smallbox nightly enable site is real: `../parse-that/rust/parse_that/Cargo.toml:97` —
  `smallbox = { version = "0.8", features = ["nightly"] }`. Root patches `parse_that` in via
  `.cargo/config.toml:51`. The `E0554` on stable is genuine.
- Skinny is `parse_that`-independent today: it consumes its own `skinny/crates/parse-that-regex`
  (via `runtime/Cargo.toml:11`, `bbnf-bench/Cargo.toml:17`), whose `[dependencies]` carry no
  smallbox and no `parse_that`. This is exactly why skinny builds clean on stable.

## The headline coupling claim is unproven and probably wrong (P3 caveat 2)

The prototype's most consequential spec recommendation — add "parse-that de-nightly'd first" as a
§3 close-gate precondition, because §7.3's regex-unification "would re-import this exact nightly
surface into skinny" (PROTO-P3:96-102) — rests on conflating two distinct crates in the parse-that
repo. §7.3 deletes skinny's 322-line facade and routes skinny onto the canonical **regex** engine,
"ONE regex home … the canonical engine via the same path patch" (SPEC:250) — that is
`bbnf-regex = { path = "../parse-that/rust/regex" }` (`.cargo/config.toml:52`), NOT the `parse_that`
parser. The regex crate's `[dependencies]` (`../parse-that/rust/regex/Cargo.toml:17-36`) are
smallvec, serde, proc-macro2, rustc-hash, egraph, egraph-derive, csp-solver — **no smallbox, no
parse_that**. The smallbox `nightly` feature lives uniquely in the `parse_that` *parser*. §7.3 swaps
the regex engine, not the parser, so on the direct dependency chain it would NOT drag the smallbox
nightly surface into skinny. The prototype's "§3 and §7.3 are coupled" thesis is PLAUSIBLE-at-best
and, on the Cargo.toml evidence, false. (I cannot run `cargo tree` to close whether egraph/csp-solver
pull smallbox transitively — NO cargo — so this is stated as a strong challenge, not a proof.)

## The root-workspace verdict tests a possibly-out-of-scope tree

The prototype takes §3's "whole repo (both workspaces) builds on the pinned stable" (SPEC:128) at
face value and declares it FALSIFIED because the root `crates/` tree fails (`bbnf` core → `parse_that`
→ smallbox). But the root `crates/` tree is the **legacy pre-restart tree** — SPEC:3-4 records that
"cutover commit 0 was never executed," i.e. the whole SK-V20/V21 arc is a cutover *to* skinny. If the
root tree is slated for abrogation/retirement in the cutover, its stable-incompatibility is **moot**,
not a blocker — you delete it, you do not build it on stable. The prototype never asks whether the
root workspace is in TW-S scope. This is the real open question the prototype surfaced without naming:
either §3's close gate over-claims (it should not require the abrogated legacy tree to build), or the
root tree is genuinely in scope and the smallbox blocker needs a scheduled fix. The spec must decide;
the prototype's flat "MIXED / whole-repo falsified" bundles the two cases and reads more pessimistic
for skinny than the evidence warrants.

## Half the assigned question is unmeasured

P3's question was two-legged: does it build clean, AND "what are the cold-build / cold-bench deltas."
The build leg is answered well; the **cold-bench delta is not measured** (out of cap; loadavg 15-18
throughout), honestly withheld. §3:115 states "Prototype P3 executes and prices this before freeze" —
the bench-parity-within-2% pricing is deferred, not discharged. The directional argument (nightly
1.96.0 and stable 1.96.1 are the same LLVM generation, so runtime parity within noise expected) is an
argument, not a measurement, and rides to the TW0 quiet re-capture (SPEC:98, §11 TW0). The spec should
not treat TW-S as priced.

## Residual scope the prototype could not reach

By its own caveat 1, the root build died at the smallbox leaf, compiled early — `core` and the rest of
`crates/` were never reached, so whether deeper nightly surfaces hide behind it is unverified; the
enumerated fix list is therefore necessary-but-not-proven-sufficient. Separately, §3.4's dependency
modernization (nom 7→8, simd-json →0.17, thiserror 1→2, toml 0.8→1.x, cssparser 0.34→0.37) and §3.3's
`target-cpu` / `panic="abort"` bench-profile additions are **entirely unprototyped** — P3 tested the
toolchain switch on the *current* dependency set, and the §3 close gate depends on the modernized one.

## Spec over-claim assessment

§3's opening "every nightly feature the toolchain pin cites is dormant" (SPEC:105) is technically
accurate — the pin's *cited* `-Z`/cranelift surface is dormant — but the spec's *implication* (the
line 128 close gate) that stable therefore builds everything is not established, and for skinny it is
established by the prototype rather than by the spec's reasoning. The genuine spec defects are: (a) the
"3 unsafe extern wraps" count in §3.1 is wrong (four; sites enumerated in PROTO-P3:47-48); (b) the
close gate's "both workspaces" needs a scope decision; (c) §7.3:251 "the Rust tree bbnf consumes is
unaffected" collides with the prototype's proposed parse-that de-nightly fix only if the root tree is
in scope AND §7.3 actually routes onto the parser (it does not) — so the collision the prototype flags
is largely illusory once the regex-vs-parser distinction is drawn.

## Verdict on the prototype

Method: honest, cold-labeled, loadavg-disclosed — passes the discipline. Primary skinny finding: solid
and verified; skinny → edition-2024 + stable is mechanically trivial and clean. Secondary findings: the
root failure is real but its significance is overstated (parser/regex conflation in the §7.3 coupling;
unexamined root-tree scope), and the bench leg — half the question — is unmeasured. The verdict's
"MIXED" framing is defensible but its two headline spec recommendations (add a de-nightly precondition;
§3/§7.3 coupled) do not survive the crate-boundary check.

## Convergence: 58/100

The leg that TW-S most needs — skinny builds clean on pinned stable — is settled and is in fact more
settled than the prototype's MIXED verdict implies. Three real items keep this below 70: (1) resolve
whether the legacy root `crates/` workspace is in TW-S scope at all, which determines if the smallbox
blocker matters; (2) correct or retract the §7.3-coupling recommendation (the regex engine skinny is
routed onto is smallbox-free); (3) capture the cold-bench delta at the TW0 quiet session. None are
deep unknowns, but each requires a spec edit or a deferred measurement, so another pass is required.
