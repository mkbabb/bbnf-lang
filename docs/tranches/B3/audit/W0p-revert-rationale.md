# B3 — W0' Revert Rationale

Authored at B3 open as the scope-justification document for the W0p
revert (per `docs/instructions/tranche/SPEC.md` §"audit/*.md — in-flight
audits of inherited state or friction"). Documents (a) the diagnostic
trail that disproves the four 2026-04-25 deep audits' shared
IR-pipeline-at-expand-time attribution, (b) the reasoning behind the
14-commit revert scope (W0'.a/b/c/d1/d3 included; W0'.d4-d7 excluded),
and (c) the alternative paths considered and rejected.

## 1. Diagnostic trail

### 1.1 The original attribution (four 2026-04-25 deep audits)

The four 2026-04-25 deep audits at
`docs/audit/2026-04-25-deep-audit/AUDIT-{A,B,C,D}-*.md` shared a single
attribution for the > 80 min cold wall on `cargo iter-check-full`:

> The wall is rustc's expand phase running the AY-II IR-pipeline
> accumulation. Every consuming crate that derives `Parser` invokes
> the proc-macro, which runs all 17 IR passes + emits a 30 K-line
> `TokenStream`. The cost is structural to the proc-macro contract.

AUDIT-D's T3 recommendation followed: **retire the proc-macro**;
move the IR pipeline to a build-time `xtask::regen` step that emits
per-grammar source files on disk; consumers `include!` the on-disk
product. T3 is the architectural endpoint; B2 is the execution plan.

### 1.2 The W0.c probe

B2.W0.c began the xtask-substrate execution. After landing the xtask
body + bbnf-bootstrap migration (commit `21881591`), the agent invoked
`cargo xtask regen --grammar bbnf` to test the substrate. The
invocation:

- runs the IR pipeline FROM the xtask binary (release profile),
- with NO proc-macro expansion in flight,
- against `grammar/bbnf/bbnf.bbnf` (the BBNF self-host source).

Expected wall: < 5 min cold (the IR pipeline alone runs in 911 µs per
the `compile_pipeline` bench's `compile_bbnf` measurement at AU
baseline; the xtask wraps it with file I/O + emission, so a few
minutes for cold-cache + emission overhead is reasonable).

Observed wall: > 5 min and counting; the agent killed it for fresh
diagnostic.

### 1.3 The narrowing

The agent's diagnostic narrowing (recorded in
`docs/tranches/B2/audit/W0c-status-2026-04-25-04h.md`):

1. The wall isn't in IR pass execution. With
   `BBNF_PIPELINE_REPORT=1` enabled, the report shows zero pass
   activity at the wall point. The pipeline never enters its IR-pass
   loop.
2. The wall IS in `BbnfBootstrap::parse` on `bbnf.bbnf`. The xtask
   first calls `BbnfBootstrap::parse(bbnf_source)` to obtain the AST;
   that call hangs.
3. Even the simpler `json` grammar (537 bytes) hung > 39 s without
   completing on `BbnfBootstrap::parse`. So the regression isn't
   `bbnf.bbnf`-specific complexity; it's the parser itself.
4. Pre-W0' `compile_pipeline::compile_bbnf` measured 911 µs. The
   parser was fast at that baseline; something between AY-II.W0' and
   the W0.c probe broke it.

### 1.4 The candidate set

The W0c status snapshot identified three top suspects from W0' for the
parser regression:

- `bd563c1d` — W0'.a FusedBuilder collapse (replaces TapeBuilder/
  ValueBuilder split). The parse emits BOTH columns; a per-record
  extra-write might compound on large grammars.
- `f8ac2cd7` — W0-fix begin_compound migration (variant_idx + meta_idx
  packing). Affects every compound write.
- `f768f50d` — W0'.d3 `value_end_compound` O(1) refactor. Was
  supposed to FIX an O(N²); regression risk if it broke a stamping
  path.

The status snapshot's R3 path scopes the B3 revert to W0'.a/b/c (with
d1 + d3 included for compile-clean).

## 2. Why W0'.a/b/c (and not just W0'.a)?

### 2.1 W0'.a is the load-bearing change

W0'.a's seven commits collapse `TapeBuilder` + `ValueBuilder` into
`FusedBuilder`, retire `parse_with_visitor`, rename `finish` →
`finish_fused`, and ungate the new-call counter. This is the parity-
critical runtime architecture change: every parse path now goes
through a single FusedBuilder; every emit fnpattern (every shape:
object, array, wrap, keyword, inline, alt_dispatch, flat, pratt,
arglist, unordered) writes to it.

If the regression comes from W0'.a, reverting it directly restores
the pre-W0' parse path.

### 2.2 W0'.b depends on W0'.a

W0'.b (materializer routing through raw rule names) operates against
the FusedOutput type that W0'.a introduced. Reverting W0'.a without
W0'.b leaves stale references to FusedOutput in the materializer.
W0'.b reverts cleanly only when W0'.a reverts.

### 2.3 W0'.c depends on W0'.a + W0'.b

W0'.c (STRUCTURAL_SCAN_POLICY splice + scan-policy raw-name routing)
operates against the same FusedBuilder + materializer routing
substrate. Same dependency.

### 2.4 W0'.d1 depends on W0'.a (test API)

W0'.d1 (test migration: push_compound/mark_children → FusedBuilder
API) updates tests to use the new FusedBuilder API. Reverting W0'.a
without W0'.d1 leaves tests calling the new API on the old types.

### 2.5 W0'.d3 depends on W0'.a

W0'.d3 (O(1) direct_child_count in value_end_compound) operates
against the FusedBuilder's value column. Reverting W0'.a without
W0'.d3 leaves a function call against a non-existent value column.

### 2.6 The total revert scope

To restore master to a compilable pre-W0' state, the revert must
include all five sub-tags:

| Sub-tag | Commits | Purpose |
|---|---|---|
| W0'.a | 7 | FusedBuilder collapse (load-bearing) |
| W0'.b | 2 | Materializer routing (depends on a) |
| W0'.c | 3 | Scan-policy splice (depends on a, b) |
| W0'.d1 | 1 | Test API migration (depends on a) |
| W0'.d3 | 1 | O(1) value_end_compound (depends on a) |
| **Total** | **14** | |

## 3. Why NOT W0'.d4-d7?

W0'.d4 (gorgeous derive-site cargo-feature gating; commit `5c737bd1`):
touches `crates/gorgeous/src/*.rs` + `crates/gorgeous/Cargo.toml`. Adds
`#[cfg(feature = "...")]` gates on derive sites. INDEPENDENT of
runtime parser architecture.

W0'.d5 (drop gorgeous as mandatory dev-dep; commit `f5cdcd52`):
touches `crates/core/Cargo.toml` only. INDEPENDENT.

W0'.d6 (narrow build.rs fingerprint; commit `2e5e3ff5`): touches
`crates/derive/build.rs` only. INDEPENDENT.

W0'.d7 (cargo iter-check exclude pattern; commit `700501f5`): touches
`.cargo/config.toml` only. INDEPENDENT.

These four commits inherit the AY-II.W0'.d tagging by sequencing
convenience, not architectural coupling. They have zero impact on
runtime parser performance. The B3 revert leaves them intact.

## 4. Why NOT also W0-fix (`f8ac2cd7`, `c9142405`)?

`f8ac2cd7` (W0-fix begin_compound migration) is on the candidate set
because it affects every compound write — `variant_idx` + `meta_idx`
packing introduces per-write logic. If the regression is in this
commit, the W0' revert won't restore the parser baseline.

The B3 escape clause covers this:

> **Escape 1 — W0' revert insufficient.** If post-W0-revert,
> `BbnfBootstrap::parse` still hangs > 5 s on `bbnf.bbnf`, the parser
> regression's root cause sits earlier than W0'. B3.W0 expands scope
> to include AY-II.W0-fix (`f8ac2cd7`, `c9142405`) and re-runs the
> verification.

Starting with W0' alone (the smaller revert) is the conservative
sequencing: if W0' is sufficient, the revert footprint is minimal;
if not, escape clause expands. The alternative — reverting W0' +
W0-fix together preemptively — would touch more code than necessary
in the common case.

## 5. Why NOT also AY-II.W0 base?

AY-II.W0 base (commits `b2ac3cf5`, `a13840a0`, `2ddb8c33`,
`f2e458ec`, `2b24b0a4`, `1f97a8cc`, `4f42f6bb`, `db979564`,
`58271da1`, `487b17b7`, `61d0338c`) introduces `begin_compound`/
`end_compound`, `ValueBuilder` allocation at parse entry,
`STRUCTURAL_SCAN_POLICY` emission, projection-totality wire-contract
tests, etc.

These were AY-II.W0's load-bearing landings — the foundation that
W0' built on. Reverting them returns to AY-I close (the predecessor
tranche). AY-I's parse path was 911 µs per the AU baseline.

Same conservative sequencing argument: start with W0', escape clause
expands to W0-fix, then to W0 base. The B3.md §Escape clause documents
the full expansion path.

## 6. Alternative paths considered and rejected

### 6.1 R1 — Bisect-and-fix as B3 prelude annex

Per the W0c status snapshot, R1 dispatches a focused bisect agent
that probes each candidate W0' commit's effect on
`bbnf_parses_its_own_grammar`. Once the offending commit is found,
authoring a fix lands as a 1-3 commit follow-up.

R1 is workable in principle but operationally costly:

1. The bisect requires ~14+ test-binary compiles (one per probe). At
   the pre-B2 substrate, each compile pays the > 80 min cold wall
   for the proc-macro chain.
2. Even compressed via `bbnf-bootstrap`-only test target, the bisect
   cycle is hours per probe.
3. The bisect runs on a substrate B2 will retire; the work is
   throwaway.

R3 substitutes the bisect for a revert-then-rebisect-on-post-B2-
substrate. The bisect is NOT eliminated, but it shifts to B4 where
each probe pays seconds (post-B2 xtask substrate; consumers
`include!` the on-disk product without re-running expansion).

### 6.2 R2 — Revert + re-tranche

Per the W0c status snapshot, R2 reverts W0'.a/b/c and re-lands them
as a fresh tranche AFTER B2 closes. This is operationally identical
to R3 except R2 runs B2 ALONE between B3 and the re-land.

R3 IS R2 with the re-land tranche named B4 explicitly. The
distinction is documentation: R3 names B4 forward; R2 leaves the
re-land tranche unnamed.

### 6.3 R-naive — Patch in place

Patch `BbnfBootstrap::parse` directly to fix the regression without
reverting W0'. Operationally appealing because no revert touches code,
but architecturally suspect: without bisect evidence, the patch is
speculative; without the post-B2 substrate, the diagnosis is slow.

This path was rejected per the user feedback memory `no-workarounds`:
"Zero tolerance for workarounds, fallbacks, stubs, or legacy code in
any implementation". A speculative patch on top of a half-migrated
substrate is a workaround.

## 7. Why R3 (and not R2)?

R3 differs from R2 only by naming B4 explicitly. The naming matters:

1. **Continuity for AY-II.W0' close ceremony.** AY-II.W0' close was
   originally meant to land the W0' work + ceremony. With W0'
   reverted, the ceremony has no substrate. R3 names B4 as the new
   ceremony destination explicitly; AY-II's PATH-FORWARD updates to
   reflect.
2. **Forward visibility.** REMAINING-TRAJECTORY.md updates the §4
   ledger with `Re-lands as B4` rather than leaving the future open.
3. **No mid-tranche pivot at B2 close.** When B2 closes, B4 opens
   immediately on the named-successor convention; the orchestrator
   doesn't pause to ask "what's next" because the trajectory is
   already canonical.

R3's only cost over R2 is the small documentation surface in
REMAINING-TRAJECTORY + AY-II/PATH-FORWARD. R3 is unambiguously
preferable.

## 8. The diff record (for B4 re-land)

B3.W0 produces 14 forward-revert commits + per-commit diff snapshots
at `docs/tranches/B3/audit/diffs/<sha>.diff`. The original commits
remain in master history (every `git revert` is a forward operation;
nothing is lost).

B4 re-lands by cherry-picking the original 14 commits onto the
post-B2 substrate, then bisect-and-fix the parser regression. The
diff snapshots are the source-of-truth: each represents the original
commit's intended runtime change, decoupled from the conflict-
resolution work the revert chain may introduce.

If a reverted commit's diff doesn't apply cleanly onto the post-B2
substrate (a likely outcome — B2 retires the proc-macro, which
changes the surface that W0' consumers depended on), B4's plan
authors a fresh implementation that captures the same architectural
intent. The diffs are documentation, not contract.

## 9. Probabilistic thesis status

The W0' half-migration thesis is **plausible but unproven** at plan-
author time. The B2.W0.c diagnostic narrowing established:

- The wall lives in `BbnfBootstrap::parse` (not the IR pipeline).
- The wall reproduces in release-profile direct parse with no
  proc-macro expansion in flight.
- Pre-W0' `compile_pipeline::compile_bbnf` measured 911 µs at AU
  baseline; current observed wall is unbounded > 5 min.

The narrowing localizes WHERE the wall lives but does not by itself
prove WHICH commit set introduced it. The "W0' half-migration"
attribution rests on:

- **Strong**: W0' was the last commit set to touch the parse path
  before the wall manifested.
- **Strong**: W0' was cherry-picked without a corresponding
  `generated.rs` regen (a known half-migrated state).
- **Weaker**: the parser table was not updated post-W0', so the
  half-migration's mismatch should plausibly slow the parser on
  recursion-dense grammars like BBNF.
- **Weak (unverified)**: no other commit between AY-I close and
  master HEAD touched the parse path heavily enough to cause this
  regression alone.

The W0.a disposable-worktree probe is the proof. It applies the W0'
revert and reads the parse-phase wall directly:

- IF parse-phase wall < 5 s: the W0' thesis is CONFIRMED. The
  reverted state IS the pre-W0' state (no other commits touched the
  parse path enough to matter), and the W0' content is the regression
  source.
- IF parse-phase wall >= 5 s: the W0' thesis is DISPROVED. The
  regression source sits earlier (W0-fix or W0 base, per the
  candidate set) or in an unexamined commit. Escape clause expansion
  runs on a fresh disposable probe.
- IF the probe is INDETERMINATE (build failed, non-parse phase
  blocked > 60 s, etc.): the proof environment is broken;
  triumvirate dispatch.

The proof-first sequencing exists precisely because the thesis is
probabilistic. Landing 14 forward-revert commits on master before
proof would create permanent history noise that escape-clause
expansion would have to layer onto. The disposable probe lets B3 test
the strongest single-commit-set hypothesis without committing to it.

## 10. Summary

B3 tests the W0' half-migration hypothesis on a disposable worktree
first. If the probe confirms, the canonical 14-commit revert chain
lands on master; if the probe disproves, B3 escalates via the escape
clause without polluting master with forward-revert noise. The scope
of the W0'-only revert is the minimal set required to make master
compile cleanly without W0' runtime types. W0'.d4-d7 (independent
infra) stay; W0-fix + W0 base stay (covered by escape clause
expansion via fresh disposable probes if W0' alone is insufficient).

B2 resumes on the post-B3 substrate (whatever it ends up being —
W0'-revert, W0'+W0-fix-revert, or W0'+W0-fix+W0-base-revert). B4
re-lands W0' on the post-B2 substrate. AY-II.W0' close ceremony
shifts to B4 close. AY-II.W1-W5 operates on the post-B4 substrate.

B3 is the necessary sequencing correction that lets the architectural
plan execute on honest substrates AND with honest evidence at every
step. No canonical history lands on master without prior probe
confirmation.
