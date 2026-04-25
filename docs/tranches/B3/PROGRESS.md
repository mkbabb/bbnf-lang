# B3 — Progress Log

Dated execution log for tranche B3, the AY-II.W0' revert tranche that
restores the parser baseline so B2 can resume W0.c re-execution
cleanly.

- `Status`: planned (W0 + W1 sequenced per `B3.md` wave summary)
- `Current wave`: W0 (planned)
- `Next wave`: W1 (opens after W0 close)

---

## 2026-04-25 — Plan authored

B3 opens as a focused revert tranche under the R3 path per
`docs/tranches/B2/audit/W0c-status-2026-04-25-04h.md`. R3 sequences:
B3 (revert) → B2 resume → B4 (re-land W0' under post-B2 substrate).

The architectural thesis: AY-II.W0' source landings cherry-picked onto
master without a corresponding `generated.rs` regen, leaving runtime
types updated but the emitted parser table out of sync. The four
2026-04-25 deep audits attributed the resulting > 80 min cold wall to
"AY-II IR-pipeline accumulation running inside rustc's expand phase".
B2.W0.c's release-profile xtask probe disproves that attribution: the
wall lives in `BbnfBootstrap::parse` itself, not in the IR pipeline.

The W0' half-migration thesis remains a HYPOTHESIS at plan-author
time; the W0.c diagnostic narrowing made it plausible but not proven.
B3 sequences the test as a proof-first disposable-worktree probe BEFORE
any canonical history lands on master: if the W0.a probe CONFIRMS the
thesis (parse-phase wall < 5 s on phase-instrumented xtask regen), the
canonical chain lands; if the probe DISPROVES, B3 escalates per the
escape clause without polluting master with forward-revert noise.

B2 alone cannot close: T3 relocates the wall from rustc-expand-time to
xtask-runtime, but the wall persists. B3's revert (if the probe
confirms the W0' thesis) restores the pre-W0' parser baseline; B2
resumes on the post-revert substrate; B4 re-lands W0' under the
post-B2 xtask substrate where bisect-and-fix is straightforward (no
proc-macro re-compile cost between bisect steps).

Authored in this initial state:

- `B3.md` — 8 invariants, proof-first 2-wave schedule (W0 splits into
  W0.a probe / W0.b canonical-IF-confirmed / W0.c orchestrator
  cherry-pick), cross-tranche debt ledger, escape clause covering
  scope expansion via fresh disposable probes (W0' → W0-fix → W0
  base) and B5-as-hard-environmental-blocker path.
- `waves/W0.md` — proof-first phase decomposition: W0.a runs a
  disposable-worktree probe with phase-instrumented xtask regen
  (parse-phase wall is the load-bearing measurement); W0.b runs
  canonical chain only IF probe CONFIRMED; W0.c is orchestrator
  cherry-pick with strict no-`git reset --hard` discipline.
- `waves/W1.md` — parity bench with built-binary parser timing
  (compile separated from runtime), `compile_pipeline::compile_bbnf`
  divan, phase-instrumented `cargo xtask regen --grammar bbnf`
  (per-phase walls inspected, not just total) + B3 FINAL + cross-
  tranche doc updates.
- `AGENT_DISPATCH.md` — sub-agent dispatch surface with explicit
  anti-patterns (proof-first; no `-X theirs`; no `git reset --hard`
  ever; no `cargo nextest run` for parser-runtime gate; no single-
  wall-clock trust on xtask regen; no touching B2.W0.a/b/c; no
  W0'.d4-d7 reverts; no pre-authoring B4).
- `audit/W0p-revert-rationale.md` — scope justification, dependency
  analysis, alternative-paths-considered, probabilistic-thesis status.
- `PROGRESS.md` — this file.

No execution wave has dispatched yet. Master HEAD: `b8cacedd` + plan
commit `fc3b5aaf`.

## 2026-04-25 — Plan amended for proof-first sequencing

User-led amendment after the initial plan landed. Critiques resolved:

1. **Proof-first revert.** W0 split into W0.a (probe), W0.b (canonical-
   if-confirmed), W0.c (orchestrator cherry-pick). The disposable-
   worktree probe runs on a throwaway branch; canonical history only
   lands when the probe confirms parser-baseline restoration.
2. **Parser-runtime gate excludes compile.** W1 uses `cargo test
   --no-run` + located-binary `time` invocation, not bare
   `cargo nextest run`.
3. **xtask regen requires phase walls.** W0.a/b instrument
   `xtask/src/regen.rs` plus the minimal env-gated hooks under
   `crates/core/src/pipeline/` needed to time `BbnfBootstrap::parse`
   separately from `compile_paths_request`; W1 hard gate consumes the
   per-phase output and rejects single-wall-clock pass/fail.
4. **Thesis framing.** B3.md and PROGRESS.md frame W0 as "test the
   revert thesis", not "execute the revert", until the probe lands.
5. **No `git reset --hard`.** AGENT_DISPATCH.md anti-patterns
   explicitly forbid destructive resets anywhere; cherry-pick
   conflicts resolve via `git cherry-pick --abort`.
6. **B4 stays unplanned.** Confirmed in AGENT_DISPATCH.md; only
   forward pointers in REMAINING-TRAJECTORY + AY-II/PATH-FORWARD.

## 2026-04-25 — Toolchain-command redress

Follow-up critique after coreutils installation. `timeout` itself is
now available as GNU coreutils (`/opt/homebrew/bin/timeout`), so the
plan keeps bare `timeout 600`. The redress narrowed to command and
artefact correctness:

1. `cargo xtask` is now a checked-in release-profile cargo alias,
   removing dependence on host-local `cargo-xtask` binaries or verbose
   `cargo run -p xtask --release --` spellings.
2. `timeout | tee` probes now use zsh `pipefail` + `${pipestatus[1]}`
   so the timeout/cargo exit status is not masked by `tee`.
3. B3 no longer deletes `.bbnf-cache` during normal cycle probes; cache
   coldness is an explicit measurement mode, not an accidental side
   effect of every verification command.
4. W0.a probe artefacts are authored inside the disposable worktree as a
   docs-only evidence commit; master receives no source reverts from the
   probe path.
5. W1 divan capture writes JSON directly via `DIVAN_BENCH_FORMAT=json`
   and saves stderr separately for diagnosis.

## 2026-04-25 — W0.a probe #1 (W0' scope) — DISPROVED

**Verdict**: DISPROVED. The W0' revert alone does NOT restore parser
baseline.

**Probe path**: disposable worktree at
`/Users/mkbabb/Programming/bbnf-wt-b3-w0a-probe`. Master HEAD pre-probe:
`d4b9272a`. Disposable-worktree commits (NOT cherry-picked to master):

- `809d4d9c` — feat(xtask): phase-timing instrumentation (W0.a).
- `a5f0a672`..`f45d7d56` — 14 W0' reverts (clean, 0 conflicts).
- `cbf84ed7` — audit(b3): W0.a probe verdict + phase-timed xtask output.

**Phase walls**:

| Phase | Wall |
|---|---|
| manifest | 11.09 ms |
| load | NEVER COMPLETED (>= 600 s timeout) |
| parse | NEVER COMPLETED — `[parse]` line never emitted |
| ir-pipeline | not reached |
| generate_all | not reached |
| prettyplease | not reached |
| write | not reached |
| total | not emitted (process killed by `timeout 600`) |

The `[manifest]` log proves the env-gate (`BBNF_REGEN_PHASE_LOG=1`)
plumbing works. The instrumented `[load]` phase wraps `compile_paths_request`
+ `load_merged_paths`; entry to that wrapper never produced exit, so
the hang is inside `load_merged_paths -> parse_to_pipeline_inputs ->
BbnfBootstrap::parse`. Reverting W0' alone does not unwind the
regression.

**Recommended next action**: Escape 1 — fresh disposable probe with
wider scope (W0' + AY-II.W0-fix; adds `c9142405` and `f8ac2cd7` to the
revert chain, totaling 16 commits).

**Latent build break observed (NOT introduced by B3)**:
`crates/bootstrap/src/bin/cost_grid_sweep.rs` imports
`bbnf_ir::passes::lift_dta` but `bbnf-ir` was purged from
`crates/bootstrap/Cargo.toml` at B2.W0.c partial-close `21881591`.
The break exists on master `d4b9272a` and predates B3. The bin path
is unrelated to xtask; `cargo build -p xtask --release` and `cargo
check -p bbnf -p xtask -p bbnf-derive` succeed. B3 does not fix it
(out of scope); B2.W0.c re-execution will surface it as a residual
issue.

The W0.a probe worktree was preserved for audit immediately post-
verdict; it is torn down before the W0.a probe #2 dispatch.

## 2026-04-25 — W0.a probe #2 (W0' + W0-fix scope) — INDETERMINATE

**Verdict**: INDETERMINATE — wider revert scope is unprobeable as-designed.

**Probe path**: disposable worktree at
`/Users/mkbabb/Programming/bbnf-wt-b3-w0a2-probe`. Master HEAD pre-probe:
`ef52c35c`. Worktree commits (NOT cherry-picked):

- `7a55c88d` — feat(xtask): phase-timing instrumentation (probe #2).
- `c418efea`..`d55f9d88` — 16 throwaway reverts (W0'.a/b/c/d1/d3 + W0-fix
  c9142405 + f8ac2cd7), 0 conflicts during `git revert`.

**Build verification**: `cargo check -p bbnf -p xtask -p bbnf_derive
--profile ax-iter` failed with **219 errors** in `bbnf` (lib). The
errors cluster on:

- `error[E0061]: this method takes 4 arguments but 6 arguments were
  supplied` — at `begin_compound` callsites.
- `error[E0599]: no method named end_compound_post_order found for
  mutable reference &mut TapeBuilder`.

**Root cause (architectural)**: `crates/core/src/grammar/generated.rs`
(the checked-in 33 293-line BBNF self-host emission) was regenerated
AFTER W0-fix landed, so it emits the 6-arg `begin_compound` and the
`end_compound_post_order` method. B3's revert deliberately does not
touch `generated.rs` (per the W0' source-vs-generated mismatch
analysis). Reverting W0-fix from source restores the 4-arg signature,
which `generated.rs` no longer compiles against.

The W0' + W0-fix scope cannot be probed without ALSO restoring a
pre-W0-fix generated.rs — a larger surgery than the original B3 plan
envisioned. Restoring an arbitrary historical generated.rs introduces
its own consistency risks (other API shifts between then and now).

**Recommended next action — sharpen probe #1, not Escape 2**: the
W0'-only revert (probe #1) succeeded at building cleanly; its hang
remains the highest-quality data point. Probe #1's instrumentation
recorded `[load]` not completing AND `[parse]` not emitting, but
those are ambiguous — both consistent with (a) hang in
`compile_paths_request` preamble before parse OR (b) hang inside
`BbnfBootstrap::parse` itself. The next probe (probe #3) re-runs the
W0'-only revert with **sharper instrumentation**: `[parse-start]`
log immediately before invoking `BbnfBootstrap::parse` + `[parse-end]`
log immediately after. This narrows the hang location precisely:
parse-start emitted + parse-end missing → parse-internal hang;
parse-start missing → hang earlier in the path.

**Latent build break still present** (NOT introduced by B3):
`cost_grid_sweep.rs` import issue persists. Out of B3 scope.

The probe #2 worktree was preserved for audit immediately post-
verdict; it is torn down before probe #3 dispatch.

## 2026-04-25 — W0.a probe #3 (W0' scope, sharper [parse-start]/[parse-end]) — DISPROVED-PARSE

**Verdict**: DISPROVED-PARSE. Hang is **inside `BbnfBootstrap::parse`**.

**Probe path**: disposable worktree at
`/Users/mkbabb/Programming/bbnf-wt-b3-w0a3-probe`. Master HEAD pre-probe:
`c325c13f`. Worktree commits (NOT cherry-picked):

- `557bf58b` — feat(pipeline): sharper parse-start/parse-end + load +
  extract-host markers (only in `crates/core/src/pipeline/directives.rs`,
  env-gated by `BBNF_REGEN_PHASE_LOG=1`).
- `23a30d76`..`6d0823c1` — 14 W0'-only throwaway reverts (matches
  probe #1's revert scope, 0 conflicts).

**Build**: `cargo check -p bbnf -p xtask -p bbnf_derive --profile
ax-iter` exit 0 in 6.21 s. `cargo build -p xtask --release` exit 0
in 1m 03s.

**Probe output** (`BBNF_REGEN_PHASE_LOG=1 timeout 600 cargo xtask
regen --grammar bbnf`):

```
[xtask::regen] bbnf: compile_paths_request started (1 paths, structural=true, prettify=true)
[xtask::regen][load-start] paths=1
[xtask::regen][parse-start] source-len=3448 bytes
EXIT: 124
```

The `[parse-start]` marker fired with the actual `bbnf.bbnf` source
length (3448 bytes), proving `BbnfBootstrap::parse(input)` was
invoked. The `[parse-end]` marker did NOT fire within the 600 s
timeout. **The parser hung INSIDE `BbnfBootstrap::parse` itself**,
on a substrate where the W0' source landings were reverted.

This is a stronger result than probe #1 (which couldn't distinguish
hang-in-parse from hang-before-parse). Probe #3 confirms the hang is
inside the parser proper.

**Implication**: the W0' source-level changes (FusedBuilder collapse,
materializer routing, scan-policy splice) are NOT the regression
source on their own. The regression is either:

1. **Deeper in the AY-II runtime stack** — entangled with W0-fix
   (begin_compound 6-arg) AND/OR W0 base (ValueBuilder allocation,
   STRUCTURAL_SCAN_POLICY emission, projection-totality wires). The
   W0' + W0-fix revert was unprobeable per probe #2's
   generated.rs / source mismatch.
2. **In the generated parser table itself** —
   `crates/core/src/grammar/generated.rs` (33 293 lines) is the BBNF
   parser's state machine. It was last regenerated near `b5bbda6c`
   (AY-II.W0 era). If a regression exists in the generated parser's
   structure (not in helpers), source-level reverts of helpers won't
   surface it.
3. **An interaction between recent helper changes and the existing
   generated table** that compounds at parse time on BBNF-shaped
   input (deep recursion + dense alternation). Subtle inlining or
   monomorphization changes between AY-II.W0 era and master could
   produce O(N^k) behavior on this specific corpus.

**Probe-cycle exhausted within original B3 scope.** Each forward
move from here requires user judgment:

- **(α) Restore historical generated.rs + retry wider revert**:
  reset generated.rs to a pre-W0-era version, revert W0' + W0-fix +
  W0 base, retry probe. Risk: pre-W0-era generated.rs has its own
  API drift; restoration may need additional source patches.
- **(β) samply-profile the hung parser**: attach samply to the
  hung process, capture self-time profile of `BbnfBootstrap::parse`,
  identify the hot path. May reveal whether the issue is one
  function or systemic.
- **(γ) Bisect non-helper commits**: the regression may be in B1
  toolchain pin / nightly drift / dependency upgrade rather than
  AY-II runtime work. Bisect from AY-I close forward, looking for
  the BBNF-parse inflection point.
- **(δ) Probe with smaller grammar**: re-run probe #3 against
  `json.bbnf` (537 bytes) to see whether the regression is
  recursion-density-specific or affects all grammars. Probe #1
  noted json hung > 39 s but didn't time-bound it. Sharp
  measurement.
- **(ε) Acknowledge B3 cannot close as-designed**: the original B3
  thesis (W0' revert restores baseline) is DISPROVED. Close B3 with
  the diagnostic record + relinquish to user direction on
  alternative diagnostic paths (B5+).

The probe #3 worktree is preserved for audit; teardown after this
record commits. Latent build break (cost_grid_sweep.rs) still present;
out of B3 scope.

## Recommendation to user

The original R3 path assumed W0' revert restores baseline. That
assumption is now DISPROVED with sharp evidence. B3 has produced
honest negative results:

1. W0' alone insufficient (probe #1 + probe #3 with sharper
   instrumentation).
2. W0' + W0-fix unprobeable without generated.rs restoration
   (probe #2).

B3 cannot close on its original thesis. The orchestrator surfaces
to user direction on the (α/β/γ/δ/ε) options above. B2 resume,
B4 plan authoring, and AY-II execution all gate on resolving this
diagnostic.

## 2026-04-25 — W0.a probe #4 (master + json grammar) — DISPROVED-GENERIC

**Verdict**: GENERIC PARSER REGRESSION. Affects ALL grammars.

User selected (δ) — probe with smaller grammar against master
substrate.

**Probe path**: disposable worktree at
`/Users/mkbabb/Programming/bbnf-wt-b3-w0a4-probe`. Master HEAD
pre-probe: `e5f77689` (no source reverts). Worktree commits (NOT
cherry-picked):

- `f7169578` — feat(pipeline): parse-start/parse-end markers (probe
  #4 — master + json grammar).

NO reverts applied. Pure master substrate + instrumentation.

**Build**: `cargo build -p xtask --release` exit 0 in 1m 04s.

**Probe** (`BBNF_REGEN_PHASE_LOG=1 timeout 120 cargo xtask regen
--grammar json`):

```
[xtask::regen] json: compile_paths_request started (1 paths, structural=false, prettify=true)
[xtask::regen][parse-start] source-len=537 bytes
EXIT json: 124
```

`json.bbnf` is 537 bytes — ~6× smaller than `bbnf.bbnf` (3448 bytes)
— with shallower recursion and no self-host complexity. The
`[parse-start]` marker fired with the correct source length;
`[parse-end]` did NOT fire within 120 s.

**Conclusion**: the parser hangs on EVERY grammar regardless of
size or recursion density. The regression is NOT BBNF-shape-specific;
it is a **generic low-level parser primitive** that misbehaves
even on small inputs.

This sharply redirects the diagnostic: the regression source is
unlikely to be in BBNF-grammar-specific code paths (which would
favor reverts in BBNF-emission-related commits). It is more likely
in:

- Common parser primitives shared across grammars (regex matching,
  scanner dispatch, tape primitives).
- Generated parser table structure that all grammars share.
- A hot loop that fires in every grammar's parse.

## 2026-04-25 — (β) samply / `sample` profile of hung parser — LOCALIZED

**Verdict**: hot path sharply localized. Hang is a tight inner loop
in `tape::finaliser::derive_frame_depth` failing the strict-monotonicity
precondition on `child_off`.

**Probe path**: reused probe #4 worktree at
`/Users/mkbabb/Programming/bbnf-wt-b3-w0a4-probe` (master HEAD `e5f77689`
+ instrumentation `f7169578`). Profiles saved under
`.profiles/b3/parser-hang/` per PROFILING.md `.profiles/`-only rule.

**Tools used**:
- `samply 0.13.1` with `--unstable-presymbolicate --save-only` (per
  PROFILING.md symbol-resolution rule + non-interactive headless
  capture for analysis).
- macOS `sample` (Xcode CLI) for plaintext stack traces.

**Hottest function**: `tape::finaliser::derive_frame_depth` at
`crates/tape/src/finaliser.rs:362-373`. Inlined into
`<FusedBuilder>::run_finaliser` → `<FusedBuilder>::finish` →
`<BbnfBootstrap>::parse`. The `[profile.release]` `debug = true`
setting allowed samply's `.syms.json` to resolve the inline chain.

**Sample concentration**:
- macOS `sample`: 21 170 / 21 170 (100.0%) hits credited to
  `BbnfBootstrap::parse` (which contains the inlined finaliser body).
- samply: 27 825 / 29 886 (93.1%) in two adjacent PCs at RVA
  `0x6423c` and `0x64258` — 28-byte gap = single tight machine-code
  loop body (the `while pos > start` body's load + store).

**Dominant call chain** (outer → inner):
```
xtask::main (main.rs:41)
  xtask::regen::regen_grammar (regen.rs:212)
    bbnf::pipeline::compile::compile_paths_request (compile.rs:117)
      bbnf::imports::loader::load_module_graph (loader.rs:41)
        bbnf::imports::loader::load_recursive (loader.rs:112)
          bbnf::pipeline::directives::parse_to_pipeline_inputs (directives.rs:104)
            <BbnfBootstrap>::parse (generated.rs:32976)
              <FusedBuilder>::finish (builder/mod.rs:1031)
                <FusedBuilder>::run_finaliser (builder/mod.rs:1060)
                  tape::finaliser::derive_frame_depth (finaliser.rs:364) ← HOT
```

**Hot-path nature**: tight inner loop failing to make progress.
Not deep recursion (sample shows fixed call depth = 10 frames). Not
regex backtracking (no regex symbols in hot stack). Not scanner
dispatch.

**Source bug** (`crates/tape/src/finaliser.rs:362-373`):

```rust
let mut pos = end;
while pos > start {
    let co = pos - 1;
    depth[co] = child_depth;
    let co_has_children = columns.has_children_at(co as u32);
    let co_child_off = columns.child_off_at(co as u32);
    pos = if co_has_children && !co_child_off.is_none() {
        co_child_off.0 as usize  // ← if co_child_off >= pos, infinite loop
    } else {
        co
    };
}
```

The post-order canonicalisation contract requires a compound's
`child_off` to point STRICTLY before the compound's own index
(`child_off < pos`). Some tape record violates this — `child_off >= pos`
— so `pos` does not decrease and the loop spins on the same record
indices forever.

**Hypothesis**: a recent refactor in `crates/tape/` either broke
the post-order canonicalisation contract or the parser-side
`begin_compound` / `end_compound_post_order` is emitting a degenerate
`child_off`. Candidate commits per the β agent:

- `f603f549` — "AY.W1.1 AoS revert: columns → flat Vec<TapeRec> +
  parallel sib_skip"
- `a13840a0` — "retire W5-era open_stack + note_push +
  SIB_SKIP_STAMPED_BIT — AY-II.W0.a"

Plus any commit since `f603f549` touching `child_off` emission or
post-order canonicalisation.

**Artefacts** (committed at `3b8c4b27`):
- `.profiles/b3/parser-hang/json-parse-hang.profile.json.gz` — samply
  profile (211 751 bytes, on-disk only — large binary, NOT git-tracked).
- `.profiles/b3/parser-hang/json-parse-hang.profile.json.syms.json` —
  presymbolicated symbol table (committed).
- `.profiles/b3/parser-hang/sample-stacks.txt` — macOS `sample`
  text report (committed).
- `.profiles/b3/parser-hang/samply-stderr.txt` — phase log + samply
  stderr (committed).
- `.profiles/b3/parser-hang/profile-symbols.txt` — symbol filter
  (committed).

## 2026-04-25 — (γ) debug_assert + bisect — pending dispatch

Next probe: add `debug_assert!(co_child_off.0 < pos as u32, ...)`
at `finaliser.rs:368` in a fresh worktree, rebuild xtask under the
debug profile, re-run the probe, capture the assertion-failure
record index + child_off pair. With that data, bisect `crates/tape/`
since `f603f549` to find the commit that introduced the
non-monotonic `child_off`. Once localized, fix at the source — no
workaround at the finaliser site.

## Pre-B3 inheritance

B3 inherits the following state from B2:

| Item | Source commit | Status |
|---|---|---|
| xtask substrate | `dec67806` (B2.W0.a) | KEEP — survives B3 revert |
| Per-grammar boundary spec | `3c68e8c4` (B2.W0.b) | KEEP — survives B3 revert |
| W0.c partial: xtask body + bbnf-bootstrap migration | `21881591` (B2.W0.c) | KEEP — survives B3 revert |
| W0.c status snapshot | `b8cacedd` (B2.W0.c.status) | KEEP — referenced from B3 PROGRESS |

B2 stays paused through B3. B2's W0.c brief at
`docs/tranches/B2/waves/W0.md` is unchanged; it re-executes against
the post-B3 substrate when B3 closes.

## Forward-looking — what B3 changes for B2 + B4 + AY-II

Once B3 closes:

- **B2 resumes from W0.c re-execution** on the post-revert substrate.
  The same brief that produced `21881591` (the partial-close commit)
  re-runs cleanly — `cargo xtask regen --grammar bbnf` no longer
  hangs because `BbnfBootstrap::parse` runs in milliseconds. B2 then
  continues W1 → W4 per its existing plan.

- **B4 opens after B2 closes** as the W0' re-land tranche. B4's plan
  is authored at that point, citing:
  - The post-B2 xtask substrate as its substrate.
  - The W0'-content diff snapshots at
    `docs/tranches/B3/audit/diffs/*.diff` as its source-of-truth.
  - Bisect-and-fix as its operational mode (each candidate W0' commit
    re-applies on the post-B2 substrate; failures bisect cleanly
    because xtask regen runs the IR pipeline once per cycle, not per
    consumer-crate compile).

- **AY-II.W0' close ceremony** shifts to **B4 close**. The
  compressed-honest 15-min ceremony per AUDIT-B remains the
  operational spec; it executes against the post-B4 substrate.

- **AY-II.W1-W5 sequencing** unchanged. Operates on whatever runtime
  substrate B4 produces.

- **AZ-I.W0 rescoped scope** unchanged (per AUDIT-C: derive-cache +
  Watt items drop). Still gated on B2 close, not on B3.
