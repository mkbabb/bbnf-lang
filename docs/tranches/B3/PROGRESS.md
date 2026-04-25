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

B2 alone cannot close: T3 relocates the wall from rustc-expand-time to
xtask-runtime, but the wall persists. B3 reverts the W0'-scope chain
to restore the pre-W0' parser baseline; B2 resumes on the post-revert
substrate; B4 re-lands W0' under the post-B2 xtask substrate where
bisect-and-fix is straightforward (no proc-macro re-compile cost
between bisect steps).

Authored in this initial state:

- `B3.md` — 8 invariants, 2-wave schedule, cross-tranche debt ledger,
  escape clause covering scope expansion (W0' → W0-fix → W0 base) and
  thesis-pivot-via-B5 hard-environmental-blocker path.
- `waves/W0.md` — revert sequence (14 W0'-scope commits) + build
  verification; 1 agent + 1 closer.
- `waves/W1.md` — parity bench (`bbnf_parses_its_own_grammar`,
  `compile_pipeline::compile_bbnf`, `cargo xtask regen --grammar
  bbnf`) + B3 FINAL + cross-tranche doc updates; 1 agent.
- `AGENT_DISPATCH.md` — sub-agent dispatch surface with explicit
  anti-patterns (no `-X theirs`, no `git reset --hard`, no touching
  B2.W0.a/b/c, no W0'.d4-d7 reverts, no pre-authoring B4).
- `PROGRESS.md` — this file.

No execution wave has dispatched yet. Master HEAD: `b8cacedd`.

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
