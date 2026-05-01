# crates/bootstrap (bbnf-bootstrap) — Modernization Plan

## Role in the fleet
Generates self-hosted BBNF grammar parser via `cargo expand`. Publish=false.
Workspace equivalent of parse-that's `regex-bootstrap`. Hosts the 133-LOC
self-host grammar as a single `#[derive(Parser)]` site (~3–5 min cold
expansion). Excluded from `iter-check`. Regen pipeline driven by
`scripts/bootstrap-bbnf.sh` at workspace root.

## Current posture (from Wave 1-B assay)
- Workspace member. lib (implicit from `[dependencies]`; no `[lib]` section,
  no `[bin]`). `publish = false`. **No features. No benches. No dev-deps.
  No tests.**
- `[dependencies]`: `bbnf_derive`, `bbnf`, `bbnf-ir`, `parse_that`, `pprint`.
- Inherits workspace toolchain + `.cargo/config.toml` + `.config/nextest.toml`.
- EXCLUDED from `iter-check` (133-LOC self-host grammar site; ~3–5 min cold
  expansion). Covered only by `scripts/bootstrap-bbnf.sh` and
  `scripts/check-bootstrap-clean.sh`.
- Per `feedback_generated_files_clean_regen`, output must be fresh regen.
  `check-bootstrap-clean.sh` preflight guards against hand-patching.
- **`rm -rf target/.bbnf-cache/` anti-pattern in its script** (per
  W1-B `feedback_no_workarounds` flag).
- 1 site of `bbnf_derive::Parser`. Second-heaviest aggregate expansion in the
  workspace (after gorgeous's 6 sites).
- Contributes to the ICE cluster.

## Target posture
- Inherits fleet pin.
- `scripts/bootstrap-bbnf.sh` rewritten:
  - Delete `rm -rf target/.bbnf-cache/` (anti-pattern).
  - Add content-hash guard so repeat invocation is idempotent (content-cache
    hit → early exit).
  - Target: ≥130s cold → ≤10s cache-hit per B1 §4.5.
- Remains excluded from `iter-check`.
- No benches added (regen pipeline is not bench-gated; `expansion-cost` gate
  in `crates/derive` covers the derive wall).

## Gap — what must change
1. Inherit workspace pin (0 min; automatic).
2. Rewrite `scripts/bootstrap-bbnf.sh` — delete `rm -rf`, add content-hash
   guard, follow B1 Step 10 action (per `patches/scripts/bootstrap-bbnf.sh.action`).
   The script belongs to the workspace not to this crate, but its
   functionality is specifically about this crate; 1 hour.
3. `scripts/check-bootstrap-clean.sh` — minor adjustment for the new
   content-hash artefact; 15 min.

**Total**: ~1.25 hours (mostly in workspace scripts, not the crate itself).

## Sequencing — when this repo lands
- **Phase A (during bbnf-lang B1)**: items 1, 2, 3. **This IS B1 Step 10**.
  The bootstrap-wall reduction (≥130s → ≤10s) is one of B1's headline metrics.
- **Phase B**: nothing.
- **Phase C**: nothing.

## Dependencies
- **Upstream blockers**: B1 Step 1 (pin); Step 2 (.cargo/config alias
  surface, for the new script's alias invocations).
- **Downstream blocks**: fleet bootstrap discipline. Every developer who
  regens the self-host grammar hits this script.
- **B1 coupling**: Steps 1, 2, 10.

## Risks
- Content-hash guard must be robust against ambient environment changes
  (different rustc versions emit different expansion output). Hash over
  `source hash + rustc version + cargo profile` should be stable.
- `rm -rf target/.bbnf-cache/` removal has no observable risk (the cache
  is content-keyed; stale entries are harmless); the removal ONLY improves
  cache hit rate.
- ICE cluster compounding: as long as `on_disk_cache.rs:663` triggers, the
  self-host derive expansion can fail intermittently. Pin MUST land before
  the script rewrite lands, otherwise the new cache-hit path never triggers.

## Verification
```bash
cd bbnf-lang
time scripts/bootstrap-bbnf.sh   # cold: ~130s
time scripts/bootstrap-bbnf.sh   # second run (cache hit): ≤10s
scripts/check-bootstrap-clean.sh # preflight passes
cargo check -p bbnf-bootstrap    # builds (takes 3–5 min cold; excluded
                                 # from iter-check by design)
```

## Specific changes (patch-ready)
- `scripts/bootstrap-bbnf.sh` — rewrite per B1 `patches/scripts/bootstrap-bbnf.sh.action`.
  Key changes:
  - Delete `rm -rf target/.bbnf-cache/`.
  - Add `content_hash=$(sha256sum crates/bootstrap/grammar.bbnf | cut -d' ' -f1)`.
  - If `target/.bbnf-cache/${content_hash}.stamp` exists, skip regen.
- `crates/bootstrap/Cargo.toml` — no changes.
