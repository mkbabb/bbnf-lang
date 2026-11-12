# W0 — Truth seal

Purpose: choose reproducible execution bases and collapse authority before source
movement.

## Scope

1. Seal exact BBNF and parse-that clean HEADs and create separate clean execution
   worktrees for both repositories.
2. Inventory every dirty path by owner/provenance/digest without changing it.
3. Classify SK-V25 and parse-that T/U/B artifacts as:
   `LANDED`, `RED_EVIDENCE`, `FORMATION`, `RETIRED`, or `UNKNOWN`.
4. Export the missing raw owner-prompt receipt or record it permanently
   unavailable.
5. Emit `artifacts/W0-AUDIT-MANIFEST.tsv` with one row per session identity and
   distinct chronological tranche root: source identity, date range, inclusion
   or exclusion reason, semantic-review state, content hash, extractor version,
   and replay command. Target 100+ independent owner sessions and 100+ distinct
   tranche roots. If the retained relevant corpus cannot supply either number,
   manifest every available identity plus the absence proof and leave the
   corresponding obligation qualified—not complete.
6. Select the minimal admitted spec slices for W1-W4.
7. Census BBNF's actual parse-that imports and classify every used facility as
   required, replaceable, or retired.
8. Create the future parse-that worktree from clean `ef10d5b` and the BBNF
   worktree from the selected clean anchor; do not import bytes yet.
9. Pin toolchains, corpora, competitor commits, and semantic manifests.
10. Make JSON/CSS benchmark inputs tracked or content-addressed.
11. Seal a content-addressed held-out grammar candidate pool and a deterministic
    post-freeze selection rule without revealing the selected identity to W2.
12. Generate an authority index and lint all execution-dispatch language against
    it; prose occurrence counts are not the gate.

## Pruning decisions

- Earlier `active/next/closed/converged` instructions retire as authority.
- The four full verticals retire.
- Per-edit two-critic/adjudicator ceremony retires.
- Self-declared model-name gates retire.
- Glass/UI close leaves the parser critical path.
- The 1.7 GiB SK-V25 tree remains protected evidence until a separate,
  owner-authorized compaction; it is not copied forward.

## Born-RED checks

- A clean checkout cannot currently run the canonical JSON/CSS benches.
- Current BBNF dirty source does not compile.
- Parse-that main is dirty and committed Rust is not green.
- Current authority is split across tracked and untracked documents.

## Exit

W0 exits only with:

- `artifacts/W0-EPOCH.tsv`;
- `artifacts/W0-CROSSING.tsv`;
- `artifacts/W0-AUTHORITY.tsv`;
- `artifacts/W0-AUDIT-MANIFEST.tsv`, with one retained identity/root and exclusion
  decision per row;
- `artifacts/W0-REPLAY.md`;
- one clean selected base per repo;
- hashes for every admitted spec/prototype/corpus;
- zero ambiguous `active` authority;
- a clean future parse-that worktree;
- a dry-run build/test/bench command that needs no ignored local input.

No architecture or implementation code lands in W0.

`W0-REPLAY.md` contains exact audit, authority, build, test, and bench commands
and expected digests. W0 is not admitted until a fresh shell in each clean
worktree reproduces them.
