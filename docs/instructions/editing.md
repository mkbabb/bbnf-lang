# Editing

## Worktrees

For tracked-file implementation work:

- use sibling worktrees
- do not place worktrees under `/tmp` or `/private/tmp`
- keep the main worktree clean before spawning parallel editing agents
- do not overlap file ownership across agents
- commit before parallelizing
- agents must never run `git checkout`, `git stash`, or `git reset` on
  the main worktree
- harden all agent claims
- each agent gets explicit file bounds
- agent prompts are self-contained
- **take great care to avoid sub-agent trammeling or your own trammeling of agent work**

Example:

```bash
ROOT=$(git rev-parse --show-toplevel)
PARENT=$(dirname "$ROOT")
git worktree add --detach "$PARENT/bbnf-wt-foo" HEAD
```

## Generated files

`generated.rs` is produced by:

```bash
bash scripts/bootstrap-bbnf.sh
```

The only legitimate edits to it are via that script.

## Testing and regen

Tests live in `tests/` directories only — never inline
`#[cfg(test)]` modules under `src/`.

Common validation commands:

```bash
cargo test -p bbnf --test grammar_roundtrip > /tmp/roundtrip.txt 2>&1
cargo test -p bbnf --test payload_layouts > /tmp/payload.txt 2>&1
```

Bootstrap regen workflow:

```bash
find . -name ".bbnf-cache" -exec rm -rf {} + 2>/dev/null
bash scripts/bootstrap-bbnf.sh
```

After regen:

- verify `grammar_roundtrip`
- verify idempotency by rerunning regen and diffing the output
