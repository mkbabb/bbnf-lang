# W0 replay

Run every command from a fresh login shell. These commands observe the two
protected mains but write only inside the isolated SK-V26 worktrees and their
dedicated target directories.

## Fixed roots

```sh
export SKV26_BBNF=/Users/mkbabb/Programming/bbnf-lang-skv26
export SKV26_PT=/Users/mkbabb/Programming/parse-that-skv26
export SKV26_PPRINT=/Users/mkbabb/Programming/pprint-skv26
export SKV26_BBNF_TARGET=/Users/mkbabb/Programming/bbnf-lang-skv26/target/skv26
export SKV26_PT_TARGET=/Users/mkbabb/Programming/parse-that-skv26/target/skv26
```

Expected identities:

```text
BBNF branch codex/sk-v26-bbnf
BBNF HEAD   af15f63e0d2d3d719938c13b906a50acbb92ea3b
BBNF tree   d4b6e212b8bac3d61a54db7d091d1c4cf6c87f5f
PT branch   codex/sk-v26-parse-that
PT W0 HEAD  627c9b46a773a278ed8c60728c9d335be6747905
PT W0 tree  b0144769472a972bcd4de8f3e67ffcfe264cef99
PPRINT HEAD 2b4d2d48657ebc4eb4c23297929ad3532e3eb6b4
PPRINT tree 93614138642f48b156ba02dbee4673db649632e9
```

Verify them:

```sh
git -C "$SKV26_BBNF" branch --show-current
git -C "$SKV26_BBNF" rev-parse HEAD HEAD^{tree}
git -C "$SKV26_PT" branch --show-current
git -C "$SKV26_PT" rev-parse HEAD HEAD^{tree}
git -C "$SKV26_PPRINT" rev-parse HEAD HEAD^{tree}
test -z "$(git -C "$SKV26_PPRINT" status --porcelain=v1 \
  --untracked-files=all)"
```

## Formation and authority

```sh
cd "$SKV26_BBNF/restart/skinny/tranches/sk-v26"
shasum -a 256 -c FORMATION-RECEIPT.sha256
cd "$SKV26_BBNF"
node scripts/skv26-authority-lint.mjs
node scripts/skv26-w0-seal.mjs --verify
```

The formation check has fourteen `OK` rows. The authority check reports one
dispatchable authority, `A001`, for W0. The final seal command refuses any
byte or path-status drift in either isolated worktree.

## Owner-session, tranche, and protected-main replay

The live capture script is intentionally absent from replay. Session logs and
project roots move, so W0 froze its exact 230-row source epoch. Replay rebuilds
the manifest only from that immutable row snapshot and never reruns either
anamnesis extractor or overwrites canonical evidence.

```sh
cd "$SKV26_BBNF"
node scripts/skv26-w0-audit-verify.mjs
node scripts/skv26-w0-dirty-manifest.mjs --verify
node scripts/skv26-w0-crossing-sites.mjs --verify
shasum -a 256 \
  restart/skinny/tranches/sk-v26/artifacts/W0-AUDIT-MANIFEST.tsv \
  restart/skinny/tranches/sk-v26/artifacts/W0-DIRTY-BBNF-MAIN.tsv \
  restart/skinny/tranches/sk-v26/artifacts/W0-DIRTY-PARSE-THAT-MAIN.tsv
```

Expected census:

```text
CLAUDE_ABSENCE       1
CLAUDE_OWNER_ROOT   13
CODEX_ABSENCE        1
CODEX_OWNER_ROOT   106
TRANCHE_ROOT        109
TOTAL               230
bbnf-main          1382 status digest 70f2f2a7e1814d143729ce1f63a222f63d50daa0a94323611505b3457e13ad41
parse-that-main     185 status digest 184a7a7c64ea90a4c625cd8094a9b283f5cbf58b8c8fa02861b37124f1567404
```

Expected file digests at admission:

```text
ddc8c5049723b0b43488bdfe18c38c06b1d641b8b604704b7f24c4eaf9d1c073  W0-AUDIT-MANIFEST.tsv
34461b219121eb30024b8efb2e57120740303faefa59471ac126a726aeced8e0  W0-DIRTY-BBNF-MAIN.tsv
6f26c249e0f437346eef90bfbacb8b00dcd5a19d53551b85df1ab98541cfc1e5  W0-DIRTY-PARSE-THAT-MAIN.tsv
```

The 106 Codex roots and 109 tranche roots are an identity/root census, not a
claim that 106 sessions or 109 tranches received fresh semantic adjudication in
W0. The missing formation-era 47-root cohort remains an explicit absence row.

## Content-addressed corpora

```sh
cd "$SKV26_BBNF"
while IFS=$'\t' read -r rel bytes digest provenance; do
  if [ "$rel" = path ]; then continue; fi
  test "$(stat -f %z "restart/skinny/tranches/sk-v26/corpora/$rel")" = "$bytes"
  test "$(shasum -a 256 "restart/skinny/tranches/sk-v26/corpora/$rel" |
    awk '{print $1}')" = "$digest"
done < restart/skinny/tranches/sk-v26/corpora/MANIFEST.tsv
```

Expected: thirteen inputs and 16,487,826 bytes. Their provenance is
`LOCAL_IGNORED_SOURCE_CONTENT_ADDRESSED`; they are valid reproducibility inputs
but non-certifying benchmark evidence until external provenance is sealed.

Materialize the exact inputs as ignored relative symlinks from the tracked
content-addressed source, then verify them:

```sh
cd "$SKV26_BBNF"
node scripts/skv26-materialize-corpora.mjs
node scripts/skv26-materialize-corpora.mjs --verify
```

Expected: thirteen mappings. The materialized paths are derived build/test
inputs, never evidence authorities.

## Build and test observations

BBNF library check:

```sh
cd "$SKV26_BBNF"
CARGO_TARGET_DIR="$SKV26_BBNF_TARGET" \
  cargo check -p bbnf --lib --locked
```

Expected: exit 0. The observed baseline reports 190 warnings; warning cleanup
is not a W0 mutation.

Minimal parse-that library check:

```sh
cd "$SKV26_PT/rust"
CARGO_TARGET_DIR="$SKV26_PT_TARGET" \
  cargo test -p parse_that --lib --locked
```

Expected: exit 0, zero library tests at the clean base. This absence is why W1
must begin with external born-RED law tests.

Full committed parse-that Rust tests:

```sh
cd "$SKV26_PT/rust"
CARGO_TARGET_DIR="$SKV26_PT_TARGET" \
  cargo test -p parse_that --tests --locked
```

Expected at W0: exit 101 solely because `parse_that::regex::parser` is absent
although a committed test imports it. The earlier CSS fixture failures are
removed by deterministic materialization rather than by ignoring the tests.

TypeScript surface:

```sh
cd "$SKV26_PT/typescript"
npm ci
npm run build
npm test
```

Expected at W0 after materialization: install, build, and test exit 0; 13 files
and 122 tests pass. `npm ci` observed 16 audit findings at capture time; no
automatic mutation command is authorized.

## Bench dry-run

```sh
cd "$SKV26_BBNF"
CARGO_TARGET_DIR="$SKV26_BBNF/target/skv26-bench" \
  cargo bench -p bbnf --bench json_monolithic --no-run --locked
```

Expected: exit 0 without consulting ignored runtime inputs. This proves only
that the pinned benchmark comparison graph compiles; it is not a performance
claim and it does not certify the locally sourced corpus.

Prove that the canonical bench resolves the tracked materialized corpus without
turning the observation into a performance claim:

```sh
cd "$SKV26_BBNF"
CARGO_TARGET_DIR="$SKV26_BBNF/target/skv26-bench" \
  cargo bench -p bbnf --bench json_monolithic --locked -- data_s
```

Expected: exit 0. Samples remain non-certifying because the corpus provenance,
semantic adapter, and paired decision plan are not yet qualifying.

## Regeneration and seal order

The audit source epoch is immutable after capture. Ordinary W0 replay is
compare-only:

```sh
cd "$SKV26_BBNF"
node scripts/skv26-w0-audit-verify.mjs
node scripts/skv26-w0-dirty-manifest.mjs --verify
node scripts/skv26-w0-crossing-sites.mjs --verify
node scripts/skv26-authority-lint.mjs
node scripts/skv26-w0-seal.mjs --verify
```

An intentional recapture is a new epoch: run
`scripts/skv26-w0-audit-manifest.mjs`, inspect its changed source epoch and
manifest, and reseal. Never use live capture as verification.
