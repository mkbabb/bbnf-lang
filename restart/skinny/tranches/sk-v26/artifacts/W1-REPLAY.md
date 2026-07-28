# W1 replay

Run from a fresh login shell. The protected BBNF and parse-that main worktrees
are observation-only; every command below targets the isolated SK-V26 trees or
a task-specific temporary target.

## Fixed identities

```sh
export SKV26_BBNF=/Users/mkbabb/Programming/bbnf-lang-skv26
export SKV26_PT=/Users/mkbabb/Programming/parse-that-skv26
export SKV26_PT_TARGET=/tmp/skv26-w1-pt-replay
export SKV26_BRIDGE_TARGET=/tmp/skv26-w1-bridge-replay
export SKV26_PROFILE_TARGET=/tmp/skv26-w1-profile-replay

git -C "$SKV26_PT" branch --show-current
git -C "$SKV26_PT" rev-parse HEAD HEAD^{tree}
git -C "$SKV26_BBNF" branch --show-current
git -C "$SKV26_BBNF" rev-parse ad7aec5c6b3731fd6d57008d051fb0799377ae22 \
  ad7aec5c6b3731fd6d57008d051fb0799377ae22^{tree}
test -z "$(git -C "$SKV26_PT" status --porcelain=v1 --untracked-files=all)"
```

Expected parse-that identity:

```text
codex/sk-v26-parse-that
9c9714dfdfa6d33b70f5738c311a9abccb4a1bdf
76141a76dee446249233056e94ea9b043968bdd4
```

Expected BBNF evidence payload:

```text
codex/sk-v26-bbnf
ad7aec5c6b3731fd6d57008d051fb0799377ae22
a6b75de6b07995a741d455d81acc246f7067e061
```

## Transaction and regex laws

```sh
cd "$SKV26_PT/rust"
CARGO_TARGET_DIR="$SKV26_PT_TARGET" \
  cargo test --workspace --all-targets --all-features --locked
CARGO_TARGET_DIR="$SKV26_BRIDGE_TARGET" \
  cargo test --locked --manifest-path contracts/runtime_bridge/Cargo.toml
CARGO_TARGET_DIR="$SKV26_PT_TARGET" \
  cargo clippy --workspace --all-targets --all-features --locked -- -D warnings
CARGO_TARGET_DIR="$SKV26_BRIDGE_TARGET" \
  cargo clippy --locked --manifest-path contracts/runtime_bridge/Cargo.toml \
    --all-targets -- -D warnings
CARGO_TARGET_DIR="$SKV26_PT_TARGET" RUSTDOCFLAGS="-D warnings" \
  cargo doc --workspace --no-deps --locked
git -C "$SKV26_PT" diff --check
```

Expected: 70 workspace laws, 7 standalone bridge laws, zero failure, zero
Clippy warning, and zero rustdoc warning.

The allocation-law process owns the global counting window and internally
checks four cases. The production runtime has no unsafe block; Miri was
unavailable for the pinned nightly and is not represented as a passing gate.

## Clean packages

```sh
cd "$SKV26_PT/rust"
CARGO_TARGET_DIR="$SKV26_PT_TARGET" cargo package -p parse_that --locked
CARGO_TARGET_DIR="$SKV26_PT_TARGET" cargo package -p bbnf-regex --locked
shasum -a 256 \
  "$SKV26_PT_TARGET/package/parse_that-0.4.0.crate" \
  "$SKV26_PT_TARGET/package/bbnf-regex-0.1.0.crate"
```

Expected:

```text
4c30d451e42fef441fab29afed4394e142a71b293dc9c4fe301a3b93e8c92293  parse_that-0.4.0.crate
c9ca32cf4022dbdd238387f9600bc7502d8de3a0f8c6f725a4f289d4e3499d50  bbnf-regex-0.1.0.crate
```

The zero-dependency runtime also installs and executes from an extracted
archive with a new empty `CARGO_HOME` under `--offline`. A literally empty
offline registry cannot resolve `bbnf-regex`'s one mandatory dependency,
`smallvec`; the same extracted package passes in an isolated warmed cache.

## BBNF corpus and crater

```sh
cd "$SKV26_BBNF"
node scripts/skv26-regex-corpus.mjs --verify
node scripts/skv26-w1-crater.mjs --verify
```

Expected:

```text
grammar files 43
regex occurrences 230
distinct regex cases 126
crater paths 148
crater sites 3930
path dispositions REQUIRED 3, REPLACED 77, RETIRED 33, BANKED 29, UNREACHABLE 6
```

The compile observation is intentionally RED:

```text
crates/gorgeous/Cargo.toml:78 requests removed parse_that feature diagnostics
```

Everything after dependency resolution is classified
`UNREACHABLE_AFTER_FIRST_BLOCKER`; no green legacy-fleet claim is made.

## Non-promoting profile

The committed profile is an observation, not a speedup decision. Verify that
its three premeasurement authorities still match:

```sh
cd "$SKV26_BBNF"
jq -r '.measurement_sources[] | [.sha256, .path] | @tsv' \
  restart/skinny/tranches/sk-v26/artifacts/W1-PROFILE.json |
while IFS=$'\t' read -r expected path; do
  test "$(shasum -a 256 "$path" | awk '{print $1}')" = "$expected"
done
jq -e '
  .disposition == "NON_PROMOTING_CORRECTNESS_TRANSPOSITION" and
  (.build.clean_compile_elapsed_ns | length) == 20 and
  ([.recognition.rows[].elapsed_ns | length] | all(. == 20)) and
  ([.recognition.rows[].checksum] | all(. != 0))
' restart/skinny/tranches/sk-v26/artifacts/W1-PROFILE.json
```

`node scripts/skv26-w1-acceptance.mjs` performs a fresh twenty-sample run and
replaces `W1-PROFILE.json` with a new timestamp, host-load observation, and raw
samples. It is regeneration, not compare-only replay.

## W2 boundary

W1 does not claim genuine generator output. The first W2 gate must generate and
execute literal, regex, sequence, choice, repeat, and EOF in both discarding and
retaining sink modes. A handwritten or template-copied fixture does not satisfy
that gate; failure reopens W1.
