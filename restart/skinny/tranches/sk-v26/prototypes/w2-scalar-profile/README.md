# W2 scalar classifier prototype

This standalone crate measures a grammar-derived eight-byte scalar lexical
classifier against a byte/UTF-8-scalar oracle inside the same consuming generic
grammar parser. It is not imported by production BBNF.

The decision thresholds are pre-registered in `PROFILE-PLAN.md`. Replay from
this directory:

```sh
cargo fmt --check
cargo test --locked
test "$(printf abc | sha256sum | cut -d ' ' -f 1)" = \
  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

export SKV26_SUBJECT_COMMIT="$(git rev-parse HEAD)"
export SKV26_DIRTY_DIGEST="$(
  repo_root="$(git rev-parse --show-toplevel)"
  cd "$repo_root"
  {
    git diff --binary
    git diff --cached --binary
    git ls-files --others --exclude-standard |
      LC_ALL=C sort |
      grep -Ev '(^|/)profile\.raw\.json$' |
      while IFS= read -r candidate_file; do
        sha256sum "$candidate_file"
      done
  } |
    sha256sum |
    cut -d ' ' -f 1
)"
export SKV26_PLAN_SHA256="$(
  sha256sum PROFILE-PLAN.md | cut -d ' ' -f 1
)"
export SKV26_SOURCE_SHA256="$(
  sha256sum Cargo.toml Cargo.lock src/main.rs |
    sha256sum |
    cut -d ' ' -f 1
)"
export SKV26_POWER_MODE="AC power; Low Power Mode state recorded from pmset"
export SKV26_OUTPUT="profile.raw.json"
export RUSTFLAGS="-C target-cpu=native -C opt-level=3 -C codegen-units=1"

cargo clean
build_start="$(python3 -c 'import time; print(time.monotonic_ns())')"
cargo build --release --locked
build_end="$(python3 -c 'import time; print(time.monotonic_ns())')"
export SKV26_CLEAN_BUILD_NS="$((build_end - build_start))"
cargo run --release --locked
python3 -m json.tool profile.raw.json >/dev/null
```

The JSON retains exact input SHA-256 values, all paired raw wall-time samples,
A/A and candidate intervals, eligibility/fallback counters, fresh/reuse
allocation probes, environment/toolchain data, the mechanical disposition, and
limitations. Hardware performance counters are explicitly unavailable unless a
separate privileged run supplies them.
