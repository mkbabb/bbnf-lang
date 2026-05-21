# SK-V13 S-P1 V3 Profile Provenance

Pass: S-P1 Profile. Cycle: V3 fold support.
Date: 2026-05-21.
Scope: durable build/run provenance for retained S-P1 profile captures.
Output: this file.

## Toolchain

```text
rustc 1.96.0-nightly (02c7f9bec 2026-04-10)
host: aarch64-apple-darwin
LLVM version: 22.1.2
samply 0.13.1
```

## V1 Retained Parse / Typed / PMU Capture

Run identity:

```text
root=/tmp/skv13-p1
bin=/tmp/skv13-profile-target-0a7b41c5/release
commit=f8be692068e9e464b6ed24027ab26edfd05303fd
date=2026-05-21T06:01:45Z
```

Binary hashes:

| Binary | SHA-256 |
|---|---|
| `/tmp/skv13-profile-target-0a7b41c5/release/xctrace_probe` | `90b7bd127795dfeba33ec2e79a6e6317a8311d6280afdf6b94316f06b8d16a0c` |
| `/tmp/skv13-profile-target-0a7b41c5/release/profile_direct` | `41d81a9233da7a39a537d9786dd80c2d5ca51c4840072f25b768a445ca1a8bb3` |

Retained capture command shapes:

```bash
samply record --save-only --unstable-presymbolicate -r 1000 \
  -o /tmp/skv13-p1/samply/profiles/parse__${corpus}__track1.json.gz \
  /tmp/skv13-profile-target-0a7b41c5/release/xctrace_probe "${path}" track1 200

samply record --save-only --unstable-presymbolicate -r 1000 \
  -o /tmp/skv13-p1/samply/profiles/typed__${corpus}__real_typed_track1.json.gz \
  /tmp/skv13-profile-target-0a7b41c5/release/profile_direct 2000 "${corpus}" real_typed_track1

bash /tmp/skv13-p1/pmu/run-pmu.sh
```

The exact original cargo build invocation for the V1 target was not preserved.
V3 therefore retains V1 parse/typed as auditable capture artefacts with binary
hashes, not as a fully rebuildable command surface. The V2 fold did not change
`skinny/crates/` behavior source from the V1 profile baseline.

## V2 Direct Capture

Run identity:

```text
root=/tmp/skv13-p1-v2
repo=/Users/mkbabb/Programming/bbnf-lang
head=7ee299096be7d7fdaa0e69344a6cd18bbd55524f
date=2026-05-21T06:56:28Z
```

Build:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv13-profile-target-v2 \
RUSTFLAGS='-C target-cpu=native' \
cargo build --release -p bbnf-bench --bin profile_direct --bin xctrace_probe
```

Binary hashes:

| Binary | SHA-256 |
|---|---|
| `/tmp/skv13-profile-target-v2/release/profile_direct` | `94545e072ae1ee639b044eba096fe9f7e721c20a6753f25f4e6192463606a555` |
| `/tmp/skv13-profile-target-v2/release/xctrace_probe` | `6f61b641b555bc95db8006622aed5102e8822e7ffb0de84e1acbdf726f70dece` |

Capture:

```bash
samply record --save-only --unstable-presymbolicate -r 1000 \
  -o /tmp/skv13-p1-v2/samply/profiles/direct__${corpus}__${mode}.json.gz \
  /tmp/skv13-profile-target-v2/release/profile_direct 3000 "${corpus}" "${mode}"
```

Status:

```bash
awk -F '\t' 'NR>1{n++; bad+=($4!=0)} END{print n,bad+0}' \
  /tmp/skv13-p1-v2/samply/direct_capture_status.tsv
# 34 0
```

## V2 CSS Declaration-Values Capture

Build:

```bash
CARGO_TARGET_DIR=/tmp/skv13-css-profiler-target-v2 \
RUSTFLAGS='-C target-cpu=native' \
cargo build --release --manifest-path /tmp/skv13-css-profiler/Cargo.toml
```

Binary/source hashes:

| Path | SHA-256 |
|---|---|
| `/tmp/skv13-css-profiler/Cargo.toml` | `2e92f45a14f7db6e400574163727fdb16b25062bc4f3bdd76be4ffae68250823` |
| `/tmp/skv13-css-profiler/Cargo.lock` | `21c3911e49fe218f8a3c3f369781c2031939578b7f3b119d24c87f7159452fae` |
| `/tmp/skv13-css-profiler/src/main.rs` | `89e8aa6296af3facaee66aec41eaf7154966d800f2955f4482c14dc5f2cfff78` |
| `/tmp/skv13-css-profiler-target-v2/release/skv13-css-profiler` | `46b88b7e85ce126cfe9d5423b49e5560e63b75a9a66d08e987adc401efaea2a9` |

Throughput/equality command:

```bash
/tmp/skv13-css-profiler-target-v2/release/skv13-css-profiler 200000 \
  > /tmp/skv13-p1-v2/css/logs/css_l4_declaration_values_all_modes.log 2>&1
```

Samply command:

```bash
samply record --save-only --unstable-presymbolicate -r 1000 \
  -o /tmp/skv13-p1-v2/css/profiles/css_l4_declaration_values_all_modes.json.gz \
  /tmp/skv13-css-profiler-target-v2/release/skv13-css-profiler 200000
```

CSS status: strict equality passed, but the V2 profile is timer/fact-sink
dominated and the absolute Mbps values are method-mismatched against the SK-V12
Criterion close.

## Offline Sidecar Extraction

Checked-in reproducer:

```bash
SKV13_P1_ROOT=/tmp/skv13-p1-v2 \
python3 restart/skinny/tranches/sk-v13/research/p1/support/extract_hotleaf_top20.py
```

Original temp extractor hash:

```text
3e94d2959ec880d56bd86955692338033d84d6c3d95c608b0fa716878939eebe  /tmp/skv13-p1-v2/summary/extract-hotleaf-top20-equivalent.py
```

The extracted TSVs remain under `/tmp/skv13-p1-v2/summary/` and are referenced
as measurement artefacts, not committed benchmark outputs.
