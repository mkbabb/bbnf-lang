# W4 — Worktree Fixture Specification

Date: 2026-05-03
Scope: Full design of the worktree fixture infrastructure at BD.W4. Documents the per-grammar fixture directory layout, xtask `--fleet` flag, symlink contract, per-grammar manifest schema, CI matrix integration.

## §1 Fixture Directory Layout

Per `docs/tranches/BD/audit/research-anchors.md:§4`, the path-dep cargo workspace pattern places per-grammar fixtures alongside grammar source. The W4 layout:

```
crates/bbnf-parse/tests/fixtures/
├── json/
│   ├── manifest.toml              (per-fixture metadata)
│   ├── twitter.json               (canonical input — public dataset)
│   ├── canada.json
│   └── citm-catalog.json
├── css_l4/
│   ├── manifest.toml
│   ├── bootstrap.css
│   ├── animate.css
│   └── tailwind-base.css
├── bbnf/
│   ├── manifest.toml
│   ├── json.bbnf
│   ├── css.bbnf
│   └── math.bbnf
├── google_sheets/
│   ├── manifest.toml
│   ├── basic-formula.txt
│   ├── complex-vlookup.txt
│   └── array-formula.txt
├── css_pretty/
│   ├── manifest.toml
│   ├── reset.css
│   ├── simple-rules.css
│   └── media-query.css
├── ebnf/
│   ├── manifest.toml
│   ├── abnf.ebnf
│   ├── modula2.ebnf
│   └── json.ebnf
├── bnf/
│   ├── manifest.toml
│   ├── algol60.bnf
│   ├── postal-code.bnf
│   └── simple.bnf
├── csv/
│   ├── manifest.toml
│   ├── basic.csv
│   ├── escaped-quotes.csv
│   └── mixed-types.csv
└── math/
    ├── manifest.toml
    ├── arithmetic.math
    ├── complex-expr.math
    └── parens.math
```

Total: 9 grammar dirs × ≥ 3 fixtures each = ≥ 27 canonical inputs + 9 manifests.

## §2 Per-Grammar Manifest Schema

Each `tests/fixtures/<grammar>/manifest.toml` declares per-fixture metadata:

```toml
schema_version = "1.0"

[[fixture]]
name = "twitter.json"
source_url = "https://github.com/cloudwego/sonic-rs/raw/main/benches/data/twitter.json"
license = "Public domain (sonic-rs benchmark dataset)"
expected_outcome = "success"  # "success" | "specific_error"
benchmark_eligible = true
size_bytes = 631265

[[fixture]]
name = "canada.json"
source_url = "https://github.com/cloudwego/sonic-rs/raw/main/benches/data/canada.json"
license = "Public domain"
expected_outcome = "success"
benchmark_eligible = true
size_bytes = 2251051

[[fixture]]
name = "citm-catalog.json"
source_url = "https://github.com/cloudwego/sonic-rs/raw/main/benches/data/citm_catalog.json"
license = "Public domain"
expected_outcome = "success"
benchmark_eligible = true
size_bytes = 1727204
```

Schema fields:

| Field | Type | Required? | Notes |
|---|---|---|---|
| `name` | string | yes | filename relative to fixture directory |
| `source_url` | string | yes (for public datasets) | URL where fixture was downloaded; "hand-crafted" for original fixtures |
| `license` | string | yes | license declaration; public-domain / MIT / Apache-2.0 / hand-crafted |
| `expected_outcome` | enum | yes | "success" or "specific_error" |
| `benchmark_eligible` | bool | yes | whether the fixture is used for performance benchmarks |
| `size_bytes` | int | yes | exact byte count (validated at xtask materialisation) |
| `coverage` | enum | optional | "minimal" / "comprehensive" / "edge-case" — for hand-crafted fixtures |
| `expected_error_kind` | string | required if expected_outcome = "specific_error" | the canonical error kind (`SyntaxErr` / `TypeErr` / etc.) |

## §3 xtask `--fleet` Flag

`xtask/src/worktree_init.rs` (extended at BD.W4 §2.1):

```rust
#[derive(Parser)]
pub struct WorktreeInitArgs {
    /// Materialise per-grammar fixtures fleet-wide
    #[arg(long)]
    pub fleet: bool,

    /// Materialise on a sibling worktree (path)
    #[arg(long)]
    pub target_worktree: Option<PathBuf>,
}

pub fn worktree_init(args: WorktreeInitArgs) -> Result<()> {
    let target = args.target_worktree.unwrap_or_else(|| PathBuf::from("."));

    // BC.W5 baseline behavior preserved
    materialize_data_dirs(&target)?;
    materialize_rewrites(&target)?;

    if args.fleet {
        materialize_per_grammar_fixtures(&target)?;
        symlink_bc_w5_data_dirs(&target)?;
    }

    Ok(())
}

fn materialize_per_grammar_fixtures(target: &Path) -> Result<()> {
    let grammars = ["json", "css_l4", "bbnf", "google_sheets", "css_pretty",
                    "ebnf", "bnf", "csv", "math"];

    for grammar in &grammars {
        let fixture_dir = target.join("crates/bbnf-parse/tests/fixtures").join(grammar);
        std::fs::create_dir_all(&fixture_dir)?;

        // load manifest
        let manifest_path = fixture_dir.join("manifest.toml");
        if !manifest_path.exists() {
            return Err(anyhow!("manifest missing for grammar {}", grammar));
        }
        let manifest: Manifest = toml::from_str(&std::fs::read_to_string(&manifest_path)?)?;

        // download or symlink each fixture per the manifest
        for entry in manifest.fixture {
            let target_path = fixture_dir.join(&entry.name);
            if !target_path.exists() {
                if let Some(url) = &entry.source_url {
                    download_fixture(url, &target_path)?;
                } else {
                    return Err(anyhow!("fixture {} not found and no source_url", entry.name));
                }
            }
            verify_fixture_size(&target_path, entry.size_bytes)?;
        }
    }

    Ok(())
}

fn symlink_bc_w5_data_dirs(target: &Path) -> Result<()> {
    // create symlinks from tests/fixtures/json/ to data/json/, etc.
    // Unix: ln -s; Windows: junction (mklink /J)
    let base = target.join("crates/bbnf-parse/tests/fixtures");
    let src_dirs = [("json", "data/json"), ("css_l4", "data/css")];

    for (grammar, src) in &src_dirs {
        let src_abs = target.join(src);
        let dest = base.join(grammar);
        if src_abs.exists() && dest.exists() {
            #[cfg(unix)]
            std::os::unix::fs::symlink(&src_abs, &dest.join("_data_link"))?;
            #[cfg(windows)]
            std::os::windows::fs::symlink_dir(&src_abs, &dest.join("_data_link"))?;
        }
    }

    Ok(())
}
```

## §4 Symlink Contract

Per BD.W4 §2.8, the symlink contract supports parallel-agent dispatch. When a worker / sub-agent opens a sibling worktree, `xtask worktree-init --fleet --target-worktree /path/to/sibling` materialises ALL fixture directories on the new worktree without copying.

### Unix-like (Linux, macOS)

```bash
ln -s /absolute/path/to/data/json /path/to/sibling/crates/bbnf-parse/tests/fixtures/json/_data_link
```

### Windows

```cmd
mklink /J "C:\path\to\sibling\crates\bbnf-parse\tests\fixtures\json\_data_link" "C:\absolute\path\to\data\json"
```

The xtask chooses the platform-specific syscall via `#[cfg(unix)]` / `#[cfg(windows)]`.

### Verification

```bash
xtask worktree-init --fleet --target-worktree /tmp/test-worktree
find /tmp/test-worktree/crates/bbnf-parse/tests/fixtures -type l | wc -l  # ≥ 2 on Unix
```

## §5 CI Matrix Integration

Per BD.W4 §2.7, the CI matrix expands fleet-wide. The matrix at `.github/workflows/parity-matrix.yml`:

```yaml
name: Parity Matrix

on: [pull_request, workflow_dispatch]

jobs:
  rust-fixtures:
    strategy:
      matrix:
        grammar: [json, css_l4, bbnf, google_sheets, css_pretty, ebnf, bnf, csv, math]
    runs-on: macos-14  # M1 Pro
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo nextest run -p bbnf-parse --test fixtures_${{ matrix.grammar }}

  ts-fixtures:
    strategy:
      matrix:
        grammar: [json, css_l4, bbnf, google_sheets, css_pretty, ebnf, bnf, csv, math]
    runs-on: macos-14
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: 20
      - run: npm ci --workspaces
      - run: npm test --workspace=npm/runtime -- --grammar=${{ matrix.grammar }}

  wasm-fixtures:
    strategy:
      matrix:
        grammar: [json, css_l4, bbnf, google_sheets, css_pretty, ebnf, bnf, csv, math]
    runs-on: macos-14
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: 20
      - run: npm ci --workspaces
      - run: npm test --workspace=npm/runtime-wasm -- --grammar=${{ matrix.grammar }}
```

Total cells: 9 grammars × 3 backends = 27 cells. Each cell has a 5-minute timeout; total wall time ≤ 20 minutes (parallel).

## §6 Per-Grammar Test Scaffolding

Each grammar gets a uniform integration test:

```rust
// crates/bbnf-parse/tests/fixtures_<grammar>.rs (xtask-generated)
use std::fs;
use std::path::PathBuf;
use serde::Deserialize;

#[derive(Deserialize)]
struct Manifest {
    schema_version: String,
    fixture: Vec<FixtureEntry>,
}

#[derive(Deserialize)]
struct FixtureEntry {
    name: String,
    expected_outcome: String,
    expected_error_kind: Option<String>,
    benchmark_eligible: bool,
    size_bytes: u64,
}

#[test]
fn parse_all_fixtures() {
    let fixture_dir = PathBuf::from("tests/fixtures").join("<GRAMMAR>");
    let manifest_path = fixture_dir.join("manifest.toml");
    let manifest_str = fs::read_to_string(&manifest_path).expect("manifest readable");
    let manifest: Manifest = toml::from_str(&manifest_str).expect("manifest parses");

    for entry in &manifest.fixture {
        let path = fixture_dir.join(&entry.name);
        let bytes = fs::read(&path).expect("fixture readable");
        assert_eq!(bytes.len() as u64, entry.size_bytes, "fixture size matches manifest");

        let result = bbnf_parse::<GRAMMAR>::parse(&bytes);
        match entry.expected_outcome.as_str() {
            "success" => {
                assert!(result.is_ok(),
                    "parse failed for {:?}: {:?}", path, result.err());
            }
            "specific_error" => {
                assert!(result.is_err(),
                    "parse unexpectedly succeeded for {:?}", path);
                if let Some(expected_kind) = &entry.expected_error_kind {
                    let err = result.unwrap_err();
                    assert_eq!(err.kind(), expected_kind, "error kind mismatch");
                }
            }
            other => panic!("unknown expected_outcome: {}", other),
        }
    }
}
```

The test file is template-generated by xtask per grammar; the `<GRAMMAR>` placeholder is filled in at codegen.

## §7 Public Dataset Licensing

| Dataset | Source | License | Notes |
|---|---|---|---|
| twitter.json | sonic-rs benchmark | Public domain | sonic-rs's MIT/Apache-2.0 license covers the benchmark data |
| canada.json | sonic-rs benchmark | Public domain | same |
| citm-catalog.json | sonic-rs benchmark | Public domain | same |
| bootstrap.css | Twitter Bootstrap | MIT | bootstrap-4 minified CSS |
| animate.css | Daniel Eden | MIT | animate.css library |
| tailwind-base.css | Tailwind Labs | MIT | tailwind-base.css preflight |
| BBNF grammar files | bbnf-lang | MIT/Apache-2.0 | self-host grammars |
| Cohort fixtures | hand-crafted | MIT/Apache-2.0 | original fixtures |

The licenses are recorded in each manifest's `license` field; cross-verified at xtask materialisation.

## §8 Fixture Size Budget

| Grammar | Largest fixture | Total grammar size |
|---|---:|---:|
| json | canada.json (2.2 MB) | ~4.6 MB |
| css_l4 | tailwind-base.css (200 KB) | ~425 KB |
| bbnf | math.bbnf (~5 KB) | ~15 KB |
| google_sheets | array-formula.txt (~2 KB) | ~6 KB |
| css_pretty | media-query.css (~1 KB) | ~3 KB |
| ebnf | abnf.ebnf (~3 KB) | ~9 KB |
| bnf | algol60.bnf (~10 KB) | ~30 KB |
| csv | mixed-types.csv (~5 KB) | ~15 KB |
| math | complex-expr.math (~1 KB) | ~3 KB |
| **TOTAL** | — | **~5.1 MB** |

Total fixture tree: ~5.1 MB. Well under GitHub's 100 MB per-file limit.

## §9 Fixture Materialisation Strategy

| Strategy | When |
|---|---|
| Download from source_url | Public datasets; first-time worktree init; `xtask worktree-init --fleet` |
| Copy from data/ (BC.W5 legacy) | When BC.W5's `data/json/twitter.json` already exists; faster than re-download |
| Symlink from data/ (cross-grammar) | When the same fixture serves multiple grammars (rare; e.g., twitter.json could feed JSON + json-via-bbnf parsing) |
| Hand-crafted (committed) | Cohort grammars; checked into git; xtask validates size only |

The xtask prefers in-tree copies > symlinks > downloads. Failed download falls back to next strategy.

## §10 Closing Posture

The worktree fixture infrastructure expands fleet-wide at BD.W4. Per-grammar `tests/fixtures/<grammar>/` directories ship 27+ canonical inputs; per-grammar `manifest.toml` declares metadata; xtask `--fleet` flag materialises (download / copy / symlink); CI matrix runs Rust + TS + WASM × 9 grammars in parallel; symlink contract supports parallel-agent dispatch. The pattern is additive to BC.W5's `data/{json,css,bbnf,sheets}` symlinks; both mechanisms coexist. Total fixture tree ~5.1 MB; well under platform limits. Public dataset licenses recorded per manifest.
