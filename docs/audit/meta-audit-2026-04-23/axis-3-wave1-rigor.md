# Axis 3 — Wave 1 Rigor

Current read-of-record for this audit: `5a260f94`.

Scope audited:

- `docs/tranches/meta-audit/05-validation.md`
- `docs/tranches/meta-audit/07-appurtenant-assay.md`
- `docs/tranches/meta-audit/08-abrogation-catalog.md`
- `docs/tranches/B1/TOOLCHAIN-SOTA.md`
- `docs/tranches/B1/TOOLCHAIN-MIGRATION.md`

Spot-check surface:

- sibling repos `../parse-that`, `../pprint`, `../csc411/CSC411_HW2_ProgrammingQuestion/csp-solver`
- gorgeous sibling/trash state
- live `rustc-ice-*.txt` corpus
- current workspace `.cargo/config.toml`, `.config/nextest.toml`, `Makefile`, and selected scripts
- official sources where the docs made toolchain or ecosystem claims:
  - Cargo Book `--timings`
  - Cargo unstable `codegen-backend`
  - `rustc_codegen_cranelift` upstream README
  - cargo-nextest configuration reference
  - Criterion docs
  - cargo-watch upstream repo
  - bacon docs
  - Rust nightly manifest at `static.rust-lang.org`

## Verified

### 1. The ICE diagnosis in `TOOLCHAIN-SOTA.md` is substantially correct

The core fact pattern checks out:

- current repo root contains **93** `rustc-ice-*.txt` files
- earliest and latest match the document's time window:
  - `rustc-ice-2026-04-15T07_03_35-73396.txt`
  - `rustc-ice-2026-04-22T15_55_47-25568.txt`
- all 93 root ICE files contain the same panic site and query stack:
  - `on_disk_cache.rs:663: cannot decode AttrId with CacheDecoder`
  - `#0 [analysis] running analysis passes on crate bbnf_analysis`
- all 93 root ICE files report the same compiler:
  - `rustc 1.96.0-nightly (9602bda1d 2026-04-05)`

`docs/tranches/B1/TOOLCHAIN-SOTA.md` is therefore right to treat the current incremental-cache failure as a real blocker, and right to separate it from ordinary proc-macro wall time.

### 2. The parse-that sibling spot-check is real, and distinct from the bbnf-lang ICE cluster

The sibling repo `../parse-that` exists at `919d77d18cd8` and currently has one untracked ICE file:

- `../parse-that/rust/parse_that/rustc-ice-2026-04-22T15_05_48-88533.txt`

That file is **not** the same failure as the 93-file bbnf-lang cluster. It is a codegen/write failure in `rustc_codegen_ssa/src/back/write.rs` while writing `pre-lto.bc`, exactly the distinction `07-appurtenant-assay.md` makes.

### 3. The csp-solver sibling assessment is directionally correct

The sibling surface exists and the criterion-heavy bench posture is real:

- `../csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/Cargo.toml` has **6** `[[bench]]` entries and `criterion = "0.5"`
- `../csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/morph-core/Cargo.toml` has **2** `[[bench]]` entries and `criterion = "0.5"`
- `../csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/scripts/bench-compare.sh` is a real criterion-baseline wrapper built around `--save-baseline` / `--baseline`

The vendored workspace copy at `crates/csp-solver` still has no bench targets, so the assay's governance concern about sibling-vs-vendored authority is real.

### 4. Several abrogation verdicts hold up cleanly

Five spot-checks were enough to confirm the catalog is not hand-waving:

- `scripts/check-cst-invariants.sh` as **ABROGATE** is correct. It is tranche-specific grep enforcement for a closed CST cleanup and is not part of any live B1/AZ path.
- `scripts/profile.sh` as **ABROGATE** is correct. It still does `cd rust`, which no longer matches the repo layout, so it is not only superseded but stale.
- `scripts/verify-w2-symbols.sh` as **ABROGATE** is correct. It is explicitly tied to `AW-IV.W2.2` symbol checks and no longer belongs on the live operational surface.
- `scripts/bootstrap-bbnf.sh` as **KEEP-MODERNIZE** is correct at the top level. It is a load-bearing self-hosting regen surface and cannot simply be deleted.
- `watch` as **REPLACE** is directionally correct. The current `Makefile` still uses `cargo watch`, and upstream `watchexec/cargo-watch` is archived as of January 18, 2025, while bacon is live and documented as a maintained background checker.

### 5. The nextest direction is sound

The repo already has a real `.config/nextest.toml`, and it is not cargo-culted:

- local profile has `slow-timeout`, `leak-timeout`, and retry policy
- CI profile exists
- the config is already structured around bounded hangs and leak detection

`TOOLCHAIN-SOTA.md` and `TOOLCHAIN-MIGRATION.md` are right that the real gap is not "should we use nextest?" but "should CI and the canonical aliases require it?" The answer is yes.

### 6. Cargo timings are the right first-principles measurement tool

The SOTA direction to use Cargo's own timing surfaces is sound. The Cargo Book explicitly recommends `cargo build --timings`, which emits unit-level and concurrency graphs into `target/cargo-timings/cargo-timing.html`. For this repo's current problem shape, that is a better primary diagnostic than intuition or framework churn.

## Refined

### 1. `05-validation.md` is rigorous, but it is rigor at a point in time

`05-validation.md` is explicit that its read-of-record is `48e6eaa9`. That matters. The document is not wrong for that commit; it is simply no longer current at `5a260f94`.

As a validation artefact, it should be read as:

- good forensic audit of the earlier state
- not a current-state truth source

That distinction needs to be explicit whenever it is reused by later orchestration.

### 2. The appurtenant bench-framework totals need to be stated as time-bounded

`07-appurtenant-assay.md` gives a total of:

- 40 bencher benches
- 8 criterion benches
- 4 `test::Bencher` benches
- 0 divan benches

That was at least plausible when authored, but the current accessible surface at `5a260f94` is:

- **38 bencher bench entries**
  - parse-that `parse_that`: 13
  - parse-that `regex`: 4
  - parse-that `bootstrap`: 1
  - `crates/core`: 19
  - `crates/json-prototype`: 1
- **8 criterion bench entries**
  - sibling `csp-solver`: 6
  - sibling `morph-core`: 2
- **2 `test::Bencher` benches**
  - `../pprint/rust/benches/pprint.rs`
  - `../pprint/rust/benches/digit_count.rs`
- **0 divan benches**

Two reasons for the delta:

- the gorgeous sibling is no longer present on disk, so its 2 bencher benches are not part of the current accessible surface
- `crates/tape` and `crates/simd-scan` were counted as unstable bench surfaces in the prose, but their live bench files are custom `main`-style harnesses, not `test::Bencher`

The recommendation to modernize the fleet still stands, but the counts must be re-stated as commit- and filesystem-bounded, not timeless.

### 3. The criterion critique is directionally right but phrased too absolutely

The SOTA docs are right that Criterion's defaults are not naturally aligned with the repo's "cold parse first" feedback:

- Criterion defaults to a warm-up period
- Criterion defaults to longer measurement windows
- Criterion defaults to plotting/report output

But the current language overstates the case. Official Criterion docs show:

- `warm_up_time` is configurable
- machine-readable output exists
- plotting is configurable, not mandatory in every workflow

The defensible claim is:

- Criterion is a weaker fit for this repo's stated measurement style and operator preferences

The over-strong claim is:

- Criterion is structurally unsuitable or inherently too heavy

Those are not the same claim.

### 4. The nextest migration should be framed as promotion, not greenfield adoption

`TOOLCHAIN-MIGRATION.md` Step 3 reads like a fresh install of `.config/nextest.toml`. That is imprecise. The repo already has a usable nextest config. The actual migration is:

- refine the existing config
- add missing profiles if needed
- make CI and canonical commands use it by default

That is a materially smaller and more credible change than "land nextest."

### 5. The cargo-watch to bacon recommendation is good, but the justification should be tightened

The catalog is right to move away from `cargo watch`, because the upstream repo is archived. The bacon alternative is also current and live. But the real justification is:

- maintained watcher
- nextest-aware workflows
- project-local `bacon.toml`

Not:

- generic "better UX" alone

The version floor in the doc (`v3.14+`) is also stale relative to the currently published `bacon 3.22.0`.

### 6. Alias-surface counts in the assay are stale

`07-appurtenant-assay.md` describes the workspace alias surface as "9 aliases". The current tracked `.cargo/config.toml` has **11** alias entries:

- `iter-check`
- `iter-check-full`
- `iter-test-leaf`
- `iter-test-grammar`
- `expand-json`
- `expand-css`
- `expand-bbnf`
- `expand-sheets`
- `asm-parse`
- `prep-bench`
- `final-bench`

This is a precision issue, not a conceptual one, but Wave 1 was explicitly about operational rigor, so the precision matters.

## Flawed

### 1. The current appurtenant-assay bench totals are not reproducible as written

At current HEAD, `07-appurtenant-assay.md`'s totals and one repo row are materially off:

- the gorgeous sibling row assumes `../gorgeous` exists; it does not
- the totals still count gorgeous's 2 bencher benches
- `crates/tape` and `crates/simd-scan` are counted under `test::Bencher`, but their live bench files are custom standalone harnesses

This is not a nit. The totals are used to justify the fleet migration burden, and the burden statement should be anchored to the current accessible surface.

### 2. The nightly-pin story is internally inconsistent and not evidence-backed

The two B1 toolchain docs disagree on the fundamental choice:

- `TOOLCHAIN-SOTA.md` Rank 2 proposes pinning **`nightly-2026-04-05`**, the exact nightly all 93 ICE files share
- `TOOLCHAIN-MIGRATION.md` instead freezes **`nightly-2026-04-11`**

Neither document actually demonstrates that either pin is clean on this workload.

This is the key problem:

- pinning the known-buggy 2026-04-05 nightly reproduces the bug, but does not fix it
- pinning 2026-04-11 may be reasonable, but it is still an unverified guess until a reproducer or smoke test clears it

The docs themselves say the right method is bisect/probe. They then fail to follow it.

### 3. The rust-toolchain draft uses the wrong Cranelift component name

`docs/tranches/B1/patches/rust-toolchain.toml.draft` lists:

- `rustc-codegen-cranelift`

The official component name is:

- `rustc-codegen-cranelift-preview`

This is not a wording issue. The draft as written would not install the component it claims to require.

### 4. The config draft's Cranelift enable path is incomplete

`docs/tranches/B1/patches/config.toml.draft` comments a profile line:

- `codegen-backend = "cranelift"`

But the draft does **not** enable Cargo's unstable gate for that setting. The Cargo Book is explicit: using `codegen-backend` in `.cargo/config.toml` requires either:

- `-Z codegen-backend`, or
- `[unstable] codegen-backend = true`

Neither is present in the draft. So even after fixing the component name, the draft is still not runnable as written.

### 5. The linker guidance is not robust enough to land

There are three different linker stories in the Wave 1 surface:

- current tracked `.cargo/config.toml` comments say:
  - `brew install lld`
  - check `/opt/homebrew/opt/lld/bin/ld64.lld`
- `TOOLCHAIN-SOTA.md` Rank 6 repeats the `ld64.lld` / `brew install lld` story
- `TOOLCHAIN-MIGRATION.md` and `patches/config.toml.draft` switch to:
  - `brew install llvm`
  - `/opt/homebrew/opt/llvm/bin/ld.lld`

On the current host:

- `/opt/homebrew/opt/llvm` exists
- `/opt/homebrew/opt/llvm/bin/ld.lld` does **not** exist
- `/opt/homebrew/opt/lld` does **not** exist as a real directory

So the migration draft is not merely unverified; it hard-codes a path that is absent on the audit host. The correct conclusion is not "reject lld"; it is "replace hard-coded assumptions with a host-check step or a documented probe."

### 6. The value-lane bench naming drift persists inside the migration drafts

The live bench target is:

- `json_monolithic_value`

But `docs/tranches/B1/patches/config.toml.draft` still uses:

- `json_value`

This is the same naming drift already visible in other live docs and scripts. If the migration draft were executed as written, the bench alias surface would still point at a non-canonical target name.

### 7. The nextest draft contains a config-shape error

`docs/tranches/B1/patches/nextest.toml.draft` ends with:

- `[profile.close.overrides]`

nextest override sections are array-of-table entries:

- `[[profile.<name>.overrides]]`

The draft already uses the correct double-bracket form earlier for `profile.default`. The final single-bracket section is therefore almost certainly a syntax error, not a stylistic variant.

### 8. The abrogation catalog's bootstrap-cache conclusion is wrong

`08-abrogation-catalog.md` says `scripts/bootstrap-bbnf.sh` should preserve:

- `rm -rf target/.bbnf-cache/`

because `clean-regen-discipline` supposedly requires it.

That is the wrong conclusion. Fresh regen discipline requires:

- deterministic regeneration
- checked-in output matching a fresh run

It does **not** require nuking the proc-macro cache on every routine invocation. The later B1 docs are correct to target that line as a speed leak. The catalog's specific "Preserve" call is therefore flawed.

### 9. The migration timeline is too optimistic for what it contains

`TOOLCHAIN-MIGRATION.md` budgets the full sequence at roughly 2 to 2.5 agent-days while including:

- nightly pinning
- `.cargo/config.toml` rewrite
- nextest rework
- Makefile rewrite
- 19-core-bench divan port
- iai-callgrind CI
- script rewrites
- cross-repo propagation
- doc pass

That may be possible for a narrow, flawless execution window, but as an audit judgement it is not a rigorous estimate. It omits the time needed for:

- nightly verification
- Cranelift verification on the pin
- bench parity debugging when the first migrated harness drifts
- host-linker detection failures

This is better treated as an optimistic floor than a planning-grade estimate.

## Open

### 1. The gorgeous sibling posture can no longer be validated directly

`../gorgeous` is absent and no corresponding `gorgeous` directory was found under `~/.Trash` during this pass. That leaves three possibilities:

- the sibling was deleted intentionally after the Wave 1 assay
- it was moved elsewhere
- the assay captured a now-stale local state

The fleet-modernization conclusions that depend on a live gorgeous sibling should therefore be treated as historically informative, not currently verified.

### 2. No evidence yet shows that `nightly-2026-04-11` actually clears the ICE on this workload

The official nightly manifest confirms that `rustc-codegen-cranelift-preview` exists for `aarch64-apple-darwin` on 2026-04-11. That answers availability.

It does **not** answer correctness on this repo.

The missing proof is one of:

- a local reproducer that fails on 2026-04-05 and passes on 2026-04-11
- a bisect note that narrows the fix window
- a committed smoke-test artefact

Until then, the chosen pin is a candidate, not a verified repair.

### 3. Cranelift is available, but not yet justified for B1

Official sources confirm:

- the component exists on nightly
- Cargo supports `codegen-backend`
- the backend is supported on macOS AArch64

What is still missing is repo-local justification that Cranelift belongs in B1 rather than as a follow-on optimization:

- no compile timing on this repo with Cranelift vs LLVM
- no evidence that it does not interact badly with the existing proc-macro-heavy path
- no evidence that it improves the actual dominant walls rather than the smaller codegen tail

That keeps Cranelift in the "interesting, plausible, not yet proven here" bucket.

### 4. The divan recommendation is still a recommendation, not a validated migration outcome

The Wave 1 docs make a strong case for leaving bencher behind. They do not yet prove:

- that divan is the best fit for every bench in this repo
- that all current benches can be ported mechanically without semantics drift
- that the post-port output shape cleanly replaces the current close-matrix artefacts

The recommendation is credible. The rigor bar would be met by one real exemplar migration with before/after proof, not by prose alone.

### 5. The existing nextest config needs a compatibility check against the proposed alias surface

The repo already has a nextest config. The drafts add:

- `ax-iter`
- `close`
- junit output expectations
- stronger CI reliance

What remains open is whether the full alias plan and the proposed nextest profiles line up one-for-one with the intended B1 routine/CI/close surfaces. The direction is right, but the exact operational map still needs a single reconciled truth document or landed patch.

## Bottom line

Wave 1 was strong at identifying the right problem classes:

- the ICE flood is real
- appurtenant repos do matter
- the bench/test/tooling surface needs modernization
- stale tranche-specific scripts should be removed instead of preserved

Wave 1 was weaker at the final 10% of operational rigor:

- some counts are stale
- one bench class is misclassified
- the gorgeous sibling state is no longer reproducible
- the nightly pin is not evidence-backed
- the migration drafts contain at least four concrete execution defects:
  - wrong Cranelift component name
  - missing Cargo unstable gate for `codegen-backend`
  - linker path assumption not valid on the audit host
  - stale `json_value` bench naming

Verdict for Axis 3: the Wave 1 audit delivered useful diagnosis and mostly sound direction, but the toolchain migration package is **not dispatchable as written**. Before B1 opens on this basis, the nightly choice, Cranelift wiring, linker detection, and bench-surface naming need one more correctness pass.
