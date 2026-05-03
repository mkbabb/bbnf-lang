# Tranche I — Sister-Crate Publication

## Gestalt

Tranche I executes the Lock 11 promotion. The path-dep sister crates that have stabilised through tranches A-H promote to crates.io: egraph, egraph-derive, csp-solver, bbnf-regex. Each undergoes API freeze audit (cargo-semver-checks + manual review against per-crate `lib.rs` doc), version bump to 1.0, crates.io publication. parse-that's disposition per Pass A Proposal 4: the synthesizer's adjudication is **submodule-as-workspace-member** (per `docs/precepts/CONSUMING.md` precedent); parse-that remains workspace-internal under that disposition unless the user opts to publish later.

simd-scan stays workspace-internal explicitly per Lock 11 ("simd-scan + bootstrap + analysis + lsp stay workspace-internal").

The publication ceremony is mechanical post-API-freeze: cargo publish per crate, crates.io listing, README + docs update. The downstream effect on per-grammar declaration crates (which path-dep onto these sisters) is bounded — once published, declaration crates pin specific versions via `[workspace.dependencies]`.

## Hard gates

| Gate | Wave | Verification |
|---|---|---|
| egraph + egraph-derive API freeze | I.W0 | cargo-semver-checks passes; manual API review at `audit/restart/api-freeze-egraph-2026-XX-XX.md` |
| csp-solver API freeze | I.W0 | cargo-semver-checks passes; manual API review |
| bbnf-regex API freeze | I.W1 | cargo-semver-checks passes; manual API review |
| parse-that disposition ratified | I.W1 | submodule-as-workspace-member ratified or alternative disposition |
| egraph + egraph-derive published | I.W2 | crates.io listings present; downstream `[workspace.dependencies]` switch from path-dep to version |
| csp-solver published | I.W2 | crates.io listing present; downstream switch |
| bbnf-regex published | I.W2 | crates.io listing present; downstream switch |

## Wave summary table

| Wave | Name | Agents | Closes-on |
|---|---|---:|---|
| I.W0 — egraph + egraph-derive + csp-solver API freeze | cargo-semver-checks + manual review per crate | 3 parallel (per crate) | API freeze audit lands per crate |
| I.W1 — bbnf-regex API freeze + parse-that disposition | bbnf-regex audit; parse-that disposition ratified | 2 parallel | bbnf-regex API freeze; parse-that disposition documented |
| I.W2 — Publication ceremony | cargo publish per crate; downstream switch from path-dep to version | 1 (orchestrator-driven; mechanical) | crates.io listings present; workspace path-deps replaced |

## Carry-tags FROM

| Carry | Source tranche | Gate |
|---|---|---|
| Sister-crate path-deps registered | A | A.W2 |
| Optimiser pipeline integrated | F | F.W4 |
| Cost-model + rewrite rule integration | F | F.W3 |

## Carry-tags TO

| Carry | Receiving tranche | Gate |
|---|---|---|
| Published sister crates | J (J's perf gates run against published versions; ensures publication does not regress) | J.W2 |

## 14-lock honoured cell map

| Lock | Status | Wave |
|---|---|---|
| 1 — Tape dead | honoured | (continuous) |
| 2 — Layout canon | honoured | (continuous) |
| 3 — Cursor + byte-skip | honoured | (continuous) |
| 4 — Per-domain orthogonal | honoured | (continuous) |
| 5 — IR + per-backend | honoured | (continuous) |
| 6 — xtask source emit | honoured | (continuous) |
| 7 — `crates/path/` consolidated | honoured | (continuous) |
| 8 — Surpass SOTA | partial | (continuous; full set at J) |
| 9 — Slice-borrow primary | honoured | (continuous) |
| 10 — Pratt + SIMD auto-detected | honoured | (continuous) |
| 11 — Path-deps for sister crates | substantively-honoured | I.W2 (publication completes Lock 11's "promote to registry once stable" mandate) |
| 12 — ser + gorgeous archive | honoured | (continuous) |
| 13 — No god directories | honoured | (continuous) |
| 14 — Full grammar generalisation | honoured | (continuous) |

## Risks + mitigations

| Risk | Mitigation |
|---|---|
| Sister-crate publication breaks workspace path-dep consumers when API freezes | I.W0-W1 API freeze audit: cargo-semver-checks + manual review; downstream consumers (per-grammar declaration crates) pinned to specific versions; per master plan §13 R15 |
| crates.io publication fails (name conflict, etc.) | I.W2 staged: per-crate publish; if conflict, name disposition adjudicated (egraph → bbnf-egraph if needed) |
| Workspace switch from path-dep to version introduces transitive-dep version-conflict | I.W2 staged: per-crate switch + `cargo update` + `cargo check --workspace`; cross-crate version compatibility audit |
| API freeze audit identifies breaking changes mid-tranche | I.W0-W1 redress: API freeze landing point sequenced to absorb minor patches; major breakage triggers triumvirate |
| parse-that disposition is unclear post-tranche | I.W1 ratified disposition documented at `docs/process/restart/parse-that-disposition-2026-XX-XX.md` |

## Build/iter time gate

| Concern | Budget | Verification |
|---|---|---|
| API freeze audit per crate | ≤ 1 day per crate | I.W0-W1 |
| Publication ceremony per crate | ≤ 30 min per crate | I.W2 |
| Generated-LOC budget | I.exit: 173,750 LOC (unchanged from H.exit) | per master plan §12.2 |

## Voice locks

Per master plan §14.

## Closing posture

Tranche I closes with the sister crates promoted. Lock 11 honours substantively — the path-deps that incubated through tranches A-H are now stable, version-pinned, crates.io-published. Tranche J's final perf gates run against the published versions to ensure publication does not regress.

The greenfield mandate carries: API freeze is honest (cargo-semver-checks + manual review); no version 0.x carry-forward; the Lock 11 mandate "promote to registry once stable" honours.
