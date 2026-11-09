# Contributing to latex-paper

## Clone + install

```bash
git clone git@github.com:mkbabb/latex-paper.git
cd latex-paper
npm install
```

## Develop

```bash
npm run build          # library → dist/ (multi-entry: index, transform, vite, vue, theme)
npm test               # vitest unit suite
npm run test:watch     # vitest watch mode
npm run test:coverage  # coverage report
```

CI runs `build` + `test` on every PR + push to the default branch
(`.github/workflows/ci.yml`).

## Version bumps + releasing

Version bumps run through **changesets** (`.changeset/config.json`). For any change
that touches `src/`, `package.json`, or build config, author a changeset:

```bash
npx changeset            # pick major/minor/patch + write the summary
```

The changeset lands in your PR. On merge to the default branch, the changesets
workflow batches accepted changesets into a `Version Packages` PR; merging that PR
bumps the version, updates `CHANGELOG.md`, and cuts the `v*.*.*` tag. The tag triggers
`.github/workflows/release.yml`, which builds + tests + publishes to npm via
`NPM_TOKEN`.

**Never `npm publish` from a dev machine** — the publish operation belongs to CI on
tag. See [`docs/precepts/cross-repo-dev-iteration.md`](https://github.com/mkbabb/glass-ui/blob/master/docs/precepts/cross-repo-dev-iteration.md)
in glass-ui (the perimeter-level dev-iteration doc).

## Cross-repo feature work

latex-paper is consumed by `fourier-analysis` + `words`. When a feature spans
latex-paper + a consumer at the same time, use the `npm link` pattern documented at the
perimeter-level `cross-repo-dev-iteration.md`. The published `latest` tag is the
consumer-default; `npm link` is the active-feature escape hatch, retired the moment the
feature publishes and the consumer reinstalls the registry version.

## Conventions

TypeScript `strict` + `verbatimModuleSyntax` (`import type` for all type-only imports);
named exports only (no defaults); the package map is built from LaTeX TOC artifacts
(ordered, not title-matched); CHANGELOG.md updated when `src/`, `package.json`, or
build config changes.

## PR flow

1. Branch off the default branch.
2. Make the change + add/update tests.
3. Author a changeset (`npx changeset`).
4. Ensure `npm run build` + `npm test` exit 0.
5. Open the PR — CI runs the same gates.
