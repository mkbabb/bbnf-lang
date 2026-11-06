# Changesets

This directory holds [changesets](https://github.com/changesets/changesets) — one
markdown file per pending change, declaring the semver bump (major/minor/patch)
and a human-readable summary.

Add one with `npx @changesets/cli` (or `npx changeset`). Merging the accumulated
changesets via the Version Packages PR cuts the version bump, updates
`CHANGELOG.md`, and tags `v*.*.*` — which fires `.github/workflows/release.yml`
to publish to npm.
