# SK-V14 W3-D: Provenance Manifest

Date: 2026-05-26.
Scope: manifest fields required for W3 provenance.
Output: this file.

## Section 1 - Findings

W3's manifest needs enough information to re-fetch and verify the exact corpus:
source URL, version pin, byte count, SHA-256, freshness stamp, and license. The
selected jsDelivr responses returned immutable cache-control headers and
`x-jsd-version-type: version`, which makes the URL version pins suitable for
W3.

Package metadata for all four npm packages reports MIT license.

## Section 2 - Recommendations

Use a human-readable markdown manifest because SPEC names `manifest.md`. Keep
the executable source of truth duplicated in `css_l4_corpus.rs` so tests can
validate the actual staged bytes.

## Section 3 - Risks

Prose-only provenance is not enough. If loader tests do not validate the staged
bytes against the manifest values, W3 could drift silently before W8.

## Section 4 - Sources

- `curl -LfsSI` against the four jsDelivr URLs.
- `curl -LfsS .../package.json` for package license metadata.
- `shasum -a 256 skinny/corpora/css-l4-sk-v14/*.css`.
