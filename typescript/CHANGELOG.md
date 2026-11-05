# @mkbabb/bbnf-lang

## 0.3.0

### Minor Changes

- The emitted parser module is smaller and parses identically (bbnf-lang#2).
  First-unit tables are packed, action names are interned, and a text rule's
  value function reuses its recognize function instead of repeating the scan.
  On value.js's grammars the minified gzip size falls by 577 B (grammar.js) and
  119 B (color.js), and the parse results are byte-identical. A size test pins
  the fixture module's bytes and checks that the module imports nothing.
- **No public API changed.** The `!` in the subject of `3a2e03671`
  (`feat(emit)!: …`) overstates the change: the generator's API, the CLI and
  the emitted module's exports are all unchanged, and only the emitted text
  differs. That is why this is a 0.x minor rather than a breaking release. The
  commit is left as it is; this note is the correction.
- Releases are now published from CI with npm provenance on a
  `typescript-v*.*.*` tag (`.github/workflows/release-npm.yml`).
- The JSON data fixture read by `bbnf.test.ts` and the conformance suite is now
  checked in. A root `data/` ignore rule had kept it out, so a clean checkout
  could not run the suite.
- `test/size.test.ts` was pinned on macOS, and its gz pin failed on the
  ubuntu-latest release runner (23,704 against 23,650), because zlib builds
  differ. The raw-byte pin is exact on every platform and is unchanged
  (112,516). The gz pin is now 23,750: the larger measured reading, rounded up
  to 50 B.

## 0.2.0

- Published by hand (no provenance) against `@mkbabb/parse-that` 2.0.0.
