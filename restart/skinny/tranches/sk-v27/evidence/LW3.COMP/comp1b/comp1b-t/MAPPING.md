COMP-1b-T mapping (law A, sk-v26 -> sk-v27):
A1 regular-file checks on workspace/core manifests, .cargo/config.toml, Cargo.lock, owned parse_that and regex manifests -> kept for all five plus rust-toolchain.toml; the owned manifests are the pinned checkout's (from cargo metadata), not ../parse-that-skv26.
A2 root .cargo one dir -> kept.
A3 root keys {profile, workspace} -> kept.
A4 workspace keys {dependencies, members, package, resolver} -> {dependencies, exclude, members, metadata, resolver} exact (master's side).
A5 members [crates/core], resolver 3 -> the 12 members in order, exclude [wasm], resolver 2; plus metadata.bbnf.grammars: 9 idents in order, each path a tracked file.
A6 workspace.package exact -> absent (exact key set).
A7 profile {bench, release} with exact keys/values -> {bench, dev, profiling-prep, release} with exact keys/values incl. dev.package overrides.
A8 workspace deps exact set, owned edges {default-features, features, path, version} with =0.5.0/=0.1.0 and ../parse-that-skv26 paths -> exact set of six; owned edges exactly {git, rev, version}, git URL, version, 40-hex rev, one parse-that rev across four edges, pprint edge; csp-solver "0.4.0"; no [patch], no path in any workspace dep.
A9 registry workspace deps blake3/serde/serde_json exact shape -> moved to core's own [dependencies] (the union), exact shape asserted there.
A10 parse_that manifest: keys, identity, no build/links, lib, features {default: []}, deps {} -> keys, identity (0.4.0, its description), no build/links, lib, exact feature map (combinators, default=combinators, diagnostics=dep:colored, nightly=smallbox/nightly), exact dep set, and the parse_that->pprint edge is git+rev at the same pprint rev as the workspace (lock-step).
A11 bbnf-regex manifest exact -> parse-that-regex manifest exact (keys, identity, lib parse_that_regex, features, dep set, regex-automata =0.4.14, regex-syntax =0.8.10, dev-deps, 4 benches in order).
A12 core: keys, package keys (autotests...=false), description, workspace-inherited edition/license/repository, lib, one bin, 11 [[test]] exact, no features/build/links, version 0.4.0, deps {workspace=true}x5, dev-deps exact -> keys exact (bench, bin, dependencies, dev-dependencies, features, lib, package, target, test), package keys exact (master's: autotests etc. absent = default), literals, CARGO_PKG_VERSION 0.3.0, lib, one bin, the same 11 [[test]] exact, exact feature map (7), 20 benches in order, linux iai-callgrind target dep, exact dep set (15) with owned edges {workspace=true}, bbnf-ir/bbnf-ser path+version, blake3/serde/serde_json exact shape; exact dev-dep set (31) with jsonschema/syn/sha2/tempfile/toml/trybuild values; every path edge inside the repository.
A13 cargo config {alias, build}, rustc-wrapper sccache, 4 aliases -> {alias, profile, target}, no build override, 4 profiles, linux target, 41 aliases with xtask/iter-clippy/iter-test/iter-check-full exact.
A14 one config authority, no alternates, no build.rs -> kept; plus xtask/Cargo.toml present (restored whole).
A15 metadata width 142, one bbnf, 0.4.0, empty features, exact target set -> width 511 sealed, one bbnf, 0.3.0, source null, exact feature keys, target inventory sha256 (158) with lib/bin/root_api_present/the 11 named.
A16 dep rows exact set of 11 -> dep rows (47) sealed by sha256, row now also carries optional, target and source; no rename, no registry; owned deps source = the pinned git edge, path deps inside the repo, others crates.io.
A17 owned dependency metadata path = sibling -> owned packages exactly {pprint, pprint_derive, parse_that, parse-that-regex, -simd, -egraph, -egraph-derive}, each source = git+URL?rev=R#R at the workspace's rev.
A18 resolve identity sha -> same formula, new seal.
A19 lock sha, keys, version 4, width = metadata, identity rows equal, owned lock entries no source/checksum -> lock sha sealed, keys, v4, width = all-features metadata (515; the default graph omits 4 inactive optional deps), identity rows equal after decoding the lock's percent-encoded git sources, default graph a subset; bbnf no source/checksum; each owned entry = its pinned git source, no checksum.
A20 lock dep tables for bbnf/parse_that/bbnf-regex -> bbnf's table holds every manifest dep and dev-dep; exact tables for parse_that (9) and parse-that-regex (8).
A21 cfg(feature) and ingress over the lib.rs graph (sk-v26 lib.rs = compiler only) -> over compiler/mod.rs's graph (the same modules; lib.rs is the hand-written root and must declare `mod compiler`), plus the bin; graph asserted to be exactly crates/core/src/compiler/*.
A22/A23 no-overfit and generated-product surfaces -> unchanged.

law B mapping:
B1 package parse_that/bbnf-regex from ../parse-that-skv26 twice, byte-identical, file lists, confined root, Cargo.toml + src/lib.rs -> the 7 upstream crates from the pinned checkouts (isolated CARGO_HOME, git dbs linked) and the 4 bbnf-lang crates, each twice, byte-identical, identical lists, confined, Cargo.toml + Cargo.toml.orig + the manifest's lib path.
B2 cargo package verify (implicit) -> each unpacked manifest (dev-deps included) resolves offline from the registry, and the registry-only consumer (now with bbnf-path) compiles every packaged library from its archive and runs; cargo's per-pass verify build dropped (time), nothing it asserted is lost.
B3 local registry from metadata + cargo cache, checksum = record -> from the union of the three locks (bbnf-lang, pinned parse-that, pinned pprint), each record from the sparse cache with cksum = lock checksum, every cached archive hash-checked.
B4 receipt sha + rows (sha256, files, byte-identical, verify PASS) -> ABI-4 receipt sha + schema + kernel_abi 4 + both revs = the workspace's pins + every row equal (name, version, repository, rev, path_in_vcs, sha256, files, byte-identical, registry_verify).
B5 (new) identity by tree: every packaged file equals the blob at the pin (Cargo.toml.orig = committed manifest; relocated readme), VCS record names the rev and path, not dirty.
B6 (new) the pinned kernel declares KERNEL_ABI 4.
B7 consumer: no path/patch/workspace; bbnf/parse_that/bbnf-regex crates.io identity; compile_instance; v6 manifest; KERNEL_ABI -> same, no git either, 9 packages checked, every package crates.io identity, KERNEL_ABI 4.
B8 runtime: dual-root identical, one harness, exact parse_that from registry, all public cases -> same at 0.4.0.
