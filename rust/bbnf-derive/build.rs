/// Build script for bbnf-derive.
///
/// Emits a `BBNF_DERIVE_BUILD_ID` environment variable containing a timestamp
/// so that the codegen cache key changes every time bbnf-derive is recompiled.
/// Since Cargo recompiles bbnf-derive whenever any of its transitive dependencies
/// (bbnf, bbnf-ir, parse_that) change, this ensures the cache is invalidated
/// when the codegen logic changes — even without a version bump.
fn main() {
    // Emit a build-time timestamp as a cargo env var.
    // This changes on every recompilation, invalidating stale caches.
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    println!("cargo:rustc-env=BBNF_DERIVE_BUILD_ID={}", now);
}
