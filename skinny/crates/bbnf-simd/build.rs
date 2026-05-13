//! bbnf-simd build script — assembles vendored + authored x86_64 .asm sources.
//!
//! Per IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT § 5 (Wave 1 Agent 2 prescription)
//! and Lock 15 force-inline / i-cache extension, the x86_64 dispatch surface
//! relies on hand-rolled .asm primitives that `%include` `ext/x86/x86inc.asm`
//! and `ext/x86/x86util.asm`.  We compile them here with `nasm-rs` (assembly
//! driver) plus `cc` (final static archive) so `cargo build --release` is
//! self-contained.
//!
//! Build-script invariants:
//!   * AArch64 / wasm32 / non-x86 targets are no-ops; the script returns
//!     immediately so cross-builds remain trivial.
//!   * The set of authored .asm sources is currently empty — every kernel is
//!     intrinsic-only at this stage of SK-V3.  The script is structurally
//!     ready to pick them up the moment `src/x86_64/**/*.asm` lands.
//!   * Rebuild triggers are conservative: any .asm or .S anywhere under `src/`
//!     or under `ext/x86/` rewinds the script.
//!
//! This file MUST keep `cargo build` green on every host: when no .asm sources
//! are present and the host is not x86_64, the script exits with no `cc::Build`
//! invocation at all.

use std::env;
use std::path::{Path, PathBuf};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=ext/x86/x86inc.asm");
    println!("cargo:rerun-if-changed=ext/x86/x86util.asm");
    println!("cargo:rerun-if-changed=ext/x86/bbnf.asm");
    println!("cargo:rerun-if-env-changed=BBNF_SIMD_DISABLE_ASM");

    if env::var_os("BBNF_SIMD_DISABLE_ASM").is_some() {
        return;
    }

    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default();
    if target_arch != "x86_64" {
        // AArch64 + wasm32 + RISC-V + scalar: no assembler invocation needed.
        return;
    }

    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let asm_sources = gather_asm_sources(&manifest_dir.join("src"));
    if asm_sources.is_empty() {
        // No authored .asm yet; the vendored headers are include-only.  This is
        // the steady state for the early SK-V3 waves where every kernel is
        // intrinsic-bodied.  Exit cleanly so the build stays fast.
        return;
    }

    let include_root = manifest_dir.join("ext").join("x86");
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    // -- 1. assemble each .asm into an object via nasm-rs --------------------
    let mut nasm = nasm_rs::Build::new();
    nasm.include(&include_root)
        .files(asm_sources.iter().map(PathBuf::as_path))
        .flag("-DPIC")
        .flag("-DARCH_X86_64=1")
        .target(&env::var("TARGET").unwrap());

    let objects = match nasm.compile_objects() {
        Ok(objects) => objects,
        Err(error) => panic!("nasm assemble failed: {error}"),
    };

    // -- 2. archive the resulting objects via cc::Build ----------------------
    let mut archive = cc::Build::new();
    archive.out_dir(&out_dir);
    for object in &objects {
        archive.object(object);
    }
    archive.compile("bbnf_simd_asm");

    println!("cargo:rustc-link-lib=static=bbnf_simd_asm");
}

fn gather_asm_sources(root: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    walk(root, &mut out);
    out.sort();
    out
}

fn walk(dir: &Path, sink: &mut Vec<PathBuf>) {
    let entries = match std::fs::read_dir(dir) {
        Ok(iter) => iter,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            walk(&path, sink);
            continue;
        }
        match path.extension().and_then(|os| os.to_str()) {
            Some("asm") | Some("S") => sink.push(path),
            _ => {}
        }
    }
}
